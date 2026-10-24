(in-package #:kitty-graphics)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(define-condition graphics-protocol-error (error)
  ((message :initarg :message :reader graphics-protocol-error-message))
  (:report (lambda (condition stream)
             (write-string (graphics-protocol-error-message condition) stream))))

(defun protocol-error (control &rest arguments)
  (error 'graphics-protocol-error :message (apply #'format nil control arguments)))

(defstruct (graphics-command (:constructor %make-graphics-command))
  ;; An alist keyed by characters. Keys are case-sensitive by specification.
  (controls '() :type list)
  (data #() :type octets))

(defun graphics-command-value (command key &optional default)
  (let ((entry (assoc key (graphics-command-controls command) :test #'char=)))
    (if entry (cdr entry) default)))

(defun graphics-command-integer (command key &optional default)
  (let ((value (graphics-command-value command key)))
    (if value
        (handler-case (parse-integer value :junk-allowed nil)
          (error () (protocol-error "Invalid integer value for ~c: ~s" key value)))
        default)))

(defun graphics-command-action (command)
  (let ((value (graphics-command-value command #\a "t")))
    (and (= (length value) 1) (char value 0))))

(defun graphics-command-more-p (command)
  (= 1 (graphics-command-integer command #\m 0)))

(defun strip-envelope (payload)
  (cond
    ((and (>= (length payload) 4)
          (char= (char payload 0) (code-char 27))
          (char= (char payload 1) #\_)
          (char= (char payload 2) #\G)
          (char= (char payload (- (length payload) 2)) (code-char 27))
          (char= (char payload (1- (length payload))) #\\))
     (subseq payload 3 (- (length payload) 2)))
    ((and (plusp (length payload)) (char= (char payload 0) #\G))
     (subseq payload 1))
    (t payload)))

(defun parse-controls (text)
  (if (zerop (length text))
      '()
      (loop with start = 0
            for comma = (position #\, text :start start)
            for end = (or comma (length text))
            for equals = (position #\= text :start start :end end)
            unless (and equals (= equals (1+ start)) (< equals (1- end)))
              do (protocol-error "Malformed Kitty graphics control field: ~s"
                                 (subseq text start end))
            collect (cons (char text start) (subseq text (1+ equals) end))
            while comma
            do (setf start (1+ comma)))))

(defun ascii-octets-to-string (octets)
  (map 'string #'code-char octets))

(defun ensure-string (payload)
  (etypecase payload
    (string payload)
    ((array (unsigned-byte 8) (*)) (ascii-octets-to-string payload))))

(defun decode-base64 (text)
  (let* ((length (length text))
         (padding (or (position #\= text) length)))
    (unless (and (zerop (mod length 4))
                 (loop for index below padding
                       for char = (char text index)
                       always (or (alphanumericp char) (find char "+/")))
                 (<= (- length padding) 2)
                 (loop for index from padding below length
                       always (char= (char text index) #\=)))
      (protocol-error "Invalid base64 image payload"))
    (handler-case (cl-base64:base64-string-to-usb8-array text)
      (error () (protocol-error "Invalid base64 image payload")))))

(defun parse-graphics-command (payload &key (decode-data t))
  "Parse a Kitty graphics APC payload or complete ESC _ G ... ESC \\ sequence."
  (let* ((body (strip-envelope (ensure-string payload)))
         (separator (position #\; body))
         (control-text (if separator (subseq body 0 separator) body))
         (encoded-data (and separator (subseq body (1+ separator)))))
    (%make-graphics-command
     :controls (parse-controls control-text)
     :data (if (and decode-data encoded-data (plusp (length encoded-data)))
               (decode-base64 encoded-data)
               (make-array 0 :element-type '(unsigned-byte 8))))))

(defun encode-controls (controls)
  (format nil "~{~c=~a~^,~}"
          (loop for (key . value) in controls append (list key value))))

(defun encode-graphics-command (controls &optional data)
  "Encode CONTROLS and optional octet DATA as a complete Kitty APC sequence."
  (let ((esc (code-char 27)))
    (format nil "~c_G~a~@[;~a~]~c\\"
            esc (encode-controls controls)
            (and data (plusp (length data))
                 (cl-base64:usb8-array-to-base64-string data))
            esc)))

(defun encode-graphics-response (controls status &optional message)
  "Encode a terminal acknowledgement such as i=1;OK or i=1;EINVAL:details."
  (let ((esc (code-char 27)))
    (format nil "~c_G~a;~a~@[:~a~]~c\\"
            esc (encode-controls controls) status message esc)))

(defstruct (graphics-decoder (:constructor make-graphics-decoder
                                (&key (max-encoded-bytes (* 64 1024 1024)))))
  (max-encoded-bytes (* 64 1024 1024) :type (integer 1 *))
  first-controls
  (encoded-parts '() :type list)
  (encoded-length 0 :type (integer 0 *)))

(defun graphics-decoder-reset (decoder)
  (setf (graphics-decoder-first-controls decoder) nil
        (graphics-decoder-encoded-parts decoder) '()
        (graphics-decoder-encoded-length decoder) 0)
  decoder)

(defun payload-parts (payload)
  (let* ((body (strip-envelope (ensure-string payload)))
         (separator (position #\; body)))
    (values (parse-controls (if separator (subseq body 0 separator) body))
            (if separator (subseq body (1+ separator)) ""))))

(defun graphics-decoder-feed (decoder payload)
  "Feed one complete APC command. Return a command when all chunks arrive.

Base64 is joined before decoding, so callers need not align input chunks to
base64 quartets. A protocol error resets the partial transfer."
  (handler-case
      (multiple-value-bind (controls encoded) (payload-parts payload)
        (let* ((continuing (graphics-decoder-first-controls decoder))
               (effective-controls (or continuing controls))
               (more (equal "1" (cdr (assoc #\m controls :test #'char=)))))
          (unless continuing
            (setf (graphics-decoder-first-controls decoder) controls))
          (incf (graphics-decoder-encoded-length decoder) (length encoded))
          (when (> (graphics-decoder-encoded-length decoder)
                   (graphics-decoder-max-encoded-bytes decoder))
            (protocol-error "Kitty graphics transfer exceeds the configured quota"))
          (push encoded (graphics-decoder-encoded-parts decoder))
          (if more
              nil
              (let* ((joined (apply #'concatenate 'string
                                    (nreverse (graphics-decoder-encoded-parts decoder))))
                     (data (if (plusp (length joined))
                               (decode-base64 joined)
                               (make-array 0 :element-type '(unsigned-byte 8))))
                     (command (%make-graphics-command
                               :controls (remove #\m effective-controls
                                                 :key #'car :test #'char=)
                               :data data)))
                (graphics-decoder-reset decoder)
                command))))
    (error (condition)
      (graphics-decoder-reset decoder)
      (error condition))))

(defun graphics-command-rgba (command)
  "Return RGBA octets, width and height for an uncompressed direct RGB command.

PNG (f=100), compressed (o=z), and non-direct transmission media are retained
by the parser but deliberately left to consumers."
  (let ((format (graphics-command-integer command #\f 32))
        (medium (graphics-command-value command #\t "d"))
        (compression (graphics-command-value command #\o))
        (width (graphics-command-integer command #\s 0))
        (height (graphics-command-integer command #\v 0))
        (data (graphics-command-data command)))
    (unless (and (member format '(24 32)) (string= medium "d") (null compression)
                 (plusp width) (plusp height))
      (protocol-error "Command is not uncompressed direct RGB/RGBA data"))
    (let ((channels (if (= format 24) 3 4)))
      (unless (= (length data) (* width height channels))
        (protocol-error "Pixel data length does not match ~dx~d f=~d" width height format))
      (if (= channels 4)
          (values data width height)
          (let ((rgba (make-array (* width height 4)
                                  :element-type '(unsigned-byte 8))))
            (loop for source from 0 below (length data) by 3
                  for target from 0 by 4
                  do (setf (aref rgba target) (aref data source)
                           (aref rgba (+ target 1)) (aref data (+ source 1))
                           (aref rgba (+ target 2)) (aref data (+ source 2))
                           (aref rgba (+ target 3)) 255))
            (values rgba width height))))))
