(in-package #:kitty-graphics)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(define-condition graphics-protocol-error (error)
  ((message :initarg :message :reader graphics-protocol-error-message)
   ;; The errno-style name a terminal puts before the message in its response.
   (code :initarg :code :initform "EINVAL" :reader graphics-protocol-error-code))
  (:report (lambda (condition stream)
             (write-string (graphics-protocol-error-message condition) stream))))

(defun protocol-error-with-code (code control &rest arguments)
  "Signal a graphics-protocol-error with response CODE, such as \"EBADF\"."
  (error 'graphics-protocol-error :code code
                                  :message (apply #'format nil control arguments)))

(defun protocol-error (control &rest arguments)
  (apply #'protocol-error-with-code "EINVAL" control arguments))

(defun file-read-error ()
  "Signal the single error the protocol requires for every image file failure."
  (protocol-error-with-code "EBADF" "Failed to read image file"))

(defstruct (graphics-command (:constructor %make-graphics-command))
  ;; An alist keyed by characters. Keys are case-sensitive by specification.
  (controls '() :type list)
  ;; The decoded payload, or NIL when the decoder put the pixels straight
  ;; into RGBA as they arrived (see DIRECT-RGBA-SIZE).
  (%data nil :type (or null octets))
  ;; Those pixels, as RGBA, and how many octets the payload held.
  (pixels nil :type (or null octets))
  (payload-length 0 :type (integer 0 *)))

(defun graphics-command-data (command)
  "COMMAND's decoded payload.  When its pixels were decoded straight into
RGBA, this rebuilds it from them, without what lay beyond the image."
  (or (graphics-command-%data command)
      (let* ((rgba (graphics-command-pixels command))
             (rgb-p (equal "24" (graphics-command-value command #\f)))
             (length (min (graphics-command-payload-length command)
                          (if rgb-p (* 3 (floor (length rgba) 4)) (length rgba))))
             (data (make-array length :element-type '(unsigned-byte 8))))
        (dotimes (index length)
          (setf (aref data index)
                (aref rgba (if rgb-p (+ index (floor index 3)) index))))
        (setf (graphics-command-%data command) data))))

(defun graphics-command-data-length (command)
  "How many octets COMMAND's payload decoded to."
  (if (graphics-command-%data command)
      (length (graphics-command-%data command))
      (graphics-command-payload-length command)))

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

(defun base64-char-p (char)
  (or (char<= #\A char #\Z) (char<= #\a char #\z) (char<= #\0 char #\9)
      (char= char #\+) (char= char #\/)))

(defun decode-base64-segment (text start end)
  (let ((length (- end start)))
    (when (= 1 (mod length 4))
      (protocol-error "Invalid base64 image payload"))
    (handler-case
        (cl-base64:base64-string-to-usb8-array
         (concatenate 'string (subseq text start end)
                      (make-string (mod (- length) 4) :initial-element #\=)))
      (error () (protocol-error "Invalid base64 image payload")))))

(defun decode-base64 (text)
  "Decode TEXT, which may lack padding or join separately padded chunks."
  (unless (every (lambda (char) (or (base64-char-p char) (char= char #\=))) text)
    (protocol-error "Invalid base64 image payload"))
  (let ((segments
          (loop with start = 0
                for padding = (position #\= text :start start)
                for end = (or padding (length text))
                when (< start end)
                  collect (decode-base64-segment text start end)
                while padding
                do (setf start (or (position #\= text :start padding :test-not #'char=)
                                   (length text))))))
    (if (and segments (null (rest segments)))
        (first segments)
        (join-vectors segments 'octets))))

(defun join-vectors (vectors type)
  "Concatenate VECTORS into one new vector of TYPE (OCTETS or STRING),
however many there are."
  (let* ((total (reduce #'+ vectors :key #'length))
         (result (if (eq type 'string)
                     (make-string total)
                     (make-array total :element-type '(unsigned-byte 8))))
         (start 0))
    (dolist (vector vectors result)
      (replace result vector :start1 start)
      (incf start (length vector)))))

(defun parse-graphics-command (payload &key (decode-data t))
  "Parse a Kitty graphics APC payload or complete ESC _ G ... ESC \\ sequence.
Without DECODE-DATA only the controls are read."
  (multiple-value-bind (controls encoded) (payload-parts payload :data decode-data)
    (%make-graphics-command
     :controls controls
     :%data (if (and decode-data (plusp (length encoded)))
                (decode-base64-parts (list encoded))
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
  (encoded-length 0 :type (integer 0 *))
  ;; Plain pixels are decoded into RGBA as their chunks arrive (see
  ;; DIRECT-RGBA-SIZE): the RGBA, whether the payload is RGB, the payload
  ;; octets expected and decoded so far, and the base64 digits (up to
  ;; three, BITS holding their values) the last chunk ended with.
  (pixels nil :type (or null octets))
  (rgb-p nil)
  (expected 0 :type fixnum)
  (fill 0 :type fixnum)
  (bits 0 :type fixnum)
  (digits 0 :type fixnum)
  ;; An error in the payload, signalled when the last chunk arrives.
  (failure nil))

(defun graphics-decoder-reset (decoder)
  "Discard any partial chunked transfer and return DECODER."
  (setf (graphics-decoder-first-controls decoder) nil
        (graphics-decoder-encoded-parts decoder) '()
        (graphics-decoder-encoded-length decoder) 0
        (graphics-decoder-pixels decoder) nil
        (graphics-decoder-fill decoder) 0
        (graphics-decoder-bits decoder) 0
        (graphics-decoder-digits decoder) 0
        (graphics-decoder-failure decoder) nil)
  decoder)

(defun graphics-decoder-pending-p (decoder)
  "True while DECODER holds a partial chunked transfer."
  (and (graphics-decoder-first-controls decoder) t))

(defun graphics-decoder-abort (decoder)
  "Abort a partial transfer, as a delete command must. True if one was pending."
  (prog1 (graphics-decoder-pending-p decoder)
    (graphics-decoder-reset decoder)))

(defun payload-octets (payload)
  "Where PAYLOAD's body (without ESC _ G ... ESC \\ or a leading G) lies, as
octets, start and end.  An octet vector displaced to a simple one is read in
place."
  (multiple-value-bind (octets start end)
      (etypecase payload
        ((simple-array (unsigned-byte 8) (*)) (values payload 0 (length payload)))
        ((array (unsigned-byte 8) (*))
         (multiple-value-bind (target offset) (array-displacement payload)
           (if (typep target 'octets)
               (values target offset (+ offset (length payload)))
               (values (coerce payload 'octets) 0 (length payload)))))
        (string (let ((octets (map 'octets (lambda (char) (logand #xff (char-code char)))
                                   payload)))
                  (values octets 0 (length octets)))))
    (let ((length (- end start)))
      (cond
        ((and (>= length 5) (= (aref octets start) 27) (= (aref octets (+ start 1)) 95)
              (= (aref octets (+ start 2)) 71)
              (= (aref octets (- end 2)) 27) (= (aref octets (1- end)) 92))
         (values octets (+ start 3) (- end 2)))
        ((and (plusp length) (= (aref octets start) 71))
         (values octets (1+ start) end))
        (t (values octets start end))))))

(defun payload-region (payload)
  "The controls of PAYLOAD, and where its base64 data lies: octets, start
and end."
  (multiple-value-bind (octets start end) (payload-octets payload)
    (let ((separator (position 59 octets :start start :end end)))
      (values (parse-controls (ascii-octets-to-string
                               (subseq octets start (or separator end))))
              octets
              (if separator (1+ separator) end)
              end))))

(defun payload-parts (payload &key (data t))
  "The controls of PAYLOAD and, with DATA, its base64 data as octets."
  (multiple-value-bind (controls octets start end) (payload-region payload)
    (values controls
            (if data
                (subseq octets start end)
                (make-array 0 :element-type '(unsigned-byte 8))))))

(defparameter +base64-values+
  (let ((table (make-array 256 :element-type '(signed-byte 8) :initial-element -1)))
    (loop for char across "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
          for value from 0
          do (setf (aref table (char-code char)) value))
    (setf (aref table (char-code #\=)) -2)
    table))

(defun decode-base64-parts (parts)
  "Decode base64 split across the octet vectors PARTS, at any boundaries.
Padding may be missing, and separately padded pieces may be joined."
  (let* ((total (reduce #'+ parts :key #'length))
         ;; What the digits before the closing padding hold, so that well
         ;; formed input fills OUT exactly.
         (padding (loop for part in (reverse parts)
                        for trailing = (or (position 61 part :from-end t :test #'/=)
                                           -1)
                        sum (- (length part) trailing 1)
                        while (minusp trailing)))
         (out (make-array (floor (* 3 (- total padding)) 4)
                          :element-type '(unsigned-byte 8)))
         (fill 0)
         (bits 0)
         (count 0)
         (table +base64-values+))
    (declare (type (simple-array (unsigned-byte 8) (*)) out)
             (type fixnum fill bits count)
             (type (simple-array (signed-byte 8) (256)) table))
    (flet ((finish-group ()
             ;; Emit what a quartet of COUNT digits holds; a lone digit holds
             ;; nothing whole and is malformed.
             (case count
               (1 (protocol-error "Invalid base64 image payload"))
               (2 (setf (aref out fill) (ldb (byte 8 4) bits)) (incf fill 1))
               (3 (setf (aref out fill) (ldb (byte 8 10) bits)
                        (aref out (1+ fill)) (ldb (byte 8 2) bits))
                (incf fill 2)))
             (setf bits 0 count 0)))
      (dolist (part parts)
        (let ((part (coerce part 'octets))
              (index 0))
          (declare (type (simple-array (unsigned-byte 8) (*)) part)
                   (type fixnum index))
          (loop while (< index (length part))
                do (let ((a 0) (b 0) (c 0) (d 0))
                     (declare (type (signed-byte 8) a b c d))
                     (if (and (zerop count)
                              (<= (+ index 4) (length part))
                              (progn (setf a (aref table (aref part index))
                                           b (aref table (aref part (+ index 1)))
                                           c (aref table (aref part (+ index 2)))
                                           d (aref table (aref part (+ index 3))))
                                     ;; Negative when any is not a digit.
                                     (>= (logior a b c d) 0)))
                         ;; A whole quartet of digits, the common case.
                         (let ((group (logior (ash a 18) (ash b 12) (ash c 6) d)))
                           (setf (aref out fill) (ldb (byte 8 16) group)
                                 (aref out (+ fill 1)) (ldb (byte 8 8) group)
                                 (aref out (+ fill 2)) (ldb (byte 8 0) group))
                           (incf fill 3)
                           (incf index 4))
                         (let ((value (aref table (aref part index))))
                           (cond
                             ((>= value 0)
                              (setf bits (logior (ash (logand bits #x3ffff) 6) value))
                              (incf count)
                              (when (= count 4)
                                (setf (aref out fill) (ldb (byte 8 16) bits)
                                      (aref out (+ fill 1)) (ldb (byte 8 8) bits)
                                      (aref out (+ fill 2)) (ldb (byte 8 0) bits))
                                (incf fill 3)
                                (setf bits 0 count 0)))
                             ((= value -2) (finish-group))
                             (t (protocol-error "Invalid base64 image payload")))
                           (incf index)))))))
      (finish-group))
    (if (= fill (length out)) out (subseq out 0 fill))))

(defun direct-rgba-size (controls)
  "When a transfer with CONTROLS sends plain RGB or RGBA pixels of a given
size, directly and uncompressed, the payload octets they take and whether
they are RGB; otherwise NIL.  Their chunks are decoded into RGBA as they
arrive, instead of being kept until the last one."
  (flet ((value (key default)
           (let ((entry (assoc key controls :test #'char=)))
             (if entry (cdr entry) default)))
         (natural (text)
           (and text (plusp (length text)) (every #'digit-char-p text)
                (parse-integer text))))
    (let ((format (value #\f "32"))
          (width (natural (value #\s nil)))
          (height (natural (value #\v nil))))
      (when (and (member (value #\a "t") '("t" "T" "q" "f") :test #'string=)
                 (member format '("24" "32") :test #'string=)
                 (string= (value #\t "d") "d")
                 (not (equal (value #\o nil) "z"))
                 width height (plusp width) (plusp height)
                 (<= (* width height 4) +safety-limit+))
        (let ((rgb-p (string= format "24")))
          (values (* width height (if rgb-p 3 4)) rgb-p))))))

(defun decode-direct (decoder octets start end &optional finish)
  "Decode the base64 in OCTETS from START to END into DECODER's pixels;
with FINISH, also the digits left over at the end of the payload.  Octets
beyond the image are counted, not kept.  An error is kept as the
decoder's failure."
  (declare (type octets octets) (type fixnum start end) (optimize speed))
  (let ((pixels (graphics-decoder-pixels decoder))
        (rgb-p (graphics-decoder-rgb-p decoder))
        (expected (graphics-decoder-expected decoder))
        (fill (graphics-decoder-fill decoder))
        (bits (graphics-decoder-bits decoder))
        (digits (graphics-decoder-digits decoder))
        (table +base64-values+)
        (index start))
    (declare (type octets pixels) (type fixnum expected fill bits digits index)
             (type (simple-array (signed-byte 8) (256)) table))
    (block decode
      (labels ((fail ()
                 (setf (graphics-decoder-failure decoder)
                       (make-condition 'graphics-protocol-error
                                       :message "Invalid base64 image payload"))
                 (return-from decode))
               (emit (octet)
                 (when (< fill expected)
                   (let ((target (if rgb-p (+ fill (floor fill 3)) fill)))
                     (setf (aref pixels target) octet)
                     (when (and rgb-p (= 2 (mod fill 3)))
                       (setf (aref pixels (1+ target)) 255))))
                 (incf fill))
               (finish-group ()
                 ;; A lone digit holds nothing whole and is malformed.
                 (case digits
                   (1 (fail))
                   (2 (emit (ldb (byte 8 4) bits)))
                   (3 (emit (ldb (byte 8 10) bits))
                    (emit (ldb (byte 8 2) bits))))
                 (setf bits 0 digits 0)))
        (loop while (< index end)
              do (let ((a 0) (b 0) (c 0) (d 0))
                   (declare (type (signed-byte 8) a b c d))
                   (if (and (zerop digits)
                            (<= (+ index 4) end)
                            (progn (setf a (aref table (aref octets index))
                                         b (aref table (aref octets (+ index 1)))
                                         c (aref table (aref octets (+ index 2)))
                                         d (aref table (aref octets (+ index 3))))
                                   ;; Negative when any is not a digit.
                                   (>= (logior a b c d) 0)))
                       ;; A whole quartet: three octets, one RGB pixel.
                       (let ((group (logior (ash a 18) (ash b 12) (ash c 6) d)))
                         (cond
                           ((and rgb-p (zerop (mod fill 3)) (<= (+ fill 3) expected))
                            (let ((target (* 4 (floor fill 3))))
                              (setf (aref pixels target) (ldb (byte 8 16) group)
                                    (aref pixels (+ target 1)) (ldb (byte 8 8) group)
                                    (aref pixels (+ target 2)) (ldb (byte 8 0) group)
                                    (aref pixels (+ target 3)) 255))
                            (incf fill 3))
                           ((and (not rgb-p) (<= (+ fill 3) expected))
                            (setf (aref pixels fill) (ldb (byte 8 16) group)
                                  (aref pixels (+ fill 1)) (ldb (byte 8 8) group)
                                  (aref pixels (+ fill 2)) (ldb (byte 8 0) group))
                            (incf fill 3))
                           (t (emit (ldb (byte 8 16) group))
                              (emit (ldb (byte 8 8) group))
                              (emit (ldb (byte 8 0) group))))
                         (incf index 4))
                       (let ((value (aref table (aref octets index))))
                         (cond
                           ((>= value 0)
                            (setf bits (logior (ash (logand bits #x3ffff) 6) value))
                            (incf digits)
                            (when (= digits 4)
                              (emit (ldb (byte 8 16) bits))
                              (emit (ldb (byte 8 8) bits))
                              (emit (ldb (byte 8 0) bits))
                              (setf bits 0 digits 0)))
                           ((= value -2) (finish-group))
                           (t (fail)))
                         (incf index)))))
        (when finish
          (finish-group))))
    (setf (graphics-decoder-fill decoder) fill
          (graphics-decoder-bits decoder) bits
          (graphics-decoder-digits decoder) digits)
    decoder))

(defun graphics-decoder-feed (decoder payload)
  "Feed one complete APC command. Return a command when all chunks arrive.

Base64 is joined before decoding, so callers need not align input chunks to
base64 quartets. Continuation chunks keep the first chunk's controls; only
their m and a q (the last one wins) are used, other keys are ignored. A
protocol error resets the partial transfer.  PAYLOAD may be reused once
this returns."
  (handler-case
      (multiple-value-bind (controls octets start end) (payload-region payload)
        (let* ((continuing (graphics-decoder-first-controls decoder))
               (quiet (assoc #\q controls :test #'char=))
               (more (equal "1" (cdr (assoc #\m controls :test #'char=)))))
          (setf (graphics-decoder-first-controls decoder)
                (cond ((not continuing) controls)
                      (quiet (append (remove #\q continuing :key #'car :test #'char=)
                                     (list quiet)))
                      (t continuing)))
          ;; Each chunk costs something, so empty ones cannot pile up.
          (incf (graphics-decoder-encoded-length decoder) (+ 64 (- end start)))
          (when (> (graphics-decoder-encoded-length decoder)
                   (graphics-decoder-max-encoded-bytes decoder))
            (protocol-error-with-code
             "EFBIG" "Kitty graphics transfer exceeds the configured quota"))
          (unless continuing
            (multiple-value-bind (size rgb-p) (direct-rgba-size controls)
              (when size
                (setf (graphics-decoder-pixels decoder)
                      (make-array (if rgb-p (* 4 (floor size 3)) size)
                                  :element-type '(unsigned-byte 8))
                      (graphics-decoder-rgb-p decoder) rgb-p
                      (graphics-decoder-expected decoder) size))))
          (cond ((null (graphics-decoder-pixels decoder))
                 (push (subseq octets start end) (graphics-decoder-encoded-parts decoder)))
                ((null (graphics-decoder-failure decoder))
                 (decode-direct decoder octets start end (not more))))
          (if more
              nil
              (let* ((controls (remove #\m (graphics-decoder-first-controls decoder)
                                       :key #'car :test #'char=))
                     (command
                       (cond ((graphics-decoder-failure decoder)
                              (error (graphics-decoder-failure decoder)))
                             ((graphics-decoder-pixels decoder)
                              (%make-graphics-command
                               :controls controls
                               :pixels (graphics-decoder-pixels decoder)
                               :payload-length (graphics-decoder-fill decoder)))
                             (t
                              (%make-graphics-command
                               :controls controls
                               :%data (decode-base64-parts
                                       (nreverse (graphics-decoder-encoded-parts decoder))))))))
                (graphics-decoder-reset decoder)
                command))))
    (error (condition)
      (graphics-decoder-reset decoder)
      (error condition))))


;;;; Files.  Every failure to read one is reported identically, so that a
;;;; remote or sandboxed client learns nothing about files it cannot read.

(defparameter *sensitive-directories* '("/proc/" "/sys/" "/dev/")
  "Trees image files are never read from, except temporary /dev/shm/.")

(defun path-under-p (path directory)
  (let ((directory (if (char= #\/ (char directory (1- (length directory))))
                       directory
                       (concatenate 'string directory "/"))))
    (and (> (length path) (length directory))
         (string= directory path :end2 (length directory)))))

(defun sensitive-path-p (path)
  (and (some (lambda (directory) (path-under-p path directory))
             *sensitive-directories*)
       (not (path-under-p path "/dev/shm/"))))

(defun resolve-path (path)
  "The native truename of PATH, following symlinks, or NIL."
  (ignore-errors
   (let ((truename (truename (uiop:parse-native-namestring path))))
     (and (pathname-name truename) (uiop:native-namestring truename)))))

(defun temporary-directories ()
  (let ((directories (append '("/tmp" "/dev/shm" "/var/tmp")
                             (let ((tmpdir (uiop:getenv "TMPDIR")))
                               (and tmpdir (plusp (length tmpdir)) (list tmpdir)))
                             #+darwin '("/private/tmp" "/private/var/folders"
                                        "/var/folders"))))
    (remove-duplicates
     (append directories
             (loop for directory in directories
                   for truename = (ignore-errors
                                   (truename (uiop:ensure-directory-pathname
                                              (uiop:parse-native-namestring directory))))
                   when truename collect (uiop:native-namestring truename)))
     :test #'string=)))

(defun temporary-path-p (resolved)
  "True if RESOLVED may be read and deleted as a t=t temporary file."
  (and (search "tty-graphics-protocol" resolved)
       (some (lambda (directory) (path-under-p resolved directory))
             (temporary-directories))))

(defun regular-file-p (resolved)
  #+sbcl (ignore-errors
          (= sb-posix:s-ifreg
             (logand sb-posix:s-ifmt (sb-posix:stat-mode (sb-posix:stat resolved)))))
  #-sbcl (and resolved t))

(defun payload-path (data)
  (let ((path (map 'string #'code-char data)))
    (when (or (zerop (length path)) (find (code-char 0) path))
      (file-read-error))
    path))

(defun read-file-range (path offset size temporary-p)
  "Read SIZE octets (zero: the rest) at OFFSET of the regular file at PATH.

With TEMPORARY-P the file must be a known temporary file, deleted after reading."
  (when (> size +safety-limit+)
    (protocol-error-with-code "EFBIG" "Image file range exceeds the safety limit"))
  ;; Check the path before opening it and again once symlinks are resolved.
  (let ((resolved (and (not (sensitive-path-p path)) (resolve-path path))))
    (unless (and resolved (not (sensitive-path-p resolved)) (regular-file-p resolved)
                 (or (not temporary-p) (temporary-path-p resolved)))
      (file-read-error))
    (unwind-protect
         (handler-case
             (with-open-file (stream (uiop:parse-native-namestring resolved)
                                     :direction :input :element-type '(unsigned-byte 8))
               (let* ((length (file-length stream))
                      (count (if (plusp size) size (- length offset))))
                 (unless (and (<= 0 offset length) (<= 0 count (- length offset))
                              (<= count +safety-limit+))
                   (file-read-error))
                 (file-position stream offset)
                 (let ((result (make-array count :element-type '(unsigned-byte 8))))
                   (unless (= count (read-sequence result stream))
                     (file-read-error))
                   result)))
           (error () (file-read-error)))
      (when temporary-p
        (ignore-errors (delete-file (uiop:parse-native-namestring resolved)))))))

(defun format-octets-per-pixel (format)
  (case format (8 1) (16 2) (24 3) (32 4)))

(defun graphics-command-pixel-data (command)
  "Return the transmitted image bytes after resolving medium and compression.

All media are supported: direct data, regular files, temporary files (only in
a known temporary directory, with tty-graphics-protocol in the path) and POSIX
shared memory. File failures signal EBADF \"Failed to read image file\"."
  (let* ((medium (graphics-command-value command #\t "d"))
         (offset (graphics-command-integer command #\O 0))
         (size (graphics-command-integer command #\S 0))
         (data (graphics-command-data command))
         (raw (cond ((string= medium "d") data)
                    ((or (string= medium "f") (string= medium "t"))
                     (read-file-range (payload-path data) offset size
                                      (string= medium "t")))
                    ((string= medium "s")
                     (read-shared-memory (map 'string #'code-char data) offset size))
                    (t (protocol-error "Unknown transmission medium: ~a" medium)))))
    (if (string= (graphics-command-value command #\o "") "z")
        (let* ((format (graphics-command-integer command #\f 32))
               (width (graphics-command-integer command #\s 0))
               (height (graphics-command-integer command #\v 0))
               (octets-per-pixel (format-octets-per-pixel format)))
          (cond ((= format 100)
                 ;; S is the size of the PNG data when PNG is compressed.
                 (inflate-octets-growing raw (if (string= medium "d") size 0)))
                ((and octets-per-pixel (plusp width) (plusp height))
                 (when (> (* width height 4) +safety-limit+)
                   (protocol-error-with-code "EFBIG" "Refusing image dimensions ~dx~d"
                                             width height))
                 (inflate-octets raw (* width height octets-per-pixel)))
                (t (protocol-error
                    "Compressed data needs f=100, or f=8/16/24/32 and dimensions"))))
        raw)))

(defun xterm-color (index)
  "The xterm 256-color table entry for INDEX as (values r g b)."
  (cond ((< index 16)
         (let ((rgb (aref #(#x000000 #xcd0000 #x00cd00 #xcdcd00 #x0000ee #xcd00cd
                            #x00cdcd #xe5e5e5 #x7f7f7f #xff0000 #x00ff00 #xffff00
                            #x5c5cff #xff00ff #x00ffff #xffffff)
                          index)))
           (values (ldb (byte 8 16) rgb) (ldb (byte 8 8) rgb) (ldb (byte 8 0) rgb))))
        ((< index 232)
         (flet ((level (n) (if (zerop n) 0 (+ 55 (* 40 n)))))
           (let ((n (- index 16)))
             (values (level (floor n 36)) (level (mod (floor n 6) 6)) (level (mod n 6))))))
        (t (let ((gray (+ 8 (* 10 (- index 232))))) (values gray gray gray)))))

(defun default-palette (index)
  "Colors for indexed images: xterm colors 0-247, then kitty's defaults for
background, foreground, selection, cursor and URL colors at 248-255."
  (if (< index 248)
      (xterm-color index)
      (let ((rgb (aref #(#x000000 #xdddddd #xfffacd #x000000
                         #xcccccc #x111111 #x0087bd #xdddddd)
                       (- index 248))))
        (values (ldb (byte 8 16) rgb) (ldb (byte 8 8) rgb) (ldb (byte 8 0) rgb)))))

(defun rgb-to-rgba (data pixels)
  "PIXELS pixels of RGB DATA as opaque RGBA."
  (declare (type octets data) (type fixnum pixels) (optimize speed))
  (let ((rgba (make-array (* 4 pixels) :element-type '(unsigned-byte 8))))
    (when (< (length data) (* 3 pixels))
      (error "Too little RGB data for ~d pixels" pixels))
    (loop for source of-type fixnum from 0 by 3
          for target of-type fixnum from 0 below (* 4 pixels) by 4
          do (setf (aref rgba target) (aref data source)
                   (aref rgba (+ target 1)) (aref data (+ source 1))
                   (aref rgba (+ target 2)) (aref data (+ source 2))
                   (aref rgba (+ target 3)) 255))
    rgba))

(defun graphics-command-rgba (command &key (palette #'default-palette))
  "Return decoded RGBA octets, width and height for a transmitted image.

PALETTE maps an index 0-255 to (values r g b) for f=8 and f=16 images."
  (let* ((format (graphics-command-integer command #\f 32))
         (width (graphics-command-integer command #\s 0))
         (height (graphics-command-integer command #\v 0))
         ;; Decoded into RGBA as they arrived, or still to decode.
         (pixels (graphics-command-pixels command))
         (data (if pixels
                   (make-array 0 :element-type '(unsigned-byte 8))
                   (graphics-command-pixel-data command)))
         (length (if pixels (graphics-command-payload-length command) (length data))))
    (when (= format 100)
      (return-from graphics-command-rgba (decode-png data)))
    (let ((octets-per-pixel (format-octets-per-pixel format)))
      (unless (and octets-per-pixel (plusp width) (plusp height))
        (protocol-error "Command needs f=8/16/24/32 with dimensions, or f=100 PNG"))
      (let ((expected (* width height octets-per-pixel)))
        (when (> (* width height 4) +safety-limit+)
          (protocol-error-with-code "EFBIG" "Refusing image dimensions ~dx~d" width height))
        (when (< length expected)
          (protocol-error-with-code "ENODATA" "Pixel data is too short for ~dx~d f=~d"
                                    width height format))
        (when (> length expected)
          (protocol-error "Pixel data is too long for ~dx~d f=~d" width height format)))
      (case (if pixels :decoded format)
        (:decoded (values pixels width height))
        (32 (values data width height))
        (24 (values (rgb-to-rgba (coerce data 'octets) (* width height)) width height))
        (t
         (let ((rgba (make-array (* width height 4) :element-type '(unsigned-byte 8)))
               (colors (make-array 256 :initial-element nil)))
           (flet ((color (index)
                    (or (aref colors index)
                        (setf (aref colors index)
                              (multiple-value-list (funcall palette index))))))
             (loop for source from 0 below (length data) by octets-per-pixel
                   for target from 0 by 4
                   do (destructuring-bind (r g b) (color (aref data source))
                        (setf (aref rgba target) r
                              (aref rgba (+ target 1)) g
                              (aref rgba (+ target 2)) b
                              (aref rgba (+ target 3))
                              (if (= format 16) (aref data (1+ source)) 255)))))
           (values rgba width height)))))))
