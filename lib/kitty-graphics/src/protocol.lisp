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

(defun parse-controls (octets start end)
  "The control fields of OCTETS from START to END, k=v,k=v..., as an alist
of (KEY-CHARACTER . VALUE-STRING)."
  (declare (type octets octets) (type fixnum start end) (optimize speed))
  (flet ((text (start end)
           (let ((string (make-string (- end start))))
             (loop for i of-type fixnum from start below end
                   for j of-type fixnum from 0
                   do (setf (schar string j) (code-char (aref octets i))))
             string)))
    (if (= start end)
        '()
        (loop for comma = (position 44 octets :start start :end end)
              for field-end of-type fixnum = (or comma end)
              unless (and (< (+ start 2) field-end) (= 61 (aref octets (1+ start))))
                do (protocol-error "Malformed Kitty graphics control field: ~s"
                                   (text start field-end))
              collect (cons (code-char (aref octets start)) (text (+ start 2) field-end))
              while comma
              do (setf start (1+ comma))))))

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
  ;; DIRECT-RGBA-SIZE), by this base64 decoder.
  (base64 nil :type (or null simd-base64:decoder))
  ;; An error in the payload, signalled when the last chunk arrives.
  (failure nil))

(defun graphics-decoder-reset (decoder)
  "Discard any partial chunked transfer and return DECODER."
  (setf (graphics-decoder-first-controls decoder) nil
        (graphics-decoder-encoded-parts decoder) '()
        (graphics-decoder-encoded-length decoder) 0
        (graphics-decoder-base64 decoder) nil
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
      (values (parse-controls octets start (or separator end))
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

(defun decode-base64-parts (parts)
  "Decode base64 split across the octet vectors PARTS, at any boundaries.
Padding may be missing, and separately padded pieces may be joined."
  (handler-case (simd-base64:decode parts)
    (simd-base64:base64-error ()
      (protocol-error "Invalid base64 image payload"))))

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
with FINISH, also the digits left over at the end of the payload.  An
error is kept as the decoder's failure."
  (let ((base64 (graphics-decoder-base64 decoder)))
    (handler-case
        (progn (simd-base64:decoder-feed base64 octets :start start :end end)
               (when finish
                 (simd-base64:decoder-finish base64)))
      (simd-base64:base64-error ()
        (setf (graphics-decoder-failure decoder)
              (make-condition 'graphics-protocol-error
                              :message "Invalid base64 image payload"))))))

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
                (setf (graphics-decoder-base64 decoder)
                      (simd-base64:make-decoder
                       (make-array (if rgb-p (* 4 (floor size 3)) size)
                                   :element-type '(unsigned-byte 8))
                       :layout (if rgb-p :rgba :octets))))))
          (cond ((null (graphics-decoder-base64 decoder))
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
                             ((graphics-decoder-base64 decoder)
                              (let ((base64 (graphics-decoder-base64 decoder)))
                                (%make-graphics-command
                                 :controls controls
                                 :pixels (simd-base64:decoder-output base64)
                                 :payload-length (simd-base64:decoder-fill base64))))
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
