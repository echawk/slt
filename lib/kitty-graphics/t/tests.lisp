(defpackage #:kitty-graphics/tests
  (:use #:cl #:kitty-graphics)
  (:export #:run-tests))

(in-package #:kitty-graphics/tests)

(defmacro is (form)
  `(assert ,form))

(defun octets (&rest values)
  (make-array (length values) :element-type '(unsigned-byte 8)
                              :initial-contents values))

(defun error-code (thunk)
  "The response code of the graphics-protocol-error THUNK signals, or NIL."
  (handler-case (progn (funcall thunk) nil)
    (graphics-protocol-error (condition)
      (format nil "~a:~a" (graphics-protocol-error-code condition)
              (graphics-protocol-error-message condition)))))

(defmacro code-of (form)
  `(let ((response (error-code (lambda () ,form))))
     (and response (subseq response 0 (position #\: response)))))

(defun file-command (medium path &rest controls)
  (parse-graphics-command
   (encode-graphics-command (list* (cons #\t medium) controls)
                            (map '(vector (unsigned-byte 8)) #'char-code
                                 (uiop:native-namestring path)))))

(defun write-octets (path octets)
  (with-open-file (stream path :direction :output :if-exists :supersede
                               :element-type '(unsigned-byte 8))
    (write-sequence octets stream))
  path)

(defun temporary-path (name)
  (merge-pathnames name (uiop:temporary-directory)))

(defun run-file-tests ()
  (let ((rgb '((#\f . "24") (#\s . "1") (#\v . "1")))
        (inside (write-octets (temporary-path "tty-graphics-protocol-kg-test.bin")
                              (octets 9 1 2 3)))
        (plain (write-octets (temporary-path "kg-test-plain.bin") (octets 1 2 3)))
        (outside (write-octets (asdf:system-relative-pathname
                                :kitty-graphics "t/tty-graphics-protocol-outside.bin")
                               (octets 1 2 3)))
        (link (temporary-path "tty-graphics-protocol-kg-link.bin")))
    (unwind-protect
         (progn
           ;; t=f reads a range of a regular file and leaves it in place.
           (is (equalp (octets 1 2 3)
                       (graphics-command-pixel-data
                        (apply #'file-command "f" inside '(#\O . "1") rgb))))
           (is (probe-file inside))
           ;; Every file failure is the same EBADF response.
           (dolist (command (list (file-command "f" (temporary-path "kg-missing.bin"))
                                  (file-command "f" (asdf:system-relative-pathname
                                                     :kitty-graphics "t/"))
                                  (file-command "f" "/dev/null")
                                  (file-command "f" inside '(#\S . "9"))
                                  (file-command "f" inside '(#\O . "5"))))
             (is (equal "EBADF:Failed to read image file"
                        (error-code (lambda () (graphics-command-pixel-data command))))))
           ;; t=t needs the magic string and a temporary directory, after
           ;; resolving symlinks, and refused files are not deleted.
           (is (equal "EBADF" (code-of (graphics-command-pixel-data
                                        (file-command "t" plain)))))
           (is (probe-file plain))
           (is (equal "EBADF" (code-of (graphics-command-pixel-data
                                        (file-command "t" outside)))))
           (is (probe-file outside))
           #+sbcl
           (progn
             (sb-posix:symlink (uiop:native-namestring outside)
                               (uiop:native-namestring link))
             (is (equal "EBADF" (code-of (graphics-command-pixel-data
                                          (file-command "t" link)))))
             (is (probe-file outside))
             (delete-file link)
             (sb-posix:symlink "/dev/null" (uiop:native-namestring link))
             (is (equal "EBADF" (code-of (graphics-command-pixel-data
                                          (file-command "f" link)))))
             (delete-file link))
           (multiple-value-bind (rgba width height)
               (graphics-command-rgba (apply #'file-command "t" inside '(#\O . "1") rgb))
             (is (and (= 1 width height) (equalp rgba (octets 1 2 3 255)))))
           (is (not (probe-file inside))))
      (dolist (path (list inside plain outside link))
        (ignore-errors (delete-file path))))))

#+(and sbcl (or darwin linux))
(defun run-shared-memory-tests ()
  (let ((name (format nil "kg-test-~d" (random 1000000)))
        (flags (logior sb-posix:o-creat sb-posix:o-rdwr)))
    ;; A name without a leading slash, and no S: the size comes from fstat.
    (let ((fd (kitty-graphics::posix-shm-open name flags #o600)))
      (when (minusp fd)
        (return-from run-shared-memory-tests))
      (unwind-protect
           (let ((size (kitty-graphics::posix-page-size)))
             (sb-posix:ftruncate fd size)
             (let ((memory (kitty-graphics::posix-mmap (cffi:null-pointer) size 3 1 fd 0)))
               (dotimes (index size)
                 (setf (cffi:mem-aref memory :uint8 index) (mod index 256)))
               (kitty-graphics::posix-munmap memory size)))
        (kitty-graphics::posix-close fd)))
    (let ((data (graphics-command-pixel-data
                 (parse-graphics-command
                  (encode-graphics-command
                   '((#\t . "s") (#\O . "2"))
                   (map '(vector (unsigned-byte 8)) #'char-code name))))))
      (is (<= 2 (length data)))
      (is (equalp (octets 2 3 4) (subseq data 0 3))))
    ;; The object was unlinked after opening.
    (is (equal "EBADF" (code-of (graphics-command-pixel-data
                                 (parse-graphics-command
                                  (encode-graphics-command
                                   '((#\t . "s") (#\S . "4"))
                                   (map '(vector (unsigned-byte 8)) #'char-code name)))))))))

(defun run-tests ()
  (let* ((data (octets 255 0 0 0 255 0))
         (encoded (encode-graphics-command
                   '((#\a . "T") (#\f . "24") (#\s . "2") (#\v . "1")) data))
         (command (parse-graphics-command encoded)))
    (is (char= #\T (graphics-command-action command)))
    (is (= 24 (graphics-command-integer command #\f)))
    (is (equalp data (graphics-command-data command)))
    (multiple-value-bind (rgba width height) (graphics-command-rgba command)
      (is (= width 2))
      (is (= height 1))
      (is (equalp rgba (octets 255 0 0 255 0 255 0 255)))))
  (let ((decoder (make-graphics-decoder)))
    (is (null (graphics-decoder-feed decoder "Ga=T,f=32,s=1,v=1,m=1;/w")))
    (let ((command (graphics-decoder-feed decoder "Gm=0;A=")))
      (is command)
      (is (equalp (octets 255 0) (graphics-command-data command)))
      (is (null (graphics-command-value command #\m)))))
  (let ((response (encode-graphics-response '((#\i . "42")) "OK")))
    (is (search "_Gi=42;OK" response)))
  (let* ((compressed (octets 120 156 227 18 145 251 15 0 1 163 1 60))
         (command (parse-graphics-command
                   (encode-graphics-command
                    '((#\f . "32") (#\s . "1") (#\v . "1") (#\o . "z"))
                    compressed))))
    (multiple-value-bind (rgba width height) (graphics-command-rgba command)
      (is (= 1 width))
      (is (= 1 height))
      (is (equalp rgba (octets 10 20 30 255)))))
  (let* ((png (octets
               137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 1
               0 0 0 1 8 6 0 0 0 31 21 196 137 0 0 0 13 73 68 65 84
               120 156 99 224 18 145 251 15 0 1 164 1 60 147 139 14 183
               0 0 0 0 73 69 78 68 174 66 96 130))
         (command (parse-graphics-command
                   (encode-graphics-command '((#\f . "100")) png))))
    (multiple-value-bind (rgba width height) (graphics-command-rgba command)
      (is (= 1 width))
      (is (= 1 height))
      (is (= 4 (length rgba)))))
  ;; Base64 without padding, and separately padded chunks joined together.
  (is (equalp (octets 1 2 3 4 5)
              (graphics-command-data (parse-graphics-command "Gf=32;AQIDBAU"))))
  (is (equalp (octets 255 255)
              (graphics-command-data (parse-graphics-command "G;/w==/w"))))
  (is (equal "EINVAL" (code-of (parse-graphics-command "G;AQ*DBAU"))))
  (is (equal "EINVAL" (code-of (parse-graphics-command "G;AQIDB"))))
  ;; Continuation chunks may carry q, the last one wins, and other keys are
  ;; ignored rather than rejected.
  (let ((decoder (make-graphics-decoder)))
    (is (not (graphics-decoder-pending-p decoder)))
    (is (null (graphics-decoder-feed decoder "Ga=T,q=1,f=32,s=1,v=1,m=1;/w")))
    (is (graphics-decoder-pending-p decoder))
    (is (null (graphics-decoder-feed decoder "Gm=1,q=2,a=f,s=9;AA")))
    (let ((command (graphics-decoder-feed decoder "Gm=0;AA")))
      (is (not (graphics-decoder-pending-p decoder)))
      (is (equal "2" (graphics-command-value command #\q)))
      (is (equal "1" (graphics-command-value command #\s)))
      (is (char= #\T (graphics-command-action command)))
      (is (equalp (octets 255 0 0 0) (graphics-command-data command)))))
  (let ((decoder (make-graphics-decoder)))
    (graphics-decoder-feed decoder "Ga=T,q=1,m=1;AAAA")
    (is (equal "1" (graphics-command-value
                    (graphics-decoder-feed decoder "Gm=0;AAAA") #\q))))
  ;; A delete command aborts a partial upload.
  (let ((decoder (make-graphics-decoder)))
    (graphics-decoder-feed decoder "Ga=T,f=24,s=1,v=1,m=1;AAAA")
    (is (graphics-decoder-abort decoder))
    (is (not (graphics-decoder-pending-p decoder)))
    (is (not (graphics-decoder-abort decoder)))
    (is (equalp (octets 0 0 0) (graphics-command-data
                                (graphics-decoder-feed decoder "Ga=d;AAAA")))))
  (let ((decoder (make-graphics-decoder :max-encoded-bytes 4)))
    (is (equal "EFBIG" (code-of (graphics-decoder-feed decoder "Gm=1;AAAAAAAA"))))
    (is (not (graphics-decoder-pending-p decoder))))
  ;; Error codes for raw data lengths.
  (flet ((rgb (data)
           (graphics-command-rgba
            (parse-graphics-command
             (encode-graphics-command '((#\f . "24") (#\s . "2") (#\v . "1")) data)))))
    (is (equal "ENODATA" (code-of (rgb (octets 1 2 3 4 5)))))
    (is (equal "EINVAL" (code-of (rgb (octets 1 2 3 4 5 6 7)))))
    (is (equal "EINVAL" (code-of (graphics-command-rgba
                                  (parse-graphics-command "Gf=24;AAAA"))))))
  ;; Compressed PNG, with and without S.
  (let ((compressed (octets 120 156 235 12 240 115 231 229 146 226 98 96 96 224 245
                            244 112 9 2 210 140 32 204 193 6 36 229 69 143 116 130 37
                            92 28 67 42 230 36 63 16 154 248 155 159 129 113 9 163 205
                            228 110 190 237 64 9 6 79 87 63 151 117 78 9 77 0 183 11
                            15 89)))
    (dolist (controls '(((#\f . "100") (#\o . "z") (#\S . "70"))
                        ((#\f . "100") (#\o . "z"))))
      (multiple-value-bind (rgba width height)
          (graphics-command-rgba
           (parse-graphics-command (encode-graphics-command controls compressed)))
        (is (and (= 1 width height) (= 4 (length rgba)))))))
  ;; Indexed color, with the default and a custom palette, and compressed.
  (let ((f8 (parse-graphics-command
             (encode-graphics-command '((#\f . "8") (#\s . "2") (#\v . "1"))
                                      (octets 1 248))))
        (f16 (parse-graphics-command
              (encode-graphics-command '((#\f . "16") (#\s . "2") (#\v . "1") (#\o . "z"))
                                       (octets 120 156 99 108 248 241 31 0 4 119 2 121)))))
    (is (equalp (octets 205 0 0 255 0 0 0 255) (graphics-command-rgba f8)))
    (is (equalp (octets 1 2 3 128 248 249 250 255)
                (graphics-command-rgba f16 :palette (lambda (index)
                                                      (if (= index 1)
                                                          (values 1 2 3)
                                                          (values index (1+ index)
                                                                  (+ 2 index)))))))
    (is (equal "EINVAL" (code-of (graphics-command-rgba (parse-graphics-command "Gf=8;AA"))))))
  (run-file-tests)
  #+(and sbcl (or darwin linux)) (run-shared-memory-tests)
  t)
