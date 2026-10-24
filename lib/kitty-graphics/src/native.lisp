(in-package #:kitty-graphics)

;;;; Small, dependency-free bindings used by the wire codec.  zlib is part of
;;;; the Kitty protocol and libpng's stable simplified API keeps PNG decoding
;;;; out of terminal frontends.

(cffi:define-foreign-library libz
  (:darwin (:or "libz.1.dylib" "libz.dylib"))
  (:unix (:or "libz.so.1" "libz.so"))
  (t (:default "z")))

(cffi:define-foreign-library libpng
  (:darwin (:or "libpng16.16.dylib" "libpng16.dylib" "libpng.dylib"))
  (:unix (:or "libpng16.so.16" "libpng16.so" "libpng.so"))
  (t (:default "png")))

(cffi:defcfun (zlib-uncompress "uncompress") :int
  (dest :pointer) (dest-length :pointer) (source :pointer) (source-length :unsigned-long))

;; png_image from png.h.  This is the ABI-stable simplified API introduced in
;; libpng 1.6.  The explicit padding is supplied by CFFI around pointer fields.
(cffi:defcstruct png-image
  (opaque :pointer)
  (version :uint32)
  (width :uint32)
  (height :uint32)
  (format :uint32)
  (flags :uint32)
  (colormap-entries :uint32)
  (warning-or-error :uint32)
  (message :char :count 64))

(cffi:defcfun (png-begin-read-memory "png_image_begin_read_from_memory") :int
  (image (:pointer (:struct png-image))) (memory :pointer) (size :size))
(cffi:defcfun (png-finish-read "png_image_finish_read") :int
  (image (:pointer (:struct png-image))) (background :pointer)
  (buffer :pointer) (row-stride :int32) (colormap :pointer))
(cffi:defcfun (png-image-free "png_image_free") :void
  (image (:pointer (:struct png-image))))

(cffi:defcfun (posix-shm-open "shm_open") :int
  (name :string) (flags :int) (mode :unsigned-int))
(cffi:defcfun (posix-shm-unlink "shm_unlink") :int (name :string))
(cffi:defcfun (posix-close "close") :int (fd :int))
(cffi:defcfun (posix-mmap "mmap") :pointer
  (address :pointer) (length :size) (protection :int) (flags :int)
  (fd :int) (offset :long))
(cffi:defcfun (posix-munmap "munmap") :int (address :pointer) (length :size))
(cffi:defcfun (posix-page-size "getpagesize") :int)

(defun ensure-libz ()
  (cffi:use-foreign-library libz))

(defun ensure-libpng ()
  (cffi:use-foreign-library libpng))

(defconstant +safety-limit+ (* 256 1024 1024)
  "Largest decoded image or transferred payload accepted, in octets.")

(defun %inflate (data capacity)
  "Inflate DATA into CAPACITY octets. Return the octets or :too-small."
  (let ((result (make-array capacity :element-type '(unsigned-byte 8))))
    (cffi:with-pointer-to-vector-data (source data)
      (cffi:with-pointer-to-vector-data (destination result)
        (cffi:with-foreign-object (length :unsigned-long)
          (setf (cffi:mem-ref length :unsigned-long) capacity)
          (let ((status (zlib-uncompress destination length source (length data))))
            (case status
              (0 (subseq result 0 (cffi:mem-ref length :unsigned-long)))
              ;; Z_BUF_ERROR: the output does not fit.
              (-5 :too-small)
              (t (protocol-error "Could not inflate image data (zlib status ~d)"
                                 status)))))))))

(defun inflate-octets (data expected-length)
  "Inflate DATA, which must expand to exactly EXPECTED-LENGTH octets."
  (ensure-libz)
  (let ((result (%inflate data expected-length)))
    (cond ((eq result :too-small)
           (protocol-error "Inflated image data exceeds ~d octets" expected-length))
          ((< (length result) expected-length)
           (protocol-error-with-code "ENODATA" "Inflated image data is shorter than ~d octets"
                                     expected-length))
          (t result))))

(defun inflate-octets-growing (data &optional (initial 0))
  "Inflate DATA of unknown size, doubling the buffer up to the safety limit."
  (ensure-libz)
  (loop for capacity = (min +safety-limit+
                            (max 4096 initial (* 4 (length data))))
          then (min +safety-limit+ (* 2 capacity))
        for result = (%inflate data capacity)
        unless (eq result :too-small)
          return result
        when (= capacity +safety-limit+)
          do (protocol-error-with-code "EFBIG" "Inflated image data exceeds the safety limit")))

(defun png-message (image)
  (cffi:foreign-string-to-lisp
   (cffi:foreign-slot-pointer image '(:struct png-image) 'message)))

(defun decode-png (data)
  (ensure-libpng)
  (cffi:with-foreign-object (image '(:struct png-image))
    (dotimes (i (cffi:foreign-type-size '(:struct png-image)))
      (setf (cffi:mem-aref image :uint8 i) 0))
    (setf (cffi:foreign-slot-value image '(:struct png-image) 'version) 1)
    (unwind-protect
         (cffi:with-pointer-to-vector-data (source data)
           (unless (= 1 (png-begin-read-memory image source (length data)))
             (protocol-error "Invalid PNG image: ~a" (png-message image)))
           (let* ((width (cffi:foreign-slot-value image '(:struct png-image) 'width))
                  (height (cffi:foreign-slot-value image '(:struct png-image) 'height))
                  (size (* width height 4)))
             (unless (and (plusp width) (plusp height) (<= size +safety-limit+))
               (protocol-error-with-code "EFBIG" "Refusing PNG dimensions ~dx~d" width height))
             ;; PNG_FORMAT_RGBA = COLOR | ALPHA.
             (setf (cffi:foreign-slot-value image '(:struct png-image) 'format) 3)
             (let ((rgba (make-array size :element-type '(unsigned-byte 8))))
               (cffi:with-pointer-to-vector-data (output rgba)
                 (unless (= 1 (png-finish-read image (cffi:null-pointer) output 0
                                              (cffi:null-pointer)))
                   (protocol-error "Could not decode PNG image: ~a" (png-message image))))
               (values rgba width height))))
      (png-image-free image))))

(defun shared-memory-size (fd)
  "Size in octets of the object open on FD."
  #+sbcl (sb-posix:stat-size (sb-posix:fstat fd))
  #-sbcl (progn fd (protocol-error "Shared-memory transfers require S=size")))

(defun read-shared-memory (name offset size)
  "Read SIZE octets at OFFSET of shared-memory object NAME, then unlink it.

NAME is passed to shm_open and shm_unlink unmodified, so names without a
leading slash work too. A SIZE of zero reads to the end of the object."
  (unless (and (plusp (length name)) (not (find (code-char 0) name)))
    (protocol-error "Invalid shared-memory object name"))
  (let ((fd (posix-shm-open name 0 0)))
    (when (minusp fd)
      (protocol-error-with-code "EBADF" "Could not open shared-memory object ~s" name))
    ;; The protocol requires unlinking as soon as the object has been opened.
    (ignore-errors (posix-shm-unlink name))
    (unwind-protect
         (let* ((object-size (if (plusp size)
                                 #+sbcl (ignore-errors (shared-memory-size fd))
                                 #-sbcl nil
                                 (shared-memory-size fd)))
                (size (if (plusp size) size (- object-size offset))))
           (when (and object-size (< object-size (+ offset (max size 0))))
             (protocol-error-with-code "ENODATA" "Shared-memory object ~s is too small" name))
           (unless (plusp size)
             (protocol-error-with-code "ENODATA" "Shared-memory object ~s has no data" name))
           (when (> size +safety-limit+)
             (protocol-error-with-code "EFBIG" "Shared-memory image exceeds the safety limit"))
           (let* ((page-size (posix-page-size))
                  (base-offset (* (floor offset page-size) page-size))
                  (delta (- offset base-offset))
                  (mapped-size (+ delta size))
                  (memory (posix-mmap (cffi:null-pointer) mapped-size 1 1 fd base-offset)))
             (when (= (cffi:pointer-address memory) #.(1- (ash 1 64)))
               (protocol-error-with-code "EBADF" "Could not map shared-memory object ~s" name))
             (unwind-protect
                  (let ((result (make-array size :element-type '(unsigned-byte 8))))
                    (dotimes (index size result)
                      (setf (aref result index)
                            (cffi:mem-aref memory :uint8 (+ delta index)))))
               (posix-munmap memory mapped-size))))
      (posix-close fd))))
