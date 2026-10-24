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

(defun inflate-octets (data expected-length)
  (ensure-libz)
  (let ((result (make-array expected-length :element-type '(unsigned-byte 8))))
    (cffi:with-pointer-to-vector-data (source data)
      (cffi:with-pointer-to-vector-data (destination result)
        (cffi:with-foreign-object (length :unsigned-long)
          (setf (cffi:mem-ref length :unsigned-long) expected-length)
          (let ((status (zlib-uncompress destination length source (length data))))
            (unless (and (zerop status)
                         (= expected-length (cffi:mem-ref length :unsigned-long)))
              (protocol-error "Could not inflate image data (zlib status ~d)" status))))))
    result))

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
             (unless (and (plusp width) (plusp height) (<= size (* 512 1024 1024)))
               (protocol-error "Refusing PNG dimensions ~dx~d" width height))
             ;; PNG_FORMAT_RGBA = COLOR | ALPHA.
             (setf (cffi:foreign-slot-value image '(:struct png-image) 'format) 3)
             (let ((rgba (make-array size :element-type '(unsigned-byte 8))))
               (cffi:with-pointer-to-vector-data (output rgba)
                 (unless (= 1 (png-finish-read image (cffi:null-pointer) output 0
                                              (cffi:null-pointer)))
                   (protocol-error "Could not decode PNG image: ~a" (png-message image))))
               (values rgba width height))))
      (png-image-free image))))

(defun read-shared-memory (name offset size)
  (unless (and (plusp size) (plusp (length name)) (char= (char name 0) #\/))
    (protocol-error "Shared-memory transfers require a /name and S=size"))
  (let ((fd (posix-shm-open name 0 0)))
    (when (minusp fd)
      (protocol-error "Could not open shared-memory object ~s" name))
    ;; The protocol requires unlinking as soon as the object has been opened.
    (ignore-errors (posix-shm-unlink name))
    (unwind-protect
         (let* ((page-size (posix-page-size))
                (base-offset (* (floor offset page-size) page-size))
                (delta (- offset base-offset))
                (mapped-size (+ delta size))
                (memory (posix-mmap (cffi:null-pointer) mapped-size 1 1 fd base-offset)))
           (when (= (cffi:pointer-address memory) #.(1- (ash 1 64)))
             (protocol-error "Could not map shared-memory object ~s" name))
           (unwind-protect
                (let ((result (make-array size :element-type '(unsigned-byte 8))))
                  (dotimes (index size result)
                    (setf (aref result index)
                          (cffi:mem-aref memory :uint8 (+ delta index)))))
             (posix-munmap memory mapped-size)))
      (posix-close fd))))
