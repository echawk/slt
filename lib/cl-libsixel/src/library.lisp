(in-package #:cl-libsixel)

(cffi:define-foreign-library libsixel-foreign
  (:darwin (:or "libsixel.1.dylib" "libsixel.dylib"))
  (:unix (:or "libsixel.so.1" "libsixel.so"))
  (:windows "libsixel.dll")
  (t (:default "libsixel")))

(defvar *libsixel-loaded-p* nil)

(defun load-libsixel ()
  "Load the system libsixel shared library."
  (unless *libsixel-loaded-p*
    (cffi:use-foreign-library libsixel-foreign)
    (setf *libsixel-loaded-p* t))
  t)

(defun libsixel-available-p ()
  "Return true when the libsixel shared library can be loaded."
  (or *libsixel-loaded-p*
      (ignore-errors (load-libsixel))))

(define-condition libsixel-error (error)
  ((status :initarg :status :reader libsixel-error-status)
   (message :initarg :message :reader libsixel-error-message))
  (:report (lambda (condition stream)
             (format stream "libsixel error ~d: ~a"
                     (libsixel-error-status condition)
                     (libsixel-error-message condition)))))

(cffi:defcfun ("sixel_allocator_new" %allocator-new) :int
  (allocator :pointer)
  (malloc :pointer)
  (calloc :pointer)
  (realloc :pointer)
  (free :pointer))
(cffi:defcfun ("sixel_allocator_unref" %allocator-unref) :void
  (allocator :pointer))
(cffi:defcfun ("sixel_allocator_free" %allocator-free) :void
  (allocator :pointer) (memory :pointer))
(cffi:defcfun ("sixel_helper_format_error" %format-error) :string
  (status :int))
(cffi:defcfun ("sixel_helper_get_additional_message" %additional-message) :string)
(cffi:defcfun ("sixel_decode_raw" %decode-raw) :int
  (data :pointer)
  (length :int)
  (pixels :pointer)
  (width :pointer)
  (height :pointer)
  (palette :pointer)
  (colors :pointer)
  (allocator :pointer))

(defun status-succeeded-p (status)
  (not (logbitp 12 status)))

(defun check-status (status)
  (unless (status-succeeded-p status)
    (let ((base (%format-error status))
          (additional (%additional-message)))
      (error 'libsixel-error
             :status status
             :message (if (and additional (plusp (length additional)))
                          (format nil "~a (~a)" base additional)
                          base))))
  status)
