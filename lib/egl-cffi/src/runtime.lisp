(in-package #:egl-cffi.generator)

;;;; Loading EGL and checking its results.  The library is opened by
;;;; LOAD-EGL, not when this is loaded, so a program can carry these
;;;; bindings and run where there is no EGL (macOS).

(cffi:define-foreign-library egl-library
  (:unix (:or "libEGL.so.1" "libEGL.so"))
  (t (:default "libEGL")))

(defparameter +get-proc-address+ "eglGetProcAddress"
  "What looks up the commands the library does not export.")

(defun egl-loaded-p ()
  (cffi:foreign-library-loaded-p 'egl-library))

(defun load-egl ()
  "Open EGL, unless it is open, and load its commands: those the library
exports, then the rest through eglGetProcAddress."
  (unless (egl-loaded-p)
    (cffi:load-foreign-library 'egl-library))
  (let ((index (position +get-proc-address+ *command-names* :test #'string=)))
    (setf (svref *command-pointers* index)
          (or (cffi:foreign-symbol-pointer +get-proc-address+ :library 'egl-library)
              (error "EGL has no ~a" +get-proc-address+))))
  (loop for name across *command-names*
        for index from 0
        do (setf (svref *command-pointers* index)
                 (or (cffi:foreign-symbol-pointer name :library 'egl-library)
                     (cffi:with-foreign-string (c-name name)
                       (egl-cffi:get-proc-address c-name)))))
  t)

(defun command-loaded-p (name)
  "Whether the command NAME (\"eglGetPlatformDisplay\") is loaded."
  (let ((index (position name *command-names* :test #'string=)))
    (and index (not (cffi:null-pointer-p (svref *command-pointers* index))))))

(define-condition egl-error (error)
  ((code :initarg :code :reader egl-error-code)
   (command :initarg :command :initform nil :reader egl-error-command))
  (:report (lambda (condition stream)
             (let ((code (egl-error-code condition)))
               (format stream "~@[~a: ~]~a (#x~x)" (egl-error-command condition)
                       (or (cdr (assoc code *constant-names*)) "unknown error") code)))))

(defun check-egl (result &optional command)
  "RESULT, unless it is false (EGL_FALSE or a null handle): then signal
EGL-ERROR with eglGetError's code, naming COMMAND."
  (if (or (eql result egl-cffi:+false+)
          (and (cffi:pointerp result) (cffi:null-pointer-p result)))
      (error 'egl-error :code (egl-cffi:get-error) :command command)
      result))

(defmacro with-attributes ((var type attributes) &body body)
  "Run BODY with VAR a foreign array of TYPE (:int32 for EGLint, :intptr
for EGLAttrib) holding the list ATTRIBUTES, ended by EGL_NONE."
  (let ((list (gensym "ATTRIBUTES")))
    `(let ((,list (append ,attributes (list egl-cffi:+none+))))
       (cffi:with-foreign-object (,var ,type (length ,list))
         (loop for value in ,list
               for i from 0
               do (setf (cffi:mem-aref ,var ,type i) value))
         ,@body))))
