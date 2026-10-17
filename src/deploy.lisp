(in-package #:slt)

;;;; Distribution build (system slt/app, built with Deploy by `make`).
;;;; Deploy dumps the executable into bin/ and copies the foreign libraries
;;;; it loaded next to it, opening them from there at startup.  Libraries
;;;; every Mac (or Linux system) already has are excluded here.

(defun define-optional-library (package name &rest initargs)
  "Configure a library that only exists when the build loaded the system
defining it (see scripts/build)."
  (let ((symbol (and (find-package package) (find-symbol name package))))
    (when (and symbol (ignore-errors (cffi::get-foreign-library symbol)))
      (eval `(deploy:define-library ,symbol ,@initargs)))))

(define-optional-library "CFFI" "LIBFFI" :dont-deploy t)
(define-optional-library "OSICAT-POSIX" "LIBRT" :dont-deploy t)

#+darwin
(progn
  (deploy:define-library org.shirakumo.cocoas.cffi:foundation :dont-deploy t)
  (deploy:define-library org.shirakumo.cocoas.cffi:appkit :dont-deploy t)
  (deploy:define-library org.shirakumo.cocoas.cffi:cocoa :dont-deploy t)
  (deploy:define-library metal-framework :dont-deploy t)
  (deploy:define-library quartz-core-framework :dont-deploy t)
  (deploy:define-library core-text-framework :dont-deploy t)
  (deploy:define-library core-graphics-framework :dont-deploy t))

;;; ghostty-vt loads its library by explicit path, which CFFI registers as
;;; an anonymous library (deployed as usual); its named definition has no
;;; path.
(define-optional-library "GHOSTTY-VT.FFI" "LIBGHOSTTY-VT" :dont-deploy t :dont-open t)

;;; The gl backend: fontconfig belongs to the system.  GLFW is deployed.
(define-optional-library "ORG.SHIRAKUMO.FONT-DISCOVERY" "FONTCONFIG" :dont-deploy t)

;;; The engines' C shims resolve their library's symbols at load time, so
;;; they must be opened after it.  Deploy reopens libraries in registration
;;; order, which puts each shim first; defer the shims to a later hook.
;;; They are anonymous (loaded by path), so they are matched by file name.

(defparameter *late-libraries* '("libvterm-cl-shim" "libghostty-vt-cl" "libslt-ghostty"))

(defun late-library-p (library)
  (let ((path (ignore-errors (cffi:foreign-library-pathname library))))
    (and path
         (find-if (lambda (prefix)
                    (eql 0 (search prefix (file-namestring path))))
                  *late-libraries*))))

(deploy:define-hook (:deploy defer-shims 10) (directory)
  (declare (ignore directory))
  (dolist (library (cffi:list-foreign-libraries :loaded-only nil))
    (when (late-library-p library)
      (setf (deploy:library-dont-open-p (deploy:ensure-library library)) t))))

(deploy:define-hook (:boot open-shims) ()
  (dolist (library deploy::*foreign-libraries-to-reload*)
    (let ((library (deploy:ensure-library library)))
      (when (and (deploy:library-dont-open-p library)
                 (not (deploy:library-dont-deploy-p library)))
        (deploy:open-library library)))))
