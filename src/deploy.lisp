(in-package #:slt)

;;;; Distribution build (system slt/app, built with Deploy).  Deploy dumps
;;;; the executable into bin/ and copies the foreign libraries it loaded
;;;; next to it, opening them from there at startup.  Libraries every Mac
;;;; (or Linux system) already has are excluded here.

(deploy:define-library cffi::libffi :dont-deploy t)
(deploy:define-library osicat-posix::librt :dont-deploy t)

#+darwin
(progn
  (deploy:define-library org.shirakumo.cocoas.cffi:foundation :dont-deploy t)
  (deploy:define-library org.shirakumo.cocoas.cffi:appkit :dont-deploy t)
  (deploy:define-library org.shirakumo.cocoas.cffi:cocoa :dont-deploy t)
  (deploy:define-library metal-framework :dont-deploy t)
  (deploy:define-library quartz-core-framework :dont-deploy t)
  (deploy:define-library core-text-framework :dont-deploy t)
  (deploy:define-library core-graphics-framework :dont-deploy t))

;;; ghostty-vt loads its dylib by explicit path, which CFFI registers as an
;;; anonymous library (deployed above); its named definition has no path.
(deploy:define-library ghostty-vt.ffi:libghostty-vt :dont-deploy t :dont-open t)

;;; The engines' C shims resolve their library's symbols at load time, so
;;; they must be opened after it.  Deploy reopens libraries in registration
;;; order, which puts each shim first; defer the shims to a later hook.
;;; They are anonymous (loaded by path), so they are matched by file name.

(defparameter *late-libraries* '("libvterm-cl-shim" "libghostty-vt-cl"))

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
