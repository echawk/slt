;;;; library.lisp -- locating and loading libvterm and the C shim.

(in-package #:libvterm)

(defparameter *libvterm-version* "0.3.3"
  "The libvterm release this binding is written against (pinned by
scripts/build-libvterm.sh).")

(defun %shared-ext ()
  #+darwin "dylib" #+windows "dll" #-(or darwin windows) "so")

(defun %build-dir ()
  (merge-pathnames "build/lib/" (asdf:system-source-directory :libvterm)))

(defun %getenv (name)
  (let ((v (uiop:getenv name)))
    (and v (plusp (length v)) v)))

(defun %libvterm-candidates ()
  "Where libvterm is looked for, in order: $LIBVTERM_LIBRARY, the subproject's
build/lib, then the system default search path."
  (let ((built (merge-pathnames (format nil "libvterm.~A" (%shared-ext)) (%build-dir))))
    (append (let ((env (%getenv "LIBVTERM_LIBRARY"))) (and env (list env)))
            (and (probe-file built) (list (namestring built)))
            #+darwin '("libvterm.0.dylib" "libvterm.dylib")
            #-darwin '("libvterm.so.0" "libvterm.so"))))

(defun %shim-candidates ()
  (let ((built (merge-pathnames (format nil "libvterm-cl-shim.~A" (%shared-ext)) (%build-dir))))
    (append (let ((env (%getenv "LIBVTERM_SHIM_LIBRARY"))) (and env (list env)))
            (and (probe-file built) (list (namestring built))))))

(defvar *loaded* nil)

(defun %try-load (candidates what)
  (dolist (c candidates
             (error "Could not load ~A (tried ~{~S~^, ~}). Run ~
                     lib/cl-libvterm/scripts/build-libvterm.sh or set ~
                     $LIBVTERM_LIBRARY / $LIBVTERM_SHIM_LIBRARY."
                    what candidates))
    (handler-case (return (cffi:load-foreign-library c))
      (cffi:load-foreign-library-error () nil))))

(defun load-libvterm (&key force)
  "Load libvterm, then the shim (whose vterm_* references resolve against the
already-loaded libvterm).  Idempotent unless FORCE."
  (when (or force (not *loaded*))
    (%try-load (%libvterm-candidates) "libvterm")
    (%try-load (%shim-candidates) "the cl-libvterm shim")
    (setf *loaded* t))
  t)

(load-libvterm)
