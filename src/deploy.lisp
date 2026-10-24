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

;;; macOS system frameworks, loaded by the cocoa backend (and the gl
;;; backend's CoreText glyphs).
(dolist (name '("FOUNDATION" "APPKIT" "COCOA"))
  (define-optional-library "ORG.SHIRAKUMO.COCOAS.CFFI" name :dont-deploy t))
(dolist (name '("METAL-FRAMEWORK" "QUARTZ-CORE-FRAMEWORK"
                "CORE-TEXT-FRAMEWORK" "CORE-GRAPHICS-FRAMEWORK"))
  (define-optional-library "SLT" name :dont-deploy t))

;;; The gtk backend: GTK and the libraries under it belong to the system
;;; (they need their own data files and modules), and cl-cffi-gtk runs
;;; gtk_init from SBCL's init hooks, which run before Deploy reopens
;;; libraries; run it from a boot hook instead.
(loop for (package name) in '(("GLIB" "GLIB") ("GLIB" "GTHREAD") ("GOBJECT" "GOBJECT")
                              ("GIO" "GIO") ("GDK" "GDK") ("GDK" "GTK") ("GTK" "GTK")
                              ("GDK-PIXBUF" "GDK-PIXBUF") ("PANGO" "PANGO")
                              ("PANGO" "PANGOCAIRO") ("CAIRO" "CAIRO"))
      do (define-optional-library package name :dont-deploy t))

;;; Deploy clears CFFI's library directories when it dumps, which drops
;;; CFFI's macOS fallback (Homebrew, MacPorts, /usr/local).  Libraries left
;;; to the system, such as GTK, are found through them, so restore them just
;;; before Deploy reloads libraries.
(defparameter *system-library-directories* cffi:*foreign-library-directories*)

(deploy:define-hook (:boot system-library-directories (- most-positive-fixnum 9)) ()
  ;; WARMly-BOOT has already installed the executable, resource and library
  ;; directories.  Keep those first: replacing the list makes every bundled
  ;; library invisible during the foreign-library boot hook.
  (setf cffi:*foreign-library-directories*
        (remove-duplicates
         (append cffi:*foreign-library-directories* *system-library-directories*)
         :test #'equal)))

;; zlib is an operating-system library (and a dependency of libpng), not an
;; application payload.  On macOS CFFI's loose library search can otherwise
;; mistake Homebrew's libzip for libz and Deploy copies it under the wrong
;; identity.
(define-optional-library "KITTY-GRAPHICS" "LIBZ" :dont-deploy t)

(defun gtk-initializers ()
  (and (find-package "GLIB") (find-symbol "RUN-INITIALIZERS" "GLIB")))

(deploy:define-hook (:deploy defer-gtk-init) (directory)
  (declare (ignore directory))
  (let ((initializers (gtk-initializers)))
    (when initializers
      (setf sb-ext:*init-hooks* (remove initializers sb-ext:*init-hooks*)))))

(deploy:define-hook (:boot gtk-init) ()
  (let ((initializers (gtk-initializers)))
    (when initializers
      ;; cl-cffi-gtk warns about each GType this GTK lacks (the X11 ones on
      ;; macOS, for instance); they are not used.
      (handler-bind ((warning #'muffle-warning))
        (funcall initializers)))))

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
