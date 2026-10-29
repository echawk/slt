(in-package #:slt)

;;;; The OpenGL renderer in a window made by the frontend itself (X11,
;;;; Wayland), through EGL 1.5: a display for the window system's platform,
;;;; a context for OpenGL 3.3 core or OpenGL ES 3.0, and a surface on the
;;;; window.  cl-opengl finds its entry points through eglGetProcAddress.

(defconstant +egl-channel-bits+ 8 "Bits per color channel of the window.")

(defstruct (egl-host (:constructor %make-egl-host))
  (display (cffi:null-pointer))
  (context (cffi:null-pointer))
  (surface (cffi:null-pointer)))

(defmacro egl-call (form)
  "Run the EGL call FORM, signalling EGL-ERROR when it fails."
  `(egl:check-egl ,form ',(first form)))

(defun egl-proc-address (name)
  (cffi:with-foreign-string (c-name name)
    (egl:get-proc-address c-name)))

(defun egl-config (display api)
  "A config of DISPLAY for windows drawn with API, 8 bits per channel and
opaque (X11's default visual has no alpha)."
  (egl:with-attributes (attributes :int32
                        (list egl:+surface-type+ egl:+window-bit+
                              egl:+renderable-type+ (if (eq api :gles)
                                                        egl:+opengl-es3-bit+
                                                        egl:+opengl-bit+)
                              egl:+red-size+ +egl-channel-bits+
                              egl:+green-size+ +egl-channel-bits+
                              egl:+blue-size+ +egl-channel-bits+
                              egl:+alpha-size+ 0))
    (cffi:with-foreign-objects ((config :pointer) (count :int32))
      (egl-call (egl:choose-config display attributes config 1 count))
      (when (zerop (cffi:mem-ref count :int32))
        (error "EGL has no config for ~a windows" api))
      (cffi:mem-ref config :pointer))))

(defun egl-context-attributes (api)
  (destructuring-bind (major minor) (gl-context-version api)
    (append (list egl:+context-major-version+ major
                  egl:+context-minor-version+ minor)
            (and (eq api :gl)
                 (list egl:+context-opengl-profile-mask+
                       egl:+context-opengl-core-profile-bit+)))))

(defun make-egl-host (platform native-display native-window api)
  "A context for API (:GL or :GLES) drawing into NATIVE-WINDOW, current on
this thread.  PLATFORM is an EGL platform (+PLATFORM-X11-KHR+);
NATIVE-DISPLAY and NATIVE-WINDOW are what eglGetPlatformDisplay and
eglCreatePlatformWindowSurface take for it: for X11 a Display* and a
pointer to the Window, for Wayland a wl_display* and a wl_egl_window*."
  (egl:load-egl)
  (let ((host (%make-egl-host)))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (free-egl-host host))))
      (let ((display (egl:with-attributes (attributes :intptr '())
                       (egl-call (egl:get-platform-display platform native-display attributes)))))
        (setf (egl-host-display host) display)
        (egl-call (egl:initialize display (cffi:null-pointer) (cffi:null-pointer)))
        (egl-call (egl:bind-api (if (eq api :gles) egl:+opengl-es-api+ egl:+opengl-api+)))
        (let ((config (egl-config display api)))
          (setf (egl-host-context host)
                (egl:with-attributes (attributes :int32 (egl-context-attributes api))
                  (egl-call (egl:create-context display config (cffi:null-pointer) attributes)))
                (egl-host-surface host)
                (egl:with-attributes (attributes :intptr '())
                  (egl-call (egl:create-platform-window-surface display config native-window
                                                                attributes)))))
        (egl-make-current host)
        (egl-call (egl:swap-interval display (if *gl-vsync* 1 0)))
        (setf cl-opengl-bindings:*gl-get-proc-address* #'egl-proc-address)))
    host))

(defun egl-make-current (host)
  (egl-call (egl:make-current (egl-host-display host) (egl-host-surface host)
                              (egl-host-surface host) (egl-host-context host))))

(defun free-egl-host (host)
  (let ((display (egl-host-display host)))
    (unless (cffi:null-pointer-p display)
      (egl:make-current display (cffi:null-pointer) (cffi:null-pointer) (cffi:null-pointer))
      (unless (cffi:null-pointer-p (egl-host-surface host))
        (egl:destroy-surface display (egl-host-surface host)))
      (unless (cffi:null-pointer-p (egl-host-context host))
        (egl:destroy-context display (egl-host-context host)))
      (egl:terminate display)
      (setf (egl-host-display host) (cffi:null-pointer)))))

;; OpenGL in an EGL surface.

(defmethod renderer-present ((renderer gl-renderer) (host egl-host) frame)
  (egl-make-current host)
  (apply #'gl-draw renderer frame)
  (egl-call (egl:swap-buffers (egl-host-display host) (egl-host-surface host))))

(defmethod renderer-snapshot ((renderer gl-renderer) (host egl-host) frame)
  (egl-make-current host)
  (apply #'gl-snapshot renderer frame))

(defmethod free-renderer ((renderer gl-renderer) (host egl-host))
  (egl-make-current host)
  (free-gl-renderer renderer)
  (free-egl-host host))
