(in-package #:slt)

;;;; The x11 frontend: the clx frontend's windows, keyboard, mouse,
;;;; selections and drops, drawn with OpenGL (through EGL) or Vulkan, as
;;;; *GL-API* says.  CLX speaks the X protocol in Lisp; EGL and Vulkan need
;;;; a C Xlib connection, so the frontend opens one too and hands it the
;;;; window, whose id is the server's and good on any connection.

(defclass x11-frontend (clx-frontend)
  ;; The Xlib Display* EGL and Vulkan draw through.
  ((xlib-display :initform nil :accessor x11-frontend-xlib-display)))

(defstruct (x11-view (:include clx-view) (:constructor %make-x11-view))
  renderer
  host)

(cffi:define-foreign-library xlib-library
  (:unix (:or "libX11.so.6" "libX11.so"))
  (t (:default "libX11")))

(defun x11-libraries-available-p ()
  "True when Xlib and the glyph rasterizer's libraries load."
  (and (ignore-errors (cffi:load-foreign-library 'xlib-library) t)
       (glyph-libraries-available-p)))

(cffi:defcfun ("XOpenDisplay" %x-open-display) :pointer (name :pointer))
(cffi:defcfun ("XCloseDisplay" %x-close-display) :int (display :pointer))

(defun x11-xlib-display (frontend)
  "The frontend's Xlib connection, to the display CLX opened ($DISPLAY)."
  (or (x11-frontend-xlib-display frontend)
      (progn
        (unless (cffi:foreign-library-loaded-p 'xlib-library)
          (cffi:load-foreign-library 'xlib-library))
        (let ((display (%x-open-display (cffi:null-pointer))))
          (when (cffi:null-pointer-p display)
            (error "Xlib cannot open the display ~a" (uiop:getenv "DISPLAY")))
          (setf (x11-frontend-xlib-display frontend) display)))))

(defun x11-vulkan-surface (instance xlib-display window-id)
  (vulkan-receive (surface vk:surface-khr)
    (vk:with-struct (info vk:xlib-surface-create-info-khr :dpy xlib-display :window window-id)
      (vulkan-call (vk:create-xlib-surface-khr instance info (cffi:null-pointer) surface)))))

;;; The clx frontend's drawing, done with a renderer

(defmethod clx-window-background ((frontend x11-frontend))
  ;; The renderer paints every pixel; X clearing first would flicker.
  :none)

(defmethod clx-drawing-resources ((frontend x11-frontend) window display)
  (declare (ignore window display))
  '())

(defmethod clx-make-view ((frontend x11-frontend) &rest arguments)
  (apply #'%make-x11-view arguments))

(defmethod clx-view-mapped ((frontend x11-frontend) view)
  ;; The window must exist on the server before the other connection
  ;; refers to it.
  (xlib:display-finish-output (clx-frontend-display frontend))
  (let ((xlib-display (x11-xlib-display frontend))
        (id (xlib:window-id (clx-view-window view))))
    (cffi:with-foreign-object (window :unsigned-long)
      (setf (cffi:mem-ref window :unsigned-long) id)
      (multiple-value-bind (renderer host)
          (make-window-renderer
           :egl-platform egl:+platform-x11-khr+
           :native-display xlib-display
           :native-window window
           :vulkan-extensions (list vk:+khr-xlib-surface-extension-name+)
           :vulkan-surface (lambda (instance)
                             (x11-vulkan-surface instance xlib-display id)))
        (setf (x11-view-renderer view) renderer
              (x11-view-host view) host)))))

(defmethod clx-view-closing ((frontend x11-frontend) view)
  (let ((renderer (shiftf (x11-view-renderer view) nil)))
    (when renderer
      (ignore-errors (free-renderer renderer (x11-view-host view))))))

(defun x11-frame (view)
  "VIEW's grid packed, as a frame for its renderer."
  (let ((grid (clx-view-grid view)))
    (clx-pack view)
    (list (clx-view-instances view)
          (grid-columns grid)
          (* (grid-rows grid) (grid-columns grid))
          (clx-view-cell-width view)
          (clx-view-cell-height view)
          (max 1 (clx-view-width view))
          (max 1 (clx-view-height view))
          (clx-view-atlas view)
          *palette*
          (clx-image-records view))))

(defmethod clx-redraw ((frontend x11-frontend) view whole-p)
  (declare (ignore whole-p))
  ;; The renderer draws whole frames; packing still covers only the rows
  ;; that changed.
  (when (and (x11-view-renderer view) (clx-view-grid view))
    (with-view-palette (view)
      (renderer-present (x11-view-renderer view) (x11-view-host view) (x11-frame view)))))

(defmethod frontend-snapshot ((frontend x11-frontend) (view x11-view))
  (with-view-palette (view)
    (values (renderer-snapshot (x11-view-renderer view) (x11-view-host view) (x11-frame view))
            (max 1 (clx-view-width view))
            (max 1 (clx-view-height view)))))

(defmethod frontend-call-with-event-loop ((frontend x11-frontend) thunk)
  (declare (ignore thunk))
  ;; Mesa and the Vulkan drivers compute with floats that would trap.
  (float-features:with-float-traps-masked t
    (unwind-protect (call-next-method)
      (dolist (view (clx-frontend-views frontend))
        (clx-view-closing frontend view))
      (let ((display (shiftf (x11-frontend-xlib-display frontend) nil)))
        (when display
          (%x-close-display display))))))

(register-frontend :x11 (lambda () (make-instance 'x11-frontend)))
