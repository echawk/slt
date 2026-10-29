(in-package #:slt)

;;;; A renderer for a window a frontend made itself, with the API *GL-API*
;;;; names: OpenGL through EGL, or Vulkan on a surface of the window.

(defun gl-apis (api)
  "The OpenGL APIs to try, in turn, for *GL-API*'s value API."
  (ecase api
    (:auto '(:gl :gles))
    (:gl '(:gl))
    (:gles '(:gles))))

(defun make-window-renderer (&key egl-platform native-display native-window
                               vulkan-extensions vulkan-surface)
  "A renderer and its host for a window, drawn with *GL-API*.  For OpenGL,
EGL-PLATFORM, NATIVE-DISPLAY and NATIVE-WINDOW are MAKE-EGL-HOST's; for
Vulkan, VULKAN-EXTENSIONS are the instance extensions the window system's
surfaces need and VULKAN-SURFACE, called with the instance, makes the
window's.  Returns (values RENDERER HOST)."
  (if (eq *gl-api* :vulkan)
      (values (make-vulkan-renderer :instance-extensions vulkan-extensions
                                    :surface-function vulkan-surface
                                    :vsync *gl-vsync*)
              nil)
      (dolist (api (gl-apis *gl-api*) (error "No OpenGL context could be made."))
        (let ((host (handler-case (make-egl-host egl-platform native-display native-window api)
                      (error (condition)
                        (format *error-output* "~&slt: no ~a context: ~a~%" api condition)
                        nil))))
          (when host
            (return (values (make-gl-renderer api) host)))))))
