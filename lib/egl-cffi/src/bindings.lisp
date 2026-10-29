(in-package #:egl-cffi.generator)

;;;; The bindings: EGL 1.5, whose platform functions take X11 and Wayland
;;;; displays and windows, with the constants naming those platforms.

(define-egl-bindings
  :version "1.5"
  :extensions ("EGL_KHR_platform_x11"
               "EGL_KHR_platform_wayland"))
