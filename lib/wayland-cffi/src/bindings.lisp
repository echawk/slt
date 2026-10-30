(in-package #:wayland-cffi.generator)

;;;; The bindings: the core protocol, xdg-shell's windows, and the
;;;; decoration and cursor shape protocols a window asks for when the
;;;; compositor has them.

(define-wayland-protocols
  "wayland"
  "xdg-shell"
  "xdg-decoration-unstable-v1"
  "cursor-shape-v1")
