;;; As in vulkan-cffi: EGL-CFFI uses no package and holds the generated
;;; definitions (GET-PLATFORM-DISPLAY, +PLATFORM-WAYLAND-KHR+) and the
;;; runtime's names; the code making them is in EGL-CFFI.GENERATOR.

(defpackage #:egl-cffi
  (:use)
  (:export
   #:egl-error
   #:egl-error-code
   #:load-egl
   #:egl-loaded-p
   #:check-egl
   #:command-loaded-p
   #:with-attributes))

(defpackage #:egl-cffi.generator
  (:use #:cl)
  (:import-from #:khronos-registry
                #:read-registry-document #:attribute #:child-elements #:child-element
                #:child-text #:split-list #:element-name #:parse-value #:version<=
                #:lisp-words #:without-prefix #:parse-declaration)
  (:import-from #:egl-cffi
                #:egl-error #:egl-error-code #:load-egl #:egl-loaded-p #:check-egl
                #:command-loaded-p #:with-attributes))
