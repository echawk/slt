;;; As in egl-cffi: WAYLAND-CFFI uses no package and holds the generated
;;; definitions (WL-SURFACE-COMMIT, +WL-SEAT-CAPABILITY-KEYBOARD+) and the
;;; runtime's names; the code making them is in WAYLAND-CFFI.GENERATOR.

(defpackage #:wayland-cffi
  (:use)
  (:export
   #:wayland-error
   #:load-wayland
   #:wayland-loaded-p
   #:display-connect
   #:display-disconnect
   #:display-fd
   #:display-flush
   #:display-roundtrip
   #:display-dispatch-pending
   #:display-prepare-read
   #:display-read-events
   #:display-cancel-read
   #:display-error
   #:check-display
   #:set-listener
   #:destroy-proxy
   #:proxy-version
   #:load-wayland-egl
   #:egl-window-create
   #:egl-window-resize
   #:egl-window-destroy))

(defpackage #:wayland-cffi.generator
  (:use #:cl)
  (:import-from #:wayland-cffi
                #:wayland-error #:load-wayland #:wayland-loaded-p
                #:display-connect #:display-disconnect #:display-fd #:display-flush
                #:display-roundtrip #:display-dispatch-pending #:display-prepare-read
                #:display-read-events #:display-cancel-read #:display-error
                #:check-display #:set-listener #:destroy-proxy #:proxy-version
                #:load-wayland-egl #:egl-window-create #:egl-window-resize
                #:egl-window-destroy))
