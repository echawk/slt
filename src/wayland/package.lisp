(in-package #:slt)

;;; The wayland frontend refers to the bindings as WL; cl-xkb's package is
;;; XKB.
(trivial-package-local-nicknames:add-package-local-nickname '#:wl '#:wayland-cffi '#:slt)
