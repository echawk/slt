(asdf:defsystem #:wayland-cffi
  :description "CFFI bindings to libwayland-client and libwayland-egl, with
requests and constants generated when compiled from Wayland protocol XML."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cffi :plump)
  :serial t
  :components ((:static-file "registry/wayland.xml")
               (:static-file "registry/xdg-shell.xml")
               (:static-file "registry/xdg-decoration-unstable-v1.xml")
               (:static-file "registry/cursor-shape-v1.xml")
               (:module "src"
                :serial t
                :components ((:file "package")
                             (:file "generate")
                             (:file "runtime")
                             (:file "bindings"))))
  :in-order-to ((test-op (test-op "wayland-cffi/tests"))))

(asdf:defsystem #:wayland-cffi/tests
  :depends-on (:wayland-cffi)
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :wayland-cffi/tests :run-tests)
               (error "wayland-cffi tests failed"))))
