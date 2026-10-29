(asdf:defsystem #:egl-cffi
  :description "CFFI bindings to EGL, generated when compiled from the EGL
registry (egl.xml) for a chosen version and extensions."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cffi :plump :khronos-registry)
  :serial t
  :components ((:static-file "registry/egl.xml")
               (:module "src"
                :serial t
                :components ((:file "package")
                             (:file "generate")
                             (:file "bindings")
                             (:file "runtime"))))
  :in-order-to ((test-op (test-op "egl-cffi/tests"))))

(asdf:defsystem #:egl-cffi/tests
  :depends-on (:egl-cffi)
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :egl-cffi/tests :run-tests)
               (error "egl-cffi tests failed"))))
