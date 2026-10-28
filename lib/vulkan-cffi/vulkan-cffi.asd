(asdf:defsystem #:vulkan-cffi
  :description "CFFI bindings to Vulkan, generated when compiled from the
Vulkan registry (vk.xml) for a chosen core version and extensions."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cffi :plump)
  :serial t
  :components ((:static-file "registry/vk.xml")
               (:module "src"
                :serial t
                :components ((:file "package")
                             (:file "registry")
                             (:file "generate")
                             (:file "bindings")
                             (:file "runtime"))))
  :in-order-to ((test-op (test-op "vulkan-cffi/tests"))))

(asdf:defsystem #:vulkan-cffi/tests
  :depends-on (:vulkan-cffi :float-features)
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :vulkan-cffi/tests :run-tests)
               (error "vulkan-cffi tests failed"))))
