(asdf:defsystem #:cl-libsixel
  :description "Thin, reusable Common Lisp bindings to libsixel's decoder."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cffi)
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "library")
               (:file "decoder"))
  :in-order-to ((test-op (test-op "cl-libsixel/tests"))))

(asdf:defsystem #:cl-libsixel/tests
  :depends-on (:cl-libsixel)
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :cl-libsixel/tests :run-tests)
               (error "cl-libsixel tests failed"))))
