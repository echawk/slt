(asdf:defsystem #:kitty-graphics
  :description "A reusable Common Lisp codec for the Kitty graphics protocol."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-base64 :cffi)
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "native")
               (:file "protocol"))
  :in-order-to ((test-op (test-op "kitty-graphics/tests"))))

(asdf:defsystem #:kitty-graphics/tests
  :depends-on (:kitty-graphics)
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :kitty-graphics/tests :run-tests)
               (error "kitty-graphics tests failed"))))
