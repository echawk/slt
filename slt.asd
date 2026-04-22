(asdf:defsystem #:slt
  :description "A simple Common Lisp terminal emulator frontend using 3bst and LTK."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:3bst :ltk)
  :serial t
  :components ((:file "package")
               (:file "core")
               (:file "frontend"))
  :in-order-to ((test-op (test-op "slt/tests"))))

(asdf:defsystem #:slt/tests
  :depends-on (:slt)
  :serial t
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call :slt/tests :run-tests)))
