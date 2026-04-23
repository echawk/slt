(asdf:defsystem #:slt
  :description "A simple Common Lisp terminal emulator frontend with pluggable GUI backends."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:3bst :ltk :osicat)
  :serial t
  :components ((:file "package")
               (:file "core")
               (:file "transport")
               (:file "backend")
               (:file "ltk-backend")
               (:file "frontend"))
  :in-order-to ((test-op (test-op "slt/tests"))))

(asdf:defsystem #:slt/tests
  :depends-on (:slt)
  :serial t
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call :slt/tests :run-tests)))
