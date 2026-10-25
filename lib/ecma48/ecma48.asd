(asdf:defsystem #:ecma48
  :description "An incremental, allocation-free scanner for ECMA-48 control
sequences in UTF-8 terminal output."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "scanner"))
  :in-order-to ((test-op (test-op "ecma48/tests"))))

(asdf:defsystem #:ecma48/tests
  :depends-on (:ecma48)
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :ecma48/tests :run-tests)
               (error "ecma48 tests failed"))))
