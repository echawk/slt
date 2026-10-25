(asdf:defsystem #:terminal-osc
  :description "Parsing and encoding of the OSC control strings modern
terminals share: colors, clipboard, hyperlinks, working directory, shell
integration marks and notifications."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:babel :cl-base64)
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "osc"))
  :in-order-to ((test-op (test-op "terminal-osc/tests"))))

(asdf:defsystem #:terminal-osc/tests
  :depends-on (:terminal-osc)
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :terminal-osc/tests :run-tests)
               (error "terminal-osc tests failed"))))
