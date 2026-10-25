(asdf:defsystem #:kitty-keyboard
  :description "The Kitty keyboard protocol: progressive-enhancement state and
key event encoding, for terminal emulators."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "keys")
               (:file "state")
               (:file "encode"))
  :in-order-to ((test-op (test-op "kitty-keyboard/tests"))))

(asdf:defsystem #:kitty-keyboard/tests
  :depends-on (:kitty-keyboard)
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :kitty-keyboard/tests :run-tests)
               (error "kitty-keyboard tests failed"))))
