(asdf:defsystem #:iterm2-images
  :description "iTerm2's inline images (OSC 1337 File): the protocol, sizing,
and decoding PNG, JPEG and GIF files into RGBA frames."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.1.0"
  ;; PNG is decoded by kitty-graphics' libpng binding.
  :depends-on (:simd-base64 :kitty-graphics :cl-jpeg :skippy :flexi-streams :uiop)
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "files")
               (:file "protocol"))
  :in-order-to ((test-op (test-op "iterm2-images/tests"))))

(asdf:defsystem #:iterm2-images/tests
  :depends-on (:iterm2-images)
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :iterm2-images/tests :run-tests)
               (error "iterm2-images tests failed"))))
