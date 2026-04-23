(asdf:defsystem #:slt
  :description "A simple Common Lisp terminal emulator frontend with pluggable GUI backends."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:3bst :clingon :ltk :osicat :sdl2 :sdl2-ttf :sdl2-image)
  :serial t
  :components ((:file "package")
               (:file "core")
               (:file "transport")
               (:file "backend")
               (:file "ltk-backend")
               (:file "sdl2-backend")
               (:file "frontend")
               (:file "cli")
               (:file "build"))
  :in-order-to ((test-op (test-op "slt/tests"))))

(asdf:defsystem #:slt/tests
  :depends-on (:slt)
  :serial t
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call :slt/tests :run-tests)))
