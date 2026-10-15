(asdf:defsystem #:slt/core
  :description "Engine-neutral terminal core: grid, engines, hooks, PTY transport."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.2.0"
  :depends-on (:3bst :babel :cffi :osicat (:feature :sbcl :sb-posix))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "hooks")
               (:file "utf8")
               (:file "grid")
               (:file "core")
               (:file "backend")
               (:file "engine")
               (:file "engine-3bst")
               (:file "transport")
               (:file "frontend")))

(asdf:defsystem #:slt
  :description "A Common Lisp terminal emulator with pluggable engines and GUI backends."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.2.0"
  :depends-on (:slt/core :clingon :ltk :sdl2 :sdl2-ttf :sdl2-image)
  :pathname "src/"
  :serial t
  :components ((:file "ltk-backend")
               (:file "sdl2-backend")
               (:file "cli")
               (:file "build"))
  :in-order-to ((test-op (test-op "slt/tests"))))

(asdf:defsystem #:slt/tests
  :depends-on (:slt)
  :pathname "t/"
  :serial t
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call :slt/tests :run-tests)))
