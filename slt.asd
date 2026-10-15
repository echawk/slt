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
  :depends-on (:slt :slt/conformance)
  :pathname "t/"
  :serial t
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call :slt/tests :run-tests)))

(asdf:defsystem #:slt/conformance
  :description "Engine conformance suite: byte streams with expected screens."
  :depends-on (:slt/core)
  :pathname "conformance/"
  :serial t
  :components ((:file "harness")
               (:file "cases")))

(asdf:defsystem #:slt/libvterm
  :description "libvterm engine for slt (requires lib/cl-libvterm to be built)."
  :depends-on (:slt/core :libvterm)
  :pathname "src/"
  :components ((:file "engine-libvterm")))

(asdf:defsystem #:slt/bench
  :description "Engine throughput benchmarks."
  :depends-on (:slt/core)
  :pathname "bench/"
  :components ((:file "throughput")))

(asdf:defsystem #:slt/ghostty
  :description "libghostty-vt engine for slt (run 'make ghostty' first)."
  :depends-on (:slt/core :ghostty-vt)
  :pathname "src/"
  :components ((:file "engine-ghostty")))
