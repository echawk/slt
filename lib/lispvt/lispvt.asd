(asdf:defsystem #:lispvt
  :description "A fast VT/xterm terminal emulation core in pure Common Lisp."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.1.0"
  ;; scan.lisp uses NEON (arm64) or SSE2 (x86-64) through sb-simd when SBCL has it.
  :serial t
  :components ((:file "package")
               (:file "scan")
               (:file "width")
               (:file "screen")
               (:file "terminal"))
  :in-order-to ((test-op (test-op "lispvt/tests"))))

(asdf:defsystem #:lispvt/tests
  :depends-on (:lispvt)
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :lispvt/tests :run-tests)
               (error "lispvt tests failed."))))
