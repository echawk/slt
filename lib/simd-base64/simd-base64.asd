(asdf:defsystem #:simd-base64
  :description "Incremental base64 decoding into octets or RGBA pixels, 16
digits per step with NEON or SSE2."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "decode"))
  :in-order-to ((test-op (test-op "simd-base64/tests"))))

(asdf:defsystem #:simd-base64/tests
  :depends-on (:simd-base64 :cl-base64)
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :simd-base64/tests :run-tests)
               (error "simd-base64 tests failed"))))
