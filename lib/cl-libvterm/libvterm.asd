;;;; libvterm.asd -- standalone CFFI binding to libvterm 0.3.

(asdf:defsystem #:libvterm
  :description "CFFI binding to libvterm 0.3 (the VT emulation library used by Neovim)."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cffi :babel)
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "library")
               (:file "ffi")
               (:file "api"))
  :in-order-to ((test-op (test-op "libvterm/tests"))))

(asdf:defsystem #:libvterm/tests
  :description "Tests for the libvterm binding."
  :depends-on (:libvterm)
  :serial t
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :libvterm/tests :run-tests)
               (error "libvterm tests failed"))))
