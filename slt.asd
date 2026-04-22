(asdf:defsystem #:slt
  :description "A simple lisp terminal emulator."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :depends-on (:sdl2 :3bst)
  :license "MIT"
  :version "0.0.1"
  :components ((:file "package")
               (:file "slt")))
