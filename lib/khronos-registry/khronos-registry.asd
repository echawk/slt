(asdf:defsystem #:khronos-registry
  :description "Reading the Khronos API registries (vk.xml, egl.xml) to
generate bindings from: their XML, C literals, versions, depends
expressions and declarations, and Lisp names for C ones."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:plump)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "registry")))
