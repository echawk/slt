(defpackage #:khronos-registry
  (:use #:cl)
  (:export
   #:+list-separator+
   #:+conjunction+
   #:read-registry-document
   #:attribute
   #:child-elements
   #:child-element
   #:child-text
   #:split-list
   #:element-name
   #:parse-value
   #:version-parts
   #:version<=
   #:depends-satisfied-p
   #:lisp-words
   #:without-prefix
   #:parse-declaration))
