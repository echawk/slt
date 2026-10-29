;;; The bindings' names are Vulkan's and cannot share a package with
;;; COMMON-LISP's (VkFormat is FORMAT), so VULKAN-CFFI uses no package; it
;;; holds the generated definitions and the runtime's exported names.  The
;;; code that generates and supports them is in VULKAN-CFFI.GENERATOR.

(defpackage #:vulkan-cffi
  (:use)
  ;; The generated bindings export their own names as well:
  ;; (CREATE-INSTANCE ...), INSTANCE-CREATE-INFO, S-TYPE,
  ;; +FORMAT-B8G8R8A8-UNORM+ and so on.
  (:export
   #:vulkan-error
   #:vulkan-error-result
   #:vulkan-error-command
   #:check-result
   #:load-vulkan
   #:load-instance-commands
   #:load-device-commands
   #:command-loaded-p
   #:make-api-version
   #:structure-type-of
   #:with-struct
   #:with-structs
   #:zero-struct))

(defpackage #:vulkan-cffi.generator
  (:use #:cl)
  (:import-from #:khronos-registry
                #:+list-separator+ #:read-registry-document #:attribute #:child-elements
                #:child-element #:child-text #:split-list #:element-name #:parse-value
                #:version<= #:depends-satisfied-p #:lisp-words #:without-prefix
                #:parse-declaration)
  (:import-from #:vulkan-cffi
                #:vulkan-error
                #:vulkan-error-result
                #:vulkan-error-command
                #:check-result
                #:load-vulkan
                #:load-instance-commands
                #:load-device-commands
                #:command-loaded-p
                #:make-api-version
                #:structure-type-of
                #:with-struct
                #:with-structs
                #:zero-struct))
