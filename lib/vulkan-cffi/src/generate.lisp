(in-package #:vulkan-cffi.generator)

;;;; Generating CFFI definitions from a SELECTION of the registry.  Names
;;;; become Lisp's: VkInstanceCreateInfo is INSTANCE-CREATE-INFO, its
;;;; member pApplicationInfo P-APPLICATION-INFO, vkCreateInstance
;;;; CREATE-INSTANCE and VK_FORMAT_B8G8R8A8_UNORM +FORMAT-B8G8R8A8-UNORM+.
;;;; Commands are called through entry points that LOAD-VULKAN,
;;;; LOAD-INSTANCE-COMMANDS and LOAD-DEVICE-COMMANDS look up.

(defparameter +type-prefix+ "Vk")
(defparameter +command-prefix+ "vk")
(defparameter +constant-prefix+ "VK_")
(defparameter +handle-types+
  '(("VK_DEFINE_HANDLE" . :pointer)
    ("VK_DEFINE_NON_DISPATCHABLE_HANDLE" . :uint64))
  "Each kind of handle, as the macro defining it is named, and its
representation: a dispatchable handle is a pointer; the others are 64
bits on every platform.")

(defparameter +flags-types+ '(("VkFlags" . :uint32) ("VkFlags64" . :uint64))
  "The types of bitmasks, by their width.")

(defparameter +enum-type+ :int32 "The representation of an enumerated type.")

(defconstant +wide-bitmask-bits+ 64)

(defparameter +command-levels+
  '(("VkInstance" . :instance) ("VkPhysicalDevice" . :instance)
    ("VkDevice" . :device) ("VkQueue" . :device) ("VkCommandBuffer" . :device))
  "What a command's first parameter makes it: an instance's or a
device's.  Commands with neither are global.")

;;; Entry points, filled in by the generated definitions and loaded by
;;; the runtime.

(defvar *command-names* (vector) "Each command's name in the registry.")
(defvar *command-levels* (vector) "Each command's level: :GLOBAL, :INSTANCE or :DEVICE.")
(defvar *command-pointers* (vector) "Each command's entry point, null until loaded.")
(defvar *result-names* '() "VkResult's values and their names, ((VALUE . NAME) ...).")
(defvar *structure-types* (make-hash-table :test 'eq)
  "Each structure's type, the value of its sType member, by its name.")

(defun command-not-loaded (index)
  (error "Vulkan command ~a is not loaded" (svref *command-names* index)))

(declaim (inline command-pointer))
(defun command-pointer (index)
  (let ((pointer (svref *command-pointers* index)))
    (if (cffi:null-pointer-p pointer) (command-not-loaded index) pointer)))

;;; Names

(defun lisp-symbol (name)
  (intern (string-upcase name) '#:vulkan-cffi))

(defun type-symbol (name)
  (lisp-symbol (lisp-words (without-prefix +type-prefix+ name))))

(defun command-symbol (name)
  (lisp-symbol (lisp-words (without-prefix +command-prefix+ name))))

(defun constant-symbol (name)
  (lisp-symbol (format nil "+~a+" (lisp-words (string-downcase
                                                (without-prefix +constant-prefix+ name))))))

(defun member-symbol (name)
  (lisp-symbol (lisp-words name)))

;;; Declarations

(defun resolve-type (selection name)
  "The <type> NAME is, after following aliases."
  (loop for type = (gethash name (selection-types selection))
        for alias = (and type (attribute type "alias"))
        while alias
        do (setf name alias)
        finally (return type)))

(defun category (selection name)
  (let ((type (resolve-type selection name)))
    (and type (attribute type "category"))))

(defun foreign-type (selection name pointers)
  "The CFFI type of POINTERS pointers to the type NAME."
  (cond ((plusp pointers) :pointer)
        ((cdr (assoc name +c-types+ :test #'string=)))
        ((cdr (assoc name +platform-types+ :test #'string=)))
        (t (let ((category (category selection name))
                 (target (element-name (resolve-type selection name))))
             (cond ((equal category "struct") `(:struct ,(type-symbol target)))
                   ((equal category "union") `(:union ,(type-symbol target)))
                   (t (type-symbol name)))))))

(defun constant-value (selection name)
  (let ((enum (gethash name (registry-enumerants (selection-registry selection)))))
    (unless enum
      (error "The Vulkan registry has no constant ~a" name))
    (enumerant-value enum)))

;;; Constants

(defun enumerants (selection)
  "Every constant and enumerant selected, ((NAME . VALUE) ...), aliases
resolved and those whose target is not selected left out."
  (let ((all (remove-duplicates
              (append (selection-constants selection)
                      (loop for type in (selection-type-order selection)
                            append (gethash type (selection-enum-values selection))))
              :key #'car :test #'string= :from-end t))
        (by-name (make-hash-table :test 'equal)))
    (loop for (name . value) in all
          do (setf (gethash name by-name) value))
    (loop for (name . value) in all
          ;; A value of (:ALIAS NAME) is that name's.
          for resolved = (loop for target = value then (gethash (second target) by-name)
                               while (consp target)
                               finally (return target))
          when resolved
            collect (cons name resolved))))

(defun constant-forms (selection)
  (loop for (name . value) in (enumerants selection)
        collect (if (stringp value)
                    `(defparameter ,(constant-symbol name) ,value)
                    `(defconstant ,(constant-symbol name) ,value))))

;;; Types

(defun scalar-type-form (selection name type)
  "The DEFCTYPE of the non-structure TYPE, or NIL when it has no
representation of its own (opaque types, used only by pointer)."
  (let ((alias (attribute type "alias"))
        (category (attribute type "category")))
    (flet ((define (representation)
             `(cffi:defctype ,(type-symbol name) ,representation)))
      (cond
        (alias (unless (member (category selection alias) '("struct" "union") :test #'equal)
                 (define (type-symbol alias))))
        ((equal category "handle")
         (define (cdr (assoc (child-text type "type") +handle-types+ :test #'string=))))
        ((equal category "enum")
         (let ((enums (gethash name (registry-enums (selection-registry selection)))))
           (define (if (and enums (equal (attribute enums "type") "bitmask"))
                       (if (equal (attribute enums "bitwidth")
                                  (princ-to-string +wide-bitmask-bits+))
                           (cdr (assoc "VkFlags64" +flags-types+ :test #'string=))
                           (cdr (assoc "VkFlags" +flags-types+ :test #'string=)))
                       +enum-type+))))
        ((equal category "bitmask")
         (define (type-symbol (child-text type "type"))))
        ((equal category "basetype")
         (let ((base (child-text type "type"))
               (pointers (count #\* (plump:text type))))
           (cond (base (define (foreign-type selection base pointers)))
                 ((plusp pointers) (define :pointer)))))
        ((equal category "funcpointer") (define :pointer))))))

(defun struct-dependencies (selection type)
  "The structures and unions TYPE holds by value."
  (loop for member in (child-elements type "member")
        when (api-p member)
          append (multiple-value-bind (name member-type pointers) (parse-declaration member)
                   (declare (ignore name))
                   (and (zerop pointers)
                        (member (category selection member-type) '("struct" "union")
                                :test #'equal)
                        (list (element-name (resolve-type selection member-type)))))))

(defun sorted-structs (selection)
  "The selected structures and unions, each after those it holds."
  (let ((done (make-hash-table :test 'equal))
        (order '()))
    (labels ((visit (name)
               (unless (gethash name done)
                 (setf (gethash name done) t)
                 (dolist (dependency (struct-dependencies selection (gethash name (selection-types selection))))
                   (visit dependency))
                 (push name order))))
      (dolist (name (selection-type-order selection))
        (let ((type (gethash name (selection-types selection))))
          (when (and (null (attribute type "alias"))
                     (member (attribute type "category") '("struct" "union") :test #'equal))
            (visit name)))))
    (nreverse order)))

(defun struct-form (selection name)
  (let* ((type (gethash name (selection-types selection)))
         (slots
           (loop for member in (child-elements type "member")
                 when (api-p member)
                   collect (multiple-value-bind (member-name member-type pointers dimensions bits)
                               (parse-declaration member)
                             (when bits
                               (error "~a has the bitfield ~a, which CFFI cannot lay out"
                                      name member-name))
                             (let ((count (reduce #'* (mapcar (lambda (dimension)
                                                                (if (stringp dimension)
                                                                    (constant-value selection dimension)
                                                                    dimension))
                                                              dimensions))))
                               `(,(member-symbol member-name)
                                 ,(foreign-type selection member-type pointers)
                                 ,@(when dimensions `(:count ,count))))))))
    `(,(if (equal (attribute type "category") "union") 'cffi:defcunion 'cffi:defcstruct)
      ,(type-symbol name)
      ,@slots)))

(defun structure-type-entries (selection)
  "((STRUCT-SYMBOL . STYPE-VALUE) ...) for the structures whose sType has
a fixed value."
  (let ((structure-types (gethash "VkStructureType" (selection-enum-values selection))))
    (loop for name in (selection-type-order selection)
          for type = (gethash name (selection-types selection))
          for value = (loop for member in (child-elements type "member")
                            for values = (attribute member "values")
                            when (and values (api-p member))
                              return (cdr (assoc values structure-types :test #'string=)))
          when (integerp value)
            collect (cons (type-symbol name) value))))

;;; Commands

(defun command-definition (selection name)
  "The <command> defining NAME's parameters, following aliases."
  (loop for command = (gethash name (selection-commands selection))
        for alias = (attribute command "alias")
        while alias
        do (setf name alias)
        finally (return command)))

(defun command-forms (selection)
  (loop for name in (selection-command-order selection)
        for index from 0
        for command = (command-definition selection name)
        for proto = (child-element command "proto")
        for params = (loop for param in (child-elements command "param")
                           when (api-p param)
                             collect (multiple-value-list (parse-declaration param)))
        for symbol = (command-symbol name)
        collect name into names
        collect (or (cdr (assoc (second (first params)) +command-levels+ :test #'string=))
                    :global)
          into levels
        collect `(defun ,symbol ,(mapcar (lambda (param) (member-symbol (first param))) params)
                   ,name
                   (cffi:foreign-funcall-pointer
                    (command-pointer ,index) ()
                    ,@(loop for (param-name type pointers) in params
                            append (list (foreign-type selection type pointers)
                                         (member-symbol param-name)))
                    ,(multiple-value-bind (proto-name type pointers) (parse-declaration proto)
                       (declare (ignore proto-name))
                       (foreign-type selection type pointers))))
          into forms
        finally (return
                  `((setf *command-names* ,(coerce names 'simple-vector)
                          *command-levels* ,(coerce levels 'simple-vector)
                          *command-pointers* (make-array ,(length names)
                                                         :initial-element (cffi:null-pointer)))
                    ,@forms))))

;;; All of it

(defun defined-symbols (forms)
  "The symbols FORMS define, with the slots of the structures they define."
  (loop for form in forms
        append (case (first form)
                 ((cffi:defcstruct cffi:defcunion)
                  (cons (second form) (mapcar #'first (cddr form))))
                 ((defun defconstant defparameter cffi:defctype) (list (second form))))))

(defun generate-bindings (selection)
  (let* ((types (loop for name in (selection-type-order selection)
                      for type = (gethash name (selection-types selection))
                      unless (member (attribute type "category") '("struct" "union") :test #'equal)
                        collect (cons name type)))
         ;; Bitmasks and aliases name other types, defined first.
         (scalar-types
           (loop for pass in (list (lambda (type)
                                     (and (null (attribute type "alias"))
                                          (equal (attribute type "category") "basetype")))
                                   (lambda (type)
                                     (and (null (attribute type "alias"))
                                          (not (equal (attribute type "category") "basetype"))))
                                   (lambda (type) (attribute type "alias")))
                 append (loop for (name . type) in types
                              when (funcall pass type)
                                collect (scalar-type-form selection name type))))
         (structs (mapcar (lambda (name) (struct-form selection name)) (sorted-structs selection)))
         (struct-aliases
           (loop for (name . type) in (loop for name in (selection-type-order selection)
                                            collect (cons name (gethash name (selection-types selection))))
                 for alias = (attribute type "alias")
                 when (and alias (member (category selection alias) '("struct" "union") :test #'equal))
                   collect `(cffi:defctype ,(type-symbol name)
                                (,(if (equal (category selection alias) "union") :union :struct)
                                 ,(type-symbol (element-name (resolve-type selection alias)))))))
         (forms (append (constant-forms selection)
                        (remove nil scalar-types)
                        structs
                        struct-aliases
                        (command-forms selection)))
         (symbols (remove-duplicates (defined-symbols forms))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',symbols '#:vulkan-cffi))
       ,@forms
       (setf *result-names*
             ',(loop for (name . value) in (gethash "VkResult" (selection-enum-values selection))
                     when (integerp value) collect (cons value name)))
       (clrhash *structure-types*)
       (loop for (name . value) in ',(structure-type-entries selection)
             do (setf (gethash name *structure-types*) value)))))

(defmacro define-vulkan-bindings (&key version extensions)
  "Define the bindings for the core VERSION (\"1.3\") and the named
EXTENSIONS, from the registry."
  (generate-bindings (select-registry (read-registry) :version version :extensions extensions)))
