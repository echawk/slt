(in-package #:wayland-cffi.generator)

;;;; Generating CFFI definitions from Wayland protocol descriptions, the XML
;;;; wayland-scanner reads: a function per request, a constant per enum
;;;; entry, and each interface's messages, of which LOAD-WAYLAND makes the
;;;; wl_interface structures libwayland-client marshals with.  Names become
;;;; Lisp's: wl_surface.set_buffer_scale is WL-SURFACE-SET-BUFFER-SCALE, the
;;;; keyboard entry of wl_seat's capability enum
;;;; +WL-SEAT-CAPABILITY-KEYBOARD+.

(defparameter +word-separator+ #\_
  "What separates words in the protocols' names.")

;;; Signatures, as wl_message's: a character per argument, after the
;;; version the message appeared in, with a mark before nullable ones.

(defparameter +argument-codes+
  '(("int" #\i :int) ("uint" #\u :uint) ("fixed" #\f :fixed) ("string" #\s :string)
    ("object" #\o :object) ("new_id" #\n :new-id) ("array" #\a :array) ("fd" #\h :fd))
  "Each argument type: its name in the XML, its character in a signature,
and its keyword here.")

(defun argument-type-code (type)
  (or (second (assoc type +argument-codes+ :test #'string=))
      (error "Unknown argument type ~a" type)))

(defun argument-kind (code)
  "The keyword of the argument type whose character in a signature is CODE."
  (or (third (find code +argument-codes+ :key #'second))
      (error "Unknown argument code ~a" code)))

(defparameter +nullable-mark+ #\?)

(defparameter +new-id-code+ (argument-type-code "new_id"))

(defparameter +untyped-new-id-codes+
  (coerce (mapcar #'argument-type-code '("string" "uint" "new_id")) 'string)
  "A new_id of no fixed interface (wl_registry.bind's) is sent as the
interface's name, the version and the id.")

(defconstant +first-version+ 1
  "The version of an interface every message is in unless it says since.")

;;; The protocols

(defun read-protocol (pathname)
  "The <protocol> element of the XML at PATHNAME."
  (let ((plump:*tag-dispatchers* plump:*xml-tags*))
    (child-element (plump:parse pathname) "protocol")))

(defun child-elements (node tag)
  (loop for child across (plump:children node)
        when (and (plump:element-p child) (string= tag (plump:tag-name child)))
          collect child))

(defun child-element (node tag)
  (first (child-elements node tag)))

(defun attribute (element name)
  (plump:attribute element name))

(defun summary (element)
  (let ((description (child-element element "description")))
    (or (attribute element "summary")
        (and description (attribute description "summary")))))

(defstruct (argument (:constructor %make-argument))
  name code interface nullable-p)

(defun parse-argument (element)
  (%make-argument :name (attribute element "name")
                  :code (argument-type-code (attribute element "type"))
                  :interface (attribute element "interface")
                  :nullable-p (equal (attribute element "allow-null") "true")))

(defun untyped-new-id-p (argument)
  (and (eql (argument-code argument) +new-id-code+) (null (argument-interface argument))))

(defun typed-new-id-p (argument)
  (and (eql (argument-code argument) +new-id-code+) (argument-interface argument)))

(defun message-arguments (element)
  (mapcar #'parse-argument (child-elements element "arg")))

(defun message-since (element)
  (let ((since (attribute element "since")))
    (if since (parse-integer since) +first-version+)))

(defun message-signature (element)
  (format nil "~@[~d~]~{~a~}"
          (let ((since (message-since element)))
            (and (> since +first-version+) since))
          (loop for argument in (message-arguments element)
                collect (concatenate 'string
                                     (if (argument-nullable-p argument)
                                         (string +nullable-mark+)
                                         "")
                                     (if (untyped-new-id-p argument)
                                         +untyped-new-id-codes+
                                         (string (argument-code argument)))))))

(defun message-types (element)
  "The interface of each argument in the signature that is an object or a
new object, else NIL."
  (loop for argument in (message-arguments element)
        append (if (untyped-new-id-p argument)
                   (make-list (length +untyped-new-id-codes+))
                   (list (argument-interface argument)))))

(defun message-description (element)
  (list (attribute element "name") (message-signature element) (message-types element)))

(defun interface-description (element)
  "(NAME VERSION REQUESTS EVENTS), each message (NAME SIGNATURE TYPES): the
wl_interface LOAD-WAYLAND makes for the <interface> ELEMENT."
  (list (attribute element "name")
        (parse-integer (attribute element "version"))
        (mapcar #'message-description (child-elements element "request"))
        (mapcar #'message-description (child-elements element "event"))))

;;; Names

(defun lisp-name (&rest names)
  "NAMES (wl_surface, attach) joined into a Lisp name (WL-SURFACE-ATTACH)."
  (substitute #\- +word-separator+ (format nil "~:@(~{~a~^-~}~)" names)))

(defun lisp-symbol (name)
  (intern name '#:wayland-cffi))

(defun parse-entry-value (text)
  (if (and (> (length text) 2) (string-equal "0x" text :end2 2))
      (parse-integer text :start 2 :radix 16)
      (parse-integer text)))

;;; Definitions

(defun enum-forms (interface)
  (loop with interface-name = (attribute interface "name")
        for enum in (child-elements interface "enum")
        append (loop for entry in (child-elements enum "entry")
                     collect `(defconstant
                                  ,(lisp-symbol
                                    (format nil "+~a+"
                                            (lisp-name interface-name (attribute enum "name")
                                                       (attribute entry "name"))))
                                ,(parse-entry-value (attribute entry "value"))
                                ,@(let ((summary (summary entry)))
                                    (and summary (list summary)))))))

(defun request-form (interface request opcode)
  "A function sending REQUEST, whose arguments follow the object it is sent
to, except a new object's, which it returns.  An object of no fixed
interface is made from the interface's name and the version."
  (let* ((interface-name (attribute interface "name"))
         (proxy (lisp-symbol (lisp-name interface-name)))
         (arguments (message-arguments request))
         (typed (find-if #'typed-new-id-p arguments))
         (untyped (find-if #'untyped-new-id-p arguments))
         (interface-parameter (lisp-symbol "INTERFACE"))
         (version-parameter (lisp-symbol "VERSION"))
         (values (loop for argument in arguments
                       append (cond ((typed-new-id-p argument) '())
                                    ((untyped-new-id-p argument)
                                     (list interface-parameter version-parameter))
                                    (t (list (lisp-symbol (lisp-name (argument-name argument)))))))))
    `(defun ,(lisp-symbol (lisp-name interface-name (attribute request "name"))) (,proxy ,@values)
       ,@(let ((summary (summary request)))
           (and summary (list (format nil "~a.~a: ~a" interface-name (attribute request "name")
                                      summary))))
       (send-request ,proxy ,opcode ,(message-signature request)
                     ,(cond (typed (argument-interface typed))
                            (untyped interface-parameter))
                     ,(cond (typed `(proxy-version ,proxy))
                            (untyped version-parameter))
                     ,(equal (attribute request "type") "destructor")
                     (list ,@values)))))

(defun interface-forms (interface)
  (append (enum-forms interface)
          (loop for request in (child-elements interface "request")
                for opcode from 0
                collect (request-form interface request opcode))))

(defun protocol-pathname (name)
  (asdf:system-relative-pathname :wayland-cffi (format nil "registry/~a.xml" name)))

(defmacro define-wayland-protocols (&rest names)
  "Define the bindings for the protocols NAMES, whose XML is in registry/."
  (let* ((interfaces (loop for name in names
                           append (child-elements (read-protocol (protocol-pathname name))
                                                  "interface")))
         (forms (mapcan #'interface-forms interfaces)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',(mapcar #'second forms) '#:wayland-cffi))
       ,@forms
       (setf *interface-descriptions* ',(mapcar #'interface-description interfaces)))))
