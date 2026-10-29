(in-package #:vulkan-cffi.generator)

;;;; Reading the Vulkan registry, vk.xml (KhronosGroup/Vulkan-Headers,
;;;; registry/vk.xml), and selecting from it what a core version and a
;;;; set of extensions require: their types, with the types those refer
;;;; to, their enumerants and constants, and their commands.  The registry
;;;; and its schema are described in the Vulkan registry documentation
;;;; (KhronosGroup/Vulkan-Docs, registry.adoc).

(defparameter +api+ "vulkan"
  "The API whose definitions are read: vk.xml also defines Vulkan SC's.")

;; An extension's enumerants are numbered from a range of its own:
;; base + (extension number - 1) * range size + offset.
(defconstant +extension-enum-base+ 1000000000)
(defconstant +extension-enum-range+ 1000)

(defun registry-pathname ()
  (asdf:system-relative-pathname :vulkan-cffi "registry/vk.xml"))

(defun api-p (element)
  "Whether ELEMENT applies to +API+: it names no API, or names it."
  (let ((api (attribute element "api")))
    (or (null api) (member +api+ (split-list api) :test #'string=))))

(defstruct (registry (:constructor %make-registry))
  ;; Name -> <type>, <enums> (of an enumerated type), <enum> (of any
  ;; <enums>), and <command>.
  (types (make-hash-table :test 'equal))
  (enums (make-hash-table :test 'equal))
  (enumerants (make-hash-table :test 'equal))
  (commands (make-hash-table :test 'equal))
  (features '())
  (extensions '()))

(defun read-registry (&optional (pathname (registry-pathname)))
  (let* ((document (read-registry-document pathname))
         (root (child-element document "registry"))
         (registry (%make-registry)))
    (flet ((store (table key element)
             ;; The first definition for +API+ of each name.
             (when (and key (api-p element))
               (unless (gethash key table)
                 (setf (gethash key table) element)))))
      (dolist (types (child-elements root "types"))
        (dolist (type (child-elements types "type"))
          (store (registry-types registry) (element-name type) type)))
      (dolist (enums (child-elements root "enums"))
        (store (registry-enums registry) (attribute enums "name") enums)
        (dolist (enum (child-elements enums "enum"))
          (store (registry-enumerants registry) (attribute enum "name") enum)))
      (dolist (commands (child-elements root "commands"))
        (dolist (command (child-elements commands "command"))
          (store (registry-commands registry)
                 (element-name command)
                 command)))
      (setf (registry-features registry)
            (remove-if-not #'api-p (child-elements root "feature"))
            (registry-extensions registry)
            (loop for extensions in (child-elements root "extensions")
                  append (child-elements extensions "extension"))))
    registry))

;;; Selection

(defstruct (selection (:constructor %make-selection (registry)))
  registry
  ;; The features and extensions selected.
  (names (make-hash-table :test 'equal))
  ;; Types and commands required, in the order first required.
  (types (make-hash-table :test 'equal))
  (type-order '())
  (commands (make-hash-table :test 'equal))
  (command-order '())
  ;; Enumerated type -> its enumerants, ((NAME . VALUE) ...), in order.
  (enum-values (make-hash-table :test 'equal))
  ;; Constants not of an enumerated type, ((NAME . VALUE) ...).
  (constants '()))

(defun enumerant-value (enum &optional extension-number)
  "The value of the <enum> ENUM, or (:ALIAS NAME); EXTENSION-NUMBER is
that of the extension requiring it, for one numbered by offset."
  (let ((value (attribute enum "value"))
        (bit (attribute enum "bitpos"))
        (offset (attribute enum "offset"))
        (alias (attribute enum "alias")))
    (cond (value (parse-value value))
          (bit (ash 1 (parse-integer bit)))
          (offset
           (let ((number (parse-integer (or (attribute enum "extnumber") extension-number))))
             (* (if (equal (attribute enum "dir") "-") -1 1)
                (+ +extension-enum-base+
                   (* (1- number) +extension-enum-range+)
                   (parse-integer offset)))))
          (alias (list :alias alias)))))

(defparameter +c-types+
  '(("void" . :void) ("char" . :char) ("float" . :float) ("double" . :double)
    ("int8_t" . :int8) ("uint8_t" . :uint8) ("int16_t" . :int16) ("uint16_t" . :uint16)
    ("int32_t" . :int32) ("uint32_t" . :uint32) ("int64_t" . :int64) ("uint64_t" . :uint64)
    ("size_t" . :size) ("int" . :int))
  "The C types the registry refers to, as CFFI's.")

(defparameter +platform-types+
  '(("Window" . :unsigned-long) ("VisualID" . :unsigned-long)
    ("xcb_window_t" . :uint32) ("xcb_visualid_t" . :uint32))
  "The window systems' types the registry passes by value, as CFFI's,
from their headers (X11/X.h, xcb/xproto.h); those passed by pointer need
none.")

(defun add-enumerant (selection type name value)
  (let ((values (gethash type (selection-enum-values selection))))
    (unless (assoc name values :test #'string=)
      (setf (gethash type (selection-enum-values selection))
            (append values (list (cons name value)))))))

(defun require-type (selection name)
  (let ((registry (selection-registry selection)))
    (unless (or (gethash name (selection-types selection))
                (assoc name +c-types+ :test #'string=)
                (assoc name +platform-types+ :test #'string=))
      (let ((type (gethash name (registry-types registry))))
        (unless type
          (error "The Vulkan registry has no type ~a" name))
        (setf (gethash name (selection-types selection)) type)
        (push name (selection-type-order selection))
        (let ((alias (attribute type "alias")))
          (when alias
            (require-type selection alias)))
        (let ((category (attribute type "category")))
          (cond
            ((member category '("struct" "union") :test #'equal)
             (dolist (member (child-elements type "member"))
               (when (api-p member)
                 (require-type selection (child-text member "type")))))
            ((equal category "bitmask")
             (let ((bits (or (attribute type "requires") (attribute type "bitvalues")))
                   (base (child-text type "type")))
               (when bits (require-type selection bits))
               (when base (require-type selection base))))
            ((equal category "basetype")
             (let ((base (child-text type "type")))
               (when base (require-type selection base))))
            ((equal category "enum")
             (let ((enums (gethash name (registry-enums registry))))
               (when enums
                 (dolist (enum (child-elements enums "enum"))
                   (when (api-p enum)
                     (add-enumerant selection name (attribute enum "name")
                                    (enumerant-value enum)))))))))))))

(defun require-command (selection name)
  (unless (gethash name (selection-commands selection))
    (let ((command (gethash name (registry-commands (selection-registry selection)))))
      (unless command
        (error "The Vulkan registry has no command ~a" name))
      (setf (gethash name (selection-commands selection)) command)
      (push name (selection-command-order selection))
      (let ((alias (attribute command "alias")))
        (if alias
            (require-command selection alias)
            (progn
              (require-type selection (child-text (child-element command "proto") "type"))
              (dolist (param (child-elements command "param"))
                (when (api-p param)
                  (require-type selection (child-text param "type"))))))))))

(defun require-enumerant (selection enum extension-number)
  (let ((name (attribute enum "name"))
        (extends (attribute enum "extends")))
    (cond
      (extends
       (require-type selection extends)
       (add-enumerant selection extends name (enumerant-value enum extension-number)))
      ((or (attribute enum "value") (attribute enum "alias"))
       (pushnew (cons name (enumerant-value enum)) (selection-constants selection)
                :key #'car :test #'string=))
      (t
       ;; A constant defined elsewhere, as the API constants are.
       (let ((definition (gethash name (registry-enumerants (selection-registry selection)))))
         (unless definition
           (error "The Vulkan registry has no constant ~a" name))
         (pushnew (cons name (enumerant-value definition)) (selection-constants selection)
                  :key #'car :test #'string=))))))

(defun select-registry (registry &key version extensions)
  "What the core VERSION (\"1.3\") and the named EXTENSIONS require of
REGISTRY, as a SELECTION."
  (let* ((selection (%make-selection registry))
         (features (remove-if-not (lambda (feature)
                                    (version<= (attribute feature "number") version))
                                  (registry-features registry)))
         (extensions
           (loop for name in extensions
                 for extension = (find name (registry-extensions registry)
                                       :key #'element-name :test #'string=)
                 do (unless (and extension
                                 (member +api+ (split-list (attribute extension "supported"))
                                         :test #'string=))
                      (error "No Vulkan extension ~a" name))
                 collect extension))
         (blocks (append features extensions)))
    (dolist (block blocks)
      (setf (gethash (element-name block) (selection-names selection)) t))
    (dolist (block blocks)
      (dolist (require (child-elements block "require"))
        (when (and (api-p require)
                   (let ((depends (attribute require "depends")))
                     (or (null depends)
                         (depends-satisfied-p depends (selection-names selection)))))
          (dolist (item (child-elements require))
            (when (api-p item)
              (let ((tag (plump:tag-name item)))
                (cond ((string= tag "type") (require-type selection (element-name item)))
                      ((string= tag "command") (require-command selection (element-name item)))
                      ((string= tag "enum")
                       (require-enumerant selection item (attribute block "number"))))))))))
    (setf (selection-type-order selection) (reverse (selection-type-order selection))
          (selection-command-order selection) (reverse (selection-command-order selection))
          (selection-constants selection) (reverse (selection-constants selection)))
    selection))
