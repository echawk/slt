(in-package #:vulkan-cffi.generator)

;;;; Loading Vulkan and its commands, checking results, and filling in
;;;; structures.  The Vulkan loader is opened by LOAD-VULKAN, not when this
;;;; is loaded, so a program can carry these bindings and still run where
;;;; there is no Vulkan.

(cffi:define-foreign-library vulkan-loader
  (:darwin (:or "libvulkan.1.dylib" "libvulkan.dylib"))
  (:unix (:or "libvulkan.so.1" "libvulkan.so"))
  (:windows "vulkan-1.dll"))

(defparameter +get-instance-proc-addr+ "vkGetInstanceProcAddr"
  "The one command the loader is asked for directly; it gives the others.")

;;; Results

(define-condition vulkan-error (error)
  ((result :initarg :result :reader vulkan-error-result)
   (command :initarg :command :initform nil :reader vulkan-error-command))
  (:report (lambda (condition stream)
             (let ((result (vulkan-error-result condition)))
               (format stream "~@[~a: ~]~a (~d)" (vulkan-error-command condition)
                       (or (cdr (assoc result *result-names*)) "unknown result")
                       result)))))

(defun check-result (result &optional command)
  "RESULT, a VkResult, unless it is an error (negative): then signal
VULKAN-ERROR, naming COMMAND."
  (if (minusp result)
      (error 'vulkan-error :result result :command command)
      result))

;;; Commands

(defun command-index (name)
  (or (position name *command-names* :test #'string=)
      (error "No Vulkan command ~a in these bindings" name)))

(defun load-commands (levels lookup)
  "Set the entry point of each command of one of LEVELS to what LOOKUP
returns for its name, a null pointer when it is not available."
  (loop for name across *command-names*
        for level across *command-levels*
        for index from 0
        when (member level levels)
          do (setf (svref *command-pointers* index)
                   (cffi:with-foreign-string (c-name name)
                     (funcall lookup c-name)))))

(defun load-vulkan ()
  "Open the Vulkan loader and load the global commands: those that
enumerate the instance's layers and extensions, and CREATE-INSTANCE."
  (cffi:load-foreign-library 'vulkan-loader)
  (setf (svref *command-pointers* (command-index +get-instance-proc-addr+))
        (or (cffi:foreign-symbol-pointer +get-instance-proc-addr+)
            (error "The Vulkan loader has no ~a" +get-instance-proc-addr+)))
  (load-commands '(:global) (lambda (name) (vulkan-cffi::get-instance-proc-addr (cffi:null-pointer) name))))

(defun load-instance-commands (instance)
  "Load the commands of INSTANCE, and of its devices until
LOAD-DEVICE-COMMANDS loads theirs more directly."
  (load-commands '(:instance :device) (lambda (name) (vulkan-cffi::get-instance-proc-addr instance name))))

(defun load-device-commands (device)
  "Load the commands of DEVICE straight from its driver."
  (load-commands '(:device) (lambda (name) (vulkan-cffi::get-device-proc-addr device name))))

(defun command-loaded-p (name)
  "Whether the command NAME (\"vkCreateMetalSurfaceEXT\") is loaded."
  (not (cffi:null-pointer-p (svref *command-pointers* (command-index name)))))

;;; Versions, as VK_MAKE_API_VERSION packs them.

(defconstant +api-version-variant-shift+ 29)
(defconstant +api-version-major-shift+ 22)
(defconstant +api-version-minor-shift+ 12)

(defun make-api-version (variant major minor patch)
  (logior (ash variant +api-version-variant-shift+)
          (ash major +api-version-major-shift+)
          (ash minor +api-version-minor-shift+)
          patch))

;;; Structures

(defun structure-type-of (name)
  "The sType of the structure NAME (INSTANCE-CREATE-INFO), or NIL when it
has none."
  (values (gethash name *structure-types*)))

(defun zero-struct (pointer type)
  "Fill the foreign TYPE at POINTER with zeros."
  (dotimes (i (cffi:foreign-type-size type) pointer)
    (setf (cffi:mem-aref pointer :uint8 i) 0)))

(defun slot-symbol (keyword)
  (or (find-symbol (symbol-name keyword) '#:vulkan-cffi)
      (error "No Vulkan structure member ~s" keyword)))

(defmacro with-struct ((var type &rest slots) &body body)
  "Run BODY with VAR a zeroed TYPE structure (INSTANCE-CREATE-INFO) on the
stack, its sType set and the members SLOTS (:P-NEXT pointer ...) stored."
  (let ((foreign-type `(:struct ,type)))
    `(cffi:with-foreign-object (,var ',foreign-type)
       (zero-struct ,var ',foreign-type)
       (let ((structure-type (load-time-value (structure-type-of ',type))))
         (when structure-type
           (setf (cffi:foreign-slot-value ,var ',foreign-type 'vulkan-cffi::s-type) structure-type)))
       ,@(loop for (slot value) on slots by #'cddr
               collect `(setf (cffi:foreign-slot-value ,var ',foreign-type ',(slot-symbol slot))
                              ,value))
       ,@body)))

(defmacro with-structs ((&rest bindings) &body body)
  "WITH-STRUCT for each of BINDINGS in turn, so later ones may refer to
earlier ones."
  (if bindings
      `(with-struct ,(first bindings) (with-structs ,(rest bindings) ,@body))
      `(progn ,@body)))
