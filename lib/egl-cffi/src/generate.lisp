(in-package #:egl-cffi.generator)

;;;; Generating CFFI definitions from the EGL registry, egl.xml
;;;; (KhronosGroup/EGL-Registry, api/egl.xml): the commands and constants a
;;;; version and a set of extensions require.  Names become Lisp's:
;;;; eglGetPlatformDisplay is GET-PLATFORM-DISPLAY and
;;;; EGL_PLATFORM_WAYLAND_KHR +PLATFORM-WAYLAND-KHR+.  Commands are called
;;;; through entry points LOAD-EGL looks up.

(defparameter +api+ "egl")
(defparameter +command-prefix+ "egl")
(defparameter +constant-prefix+ "EGL_")

(defparameter +platform-types+
  '(("EGLint" . :int32)
    ;; EGL 1.5's platform functions take the native display and window as
    ;; pointers; the older ones take them as eglplatform.h's types, which
    ;; for X11 and Wayland are a pointer and a pointer-sized integer.
    ("EGLNativeDisplayType" . :pointer)
    ("EGLNativeWindowType" . :uintptr)
    ("EGLNativePixmapType" . :uintptr)
    ("khronos_utime_nanoseconds_t" . :uint64)
    ("khronos_stime_nanoseconds_t" . :int64)
    ("khronos_uint64_t" . :uint64)
    ("khronos_ssize_t" . :long))
  "The types the registry takes from eglplatform.h and khrplatform.h, as
CFFI's.")

(defparameter +c-types+
  '(("void" . :void) ("char" . :char) ("int" . :int) ("unsigned int" . :uint)
    ("intptr_t" . :intptr))
  "The C types the registry's typedefs are made of, as CFFI's.")

;;; Entry points, filled in by the generated definitions and loaded by the
;;; runtime.

(defvar *command-names* (vector) "Each command's name in the registry.")
(defvar *command-pointers* (vector) "Each command's entry point, null until loaded.")
(defvar *constant-names* '() "The constants' values and names, ((VALUE . NAME) ...).")

(defun command-not-loaded (index)
  (error "EGL command ~a is not loaded" (svref *command-names* index)))

(declaim (inline command-pointer))
(defun command-pointer (index)
  (let ((pointer (svref *command-pointers* index)))
    (if (cffi:null-pointer-p pointer) (command-not-loaded index) pointer)))

;;; The registry

(defstruct (registry (:constructor %make-registry))
  ;; Name -> <type>, <enum> and <command>.
  (types (make-hash-table :test 'equal))
  (enums (make-hash-table :test 'equal))
  (commands (make-hash-table :test 'equal))
  (features '())
  (extensions '()))

(defun registry-pathname ()
  (asdf:system-relative-pathname :egl-cffi "registry/egl.xml"))

(defun read-registry (&optional (pathname (registry-pathname)))
  (let* ((root (child-element (read-registry-document pathname) "registry"))
         (registry (%make-registry)))
    (flet ((store (table key element)
             (when (and key (not (gethash key table)))
               (setf (gethash key table) element))))
      (dolist (types (child-elements root "types"))
        (dolist (type (child-elements types "type"))
          (store (registry-types registry) (element-name type) type)))
      (dolist (enums (child-elements root "enums"))
        (dolist (enum (child-elements enums "enum"))
          (store (registry-enums registry) (attribute enum "name") enum)))
      (dolist (commands (child-elements root "commands"))
        (dolist (command (child-elements commands "command"))
          (store (registry-commands registry) (element-name command) command)))
      (setf (registry-features registry)
            (remove-if-not (lambda (feature)
                             (member +api+ (split-list (attribute feature "api"))
                                     :test #'string=))
                           (child-elements root "feature"))
            (registry-extensions registry)
            (loop for extensions in (child-elements root "extensions")
                  append (child-elements extensions "extension"))))
    registry))

(defun select-registry (registry &key version extensions)
  "The commands and constants (C names, in order) that VERSION (\"1.5\")
and the named EXTENSIONS require."
  (let ((blocks (append (remove-if-not (lambda (feature)
                                         (version<= (attribute feature "number") version))
                                       (registry-features registry))
                        (loop for name in extensions
                              for extension = (find name (registry-extensions registry)
                                                    :key #'element-name :test #'string=)
                              do (unless (and extension
                                              (member +api+ (split-list
                                                             (attribute extension "supported"))
                                                      :test #'string=))
                                   (error "No EGL extension ~a" name))
                              collect extension)))
        (commands '())
        (constants '()))
    (dolist (block blocks)
      (dolist (require (child-elements block "require"))
        (dolist (item (child-elements require))
          (let ((name (element-name item))
                (tag (plump:tag-name item)))
            (cond ((string= tag "command") (pushnew name commands :test #'string=))
                  ((string= tag "enum") (pushnew name constants :test #'string=)))))))
    (values (reverse commands) (reverse constants))))

;;; Types

(defun foreign-type (registry name pointers)
  "The CFFI type of POINTERS pointers to the registry type NAME."
  (cond
    ((plusp pointers) :pointer)
    ((cdr (assoc name +platform-types+ :test #'string=)))
    ((cdr (assoc name +c-types+ :test #'string=)))
    (t
     (let ((type (gethash name (registry-types registry))))
       (unless type
         (error "The EGL registry has no type ~a" name))
       ;; typedef BASE *NAME; or typedef BASE NAME;
       (let* ((text (plump:text type))
              (base (string-trim " " (subseq text (length "typedef ")
                                             (search name text)))))
         (cond ((find #\* base) :pointer)
               ((find #\( base) :pointer)
               (t (foreign-type registry base 0))))))))

;;; Names

(defun lisp-symbol (name)
  (intern (string-upcase name) '#:egl-cffi))

(defun command-symbol (name)
  (lisp-symbol (lisp-words (without-prefix +command-prefix+ name))))

(defun constant-symbol (name)
  (lisp-symbol (format nil "+~a+" (lisp-words (string-downcase
                                                (without-prefix +constant-prefix+ name))))))

;;; Definitions

(defun constant-forms (registry constants)
  (loop for name in constants
        for enum = (gethash name (registry-enums registry))
        do (unless enum (error "The EGL registry has no constant ~a" name))
        collect `(defconstant ,(constant-symbol name) ,(parse-value (attribute enum "value")))))

(defun command-forms (registry commands)
  (loop for name in commands
        for index from 0
        for command = (or (gethash name (registry-commands registry))
                          (error "The EGL registry has no command ~a" name))
        for params = (loop for param in (child-elements command "param")
                           collect (multiple-value-list (parse-declaration param)))
        collect `(defun ,(command-symbol name)
                     ,(mapcar (lambda (param) (lisp-symbol (lisp-words (first param)))) params)
                   ,name
                   (cffi:foreign-funcall-pointer
                    (command-pointer ,index) ()
                    ,@(loop for (param-name type pointers) in params
                            append (list (foreign-type registry type pointers)
                                         (lisp-symbol (lisp-words param-name))))
                    ,(multiple-value-bind (proto-name type pointers)
                         (parse-declaration (child-element command "proto"))
                       (declare (ignore proto-name))
                       (foreign-type registry type pointers))))))

(defun generate-bindings (registry commands constants)
  (let* ((forms (append (constant-forms registry constants)
                        (command-forms registry commands)))
         (symbols (mapcar #'second forms)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',symbols '#:egl-cffi))
       ,@forms
       (setf *command-names* ,(coerce commands 'simple-vector)
             *command-pointers* (make-array ,(length commands)
                                            :initial-element (cffi:null-pointer))
             *constant-names*
             ',(loop for name in constants
                     for value = (parse-value (attribute (gethash name (registry-enums registry))
                                                         "value"))
                     collect (cons value name))))))

(defmacro define-egl-bindings (&key version extensions)
  "Define the bindings for EGL VERSION (\"1.5\") and the named EXTENSIONS,
from the registry."
  (let ((registry (read-registry)))
    (multiple-value-bind (commands constants)
        (select-registry registry :version version :extensions extensions)
      (generate-bindings registry commands constants))))
