(in-package #:slt)

;;;; Minimal Objective-C messaging for the macOS backend, on top of the
;;;; runtime bindings in cocoas (codeberg.org/shinmera/cocoas).
;;;;
;;;; On arm64 every message goes through objc_msgSend, including those
;;;; returning floats or structs.  Struct arguments follow AAPCS64: a
;;;; homogeneous float aggregate such as NSRect or CGSize travels in FP
;;;; registers exactly like its fields passed one by one, so callers
;;;; flatten them into :double arguments; structs larger than 16 bytes that
;;;; are not float aggregates (MTLRegion) are passed as a pointer.  Struct
;;;; results are read through key-value coding instead (see OBJC-RECT).

#-arm64 (warn "slt's Cocoa backend has only been written for arm64 calling conventions.")

(cffi:define-foreign-library metal-framework (:darwin (:framework "Metal")))
(cffi:define-foreign-library quartz-core-framework (:darwin (:framework "QuartzCore")))
(cffi:define-foreign-library core-text-framework (:darwin (:framework "CoreText")))
(cffi:define-foreign-library core-graphics-framework (:darwin (:framework "CoreGraphics")))

(defvar *objc-initialized* nil)

(defun ensure-objc ()
  (unless *objc-initialized*
    (org.shirakumo.cocoas:init)
    (cffi:use-foreign-library metal-framework)
    (cffi:use-foreign-library quartz-core-framework)
    (cffi:use-foreign-library core-text-framework)
    (cffi:use-foreign-library core-graphics-framework)
    (setf *objc-initialized* t)))

(defmacro selector (name)
  "The selector NAME, registered on first use and cached at the call site."
  (let ((cache (gensym "CACHE")))
    `(let ((,cache (load-time-value (cons nil nil))))
       (or (car ,cache)
           (setf (car ,cache) (org.shirakumo.cocoas.cffi:register-name ,name))))))

(defmacro msg (receiver selector &rest arguments)
  "Send SELECTOR to RECEIVER (an object pointer, or a string naming a class).
ARGUMENTS alternate type and value and end with the return type, which
defaults to :POINTER."
  (when (evenp (length arguments))
    (setf arguments (append arguments '(:pointer))))
  `(cffi:foreign-funcall "objc_msgSend"
                         :pointer ,(if (stringp receiver)
                                       `(org.shirakumo.cocoas.cffi:get-class ,receiver)
                                       receiver)
                         :pointer (selector ,selector)
                         ,@arguments))

(defun ns-string (string)
  "An autoreleased NSString holding STRING."
  (msg "NSString" "stringWithUTF8String:" :string string))

(defun ns-string->string (pointer)
  (unless (cffi:null-pointer-p pointer)
    (msg pointer "UTF8String" :string)))

(cffi:defcfun ("objc_autoreleasePoolPush" autorelease-pool-push) :pointer)
(cffi:defcfun ("objc_autoreleasePoolPop" autorelease-pool-pop) :void (pool :pointer))

(defmacro with-autorelease-pool (() &body body)
  (let ((pool (gensym "POOL")))
    `(let ((,pool (autorelease-pool-push)))
       (unwind-protect (progn ,@body)
         (autorelease-pool-pop ,pool)))))

(defun objc-rect (object key)
  "Read the NSRect-valued property KEY of OBJECT as (values X Y WIDTH HEIGHT)."
  (let ((value (msg object "valueForKey:" :pointer (ns-string key))))
    (cffi:with-foreign-object (rect :double 4)
      (msg value "getValue:size:" :pointer rect :unsigned-long 32 :void)
      (values (cffi:mem-aref rect :double 0) (cffi:mem-aref rect :double 1)
              (cffi:mem-aref rect :double 2) (cffi:mem-aref rect :double 3)))))

(cffi:defcfun ("objc_allocateClassPair" %objc-allocate-class-pair) :pointer
  (superclass :pointer) (name :string) (extra-bytes :size))
(cffi:defcfun ("objc_registerClassPair" %objc-register-class-pair) :void (class :pointer))
(cffi:defcfun ("class_addMethod" %class-add-method) :bool
  (class :pointer) (selector :pointer) (implementation :pointer) (types :string))

(defun define-objc-subclass (name superclass methods)
  "Register class NAME inheriting from SUPERCLASS (a class name) unless it
exists.  METHODS is a list of (SELECTOR CALLBACK-NAME TYPE-ENCODING)."
  (let ((existing (org.shirakumo.cocoas.cffi:get-class name)))
    (if (not (cffi:null-pointer-p existing))
        existing
        (let ((class (%objc-allocate-class-pair
                      (org.shirakumo.cocoas.cffi:get-class superclass) name 0)))
          (when (cffi:null-pointer-p class)
            (error "Could not allocate Objective-C class ~a." name))
          (loop for (selector callback types) in methods
                do (%class-add-method class (org.shirakumo.cocoas.cffi:register-name selector)
                                      (cffi:get-callback callback) types))
          (%objc-register-class-pair class)
          class))))
