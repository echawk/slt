(in-package #:wayland-cffi.generator)

;;;; libwayland-client: connecting, dispatching, and the generated
;;;; interfaces' requests and events.  The libraries are opened by
;;;; LOAD-WAYLAND and LOAD-WAYLAND-EGL, not when this is loaded, so a
;;;; program can carry these bindings and run where there is no Wayland.

(cffi:define-foreign-library wayland-client
  (:unix (:or "libwayland-client.so.0" "libwayland-client.so"))
  (t (:default "libwayland-client")))

(cffi:define-foreign-library wayland-egl
  (:unix (:or "libwayland-egl.so.1" "libwayland-egl.so"))
  (t (:default "libwayland-egl")))

;;; The libraries' functions, called through entry points looked up when
;;; the library is loaded.

(defvar *library-functions* '()
  "Each library function: (SYMBOL C-NAME LIBRARY).")

(defvar *entry-points* (make-hash-table :test 'eq)
  "Each loaded library function's entry point, by its symbol.")

(defun entry-point (symbol)
  (or (gethash symbol *entry-points*)
      (error "~a is not loaded (see LOAD-WAYLAND)"
             (second (assoc symbol *library-functions*)))))

(defmacro define-library-function ((name c-name library) return-type &rest parameters)
  "Define NAME, calling C-NAME of LIBRARY with PARAMETERS, (NAME TYPE) each."
  `(progn
     (pushnew (list ',name ,c-name ',library) *library-functions* :key #'first)
     (defun ,name ,(mapcar #'first parameters)
       (cffi:foreign-funcall-pointer (entry-point ',name) ()
                                     ,@(loop for (parameter type) in parameters
                                             append (list type parameter))
                                     ,return-type))))

(defun load-library (library)
  "Open LIBRARY, unless it is open, and look up its functions."
  (unless (cffi:foreign-library-loaded-p library)
    (cffi:load-foreign-library library))
  (loop for (symbol c-name owner) in *library-functions*
        when (eq owner library)
          do (setf (gethash symbol *entry-points*)
                   (or (cffi:foreign-symbol-pointer c-name :library library)
                       (error "~(~a~) has no ~a" library c-name)))))

(define-library-function (%display-connect "wl_display_connect" wayland-client) :pointer
  (name :pointer))
(define-library-function (display-disconnect "wl_display_disconnect" wayland-client) :void
  (display :pointer))
(define-library-function (display-fd "wl_display_get_fd" wayland-client) :int
  (display :pointer))
(define-library-function (display-flush "wl_display_flush" wayland-client) :int
  (display :pointer))
(define-library-function (display-roundtrip "wl_display_roundtrip" wayland-client) :int
  (display :pointer))
(define-library-function (display-dispatch-pending "wl_display_dispatch_pending" wayland-client)
    :int
  (display :pointer))
(define-library-function (display-prepare-read "wl_display_prepare_read" wayland-client) :int
  (display :pointer))
(define-library-function (display-read-events "wl_display_read_events" wayland-client) :int
  (display :pointer))
(define-library-function (display-cancel-read "wl_display_cancel_read" wayland-client) :void
  (display :pointer))
(define-library-function (display-error "wl_display_get_error" wayland-client) :int
  (display :pointer))
;; libwayland 1.20 and later.
(define-library-function (%marshal-array-flags "wl_proxy_marshal_array_flags" wayland-client)
    :pointer
  (proxy :pointer) (opcode :uint32) (interface :pointer) (version :uint32) (flags :uint32)
  (arguments :pointer))
(define-library-function (%add-dispatcher "wl_proxy_add_dispatcher" wayland-client) :int
  (proxy :pointer) (dispatcher :pointer) (dispatcher-data :pointer) (data :pointer))
(define-library-function (%proxy-destroy "wl_proxy_destroy" wayland-client) :void
  (proxy :pointer))
(define-library-function (proxy-version "wl_proxy_get_version" wayland-client) :uint32
  (proxy :pointer))

(define-library-function (egl-window-create "wl_egl_window_create" wayland-egl) :pointer
  (surface :pointer) (width :int) (height :int))
(define-library-function (egl-window-resize "wl_egl_window_resize" wayland-egl) :void
  (window :pointer) (width :int) (height :int) (dx :int) (dy :int))
(define-library-function (egl-window-destroy "wl_egl_window_destroy" wayland-egl) :void
  (window :pointer))

;;; Errors

(define-condition wayland-error (error)
  ((code :initarg :code :reader wayland-error-code)
   (operation :initarg :operation :initform nil :reader wayland-error-operation))
  (:report (lambda (condition stream)
             (format stream "~@[~a: ~]~a" (wayland-error-operation condition)
                     (cffi:foreign-funcall "strerror" :int (wayland-error-code condition)
                                                      :string)))))

(defun check-display (result display &optional operation)
  "RESULT, unless it is negative, libwayland's failure: then signal
WAYLAND-ERROR with DISPLAY's error, naming OPERATION."
  (if (minusp result)
      (error 'wayland-error :code (display-error display) :operation operation)
      result))

(defun display-connect (&optional name)
  "A connection to the compositor's socket NAME, by default
$WAYLAND_DISPLAY's, or NIL when there is none."
  (let ((display (if name
                     (cffi:with-foreign-string (c-name name) (%display-connect c-name))
                     (%display-connect (cffi:null-pointer)))))
    (and (not (cffi:null-pointer-p display)) display)))

;;; The interfaces, as libwayland's structures

(cffi:defcstruct wl-message
  (name :pointer)
  (signature :pointer)
  (types :pointer))

(cffi:defcstruct wl-interface
  (name :pointer)
  (version :int)
  (method-count :int)
  (methods :pointer)
  (event-count :int)
  (events :pointer))

(cffi:defcunion wl-argument
  (i :int32) (u :uint32) (f :int32) (s :pointer) (o :pointer) (n :uint32) (a :pointer) (h :int32))

(cffi:defcstruct wl-array
  (size :size)
  (alloc :size)
  (data :pointer))

(defvar *interface-descriptions* '()
  "The interfaces the generated requests use: see INTERFACE-DESCRIPTION.")

(defvar *interfaces* (make-hash-table :test 'equal)
  "Each interface's wl_interface, by name, once LOAD-WAYLAND made them.
They live as long as the program.")

(defun interface-pointer (name)
  (or (gethash name *interfaces*)
      (error "No Wayland interface ~a is loaded" name)))

(defun make-type-array (types)
  "An array of the interfaces named TYPES; those of protocols not bound
here, and NIL, are null."
  (let ((array (cffi:foreign-alloc :pointer :count (max 1 (length types)))))
    (loop for type in types
          for index from 0
          do (setf (cffi:mem-aref array :pointer index)
                   (or (and type (gethash type *interfaces*)) (cffi:null-pointer))))
    array))

(defun make-messages (messages)
  "A wl_message array of MESSAGES, (NAME SIGNATURE TYPES) each."
  (let ((array (cffi:foreign-alloc '(:struct wl-message) :count (max 1 (length messages)))))
    (loop for (name signature types) in messages
          for index from 0
          for message = (cffi:mem-aptr array '(:struct wl-message) index)
          do (setf (cffi:foreign-slot-value message '(:struct wl-message) 'name)
                   (cffi:foreign-string-alloc name)
                   (cffi:foreign-slot-value message '(:struct wl-message) 'signature)
                   (cffi:foreign-string-alloc signature)
                   (cffi:foreign-slot-value message '(:struct wl-message) 'types)
                   (make-type-array types)))
    array))

(defun make-interfaces ()
  (dolist (description *interface-descriptions*)
    (setf (gethash (first description) *interfaces*)
          (cffi:foreign-alloc '(:struct wl-interface))))
  (loop for (name version requests events) in *interface-descriptions*
        for interface = (gethash name *interfaces*)
        do (macrolet ((slot (slot-name)
                        `(cffi:foreign-slot-value interface '(:struct wl-interface) ',slot-name)))
             (setf (slot name) (cffi:foreign-string-alloc name)
                   (slot version) version
                   (slot method-count) (length requests)
                   (slot methods) (make-messages requests)
                   (slot event-count) (length events)
                   (slot events) (make-messages events)))))

(defun wayland-loaded-p ()
  (cffi:foreign-library-loaded-p 'wayland-client))

(defun load-wayland ()
  "Open libwayland-client, unless it is open, and make the interfaces."
  (load-library 'wayland-client)
  (when (zerop (hash-table-count *interfaces*))
    (make-interfaces))
  t)

(defun load-wayland-egl ()
  "Open libwayland-egl, for EGL-WINDOW-CREATE and the rest."
  (load-library 'wayland-egl)
  t)

;;; Requests and events.  Each proxy's events go to one Lisp function, its
;;; listener, through a single dispatcher, which decodes them by their
;;; signatures.

(defvar *listeners* (make-hash-table)
  "Each proxy's listener, by the proxy's address.")

(defvar *messages* (make-hash-table)
  "Each event's (NAME . CODES), by its wl_message's address.")

(defconstant +marshal-flag-destroy+ 1
  "WL_MARSHAL_FLAG_DESTROY: destroy the proxy once the request is sent.")

(defconstant +no-flags+ 0)

(defconstant +fixed-fraction-bits+ 8
  "wl_fixed_t is a signed 24.8 fixed-point number.")

(defconstant +no-version+ 0
  "The version passed for a request that makes no object; it is not used.")

(defun signature-codes (signature)
  "SIGNATURE's argument characters, without the version and nullable marks."
  (remove-if (lambda (char) (or (digit-char-p char) (char= char +nullable-mark+))) signature))

(defun foreign-array (octets)
  "A wl_array of OCTETS, and its data, to free."
  (let ((array (cffi:foreign-alloc '(:struct wl-array)))
        (data (cffi:foreign-alloc :uint8 :count (max 1 (length octets)))))
    (loop for octet across octets
          for index from 0
          do (setf (cffi:mem-aref data :uint8 index) octet))
    (setf (cffi:foreign-slot-value array '(:struct wl-array) 'size) (length octets)
          (cffi:foreign-slot-value array '(:struct wl-array) 'alloc) (length octets)
          (cffi:foreign-slot-value array '(:struct wl-array) 'data) data)
    (values array data)))

(defun send-request (proxy opcode signature interface version destroy-p values)
  "Send PROXY the request OPCODE, of SIGNATURE, with VALUES: one for each
argument but a new object's.  A new object is made of the interface named
INTERFACE at VERSION, and returned.  When DESTROY-P, PROXY is destroyed."
  (let* ((codes (signature-codes signature))
         (allocations '()))
    (cffi:with-foreign-object (arguments '(:union wl-argument) (max 1 (length codes)))
      (unwind-protect
           (progn
             (loop for code across codes
                   for index from 0
                   for argument = (cffi:mem-aptr arguments '(:union wl-argument) index)
                   for kind = (argument-kind code)
                   for value = (unless (eq kind :new-id) (pop values))
                   do (macrolet ((slot (name)
                                   `(cffi:foreign-slot-value argument '(:union wl-argument) ',name)))
                        (ecase kind
                          (:int (setf (slot i) value))
                          (:uint (setf (slot u) value))
                          (:fd (setf (slot h) value))
                          (:fixed (setf (slot f) (round (* value (ash 1 +fixed-fraction-bits+)))))
                          (:string (setf (slot s)
                                         (if value
                                             (first (push (cffi:foreign-string-alloc value)
                                                          allocations))
                                             (cffi:null-pointer))))
                          (:object (setf (slot o) (or value (cffi:null-pointer))))
                          (:new-id (setf (slot o) (cffi:null-pointer)))
                          (:array (multiple-value-bind (array data) (foreign-array value)
                                    (push array allocations)
                                    (push data allocations)
                                    (setf (slot a) array))))))
             (when destroy-p
               (remhash (cffi:pointer-address proxy) *listeners*))
             (let ((result (%marshal-array-flags
                            proxy opcode
                            (if interface (interface-pointer interface) (cffi:null-pointer))
                            (or version +no-version+)
                            (if destroy-p +marshal-flag-destroy+ +no-flags+)
                            arguments)))
               (when interface
                 (when (cffi:null-pointer-p result)
                   (error "No ~a could be made" interface))
                 result)))
        (mapc #'cffi:foreign-free allocations)))))

(defun destroy-proxy (proxy)
  "Destroy PROXY, an object with no destructor request (a wl_callback)."
  (remhash (cffi:pointer-address proxy) *listeners*)
  (%proxy-destroy proxy))

;;; Events

(defun event-message (message)
  (let ((address (cffi:pointer-address message)))
    (or (gethash address *messages*)
        (setf (gethash address *messages*)
              (cffi:with-foreign-slots ((name signature) message (:struct wl-message))
                (cons (intern (substitute #\- +word-separator+
                                          (string-upcase (cffi:foreign-string-to-lisp name)))
                              :keyword)
                      (signature-codes (cffi:foreign-string-to-lisp signature))))))))

(defun array-octets (array)
  (cffi:with-foreign-slots ((size data) array (:struct wl-array))
    (let ((octets (make-array size :element-type '(unsigned-byte 8))))
      (dotimes (index size octets)
        (setf (aref octets index) (cffi:mem-aref data :uint8 index))))))

(defun event-values (codes arguments)
  (loop for code across codes
        for index from 0
        for argument = (cffi:mem-aptr arguments '(:union wl-argument) index)
        collect (macrolet ((slot (name)
                             `(cffi:foreign-slot-value argument '(:union wl-argument) ',name)))
                  (ecase (argument-kind code)
                    (:int (slot i))
                    (:uint (slot u))
                    (:fd (slot h))
                    (:fixed (/ (slot f) (float (ash 1 +fixed-fraction-bits+) 1d0)))
                    (:string (let ((pointer (slot s)))
                               (and (not (cffi:null-pointer-p pointer))
                                    (cffi:foreign-string-to-lisp pointer))))
                    ;; libwayland has made a new object's proxy.
                    ((:object :new-id) (let ((pointer (slot o)))
                                         (and (not (cffi:null-pointer-p pointer)) pointer)))
                    (:array (array-octets (slot a)))))))

(cffi:defcallback dispatch-event :int ((data :pointer) (proxy :pointer) (opcode :uint32)
                                       (message :pointer) (arguments :pointer))
  (declare (ignore data opcode))
  (let ((listener (gethash (cffi:pointer-address proxy) *listeners*)))
    (when listener
      (destructuring-bind (name . codes) (event-message message)
        ;; libwayland is in the middle of dispatching: report rather than
        ;; unwind through it.
        (handler-case (apply listener name (event-values codes arguments))
          (error (condition)
            (format *error-output* "~&wayland-cffi: ~a event: ~a~%" name condition))))))
  0)

(defun set-listener (proxy function)
  "Call FUNCTION with each event PROXY receives: the event's name as a
keyword (:CONFIGURE) and its arguments, objects as proxies (NIL for a null
one), fixed-point numbers as doubles and arrays as octet vectors.  An
error in FUNCTION is reported on *ERROR-OUTPUT*, not signalled through
libwayland."
  (let ((address (cffi:pointer-address proxy)))
    (unless (gethash address *listeners*)
      (when (minusp (%add-dispatcher proxy (cffi:callback dispatch-event)
                                     (cffi:null-pointer) (cffi:null-pointer)))
        (error "The proxy ~a already has a listener" proxy)))
    (setf (gethash address *listeners*) function)))
