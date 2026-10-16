(in-package #:slt)

;;;; Native macOS backend: an NSWindow whose content view is layer-hosting
;;;; with a CAMetalLayer, glyphs rasterized by CoreText at the backing
;;;; scale, and the grid drawn by the Metal renderer.  All sizes the
;;;; frontend sees are device pixels.

(defvar *option-as-meta* t
  "When true, the Option key acts as Meta instead of composing characters.")

(defclass cocoa-backend (terminal-backend)
  ((view :initform nil :accessor cocoa-backend-view)
   (timers :initform '() :accessor cocoa-backend-timers)
   (timer-seq :initform 0 :accessor cocoa-backend-timer-seq)
   (quit-p :initform nil :accessor cocoa-backend-quit-p)
   ;; Work posted from other threads (see BACKEND-WAKEUP).
   (wake-lock :initform (bt:make-lock "slt cocoa wakeup") :reader cocoa-backend-wake-lock)
   (wake-thunks :initform '() :accessor cocoa-backend-wake-thunks)))

(defstruct (cocoa-view (:constructor %make-cocoa-view))
  backend
  window
  ns-view
  delegate
  renderer
  font
  atlas
  (instances nil)
  (grid nil)
  (scale 1d0 :type double-float)
  (cell-width 1 :type fixnum)
  (cell-height 1 :type fixnum)
  resize-callback
  keypress-callback
  scroll-callback
  close-callback
  (closed-p nil)
  ;; Fractional rows from trackpad scrolling, carried between events.
  (scroll-remainder 0d0 :type double-float)
  (live-resize-p nil))

;;; Objective-C objects call back into Lisp through these addresses.
(defvar *cocoa-views* (make-hash-table))

(defun cocoa-view-for (pointer)
  (gethash (cffi:pointer-address pointer) *cocoa-views*))

;;; Keyboard

(defparameter *mac-keycode-keysyms*
  '((36 . "Return") (48 . "Tab") (51 . "BackSpace") (53 . "Escape")
    (76 . "KP_Enter") (114 . "Insert") (115 . "Home") (116 . "Prior")
    (117 . "Delete") (119 . "End") (121 . "Next")
    (123 . "Left") (124 . "Right") (125 . "Down") (126 . "Up")
    (122 . "F1") (120 . "F2") (99 . "F3") (118 . "F4") (96 . "F5") (97 . "F6")
    (98 . "F7") (100 . "F8") (101 . "F9") (109 . "F10") (103 . "F11") (111 . "F12"))
  "Virtual key codes of keys whose characters are not useful to a terminal.")

(defconstant +ns-shift-key-mask+ (ash 1 17))
(defconstant +ns-control-key-mask+ (ash 1 18))
(defconstant +ns-option-key-mask+ (ash 1 19))
(defconstant +ns-command-key-mask+ (ash 1 20))

(defun ns-modifiers->state (flags)
  (logior (if (logtest flags +ns-shift-key-mask+) +shift-mask+ 0)
          (if (logtest flags +ns-control-key-mask+) +control-mask+ 0)
          (if (and *option-as-meta* (logtest flags +ns-option-key-mask+)) +mod1-mask+ 0)
          (if (logtest flags +ns-command-key-mask+) +mod4-mask+ 0)))

(defun ns-event-key-data (event)
  "Translate an NSKeyDown event into the (CHAR KEYSYM STATE) the frontend
expects, or NIL for keys the terminal should not see."
  (let* ((flags (msg event "modifierFlags" :unsigned-long))
         (keycode (msg event "keyCode" :unsigned-short))
         (state (ns-modifiers->state flags))
         (keysym (cdr (assoc keycode *mac-keycode-keysyms*)))
         (text (ns-string->string
                (if (or (logtest flags +ns-control-key-mask+)
                        (and *option-as-meta* (logtest flags +ns-option-key-mask+)))
                    (msg event "charactersIgnoringModifiers")
                    (msg event "characters")))))
    (cond
      ((logtest flags +ns-command-key-mask+) nil)
      (keysym (list "" keysym state))
      ((and text (plusp (length text)))
       ;; Shift is already reflected in the character.
       (list text text (logand state (lognot +shift-mask+))))
      (t nil))))

(cffi:defcallback slt-view-accepts-first-responder :bool ((self :pointer) (command :pointer))
  (declare (ignore self command))
  t)

(cffi:defcallback slt-view-key-down :void ((self :pointer) (command :pointer) (event :pointer))
  (declare (ignore command))
  (let ((view (cocoa-view-for self)))
    (when (and view (cocoa-view-keypress-callback view))
      (let ((data (ns-event-key-data event)))
        (when data
          (handler-case (apply (cocoa-view-keypress-callback view) data)
            (error (condition)
              (format *error-output* "~&slt: key handler error: ~a~%" condition))))))))

(cffi:defcallback slt-view-scroll-wheel :void ((self :pointer) (command :pointer) (event :pointer))
  (declare (ignore command))
  (let ((view (cocoa-view-for self)))
    (when (and view (cocoa-view-scroll-callback view))
      (let* ((delta (msg event "scrollingDeltaY" :double))
             (precise (msg event "hasPreciseScrollingDeltas" :bool))
             ;; Trackpads report points, wheels report lines.
             (rows (+ (cocoa-view-scroll-remainder view)
                      (if precise
                          (/ delta (/ (cocoa-view-cell-height view) (cocoa-view-scale view)))
                          (* 3 delta))))
             (whole (truncate rows)))
        (setf (cocoa-view-scroll-remainder view) (- rows whole))
        (unless (zerop whole)
          ;; Scrolling content down (positive delta) moves back in history.
          (funcall (cocoa-view-scroll-callback view) (- whole)))))))

(cffi:defcallback slt-window-will-start-live-resize :void
    ((self :pointer) (command :pointer) (notification :pointer))
  (declare (ignore command notification))
  (let ((view (cocoa-view-for self)))
    (when view
      (setf (cocoa-view-live-resize-p view) t)
      (msg (metal-renderer-layer (cocoa-view-renderer view)) "setPresentsWithTransaction:"
           :bool t :void))))

(cffi:defcallback slt-window-did-end-live-resize :void
    ((self :pointer) (command :pointer) (notification :pointer))
  (declare (ignore command notification))
  (let ((view (cocoa-view-for self)))
    (when view
      (setf (cocoa-view-live-resize-p view) nil)
      (msg (metal-renderer-layer (cocoa-view-renderer view)) "setPresentsWithTransaction:"
           :bool nil :void))))

(cffi:defcallback slt-window-will-close :void ((self :pointer) (command :pointer) (notification :pointer))
  (declare (ignore command notification))
  (let ((view (cocoa-view-for self)))
    (when (and view (not (cocoa-view-closed-p view)))
      (setf (cocoa-view-closed-p view) t)
      (when (cocoa-view-close-callback view)
        (funcall (cocoa-view-close-callback view)))
      (setf (cocoa-backend-quit-p (cocoa-view-backend view)) t))))

(cffi:defcallback slt-window-did-resize :void ((self :pointer) (command :pointer) (notification :pointer))
  (declare (ignore command notification))
  (let ((view (cocoa-view-for self)))
    (when view
      (update-cocoa-drawable-size view)
      (when (cocoa-view-resize-callback view)
        ;; AppKit runs its own loop during a drag, so resize now rather
        ;; than through a timer, and run work the reader thread posted so
        ;; the program's redraw appears while dragging.
        (funcall (cocoa-view-resize-callback view) (cocoa-view-live-resize-p view))
        (run-cocoa-wake-thunks (cocoa-view-backend view)))
      (redraw-cocoa-view view))))

(defun register-cocoa-classes ()
  (define-objc-subclass "SLTTerminalView" "NSView"
                        '(("acceptsFirstResponder" slt-view-accepts-first-responder "B@:")
                          ("keyDown:" slt-view-key-down "v@:@")
                          ("scrollWheel:" slt-view-scroll-wheel "v@:@")))
  (define-objc-subclass "SLTWindowDelegate" "NSObject"
                        '(("windowWillClose:" slt-window-will-close "v@:@")
                          ("windowDidResize:" slt-window-did-resize "v@:@")
                          ("windowWillStartLiveResize:" slt-window-will-start-live-resize "v@:@")
                          ("windowDidEndLiveResize:" slt-window-did-end-live-resize "v@:@"))))

;;; Application setup and event loop

(defun install-application-menu (app)
  (let ((bar (msg (msg "NSMenu" "alloc") "init"))
        (app-item (msg (msg "NSMenuItem" "alloc") "init"))
        (app-menu (msg (msg "NSMenu" "alloc") "init"))
        (quit (msg (msg "NSMenuItem" "alloc") "initWithTitle:action:keyEquivalent:"
                   :pointer (ns-string "Quit slt")
                   :pointer (selector "terminate:")
                   :pointer (ns-string "q"))))
    (msg app-menu "addItem:" :pointer quit :void)
    (msg app-item "setSubmenu:" :pointer app-menu :void)
    (msg bar "addItem:" :pointer app-item :void)
    (msg app "setMainMenu:" :pointer bar :void)))

(defun cocoa-pump-events (app timeout)
  "Dispatch pending events, waiting up to TIMEOUT seconds for the first."
  (with-autorelease-pool ()
    (let ((mode (foreign-global "NSDefaultRunLoopMode"))
          (date (msg "NSDate" "dateWithTimeIntervalSinceNow:"
                     :double (coerce timeout 'double-float))))
      (loop for event = (msg app "nextEventMatchingMask:untilDate:inMode:dequeue:"
                             :uint64 (1- (ash 1 64)) :pointer date :pointer mode :bool t)
            until (cffi:null-pointer-p event)
            do (msg app "sendEvent:" :pointer event :void)
               (setf date (msg "NSDate" "distantPast")))
      (msg app "updateWindows" :void))))

(defconstant +ns-event-type-application-defined+ 15)

(defun post-wakeup-event ()
  "Unblock the main thread's event wait.  Safe from any thread."
  (float-features:with-float-traps-masked t
    (with-autorelease-pool ()
      (let ((event (msg "NSEvent"
                        "otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:"
                        :unsigned-long +ns-event-type-application-defined+
                        :double 0d0 :double 0d0
                        :unsigned-long 0 :double 0d0 :long 0 :pointer (cffi:null-pointer)
                        :short 0 :long 0 :long 0)))
        (msg (msg "NSApplication" "sharedApplication") "postEvent:atStart:"
             :pointer event :bool nil :void)))))

(defmethod backend-wakeup ((backend cocoa-backend) view thunk)
  (declare (ignore view))
  (bt:with-lock-held ((cocoa-backend-wake-lock backend))
    (push thunk (cocoa-backend-wake-thunks backend)))
  (post-wakeup-event)
  t)

(defun run-cocoa-wake-thunks (backend)
  (let ((thunks (bt:with-lock-held ((cocoa-backend-wake-lock backend))
                  (shiftf (cocoa-backend-wake-thunks backend) '()))))
    (dolist (thunk (nreverse thunks))
      (funcall thunk))))

(defun run-cocoa-timers (backend)
  (let ((now (get-internal-real-time))
        (due '()))
    (setf (cocoa-backend-timers backend)
          (remove-if (lambda (timer)
                       (when (<= (first timer) now)
                         (push timer due)
                         t))
                     (cocoa-backend-timers backend)))
    (dolist (timer (sort due #'< :key #'first))
      (funcall (third timer)))))

(defun next-timer-timeout (backend)
  (let ((timers (cocoa-backend-timers backend)))
    (if timers
        (max 0 (/ (- (reduce #'min timers :key #'first) (get-internal-real-time))
                  internal-time-units-per-second))
        0.25)))

(defmethod backend-call-with-event-loop ((backend cocoa-backend) thunk)
  (float-features:with-float-traps-masked t
    (ensure-objc)
    (register-cocoa-classes)
    (let ((app (msg "NSApplication" "sharedApplication")))
      (msg app "setActivationPolicy:" :long 0 :bool)
      (install-application-menu app)
      (msg app "finishLaunching" :void)
      (unwind-protect
           (progn
             (funcall thunk)
             (msg app "activateIgnoringOtherApps:" :bool t :void)
             (loop until (cocoa-backend-quit-p backend)
                   do (cocoa-pump-events app (min 0.25 (next-timer-timeout backend)))
                      (run-cocoa-wake-thunks backend)
                      (run-cocoa-timers backend)))
        (let ((view (cocoa-backend-view backend)))
          (when view
            (destroy-cocoa-view view)))))))

;;; Views

(defun cocoa-view-pixel-size (view)
  (multiple-value-bind (x y width height) (objc-rect (cocoa-view-ns-view view) "frame")
    (declare (ignore x y))
    (values (round (* width (cocoa-view-scale view)))
            (round (* height (cocoa-view-scale view))))))

(defun update-cocoa-drawable-size (view)
  (multiple-value-bind (width height) (cocoa-view-pixel-size view)
    (metal-set-drawable-size (cocoa-view-renderer view) width height)))

(defun redraw-cocoa-view (view &optional (palette *palette*))
  (let ((grid (cocoa-view-grid view)))
    (when (and grid (not (cocoa-view-closed-p view)))
      (setf (cocoa-view-instances view)
            (pack-grid grid (cocoa-view-atlas view) palette (cocoa-view-instances view)))
      (multiple-value-bind (width height) (cocoa-view-pixel-size view)
        (metal-draw (cocoa-view-renderer view)
                    (cocoa-view-instances view)
                    (grid-columns grid)
                    (* (grid-rows grid) (grid-columns grid))
                    (cocoa-view-cell-width view)
                    (cocoa-view-cell-height view)
                    width height
                    (cocoa-view-atlas view)
                    palette)))))

(defun snapshot-cocoa-view (view pathname &optional (palette *palette*))
  "Render VIEW's current grid offscreen and write it to PATHNAME as a PPM."
  (let ((grid (cocoa-view-grid view)))
    (multiple-value-bind (width height) (cocoa-view-pixel-size view)
      (setf (cocoa-view-instances view)
            (pack-grid grid (cocoa-view-atlas view) palette (cocoa-view-instances view)))
      (write-ppm pathname
                 (metal-snapshot (cocoa-view-renderer view) (cocoa-view-instances view)
                                 (grid-columns grid) (* (grid-rows grid) (grid-columns grid))
                                 (cocoa-view-cell-width view) (cocoa-view-cell-height view)
                                 width height (cocoa-view-atlas view) palette)
                 width height))))

(defun destroy-cocoa-view (view)
  (remhash (cffi:pointer-address (cocoa-view-ns-view view)) *cocoa-views*)
  (remhash (cffi:pointer-address (cocoa-view-delegate view)) *cocoa-views*)
  (ignore-errors (free-metal-renderer (cocoa-view-renderer view)))
  (ignore-errors (free-coretext-font (cocoa-view-font view)))
  (setf (cocoa-view-closed-p view) t))

(defmethod backend-available-font-families ((backend cocoa-backend))
  '())

(defmethod backend-resolve-font-size ((backend cocoa-backend) font-size)
  (abs (or font-size 15)))

(defmethod backend-create-view ((backend cocoa-backend)
                                &key rows columns font-family font-size
                                  cell-width cell-height title)
  (let* ((screen-scale (msg (msg "NSScreen" "mainScreen") "backingScaleFactor" :double))
         (font (make-coretext-font font-family (* font-size screen-scale)))
         (cw (or (and cell-width (round (* cell-width screen-scale)))
                 (coretext-font-cell-width font)))
         (ch (or (and cell-height (round (* cell-height screen-scale)))
                 (coretext-font-cell-height font)))
         (window (msg (msg "NSWindow" "alloc") "initWithContentRect:styleMask:backing:defer:"
                      :double 0d0 :double 0d0
                      :double (/ (* columns cw) screen-scale)
                      :double (/ (* rows ch) screen-scale)
                      :unsigned-long 15 :unsigned-long 2 :bool nil))
         (ns-view (msg (msg "SLTTerminalView" "alloc") "init"))
         (delegate (msg (msg "SLTWindowDelegate" "alloc") "init"))
         (renderer (make-metal-renderer))
         (view (%make-cocoa-view :backend backend
                                 :window window
                                 :ns-view ns-view
                                 :delegate delegate
                                 :renderer renderer
                                 :font font
                                 :atlas (make-glyph-atlas cw ch (coretext-rasterizer font))
                                 :scale (msg window "backingScaleFactor" :double)
                                 :cell-width cw
                                 :cell-height ch)))
    (setf (gethash (cffi:pointer-address ns-view) *cocoa-views*) view
          (gethash (cffi:pointer-address delegate) *cocoa-views*) view
          (cocoa-backend-view backend) view)
    (msg (metal-renderer-layer renderer) "setContentsScale:" :double (cocoa-view-scale view) :void)
    (msg ns-view "setLayer:" :pointer (metal-renderer-layer renderer) :void)
    (msg ns-view "setWantsLayer:" :bool t :void)
    (msg window "setContentView:" :pointer ns-view :void)
    (msg window "setDelegate:" :pointer delegate :void)
    (msg window "setReleasedWhenClosed:" :bool nil :void)
    ;; A dark title bar to match the terminal.
    (msg window "setAppearance:"
         :pointer (msg "NSAppearance" "appearanceNamed:"
                       :pointer (foreign-global "NSAppearanceNameDarkAqua"))
         :void)
    (msg window "setTitle:" :pointer (ns-string title) :void)
    (msg window "center" :void)
    (msg window "makeKeyAndOrderFront:" :pointer (cffi:null-pointer) :void)
    (msg window "makeFirstResponder:" :pointer ns-view :bool)
    (update-cocoa-drawable-size view)
    view))

(defmethod backend-view-cell-width ((backend cocoa-backend) (view cocoa-view))
  (cocoa-view-cell-width view))

(defmethod backend-view-cell-height ((backend cocoa-backend) (view cocoa-view))
  (cocoa-view-cell-height view))

(defmethod backend-view-window-size ((backend cocoa-backend) (view cocoa-view))
  (cocoa-view-pixel-size view))

(defmethod backend-resize-view ((backend cocoa-backend) (view cocoa-view) rows columns)
  (declare (ignore rows columns))
  (update-cocoa-drawable-size view)
  view)

(defmethod backend-render-grid ((backend cocoa-backend) (view cocoa-view) grid)
  (setf (cocoa-view-grid view) grid)
  (redraw-cocoa-view view))

(defmethod backend-set-title ((backend cocoa-backend) (view cocoa-view) title)
  (with-autorelease-pool ()
    (msg (cocoa-view-window view) "setTitle:" :pointer (ns-string title) :void)))

(defmethod backend-schedule ((backend cocoa-backend) (view cocoa-view) delay-ms thunk)
  (let ((token (incf (cocoa-backend-timer-seq backend))))
    (push (list (+ (get-internal-real-time)
                   (round (* (max 0 delay-ms) internal-time-units-per-second) 1000))
                token
                thunk)
          (cocoa-backend-timers backend))
    token))

(defmethod backend-cancel-scheduled ((backend cocoa-backend) (view cocoa-view) token)
  (setf (cocoa-backend-timers backend)
        (remove token (cocoa-backend-timers backend) :key #'second)))

(defmethod backend-bind-resize ((backend cocoa-backend) (view cocoa-view) callback)
  (setf (cocoa-view-resize-callback view) callback))

(defmethod backend-bind-keypress ((backend cocoa-backend) (view cocoa-view) callback)
  (setf (cocoa-view-keypress-callback view) callback))

(defmethod backend-bind-scroll ((backend cocoa-backend) (view cocoa-view) callback)
  (setf (cocoa-view-scroll-callback view) callback))

(defmethod backend-on-close ((backend cocoa-backend) (view cocoa-view) callback)
  (setf (cocoa-view-close-callback view) callback))

(defmethod backend-focus ((backend cocoa-backend) (view cocoa-view))
  (msg (cocoa-view-window view) "makeKeyAndOrderFront:" :pointer (cffi:null-pointer) :void))

(defmethod backend-request-exit ((backend cocoa-backend) (view cocoa-view))
  (setf (cocoa-backend-quit-p backend) t)
  (unless (cocoa-view-closed-p view)
    (setf (cocoa-view-closed-p view) t)
    (msg (cocoa-view-window view) "close" :void)))

(register-backend :cocoa (lambda () (make-instance 'cocoa-backend)))
