(in-package #:slt)

;;;; Native macOS backend: an NSWindow whose content view is layer-hosting
;;;; with a CAMetalLayer, glyphs rasterized by CoreText at the backing
;;;; scale, and the grid drawn by the Metal renderer.  All sizes the
;;;; frontend sees are device pixels.

(defclass cocoa-backend (terminal-backend)
  ((views :initform '() :accessor cocoa-backend-views)
   (timer-seq :initform 0 :accessor cocoa-backend-timer-seq)
   (quit-p :initform nil :accessor cocoa-backend-quit-p)
   ;; Work posted from other threads (see BACKEND-WAKEUP), and the run
   ;; loop source that runs it.
   (wake-lock :initform (bt:make-lock "slt cocoa wakeup") :reader cocoa-backend-wake-lock)
   (wake-thunks :initform '() :accessor cocoa-backend-wake-thunks)
   (wake-source :initform nil :accessor cocoa-backend-wake-source)))

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
  (image-store nil)
  (scale 1d0 :type double-float)
  (cell-width 1 :type fixnum)
  (cell-height 1 :type fixnum)
  resize-callback
  keypress-callback
  scroll-callback
  mouse-callback
  close-callback
  focus-callback
  drop-callback
  ;; The font's family and size in points, for changing its size.
  font-family
  (font-size 15)
  ;; Device pixels between the window's edges and the grid.
  (padding 0 :type fixnum)
  ;; The target object of slt's menu items, and their (LABEL FUNCTION)
  ;; entries, indexed by tag.
  menu-target
  (menu-items (make-array 0 :adjustable t :fill-pointer t) :type vector)
  commands
  lisp-menu-items
  ;; The right button's menu, shown when the child does not track the mouse.
  context-menu
  (mouse-tracking nil)
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
                (if (or (logtest flags (logior +ns-control-key-mask+ +ns-command-key-mask+))
                        (and *option-as-meta* (logtest flags +ns-option-key-mask+)))
                    (msg event "charactersIgnoringModifiers")
                    (msg event "characters")))))
    (cond
      (keysym (list "" keysym state))
      ;; Command combinations arrive too, for the frontend's shortcuts.
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

(defun deliver-cocoa-mouse (self event action &optional (button 1))
  (let ((view (cocoa-view-for self)))
    (when (and view (cocoa-view-mouse-callback view))
      (multiple-value-bind (x y) (objc-point event "locationInWindow")
        (multiple-value-bind (left bottom width height) (objc-rect self "frame")
          (declare (ignore left bottom width))
          (let ((scale (cocoa-view-scale view))
                (padding (cocoa-view-padding view)))
            ;; Window coordinates start at the bottom left.
            (funcall (cocoa-view-mouse-callback view)
                     action
                     (floor (- (* (- height y) scale) padding) (cocoa-view-cell-height view))
                     (floor (- (* x scale) padding) (cocoa-view-cell-width view))
                     (if (eq action :motion) 0 (msg event "clickCount" :long))
                     button
                     (ns-modifiers->state (msg event "modifierFlags" :unsigned-long)))))))))

(defmacro define-cocoa-mouse-callbacks (&rest definitions)
  "Define view callbacks from (NAME ACTION BUTTON); BUTTON :OTHER is the
middle button, which Cocoa numbers 2 among its other buttons."
  `(progn
     ,@(loop for (name action button) in definitions
             collect `(cffi:defcallback ,name :void
                          ((self :pointer) (command :pointer) (event :pointer))
                        (declare (ignore command))
                        ,(if (eq button :other)
                             `(when (= (msg event "buttonNumber" :long) 2)
                                (deliver-cocoa-mouse self event ,action 2))
                             `(deliver-cocoa-mouse self event ,action ,button))))))

(cffi:defcallback slt-view-right-mouse-down :void ((self :pointer) (command :pointer) (event :pointer))
  (declare (ignore command))
  (let ((view (cocoa-view-for self)))
    (if (and view (cocoa-view-context-menu view) (not (cocoa-view-mouse-tracking view)))
        (msg "NSMenu" "popUpContextMenu:withEvent:forView:"
             :pointer (cocoa-view-context-menu view) :pointer event :pointer self :void)
        (deliver-cocoa-mouse self event :press 3))))

(define-cocoa-mouse-callbacks
  (slt-view-mouse-down :press 1)
  (slt-view-mouse-dragged :drag 1)
  (slt-view-mouse-up :release 1)
  (slt-view-right-mouse-dragged :drag 3)
  (slt-view-right-mouse-up :release 3)
  (slt-view-other-mouse-down :press :other)
  (slt-view-other-mouse-dragged :drag :other)
  (slt-view-other-mouse-up :release :other)
  (slt-view-mouse-moved :motion nil))

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
    (when view
      (unless (cocoa-view-closed-p view)
        (setf (cocoa-view-closed-p view) t)
        (when (cocoa-view-close-callback view)
          (funcall (cocoa-view-close-callback view))))
      (retire-cocoa-view view))))

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

(defun call-cocoa-focus-callback (self focused)
  (let ((view (cocoa-view-for self)))
    (when (and view focused (cocoa-view-commands view))
      ;; The application menu is global, so point it at the key window.
      (backend-bind-commands (cocoa-view-backend view) view
                             (cocoa-view-commands view))
      (backend-bind-menu (cocoa-view-backend view) view
                         (cocoa-view-lisp-menu-items view)))
    (when (and view (cocoa-view-focus-callback view))
      (handler-case (funcall (cocoa-view-focus-callback view) focused)
        (error (condition)
          (format *error-output* "~&slt: focus handler error: ~a~%" condition))))))

(cffi:defcallback slt-window-did-become-key :void
    ((self :pointer) (command :pointer) (notification :pointer))
  (declare (ignore command notification))
  (call-cocoa-focus-callback self t))

(cffi:defcallback slt-window-did-resign-key :void
    ((self :pointer) (command :pointer) (notification :pointer))
  (declare (ignore command notification))
  (call-cocoa-focus-callback self nil))

(defconstant +ns-drag-operation-copy+ 1)

(cffi:defcallback slt-view-dragging-entered :unsigned-long
    ((self :pointer) (command :pointer) (info :pointer))
  (declare (ignore self command info))
  +ns-drag-operation-copy+)

(defun dragged-file-names (info)
  (let* ((classes (msg "NSArray" "arrayWithObject:"
                       :pointer (org.shirakumo.cocoas.cffi:get-class "NSURL")))
         (urls (msg (msg info "draggingPasteboard") "readObjectsForClasses:options:"
                    :pointer classes :pointer (cffi:null-pointer))))
    (unless (cffi:null-pointer-p urls)
      (loop for index below (msg urls "count" :unsigned-long)
            for url = (msg urls "objectAtIndex:" :unsigned-long index)
            when (msg url "isFileURL" :bool)
              collect (ns-string->string (msg url "path"))))))

(cffi:defcallback slt-view-perform-drag-operation :bool
    ((self :pointer) (command :pointer) (info :pointer))
  (declare (ignore command))
  (let ((view (cocoa-view-for self)))
    (when (and view (cocoa-view-drop-callback view))
      (let ((names (dragged-file-names info)))
        (when names
          (handler-case (progn (funcall (cocoa-view-drop-callback view) names) t)
            (error (condition)
              (format *error-output* "~&slt: drop handler error: ~a~%" condition)
              nil)))))))

(defun cocoa-menu-item-entry (self item)
  "The (LABEL FUNCTION) entry of the Lisp menu's ITEM, sent to target SELF."
  (let ((view (cocoa-view-for self))
        (tag (msg item "tag" :long)))
    (when (and view (< -1 tag (length (cocoa-view-menu-items view))))
      (aref (cocoa-view-menu-items view) tag))))

(cffi:defcallback slt-menu-item-chosen :void ((self :pointer) (command :pointer) (item :pointer))
  (declare (ignore command))
  (let ((entry (cocoa-menu-item-entry self item)))
    (when entry
      (handler-case (funcall (second entry))
        (error (condition)
          (format *error-output* "~&slt: menu item error: ~a~%" condition))))))

(cffi:defcallback slt-validate-menu-item :bool ((self :pointer) (command :pointer) (item :pointer))
  (declare (ignore command))
  ;; Called as the menu opens: refresh labels computed by functions.
  (let ((label (first (cocoa-menu-item-entry self item))))
    (when (and label (not (stringp label)))
      (let ((title (ignore-errors (funcall label))))
        (when (stringp title)
          (msg item "setTitle:" :pointer (ns-string title) :void)))))
  t)

(defun register-cocoa-classes ()
  (define-objc-subclass "SLTTerminalView" "NSView"
                        '(("acceptsFirstResponder" slt-view-accepts-first-responder "B@:")
                          ("keyDown:" slt-view-key-down "v@:@")
                          ("scrollWheel:" slt-view-scroll-wheel "v@:@")
                          ("mouseDown:" slt-view-mouse-down "v@:@")
                          ("mouseDragged:" slt-view-mouse-dragged "v@:@")
                          ("mouseUp:" slt-view-mouse-up "v@:@")
                          ("rightMouseDown:" slt-view-right-mouse-down "v@:@")
                          ("rightMouseDragged:" slt-view-right-mouse-dragged "v@:@")
                          ("rightMouseUp:" slt-view-right-mouse-up "v@:@")
                          ("otherMouseDown:" slt-view-other-mouse-down "v@:@")
                          ("otherMouseDragged:" slt-view-other-mouse-dragged "v@:@")
                          ("otherMouseUp:" slt-view-other-mouse-up "v@:@")
                          ("mouseMoved:" slt-view-mouse-moved "v@:@")
                          ("draggingEntered:" slt-view-dragging-entered "Q@:@")
                          ("performDragOperation:" slt-view-perform-drag-operation "B@:@")))
  (define-objc-subclass "SLTWindowDelegate" "NSObject"
                        '(("windowWillClose:" slt-window-will-close "v@:@")
                          ("windowDidBecomeKey:" slt-window-did-become-key "v@:@")
                          ("windowDidResignKey:" slt-window-did-resign-key "v@:@")
                          ("windowDidResize:" slt-window-did-resize "v@:@")
                          ("windowWillStartLiveResize:" slt-window-will-start-live-resize "v@:@")
                          ("windowDidEndLiveResize:" slt-window-did-end-live-resize "v@:@")))
  (define-objc-subclass "SLTMenuTarget" "NSObject"
                        '(("sltMenuItemChosen:" slt-menu-item-chosen "v@:@")
                          ("validateMenuItem:" slt-validate-menu-item "B@:@"))))

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

;;; Work from other threads and timers run from the main run loop in its
;;; common modes.  Those include the modes AppKit runs it in while the user
;;; drags a window edge, so a program's redraw after a resize appears
;;; during the drag rather than when the mouse is released.

(cffi:defcfun ("CFRunLoopGetMain" %cf-run-loop-get-main) :pointer)
(cffi:defcfun ("CFRunLoopWakeUp" %cf-run-loop-wake-up) :void (run-loop :pointer))
(cffi:defcfun ("CFRunLoopSourceCreate" %cf-run-loop-source-create) :pointer
  (allocator :pointer) (order :long) (context :pointer))
(cffi:defcfun ("CFRunLoopAddSource" %cf-run-loop-add-source) :void
  (run-loop :pointer) (source :pointer) (mode :pointer))
(cffi:defcfun ("CFRunLoopSourceSignal" %cf-run-loop-source-signal) :void (source :pointer))
(cffi:defcfun ("CFAbsoluteTimeGetCurrent" %cf-absolute-time-get-current) :double)
(cffi:defcfun ("CFRunLoopTimerCreate" %cf-run-loop-timer-create) :pointer
  (allocator :pointer) (fire-date :double) (interval :double) (flags :unsigned-long)
  (order :long) (callout :pointer) (context :pointer))
(cffi:defcfun ("CFRunLoopAddTimer" %cf-run-loop-add-timer) :void
  (run-loop :pointer) (timer :pointer) (mode :pointer))
(cffi:defcfun ("CFRunLoopTimerInvalidate" %cf-run-loop-timer-invalidate) :void (timer :pointer))

(defun call-from-run-loop (thunk)
  "Call THUNK from a run loop callback, which must not unwind into C."
  (handler-case (funcall thunk)
    (error (condition)
      (format *error-output* "~&slt: ~a~%" condition))))

(defvar *cocoa-wake-backend* nil
  "The backend whose posted work the run loop source runs.")

(defun run-cocoa-wake-thunks (backend)
  (let ((thunks (bt:with-lock-held ((cocoa-backend-wake-lock backend))
                  (shiftf (cocoa-backend-wake-thunks backend) '()))))
    (dolist (thunk (nreverse thunks))
      (call-from-run-loop thunk))))

(cffi:defcallback slt-run-loop-perform :void ((info :pointer))
  (declare (ignore info))
  (when *cocoa-wake-backend*
    (run-cocoa-wake-thunks *cocoa-wake-backend*)))

(defun install-cocoa-wake-source (backend)
  ;; CFRunLoopSourceContext: version, info, retain, release,
  ;; copyDescription, equal, hash, schedule, cancel, perform.
  (cffi:with-foreign-object (context :pointer 10)
    (dotimes (i 10)
      (setf (cffi:mem-aref context :pointer i) (cffi:null-pointer)))
    (setf (cffi:mem-aref context :pointer 9) (cffi:callback slt-run-loop-perform))
    (let ((source (%cf-run-loop-source-create (cffi:null-pointer) 0 context)))
      (%cf-run-loop-add-source (%cf-run-loop-get-main) source
                               (foreign-global "kCFRunLoopCommonModes"))
      (setf (cocoa-backend-wake-source backend) source
            *cocoa-wake-backend* backend))))

(defmethod backend-wakeup ((backend cocoa-backend) view thunk)
  (declare (ignore view))
  (bt:with-lock-held ((cocoa-backend-wake-lock backend))
    (push thunk (cocoa-backend-wake-thunks backend)))
  (let ((source (cocoa-backend-wake-source backend)))
    (if source
        (progn (%cf-run-loop-source-signal source)
               (%cf-run-loop-wake-up (%cf-run-loop-get-main)))
        (post-wakeup-event)))
  t)

(defvar *cocoa-timers* (make-hash-table)
  "Scheduled timers: token to (CFRUNLOOPTIMER . THUNK).")

(cffi:defcallback slt-run-loop-timer :void ((timer :pointer) (info :pointer))
  (declare (ignore timer))
  (let* ((token (cffi:pointer-address info))
         (entry (gethash token *cocoa-timers*)))
    (when entry
      (remhash token *cocoa-timers*)
      ;; The run loop keeps its own reference while it calls us.
      (cf-release (car entry))
      (call-from-run-loop (cdr entry)))))

(defmethod backend-call-with-event-loop ((backend cocoa-backend) thunk)
  (float-features:with-float-traps-masked t
    (ensure-objc)
    (register-cocoa-classes)
    (let ((app (msg "NSApplication" "sharedApplication")))
      (msg app "setActivationPolicy:" :long 0 :bool)
      (install-application-menu app)
      (msg app "finishLaunching" :void)
      (install-cocoa-wake-source backend)
      (unwind-protect
           (progn
             (funcall thunk)
             (msg app "activateIgnoringOtherApps:" :bool t :void)
             (loop until (cocoa-backend-quit-p backend)
                   do (cocoa-pump-events app 0.25)
                      (run-cocoa-wake-thunks backend)))
        (dolist (view (copy-list (cocoa-backend-views backend)))
          (destroy-cocoa-view view))))))

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
                    palette
                    (and (cocoa-view-image-store view)
                         (terminal-image-render-list
                          (cocoa-view-image-store view)
                          (cocoa-view-cell-width view)
                          (cocoa-view-cell-height view))))))))

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
                                 width height (cocoa-view-atlas view) palette
                                 (and (cocoa-view-image-store view)
                                      (terminal-image-render-list
                                       (cocoa-view-image-store view)
                                       (cocoa-view-cell-width view)
                                       (cocoa-view-cell-height view))))
                 width height))))

(defun destroy-cocoa-view (view)
  (remhash (cffi:pointer-address (cocoa-view-ns-view view)) *cocoa-views*)
  (when (cocoa-view-menu-target view)
    (remhash (cffi:pointer-address (cocoa-view-menu-target view)) *cocoa-views*))
  (remhash (cffi:pointer-address (cocoa-view-delegate view)) *cocoa-views*)
  (ignore-errors (free-metal-renderer (cocoa-view-renderer view)))
  (ignore-errors (free-coretext-font (cocoa-view-font view)))
  (setf (cocoa-view-closed-p view) t))

(defun retire-cocoa-view (view)
  "Remove VIEW from the application and release it after AppKit finishes
delivering its close notification."
  (let ((backend (cocoa-view-backend view)))
    (when (member view (cocoa-backend-views backend))
      (setf (cocoa-backend-views backend)
            (delete view (cocoa-backend-views backend)))
      (backend-wakeup backend view (lambda () (destroy-cocoa-view view)))
      (when (null (cocoa-backend-views backend))
        (setf (cocoa-backend-quit-p backend) t)))))

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
         (padding (round (* (max 0 *window-padding*) screen-scale)))
         (window (progn
                   ;; slt has no tabs; this keeps AppKit's tab menu items away.
                   (msg "NSWindow" "setAllowsAutomaticWindowTabbing:" :bool nil :void)
                   (msg (msg "NSWindow" "alloc") "initWithContentRect:styleMask:backing:defer:"
                        :double 0d0 :double 0d0
                        :double (/ (+ (* columns cw) (* 2 padding)) screen-scale)
                        :double (/ (+ (* rows ch) (* 2 padding)) screen-scale)
                        :unsigned-long 15 :unsigned-long 2 :bool nil)))
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
                                 :cell-height ch
                                 :font-family font-family
                                 :font-size font-size
                                 :padding padding)))
    (setf (metal-renderer-padding renderer) padding)
    (setf (gethash (cffi:pointer-address ns-view) *cocoa-views*) view
          (gethash (cffi:pointer-address delegate) *cocoa-views*) view
          (cocoa-backend-views backend) (cons view (cocoa-backend-views backend)))
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
    (msg window "setAcceptsMouseMovedEvents:" :bool t :void)
    ;; NSWindowCollectionBehaviorFullScreenPrimary: the green button goes full screen.
    (msg window "setCollectionBehavior:" :unsigned-long 128 :void)
    (msg window "setMinSize:" :double 200d0 :double 100d0 :void)
    (msg ns-view "registerForDraggedTypes:"
         :pointer (msg "NSArray" "arrayWithObject:"
                       :pointer (foreign-global "NSPasteboardTypeFileURL"))
         :void)
    (update-cocoa-drawable-size view)
    view))

(defmethod backend-view-cell-width ((backend cocoa-backend) (view cocoa-view))
  (cocoa-view-cell-width view))

(defmethod backend-view-cell-height ((backend cocoa-backend) (view cocoa-view))
  (cocoa-view-cell-height view))

(defmethod backend-view-window-size ((backend cocoa-backend) (view cocoa-view))
  (multiple-value-bind (width height) (cocoa-view-pixel-size view)
    (let ((padding (* 2 (cocoa-view-padding view))))
      (values (max 1 (- width padding)) (max 1 (- height padding))))))

(defmethod backend-resize-view ((backend cocoa-backend) (view cocoa-view) rows columns)
  (declare (ignore rows columns))
  (update-cocoa-drawable-size view)
  view)

(defmethod backend-render-grid ((backend cocoa-backend) (view cocoa-view) grid)
  (setf (cocoa-view-grid view) grid)
  (redraw-cocoa-view view))

(defmethod backend-render-images ((backend cocoa-backend) (view cocoa-view) store)
  (declare (ignore backend))
  (setf (cocoa-view-image-store view) store)
  (redraw-cocoa-view view))

(defmethod backend-set-title ((backend cocoa-backend) (view cocoa-view) title)
  (with-autorelease-pool ()
    (msg (cocoa-view-window view) "setTitle:" :pointer (ns-string title) :void)))

(defmethod backend-schedule ((backend cocoa-backend) (view cocoa-view) delay-ms thunk)
  (let ((token (incf (cocoa-backend-timer-seq backend))))
    ;; CFRunLoopTimerContext: version, info, retain, release, copyDescription.
    (cffi:with-foreign-object (context :pointer 5)
      (dotimes (i 5)
        (setf (cffi:mem-aref context :pointer i) (cffi:null-pointer)))
      (setf (cffi:mem-aref context :pointer 1) (cffi:make-pointer token))
      (let ((timer (%cf-run-loop-timer-create
                    (cffi:null-pointer)
                    (+ (%cf-absolute-time-get-current) (/ (max 0 delay-ms) 1000d0))
                    0d0 0 0 (cffi:callback slt-run-loop-timer) context)))
        (setf (gethash token *cocoa-timers*) (cons timer thunk))
        (%cf-run-loop-add-timer (%cf-run-loop-get-main) timer
                                (foreign-global "kCFRunLoopCommonModes"))))
    token))

(defmethod backend-cancel-scheduled ((backend cocoa-backend) (view cocoa-view) token)
  (let ((entry (gethash token *cocoa-timers*)))
    (when entry
      (remhash token *cocoa-timers*)
      (%cf-run-loop-timer-invalidate (car entry))
      (cf-release (car entry)))))

(defmethod backend-bind-resize ((backend cocoa-backend) (view cocoa-view) callback)
  (setf (cocoa-view-resize-callback view) callback))

(defmethod backend-bind-keypress ((backend cocoa-backend) (view cocoa-view) callback)
  (setf (cocoa-view-keypress-callback view) callback))

(defmethod backend-bind-scroll ((backend cocoa-backend) (view cocoa-view) callback)
  (setf (cocoa-view-scroll-callback view) callback))

(defmethod backend-bind-mouse ((backend cocoa-backend) (view cocoa-view) callback)
  (setf (cocoa-view-mouse-callback view) callback))

;;; Menus

(defun cocoa-menu-target (view)
  (or (cocoa-view-menu-target view)
      (let ((target (msg (msg "SLTMenuTarget" "alloc") "init")))
        (setf (gethash (cffi:pointer-address target) *cocoa-views*) view
              (cocoa-view-menu-target view) target))))

(defun make-ns-menu-item (title action key &optional (modifiers +ns-command-key-mask+))
  "A menu item sending ACTION (a selector) along the responder chain."
  (let ((item (msg (msg "NSMenuItem" "alloc") "initWithTitle:action:keyEquivalent:"
                   :pointer (ns-string title) :pointer action :pointer (ns-string key))))
    (msg item "setKeyEquivalentModifierMask:" :unsigned-long modifiers :void)
    item))

(defun make-slt-menu-item (view label function &optional (key "") (modifiers +ns-command-key-mask+))
  "A menu item calling FUNCTION, with LABEL a string or a function
returning one (see *MENU-ITEMS*)."
  (let ((item (make-ns-menu-item (if (stringp label) label (funcall label))
                                 (selector "sltMenuItemChosen:") key modifiers))
        (tag (vector-push-extend (list label function) (cocoa-view-menu-items view))))
    (msg item "setTarget:" :pointer (cocoa-menu-target view) :void)
    (msg item "setTag:" :long tag :void)
    item))

(defun make-ns-menu (title items)
  "An NSMenu holding ITEMS, where NIL is a separator."
  (let ((menu (msg (msg "NSMenu" "alloc") "initWithTitle:" :pointer (ns-string title))))
    (dolist (item items menu)
      (msg menu "addItem:" :pointer (or item (msg "NSMenuItem" "separatorItem")) :void))))

(defun add-menu-bar-menu (bar menu &optional index)
  (let ((bar-item (msg (msg "NSMenuItem" "alloc") "init")))
    (msg bar-item "setSubmenu:" :pointer menu :void)
    (if index
        (msg bar "insertItem:atIndex:" :pointer bar-item :long index :void)
        (msg bar "addItem:" :pointer bar-item :void))
    bar-item))

(defmethod backend-bind-commands ((backend cocoa-backend) (view cocoa-view) commands)
  "Replace the menu bar with the standard application, Shell, Edit, View
and Window menus, whose slt items call COMMANDS, and set the right
button's menu."
  (setf (cocoa-view-commands view) commands)
  (flet ((command (name title &optional (key "") (modifiers +ns-command-key-mask+))
           (let ((function (cdr (assoc name commands))))
             (and function (make-slt-menu-item view title function key modifiers))))
         (standard (title action &optional (key "") (modifiers +ns-command-key-mask+))
           (make-ns-menu-item title action key modifiers)))
    (let* ((cmd +ns-command-key-mask+)
           (app (msg "NSApplication" "sharedApplication"))
           (bar (msg (msg "NSMenu" "alloc") "init"))
           (window-menu
             (make-ns-menu "Window"
                           (list (standard "Minimize" (selector "performMiniaturize:") "m")
                                 (standard "Zoom" (selector "performZoom:"))
                                 nil
                                 (standard "Bring All to Front" (selector "arrangeInFront:"))))))
      (dolist (menu
               (list
                (make-ns-menu "slt"
                              (list (standard "About slt" (selector "orderFrontStandardAboutPanel:"))
                                    nil
                                    (command :edit-config "Settings…" ",")
                                    (command :reload-config "Reload Settings" "r"
                                             (logior cmd +ns-shift-key-mask+))
                                    nil
                                    (standard "Hide slt" (selector "hide:") "h")
                                    (standard "Hide Others" (selector "hideOtherApplications:") "h"
                                              (logior cmd +ns-option-key-mask+))
                                    (standard "Show All" (selector "unhideAllApplications:"))
                                    nil
                                    (standard "Quit slt" (selector "terminate:") "q")))
                (make-ns-menu "Shell"
                              (list (command :new-window "New Window" "n")
                                    (standard "Close Window" (selector "performClose:") "w")))
                (make-ns-menu "Edit"
                              (list (command :copy "Copy" "c")
                                    (command :paste "Paste" "v")
                                    (command :select-all "Select All" "a")
                                    nil
                                    (command :clear-scrollback "Clear Scrollback" "k")))
                (make-ns-menu "View"
                              (list (command :bigger "Bigger" "=")
                                    (command :smaller "Smaller" "-")
                                    (command :actual-size "Actual Size" "0")
                                    nil
                                    (standard "Enter Full Screen" (selector "toggleFullScreen:") "f"
                                              (logior cmd +ns-control-key-mask+))))
                window-menu))
        (add-menu-bar-menu bar menu))
      (msg app "setMainMenu:" :pointer bar :void)
      (msg app "setWindowsMenu:" :pointer window-menu :void)
      (setf (cocoa-view-context-menu view)
            (make-ns-menu "" (list (command :copy "Copy")
                                   (command :paste "Paste")
                                   (command :select-all "Select All")
                                   nil
                                   (command :clear-scrollback "Clear Scrollback")))))))

(defmethod backend-bind-menu ((backend cocoa-backend) (view cocoa-view) items)
  "Add a Lisp menu holding ITEMS to the menu bar, before the Window menu."
  (setf (cocoa-view-lisp-menu-items view) items)
  (when items
    (let ((bar (msg (msg "NSApplication" "sharedApplication") "mainMenu")))
      (add-menu-bar-menu bar
                         (make-ns-menu "Lisp"
                                       (loop for (label function) in items
                                             collect (make-slt-menu-item view label function)))
                         (max 0 (1- (msg bar "numberOfItems" :long)))))))

(defmethod backend-set-mouse-tracking ((backend cocoa-backend) (view cocoa-view) tracking)
  (setf (cocoa-view-mouse-tracking view) tracking))

(defmethod backend-bind-focus ((backend cocoa-backend) (view cocoa-view) callback)
  (setf (cocoa-view-focus-callback view) callback))

(defmethod backend-bind-drop ((backend cocoa-backend) (view cocoa-view) callback)
  (setf (cocoa-view-drop-callback view) callback))

(defmethod backend-bell ((backend cocoa-backend) (view cocoa-view) audible)
  (let ((app (msg "NSApplication" "sharedApplication")))
    (cond
      ;; NSInformationalRequest: bounce the Dock icon once.
      ((not (msg app "isActive" :bool))
       (msg app "requestUserAttention:" :unsigned-long 10 :long))
      (audible
       (cffi:foreign-funcall "NSBeep" :void)))))

(defmethod backend-set-font-size ((backend cocoa-backend) (view cocoa-view) size)
  (let* ((old (cocoa-view-font view))
         (font (make-coretext-font (cocoa-view-font-family view)
                                   (* size (cocoa-view-scale view))))
         (cw (coretext-font-cell-width font))
         (ch (coretext-font-cell-height font)))
    (setf (cocoa-view-font view) font
          (cocoa-view-font-size view) size
          (cocoa-view-cell-width view) cw
          (cocoa-view-cell-height view) ch
          (cocoa-view-atlas view) (make-glyph-atlas cw ch (coretext-rasterizer font))
          ;; A new atlas starts at generation 0 too: make the renderer
          ;; replace its texture.
          (metal-renderer-atlas-texture-generation (cocoa-view-renderer view)) -1)
    (free-coretext-font old)
    t))

(defmethod backend-clipboard-text ((backend cocoa-backend) (view cocoa-view))
  (with-autorelease-pool ()
    (ns-string->string (msg (msg "NSPasteboard" "generalPasteboard") "stringForType:"
                            :pointer (foreign-global "NSPasteboardTypeString")))))

(defmethod backend-set-clipboard-text ((backend cocoa-backend) (view cocoa-view) text)
  (with-autorelease-pool ()
    (let ((pasteboard (msg "NSPasteboard" "generalPasteboard")))
      (msg pasteboard "clearContents" :long)
      (msg pasteboard "setString:forType:"
           :pointer (ns-string text)
           :pointer (foreign-global "NSPasteboardTypeString")
           :bool))))

(defmethod backend-on-close ((backend cocoa-backend) (view cocoa-view) callback)
  (setf (cocoa-view-close-callback view) callback))

(defmethod backend-focus ((backend cocoa-backend) (view cocoa-view))
  (msg (cocoa-view-window view) "makeKeyAndOrderFront:" :pointer (cffi:null-pointer) :void))

(defmethod backend-request-exit ((backend cocoa-backend) (view cocoa-view))
  (unless (cocoa-view-closed-p view)
    (setf (cocoa-view-closed-p view) t)
    (msg (cocoa-view-window view) "close" :void))
  ;; Closing an already-closing window reaches here from its close callback.
  ;; The delegate retires it once that callback returns.
  (when (null (cocoa-backend-views backend))
    (setf (cocoa-backend-quit-p backend) t)
    (post-wakeup-event)))

(register-backend :cocoa (lambda () (make-instance 'cocoa-backend)))
