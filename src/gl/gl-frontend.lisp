(in-package #:slt)

;;;; The gl frontend: a GLFW window, an OpenGL 3.3 core (or OpenGL ES 3.0)
;;;; renderer and FreeType glyphs.  GLFW picks X11 or Wayland at run time on
;;;; Linux and the BSDs, and runs on macOS too, where it is useful for
;;;; testing (the cocoa frontend, on Metal, is the one to use there).  All
;;;; sizes the emulator sees are framebuffer pixels.

(defvar *gl-api* :auto
  "Which API to draw the window with: :GL (OpenGL 3.3 core), :GLES (OpenGL
ES 3.0), :AUTO for OpenGL with a fallback to OpenGL ES, or :VULKAN (with
slt/glfw-vulkan loaded).  Contexts are created through EGL except on
macOS, also on X11: GLX does not start on some systems (musl Linux under
Xorg), and Mesa ships EGL wherever it ships GLX.")

(defvar *gl-vsync* t
  "When true, buffer swaps wait for the display's refresh.")

(defvar *gl-window-visible* t
  "NIL creates the window hidden, for offscreen tests.")

(defvar *gl-scroll-rows* (if (uiop:os-macosx-p) 1 3)
  "Rows to scroll per wheel step.  macOS reports trackpad motion in finer
steps, so it uses a smaller multiplier.")

(defparameter *glfw-key-keysyms*
  '((:escape . "Escape") (:enter . "Return") (:tab . "Tab") (:backspace . "BackSpace")
    (:insert . "Insert") (:delete . "Delete") (:right . "Right") (:left . "Left")
    (:down . "Down") (:up . "Up") (:page-up . "Prior") (:page-down . "Next")
    (:home . "Home") (:end . "End") (:kp-enter . "KP_Enter")
    (:f1 . "F1") (:f2 . "F2") (:f3 . "F3") (:f4 . "F4") (:f5 . "F5") (:f6 . "F6")
    (:f7 . "F7") (:f8 . "F8") (:f9 . "F9") (:f10 . "F10") (:f11 . "F11") (:f12 . "F12")
    (:left-shift . "Shift_L") (:right-shift . "Shift_R")
    (:left-control . "Control_L") (:right-control . "Control_R")
    (:left-alt . "Alt_L") (:right-alt . "Alt_R")
    (:left-super . "Super_L") (:right-super . "Super_R")
    (:caps-lock . "Caps_Lock") (:num-lock . "Num_Lock"))
  "GLFW keys the terminal sees as X11 keysyms rather than text.")

(defun gl-libraries-available-p ()
  "True when GLFW and the glyph rasterizer's libraries load."
  (and (ignore-errors (cffi:load-foreign-library '%glfw::glfw) t)
       (glyph-libraries-available-p)))

;;; GLFW 3.2 and 3.3 calls and window hints cl-glfw3 does not bind.

(cffi:defcfun ("glfwWaitEventsTimeout" %glfw-wait-events-timeout) :void (timeout :double))
(cffi:defcfun ("glfwGetKeyName" %glfw-get-key-name) :string (key :int) (scan-code :int))
(cffi:defcfun ("glfwFocusWindow" %glfw-focus-window) :void (window :pointer))
(cffi:defcfun ("glfwWindowHint" %glfw-window-hint) :void (hint :int) (value :int))
(cffi:defcfun ("glfwSetDropCallback" %glfw-set-drop-callback) :pointer
  (window :pointer) (callback :pointer))
(cffi:defcfun ("glfwSetWindowContentScaleCallback" %glfw-set-window-content-scale-callback)
    :pointer
  (window :pointer) (callback :pointer))

(defconstant +glfw-context-creation-api+ #x0002200B)
(defconstant +glfw-egl-context-api+ #x00036002)
(defconstant +glfw-scale-to-monitor+ #x0002200C)
(defconstant +glfw-key-unknown+ -1)

(defclass gl-frontend (terminal-frontend)
  ((view :initform nil :accessor gl-frontend-view)
   (timers :initform '() :accessor gl-frontend-timers)
   (timer-seq :initform 0 :accessor gl-frontend-timer-seq)
   (quit-p :initform nil :accessor gl-frontend-quit-p)
   (wake-lock :initform (bt:make-lock "slt gl wakeup") :reader gl-frontend-wake-lock)
   (wake-thunks :initform '() :accessor gl-frontend-wake-thunks)))

(defclass gl-view ()
  ((pointer :initform nil :accessor gl-view-pointer)
   (frontend :initarg :frontend :reader gl-view-frontend)
   (api :initarg :api :reader gl-view-api)
   (renderer :initform nil :accessor gl-view-renderer)
   (font :initform nil :accessor gl-view-font)
   ;; (FAMILY SIZE CELL-WIDTH CELL-HEIGHT), to re-rasterize on a scale change.
   (font-options :initarg :font-options :reader gl-view-font-options)
   (atlas :initform nil :accessor gl-view-atlas)
   (instances :initform nil :accessor gl-view-instances)
   (grid :initform nil :accessor gl-view-grid)
   (image-store :initform nil :accessor gl-view-image-store)
   (scale :initform 0.0 :accessor gl-view-scale)
   (cell-width :initform 1 :accessor gl-view-cell-width)
   (cell-height :initform 1 :accessor gl-view-cell-height)
   (resize-callback :initform nil :accessor gl-view-resize-callback)
   (keypress-callback :initform nil :accessor gl-view-keypress-callback)
   (drop-callback :initform nil :accessor gl-view-drop-callback)
   (scroll-callback :initform nil :accessor gl-view-scroll-callback)
   (mouse-callback :initform nil :accessor gl-view-mouse-callback)
   ;; Primary button state, and the last press for counting multi-clicks.
   ;; The mouse button held down, or NIL.
   (button :initform nil :accessor gl-view-button)
   (last-click :initform nil :accessor gl-view-last-click)
   (click-count :initform 0 :accessor gl-view-click-count)
   (close-callback :initform nil :accessor gl-view-close-callback)
   (closed-p :initform nil :accessor gl-view-closed-p)
   (scroll-remainder :initform 0d0 :accessor gl-view-scroll-remainder)
   ;; Modifiers of the last key press, for the character that follows it.
   (key-modifiers :initform '() :accessor gl-view-key-modifiers)
   ;; Set when the key callback already handled the press, so the character
   ;; callback that follows must not send it again.
   (suppress-char :initform nil :accessor gl-view-suppress-char)
   ;; :PRESS or :REPEAT: the action of the key whose character follows.
   (key-event :initform :press :accessor gl-view-key-event)))

(defmacro with-live-view ((view) &body body)
  "Run BODY for an open VIEW, reporting rather than unwinding through
GLFW's C frames on error."
  `(unless (gl-view-closed-p ,view)
     (handler-case (progn ,@body)
       (error (condition)
         (format *error-output* "~&slt: ~a~%" condition)))))

;;; GLFW calls back with the window's pointer.

(defvar *gl-views* (make-hash-table)
  "Open views by the address of their GLFW window.")

(defmacro define-gl-callback (name (view &rest arguments) &body body)
  "Define the GLFW window callback NAME, running BODY for the open VIEW
whose window it is.  ARGUMENTS follow the window, as in DEFCALLBACK."
  (let ((window (gensym "WINDOW"))
        (declarations (loop while (and (consp (first body)) (eq (first (first body)) 'declare))
                            collect (pop body))))
    `(%glfw:define-glfw-callback ,name ((,window :pointer) ,@arguments)
       ,@declarations
       (let ((,view (gethash (cffi:pointer-address ,window) *gl-views*)))
         (when ,view
           (with-live-view (,view) ,@body))))))

;;; Fonts come from slt/glyphs.  Sizes are pixels.

(defun gl-view-set-font (view scale)
  "Open the view's font for SCALE and size its cells and atlas."
  (destructuring-bind (family size cell-width cell-height) (gl-view-font-options view)
    (let ((font (open-glyph-font family (* size scale))))
      (when (gl-view-font view)
        (close-glyph-font (gl-view-font view)))
      (multiple-value-bind (font-width font-height) (glyph-font-cell-size font)
        (let ((cw (or (and cell-width (round (* cell-width scale))) font-width))
              (ch (or (and cell-height (round (* cell-height scale))) font-height)))
          (setf (gl-view-font view) font
                (gl-view-scale view) scale
                (gl-view-cell-width view) cw
                (gl-view-cell-height view) ch
                (gl-view-atlas view) (make-glyph-atlas cw ch (glyph-font-rasterizer font)
                                                              :shaper (glyph-font-shaper font))
                (gl-view-instances view) nil))))))

(defun gl-view-content-scale (view)
  (let ((scale (second (glfw:get-window-content-scale (gl-view-pointer view)))))
    (if (plusp scale) scale 1.0)))

;;; Keyboard

(defun glfw-modifiers->state (modifiers)
  (logior (if (member :shift modifiers) +shift-mask+ 0)
          (if (member :control modifiers) +control-mask+ 0)
          (if (member :alt modifiers) +mod1-mask+ 0)
          (if (member :super modifiers) +mod4-mask+ 0)))

(defun gl-printable-key-code (code)
  "CODE when it is a printable key's (space through grave accent)."
  (and (<= 32 code 96) code))

(defun gl-key-text (code scan-code modifiers)
  "The character the printable key CODE produces with Control or Meta held,
from the keyboard layout where GLFW knows it."
  (let ((name (or (%glfw-get-key-name +glfw-key-unknown+ scan-code)
                  (and (/= code 32) (%glfw-get-key-name code 0))
                  (string (char-downcase (code-char code))))))
    (if (and (member :shift modifiers) (= 1 (length name)) (alpha-char-p (char name 0)))
        (string-upcase name)
        name)))

(defun gl-key-data (key-code scan-code modifiers)
  "The (CHAR KEYSYM STATE) to send for a press of the key KEY-CODE, or NIL
to leave it to the character callback."
  (let ((state (glfw-modifiers->state modifiers))
        (keysym (cdr (assoc (cffi:foreign-enum-keyword '%glfw::key key-code :errorp nil)
                            *glfw-key-keysyms*)))
        (code (gl-printable-key-code key-code))
        ;; With Option as Meta, macOS's composed character is not wanted.
        ;; Elsewhere Alt+key still produces the plain character, which the
        ;; character callback sends with Meta.
        (meta (and (member :alt modifiers) (uiop:os-macosx-p) *option-as-meta*)))
    (cond
      (keysym (list "" keysym state))
      ;; Super combinations go to the emulator for its shortcuts.
      ((and code (or (member :control modifiers) (member :super modifiers) meta))
       (let ((text (gl-key-text code scan-code modifiers)))
         ;; Shift is reflected in the character.
         (list text text (logand state (lognot +shift-mask+)))))
      (t nil))))

(defun deliver-gl-key (view data &optional (event :press))
  (when (gl-view-keypress-callback view)
    (apply (gl-view-keypress-callback view) (append data (list event)))))

(define-gl-callback gl-key-changed (view (key :int) (scan-code :int)
                                         (action %glfw::key-action)
                                         (modifiers %glfw::mod-keys))
  (if (eq action :release)
      ;; Releases have no character event: name the key from the layout.
      (let ((data (or (gl-key-data key scan-code modifiers)
                      (let ((code (gl-printable-key-code key)))
                        (and code
                             (let ((text (gl-key-text code scan-code modifiers)))
                               (list text text (logand (glfw-modifiers->state modifiers)
                                                       (lognot +shift-mask+)))))))))
        (when data
          (deliver-gl-key view data :release)))
      (progn
        (setf (gl-view-key-modifiers view) modifiers
              (gl-view-suppress-char view) nil
              (gl-view-key-event view) action)
        (let ((data (gl-key-data key scan-code modifiers)))
          (when data
            (setf (gl-view-suppress-char view) t)
            (deliver-gl-key view data action))))))

(define-gl-callback gl-char-entered (view (code-point :unsigned-int))
  (unless (gl-view-suppress-char view)
    (let ((text (string (code-char code-point)))
          (state (if (member :alt (gl-view-key-modifiers view)) +mod1-mask+ 0)))
      (deliver-gl-key view (list text text state) (gl-view-key-event view)))))

;;; Mouse

(defvar *multi-click-seconds* 0.4
  "The longest gap between clicks of a double or triple click.")

(defun gl-pointer-pixels (view x y)
  "Window coordinates X, Y as framebuffer pixels, the grid's units."
  (let ((scale (/ (first (glfw:get-framebuffer-size (gl-view-pointer view)))
                 (max 1 (first (glfw:get-window-size (gl-view-pointer view)))))))
    (list (* x scale) (* y scale))))

(defun gl-pointer-cell (view x y)
  "The (ROW COLUMN) under window coordinates X, Y."
  (destructuring-bind (px py) (gl-pointer-pixels view x y)
    (list (floor py (gl-view-cell-height view))
          (floor px (gl-view-cell-width view)))))

(defun count-gl-click (view cell)
  "Count a press at CELL as part of a multi-click when it follows the last
one quickly at the same cell."
  (let ((now (get-internal-real-time))
        (last (gl-view-last-click view)))
    (setf (gl-view-click-count view)
          (if (and last
                   (equal cell (cdr last))
                   (< (- now (car last))
                      (* *multi-click-seconds* internal-time-units-per-second)))
              (1+ (gl-view-click-count view))
              1)
          (gl-view-last-click view) (cons now cell))
    (gl-view-click-count view)))

(defun deliver-gl-mouse (view action position clicks button state)
  "Pass a mouse event at window coordinates POSITION, (X Y), to the view's
callback."
  (when (gl-view-mouse-callback view)
    (destructuring-bind (row column) (apply #'gl-pointer-cell view position)
      (destructuring-bind (x y) (apply #'gl-pointer-pixels view position)
        (funcall (gl-view-mouse-callback view) action row column clicks
                 button state x y)))))

(define-gl-callback gl-mouse-button-changed (view (button :int)
                                                 (action %glfw::key-action)
                                                 (modifiers %glfw::mod-keys))
  ;; GLFW numbers the left, right and middle buttons 0, 1 and 2.
  (let ((number (case button (0 1) (2 2) (1 3))))
    (when number
      (let* ((position (glfw:get-cursor-position (gl-view-pointer view)))
             (cell (apply #'gl-pointer-cell view position))
             (state (glfw-modifiers->state modifiers)))
        (case action
          (:press
           (setf (gl-view-button view) number)
           (deliver-gl-mouse view :press position (count-gl-click view cell) number state))
          (:release
           (setf (gl-view-button view) nil)
           (deliver-gl-mouse view :release position (gl-view-click-count view)
                             number state)))))))

(define-gl-callback gl-mouse-moved (view (x :double) (y :double))
  (let ((button (gl-view-button view)))
    (deliver-gl-mouse view (if button :drag :motion) (list x y)
                      (gl-view-click-count view) button
                      0)))

;;; Other window events

(define-gl-callback gl-mouse-scrolled (view (xoffset :double) (yoffset :double))
  (declare (ignore xoffset))
  (when (gl-view-scroll-callback view)
    (let* ((rows (+ (gl-view-scroll-remainder view) (* yoffset *gl-scroll-rows*)))
           (whole (truncate rows)))
      (setf (gl-view-scroll-remainder view) (- rows whole))
      (unless (zerop whole)
        ;; Scrolling content down (positive offset) moves back in history.
        (funcall (gl-view-scroll-callback view) (- whole))))))

(define-gl-callback gl-framebuffer-resized (view (width :int) (height :int))
  (declare (ignore width height))
  (when (gl-view-resize-callback view)
    ;; Resize at once: on macOS GLFW calls this from inside AppKit's
    ;; live-resize loop, where timers do not run.  Then run work the
    ;; reader thread posted, so the program's redraw shows while dragging.
    (funcall (gl-view-resize-callback view) t)
    (run-gl-wake-thunks (gl-view-frontend view)))
  (redraw-gl-view view))

(define-gl-callback gl-window-refreshed (view)
  (redraw-gl-view view))

(define-gl-callback gl-window-content-scale-changed (view (xscale :float) (yscale :float))
  (declare (ignore xscale))
  ;; The window moved to a display with another scale: re-rasterize.
  (unless (= yscale (gl-view-scale view))
    (gl-view-set-font view yscale)
    (when (gl-view-resize-callback view)
      (funcall (gl-view-resize-callback view) t))
    (redraw-gl-view view)))

(define-gl-callback gl-window-closed (view)
  (setf (gl-view-closed-p view) t)
  (when (gl-view-close-callback view)
    (funcall (gl-view-close-callback view)))
  (setf (gl-frontend-quit-p (gl-view-frontend view)) t))

(define-gl-callback gl-file-dropped (view (count :int) (paths :pointer))
  (when (and (plusp count) (gl-view-drop-callback view))
    (funcall (gl-view-drop-callback view)
             (loop for i below count collect (cffi:mem-aref paths :string i)))))

(defun bind-gl-callbacks (view)
  (let ((window (gl-view-pointer view)))
    (glfw:set-key-callback 'gl-key-changed window)
    (glfw:set-char-callback 'gl-char-entered window)
    (glfw:set-mouse-button-callback 'gl-mouse-button-changed window)
    (glfw:set-cursor-position-callback 'gl-mouse-moved window)
    (glfw:set-scroll-callback 'gl-mouse-scrolled window)
    (glfw:set-framebuffer-size-callback 'gl-framebuffer-resized window)
    (glfw:set-window-refresh-callback 'gl-window-refreshed window)
    (glfw:set-window-close-callback 'gl-window-closed window)
    (%glfw-set-window-content-scale-callback
     window (cffi:callback gl-window-content-scale-changed))
    (%glfw-set-drop-callback window (cffi:callback gl-file-dropped))))

;;; Event loop

(defmethod frontend-wakeup ((frontend gl-frontend) view thunk)
  (declare (ignore view))
  (bt:with-lock-held ((gl-frontend-wake-lock frontend))
    (push thunk (gl-frontend-wake-thunks frontend)))
  ;; Thread-safe: unblocks the main thread's wait for events.
  (%glfw:post-empty-event)
  t)

(defun run-gl-wake-thunks (frontend)
  (let ((thunks (bt:with-lock-held ((gl-frontend-wake-lock frontend))
                  (shiftf (gl-frontend-wake-thunks frontend) '()))))
    (dolist (thunk (nreverse thunks))
      (funcall thunk))))

(defun run-gl-timers (frontend)
  (let ((now (get-internal-real-time))
        (due '()))
    (setf (gl-frontend-timers frontend)
          (remove-if (lambda (timer)
                       (when (<= (first timer) now)
                         (push timer due)
                         t))
                     (gl-frontend-timers frontend)))
    (dolist (timer (sort due #'< :key #'first))
      (funcall (third timer)))))

(defun gl-next-timeout (frontend)
  (let ((timers (gl-frontend-timers frontend)))
    (if timers
        (max 0 (/ (- (reduce #'min timers :key #'first) (get-internal-real-time))
                  internal-time-units-per-second))
        0.25)))

(%glfw:define-glfw-callback gl-glfw-error ((code :int) (message :string))
  ;; Report rather than signal: an error must not unwind through GLFW.
  (format *error-output* "~&slt: GLFW error ~x: ~a~%" code message))

(defun wait-gl-events (timeout)
  "Handle window events, waiting up to TIMEOUT seconds for the first."
  (if (plusp timeout)
      (%glfw-wait-events-timeout (coerce timeout 'double-float))
      (%glfw:poll-events)))

(defgeneric prepare-glfw (api)
  (:documentation "Prepare GLFW, before it is initialized, for windows drawn
with API.")
  (:method (api) (declare (ignore api))))

(defun call-with-glfw (thunk)
  "Run THUNK with GLFW initialized and cl-opengl resolving entry points
through GLFW.  Masks float traps, which platform code (AppKit in
particular) trips over."
  (float-features:with-float-traps-masked t
    (%glfw:set-error-callback (cffi:callback gl-glfw-error))
    (when (eq *gl-api* :vulkan)
      ;; Vulkan lives in its own system, loaded when built in or found.
      (load-frontend-system "glfw-vulkan"))
    (prepare-glfw *gl-api*)
    (glfw:initialize)
    (let ((cl-opengl-bindings:*gl-get-proc-address* #'%glfw:get-proc-address))
      (unwind-protect (funcall thunk)
        (glfw:terminate)))))

(defmethod frontend-call-with-event-loop ((frontend gl-frontend) thunk)
  (call-with-glfw
   (lambda ()
     (unwind-protect
          (progn
            (funcall thunk)
            (loop until (gl-frontend-quit-p frontend)
                  do (wait-gl-events (min 0.25 (gl-next-timeout frontend)))
                     (run-gl-wake-thunks frontend)
                     (run-gl-timers frontend)))
       (let ((view (gl-frontend-view frontend)))
         (when view
           (destroy-gl-view view)))))))

;;; Views

(defun gl-view-pixel-size (view)
  (values-list (glfw:get-framebuffer-size (gl-view-pointer view))))

;;; Renderers.  A view's renderer draws with its API: OpenGL, in the
;;; window's context, here; slt/glfw-vulkan adds Vulkan.  A frame is the
;;; packed cells, their layout, the atlas, the palette and the images.

(defgeneric api-window-hints (api)
  (:documentation "CREATE-WINDOW's arguments for a window drawn with API.")
  (:method ((api (eql :gl)))
    '(:client-api :opengl-api :context-version-major 3 :context-version-minor 3
      :opengl-profile :opengl-core-profile :opengl-forward-compat t))
  (:method ((api (eql :gles)))
    '(:client-api :opengl-es-api :context-version-major 3 :context-version-minor 0)))

(defgeneric make-api-renderer (api view)
  (:documentation "A renderer drawing VIEW's window with API.")
  (:method ((api symbol) view)
    (glfw:make-context-current (gl-view-pointer view))
    (%glfw:swap-interval (if *gl-vsync* 1 0))
    (make-gl-renderer api)))

(defgeneric renderer-present (renderer view frame)
  (:documentation "Draw FRAME (GL-DRAW's arguments after the renderer) into
VIEW's window and show it.")
  (:method ((renderer gl-renderer) view frame)
    (glfw:make-context-current (gl-view-pointer view))
    (apply #'gl-draw renderer frame)
    (glfw:swap-buffers (gl-view-pointer view))))

(defgeneric renderer-snapshot (renderer view frame)
  (:documentation "Render FRAME offscreen; return its RGB octets.")
  (:method ((renderer gl-renderer) view frame)
    (glfw:make-context-current (gl-view-pointer view))
    (apply #'gl-snapshot renderer frame)))

(defgeneric free-renderer (renderer view)
  (:method ((renderer gl-renderer) view)
    (glfw:make-context-current (gl-view-pointer view))
    (free-gl-renderer renderer)))

(defun gl-view-frame (view palette)
  "VIEW's grid packed, as a frame to draw: GL-DRAW's arguments after the
renderer."
  (let ((grid (gl-view-grid view)))
    (setf (gl-view-instances view)
          (pack-grid grid (gl-view-atlas view) palette (gl-view-instances view)))
    (multiple-value-bind (width height) (gl-view-pixel-size view)
      (list (gl-view-instances view)
            (grid-columns grid)
            (* (grid-rows grid) (grid-columns grid))
            (gl-view-cell-width view)
            (gl-view-cell-height view)
            width height
            (gl-view-atlas view)
            palette
            (and (gl-view-image-store view)
                 (terminal-image-render-list
                  (gl-view-image-store view)
                  (gl-view-cell-width view) (gl-view-cell-height view)))))))

(defun redraw-gl-view (view &optional (palette (view-palette view)))
  (when (and (gl-view-grid view) (not (gl-view-closed-p view)))
    (renderer-present (gl-view-renderer view) view (gl-view-frame view palette))))

(defun snapshot-gl-view (view &optional pathname (palette (view-palette view)))
  "Render VIEW's current grid offscreen.  Return the RGB octets, width and
height, after writing them to PATHNAME as a PPM when given."
  (let* ((frame (gl-view-frame view palette))
         (rgb (renderer-snapshot (gl-view-renderer view) view frame)))
    (multiple-value-bind (width height) (gl-view-pixel-size view)
      (when pathname
        (write-ppm pathname rgb width height))
      (values rgb width height))))

(defun create-gl-window (width height title api)
  "Create a GLFW window for API: with its context, current on this thread,
for OpenGL."
  ;; Hints cl-glfw3's CREATE-WINDOW does not know stay set for it.  Window
  ;; sizes are in screen coordinates; let GLFW scale them and the
  ;; framebuffer.  macOS has only NSGL; everywhere else, EGL rather than GLX.
  (%glfw-window-hint +glfw-scale-to-monitor+ 1)
  (unless (uiop:os-macosx-p)
    (%glfw-window-hint +glfw-context-creation-api+ +glfw-egl-context-api+))
  (apply #'glfw:create-window :width width :height height :title title
                              :visible *gl-window-visible*
                              (api-window-hints api)))

(defun make-gl-view (frontend width height title font-options scale)
  "Create the window with the first API in *GL-API* that works."
  (dolist (api (ecase *gl-api*
                 (:auto (if (uiop:os-macosx-p) '(:gl) '(:gl :gles)))
                 (:gl '(:gl))
                 (:gles '(:gles))
                 (:vulkan '(:vulkan)))
               (error "No window for ~a could be created." *gl-api*))
    (let ((window (handler-case (create-gl-window width height title api)
                    (error (condition)
                      (format *error-output* "~&slt: no ~a window: ~a~%" api condition)
                      nil))))
      (when window
        (let ((view (make-instance 'gl-view :frontend frontend :api api
                                            :font-options font-options)))
          (setf (gl-view-pointer view) window
                (gl-view-scale view) scale
                (gethash (cffi:pointer-address window) *gl-views*) view)
          (bind-gl-callbacks view)
          (return view))))))

(defmethod frontend-available-font-families ((frontend gl-frontend))
  (glyph-font-families))

(defmethod frontend-resolve-font-size ((frontend gl-frontend) font-size)
  (abs (or font-size 15)))

(defmethod frontend-create-view ((frontend gl-frontend)
                                &key rows columns font-family font-size
                                  cell-width cell-height title)
  (let* ((options (list font-family font-size cell-width cell-height))
         (scale (let ((monitor (glfw:get-primary-monitor)))
                  (if (cffi:null-pointer-p monitor)
                      1.0
                      (max 1.0 (second (glfw:get-monitor-content-scale monitor))))))
         ;; Measure at the primary display's scale to size the window.
         (font (open-glyph-font font-family (* font-size scale)))
         (cw (or (and cell-width (round (* cell-width scale)))
                 (nth-value 0 (glyph-font-cell-size font))))
         (ch (or (and cell-height (round (* cell-height scale)))
                 (nth-value 1 (glyph-font-cell-size font))))
         (view (make-gl-view frontend
                             (max 1 (round (* columns cw) scale))
                             (max 1 (round (* rows ch) scale))
                             (or title "slt") options scale)))
    (close-glyph-font font)
    (setf (gl-view-renderer view) (make-api-renderer (gl-view-api view) view)
          (gl-frontend-view frontend) view)
    ;; The window may have opened on a display with another scale.
    (gl-view-set-font view (gl-view-content-scale view))
    view))

(defun destroy-gl-view (view)
  ;; The renderer goes first, while its window is there.
  (when (and (gl-view-pointer view) (gl-view-renderer view))
    (ignore-errors (free-renderer (gl-view-renderer view) view)))
  (let ((window (shiftf (gl-view-pointer view) nil)))
    (when window
      (remhash (cffi:pointer-address window) *gl-views*)
      (glfw:destroy-window window)))
  (when (gl-view-font view)
    (ignore-errors (close-glyph-font (gl-view-font view)))
    (setf (gl-view-font view) nil))
  (setf (gl-view-closed-p view) t))

(defmethod frontend-view-cell-width ((frontend gl-frontend) (view gl-view))
  (gl-view-cell-width view))

(defmethod frontend-view-cell-height ((frontend gl-frontend) (view gl-view))
  (gl-view-cell-height view))

(defmethod frontend-view-window-size ((frontend gl-frontend) (view gl-view))
  (gl-view-pixel-size view))

(defmethod frontend-resize-view ((frontend gl-frontend) (view gl-view) rows columns)
  (declare (ignore rows columns))
  view)

(defmethod frontend-render-grid ((frontend gl-frontend) (view gl-view) grid)
  (setf (gl-view-grid view) grid)
  (redraw-gl-view view))

(defmethod frontend-render-images ((frontend gl-frontend) (view gl-view) store)
  (declare (ignore frontend))
  (setf (gl-view-image-store view) store)
  (redraw-gl-view view))

(defmethod frontend-set-title ((frontend gl-frontend) (view gl-view) title)
  (unless (gl-view-closed-p view)
    (glfw:set-window-title title (gl-view-pointer view))))

(defmethod frontend-schedule ((frontend gl-frontend) (view gl-view) delay-ms thunk)
  (let ((token (incf (gl-frontend-timer-seq frontend))))
    (push (list (+ (get-internal-real-time)
                   (round (* (max 0 delay-ms) internal-time-units-per-second) 1000))
                token
                thunk)
          (gl-frontend-timers frontend))
    token))

(defmethod frontend-cancel-scheduled ((frontend gl-frontend) (view gl-view) token)
  (setf (gl-frontend-timers frontend)
        (remove token (gl-frontend-timers frontend) :key #'second)))

(defmethod frontend-bind-resize ((frontend gl-frontend) (view gl-view) callback)
  (setf (gl-view-resize-callback view) callback))

(defmethod frontend-bind-keypress ((frontend gl-frontend) (view gl-view) callback)
  (setf (gl-view-keypress-callback view) callback))

(defmethod frontend-bind-drop ((frontend gl-frontend) (view gl-view) callback)
  (setf (gl-view-drop-callback view) callback))

(defmethod frontend-bind-scroll ((frontend gl-frontend) (view gl-view) callback)
  (setf (gl-view-scroll-callback view) callback))

(defmethod frontend-bind-mouse ((frontend gl-frontend) (view gl-view) callback)
  (setf (gl-view-mouse-callback view) callback))

(defmethod frontend-clipboard-text ((frontend gl-frontend) (view gl-view))
  (glfw:get-clipboard-string (gl-view-pointer view)))

(defmethod frontend-set-clipboard-text ((frontend gl-frontend) (view gl-view) text)
  (glfw:set-clipboard-string text (gl-view-pointer view)))

(defmethod frontend-on-close ((frontend gl-frontend) (view gl-view) callback)
  (setf (gl-view-close-callback view) callback))

(defmethod frontend-focus ((frontend gl-frontend) (view gl-view))
  (when *gl-window-visible*
    (glfw:show-window (gl-view-pointer view))
    (%glfw-focus-window (gl-view-pointer view))))

(defmethod frontend-request-exit ((frontend gl-frontend) (view gl-view))
  (setf (gl-frontend-quit-p frontend) t)
  (setf (gl-view-closed-p view) t))

(register-frontend :gl (lambda () (make-instance 'gl-frontend)))
