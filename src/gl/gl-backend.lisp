(in-package #:slt)

;;;; The gl backend: a GLFW window, an OpenGL 3.3 core (or OpenGL ES 3.0)
;;;; renderer and FreeType glyphs.  GLFW picks X11 or Wayland at run time on
;;;; Linux and the BSDs, and runs on macOS too, where it is useful for
;;;; testing (the cocoa backend, on Metal, is the one to use there).  All
;;;; sizes the frontend sees are framebuffer pixels.

(defvar *gl-api* :auto
  "Which API to ask GLFW for: :GL (OpenGL 3.3 core), :GLES (OpenGL ES 3.0),
or :AUTO for OpenGL with a fallback to OpenGL ES.  Contexts are created
through EGL except on macOS, also on X11: GLX does not start on some
systems (musl Linux under Xorg), and Mesa ships EGL wherever it ships GLX.")

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
    (:f7 . "F7") (:f8 . "F8") (:f9 . "F9") (:f10 . "F10") (:f11 . "F11") (:f12 . "F12"))
  "GLFW keys the terminal sees as X11 keysyms rather than text.")

(defun gl-libraries-available-p ()
  "True when GLFW and the glyph rasterizer's libraries load."
  (ignore-errors
   (cffi:load-foreign-library '%glfw:libglfw)
   #-darwin (load-freetype)
   t))

(defclass gl-backend (terminal-backend)
  ((view :initform nil :accessor gl-backend-view)
   (timers :initform '() :accessor gl-backend-timers)
   (timer-seq :initform 0 :accessor gl-backend-timer-seq)
   (quit-p :initform nil :accessor gl-backend-quit-p)
   (wake-lock :initform (bt:make-lock "slt gl wakeup") :reader gl-backend-wake-lock)
   (wake-thunks :initform '() :accessor gl-backend-wake-thunks)))

(defclass gl-view (glfw:window)
  ((backend :initarg :backend :reader gl-view-backend)
   (api :initarg :api :reader gl-view-api)
   (renderer :initform nil :accessor gl-view-renderer)
   (font :initform nil :accessor gl-view-font)
   ;; (FAMILY SIZE CELL-WIDTH CELL-HEIGHT), to re-rasterize on a scale change.
   (font-options :initarg :font-options :reader gl-view-font-options)
   (atlas :initform nil :accessor gl-view-atlas)
   (instances :initform nil :accessor gl-view-instances)
   (grid :initform nil :accessor gl-view-grid)
   (scale :initform 0.0 :accessor gl-view-scale)
   (cell-width :initform 1 :accessor gl-view-cell-width)
   (cell-height :initform 1 :accessor gl-view-cell-height)
   (resize-callback :initform nil :accessor gl-view-resize-callback)
   (keypress-callback :initform nil :accessor gl-view-keypress-callback)
   (scroll-callback :initform nil :accessor gl-view-scroll-callback)
   (mouse-callback :initform nil :accessor gl-view-mouse-callback)
   ;; Primary button state, and the last press for counting multi-clicks.
   (button-down-p :initform nil :accessor gl-view-button-down-p)
   (last-click :initform nil :accessor gl-view-last-click)
   (click-count :initform 0 :accessor gl-view-click-count)
   (close-callback :initform nil :accessor gl-view-close-callback)
   (closed-p :initform nil :accessor gl-view-closed-p)
   (scroll-remainder :initform 0d0 :accessor gl-view-scroll-remainder)
   ;; Modifiers of the last key press, for the character that follows it.
   (key-modifiers :initform '() :accessor gl-view-key-modifiers)
   ;; Set when the key callback already handled the press, so the character
   ;; callback that follows must not send it again.
   (suppress-char :initform nil :accessor gl-view-suppress-char)))

(defmacro with-live-view ((view) &body body)
  "Run BODY for an open VIEW, reporting rather than unwinding through
GLFW's C frames on error."
  `(unless (gl-view-closed-p ,view)
     (handler-case (progn ,@body)
       (error (condition)
         (format *error-output* "~&slt: ~a~%" condition)))))

;;; Fonts: CoreText on macOS, FreeType elsewhere.  Sizes are pixels.

(defun open-glyph-font (family pixel-size)
  #+darwin (make-coretext-font family pixel-size)
  #-darwin (make-ft-font family pixel-size))

(defun glyph-font-cell-size (font)
  #+darwin (values (coretext-font-cell-width font) (coretext-font-cell-height font))
  #-darwin (values (ft-font-cell-width font) (ft-font-cell-height font)))

(defun glyph-font-rasterizer (font)
  #+darwin (coretext-rasterizer font)
  #-darwin (freetype-rasterizer font))

(defun close-glyph-font (font)
  #+darwin (free-coretext-font font)
  #-darwin (free-ft-font font))

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
                (gl-view-atlas view) (make-glyph-atlas cw ch (glyph-font-rasterizer font))
                (gl-view-instances view) nil))))))

(defun gl-view-content-scale (view)
  (let ((scale (second (glfw:content-scale view))))
    (if (plusp scale) scale 1.0)))

;;; Keyboard

(defun glfw-modifiers->state (modifiers)
  (logior (if (member :shift modifiers) +shift-mask+ 0)
          (if (member :control modifiers) +control-mask+ 0)
          (if (member :alt modifiers) +mod1-mask+ 0)
          (if (member :super modifiers) +mod4-mask+ 0)))

(defun gl-printable-key-code (key)
  "The code of KEY when it is a printable key (space through grave accent)."
  (let ((code (cffi:foreign-enum-value '%glfw:key key :errorp nil)))
    (and code (<= 32 code 96) code)))

(defun gl-key-text (key code scan-code modifiers)
  "The character a printable KEY produces with Control or Meta held, from
the keyboard layout where GLFW knows it."
  (let ((name (or (ignore-errors (glfw:key-name scan-code))
                  (and (not (eq key :space)) (ignore-errors (glfw:key-name key)))
                  (string (char-downcase (code-char code))))))
    (if (and (member :shift modifiers) (= 1 (length name)) (alpha-char-p (char name 0)))
        (string-upcase name)
        name)))

(defun gl-key-data (key scan-code modifiers)
  "The (CHAR KEYSYM STATE) to send for a key press, or NIL to leave it to
the character callback."
  (let ((state (glfw-modifiers->state modifiers))
        (keysym (cdr (assoc key *glfw-key-keysyms*)))
        (code (gl-printable-key-code key))
        ;; With Option as Meta, macOS's composed character is not wanted.
        ;; Elsewhere Alt+key still produces the plain character, which the
        ;; character callback sends with Meta.
        (meta (and (member :alt modifiers) (uiop:os-macosx-p) *option-as-meta*)))
    (cond
      (keysym (list "" keysym state))
      ;; Super combinations go to the frontend for its shortcuts.
      ((and code (or (member :control modifiers) (member :super modifiers) meta))
       (let ((text (gl-key-text key code scan-code modifiers)))
         ;; Shift is reflected in the character.
         (list text text (logand state (lognot +shift-mask+)))))
      (t nil))))

(defun deliver-gl-key (view data)
  (when (gl-view-keypress-callback view)
    (apply (gl-view-keypress-callback view) data)))

(defmethod glfw:key-changed ((view gl-view) key scan-code action modifiers)
  (unless (eq action :release)
    (with-live-view (view)
      (setf (gl-view-key-modifiers view) modifiers
            (gl-view-suppress-char view) nil)
      (let ((data (gl-key-data key scan-code modifiers)))
        (when data
          (setf (gl-view-suppress-char view) t)
          (deliver-gl-key view data))))))

(defmethod glfw:char-entered ((view gl-view) code-point)
  (with-live-view (view)
    (unless (gl-view-suppress-char view)
      (let ((text (string (code-char code-point)))
            (state (if (member :alt (gl-view-key-modifiers view)) +mod1-mask+ 0)))
        (deliver-gl-key view (list text text state))))))

;;; Mouse

(defvar *multi-click-seconds* 0.4
  "The longest gap between clicks of a double or triple click.")

(defun gl-pointer-cell (view x y)
  "The (ROW COLUMN) under window coordinates X, Y."
  (let ((scale (/ (first (glfw:framebuffer-size view)) (max 1 (glfw:width view)))))
    (list (floor (* y scale) (gl-view-cell-height view))
          (floor (* x scale) (gl-view-cell-width view)))))

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

(defun deliver-gl-mouse (view action cell clicks)
  (when (gl-view-mouse-callback view)
    (funcall (gl-view-mouse-callback view) action (first cell) (second cell) clicks)))

(defmethod glfw:mouse-button-changed ((view gl-view) button action modifiers)
  (declare (ignore modifiers))
  (when (eq button :left)
    (with-live-view (view)
      (let ((cell (apply #'gl-pointer-cell view (glfw:cursor-location view))))
        (case action
          (:press
           (setf (gl-view-button-down-p view) t)
           (deliver-gl-mouse view :press cell (count-gl-click view cell)))
          (:release
           (setf (gl-view-button-down-p view) nil)
           (deliver-gl-mouse view :release cell (gl-view-click-count view))))))))

(defmethod glfw:mouse-moved ((view gl-view) x y)
  (when (gl-view-button-down-p view)
    (with-live-view (view)
      (deliver-gl-mouse view :drag (gl-pointer-cell view x y) (gl-view-click-count view)))))

;;; Other window events

(defmethod glfw:mouse-scrolled ((view gl-view) xoffset yoffset)
  (declare (ignore xoffset))
  (with-live-view (view)
    (when (gl-view-scroll-callback view)
      (let* ((rows (+ (gl-view-scroll-remainder view) (* yoffset *gl-scroll-rows*)))
             (whole (truncate rows)))
        (setf (gl-view-scroll-remainder view) (- rows whole))
        (unless (zerop whole)
          ;; Scrolling content down (positive offset) moves back in history.
          (funcall (gl-view-scroll-callback view) (- whole)))))))

(defmethod glfw:framebuffer-resized ((view gl-view) width height)
  (declare (ignore width height))
  (with-live-view (view)
    (when (gl-view-resize-callback view)
      ;; Resize at once: on macOS GLFW calls this from inside AppKit's
      ;; live-resize loop, where timers do not run.  Then run work the
      ;; reader thread posted, so the program's redraw shows while dragging.
      (funcall (gl-view-resize-callback view) t)
      (run-gl-wake-thunks (gl-view-backend view)))
    (redraw-gl-view view)))

(defmethod glfw:window-refreshed ((view gl-view))
  (with-live-view (view)
    (redraw-gl-view view)))

(defmethod glfw:window-content-scale-changed ((view gl-view) xscale yscale)
  (declare (ignore xscale))
  (with-live-view (view)
    ;; The window moved to a display with another scale: re-rasterize.
    (unless (= yscale (gl-view-scale view))
      (gl-view-set-font view yscale)
      (when (gl-view-resize-callback view)
        (funcall (gl-view-resize-callback view) t))
      (redraw-gl-view view))))

(defmethod glfw:window-closed ((view gl-view))
  (with-live-view (view)
    (setf (gl-view-closed-p view) t)
    (when (gl-view-close-callback view)
      (funcall (gl-view-close-callback view)))
    (setf (gl-backend-quit-p (gl-view-backend view)) t)))

;;; Event loop

(defmethod backend-wakeup ((backend gl-backend) view thunk)
  (declare (ignore view))
  (bt:with-lock-held ((gl-backend-wake-lock backend))
    (push thunk (gl-backend-wake-thunks backend)))
  ;; Thread-safe: unblocks the main thread's wait for events.
  (%glfw:post-empty-event)
  t)

(defun run-gl-wake-thunks (backend)
  (let ((thunks (bt:with-lock-held ((gl-backend-wake-lock backend))
                  (shiftf (gl-backend-wake-thunks backend) '()))))
    (dolist (thunk (nreverse thunks))
      (funcall thunk))))

(defun run-gl-timers (backend)
  (let ((now (get-internal-real-time))
        (due '()))
    (setf (gl-backend-timers backend)
          (remove-if (lambda (timer)
                       (when (<= (first timer) now)
                         (push timer due)
                         t))
                     (gl-backend-timers backend)))
    (dolist (timer (sort due #'< :key #'first))
      (funcall (third timer)))))

(defun gl-next-timeout (backend)
  (let ((timers (gl-backend-timers backend)))
    (if timers
        (max 0 (/ (- (reduce #'min timers :key #'first) (get-internal-real-time))
                  internal-time-units-per-second))
        0.25)))

(defun call-with-glfw (thunk)
  "Run THUNK with GLFW initialized and cl-opengl resolving entry points
through GLFW.  Masks float traps, which platform code (AppKit in
particular) trips over."
  (float-features:with-float-traps-masked t
    (glfw:init)
    (let ((cl-opengl-bindings:*gl-get-proc-address* #'%glfw:get-proc-address))
      (unwind-protect (funcall thunk)
        (glfw:shutdown)))))

(defmethod backend-call-with-event-loop ((backend gl-backend) thunk)
  (call-with-glfw
   (lambda ()
     (unwind-protect
          (progn
            (funcall thunk)
            (loop until (gl-backend-quit-p backend)
                  do (glfw:poll-events :timeout (min 0.25 (gl-next-timeout backend)))
                     (run-gl-wake-thunks backend)
                     (run-gl-timers backend)))
       (let ((view (gl-backend-view backend)))
         (when view
           (destroy-gl-view view)))))))

;;; Views

(defun gl-view-pixel-size (view)
  (values-list (glfw:framebuffer-size view)))

(defun redraw-gl-view (view &optional (palette *palette*))
  (let ((grid (gl-view-grid view)))
    (when (and grid (not (gl-view-closed-p view)))
      (glfw:make-current view)
      (setf (gl-view-instances view)
            (pack-grid grid (gl-view-atlas view) palette (gl-view-instances view)))
      (multiple-value-bind (width height) (gl-view-pixel-size view)
        (gl-draw (gl-view-renderer view)
                 (gl-view-instances view)
                 (grid-columns grid)
                 (* (grid-rows grid) (grid-columns grid))
                 (gl-view-cell-width view)
                 (gl-view-cell-height view)
                 width height
                 (gl-view-atlas view)
                 palette))
      (glfw:swap-buffers view))))

(defun snapshot-gl-view (view &optional pathname (palette *palette*))
  "Render VIEW's current grid offscreen.  Return the RGB octets, width and
height, after writing them to PATHNAME as a PPM when given."
  (let ((grid (gl-view-grid view)))
    (glfw:make-current view)
    (multiple-value-bind (width height) (gl-view-pixel-size view)
      (setf (gl-view-instances view)
            (pack-grid grid (gl-view-atlas view) palette (gl-view-instances view)))
      (let ((rgb (gl-snapshot (gl-view-renderer view) (gl-view-instances view)
                              (grid-columns grid) (* (grid-rows grid) (grid-columns grid))
                              (gl-view-cell-width view) (gl-view-cell-height view)
                              width height (gl-view-atlas view) palette)))
        (when pathname
          (write-ppm pathname rgb width height))
        (values rgb width height)))))

(defun gl-api-hints (api)
  (append
   (ecase api
     (:gl '(:client-api :opengl-api :context-version-major 3 :context-version-minor 3
            :opengl-profile :opengl-core-profile :opengl-forward-compat t))
     (:gles '(:client-api :opengl-es-api :context-version-major 3 :context-version-minor 0)))
   ;; macOS has only NSGL; everywhere else, EGL rather than GLX.
   (unless (uiop:os-macosx-p)
     '(:context-creation-api :egl-context-api))))

(defun make-gl-view (backend width height title font-options scale)
  "Create the window with the first API in *GL-API* that works."
  (dolist (api (ecase *gl-api*
                 (:auto (if (uiop:os-macosx-p) '(:gl) '(:gl :gles)))
                 (:gl '(:gl))
                 (:gles '(:gles)))
               (error "No OpenGL 3.3 or OpenGL ES 3.0 context could be created."))
    (let ((view (handler-case
                    (apply #'make-instance 'gl-view
                           ;; Lets the wrapper tell our initargs from hints.
                           :allow-other-keys t
                           :backend backend :api api :font-options font-options
                           :width width :height height :title title
                           :visible *gl-window-visible*
                           ;; Window sizes are in screen coordinates; let
                           ;; GLFW scale them and the framebuffer.
                           :scale-to-monitor t
                           (gl-api-hints api))
                  (error (condition)
                    (format *error-output* "~&slt: no ~a context: ~a~%" api condition)
                    nil))))
      (when view
        (setf (gl-view-scale view) scale)
        (return view)))))

(defmethod backend-available-font-families ((backend gl-backend))
  #+darwin '()
  #-darwin (or (ignore-errors (font-monospace-families)) '()))

(defmethod backend-resolve-font-size ((backend gl-backend) font-size)
  (abs (or font-size 15)))

(defmethod backend-create-view ((backend gl-backend)
                                &key rows columns font-family font-size
                                  cell-width cell-height title)
  (let* ((options (list font-family font-size cell-width cell-height))
         (scale (let ((monitor (glfw:primary-monitor)))
                  (if monitor (max 1.0 (second (glfw:content-scale monitor))) 1.0)))
         ;; Measure at the primary display's scale to size the window.
         (font (open-glyph-font font-family (* font-size scale)))
         (cw (or (and cell-width (round (* cell-width scale)))
                 (nth-value 0 (glyph-font-cell-size font))))
         (ch (or (and cell-height (round (* cell-height scale)))
                 (nth-value 1 (glyph-font-cell-size font))))
         (view (make-gl-view backend
                             (max 1 (round (* columns cw) scale))
                             (max 1 (round (* rows ch) scale))
                             (or title "slt") options scale)))
    (close-glyph-font font)
    (glfw:make-current view)
    (setf (glfw:swap-interval view) (if *gl-vsync* 1 0)
          (gl-view-renderer view) (make-gl-renderer (gl-view-api view))
          (gl-backend-view backend) view)
    ;; The window may have opened on a display with another scale.
    (gl-view-set-font view (gl-view-content-scale view))
    view))

(defun destroy-gl-view (view)
  (when (glfw:pointer view)
    (glfw:make-current view)
    (ignore-errors (free-gl-renderer (gl-view-renderer view)))
    (glfw:destroy view))
  (when (gl-view-font view)
    (ignore-errors (close-glyph-font (gl-view-font view)))
    (setf (gl-view-font view) nil))
  (setf (gl-view-closed-p view) t))

(defmethod backend-view-cell-width ((backend gl-backend) (view gl-view))
  (gl-view-cell-width view))

(defmethod backend-view-cell-height ((backend gl-backend) (view gl-view))
  (gl-view-cell-height view))

(defmethod backend-view-window-size ((backend gl-backend) (view gl-view))
  (gl-view-pixel-size view))

(defmethod backend-resize-view ((backend gl-backend) (view gl-view) rows columns)
  (declare (ignore rows columns))
  view)

(defmethod backend-render-grid ((backend gl-backend) (view gl-view) grid)
  (setf (gl-view-grid view) grid)
  (redraw-gl-view view))

(defmethod backend-set-title ((backend gl-backend) (view gl-view) title)
  (unless (gl-view-closed-p view)
    (setf (glfw:title view) title)))

(defmethod backend-schedule ((backend gl-backend) (view gl-view) delay-ms thunk)
  (let ((token (incf (gl-backend-timer-seq backend))))
    (push (list (+ (get-internal-real-time)
                   (round (* (max 0 delay-ms) internal-time-units-per-second) 1000))
                token
                thunk)
          (gl-backend-timers backend))
    token))

(defmethod backend-cancel-scheduled ((backend gl-backend) (view gl-view) token)
  (setf (gl-backend-timers backend)
        (remove token (gl-backend-timers backend) :key #'second)))

(defmethod backend-bind-resize ((backend gl-backend) (view gl-view) callback)
  (setf (gl-view-resize-callback view) callback))

(defmethod backend-bind-keypress ((backend gl-backend) (view gl-view) callback)
  (setf (gl-view-keypress-callback view) callback))

(defmethod backend-bind-scroll ((backend gl-backend) (view gl-view) callback)
  (setf (gl-view-scroll-callback view) callback))

(defmethod backend-bind-mouse ((backend gl-backend) (view gl-view) callback)
  (setf (gl-view-mouse-callback view) callback))

(defmethod backend-clipboard-text ((backend gl-backend) (view gl-view))
  (glfw:clipboard-string view))

(defmethod backend-set-clipboard-text ((backend gl-backend) (view gl-view) text)
  (setf (glfw:clipboard-string view) text))

(defmethod backend-on-close ((backend gl-backend) (view gl-view) callback)
  (setf (gl-view-close-callback view) callback))

(defmethod backend-focus ((backend gl-backend) (view gl-view))
  (when *gl-window-visible*
    (glfw:show view)
    (glfw:focus view)))

(defmethod backend-request-exit ((backend gl-backend) (view gl-view))
  (setf (gl-backend-quit-p backend) t)
  (setf (gl-view-closed-p view) t))

(register-backend :gl (lambda () (make-instance 'gl-backend)))
