(in-package #:slt)

(defclass terminal-frontend () ())

(defvar *option-as-meta* t
  "When true, macOS's Option key acts as Meta instead of composing characters.")

(defvar *frontend-constructors* (make-hash-table :test #'eq))

(defun normalize-frontend-name (designator)
  (etypecase designator
    (keyword designator)
    (symbol (intern (string-upcase (symbol-name designator)) :keyword))
    (string (intern (string-upcase designator) :keyword))))

(defun register-frontend (name constructor)
  (setf (gethash (normalize-frontend-name name) *frontend-constructors*)
        constructor)
  (normalize-frontend-name name))

(defvar *optional-frontends* '(:clx :gl :gtk :ltk :sdl2)
  "Frontends that live in their own slt/NAME systems and load on first use.")

(defun load-frontend-system (name)
  "Load slt/NAME if it exists, which registers that frontend."
  (let ((system (format nil "slt/~(~a~)" name)))
    (when (asdf:find-system system nil)
      (asdf:load-system system)
      t)))

(defun frontend-available-p (name)
  "True when frontend NAME is registered or its system loads."
  (or (gethash name *frontend-constructors*)
      (and (ignore-errors (load-frontend-system name))
           (gethash name *frontend-constructors*))))

(defun available-frontends (&key probe)
  "The registered frontends.  With PROBE, first try loading the optional
ones, so that frontends not used yet are listed too."
  (when probe
    (mapc #'frontend-available-p *optional-frontends*))
  (sort (loop for name being the hash-keys of *frontend-constructors*
              collect name)
        #'string<
        :key #'symbol-name))

(defun make-frontend (designator)
  (typecase designator
    (terminal-frontend designator)
    ((or symbol string)
     (let* ((name (normalize-frontend-name designator))
            (constructor (or (gethash name *frontend-constructors*)
                             ;; Optional frontends live in their own systems.
                             (and (load-frontend-system name)
                                  (gethash name *frontend-constructors*)))))
       (unless constructor
         (error "Unknown frontend ~S. Available frontends: ~{~S~^, ~}."
                name
                (available-frontends)))
       (funcall constructor)))))

(defgeneric frontend-call-with-event-loop (frontend thunk))
(defgeneric frontend-available-font-families (frontend))
(defgeneric frontend-resolve-font-size (frontend font-size))
(defgeneric frontend-create-view (frontend &key rows columns font-family font-size
                                            cell-width cell-height title))
(defgeneric frontend-view-cell-width (frontend view))
(defgeneric frontend-view-cell-height (frontend view))
(defgeneric frontend-view-window-size (frontend view))
(defgeneric frontend-resize-view (frontend view rows columns))
(defgeneric frontend-render-grid (frontend view grid)
  (:documentation "Draw the rows of GRID whose dirty bit is set."))
(defgeneric frontend-render-images (frontend view store)
  (:documentation "Draw the image placements in STORE. Text and image stacking
is described by each placement's z-index. Frontends without image rendering may
retain STORE for inspection but otherwise ignore it.")
  (:method (frontend view store)
    (declare (ignore frontend view store))
    nil))
(defgeneric frontend-set-title (frontend view title))
(defgeneric frontend-schedule (frontend view delay-ms thunk))
(defgeneric frontend-cancel-scheduled (frontend view token))
(defgeneric frontend-bind-resize (frontend view callback))
(defgeneric frontend-bind-keypress (frontend view callback))
(defgeneric frontend-on-close (frontend view callback))
(defgeneric frontend-wakeup (frontend view thunk)
  (:documentation "Called from a non-UI thread: arrange for THUNK to run
soon on the UI thread and return true.  Frontends that cannot be woken from
another thread return NIL; the emulator's poll timer then runs the work.")
  (:method (frontend view thunk)
    (declare (ignore frontend view thunk))
    nil))

(defgeneric frontend-bind-scroll (frontend view callback)
  (:documentation "Arrange for CALLBACK to be called with a number of rows
to scroll (negative is back through history) when the user scrolls.")
  (:method (frontend view callback)
    (declare (ignore frontend view callback))
    nil))

(defgeneric frontend-bind-paste (frontend view callback)
  (:documentation "Arrange for CALLBACK to be called with pasted text.")
  (:method (frontend view callback)
    (declare (ignore frontend view callback))
    nil))
(defgeneric frontend-bind-mouse (frontend view callback)
  (:documentation "Arrange for CALLBACK to be called with ACTION (:PRESS,
:DRAG or :RELEASE), ROW, COLUMN and CLICKS (1 for a single click, 2 for a
double click, ...), and optionally BUTTON (1, 2 or 3 for left, middle and
right; 1 when omitted) and the modifier STATE (as for keypresses), when a
mouse button is used.  Frontends that see the pointer move with no button
held may also call it with ACTION :MOTION.  ROW and COLUMN are the cell
under the pointer and may lie outside the grid.")
  (:method (frontend view callback)
    (declare (ignore frontend view callback))
    nil))

(defgeneric frontend-set-mouse-tracking (frontend view tracking)
  (:documentation "Called when the child starts (TRACKING true) or stops
asking for mouse reports.  Frontends whose own text selection competes with
the mouse, such as a browser's, use it to hand the mouse over.")
  (:method (frontend view tracking)
    (declare (ignore frontend view tracking))
    nil))

(defgeneric frontend-bind-menu (frontend view items)
  (:documentation "Offer ITEMS in a menu: a list of (LABEL FUNCTION), where
LABEL is a string or a function returning one, called each time the menu
is shown, and FUNCTION is called with no arguments when the item is
chosen.  Frontends without menus ignore it.")
  (:method (frontend view items)
    (declare (ignore frontend view items))
    nil))

(defvar *view-palettes* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Palettes of views whose colors a program changed (OSC 4, 10, 11 ...).")

(defun view-palette (view)
  "The palette VIEW is drawn with: its own, or *PALETTE*."
  (or (and view (gethash view *view-palettes*)) *palette*))

(defun (setf view-palette) (palette view)
  (if palette
      (setf (gethash view *view-palettes*) palette)
      (remhash view *view-palettes*))
  palette)

(defmacro with-view-palette ((view) &body body)
  "Run BODY with *PALETTE* bound to VIEW's palette."
  `(let ((*palette* (view-palette ,view)))
     ,@body))

(defun applescript-string (string)
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across string
          do (when (member char '(#\" #\\)) (write-char #\\ out))
             (write-char (if (char< char #\Space) #\Space char) out))
    (write-char #\" out)))

(defgeneric frontend-notify (frontend view title body)
  (:documentation "Show a desktop notification.  The default uses the
platform's command: osascript on macOS, notify-send elsewhere.")
  (:method (frontend view title body)
    (declare (ignore frontend view))
    (let ((title (or title "slt"))
          (body (or body "")))
      (ignore-errors
       (if (uiop:os-macosx-p)
           (uiop:launch-program
            (list "osascript" "-e"
                  (format nil "display notification ~a with title ~a"
                          (applescript-string body) (applescript-string title)))
            :output nil :error-output nil)
           (when (probe-file "/usr/bin/notify-send")
             (uiop:launch-program (list "notify-send" "--" title body)
                                  :output nil :error-output nil)))))))

(defgeneric frontend-clipboard-text (frontend view)
  (:documentation "The text on the system clipboard, or NIL.")
  (:method (frontend view)
    (declare (ignore frontend view))
    nil))

(defgeneric frontend-set-clipboard-text (frontend view text)
  (:documentation "Put TEXT on the system clipboard.")
  (:method (frontend view text)
    (declare (ignore frontend view text))
    nil))

(defvar *window-padding* 4
  "Space in points between the window's edges and the grid, on frontends
that support it (cocoa).")

(defgeneric frontend-set-font-size (frontend view size)
  (:documentation "Draw with the font at SIZE points from now on, and
return true; the emulator then refits the grid to the window.  Frontends
that cannot change the font return NIL.")
  (:method (frontend view size)
    (declare (ignore frontend view size))
    nil))

(defgeneric frontend-bind-focus (frontend view callback)
  (:documentation "Arrange for CALLBACK to be called with true when the
window gains the keyboard focus and NIL when it loses it.")
  (:method (frontend view callback)
    (declare (ignore frontend view callback))
    nil))

(defgeneric frontend-bind-drop (frontend view callback)
  (:documentation "Arrange for CALLBACK to be called with a list of native
file names when files are dropped on the window.")
  (:method (frontend view callback)
    (declare (ignore frontend view callback))
    nil))

(defgeneric frontend-bind-commands (frontend view commands)
  (:documentation "Offer COMMANDS, an alist from a command keyword (:NEW-WINDOW, :COPY,
:PASTE, :SELECT-ALL, :CLEAR-SCROLLBACK, :BIGGER, :SMALLER, :ACTUAL-SIZE,
:RELOAD-CONFIG, :EDIT-CONFIG) to a function of no arguments, in the
frontend's application menus.  Frontends without menus ignore it.")
  (:method (frontend view commands)
    (declare (ignore frontend view commands))
    nil))

(defgeneric frontend-bell (frontend view audible)
  (:documentation "Ring the bell: beep when AUDIBLE, and draw the user's
attention when the window is in the background.")
  (:method (frontend view audible)
    (declare (ignore frontend view audible))
    nil))

(defgeneric frontend-focus (frontend view))
(defgeneric frontend-request-exit (frontend view))

(defmethod frontend-call-with-event-loop ((frontend terminal-frontend) thunk)
  (declare (ignore frontend))
  (funcall thunk))

(defmethod frontend-available-font-families ((frontend terminal-frontend))
  (declare (ignore frontend))
  '())

(defmethod frontend-resolve-font-size ((frontend terminal-frontend) font-size)
  (declare (ignore frontend))
  (terminal-font-size font-size))

(defun default-font-family (&optional (operating-system (uiop:operating-system)))
  (case operating-system
    ((:darwin :macosx) "Menlo")
    (:linux "DejaVu Sans Mono")
    ((:freebsd :openbsd :netbsd) "Monospace")
    (otherwise "Monospace")))

(defun preferred-font-families (&optional (operating-system (uiop:operating-system)))
  (case operating-system
    ((:darwin :macosx)
     '("SF Mono" "JetBrains Mono" "Iosevka Term" "Menlo" "Monaco"))
    (:linux
     '("JetBrains Mono" "Iosevka Term" "DejaVu Sans Mono"
       "Hack" "Liberation Mono" "Noto Sans Mono"))
    ((:freebsd :openbsd :netbsd)
     '("JetBrains Mono" "Iosevka Term" "DejaVu Sans Mono"
       "Liberation Mono" "Monospace"))
    (otherwise
     '("Monospace"))))

(defun pick-font-family (available-families preferred-families fallback)
  (or (find-if (lambda (family)
                 (member family available-families :test #'string-equal))
               preferred-families)
      fallback))

(defun resolve-font-family (&optional font-family
                              (available-families '())
                              (operating-system (uiop:operating-system)))
  (or font-family
      (pick-font-family available-families
                        (preferred-font-families operating-system)
                        (default-font-family operating-system))))

(defun terminal-font-size (font-size)
  (let ((size (or font-size 15)))
    (if (plusp size)
        (- size)
        size)))

(defun normalize-font-metric (value &optional (default 0))
  (typecase value
    (integer value)
    (string (or (parse-integer value :junk-allowed t) default))
    (null default)
    (t default)))

(defun resolve-cell-dimensions (character-width linespace
                                &key cell-width cell-height)
  (values (or cell-width character-width)
          (or cell-height linespace)))

;;; Decorations for frontends that draw with rectangles.  They match the GPU
;;; shaders: lines MAX(1, CELL-HEIGHT/18) pixels thick, patterns following
;;; the window's x so they run on across cells.

(defun decoration-thickness (cell-height)
  (max 1 (floor cell-height 18)))

(defun underline-rectangles (style x width cell-height)
  "The rectangles (X Y WIDTH HEIGHT), Y from the cell's top, that draw an
underline of STYLE (see UNDERLINE-STYLE) under cells from X, WIDTH pixels
wide."
  (let* ((thickness (decoration-thickness cell-height))
         (band (- cell-height (* 2 thickness))))
    (flet ((pattern (on-p)
             ;; Runs of pixel columns for which ON-P is true.
             (loop with rectangles = '()
                   with start = nil
                   for px from x to (+ x width)
                   for on = (and (< px (+ x width)) (funcall on-p px))
                   do (cond ((and on (not start)) (setf start px))
                            ((and start (not on))
                             (push (list start band (- px start) thickness) rectangles)
                             (setf start nil)))
                   finally (return (nreverse rectangles)))))
      (ecase style
        ((nil) '())
        (:single (list (list x band width thickness)))
        (:double (list (list x (- cell-height thickness) width thickness)
                       (list x (- cell-height (* 3 thickness)) width thickness)))
        (:dotted (pattern (lambda (px) (evenp (floor px (* 2 thickness))))))
        (:dashed (pattern (lambda (px) (< (mod px (* 8 thickness)) (* 6 thickness)))))
        (:curly
         (let* ((amplitude thickness)
                (centre (- cell-height (* 1.5 thickness) amplitude))
                (period (max 6 (* 8 amplitude))))
           (loop for px from x below (+ x width)
                 collect (list px
                               (round (- (+ centre (* amplitude (sin (/ (* px 2 pi) period))))
                                         (/ thickness 2)))
                               1 thickness))))))))

(defun uri-list-paths (uris)
  "The local file names in URIS, a text/uri-list as a string or a list of
URIs, as X11 drag and drop delivers dropped files."
  (loop for uri in (if (stringp uris)
                       (uiop:split-string uris :separator '(#\Newline))
                       uris)
        for trimmed = (string-trim '(#\Return #\Space) uri)
        for path = (and (plusp (length trimmed))
                        (char/= (char trimmed 0) #\#)
                        (terminal-osc:parse-working-directory trimmed))
        when path collect path))
