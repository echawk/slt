(in-package #:slt)

(defclass terminal-backend () ())

(defvar *option-as-meta* t
  "When true, macOS's Option key acts as Meta instead of composing characters.")

(defvar *backend-constructors* (make-hash-table :test #'eq))

(defun normalize-backend-name (designator)
  (etypecase designator
    (keyword designator)
    (symbol (intern (string-upcase (symbol-name designator)) :keyword))
    (string (intern (string-upcase designator) :keyword))))

(defun register-backend (name constructor)
  (setf (gethash (normalize-backend-name name) *backend-constructors*)
        constructor)
  (normalize-backend-name name))

(defvar *optional-backends* '(:gl :sdl2)
  "Backends that live in their own slt/NAME systems and load on first use.")

(defun load-backend-system (name)
  "Load slt/NAME if it exists, which registers that backend."
  (let ((system (format nil "slt/~(~a~)" name)))
    (when (asdf:find-system system nil)
      (asdf:load-system system)
      t)))

(defun backend-available-p (name)
  "True when backend NAME is registered or its system loads."
  (or (gethash name *backend-constructors*)
      (and (ignore-errors (load-backend-system name))
           (gethash name *backend-constructors*))))

(defun available-backends (&key probe)
  "The registered backends.  With PROBE, first try loading the optional
ones, so that backends not used yet are listed too."
  (when probe
    (mapc #'backend-available-p *optional-backends*))
  (sort (loop for name being the hash-keys of *backend-constructors*
              collect name)
        #'string<
        :key #'symbol-name))

(defun make-backend (designator)
  (typecase designator
    (terminal-backend designator)
    ((or symbol string)
     (let* ((name (normalize-backend-name designator))
            (constructor (or (gethash name *backend-constructors*)
                             ;; Optional backends live in their own systems.
                             (and (load-backend-system name)
                                  (gethash name *backend-constructors*)))))
       (unless constructor
         (error "Unknown backend ~S. Available backends: ~{~S~^, ~}."
                name
                (available-backends)))
       (funcall constructor)))))

(defgeneric backend-call-with-event-loop (backend thunk))
(defgeneric backend-available-font-families (backend))
(defgeneric backend-resolve-font-size (backend font-size))
(defgeneric backend-create-view (backend &key rows columns font-family font-size
                                            cell-width cell-height title))
(defgeneric backend-view-cell-width (backend view))
(defgeneric backend-view-cell-height (backend view))
(defgeneric backend-view-window-size (backend view))
(defgeneric backend-resize-view (backend view rows columns))
(defgeneric backend-render-grid (backend view grid)
  (:documentation "Draw the rows of GRID whose dirty bit is set."))
(defgeneric backend-set-title (backend view title))
(defgeneric backend-schedule (backend view delay-ms thunk))
(defgeneric backend-cancel-scheduled (backend view token))
(defgeneric backend-bind-resize (backend view callback))
(defgeneric backend-bind-keypress (backend view callback))
(defgeneric backend-on-close (backend view callback))
(defgeneric backend-wakeup (backend view thunk)
  (:documentation "Called from a non-UI thread: arrange for THUNK to run
soon on the UI thread and return true.  Backends that cannot be woken from
another thread return NIL; the frontend's poll timer then runs the work.")
  (:method (backend view thunk)
    (declare (ignore backend view thunk))
    nil))

(defgeneric backend-bind-scroll (backend view callback)
  (:documentation "Arrange for CALLBACK to be called with a number of rows
to scroll (negative is back through history) when the user scrolls.")
  (:method (backend view callback)
    (declare (ignore backend view callback))
    nil))

(defgeneric backend-bind-paste (backend view callback)
  (:documentation "Arrange for CALLBACK to be called with pasted text.")
  (:method (backend view callback)
    (declare (ignore backend view callback))
    nil))
(defgeneric backend-bind-mouse (backend view callback)
  (:documentation "Arrange for CALLBACK to be called with ACTION (:PRESS,
:DRAG or :RELEASE), ROW, COLUMN and CLICKS (1 for a single click, 2 for a
double click, ...) for the primary mouse button.  ROW and COLUMN are the
cell under the pointer and may lie outside the grid.")
  (:method (backend view callback)
    (declare (ignore backend view callback))
    nil))

(defgeneric backend-bind-menu (backend view items)
  (:documentation "Offer ITEMS in a menu: a list of (LABEL FUNCTION), where
LABEL is a string or a function returning one, called each time the menu
is shown, and FUNCTION is called with no arguments when the item is
chosen.  Backends without menus ignore it.")
  (:method (backend view items)
    (declare (ignore backend view items))
    nil))

(defgeneric backend-clipboard-text (backend view)
  (:documentation "The text on the system clipboard, or NIL.")
  (:method (backend view)
    (declare (ignore backend view))
    nil))

(defgeneric backend-set-clipboard-text (backend view text)
  (:documentation "Put TEXT on the system clipboard.")
  (:method (backend view text)
    (declare (ignore backend view text))
    nil))

(defgeneric backend-focus (backend view))
(defgeneric backend-request-exit (backend view))

(defmethod backend-call-with-event-loop ((backend terminal-backend) thunk)
  (declare (ignore backend))
  (funcall thunk))

(defmethod backend-available-font-families ((backend terminal-backend))
  (declare (ignore backend))
  '())

(defmethod backend-resolve-font-size ((backend terminal-backend) font-size)
  (declare (ignore backend))
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
