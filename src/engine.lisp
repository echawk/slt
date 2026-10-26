(in-package #:slt)

;;;; An engine is the VT state machine: it consumes the child's output,
;;;; maintains the screen, and copies it into a GRID for rendering.
;;;; Engines are interchangeable (lispvt, libvterm, libghostty-vt) and
;;;; are exercised against each other by the conformance suite.

(defclass engine ()
  ((output-function
    :initarg :output-function
    :initform nil
    :accessor engine-output-function
    :documentation "Called with a string the engine wants written to the
child, e.g. replies to device attribute queries.")
   (title-function
    :initarg :title-function
    :initform nil
    :accessor engine-title-function)
   (bell-function
    :initarg :bell-function
    :initform nil
    :accessor engine-bell-function)
   (scroll-function
    :initarg :scroll-function
    :initform nil
    :accessor engine-scroll-function
    :documentation "Called during ENGINE-FEED or ENGINE-RESIZE with TOP,
BOTTOM, COUNT and optionally SCREEN (:PRIMARY or :ALTERNATE, the current
screen when omitted) when rows TOP..BOTTOM (inclusive) move up COUNT rows
(down when negative), by engines whose ENGINE-REPORTS-SCROLLS-P is true.")))

(defgeneric engine-reports-scrolls-p (engine)
  (:documentation "True when ENGINE calls its scroll function for every scroll.")
  (:method ((engine engine))
    nil))

(defgeneric engine-reports-all-scrolls-p (engine)
  (:documentation "True when the scroll reports are complete; otherwise the
emulator also infers scrolls from the screen contents.")
  (:method ((engine engine))
    (engine-reports-scrolls-p engine)))

(defgeneric engine-viewport-offset (engine)
  (:documentation "Rows of scrollback shown above the screen: 0 when following
the live screen, NIL when scrolled back by an unknown amount.")
  (:method ((engine engine))
    0))

(defgeneric engine-rows (engine))
(defgeneric engine-columns (engine))

(defgeneric engine-feed (engine octets &key start end)
  (:documentation "Process OCTETS (child output, UTF-8) from START to END."))

(defgeneric engine-resize (engine rows columns))

(defgeneric engine-cursor-position (engine)
  (:documentation "Return the engine's current zero-based cursor ROW and COLUMN.")
  (:method ((engine engine))
    (values 0 0)))

(defgeneric engine-update-grid (engine grid &key force)
  (:documentation "Copy the engine's screen into GRID, resizing GRID when its
dimensions differ.  Set the dirty bit for every row whose contents or
cursor state changed.  With FORCE, copy and dirty every row."))

(defgeneric engine-mode (engine mode)
  (:documentation "Query a terminal mode; MODE is a keyword such as
:application-cursor, :application-keypad or :bracketed-paste.  The mode
:mouse-tracking answers NIL or the mouse events the child asked for: :X10
(presses), :NORMAL (presses and releases), :BUTTON (also motion with a
button held) or :ANY (all motion); :mouse-sgr whether reports use the SGR
(1006) encoding.")
  (:method ((engine engine) mode)
    (declare (ignore mode))
    nil))

(defgeneric engine-encode-key (engine key modifiers)
  (:documentation "Return the string to send to the child for KEY (a
character or a key keyword) with MODIFIERS (a list of :shift, :control,
:meta, :super), or NIL when the key produces no input.")
  (:method ((engine engine) key modifiers)
    (xterm-encode-key key modifiers
                      :application-cursor (engine-mode engine :application-cursor))))

(defgeneric engine-encode-mouse (engine action button row column modifiers)
  (:documentation "Return the string reporting a mouse event to the child,
or NIL when the child's tracking mode does not report it.  ACTION is
:PRESS, :RELEASE or :MOTION; BUTTON is 1, 2 or 3 (left, middle, right),
4 or 5 (wheel up, down) or NIL for motion with no button held; ROW and
COLUMN are the zero-based cell; MODIFIERS as for ENGINE-ENCODE-KEY.")
  (:method ((engine engine) action button row column modifiers)
    (xterm-encode-mouse action button row column modifiers
                        :tracking (engine-mode engine :mouse-tracking)
                        :sgr (engine-mode engine :mouse-sgr))))

(defgeneric engine-scroll (engine how)
  (:documentation "Scroll the viewport through scrollback: HOW is :TOP,
:BOTTOM or a number of rows (negative scrolls back).  Return true when the
engine supports scrollback.")
  (:method ((engine engine) how)
    (declare (ignore how))
    nil))

(defgeneric engine-close (engine)
  (:method ((engine engine))
    nil))

(defun engine-emit-output (engine string)
  (let ((function (engine-output-function engine)))
    (when (and function string (plusp (length string)))
      (funcall function string))))

(defun engine-emit-title (engine title)
  (let ((function (engine-title-function engine)))
    (when function
      (funcall function title))))

(defun engine-emit-scroll (engine top bottom count &optional screen)
  (let ((function (engine-scroll-function engine)))
    (when function
      (if screen
          (funcall function top bottom count screen)
          (funcall function top bottom count)))))

(defun engine-emit-bell (engine)
  (let ((function (engine-bell-function engine)))
    (when function
      (funcall function))))

(defun engine-feed-string (engine string)
  "Convenience for tests and REPL use: feed STRING encoded as UTF-8."
  (engine-feed engine (string->octets string)))

;;; Registry

(defvar *engine-constructors* (make-hash-table :test #'eq))

(defvar *default-engine* nil
  "The engine used when none is requested; NIL picks the first entry of
*ENGINE-PREFERENCE* that is available.")

(defvar *engine-preference* '(:ghostty :lispvt :libvterm)
  "Engines tried, fastest first, when *DEFAULT-ENGINE* is NIL.")

(defun register-engine (name constructor)
  "CONSTRUCTOR is called with ROWS, COLUMNS and engine initargs."
  (setf (gethash (normalize-frontend-name name) *engine-constructors*)
        constructor)
  (normalize-frontend-name name))

(defun available-engines (&key probe)
  "The registered engines.  With PROBE, first try loading the engines in
*ENGINE-PREFERENCE*, so that engines not used yet are listed too."
  (when probe
    (mapc #'engine-available-p *engine-preference*))
  (sort (loop for name being the hash-keys of *engine-constructors*
              collect name)
        #'string<
        :key #'symbol-name))

(defun load-engine-system (name)
  "Load the ASDF system slt/NAME (e.g. slt/ghostty) if it exists, which
registers that engine.  Engines with native libraries live in their own
systems so that slt itself does not require them."
  (let ((system (format nil "slt/~(~a~)" name)))
    (when (asdf:find-system system nil)
      (asdf:load-system system)
      t)))

(defun engine-available-p (name)
  "True when engine NAME is registered or its system loads (which fails,
for example, when a native library has not been built)."
  (or (gethash name *engine-constructors*)
      (and (ignore-errors (load-engine-system name))
           (gethash name *engine-constructors*))))

(defun default-engine ()
  (or *default-engine*
      (find-if #'engine-available-p *engine-preference*)
      :lispvt))

(defun make-engine (designator &rest arguments &key rows columns &allow-other-keys)
  (declare (ignore rows columns))
  (if (typep designator 'engine)
      designator
      (let* ((name (normalize-frontend-name (or designator (default-engine))))
             (constructor (or (gethash name *engine-constructors*)
                              (and (load-engine-system name)
                                   (gethash name *engine-constructors*)))))
        (unless constructor
          (error "Unknown engine ~S. Available engines: ~{~S~^, ~}."
                 name
                 (available-engines)))
        (apply constructor arguments))))

;;; Default xterm key encoding, shared by engines without their own encoder.

(defun xterm-modifier-parameter (modifiers)
  "The xterm modifier parameter: 1 + shift + 2*meta + 4*control + 8*super."
  (+ 1
     (if (member :shift modifiers) 1 0)
     (if (member :meta modifiers) 2 0)
     (if (member :control modifiers) 4 0)
     (if (member :super modifiers) 8 0)))

(defparameter *xterm-cursor-keys*
  '((:up . #\A) (:down . #\B) (:right . #\C) (:left . #\D)
    (:home . #\H) (:end . #\F)
    (:f1 . #\P) (:f2 . #\Q) (:f3 . #\R) (:f4 . #\S)))

(defparameter *xterm-tilde-keys*
  '((:insert . 2) (:delete . 3) (:page-up . 5) (:page-down . 6)
    (:f5 . 15) (:f6 . 17) (:f7 . 18) (:f8 . 19) (:f9 . 20)
    (:f10 . 21) (:f11 . 23) (:f12 . 24)))

(defun control-string-for-char (char)
  (cond
    ((char= char #\Space)
     (string (code-char 0)))
    ((char= char #\?)
     (string (code-char 127)))
    (t
     (let ((code (char-code (char-upcase char))))
       (when (<= (char-code #\@) code (char-code #\_))
         (string (code-char (- code 64))))))))

(defun xterm-encode-key (key modifiers &key application-cursor)
  (let* ((esc (string (code-char 27)))
         (parameter (xterm-modifier-parameter modifiers))
         (modifiedp (> parameter 1))
         (metap (member :meta modifiers)))
    (flet ((meta-prefix (string)
             (if (and metap string)
                 (concatenate 'string esc string)
                 string)))
      (cond
        ((characterp key)
         (meta-prefix
          (if (member :control modifiers)
              (or (control-string-for-char key) (string key))
              (string key))))
        ((assoc key *xterm-cursor-keys*)
         (let ((final (cdr (assoc key *xterm-cursor-keys*)))
               (function-key-p (member key '(:f1 :f2 :f3 :f4))))
           (cond
             (modifiedp (format nil "~a[1;~d~c" esc parameter final))
             ((or function-key-p application-cursor)
              (format nil "~aO~c" esc final))
             (t (format nil "~a[~c" esc final)))))
        ((assoc key *xterm-tilde-keys*)
         (let ((number (cdr (assoc key *xterm-tilde-keys*))))
           (if modifiedp
               (format nil "~a[~d;~d~~" esc number parameter)
               (format nil "~a[~d~~" esc number))))
        (t
         (case key
           (:enter (meta-prefix (string #\Return)))
           (:tab (if (member :shift modifiers)
                     (format nil "~a[Z" esc)
                     (meta-prefix (string #\Tab))))
           (:backspace (meta-prefix
                        (string (code-char (if (member :control modifiers) 8 127)))))
           (:escape (meta-prefix esc))
           (t nil)))))))

;;; Default xterm mouse reporting.

(defun xterm-encode-mouse (action button row column modifiers &key tracking sgr)
  "The xterm report of a mouse event under TRACKING (see ENGINE-MODE), in
the SGR encoding when SGR, else the legacy one.  SGR :PIXELS is SGR-Pixels
(mode 1016): ROW and COLUMN are then pixels, reported from 0 as Ghostty
does."
  (let ((code (case button
                ((1 2 3) (1- button))
                (4 64)
                (5 65)
                (t 3))))
    (when (and tracking
               (ecase action
                 (:press button)
                 ;; Wheel buttons have no release.
                 (:release (and (not (eq tracking :x10)) (member button '(nil 1 2 3))))
                 (:motion (or (eq tracking :any)
                              (and (eq tracking :button) button)))))
      (when (eq action :motion)
        (incf code 32))
      (unless (eq tracking :x10)
        (incf code (+ (if (member :shift modifiers) 4 0)
                      (if (member :meta modifiers) 8 0)
                      (if (member :control modifiers) 16 0))))
      (cond
        (sgr
         (let ((origin (if (eq sgr :pixels) 0 1)))
           (format nil "~c[<~d;~d;~d~c" (code-char 27) code (+ origin column) (+ origin row)
                   (if (eq action :release) #\m #\M))))
        ;; Input reaches the child as UTF-8, so only coordinates that fit
        ;; in one ASCII octet can be sent in the legacy encoding.
        ((and (< (+ 33 column) 128) (< (+ 33 row) 128))
         (format nil "~c[M~c~c~c" (code-char 27)
                 (code-char (+ 32 (if (eq action :release)
                                      (logior 3 (logand code (lognot 3)))
                                      code)))
                 (code-char (+ 33 column))
                 (code-char (+ 33 row))))))))
