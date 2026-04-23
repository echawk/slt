(in-package #:slt)

(defclass terminal-backend () ())

(defparameter *backend-constructors* (make-hash-table :test #'eq))

(defun normalize-backend-name (designator)
  (etypecase designator
    (keyword designator)
    (symbol (intern (string-upcase (symbol-name designator)) :keyword))
    (string (intern (string-upcase designator) :keyword))))

(defun register-backend (name constructor)
  (setf (gethash (normalize-backend-name name) *backend-constructors*)
        constructor)
  (normalize-backend-name name))

(defun available-backends ()
  (sort (loop for name being the hash-keys of *backend-constructors*
              collect name)
        #'string<
        :key #'symbol-name))

(defun make-backend (designator)
  (typecase designator
    (terminal-backend designator)
    ((or symbol string)
     (let* ((name (normalize-backend-name designator))
            (constructor (gethash name *backend-constructors*)))
       (unless constructor
         (error "Unknown backend ~S. Available backends: ~{~S~^, ~}."
                name
                (available-backends)))
       (funcall constructor)))))

(defgeneric backend-call-with-event-loop (backend thunk))
(defgeneric backend-available-font-families (backend))
(defgeneric backend-create-view (backend &key rows columns font-family font-size
                                            cell-width cell-height title))
(defgeneric backend-view-cell-width (backend view))
(defgeneric backend-view-cell-height (backend view))
(defgeneric backend-view-window-size (backend view))
(defgeneric backend-resize-view (backend view rows columns))
(defgeneric backend-render-rows (backend view term rows))
(defgeneric backend-set-title (backend view title))
(defgeneric backend-schedule (backend view delay-ms thunk))
(defgeneric backend-cancel-scheduled (backend view token))
(defgeneric backend-bind-resize (backend view callback))
(defgeneric backend-bind-keypress (backend view callback))
(defgeneric backend-on-close (backend view callback))
(defgeneric backend-focus (backend view))
(defgeneric backend-request-exit (backend view))

(defmethod backend-call-with-event-loop ((backend terminal-backend) thunk)
  (declare (ignore backend))
  (funcall thunk))

(defmethod backend-available-font-families ((backend terminal-backend))
  (declare (ignore backend))
  '())

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
