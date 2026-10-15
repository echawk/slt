(in-package #:slt)

;;;; A backend with no user interface.  It runs the frontend's event loop
;;;; (timers plus a thread-safe inbox), keeps the most recently rendered
;;;; grid, and exposes the inputs a GUI would produce -- keys, paste,
;;;; window resizes, close -- as functions callable from any thread.
;;;;
;;;; It is useful on its own for tests and scripting, and is the base for
;;;; remote frontends: a subclass only has to ship rendered grids somewhere
;;;; (see HEADLESS-PRESENT) and feed input back with the HEADLESS-* calls.

(defclass headless-backend (terminal-backend)
  ((view :initform nil :accessor headless-backend-view)
   (timers :initform '() :accessor headless-backend-timers)
   (timer-seq :initform 0 :accessor headless-backend-timer-seq)
   (quit-p :initform nil :accessor headless-backend-quit-p)
   (inbox :initform '() :accessor headless-backend-inbox)
   (lock :initform (bt:make-lock "slt headless inbox") :reader headless-backend-lock)
   (wakeup :initform (bt:make-condition-variable) :reader headless-backend-wakeup)
   (cell-width :initarg :cell-width :initform 8 :reader headless-backend-default-cell-width)
   (cell-height :initarg :cell-height :initform 16 :reader headless-backend-default-cell-height)))

(defstruct (headless-view (:constructor %make-headless-view))
  backend
  (rows 24 :type fixnum)
  (columns 80 :type fixnum)
  (cell-width 8 :type fixnum)
  (cell-height 16 :type fixnum)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (title "" :type string)
  (grid nil)
  (render-count 0 :type fixnum)
  resize-callback
  keypress-callback
  paste-callback
  close-callback
  (closed-p nil))

(defun headless-call (backend function)
  "Run FUNCTION on BACKEND's event loop thread.  Safe from any thread."
  (bt:with-lock-held ((headless-backend-lock backend))
    (setf (headless-backend-inbox backend)
          (append (headless-backend-inbox backend) (list function)))
    (bt:condition-notify (headless-backend-wakeup backend)))
  function)

(defun take-headless-inbox (backend timeout)
  "Wait up to TIMEOUT seconds for queued functions and return them all."
  (bt:with-lock-held ((headless-backend-lock backend))
    (when (and (null (headless-backend-inbox backend)) (plusp timeout))
      (bt:condition-wait (headless-backend-wakeup backend)
                         (headless-backend-lock backend)
                         :timeout timeout))
    (shiftf (headless-backend-inbox backend) '())))

(defun run-headless-timers (backend)
  (let ((now (get-internal-real-time))
        (due '()))
    (setf (headless-backend-timers backend)
          (remove-if (lambda (timer)
                       (when (<= (first timer) now)
                         (push timer due)
                         t))
                     (headless-backend-timers backend)))
    (dolist (timer (sort due #'< :key #'first))
      (funcall (third timer)))))

(defun headless-timeout (backend)
  (let ((timers (headless-backend-timers backend)))
    (if timers
        (max 0 (/ (- (reduce #'min timers :key #'first) (get-internal-real-time))
                  internal-time-units-per-second))
        0.25)))

(defmethod backend-call-with-event-loop ((backend headless-backend) thunk)
  (funcall thunk)
  (loop until (headless-backend-quit-p backend)
        do (dolist (function (take-headless-inbox backend
                                                  (min 0.25 (headless-timeout backend))))
             (funcall function))
           (run-headless-timers backend)))

(defgeneric headless-present (backend view grid)
  (:documentation "Called on the loop thread after GRID's dirty rows were
rendered into VIEW; subclasses ship them to their display.  GRID's dirty
bits are still set during the call.")
  (:method ((backend headless-backend) view grid)
    (declare (ignore view grid))
    nil))

;;; Inputs, callable from any thread.

(defun headless-key (view char keysym state)
  "Deliver a key event in the backend-neutral (CHAR KEYSYM STATE) form."
  (headless-call (headless-view-backend view)
                 (lambda ()
                   (when (headless-view-keypress-callback view)
                     (funcall (headless-view-keypress-callback view) char keysym state)))))

(defun headless-paste (view text)
  (headless-call (headless-view-backend view)
                 (lambda ()
                   (when (headless-view-paste-callback view)
                     (funcall (headless-view-paste-callback view) text)))))

(defun headless-resize-window (view width height)
  "Pretend the window is now WIDTH x HEIGHT pixels."
  (headless-call (headless-view-backend view)
                 (lambda ()
                   (setf (headless-view-width view) (max 1 width)
                         (headless-view-height view) (max 1 height))
                   (when (headless-view-resize-callback view)
                     (funcall (headless-view-resize-callback view))))))

(defun headless-resize-cells (view rows columns)
  "Resize to ROWS x COLUMNS cells."
  (headless-resize-window view
                          (* columns (headless-view-cell-width view))
                          (* rows (headless-view-cell-height view))))

(defun headless-close (view)
  (headless-call (headless-view-backend view)
                 (lambda ()
                   (unless (headless-view-closed-p view)
                     (setf (headless-view-closed-p view) t)
                     (when (headless-view-close-callback view)
                       (funcall (headless-view-close-callback view)))
                     (setf (headless-backend-quit-p (headless-view-backend view)) t)))))

;;; Backend protocol

(defmethod backend-create-view ((backend headless-backend)
                                &key rows columns font-family font-size
                                  cell-width cell-height title)
  (declare (ignore font-family font-size))
  (let* ((cw (or cell-width (headless-backend-default-cell-width backend)))
         (ch (or cell-height (headless-backend-default-cell-height backend)))
         (view (%make-headless-view :backend backend
                                    :rows rows :columns columns
                                    :cell-width cw :cell-height ch
                                    :width (* columns cw) :height (* rows ch)
                                    :title (or title ""))))
    (setf (headless-backend-view backend) view)))

(defmethod backend-view-cell-width ((backend headless-backend) (view headless-view))
  (headless-view-cell-width view))

(defmethod backend-view-cell-height ((backend headless-backend) (view headless-view))
  (headless-view-cell-height view))

(defmethod backend-view-window-size ((backend headless-backend) (view headless-view))
  (values (headless-view-width view) (headless-view-height view)))

(defmethod backend-resize-view ((backend headless-backend) (view headless-view) rows columns)
  (setf (headless-view-rows view) rows
        (headless-view-columns view) columns)
  view)

(defmethod backend-render-grid ((backend headless-backend) (view headless-view) grid)
  (setf (headless-view-grid view) grid)
  (incf (headless-view-render-count view))
  (headless-present backend view grid)
  view)

(defmethod backend-set-title ((backend headless-backend) (view headless-view) title)
  (setf (headless-view-title view) title))

(defmethod backend-schedule ((backend headless-backend) (view headless-view) delay-ms thunk)
  (let ((token (incf (headless-backend-timer-seq backend))))
    (push (list (+ (get-internal-real-time)
                   (round (* (max 0 delay-ms) internal-time-units-per-second) 1000))
                token
                thunk)
          (headless-backend-timers backend))
    token))

(defmethod backend-cancel-scheduled ((backend headless-backend) (view headless-view) token)
  (setf (headless-backend-timers backend)
        (remove token (headless-backend-timers backend) :key #'second)))

(defmethod backend-bind-resize ((backend headless-backend) (view headless-view) callback)
  (setf (headless-view-resize-callback view) callback))

(defmethod backend-bind-keypress ((backend headless-backend) (view headless-view) callback)
  (setf (headless-view-keypress-callback view) callback))

(defmethod backend-bind-paste ((backend headless-backend) (view headless-view) callback)
  (setf (headless-view-paste-callback view) callback))

(defmethod backend-on-close ((backend headless-backend) (view headless-view) callback)
  (setf (headless-view-close-callback view) callback))

(defmethod backend-focus ((backend headless-backend) (view headless-view))
  nil)

(defmethod backend-request-exit ((backend headless-backend) (view headless-view))
  (setf (headless-view-closed-p view) t
        (headless-backend-quit-p backend) t))

(register-backend :headless (lambda () (make-instance 'headless-backend)))
