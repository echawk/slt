(in-package #:slt)

;;;; A frontend with no user interface.  It runs the emulator's event loop
;;;; (timers plus a thread-safe inbox), keeps the most recently rendered
;;;; grid, and exposes the inputs a GUI would produce -- keys, paste,
;;;; window resizes, close -- as functions callable from any thread.
;;;;
;;;; It is useful on its own for tests and scripting, and is the base for
;;;; remote frontends: a subclass only has to ship rendered grids somewhere
;;;; (see HEADLESS-PRESENT) and feed input back with the HEADLESS-* calls.

(defclass headless-frontend (terminal-frontend)
  ((view :initform nil :accessor headless-frontend-view)
   (timers :initform '() :accessor headless-frontend-timers)
   (timer-seq :initform 0 :accessor headless-frontend-timer-seq)
   (quit-p :initform nil :accessor headless-frontend-quit-p)
   (inbox :initform '() :accessor headless-frontend-inbox)
   (lock :initform (bt:make-lock "slt headless inbox") :reader headless-frontend-lock)
   (wakeup :initform (bt:make-condition-variable) :reader headless-frontend-wakeup)
   (cell-width :initarg :cell-width :initform 8 :reader headless-frontend-default-cell-width)
   (cell-height :initarg :cell-height :initform 16 :reader headless-frontend-default-cell-height)))

(defstruct (headless-view (:constructor %make-headless-view))
  frontend
  (rows 24 :type fixnum)
  (columns 80 :type fixnum)
  (cell-width 8 :type fixnum)
  (cell-height 16 :type fixnum)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (title "" :type string)
  (mouse-tracking nil)
  (grid nil)
  (image-store nil)
  (render-count 0 :type fixnum)
  resize-callback
  keypress-callback
  scroll-callback
  paste-callback
  mouse-callback
  close-callback
  ;; Stands in for the system clipboard.
  (clipboard nil)
  (closed-p nil))

(defun headless-call (frontend function)
  "Run FUNCTION on FRONTEND's event loop thread.  Safe from any thread."
  (bt:with-lock-held ((headless-frontend-lock frontend))
    (setf (headless-frontend-inbox frontend)
          (append (headless-frontend-inbox frontend) (list function)))
    (bt:condition-notify (headless-frontend-wakeup frontend)))
  function)

(defun take-headless-inbox (frontend timeout)
  "Wait up to TIMEOUT seconds for queued functions and return them all."
  (bt:with-lock-held ((headless-frontend-lock frontend))
    (when (and (null (headless-frontend-inbox frontend)) (plusp timeout))
      (bt:condition-wait (headless-frontend-wakeup frontend)
                         (headless-frontend-lock frontend)
                         :timeout timeout))
    (shiftf (headless-frontend-inbox frontend) '())))

(defun run-headless-timers (frontend)
  (let ((now (get-internal-real-time))
        (due '()))
    (setf (headless-frontend-timers frontend)
          (remove-if (lambda (timer)
                       (when (<= (first timer) now)
                         (push timer due)
                         t))
                     (headless-frontend-timers frontend)))
    (dolist (timer (sort due #'< :key #'first))
      (funcall (third timer)))))

(defun headless-timeout (frontend)
  (let ((timers (headless-frontend-timers frontend)))
    (if timers
        (max 0 (/ (- (reduce #'min timers :key #'first) (get-internal-real-time))
                  internal-time-units-per-second))
        0.25)))

(defmethod frontend-call-with-event-loop ((frontend headless-frontend) thunk)
  (funcall thunk)
  (loop until (headless-frontend-quit-p frontend)
        do (dolist (function (take-headless-inbox frontend
                                                  (min 0.25 (headless-timeout frontend))))
             (funcall function))
           (run-headless-timers frontend)))

(defgeneric headless-present (frontend view grid)
  (:documentation "Called on the loop thread after GRID's dirty rows were
rendered into VIEW; subclasses ship them to their display.  GRID's dirty
bits are still set during the call.")
  (:method ((frontend headless-frontend) view grid)
    (declare (ignore view grid))
    nil))

;;; Inputs, callable from any thread.

(defun headless-key (view char keysym state &optional (event :press))
  "Deliver a key event in the frontend-neutral (CHAR KEYSYM STATE) form;
EVENT is :PRESS, :REPEAT or :RELEASE."
  (headless-call (headless-view-frontend view)
                 (lambda ()
                   (when (headless-view-keypress-callback view)
                     (funcall (headless-view-keypress-callback view) char keysym state
                              event)))))

(defun headless-scroll (view rows)
  "Scroll by ROWS (negative is back through history), as a mouse wheel would."
  (headless-call (headless-view-frontend view)
                 (lambda ()
                   (when (headless-view-scroll-callback view)
                     (funcall (headless-view-scroll-callback view) rows)))))

(defun headless-mouse (view action row column &optional (clicks 1) (button 1) (state 0))
  "Deliver ACTION (:PRESS, :DRAG, :RELEASE or :MOTION) of BUTTON, with the
modifier STATE, at a cell."
  (headless-call (headless-view-frontend view)
                 (lambda ()
                   (when (headless-view-mouse-callback view)
                     (funcall (headless-view-mouse-callback view)
                              action row column clicks button state)))))

(defun headless-paste (view text)
  (headless-call (headless-view-frontend view)
                 (lambda ()
                   (when (headless-view-paste-callback view)
                     (funcall (headless-view-paste-callback view) text)))))

(defun headless-resize-window (view width height)
  "Pretend the window is now WIDTH x HEIGHT pixels."
  (headless-call (headless-view-frontend view)
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
  (headless-call (headless-view-frontend view)
                 (lambda ()
                   (unless (headless-view-closed-p view)
                     (setf (headless-view-closed-p view) t)
                     (when (headless-view-close-callback view)
                       (funcall (headless-view-close-callback view)))
                     (setf (headless-frontend-quit-p (headless-view-frontend view)) t)))))

;;; Frontend protocol

(defmethod frontend-create-view ((frontend headless-frontend)
                                &key rows columns font-family font-size
                                  cell-width cell-height title)
  (declare (ignore font-family font-size))
  (let* ((cw (or cell-width (headless-frontend-default-cell-width frontend)))
         (ch (or cell-height (headless-frontend-default-cell-height frontend)))
         (view (%make-headless-view :frontend frontend
                                    :rows rows :columns columns
                                    :cell-width cw :cell-height ch
                                    :width (* columns cw) :height (* rows ch)
                                    :title (or title ""))))
    (setf (headless-frontend-view frontend) view)))

(defmethod frontend-view-cell-width ((frontend headless-frontend) (view headless-view))
  (headless-view-cell-width view))

(defmethod frontend-view-cell-height ((frontend headless-frontend) (view headless-view))
  (headless-view-cell-height view))

(defmethod frontend-view-window-size ((frontend headless-frontend) (view headless-view))
  (values (headless-view-width view) (headless-view-height view)))

(defmethod frontend-resize-view ((frontend headless-frontend) (view headless-view) rows columns)
  (setf (headless-view-rows view) rows
        (headless-view-columns view) columns)
  view)

(defmethod frontend-render-grid ((frontend headless-frontend) (view headless-view) grid)
  (setf (headless-view-grid view) grid)
  (incf (headless-view-render-count view))
  (headless-present frontend view grid)
  view)

(defmethod frontend-render-images ((frontend headless-frontend) (view headless-view) store)
  (declare (ignore frontend))
  (setf (headless-view-image-store view) store)
  view)

(defmethod frontend-set-title ((frontend headless-frontend) (view headless-view) title)
  (setf (headless-view-title view) title))

(defmethod frontend-set-mouse-tracking ((frontend headless-frontend) (view headless-view) tracking)
  (setf (headless-view-mouse-tracking view) tracking))

(defmethod frontend-schedule ((frontend headless-frontend) (view headless-view) delay-ms thunk)
  (let ((token (incf (headless-frontend-timer-seq frontend))))
    (push (list (+ (get-internal-real-time)
                   (round (* (max 0 delay-ms) internal-time-units-per-second) 1000))
                token
                thunk)
          (headless-frontend-timers frontend))
    token))

(defmethod frontend-cancel-scheduled ((frontend headless-frontend) (view headless-view) token)
  (setf (headless-frontend-timers frontend)
        (remove token (headless-frontend-timers frontend) :key #'second)))

(defmethod frontend-bind-resize ((frontend headless-frontend) (view headless-view) callback)
  (setf (headless-view-resize-callback view) callback))

(defmethod frontend-bind-keypress ((frontend headless-frontend) (view headless-view) callback)
  (setf (headless-view-keypress-callback view) callback))

(defmethod frontend-bind-scroll ((frontend headless-frontend) (view headless-view) callback)
  (setf (headless-view-scroll-callback view) callback))

(defmethod frontend-bind-paste ((frontend headless-frontend) (view headless-view) callback)
  (setf (headless-view-paste-callback view) callback))

(defmethod frontend-bind-mouse ((frontend headless-frontend) (view headless-view) callback)
  (setf (headless-view-mouse-callback view) callback))

(defmethod frontend-clipboard-text ((frontend headless-frontend) (view headless-view))
  (headless-view-clipboard view))

(defmethod frontend-set-clipboard-text ((frontend headless-frontend) (view headless-view) text)
  (setf (headless-view-clipboard view) text))

(defmethod frontend-on-close ((frontend headless-frontend) (view headless-view) callback)
  (setf (headless-view-close-callback view) callback))

(defmethod frontend-wakeup ((frontend headless-frontend) view thunk)
  (declare (ignore view))
  (headless-call frontend thunk)
  t)

(defmethod frontend-focus ((frontend headless-frontend) (view headless-view))
  nil)

(defmethod frontend-request-exit ((frontend headless-frontend) (view headless-view))
  (setf (headless-view-closed-p view) t
        (headless-frontend-quit-p frontend) t))

(register-frontend :headless (lambda () (make-instance 'headless-frontend)))
