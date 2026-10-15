(in-package #:slt)

(defvar *read-buffer-size* (* 64 1024))

(defvar *read-budget-octets* (* 1024 1024)
  "The most child output processed per poll before yielding to render.")

(defvar *load-user-config* t
  "When true, LAUNCH-TERMINAL loads USER-CONFIG-PATHNAME if it exists.")

(defstruct (terminal-emulator
            (:constructor %make-terminal-emulator))
  engine
  (grid nil :type (or null grid))
  backend
  view
  (rows 24 :type fixnum)
  (columns 80 :type fixnum)
  process
  (fd nil :type (or null fixnum))
  (read-buffer (make-array *read-buffer-size* :element-type '(unsigned-byte 8))
   :type octets)
  resize-after-id
  (poll-interval 16 :type fixnum)
  (title "slt" :type string)
  (closed-p nil))

(defun default-backend ()
  "The native backend when one is loaded for this platform, else LTK."
  (if (and (uiop:os-macosx-p) (member :cocoa (available-backends)))
      :cocoa
      :ltk))

(defun user-config-pathname ()
  (uiop:xdg-config-home "slt/init.lisp"))

(defun load-user-config (&optional (pathname (user-config-pathname)))
  (when (and pathname (probe-file pathname))
    (let ((*package* (find-package '#:slt)))
      (load pathname))
    pathname))

(defun send-to-process (emulator string)
  (let ((fd (terminal-emulator-fd emulator)))
    (when (and fd string (plusp (length string)))
      (handler-case (write-octets fd (string->octets string))
        (error () nil)))))

(defun fit-grid-to-size (width height cell-width cell-height)
  (values (max 1 (floor width (max 1 cell-width)))
          (max 1 (floor height (max 1 cell-height)))))

(defun render-emulator (emulator &key force)
  (let ((grid (terminal-emulator-grid emulator)))
    (engine-update-grid (terminal-emulator-engine emulator) grid :force force)
    (when (find 1 (grid-dirty grid))
      (backend-render-grid (terminal-emulator-backend emulator)
                           (terminal-emulator-view emulator)
                           grid)
      (grid-clear-dirty grid))
    emulator))

(defun write-input (emulator string)
  "Send STRING to the child, or echo it locally when there is no child."
  (cond
    ((or (null string) (zerop (length string))))
    ((terminal-emulator-fd emulator)
     (send-to-process emulator string))
    (t
     (engine-feed-string (terminal-emulator-engine emulator) string)
     (render-emulator emulator)))
  emulator)

(defun destroy-emulator (emulator)
  (unless (terminal-emulator-closed-p emulator)
    (setf (terminal-emulator-closed-p emulator) t)
    (ignore-errors (run-hook '*exit-hook* emulator))
    (when (terminal-emulator-resize-after-id emulator)
      (ignore-errors
        (backend-cancel-scheduled (terminal-emulator-backend emulator)
                                  (terminal-emulator-view emulator)
                                  (terminal-emulator-resize-after-id emulator))))
    (let ((process (terminal-emulator-process emulator)))
      (when process
        (ignore-errors
          (when (process-alive-p process)
            (terminate-process process)))
        (ignore-errors
          (close (process-pty-stream process)))))
    (ignore-errors (engine-close (terminal-emulator-engine emulator))))
  emulator)

(defun emulator-cell-pixels (emulator)
  (let ((backend (terminal-emulator-backend emulator))
        (view (terminal-emulator-view emulator)))
    (values (backend-view-cell-width backend view)
            (backend-view-cell-height backend view))))

(defun resize-emulator (emulator rows columns)
  (setf (terminal-emulator-rows emulator) rows
        (terminal-emulator-columns emulator) columns)
  (engine-resize (terminal-emulator-engine emulator) rows columns)
  (backend-resize-view (terminal-emulator-backend emulator)
                       (terminal-emulator-view emulator)
                       rows
                       columns)
  (render-emulator emulator :force t))

(defun apply-resize (emulator)
  (setf (terminal-emulator-resize-after-id emulator) nil)
  (multiple-value-bind (width height)
      (backend-view-window-size (terminal-emulator-backend emulator)
                                (terminal-emulator-view emulator))
    (multiple-value-bind (cell-width cell-height) (emulator-cell-pixels emulator)
      (multiple-value-bind (columns rows)
          (fit-grid-to-size width height cell-width cell-height)
        (unless (and (= rows (terminal-emulator-rows emulator))
                     (= columns (terminal-emulator-columns emulator)))
          (resize-emulator emulator rows columns)
          (resize-process-pty (terminal-emulator-process emulator)
                              rows
                              columns
                              :pixel-width (* columns cell-width)
                              :pixel-height (* rows cell-height)))))))

(defun schedule-resize (emulator)
  (when (terminal-emulator-resize-after-id emulator)
    (backend-cancel-scheduled (terminal-emulator-backend emulator)
                              (terminal-emulator-view emulator)
                              (terminal-emulator-resize-after-id emulator)))
  (setf (terminal-emulator-resize-after-id emulator)
        (backend-schedule (terminal-emulator-backend emulator)
                          (terminal-emulator-view emulator)
                          120
                          (lambda ()
                            (unless (terminal-emulator-closed-p emulator)
                              (apply-resize emulator))))))

(defun handle-keypress (emulator char keysym state)
  (let ((key (event-key char keysym))
        (modifiers (decode-modifier-state state)))
    (when (and key
               (not (run-hook-until-success '*key-hook* emulator key modifiers)))
      (write-input emulator
                   (engine-encode-key (terminal-emulator-engine emulator)
                                      key
                                      modifiers)))))

(defun pump-process-output (emulator)
  "Feed pending child output to the engine.  Return :EOF once the child's
side of the PTY has closed, otherwise the number of octets processed."
  (let ((fd (terminal-emulator-fd emulator))
        (buffer (terminal-emulator-read-buffer emulator))
        (engine (terminal-emulator-engine emulator))
        (total 0))
    (declare (type fixnum total))
    (loop
      (let ((count (read-available-octets fd buffer)))
        (cond
          ((eq count :eof) (return :eof))
          ((zerop count) (return total))
          (t
           (engine-feed engine buffer :end count)
           (incf total count)
           (when (>= total *read-budget-octets*)
             (return total))))))))

(defun finish-emulator (emulator)
  (setf (terminal-emulator-process emulator) nil)
  (destroy-emulator emulator)
  (backend-request-exit (terminal-emulator-backend emulator)
                        (terminal-emulator-view emulator)))

(defun schedule-poll (emulator)
  (labels ((poll ()
             (unless (terminal-emulator-closed-p emulator)
               (let ((result (if (terminal-emulator-fd emulator)
                                 (pump-process-output emulator)
                                 0)))
                 (unless (eql result 0)
                   (render-emulator emulator))
                 (when (or (eq result :eof)
                           (and (terminal-emulator-process emulator)
                                (not (process-alive-p (terminal-emulator-process emulator)))
                                (eql (pump-process-output emulator) :eof)))
                   (render-emulator emulator)
                   (finish-emulator emulator)))
               (unless (terminal-emulator-closed-p emulator)
                 (backend-schedule (terminal-emulator-backend emulator)
                                   (terminal-emulator-view emulator)
                                   (terminal-emulator-poll-interval emulator)
                                   #'poll)))))
    (backend-schedule (terminal-emulator-backend emulator)
                      (terminal-emulator-view emulator)
                      (terminal-emulator-poll-interval emulator)
                      #'poll)))

(defun make-emulator-engine (engine rows columns emulator-cell)
  "Create ENGINE wired to the emulator held in the cons EMULATOR-CELL."
  (make-engine engine
               :rows rows
               :columns columns
               :output-function (lambda (string)
                                  (when (car emulator-cell)
                                    (send-to-process (car emulator-cell) string)))
               :title-function (lambda (title)
                                 (let ((emulator (car emulator-cell)))
                                   (when emulator
                                     (backend-set-title
                                      (terminal-emulator-backend emulator)
                                      (terminal-emulator-view emulator)
                                      (format nil "~a - ~a"
                                              (terminal-emulator-title emulator)
                                              title))
                                     (run-hook '*title-change-hook* emulator title))))
               :bell-function (lambda ()
                                (when (car emulator-cell)
                                  (run-hook '*bell-hook* (car emulator-cell))))))

(defun launch-terminal (&key
                          (backend (or (uiop:getenv "SLT_BACKEND")
                                       (default-backend)))
                          (engine (or (uiop:getenv "SLT_ENGINE")
                                      *default-engine*))
                          (rows 24)
                          (columns 80)
                          cell-width
                          cell-height
                          font-family
                          (font-size 15)
                          (shell (or (uiop:getenv "SHELL") "/bin/sh"))
                          (term-name (or (uiop:getenv "SLT_TERM")
                                         "xterm-256color"))
                          (poll-interval 16)
                          (title "slt")
                          (load-config *load-user-config*))
  (when load-config
    (load-user-config))
  (let* ((backend-instance (make-backend backend))
         (emulator-cell (list nil)))
    (backend-call-with-event-loop
     backend-instance
     (lambda ()
       (let* ((resolved-font-family
                (resolve-font-family
                 font-family
                 (backend-available-font-families backend-instance)))
              (resolved-font-size (backend-resolve-font-size backend-instance
                                                             font-size))
              (engine-instance (make-emulator-engine engine rows columns emulator-cell))
              (view (backend-create-view backend-instance
                                         :rows rows
                                         :columns columns
                                         :font-family resolved-font-family
                                         :font-size resolved-font-size
                                         :cell-width cell-width
                                         :cell-height cell-height
                                         :title title))
              (process nil)
              (emulator nil))
         (handler-case
             (setf process
                   (launch-shell-process shell
                                         :term-name term-name
                                         :rows rows
                                         :columns columns
                                         :pixel-width (* columns
                                                         (backend-view-cell-width
                                                          backend-instance view))
                                         :pixel-height (* rows
                                                          (backend-view-cell-height
                                                           backend-instance view))))
           (error (condition)
             (engine-feed-string
              engine-instance
              (format nil "Unable to start shell PTY: ~a~c~c~c~cRunning in local echo mode.~c~c"
                      condition #\Return #\Linefeed #\Return #\Linefeed
                      #\Return #\Linefeed))))
         (setf emulator (%make-terminal-emulator
                         :engine engine-instance
                         :grid (make-grid rows columns)
                         :backend backend-instance
                         :view view
                         :rows rows
                         :columns columns
                         :process process
                         :fd (and process (set-fd-nonblocking (process-fd process)))
                         :poll-interval poll-interval
                         :title title)
               (car emulator-cell) emulator)
         (render-emulator emulator :force t)
         (backend-bind-resize backend-instance
                              view
                              (lambda ()
                                (schedule-resize emulator)))
         (backend-bind-keypress backend-instance
                                view
                                (lambda (char keysym state)
                                  (handle-keypress emulator char keysym state)))
         (backend-on-close backend-instance
                           view
                           (lambda ()
                             (destroy-emulator emulator)
                             (backend-request-exit backend-instance view)))
         (backend-focus backend-instance view)
         (run-hook '*after-init-hook* emulator)
         (schedule-poll emulator))))))
