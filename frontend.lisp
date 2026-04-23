(in-package #:slt)

(defstruct (terminal-emulator
            (:constructor %make-terminal-emulator))
  term
  backend
  view
  rows
  columns
  process
  process-input
  process-output
  resize-after-id
  poll-interval
  (closed-p nil))

(defun send-to-process (emulator string)
  (let ((stream (terminal-emulator-process-input emulator)))
    (when (and stream string)
      (ignore-errors
        (write-string string stream)
        (finish-output stream)))))

(defun fit-grid-to-size (width height cell-width cell-height)
  (values (max 1 (floor width (max 1 cell-width)))
          (max 1 (floor height (max 1 cell-height)))))

(defun write-input (emulator string)
  (cond
    ((or (null string) (zerop (length string)))
     emulator)
    ((terminal-emulator-process emulator)
     (send-to-process emulator string))
    (t
     (handle-term-input (terminal-emulator-term emulator) string)
     (render-emulator emulator :force nil)))
  emulator)

(defun render-emulator (emulator &key force)
  (let ((rows nil)
        (term (terminal-emulator-term emulator)))
    (when force
      (mark-all-dirty term))
    (setf rows (dirty-rows term))
    (when rows
      (backend-render-rows (terminal-emulator-backend emulator)
                           (terminal-emulator-view emulator)
                           term
                           rows)
      (clear-dirty-rows term rows))
    emulator))

(defun destroy-emulator (emulator)
  (unless (terminal-emulator-closed-p emulator)
    (setf (terminal-emulator-closed-p emulator) t)
    (when (terminal-emulator-resize-after-id emulator)
      (ignore-errors
        (backend-cancel-scheduled (terminal-emulator-backend emulator)
                                  (terminal-emulator-view emulator)
                                  (terminal-emulator-resize-after-id emulator))))
    (when (terminal-emulator-process emulator)
      (ignore-errors
        (when (process-alive-p (terminal-emulator-process emulator))
          (terminate-process (terminal-emulator-process emulator)))))
    (ignore-errors
      (when (terminal-emulator-process-input emulator)
        (close (terminal-emulator-process-input emulator))))
    (ignore-errors
      (when (terminal-emulator-process-output emulator)
        (close (terminal-emulator-process-output emulator)))))
  emulator)

(defun resize-emulator (emulator rows columns)
  (setf (terminal-emulator-rows emulator) rows
        (terminal-emulator-columns emulator) columns)
  (resize-term (terminal-emulator-term emulator) rows columns)
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
    (multiple-value-bind (columns rows)
        (fit-grid-to-size width
                          height
                          (backend-view-cell-width (terminal-emulator-backend emulator)
                                                   (terminal-emulator-view emulator))
                          (backend-view-cell-height (terminal-emulator-backend emulator)
                                                    (terminal-emulator-view emulator)))
      (unless (and (= rows (terminal-emulator-rows emulator))
                   (= columns (terminal-emulator-columns emulator)))
        (resize-emulator emulator rows columns)
        (resize-process-pty (terminal-emulator-process emulator)
                            rows
                            columns
                            :pixel-width (* columns
                                            (backend-view-cell-width
                                             (terminal-emulator-backend emulator)
                                             (terminal-emulator-view emulator)))
                            :pixel-height (* rows
                                             (backend-view-cell-height
                                              (terminal-emulator-backend emulator)
                                              (terminal-emulator-view emulator))))))))

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
  (let ((input (translate-key-event (terminal-emulator-term emulator) char keysym state)))
    (when input
      (write-input emulator input))))

(defun schedule-poll (emulator)
  (labels ((poll ()
             (unless (terminal-emulator-closed-p emulator)
               (let ((stream (terminal-emulator-process-output emulator)))
                 (when stream
                   (let ((chunk (read-available-output stream)))
                     (when (plusp (length chunk))
                       (handle-term-input (terminal-emulator-term emulator) chunk)
                       (render-emulator emulator :force nil)))))
               (when (and (terminal-emulator-process emulator)
                          (not (process-alive-p (terminal-emulator-process emulator))))
                 (let ((final-chunk (read-available-output
                                     (terminal-emulator-process-output emulator))))
                   (when (plusp (length final-chunk))
                     (handle-term-input (terminal-emulator-term emulator) final-chunk)
                     (render-emulator emulator :force nil)))
                 (setf (terminal-emulator-process emulator) nil)
                 (destroy-emulator emulator)
                 (backend-request-exit (terminal-emulator-backend emulator)
                                       (terminal-emulator-view emulator)))
               (unless (terminal-emulator-closed-p emulator)
                 (backend-schedule (terminal-emulator-backend emulator)
                                   (terminal-emulator-view emulator)
                                   (terminal-emulator-poll-interval emulator)
                                   #'poll)))))
    (backend-schedule (terminal-emulator-backend emulator)
                      (terminal-emulator-view emulator)
                      (terminal-emulator-poll-interval emulator)
                      #'poll)))

(defun launch-terminal (&key
                          (backend (or (uiop:getenv "SLT_BACKEND")
                                       :ltk))
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
                          (title "slt"))
  (let ((emulator nil)
        (backend-instance (make-backend backend)))
    (let ((3bst::*write-to-child-hook*
            (lambda (term string)
              (declare (ignore term))
              (when emulator
                (send-to-process emulator string)))))
      (backend-call-with-event-loop
       backend-instance
       (lambda ()
         (let* ((resolved-font-family
                  (resolve-font-family
                   font-family
                   (backend-available-font-families backend-instance)))
                (resolved-font-size (backend-resolve-font-size backend-instance
                                                               font-size))
                (term (make-instance '3bst:term :rows rows :columns columns))
                (view (backend-create-view backend-instance
                                           :rows rows
                                           :columns columns
                                           :font-family resolved-font-family
                                           :font-size resolved-font-size
                                           :cell-width cell-width
                                           :cell-height cell-height
                                           :title title))
                (process nil)
                (process-input nil)
                (process-output nil))
           (handler-case
               (multiple-value-setq (process process-input process-output)
                 (launch-shell-process shell
                                       :term-name term-name
                                       :rows rows
                                       :columns columns
                                       :pixel-width (* columns
                                                       (backend-view-cell-width
                                                        backend-instance
                                                        view))
                                       :pixel-height (* rows
                                                        (backend-view-cell-height
                                                         backend-instance
                                                         view))))
             (error (condition)
               (handle-term-input
                term
                (format nil "Unable to start shell PTY: ~a~%~%Running in local echo mode.~%"
                        condition))))
           (setf emulator (%make-terminal-emulator
                           :term term
                           :backend backend-instance
                           :view view
                           :rows rows
                           :columns columns
                           :process process
                           :process-input process-input
                           :process-output process-output
                           :poll-interval poll-interval))
           (setf (3bst::on-set-title term)
                 (lambda (terminal new-title)
                   (declare (ignore terminal))
                   (backend-set-title backend-instance
                                      view
                                      (format nil "~a - ~a" title new-title))))
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
           (schedule-poll emulator)))))))
