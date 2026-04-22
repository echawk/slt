(in-package #:slt)

(defstruct (terminal-emulator
            (:constructor %make-terminal-emulator))
  term
  canvas
  rows
  columns
  cell-width
  cell-height
  font-name
  background-items
  text-items
  process
  process-input
  process-output
  poll-interval
  (closed-p nil))

(defun tcl-bool (value)
  (if value 1 0))

(defun normalize-event-state (state)
  (typecase state
    (integer state)
    (string (or (parse-integer state :junk-allowed t) 0))
    (null 0)
    (t (error "Unsupported event state value: ~S" state))))

(defun default-font-family (&optional (operating-system (uiop:operating-system)))
  (case operating-system
    ((:darwin :macosx) "Menlo")
    (:linux "DejaVu Sans Mono")
    ((:freebsd :openbsd :netbsd) "Monospace")
    (otherwise "Monospace")))

(defun make-script-command (shell &key gnu-script-p)
  (if gnu-script-p
      (list "script"
            "-qefc"
            (format nil "exec ~a -i" (uiop:escape-shell-token shell))
            "/dev/null")
      (list "script" "-q" "/dev/null" shell "-i")))

(defun detect-gnu-script ()
  (handler-case
      (search "util-linux"
              (uiop:run-program '("script" "--version")
                                :ignore-error-status t
                                :output :string))
    (error ()
      nil)))

(defun launch-shell-process (shell)
  (let* ((command (make-script-command shell :gnu-script-p (detect-gnu-script)))
         (process (uiop:launch-program command
                                       :input :stream
                                       :output :stream
                                       :error-output :output
                                       :wait nil)))
    (values process
            (uiop:process-info-input process)
            (uiop:process-info-output process))))

(defun read-available-output (stream)
  (with-output-to-string (buffer)
    (loop for character = (read-char-no-hang stream nil :eof)
          until (or (null character) (eq character :eof))
          do (write-char character buffer))))

(defun send-to-process (emulator string)
  (let ((stream (terminal-emulator-process-input emulator)))
    (when (and stream string)
      (ignore-errors
        (write-string string stream)
        (finish-output stream)))))

(defun write-input (emulator string)
  (cond
    ((or (null string) (zerop (length string)))
     emulator)
    ((terminal-emulator-process emulator)
     (send-to-process emulator string))
    (t
     (3bst:handle-input string :term (terminal-emulator-term emulator))
     (render-emulator emulator :force nil)))
  emulator)

(defun render-row (emulator row)
  (dotimes (column (terminal-emulator-columns emulator))
    (let* ((cell (term-cell-view (terminal-emulator-term emulator) row column))
           (background (aref (terminal-emulator-background-items emulator) row column))
           (text (aref (terminal-emulator-text-items emulator) row column)))
      (ltk:configure background :fill (cell-view-bg cell)
                                :outline (cell-view-bg cell))
      (ltk:configure text :fill (cell-view-fg cell)
                          :font (terminal-emulator-font-name emulator)
                          :text (cell-view-char cell)))))

(defun render-emulator (emulator &key force)
  (when force
    (mark-all-dirty (terminal-emulator-term emulator)))
  (dolist (row (dirty-rows (terminal-emulator-term emulator)))
    (render-row emulator row))
  (clear-dirty-rows (terminal-emulator-term emulator))
  emulator)

(defun destroy-emulator (emulator)
  (unless (terminal-emulator-closed-p emulator)
    (setf (terminal-emulator-closed-p emulator) t)
    (when (terminal-emulator-process emulator)
      (ignore-errors
        (when (uiop:process-alive-p (terminal-emulator-process emulator))
          (uiop:terminate-process (terminal-emulator-process emulator)))))
    (ignore-errors
      (when (terminal-emulator-process-input emulator)
        (close (terminal-emulator-process-input emulator))))
    (ignore-errors
      (when (terminal-emulator-process-output emulator)
        (close (terminal-emulator-process-output emulator)))))
  emulator)

(defun rebuild-grid (emulator)
  (let* ((canvas (terminal-emulator-canvas emulator))
         (rows (terminal-emulator-rows emulator))
         (columns (terminal-emulator-columns emulator))
         (cell-width (terminal-emulator-cell-width emulator))
         (cell-height (terminal-emulator-cell-height emulator))
         (background-items (make-array (list rows columns)))
         (text-items (make-array (list rows columns))))
    (ltk:clear canvas)
    (dotimes (row rows)
      (dotimes (column columns)
        (let* ((x (* column cell-width))
               (y (* row cell-height))
               (background (ltk:make-rectangle canvas x y (+ x cell-width) (+ y cell-height)))
               (text (make-instance 'ltk:canvas-text
                                    :canvas canvas
                                    :x (+ x 1)
                                    :y y
                                    :text " ")))
          (ltk:configure background :fill "#000000" :outline "#000000")
          (ltk:configure text :fill "#E5E5E5"
                              :font (terminal-emulator-font-name emulator))
          (setf (aref background-items row column) background
                (aref text-items row column) text))))
    (setf (terminal-emulator-background-items emulator) background-items
          (terminal-emulator-text-items emulator) text-items)
    (ltk:scrollregion canvas 0 0 (* columns cell-width) (* rows cell-height))
    (ltk:configure canvas :width (* columns cell-width)
                          :height (* rows cell-height))
    emulator))

(defun resize-emulator (emulator rows columns)
  (setf (terminal-emulator-rows emulator) rows
        (terminal-emulator-columns emulator) columns)
  (resize-term (terminal-emulator-term emulator) rows columns)
  (rebuild-grid emulator)
  (render-emulator emulator :force t))

(defun handle-keypress (emulator char keysym state)
  (let ((input (translate-key-event (terminal-emulator-term emulator) char keysym state)))
    (when input
      (write-input emulator input))))

(defun install-key-bindings (widget handler)
  (let ((name (ltk::create-name)))
    (ltk::add-callback name handler)
    (ltk:format-wish
     "bind ~a <KeyPress> {global server; puts $server \"(:callback \\\"~a\\\" \\\"[escape_for_lisp %A]\\\" \\\"[escape_for_lisp %K]\\\" %s)\"; flush $server; break}"
     (ltk:widget-path widget)
     name)
    widget))

(defun schedule-poll (emulator)
  (labels ((poll ()
             (unless (terminal-emulator-closed-p emulator)
               (let ((stream (terminal-emulator-process-output emulator)))
                 (when stream
                   (let ((chunk (read-available-output stream)))
                     (when (plusp (length chunk))
                       (3bst:handle-input chunk :term (terminal-emulator-term emulator))
                       (render-emulator emulator :force nil)))))
               (when (and (terminal-emulator-process emulator)
                          (not (uiop:process-alive-p (terminal-emulator-process emulator))))
                 (let ((final-chunk (read-available-output
                                     (terminal-emulator-process-output emulator))))
                   (when (plusp (length final-chunk))
                     (3bst:handle-input final-chunk :term (terminal-emulator-term emulator))
                     (render-emulator emulator :force nil)))
                 (setf (ltk:title ltk:*tk*)
                       (format nil "~a [exited]" (ltk:title ltk:*tk*))
                       (terminal-emulator-process emulator) nil))
               (unless (terminal-emulator-closed-p emulator)
                 (ltk:after (terminal-emulator-poll-interval emulator) #'poll)))))
    (ltk:after (terminal-emulator-poll-interval emulator) #'poll)))

(defun launch-terminal (&key
                          (rows 24)
                          (columns 80)
                          (cell-width 9)
                          (cell-height 18)
                          (font-family (default-font-family))
                          (font-size 14)
                          (shell (or (uiop:getenv "SHELL") "/bin/sh"))
                          (poll-interval 16)
                          (title "slt"))
  (let ((emulator nil))
    (let ((3bst::*write-to-child-hook*
            (lambda (term string)
              (declare (ignore term))
              (when emulator
                (send-to-process emulator string)))))
      (ltk:with-ltk ()
        (let* ((term (make-instance '3bst:term :rows rows :columns columns))
               (canvas (ltk:make-canvas ltk:*tk*
                                        :width (* columns cell-width)
                                        :height (* rows cell-height)))
               (font-name "slt-terminal-font")
               (process nil)
               (process-input nil)
               (process-output nil))
          (handler-case
              (multiple-value-setq (process process-input process-output)
                (launch-shell-process shell))
            (error (condition)
              (3bst:handle-input
               (format nil "Unable to start shell with script(1): ~a~%~%Running in local echo mode.~%"
                       condition)
               :term term)))
          (setf (ltk:title ltk:*tk*) title)
          (ltk:resizable ltk:*tk* (tcl-bool nil) (tcl-bool nil))
          (ltk:font-create font-name :family font-family :size font-size)
          (setf emulator (%make-terminal-emulator
                          :term term
                          :canvas canvas
                          :rows rows
                          :columns columns
                          :cell-width cell-width
                          :cell-height cell-height
                          :font-name font-name
                          :process process
                          :process-input process-input
                          :process-output process-output
                          :poll-interval poll-interval))
          (setf (3bst::on-set-title term)
                (lambda (terminal new-title)
                  (declare (ignore terminal))
                  (setf (ltk:title ltk:*tk*)
                        (format nil "~a - ~a" title new-title))))
          (ltk:configure canvas :background "#000000"
                                :highlightthickness 0
                                :borderwidth 0)
          (ltk:pack canvas :fill :both :expand t)
          (rebuild-grid emulator)
          (render-emulator emulator :force t)
          (install-key-bindings
           canvas
           (lambda (char keysym state)
             (handle-keypress emulator char keysym
                              (normalize-event-state state))))
          (ltk:on-close ltk:*tk*
                        (lambda ()
                          (destroy-emulator emulator)
                          (setf ltk:*exit-mainloop* t)))
          (ltk:focus canvas)
          (schedule-poll emulator))))))
