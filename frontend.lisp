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
  resize-after-id
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

(defun font-measure (font text)
  (ltk:format-wish "senddata [font measure {~/ltk::down/} {~a}]"
                   font
                   text)
  (normalize-font-metric (ltk::read-data)))

(defun resolve-cell-dimensions (character-width linespace
                                &key cell-width cell-height)
  (values (or cell-width character-width)
          (or cell-height linespace)))

(defun measure-font-grid (font-name &key cell-width cell-height)
  (let* ((metrics (ltk:font-metrics font-name))
         (character-width (max 1 (font-measure font-name "M")))
         (linespace (max 1 (normalize-font-metric (getf metrics :linespace)))))
    (resolve-cell-dimensions character-width
                             linespace
                             :cell-width cell-width
                             :cell-height cell-height)))

(defun send-to-process (emulator string)
  (let ((stream (terminal-emulator-process-input emulator)))
    (when (and stream string)
      (ignore-errors
        (write-string string stream)
        (finish-output stream)))))

(defun escape-tcl-text (text)
  (unless (stringp text)
    (setf text (format nil "~A" text)))
  (with-output-to-string (out)
    (loop for char across text
          do (when (member char '(#\\ #\$ #\[ #\] #\{ #\} #\"))
               (write-char #\\ out))
             (write-char char out))))

(defun render-text-command (canvas-path handle font-name foreground text)
  (format nil "~A itemconfigure ~A -fill {~A} -font {~A} -text {~A}"
          canvas-path
          handle
          foreground
          font-name
          (escape-tcl-text text)))

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

(defun render-row-command-string (emulator row)
  (with-output-to-string (out)
    (dotimes (column (terminal-emulator-columns emulator))
      (let* ((cell (term-cell-view (terminal-emulator-term emulator) row column))
             (background (aref (terminal-emulator-background-items emulator) row column))
             (text (aref (terminal-emulator-text-items emulator) row column)))
        (format out "~A itemconfigure ~A -fill {~A} -outline {~A}~%"
                (ltk:widget-path (ltk::canvas background))
                (ltk::handle background)
                (cell-view-bg cell)
                (cell-view-bg cell))
        (format out "~A~%"
                (render-text-command (ltk:widget-path (ltk::canvas text))
                                     (ltk::handle text)
                                     (terminal-emulator-font-name emulator)
                                     (cell-view-fg cell)
                                     (cell-view-char cell)))))))

(defun render-emulator (emulator &key force)
  (let ((rows nil))
    (when force
      (mark-all-dirty (terminal-emulator-term emulator)))
    (setf rows (dirty-rows (terminal-emulator-term emulator)))
    (when rows
      (ltk:format-wish "~A"
                       (with-output-to-string (out)
                         (dolist (row rows)
                           (write-string (render-row-command-string emulator row) out))))
      (clear-dirty-rows (terminal-emulator-term emulator) rows))
    emulator))

(defun destroy-emulator (emulator)
  (unless (terminal-emulator-closed-p emulator)
    (setf (terminal-emulator-closed-p emulator) t)
    (when (terminal-emulator-resize-after-id emulator)
      (ignore-errors
        (ltk:after-cancel (terminal-emulator-resize-after-id emulator))))
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

(defun build-grid-items (rows columns cell-width cell-height font-name)
  (let ((specs '()))
    (dotimes (row rows)
      (dotimes (column columns)
        (let ((x (* column cell-width))
              (y (* row cell-height)))
          (push `(:rectangle
                  ,x ,y ,(+ x cell-width) ,(+ y cell-height)
                  :fill "#000000" :outline "#000000")
                specs)
          (push `(:text
                  ,x ,y " "
                  :fill "#E5E5E5" :font ,font-name)
                specs))))
    (nreverse specs)))

(defun rebuild-grid (emulator)
  (let* ((canvas (terminal-emulator-canvas emulator))
         (rows (terminal-emulator-rows emulator))
         (columns (terminal-emulator-columns emulator))
         (cell-width (terminal-emulator-cell-width emulator))
         (cell-height (terminal-emulator-cell-height emulator))
         (background-items (make-array (list rows columns)))
         (text-items (make-array (list rows columns))))
    (ltk:clear canvas)
    (let ((items (ltk:make-items canvas
                                 (build-grid-items rows
                                                   columns
                                                   cell-width
                                                   cell-height
                                                   (terminal-emulator-font-name emulator)))))
    (loop for row below rows do
      (loop for column below columns
            for background = (pop items)
            for text = (pop items)
            do (setf (aref background-items row column) background
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

(defun apply-resize (emulator)
  (setf (terminal-emulator-resize-after-id emulator) nil)
  (let* ((canvas (terminal-emulator-canvas emulator))
         (width (ltk:window-width canvas))
         (height (ltk:window-height canvas)))
    (multiple-value-bind (columns rows)
        (fit-grid-to-size width
                          height
                          (terminal-emulator-cell-width emulator)
                          (terminal-emulator-cell-height emulator))
      (unless (and (= rows (terminal-emulator-rows emulator))
                   (= columns (terminal-emulator-columns emulator)))
        (resize-emulator emulator rows columns)
        (resize-process-pty (terminal-emulator-process emulator)
                            rows
                            columns
                            :pixel-width (* columns
                                            (terminal-emulator-cell-width emulator))
                            :pixel-height (* rows
                                             (terminal-emulator-cell-height emulator)))))))

(defun schedule-resize (emulator)
  (when (terminal-emulator-resize-after-id emulator)
    (ltk:after-cancel (terminal-emulator-resize-after-id emulator)))
  (setf (terminal-emulator-resize-after-id emulator)
        (ltk:after 120
                   (lambda ()
                     (unless (terminal-emulator-closed-p emulator)
                       (apply-resize emulator))))))

(defun install-resize-binding (widget emulator)
  (let ((name (ltk::create-name)))
    (ltk::add-callback name
                       (lambda ()
                         (schedule-resize emulator)))
    (ltk:format-wish
     "bind ~a <Configure> {callback ~A}"
     (ltk:widget-path widget)
     name)
    widget))

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
                 (setf ltk:*exit-mainloop* t))
               (unless (terminal-emulator-closed-p emulator)
                 (ltk:after (terminal-emulator-poll-interval emulator) #'poll)))))
    (ltk:after (terminal-emulator-poll-interval emulator) #'poll)))

(defun launch-terminal (&key
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
  (let ((emulator nil))
    (let ((3bst::*write-to-child-hook*
            (lambda (term string)
              (declare (ignore term))
              (when emulator
                (send-to-process emulator string)))))
      (ltk:with-ltk ()
        (let* ((font-name "slt-terminal-font")
               (resolved-font-family (resolve-font-family
                                      font-family
                                      (ignore-errors (ltk:font-families))))
               (resolved-font-size (terminal-font-size font-size))
               (resolved-cell-width nil)
               (resolved-cell-height nil)
               (term nil)
               (canvas nil)
               (process nil)
               (process-input nil)
               (process-output nil))
          (ltk:font-create font-name
                           :family resolved-font-family
                           :size resolved-font-size)
          (multiple-value-setq (resolved-cell-width resolved-cell-height)
            (measure-font-grid font-name
                               :cell-width cell-width
                               :cell-height cell-height))
          (setf term (make-instance '3bst:term :rows rows :columns columns)
                canvas (ltk:make-canvas ltk:*tk*
                                        :width (* columns resolved-cell-width)
                                        :height (* rows resolved-cell-height)))
          (handler-case
              (multiple-value-setq (process process-input process-output)
                (launch-shell-process shell
                                      :term-name term-name
                                      :rows rows
                                      :columns columns
                                      :pixel-width (* columns resolved-cell-width)
                                      :pixel-height (* rows resolved-cell-height)))
            (error (condition)
              (handle-term-input
               term
               (format nil "Unable to start shell PTY: ~a~%~%Running in local echo mode.~%"
                       condition))))
          (setf (ltk:title ltk:*tk*) title)
          (ltk:resizable ltk:*tk* (tcl-bool t) (tcl-bool t))
          (setf emulator (%make-terminal-emulator
                          :term term
                          :canvas canvas
                          :rows rows
                          :columns columns
                          :cell-width resolved-cell-width
                          :cell-height resolved-cell-height
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
          (install-resize-binding canvas emulator)
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
