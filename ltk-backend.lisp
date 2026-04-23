(in-package #:slt)

(defclass ltk-backend (terminal-backend) ())

(defstruct (ltk-view
            (:constructor %make-ltk-view))
  root
  canvas
  font-name
  cell-width
  cell-height
  background-items
  text-items)

(defun tcl-bool (value)
  (if value 1 0))

(defun normalize-event-state (state)
  (typecase state
    (integer state)
    (string (or (parse-integer state :junk-allowed t) 0))
    (null 0)
    (t (error "Unsupported event state value: ~S" state))))

(defun font-measure (font text)
  (ltk:format-wish "senddata [font measure {~/ltk::down/} {~a}]"
                   font
                   text)
  (normalize-font-metric (ltk::read-data)))

(defun measure-font-grid (font-name &key cell-width cell-height)
  (let* ((metrics (ltk:font-metrics font-name))
         (character-width (max 1 (font-measure font-name "M")))
         (linespace (max 1 (normalize-font-metric (getf metrics :linespace)))))
    (resolve-cell-dimensions character-width
                             linespace
                             :cell-width cell-width
                             :cell-height cell-height)))

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

(defun render-row-command-string (view term row)
  (with-output-to-string (out)
    (dotimes (column (3bst:columns term))
      (let* ((cell (term-cell-view term row column))
             (background (aref (ltk-view-background-items view) row column))
             (text (aref (ltk-view-text-items view) row column)))
        (format out "~A itemconfigure ~A -fill {~A} -outline {~A}~%"
                (ltk:widget-path (ltk::canvas background))
                (ltk::handle background)
                (cell-view-bg cell)
                (cell-view-bg cell))
        (format out "~A~%"
                (render-text-command (ltk:widget-path (ltk::canvas text))
                                     (ltk::handle text)
                                     (ltk-view-font-name view)
                                     (cell-view-fg cell)
                                     (cell-view-char cell)))))))

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

(defun ltk-rebuild-grid (view rows columns)
  (let* ((canvas (ltk-view-canvas view))
         (cell-width (ltk-view-cell-width view))
         (cell-height (ltk-view-cell-height view))
         (background-items (make-array (list rows columns)))
         (text-items (make-array (list rows columns))))
    (ltk:clear canvas)
    (let ((items (ltk:make-items canvas
                                 (build-grid-items rows
                                                   columns
                                                   cell-width
                                                   cell-height
                                                   (ltk-view-font-name view)))))
      (loop for row below rows do
        (loop for column below columns
              for background = (pop items)
              for text = (pop items)
              do (setf (aref background-items row column) background
                       (aref text-items row column) text))))
    (setf (ltk-view-background-items view) background-items
          (ltk-view-text-items view) text-items)
    (ltk:scrollregion canvas 0 0 (* columns cell-width) (* rows cell-height))
    (ltk:configure canvas :width (* columns cell-width)
                          :height (* rows cell-height))
    view))

(defmethod backend-call-with-event-loop ((backend ltk-backend) thunk)
  (declare (ignore backend))
  (ltk:with-ltk ()
    (funcall thunk)))

(defmethod backend-available-font-families ((backend ltk-backend))
  (declare (ignore backend))
  (or (ignore-errors (ltk:font-families))
      '()))

(defmethod backend-create-view ((backend ltk-backend)
                                &key rows columns font-family font-size
                                  cell-width cell-height title)
  (declare (ignore backend))
  (let* ((root ltk:*tk*)
         (font-name "slt-terminal-font")
         (canvas nil)
         (resolved-cell-width nil)
         (resolved-cell-height nil)
         (view nil))
    (ltk:font-create font-name
                     :family font-family
                     :size font-size)
    (multiple-value-setq (resolved-cell-width resolved-cell-height)
      (measure-font-grid font-name
                         :cell-width cell-width
                         :cell-height cell-height))
    (setf canvas (ltk:make-canvas root
                                  :width (* columns resolved-cell-width)
                                  :height (* rows resolved-cell-height)))
    (setf (ltk:title root) title)
    (ltk:resizable root (tcl-bool t) (tcl-bool t))
    (ltk:configure canvas :background "#000000"
                          :highlightthickness 0
                          :borderwidth 0)
    (ltk:pack canvas :fill :both :expand t)
    (setf view (%make-ltk-view :root root
                               :canvas canvas
                               :font-name font-name
                               :cell-width resolved-cell-width
                               :cell-height resolved-cell-height))
    (ltk-rebuild-grid view rows columns)
    view))

(defmethod backend-view-cell-width ((backend ltk-backend) (view ltk-view))
  (declare (ignore backend))
  (ltk-view-cell-width view))

(defmethod backend-view-cell-height ((backend ltk-backend) (view ltk-view))
  (declare (ignore backend))
  (ltk-view-cell-height view))

(defmethod backend-view-window-size ((backend ltk-backend) (view ltk-view))
  (declare (ignore backend))
  (values (ltk:window-width (ltk-view-canvas view))
          (ltk:window-height (ltk-view-canvas view))))

(defmethod backend-resize-view ((backend ltk-backend) (view ltk-view) rows columns)
  (declare (ignore backend))
  (ltk-rebuild-grid view rows columns))

(defmethod backend-render-rows ((backend ltk-backend) (view ltk-view) term rows)
  (declare (ignore backend))
  (when rows
    (ltk:format-wish "~A"
                     (with-output-to-string (out)
                       (dolist (row rows)
                         (write-string (render-row-command-string view term row) out)))))
  view)

(defmethod backend-set-title ((backend ltk-backend) (view ltk-view) title)
  (declare (ignore backend))
  (setf (ltk:title (ltk-view-root view)) title))

(defmethod backend-schedule ((backend ltk-backend) (view ltk-view) delay-ms thunk)
  (declare (ignore backend view))
  (ltk:after delay-ms thunk))

(defmethod backend-cancel-scheduled ((backend ltk-backend) (view ltk-view) token)
  (declare (ignore backend view))
  (ltk:after-cancel token))

(defmethod backend-bind-resize ((backend ltk-backend) (view ltk-view) callback)
  (declare (ignore backend))
  (let ((name (ltk::create-name)))
    (ltk::add-callback name
                       (lambda ()
                         (funcall callback)))
    (ltk:format-wish
     "bind ~a <Configure> {callback ~A}"
     (ltk:widget-path (ltk-view-canvas view))
     name)
    view))

(defmethod backend-bind-keypress ((backend ltk-backend) (view ltk-view) callback)
  (declare (ignore backend))
  (let ((name (ltk::create-name)))
    (ltk::add-callback name
                       (lambda (char keysym state)
                         (funcall callback char keysym
                                  (normalize-event-state state))))
    (ltk:format-wish
     "bind ~a <KeyPress> {global server; puts $server \"(:callback \\\"~a\\\" \\\"[escape_for_lisp %A]\\\" \\\"[escape_for_lisp %K]\\\" %s)\"; flush $server; break}"
     (ltk:widget-path (ltk-view-canvas view))
     name)
    view))

(defmethod backend-on-close ((backend ltk-backend) (view ltk-view) callback)
  (declare (ignore backend))
  (ltk:on-close (ltk-view-root view)
                callback))

(defmethod backend-focus ((backend ltk-backend) (view ltk-view))
  (declare (ignore backend))
  (ltk:focus (ltk-view-canvas view)))

(defmethod backend-request-exit ((backend ltk-backend) (view ltk-view))
  (declare (ignore backend view))
  (setf ltk:*exit-mainloop* t))

(register-backend :ltk (lambda () (make-instance 'ltk-backend)))
