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
  text-items
  image-store
  (image-items '())
  (image-photos '())
  ;; The mouse button held down, the last click's time and cell, and the
  ;; clicks counted so far.
  (button nil)
  (last-click nil)
  (click-count 1))

(defvar *ltk-multi-click-seconds* 0.4
  "The longest gap between clicks of a double or triple click.")

(defvar *ltk-scroll-rows* 3
  "Rows scrolled per mouse wheel notch.")

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

(defvar *hex-color-cache* (make-hash-table))

(defun rgb-hex (rgb)
  (or (gethash rgb *hex-color-cache*)
      (setf (gethash rgb *hex-color-cache*) (rgb->hex rgb))))

(defun render-row-command-string (view grid row &optional (palette *palette*))
  (with-output-to-string (out)
    (dotimes (column (grid-columns grid))
      (let* ((index (grid-index grid row column))
             (background (aref (ltk-view-background-items view) row column))
             (text (aref (ltk-view-text-items view) row column)))
        (multiple-value-bind (fg bg) (cell-rgb grid index palette)
          (let ((bg-hex (rgb-hex bg)))
            (format out "~A itemconfigure ~A -fill {~A} -outline {~A}~%"
                    (ltk:widget-path (ltk::canvas background))
                    (ltk::handle background)
                    bg-hex
                    bg-hex))
          (format out "~A~%"
                  (render-text-command (ltk:widget-path (ltk::canvas text))
                                       (ltk::handle text)
                                       (ltk-view-font-name view)
                                       (rgb-hex fg)
                                       (grid-cell-string grid index))))))))

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

(defmethod backend-render-grid ((backend ltk-backend) (view ltk-view) grid)
  (declare (ignore backend))
  (ltk:format-wish "~A"
                   (with-output-to-string (out)
                     (loop for row below (min (grid-rows grid)
                                              (array-dimension
                                               (ltk-view-text-items view) 0))
                           when (= 1 (sbit (grid-dirty grid) row))
                             do (write-string (render-row-command-string view grid row)
                                              out))))
  view)

(defun ltk-clear-images (view)
  (dolist (handle (ltk-view-image-items view))
    (ltk:format-wish "~a delete ~a" (ltk:widget-path (ltk-view-canvas view)) handle))
  (dolist (photo (ltk-view-image-photos view))
    (ltk:format-wish "image delete ~a" (ltk::name photo)))
  (setf (ltk-view-image-items view) '()
        (ltk-view-image-photos view) '()))

(defun ltk-image-pixels (placement image width height)
  (let* ((iw (terminal-image-width image)) (ih (terminal-image-height image))
         (rgba (terminal-image-rgba image))
         (sx (terminal-image-placement-source-x placement))
         (sy (terminal-image-placement-source-y placement))
         (sw (or (terminal-image-placement-source-width placement) (- iw sx)))
         (sh (or (terminal-image-placement-source-height placement) (- ih sy)))
         (background (palette-background *palette*)))
    (loop for dy below height
          collect (loop for dx below width
                        for source-x = (+ sx (min (1- sw) (floor (* dx sw) width)))
                        for source-y = (+ sy (min (1- sh) (floor (* dy sh) height)))
                        for index = (* 4 (+ source-x (* source-y iw)))
                        for alpha = (aref rgba (+ index 3))
                        for inverse = (- 255 alpha)
                        collect (list
                                 (floor (+ (* (aref rgba index) alpha)
                                           (* (ldb (byte 8 16) background) inverse)) 255)
                                 (floor (+ (* (aref rgba (+ index 1)) alpha)
                                           (* (ldb (byte 8 8) background) inverse)) 255)
                                 (floor (+ (* (aref rgba (+ index 2)) alpha)
                                           (* (ldb (byte 8 0) background) inverse)) 255))))))

(defmethod backend-render-images ((backend ltk-backend) (view ltk-view) store)
  (declare (ignore backend))
  (setf (ltk-view-image-store view) store)
  (ltk-clear-images view)
  (dolist (record (terminal-image-render-list
                   store (ltk-view-cell-width view) (ltk-view-cell-height view)))
    (destructuring-bind (placement image x y width height) record
      (let ((photo (make-instance 'ltk:photo-image :width width :height height)))
        (ltk:image-setpixel photo (ltk-image-pixels placement image width height) 0 0)
        (push (ltk:create-image (ltk-view-canvas view) x y :image photo)
              (ltk-view-image-items view))
        (push photo (ltk-view-image-photos view)))))
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

(defun ltk-count-click (view cell)
  (let ((now (get-internal-real-time))
        (last (ltk-view-last-click view)))
    (setf (ltk-view-click-count view)
          (if (and last
                   (equal cell (cdr last))
                   (< (- now (car last))
                      (* *ltk-multi-click-seconds* internal-time-units-per-second)))
              (1+ (ltk-view-click-count view))
              1)
          (ltk-view-last-click view) (cons now cell))))

(defun ltk-mouse-event (view callback action button x y state)
  (let* ((cell (list (floor y (ltk-view-cell-height view))
                     (floor x (ltk-view-cell-width view))))
         (state (logand (normalize-event-state state)
                        (logior +shift-mask+ +control-mask+ +mod1-mask+ +mod4-mask+))))
    (case action
      (:press (setf (ltk-view-button view) button)
       (ltk-count-click view cell))
      (:release (setf (ltk-view-button view) nil))
      (:motion (setf button (ltk-view-button view))
       (when button (setf action :drag))))
    (funcall callback action (first cell) (second cell) (ltk-view-click-count view)
             button state)))

(defmethod backend-bind-mouse ((backend ltk-backend) (view ltk-view) callback)
  (declare (ignore backend))
  (let ((path (ltk:widget-path (ltk-view-canvas view))))
    ;; Motion events have no button (%b is "??").
    (loop for (event action button) in '(("ButtonPress" :press "%b")
                                         ("ButtonRelease" :release "%b")
                                         ("Motion" :motion "0"))
          for name = (ltk::create-name)
          do (let ((action action))
               (ltk::add-callback name
                                  (lambda (button x y state)
                                    (when (or (eq action :motion) (<= 1 button 3))
                                      (ltk-mouse-event view callback action button x y
                                                       state)))))
             (ltk:format-wish "bind ~a <~a> {global server; puts $server \"(:callback \\\"~a\\\" ~a %x %y %s)\"; flush $server}"
                              path event name button))
    view))

(defmethod backend-bind-scroll ((backend ltk-backend) (view ltk-view) callback)
  (declare (ignore backend))
  (let ((path (ltk:widget-path (ltk-view-canvas view)))
        (name (ltk::create-name)))
    (ltk::add-callback name
                       (lambda (direction)
                         ;; Positive is wheel up: back through history.
                         (unless (zerop direction)
                           (funcall callback (if (plusp direction)
                                                 (- *ltk-scroll-rows*)
                                                 *ltk-scroll-rows*)))))
    ;; X11 Tk before 8.7 reports the wheel as buttons 4 and 5.
    (loop for (event direction) in '(("MouseWheel" "%D") ("Button-4" "1") ("Button-5" "-1"))
          do (ltk:format-wish "bind ~a <~a> {global server; puts $server \"(:callback \\\"~a\\\" ~a)\"; flush $server}"
                              path event name direction))
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
