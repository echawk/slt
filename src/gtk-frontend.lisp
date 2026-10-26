(in-package #:slt)

;;;; GTK 3 frontend, through cl-cffi-gtk: a GtkWindow holding a
;;;; GtkDrawingArea that is painted with Cairo and Pango.  Each draw paints
;;;; the whole grid: GTK clips it to the damaged area and, with HiDPI, hands
;;;; us an already scaled Cairo context, so all sizes here are logical
;;;; pixels.  Text is drawn in runs of ASCII cells sharing a style; other
;;;; characters are drawn one cell at a time, so fallback fonts with other
;;;; advances cannot shift the columns after them.

(defvar *gtk-scroll-rows* 3
  "Rows scrolled per wheel step.")

(defclass gtk-frontend (terminal-frontend) ())

(defstruct (gtk-view (:constructor %make-gtk-view))
  window
  area
  ;; Pango font descriptions: regular, bold, italic, bold italic.
  fonts
  cell-width
  cell-height
  grid
  image-store
  keypress-callback
  drop-callback
  ;; Keyvals down, to tell repeats from presses.
  (keys-held (make-hash-table) :type hash-table)
  resize-callback
  scroll-callback
  mouse-callback
  paste-callback
  close-callback
  (scroll-remainder 0d0)
  ;; The mouse button held down, or NIL.
  (button nil)
  (last-click nil)
  (click-count 1)
  (closed-p nil))

(defun gtk-font-descriptions (family size)
  ;; Sizes in pixels: the widget's Pango context (used to measure) and
  ;; Cairo's (used to draw) may assume different resolutions for points.
  (let ((family (or family (default-font-family))))
    (mapcar (lambda (style)
              (pango:pango-font-description-from-string
               (format nil "~a~@[ ~a~] ~dpx" family style size)))
            '(nil "Bold" "Italic" "Bold Italic"))))

(defun gtk-layout-set-text (layout text)
  ;; PANGO-LAYOUT-SET-TEXT passes the length in characters where Pango
  ;; wants bytes, cutting off multibyte text; -1 means NUL-terminated.
  (pango::%pango-layout-set-text layout text -1))

(defun gtk-measure-cell (widget font)
  "The width and height of a cell for FONT, in logical pixels."
  (let ((layout (gtk:gtk-widget-create-pango-layout widget "")))
    (gtk-layout-set-text layout "M")
    (pango:pango-layout-set-font-description layout font)
    (multiple-value-bind (width height) (pango:pango-layout-get-size layout)
      (values (max 1 (ceiling width pango:+pango-scale+))
              (max 1 (ceiling height pango:+pango-scale+))))))

;;; Drawing

(defun gtk-set-color (cr rgb)
  (cairo:cairo-set-source-rgb cr
                              (/ (ldb (byte 8 16) rgb) 255d0)
                              (/ (ldb (byte 8 8) rgb) 255d0)
                              (/ (ldb (byte 8 0) rgb) 255d0)))

(defun gtk-cell-colors (grid index)
  (multiple-value-bind (fg bg) (cell-rgb grid index *palette*)
    (if (grid-selected-p grid index)
        (values fg (palette-selection *palette*))
        (values fg bg))))

(defun gtk-font-index (attrs)
  (+ (if (logtest attrs +attr-bold+) 1 0)
     (if (logtest attrs +attr-italic+) 2 0)))

(defun gtk-draw-text (view cr layout text font-index x y)
  (pango:pango-layout-set-font-description layout (nth font-index (gtk-view-fonts view)))
  (gtk-layout-set-text layout text)
  (cairo:cairo-move-to cr x y)
  (pango:pango-cairo-show-layout cr layout))

(defun gtk-draw-lines (view cr attrs x width y underline-rgb)
  "Underline and strike-through for cells from X, WIDTH pixels wide, in the
current color but for the underline's UNDERLINE-RGB."
  (let ((ch (gtk-view-cell-height view)))
    (when (logtest attrs +attr-strike+)
      (cairo:cairo-rectangle cr x (+ y (floor ch 2)) width 1)
      (cairo:cairo-fill cr))
    (when (logtest attrs +attr-underlines+)
      (gtk-set-color cr underline-rgb)
      (loop for (rx ry rw rh) in (underline-rectangles (underline-style attrs) x width ch)
            do (cairo:cairo-rectangle cr rx (+ y ry) rw rh))
      (cairo:cairo-fill cr))))

(defconstant +gtk-style-attrs+
  (logior +attr-bold+ +attr-italic+ +attr-underlines+ +attr-strike+ +attr-invisible+))

(defun gtk-draw-row (view cr layout grid row &key (part :all) skip-default-background)
  "Draw ROW's backgrounds, text or both (PART :BACKGROUNDS, :TEXT or :ALL).
SKIP-DEFAULT-BACKGROUND leaves cells with the default background alone, for
images under the text."
  (let* ((columns (grid-columns grid))
         (cw (gtk-view-cell-width view))
         (ch (gtk-view-cell-height view))
         (y (* row ch))
         (codes (grid-codes grid))
         (attrs (grid-attrs grid)))
    ;; Backgrounds, in runs of one color.
    (let ((column 0))
      (loop while (and (< column columns) (not (eq part :text)))
            do (let* ((index (grid-index grid row column))
                      (bg (nth-value 1 (gtk-cell-colors grid index)))
                      (end (1+ column)))
                 (loop while (and (< end columns)
                                  (= bg (nth-value 1 (gtk-cell-colors
                                                      grid (grid-index grid row end)))))
                       do (incf end))
                 (unless (and skip-default-background (= bg (palette-background *palette*)))
                   (gtk-set-color cr bg)
                   (cairo:cairo-rectangle cr (* column cw) y (* (- end column) cw) ch)
                   (cairo:cairo-fill cr))
                 (setf column end))))
    ;; Text: runs of ASCII cells with one foreground and style.
    (let ((column (if (eq part :backgrounds) columns 0)))
      (loop while (< column columns)
            do (let* ((index (grid-index grid row column))
                      (code (grid-glyph-code grid index))
                      (cell-attrs (aref attrs index))
                      (fg (gtk-cell-colors grid index))
                      (style (logand cell-attrs +gtk-style-attrs+))
                      (ul (aref (grid-ul grid) index))
                      (ascii-p (and (< code 128)
                                    (not (gethash index (grid-combining grid)))))
                      (end (1+ column)))
                 (when ascii-p
                   (loop while (and (< end columns)
                                    (let ((next (grid-index grid row end)))
                                      (and (< (aref codes next) 128)
                                           (not (gethash next (grid-combining grid)))
                                           (= style (logand (aref attrs next) +gtk-style-attrs+))
                                           (= ul (aref (grid-ul grid) next))
                                           (= fg (gtk-cell-colors grid next)))))
                         do (incf end)))
                 (unless (logtest cell-attrs +attr-wide-spacer+)
                   (let ((text (if ascii-p
                                   (with-output-to-string (out)
                                     (loop for c from column below end
                                           do (write-string
                                               (grid-cell-string grid (grid-index grid row c))
                                               out)))
                                   (grid-cell-string grid index)))
                         (width (* cw (if (logtest cell-attrs +attr-wide+) 2 (- end column)))))
                     (gtk-set-color cr fg)
                     (unless (or (logtest style +attr-invisible+)
                                 (zerop (length (string-trim " " text))))
                       (gtk-draw-text view cr layout text (gtk-font-index style)
                                      (* column cw) y))
                     (gtk-draw-lines view cr style (* column cw) width y
                                     (cell-underline-rgb grid index *palette* fg))))
                 (setf column end))))))

(defun gtk-draw-cursor (view cr grid)
  "Bar and underline cursors; cell colors already show a block cursor."
  (when (and (grid-cursor-visible-p grid)
             (not (eq (grid-cursor-shape grid) :block)))
    (let ((cw (gtk-view-cell-width view))
          (ch (gtk-view-cell-height view))
          (x (* (grid-cursor-column grid) (gtk-view-cell-width view)))
          (y (* (grid-cursor-row grid) (gtk-view-cell-height view))))
      (gtk-set-color cr (palette-cursor *palette*))
      (if (eq (grid-cursor-shape grid) :bar)
          (cairo:cairo-rectangle cr x y 2 ch)
          (cairo:cairo-rectangle cr x (- (+ y ch) 2) cw 2))
      (cairo:cairo-fill cr))))

(defun gtk-draw (view widget context)
  "Paint VIEW with CONTEXT: the draw signal's boxed Cairo context, or a
cairo_t pointer."
  (declare (ignore widget))
  (let ((cr (if (cffi:pointerp context) context (gobject:pointer context))))
    (with-view-palette (view)
      (gtk-draw-grid view cr)))
  t)

(defun gtk-draw-grid (view cr)
  "Paint the whole view: images under the text, if any, between the cell
backgrounds and the text."
  (gtk-set-color cr (palette-background *palette*))
  (cairo:cairo-paint cr)
  (let* ((store (gtk-view-image-store view))
         (records (and store (terminal-image-render-list store (gtk-view-cell-width view)
                                                         (gtk-view-cell-height view))))
         (grid (gtk-view-grid view))
         (layout (and grid (pango:pango-cairo-create-layout cr))))
    (multiple-value-bind (below-backgrounds below-text above) (image-layers records)
      (let ((layered (or below-backgrounds below-text)))
        (gtk-draw-images cr below-backgrounds)
        (when grid
          (dotimes (row (grid-rows grid))
            (gtk-draw-row view cr layout grid row :part (if layered :backgrounds :all)
                                                  :skip-default-background layered)))
        (gtk-draw-images cr below-text)
        (when grid
          (when layered
            (dotimes (row (grid-rows grid))
              (gtk-draw-row view cr layout grid row :part :text)))
          (gtk-draw-cursor view cr grid))
        (gtk-draw-images cr above)))))

(defun rgba-to-cairo-argb32 (rgba)
  ;; Cairo ARGB32 is premultiplied BGRA in memory on little-endian machines.
  (let ((result (make-array (length rgba) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length rgba) by 4
          for alpha = (aref rgba (+ i 3))
          do (setf (aref result i) (round (* (aref rgba (+ i 2)) alpha) 255)
                   (aref result (+ i 1)) (round (* (aref rgba (+ i 1)) alpha) 255)
                   (aref result (+ i 2)) (round (* (aref rgba i) alpha) 255)
                   (aref result (+ i 3)) alpha))
    result))

(defun gtk-draw-images (cr records)
  "Draw image RECORDS (see TERMINAL-IMAGE-RENDER-LIST)."
  (dolist (record records)
    (destructuring-bind (placement image x y width height) record
      (let* ((pixels (rgba-to-cairo-argb32 (terminal-image-rgba image)))
             (iw (terminal-image-width image)) (ih (terminal-image-height image))
             (sx (terminal-image-placement-source-x placement))
             (sy (terminal-image-placement-source-y placement))
             (sw (or (terminal-image-placement-source-width placement) (- iw sx)))
             (sh (or (terminal-image-placement-source-height placement) (- ih sy))))
        (cffi:with-pointer-to-vector-data (pointer pixels)
          (let ((surface (cairo:cairo-image-surface-create-for-data
                          pointer :argb32 iw ih (* iw 4))))
            (unwind-protect
                 (progn
                   (cairo:cairo-save cr)
                   (cairo:cairo-rectangle cr x y width height)
                   (cairo:cairo-clip cr)
                   (cairo:cairo-translate cr x y)
                   (cairo:cairo-scale cr (/ width sw) (/ height sh))
                   (cairo:cairo-set-source-surface cr surface (- sx) (- sy))
                   (cairo:cairo-paint cr)
                   (cairo:cairo-restore cr))
              (cairo:cairo-surface-destroy surface))))))))

;;; Input

(defun gtk-modifier-state (flags)
  "X11-style modifier bits for a list of GdkModifierType flags."
  (logior (if (member :shift-mask flags) +shift-mask+ 0)
          (if (member :control-mask flags) +control-mask+ 0)
          (if (member :mod1-mask flags) +mod1-mask+ 0)
          ;; Command is Mod2 (and Meta) in GTK's macOS frontend.
          (if (or (member :super-mask flags)
                  (and (uiop:os-macosx-p)
                       (or (member :mod2-mask flags) (member :meta-mask flags))))
              +mod4-mask+
              0)))

(defun gtk-key-press (view event &optional (kind :press))
  "Pass a key EVENT on; KIND is :PRESS or :RELEASE.  GTK does not mark
repeats, so a press of a key already down is one."
  (let* ((keyval (gdk:gdk-event-key-keyval event))
         (held (gtk-view-keys-held view))
         (kind (cond ((eq kind :release) (remhash keyval held) :release)
                     ((gethash keyval held) :repeat)
                     (t (setf (gethash keyval held) t) :press)))
         (keysym (gdk:gdk-keyval-name keyval))
         (char (let ((code (gdk:gdk-keyval-to-unicode keyval)))
                 (if (characterp code) code (and (integerp code) (plusp code)
                                                 (code-char code)))))
         (text (if (and char (graphic-char-p char)) (string char) ""))
         (state (gtk-modifier-state (gdk:gdk-event-key-state event))))
    (when (and keysym (gtk-view-keypress-callback view))
      (funcall (gtk-view-keypress-callback view) text keysym state kind))
    ;; Keep Tab and the arrows from moving the focus.
    t))

(defvar *gtk-multi-click-seconds* 0.4
  "The longest gap between clicks of a double or triple click.")

(defun gtk-pointer-cell (view x y)
  (list (floor y (gtk-view-cell-height view))
        (floor x (gtk-view-cell-width view))))

(defun gtk-count-click (view cell)
  (let ((now (get-internal-real-time))
        (last (gtk-view-last-click view)))
    (setf (gtk-view-click-count view)
          (if (and last
                   (equal cell (cdr last))
                   (< (- now (car last))
                      (* *gtk-multi-click-seconds* internal-time-units-per-second)))
              (1+ (gtk-view-click-count view))
              1)
          (gtk-view-last-click view) (cons now cell))))

(defun gtk-deliver-mouse (view action cell button flags x y)
  "Pass a mouse event at CELL, pixel X, Y, to the view's callback."
  (when (gtk-view-mouse-callback view)
    (funcall (gtk-view-mouse-callback view)
             action (first cell) (second cell) (gtk-view-click-count view)
             button (gtk-modifier-state flags) x y)))

(defun gtk-button-event (view event action)
  ;; GTK reports double and triple clicks as extra events; count our own.
  (let ((button (gdk:gdk-event-button-button event)))
    (when (and (<= 1 button 3)
               (member (gdk:gdk-event-button-type event) '(:button-press :button-release)))
      (let ((cell (gtk-pointer-cell view
                                    (gdk:gdk-event-button-x event)
                                    (gdk:gdk-event-button-y event))))
        (setf (gtk-view-button view) (and (eq action :press) button))
        (when (eq action :press)
          (gtk:gtk-widget-grab-focus (gtk-view-area view))
          (gtk-count-click view cell))
        (gtk-deliver-mouse view action cell button (gdk:gdk-event-button-state event)
                           (gdk:gdk-event-button-x event) (gdk:gdk-event-button-y event)))))
  t)

(defun gtk-scroll (view event)
  (let ((callback (gtk-view-scroll-callback view)))
    (when callback
      (let ((rows (case (gdk:gdk-event-scroll-direction event)
                    (:up (- *gtk-scroll-rows*))
                    (:down *gtk-scroll-rows*)
                    (:smooth (* (gdk:gdk-event-scroll-delta-y event) *gtk-scroll-rows*))
                    (t 0))))
        (let* ((rows (+ (gtk-view-scroll-remainder view) rows))
               (whole (truncate rows)))
          (setf (gtk-view-scroll-remainder view) (- rows whole))
          (unless (zerop whole)
            (funcall callback whole))))))
  t)

;;; The frontend protocol

(defmethod frontend-call-with-event-loop ((frontend gtk-frontend) thunk)
  ;; GTK and the platform libraries under it trip SBCL's float traps.
  (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow :inexact)
    (funcall thunk)
    (gtk:gtk-main)))

(defmethod frontend-available-font-families ((frontend gtk-frontend))
  '())

(defmethod frontend-resolve-font-size ((frontend gtk-frontend) font-size)
  (abs (or font-size 13)))

(defmethod frontend-create-view ((frontend gtk-frontend)
                                &key rows columns font-family font-size
                                  cell-width cell-height title)
  (let* ((window (make-instance 'gtk:gtk-window :type :toplevel :title (or title "slt")))
         (area (make-instance 'gtk:gtk-drawing-area :can-focus t))
         (fonts (gtk-font-descriptions font-family font-size))
         (view (%make-gtk-view :window window :area area :fonts fonts)))
    (multiple-value-bind (cw ch) (gtk-measure-cell area (first fonts))
      (setf (gtk-view-cell-width view) (or cell-width cw)
            (gtk-view-cell-height view) (or cell-height ch)))
    (gtk:gtk-widget-add-events area '(:button-press-mask :button-release-mask
                                      :pointer-motion-mask :scroll-mask
                                      :smooth-scroll-mask :key-press-mask
                                      :key-release-mask))
    (gtk:gtk-container-add window area)
    (gtk:gtk-window-resize window
                           (* columns (gtk-view-cell-width view))
                           (* rows (gtk-view-cell-height view)))
    (gobject:g-signal-connect area "draw"
                              (lambda (widget cr) (gtk-draw view widget cr)))
    (gobject:g-signal-connect area "key-press-event"
                              (lambda (widget event)
                                (declare (ignore widget))
                                (gtk-key-press view event)))
    ;; Files dropped on the window arrive as a text/uri-list.
    (gtk:gtk-drag-dest-set area '(:all) nil '(:copy))
    (gtk:gtk-drag-dest-add-uri-targets area)
    (gobject:g-signal-connect area "drag-data-received"
                              (lambda (widget context x y data info time)
                                (declare (ignore widget context x y info time))
                                (let ((paths (uri-list-paths
                                              (gtk:gtk-selection-data-get-uris data))))
                                  (when (and paths (gtk-view-drop-callback view))
                                    (funcall (gtk-view-drop-callback view) paths)))))
    (gobject:g-signal-connect area "key-release-event"
                              (lambda (widget event)
                                (declare (ignore widget))
                                (gtk-key-press view event :release)))
    (gobject:g-signal-connect area "button-press-event"
                              (lambda (widget event)
                                (declare (ignore widget))
                                (gtk-button-event view event :press)))
    (gobject:g-signal-connect area "button-release-event"
                              (lambda (widget event)
                                (declare (ignore widget))
                                (gtk-button-event view event :release)))
    (gobject:g-signal-connect area "motion-notify-event"
                              (lambda (widget event)
                                (declare (ignore widget))
                                (let ((button (gtk-view-button view)))
                                  (gtk-deliver-mouse view (if button :drag :motion)
                                                     (gtk-pointer-cell
                                                      view
                                                      (gdk:gdk-event-motion-x event)
                                                      (gdk:gdk-event-motion-y event))
                                                     button
                                                     (gdk:gdk-event-motion-state event)
                                                     (gdk:gdk-event-motion-x event)
                                                     (gdk:gdk-event-motion-y event)))
                                t))
    (gobject:g-signal-connect area "scroll-event"
                              (lambda (widget event)
                                (declare (ignore widget))
                                (gtk-scroll view event)))
    (gobject:g-signal-connect area "size-allocate"
                              (lambda (widget allocation)
                                (declare (ignore widget allocation))
                                (when (gtk-view-resize-callback view)
                                  (funcall (gtk-view-resize-callback view)))))
    (gobject:g-signal-connect window "delete-event"
                              (lambda (widget event)
                                (declare (ignore widget event))
                                (when (gtk-view-close-callback view)
                                  (funcall (gtk-view-close-callback view)))
                                ;; The close callback requests the exit.
                                t))
    (gtk:gtk-widget-show-all window)
    view))

(defmethod frontend-view-cell-width ((frontend gtk-frontend) (view gtk-view))
  (gtk-view-cell-width view))

(defmethod frontend-view-cell-height ((frontend gtk-frontend) (view gtk-view))
  (gtk-view-cell-height view))

(defmethod frontend-view-window-size ((frontend gtk-frontend) (view gtk-view))
  (let ((area (gtk-view-area view)))
    (values (gtk:gtk-widget-get-allocated-width area)
            (gtk:gtk-widget-get-allocated-height area))))

(defmethod frontend-resize-view ((frontend gtk-frontend) (view gtk-view) rows columns)
  (declare (ignore rows columns))
  view)

(defmethod frontend-render-grid ((frontend gtk-frontend) (view gtk-view) grid)
  (setf (gtk-view-grid view) grid)
  (unless (gtk-view-closed-p view)
    (let ((area (gtk-view-area view))
          (cw (gtk-view-cell-width view))
          (ch (gtk-view-cell-height view)))
      (dotimes (row (grid-rows grid))
        (when (= 1 (sbit (grid-dirty grid) row))
          (gtk:gtk-widget-queue-draw-area area 0 (* row ch)
                                          (* (grid-columns grid) cw) ch)))))
  view)

(defmethod frontend-render-images ((frontend gtk-frontend) (view gtk-view) store)
  (declare (ignore frontend))
  (setf (gtk-view-image-store view) store)
  (unless (gtk-view-closed-p view)
    (gtk:gtk-widget-queue-draw (gtk-view-area view)))
  view)

(defmethod frontend-set-title ((frontend gtk-frontend) (view gtk-view) title)
  (unless (gtk-view-closed-p view)
    (setf (gtk:gtk-window-title (gtk-view-window view)) title)))

(defmethod frontend-schedule ((frontend gtk-frontend) (view gtk-view) delay-ms thunk)
  (glib:g-timeout-add (max 0 (round delay-ms))
                      (lambda ()
                        (funcall thunk)
                        ;; Run once.
                        nil)))

(defmethod frontend-cancel-scheduled ((frontend gtk-frontend) (view gtk-view) token)
  (glib:g-source-remove token))

(defmethod frontend-wakeup ((frontend gtk-frontend) view thunk)
  (declare (ignore view))
  ;; g_idle_add may be called from any thread.
  (glib:g-idle-add (lambda () (funcall thunk) nil))
  t)

(defmethod frontend-bind-resize ((frontend gtk-frontend) (view gtk-view) callback)
  (setf (gtk-view-resize-callback view) callback))

(defmethod frontend-bind-drop ((frontend gtk-frontend) (view gtk-view) callback)
  (setf (gtk-view-drop-callback view) callback))

(defmethod frontend-bind-keypress ((frontend gtk-frontend) (view gtk-view) callback)
  (setf (gtk-view-keypress-callback view) callback))

(defmethod frontend-bind-scroll ((frontend gtk-frontend) (view gtk-view) callback)
  (setf (gtk-view-scroll-callback view) callback))

(defmethod frontend-bind-mouse ((frontend gtk-frontend) (view gtk-view) callback)
  (setf (gtk-view-mouse-callback view) callback))

(defmethod frontend-bind-paste ((frontend gtk-frontend) (view gtk-view) callback)
  (setf (gtk-view-paste-callback view) callback))

(defun gtk-clipboard ()
  (gtk:gtk-clipboard-get "CLIPBOARD"))

(defmethod frontend-clipboard-text ((frontend gtk-frontend) (view gtk-view))
  ;; The binding has only the asynchronous request: the text arrives
  ;; through the paste callback, and nothing is returned now.
  (gtk:gtk-clipboard-request-text (gtk-clipboard)
                                  (lambda (clipboard text)
                                    (declare (ignore clipboard))
                                    (when (and text (plusp (length text))
                                               (gtk-view-paste-callback view))
                                      (funcall (gtk-view-paste-callback view) text))))
  nil)

(defmethod frontend-set-clipboard-text ((frontend gtk-frontend) (view gtk-view) text)
  (gtk:gtk-clipboard-set-text (gtk-clipboard) text))

(defmethod frontend-on-close ((frontend gtk-frontend) (view gtk-view) callback)
  (setf (gtk-view-close-callback view) callback))

(defmethod frontend-focus ((frontend gtk-frontend) (view gtk-view))
  (gtk:gtk-window-present (gtk-view-window view))
  (gtk:gtk-widget-grab-focus (gtk-view-area view)))

(defmethod frontend-request-exit ((frontend gtk-frontend) (view gtk-view))
  (unless (gtk-view-closed-p view)
    (setf (gtk-view-closed-p view) t)
    (gtk:gtk-widget-destroy (gtk-view-window view))
    (gtk:gtk-main-quit)))

(register-frontend :gtk (lambda () (make-instance 'gtk-frontend)))
