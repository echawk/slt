(in-package #:slt)

;;;; X11 backend through CLX, the Lisp implementation of the X protocol, so
;;;; it needs no foreign libraries.  Text is drawn with X core fonts, chosen
;;;; by XLFD pattern and preferably ISO 10646 encoded so that glyph indices
;;;; are code points (the Basic Multilingual Plane only).  Cell backgrounds
;;;; are filled in runs, ASCII text is drawn in runs of one style and other
;;;; characters one cell at a time.  Colors assume a TrueColor visual.
;;;;
;;;; All X requests come from the event loop's thread except wakeups, which
;;;; send a ClientMessage to the window; CLX locks the display around each
;;;; request.

(defvar *clx-scroll-rows* 3
  "Rows scrolled per wheel step.")

(defvar *clx-multi-click-seconds* 0.4
  "The longest gap between clicks of a double or triple click.")

(defclass clx-backend (terminal-backend)
  ((display :initform nil :accessor clx-backend-display)
   (views :initform '() :accessor clx-backend-views)
   (timers :initform '() :accessor clx-backend-timers)
   (wakeups :initform '() :accessor clx-backend-wakeups)
   (lock :initform (sb-thread:make-mutex :name "clx backend")
         :reader clx-backend-lock)
   (exit-p :initform nil :accessor clx-backend-exit-p)))

(defstruct (clx-view (:constructor %make-clx-view))
  window
  gcontext
  ;; Core fonts: regular, bold, italic, bold italic.
  fonts
  ;; Whether the fonts are indexed by code point (ISO 10646).
  unicode-p
  ascent
  cell-width
  cell-height
  (width 0)
  (height 0)
  grid
  keypress-callback
  resize-callback
  scroll-callback
  mouse-callback
  paste-callback
  close-callback
  ;; The text this window offers as the CLIPBOARD selection.
  clipboard
  (button-down-p nil)
  (last-click nil)
  (click-count 1)
  (closed-p nil))

;;; Fonts

(defun clx-font-pattern (family weight slant size)
  (format nil "-*-~a-~a-~a-normal--~d-*-*-*-*-*-iso10646-1"
          (or family "*") weight slant size))

(defun clx-try-open-font (display name)
  "The font NAME, or NIL when the server cannot load it: servers can list
fonts, such as scalable ones without a renderer, that they fail to open."
  (handler-case (let ((font (xlib:open-font display name)))
                  (xlib:font-ascent font)
                  font)
    (xlib:request-error ()
      ;; OpenFont and QueryFont each fail; collect the second error too.
      (handler-case (xlib:display-finish-output display)
        (xlib:request-error () nil))
      nil)))

(defun clx-open-font (display family weight slants size)
  "The first loadable font of FAMILY (any family when NIL), WEIGHT, one of
SLANTS and pixel SIZE."
  (loop for slant in slants
        thereis (loop for name in (xlib:list-font-names
                                   display (clx-font-pattern family weight slant size)
                                   :max-fonts 50)
                      thereis (clx-try-open-font display name))))

(defun clx-open-fonts (display family size)
  "Regular, bold, italic and bold italic fonts, the others falling back to
the regular one, and whether they are ISO 10646 encoded."
  (let* ((family-p (and family (clx-open-font display family "medium" '("r") size)))
         (family (and family-p family))
         (regular (or family-p
                      (clx-open-font display nil "medium" '("r") size)
                      (clx-open-font display nil "*" '("r") size))))
    (if (null regular)
        (let ((fixed (xlib:open-font display "fixed")))
          (values (list fixed fixed fixed fixed) nil))
        (values (cons regular
                      (loop for (weight slants) in '(("bold" ("r"))
                                                     ("medium" ("i" "o"))
                                                     ("bold" ("i" "o")))
                            collect (or (clx-open-font display family weight slants size)
                                        regular)))
                t))))

(defun clx-translate (source start end font destination destination-start)
  ;; Glyph indices for ISO 10646 fonts are code points; CLX's default
  ;; translation stops at the first character above 255.
  (declare (ignore font))
  (loop for i from start below end
        for j from destination-start
        do (let ((code (char-code (char source i))))
             (setf (aref destination j) (if (> code #xFFFF) #xFFFD code))))
  end)

;;; Drawing

(defun clx-pixel (rgb)
  "The pixel for #xRRGGBB on a 24-bit TrueColor visual."
  rgb)

(defun clx-cell-colors (grid index)
  (multiple-value-bind (fg bg) (cell-rgb grid index *palette*)
    (if (grid-selected-p grid index)
        (values fg (palette-selection *palette*))
        (values fg bg))))

(defun clx-font-index (attrs)
  (+ (if (logtest attrs +attr-bold+) 1 0)
     (if (logtest attrs +attr-italic+) 2 0)))

(defun clx-fill (view rgb x y width height)
  (let ((gc (clx-view-gcontext view)))
    (setf (xlib:gcontext-foreground gc) (clx-pixel rgb))
    (xlib:draw-rectangle (clx-view-window view) gc x y width height t)))

(defun clx-draw-text (view text font-index rgb x y)
  (let ((gc (clx-view-gcontext view)))
    (setf (xlib:gcontext-foreground gc) (clx-pixel rgb)
          (xlib:gcontext-font gc) (nth font-index (clx-view-fonts view)))
    (if (clx-view-unicode-p view)
        (xlib:draw-glyphs (clx-view-window view) gc x (+ y (clx-view-ascent view)) text
                          :translate #'clx-translate :size 16)
        (xlib:draw-glyphs (clx-view-window view) gc x (+ y (clx-view-ascent view))
                          (substitute-if #\? (lambda (c) (> (char-code c) 255)) text)))))

(defun clx-draw-lines (view attrs rgb x width y)
  "Underline and strike-through for cells from X, WIDTH pixels wide."
  (let ((ch (clx-view-cell-height view)))
    (when (logtest attrs (logior +attr-underline+ +attr-double-underline+))
      (clx-fill view rgb x (+ y (- ch 2)) width 1))
    (when (logtest attrs +attr-double-underline+)
      (clx-fill view rgb x (+ y (- ch 4)) width 1))
    (when (logtest attrs +attr-strike+)
      (clx-fill view rgb x (+ y (floor ch 2)) width 1))))

(defconstant +clx-style-attrs+
  (logior +attr-bold+ +attr-italic+ +attr-underline+ +attr-strike+
          +attr-double-underline+ +attr-invisible+))

(defun clx-draw-row (view grid row)
  (let* ((columns (grid-columns grid))
         (cw (clx-view-cell-width view))
         (ch (clx-view-cell-height view))
         (y (* row ch))
         (codes (grid-codes grid))
         (attrs (grid-attrs grid)))
    ;; Backgrounds, in runs of one color.
    (let ((column 0))
      (loop while (< column columns)
            do (let* ((bg (nth-value 1 (clx-cell-colors grid (grid-index grid row column))))
                      (end (1+ column)))
                 (loop while (and (< end columns)
                                  (= bg (nth-value 1 (clx-cell-colors
                                                      grid (grid-index grid row end)))))
                       do (incf end))
                 (clx-fill view bg (* column cw) y (* (- end column) cw) ch)
                 (setf column end))))
    ;; Text: runs of ASCII cells with one foreground and style.
    (let ((column 0))
      (loop while (< column columns)
            do (let* ((index (grid-index grid row column))
                      (code (aref codes index))
                      (cell-attrs (aref attrs index))
                      (fg (clx-cell-colors grid index))
                      (style (logand cell-attrs +clx-style-attrs+))
                      ;; Runs neither start nor continue on the spacer
                      ;; after a wide character, which draws nothing.
                      (ascii-p (and (< code 128)
                                    (not (logtest cell-attrs +attr-wide-spacer+))
                                    (not (gethash index (grid-combining grid)))))
                      (end (1+ column)))
                 (when ascii-p
                   (loop while (and (< end columns)
                                    (let ((next (grid-index grid row end)))
                                      (and (< (aref codes next) 128)
                                           (not (gethash next (grid-combining grid)))
                                           (= style (logand (aref attrs next)
                                                            +clx-style-attrs+))
                                           (= fg (clx-cell-colors grid next)))))
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
                     (unless (or (logtest style +attr-invisible+)
                                 (zerop (length (string-trim " " text))))
                       (clx-draw-text view text (clx-font-index style) fg (* column cw) y))
                     (clx-draw-lines view style fg (* column cw) width y)))
                 (setf column end))))))

(defun clx-draw-cursor (view grid)
  "Bar and underline cursors; cell colors already show a block cursor."
  (when (and (grid-cursor-visible-p grid)
             (not (eq (grid-cursor-shape grid) :block)))
    (let ((cw (clx-view-cell-width view))
          (ch (clx-view-cell-height view))
          (x (* (grid-cursor-column grid) (clx-view-cell-width view)))
          (y (* (grid-cursor-row grid) (clx-view-cell-height view))))
      (if (eq (grid-cursor-shape grid) :bar)
          (clx-fill view (palette-cursor *palette*) x y 2 ch)
          (clx-fill view (palette-cursor *palette*) x (- (+ y ch) 2) cw 2)))))

(defun clx-draw-grid (view)
  "Paint the whole window."
  (clx-fill view (palette-background *palette*) 0 0
            (max 1 (clx-view-width view)) (max 1 (clx-view-height view)))
  (let ((grid (clx-view-grid view)))
    (when grid
      (dotimes (row (grid-rows grid))
        (clx-draw-row view grid row))
      (clx-draw-cursor view grid))))

;;; Input

(defparameter *clx-keysym-names*
  '((#xff08 . "BackSpace") (#xff09 . "Tab") (#xff0d . "Return")
    (#xff1b . "Escape") (#xffff . "Delete") (#xff50 . "Home")
    (#xff51 . "Left") (#xff52 . "Up") (#xff53 . "Right") (#xff54 . "Down")
    (#xff55 . "Prior") (#xff56 . "Next") (#xff57 . "End") (#xff63 . "Insert")
    (#xfe20 . "ISO_Left_Tab") (#xff8d . "KP_Enter") (#xff95 . "KP_Home")
    (#xff96 . "KP_Left") (#xff97 . "KP_Up") (#xff98 . "KP_Right")
    (#xff99 . "KP_Down") (#xff9a . "KP_Prior") (#xff9b . "KP_Next")
    (#xff9c . "KP_End") (#xff9e . "KP_Insert") (#xff9f . "KP_Delete")
    (#x0020 . "space"))
  "X11 names of the keysyms the terminal treats specially.")

(defun clx-keysym-name (keysym)
  (cond ((cdr (assoc keysym *clx-keysym-names*)))
        ((<= #xffb0 keysym #xffb9) (format nil "KP_~d" (- keysym #xffb0)))
        ((<= #xffbe keysym #xffe0) (format nil "F~d" (- keysym #xffbd)))))

(defun clx-keysym-character (keysym)
  "The character KEYSYM types, if any: Latin-1 keysyms are their code
points and Unicode keysyms are #x01000000 plus theirs."
  (cond ((or (<= #x20 keysym #x7e) (<= #xa0 keysym #xff)) (code-char keysym))
        ((<= #x01000100 keysym #x0110ffff) (code-char (- keysym #x01000000)))))

(defun clx-key-press (view display code state)
  (let* ((shiftp (logtest state +shift-mask+))
         (keysym (let ((shifted (and shiftp (xlib:keycode->keysym display code 1))))
                   (if (and shifted (plusp shifted))
                       shifted
                       (xlib:keycode->keysym display code 0))))
         (char (clx-keysym-character keysym))
         (char (if (and char (logtest state #x02) (lower-case-p char))
                   (char-upcase char)    ; Caps Lock
                   char))
         (name (or (clx-keysym-name keysym) (and char (string char))))
         (text (if (and char (graphic-char-p char) (char/= char #\Space))
                   (string char)
                   (if (eql char #\Space) " " ""))))
    (when (and name (clx-view-keypress-callback view))
      (funcall (clx-view-keypress-callback view) text name
               (logand state (logior +shift-mask+ +control-mask+
                                     +mod1-mask+ +mod4-mask+))))))

(defun clx-pointer-cell (view x y)
  (list (max 0 (floor y (clx-view-cell-height view)))
        (max 0 (floor x (clx-view-cell-width view)))))

(defun clx-count-click (view cell)
  (let ((now (get-internal-real-time))
        (last (clx-view-last-click view)))
    (setf (clx-view-click-count view)
          (if (and last
                   (equal cell (cdr last))
                   (< (- now (car last))
                      (* *clx-multi-click-seconds* internal-time-units-per-second)))
              (1+ (clx-view-click-count view))
              1)
          (clx-view-last-click view) (cons now cell))))

(defun clx-deliver-mouse (view action cell)
  (when (clx-view-mouse-callback view)
    (funcall (clx-view-mouse-callback view)
             action (first cell) (second cell) (clx-view-click-count view))))

(defun clx-button (view action code x y)
  (case code
    (1 (let ((cell (clx-pointer-cell view x y)))
         (setf (clx-view-button-down-p view) (eq action :press))
         (when (eq action :press)
           (clx-count-click view cell))
         (clx-deliver-mouse view action cell)))
    ((4 5) (when (and (eq action :press) (clx-view-scroll-callback view))
             (funcall (clx-view-scroll-callback view)
                      (if (= code 4) (- *clx-scroll-rows*) *clx-scroll-rows*))))))

;;; Selections

(defun clx-answer-selection-request (view display requestor selection target property time)
  (let* ((text (clx-view-clipboard view))
         (property (or property target))
         (answered
           (when text
             (case target
               (:targets
                (xlib:change-property requestor property
                                      (mapcar (lambda (atom) (xlib:intern-atom display atom))
                                              '(:targets :utf8_string :string))
                                      :atom 32)
                t)
               (:utf8_string
                (xlib:change-property requestor property
                                      (sb-ext:string-to-octets text :external-format :utf-8)
                                      :utf8_string 8)
                t)
               (:string
                (xlib:change-property requestor property
                                      (sb-ext:string-to-octets
                                       (substitute-if #\? (lambda (c) (> (char-code c) 255))
                                                      text)
                                       :external-format :latin-1)
                                      :string 8)
                t)))))
    (xlib:send-event requestor :selection-notify nil
                     :window requestor :event-window requestor
                     :selection selection :target target
                     :property (and answered property) :time time)))

(defun clx-receive-selection (view window property)
  (when property
    (let ((octets (xlib:get-property window property
                                     :result-type '(vector (unsigned-byte 8))
                                     :delete-p t)))
      (when (and octets (plusp (length octets)) (clx-view-paste-callback view))
        (funcall (clx-view-paste-callback view)
                 (sb-ext:octets-to-string (coerce octets '(vector (unsigned-byte 8)))
                                          :external-format :utf-8))))))

;;; Events

(defun clx-find-view (backend window)
  (find window (clx-backend-views backend) :key #'clx-view-window :test #'xlib:window-equal))

(defun clx-handle-event (backend &rest event &key event-key window event-window
                                                  &allow-other-keys)
  (let* ((display (clx-backend-display backend))
         (view (clx-find-view backend (or event-window window))))
    (when view
      (destructuring-bind (&key code state x y width height count type data
                             requestor selection target property time
                           &allow-other-keys)
          event
        (case event-key
          (:exposure
           (when (zerop count)
             (clx-draw-grid view)))
          (:configure-notify
           (unless (and (= width (clx-view-width view)) (= height (clx-view-height view)))
             (setf (clx-view-width view) width
                   (clx-view-height view) height)
             (when (clx-view-resize-callback view)
               (funcall (clx-view-resize-callback view)))))
          (:key-press
           (clx-key-press view display code state))
          (:button-press (clx-button view :press code x y))
          (:button-release (clx-button view :release code x y))
          (:motion-notify
           (when (clx-view-button-down-p view)
             (clx-deliver-mouse view :drag (clx-pointer-cell view x y))))
          (:client-message
           (when (and (eq type :wm_protocols)
                      (eq (xlib:atom-name display (aref data 0)) :wm_delete_window)
                      (clx-view-close-callback view))
             (funcall (clx-view-close-callback view))))
          (:selection-request
           (clx-answer-selection-request view display requestor selection target
                                         property time))
          (:selection-clear
           (setf (clx-view-clipboard view) nil))
          (:selection-notify
           (clx-receive-selection view (clx-view-window view) property))))))
  t)

(defun clx-run-due (backend)
  "Run the wakeups and the timers that are due; return the seconds until
the next timer, or NIL."
  (let ((now (get-internal-real-time))
        (due '())
        (wakeups '()))
    (sb-thread:with-mutex ((clx-backend-lock backend))
      (setf wakeups (reverse (clx-backend-wakeups backend))
            (clx-backend-wakeups backend) '())
      (setf due (remove-if (lambda (timer) (> (car timer) now)) (clx-backend-timers backend))
            (clx-backend-timers backend) (set-difference (clx-backend-timers backend) due)))
    (mapc #'funcall wakeups)
    (dolist (timer (sort due #'< :key #'car))
      (funcall (cdr timer)))
    (let ((times (sb-thread:with-mutex ((clx-backend-lock backend))
                   (mapcar #'car (clx-backend-timers backend)))))
      (when times
        (max 0 (/ (- (reduce #'min times) (get-internal-real-time))
                  internal-time-units-per-second))))))

;;; The backend protocol

(defmethod backend-call-with-event-loop ((backend clx-backend) thunk)
  (funcall thunk)
  (let ((display (clx-backend-display backend)))
    (unwind-protect
         (loop until (or (clx-backend-exit-p backend) (null display))
               do (let ((timeout (clx-run-due backend)))
                    (unless (clx-backend-exit-p backend)
                      (xlib:display-force-output display)
                      (xlib:process-event display
                                          :timeout (if timeout (min timeout 1) 1)
                                          :discard-p t
                                          :handler (lambda (&rest event)
                                                     (apply #'clx-handle-event backend event))))))
      (when display
        (ignore-errors (xlib:close-display display))
        (setf (clx-backend-display backend) nil)))))

(defmethod backend-available-font-families ((backend clx-backend))
  '())

(defmethod backend-resolve-font-size ((backend clx-backend) font-size)
  (abs (or font-size 13)))

(defun clx-ensure-display (backend)
  (or (clx-backend-display backend)
      (setf (clx-backend-display backend) (xlib:open-default-display))))

(defmethod backend-create-view ((backend clx-backend)
                                &key rows columns font-family font-size
                                  cell-width cell-height title)
  (let* ((display (clx-ensure-display backend))
         (screen (xlib:display-default-screen display))
         (root (xlib:screen-root screen)))
    (multiple-value-bind (fonts unicode-p)
        (clx-open-fonts display font-family (backend-resolve-font-size backend font-size))
      (let* ((font (first fonts))
             (cw (or cell-width (max 1 (xlib:max-char-width font))))
             (ch (or cell-height (max 1 (+ (xlib:font-ascent font) (xlib:font-descent font)))))
             (width (* columns cw))
             (height (* rows ch))
             (window (xlib:create-window
                      :parent root :x 0 :y 0 :width width :height height
                      :background (clx-pixel (palette-background *palette*))
                      :event-mask (xlib:make-event-mask
                                   :exposure :structure-notify :key-press
                                   :button-press :button-release :button-1-motion)))
             (gcontext (xlib:create-gcontext :drawable window :font font
                                             :foreground (clx-pixel (palette-foreground *palette*))
                                             :background (clx-pixel (palette-background *palette*))))
             (view (%make-clx-view :window window :gcontext gcontext :fonts fonts
                                   :unicode-p unicode-p :ascent (xlib:font-ascent font)
                                   :cell-width cw :cell-height ch
                                   :width width :height height)))
        (push view (clx-backend-views backend))
        (xlib:set-wm-properties window :name (or title "slt") :icon-name (or title "slt")
                                       :resource-name "slt" :resource-class "Slt"
                                       :width-inc cw :height-inc ch
                                       :base-width 0 :base-height 0)
        (setf (xlib:wm-protocols window) '(:wm_delete_window))
        (xlib:map-window window)
        (xlib:display-force-output display)
        view))))

(defmethod backend-view-cell-width ((backend clx-backend) (view clx-view))
  (clx-view-cell-width view))

(defmethod backend-view-cell-height ((backend clx-backend) (view clx-view))
  (clx-view-cell-height view))

(defmethod backend-view-window-size ((backend clx-backend) (view clx-view))
  (values (clx-view-width view) (clx-view-height view)))

(defmethod backend-resize-view ((backend clx-backend) (view clx-view) rows columns)
  (declare (ignore rows columns))
  view)

(defmethod backend-render-grid ((backend clx-backend) (view clx-view) grid)
  (let ((resized (not (eq grid (clx-view-grid view)))))
    (setf (clx-view-grid view) grid)
    (unless (clx-view-closed-p view)
      (if resized
          (clx-draw-grid view)
          (progn
            (dotimes (row (grid-rows grid))
              (when (= 1 (sbit (grid-dirty grid) row))
                (clx-draw-row view grid row)))
            (clx-draw-cursor view grid)))
      (xlib:display-force-output (clx-backend-display backend))))
  view)

(defmethod backend-set-title ((backend clx-backend) (view clx-view) title)
  (unless (clx-view-closed-p view)
    (let ((window (clx-view-window view)))
      (setf (xlib:wm-name window)
            (substitute-if #\? (lambda (c) (> (char-code c) 255)) title))
      (xlib:change-property window :_net_wm_name
                            (sb-ext:string-to-octets title :external-format :utf-8)
                            :utf8_string 8))))

(defmethod backend-schedule ((backend clx-backend) (view clx-view) delay-ms thunk)
  (let ((timer (cons (+ (get-internal-real-time)
                        (round (* (max 0 delay-ms) internal-time-units-per-second) 1000))
                     thunk)))
    (sb-thread:with-mutex ((clx-backend-lock backend))
      (push timer (clx-backend-timers backend)))
    timer))

(defmethod backend-cancel-scheduled ((backend clx-backend) (view clx-view) token)
  (sb-thread:with-mutex ((clx-backend-lock backend))
    (setf (clx-backend-timers backend) (remove token (clx-backend-timers backend)))))

(defmethod backend-wakeup ((backend clx-backend) view thunk)
  (sb-thread:with-mutex ((clx-backend-lock backend))
    (setf (clx-backend-wakeups backend)
          (cons thunk (clx-backend-wakeups backend))))
  ;; Interrupt the event loop's wait with an event of our own.
  (let ((display (clx-backend-display backend))
        (window (and view (clx-view-window view))))
    (when (and display window (not (clx-view-closed-p view)))
      (ignore-errors
        (xlib:send-event window :client-message nil
                         :window window :type :_slt_wakeup :format 32
                         :data '(0 0 0 0 0))
        (xlib:display-force-output display))))
  t)

(defmethod backend-bind-resize ((backend clx-backend) (view clx-view) callback)
  (setf (clx-view-resize-callback view) callback))

(defmethod backend-bind-keypress ((backend clx-backend) (view clx-view) callback)
  (setf (clx-view-keypress-callback view) callback))

(defmethod backend-bind-scroll ((backend clx-backend) (view clx-view) callback)
  (setf (clx-view-scroll-callback view) callback))

(defmethod backend-bind-mouse ((backend clx-backend) (view clx-view) callback)
  (setf (clx-view-mouse-callback view) callback))

(defmethod backend-bind-paste ((backend clx-backend) (view clx-view) callback)
  (setf (clx-view-paste-callback view) callback))

(defmethod backend-clipboard-text ((backend clx-backend) (view clx-view))
  ;; Our own selection is returned now; another client's arrives through
  ;; the paste callback once it answers.
  (or (clx-view-clipboard view)
      (progn
        (xlib:convert-selection :clipboard :utf8_string (clx-view-window view)
                                :_slt_selection)
        nil)))

(defmethod backend-set-clipboard-text ((backend clx-backend) (view clx-view) text)
  (setf (clx-view-clipboard view) text)
  (setf (xlib:selection-owner (clx-backend-display backend) :clipboard)
        (clx-view-window view)))

(defmethod backend-on-close ((backend clx-backend) (view clx-view) callback)
  (setf (clx-view-close-callback view) callback))

(defmethod backend-focus ((backend clx-backend) (view clx-view))
  (let ((window (clx-view-window view)))
    (xlib:map-window window)
    (setf (xlib:window-priority window) :above)))

(defmethod backend-request-exit ((backend clx-backend) (view clx-view))
  (unless (clx-view-closed-p view)
    (setf (clx-view-closed-p view) t)
    (setf (clx-backend-views backend) (remove view (clx-backend-views backend)))
    (xlib:destroy-window (clx-view-window view))
    (xlib:display-force-output (clx-backend-display backend))
    (setf (clx-backend-exit-p backend) t)))

(register-backend :clx (lambda () (make-instance 'clx-backend)))
