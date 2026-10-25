(in-package #:slt)

;;;; X11 frontend through CLX, the Lisp implementation of the X protocol, so
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

(defclass clx-frontend (terminal-frontend)
  ((display :initform nil :accessor clx-frontend-display)
   (views :initform '() :accessor clx-frontend-views)
   (timers :initform '() :accessor clx-frontend-timers)
   (wakeups :initform '() :accessor clx-frontend-wakeups)
   (lock :initform (sb-thread:make-mutex :name "clx frontend")
         :reader clx-frontend-lock)
   (exit-p :initform nil :accessor clx-frontend-exit-p)))

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
  image-store
  keypress-callback
  ;; The keycode whose next press is an autorepeat.
  (repeat-code nil)
  drop-callback
  ;; The window dragging files over this one (XDND), as an X window id.
  (drag-source nil)
  resize-callback
  scroll-callback
  mouse-callback
  paste-callback
  close-callback
  ;; The text this window offers as the CLIPBOARD selection.
  clipboard
  ;; The mouse button held down, or NIL.
  (button nil)
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

(defun clx-draw-lines (view attrs rgb x width y &optional (underline-rgb rgb))
  "Underline and strike-through for cells from X, WIDTH pixels wide."
  (let ((ch (clx-view-cell-height view)))
    (loop for (rx ry rw rh) in (underline-rectangles (underline-style attrs) x width ch)
          do (clx-fill view underline-rgb rx (+ y ry) rw rh))
    (when (logtest attrs +attr-strike+)
      (clx-fill view rgb x (+ y (floor ch 2)) width 1))))

(defconstant +clx-style-attrs+
  (logior +attr-bold+ +attr-italic+ +attr-underlines+ +attr-strike+ +attr-invisible+))

(defun clx-draw-row (view grid row &key (part :all) skip-default-background)
  "Draw ROW's backgrounds, text or both (PART :BACKGROUNDS, :TEXT or :ALL).
SKIP-DEFAULT-BACKGROUND leaves cells with the default background alone, for
images under the text."
  (let* ((columns (grid-columns grid))
         (cw (clx-view-cell-width view))
         (ch (clx-view-cell-height view))
         (y (* row ch))
         (codes (grid-codes grid))
         (attrs (grid-attrs grid)))
    ;; Backgrounds, in runs of one color.
    (let ((column 0))
      (loop while (and (< column columns) (not (eq part :text)))
            do (let* ((bg (nth-value 1 (clx-cell-colors grid (grid-index grid row column))))
                      (end (1+ column)))
                 (loop while (and (< end columns)
                                  (= bg (nth-value 1 (clx-cell-colors
                                                      grid (grid-index grid row end)))))
                       do (incf end))
                 (unless (and skip-default-background (= bg (palette-background *palette*)))
                   (clx-fill view bg (* column cw) y (* (- end column) cw) ch))
                 (setf column end))))
    ;; Text: runs of ASCII cells with one foreground and style.
    (let ((column (if (eq part :backgrounds) columns 0)))
      (loop while (< column columns)
            do (let* ((index (grid-index grid row column))
                      (code (grid-glyph-code grid index))
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
                                           (= (aref (grid-ul grid) index)
                                              (aref (grid-ul grid) next))
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
                     (clx-draw-lines view style fg (* column cw) width y
                                     (cell-underline-rgb grid index *palette* fg))))
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

(defun clx-image-records (view)
  (let ((store (clx-view-image-store view)))
    (and store (terminal-image-render-list store (clx-view-cell-width view)
                                           (clx-view-cell-height view)))))

(defun clx-images-under-text-p (view)
  (find-if (lambda (record) (minusp (terminal-image-placement-z-index (first record))))
           (clx-image-records view)))

(defun clx-draw-grid (view)
  "Paint the whole window: images under the text, if any, between the cell
backgrounds and the text."
  (with-view-palette (view)
    (clx-fill view (palette-background *palette*) 0 0
              (max 1 (clx-view-width view)) (max 1 (clx-view-height view)))
    (multiple-value-bind (below-backgrounds below-text above)
        (image-layers (clx-image-records view))
      (let ((grid (clx-view-grid view))
            (layered (or below-backgrounds below-text)))
        (clx-draw-images view below-backgrounds)
        (when grid
          (dotimes (row (grid-rows grid))
            (clx-draw-row view grid row :part (if layered :backgrounds :all)
                                        :skip-default-background layered)))
        (clx-draw-images view below-text)
        (when grid
          (when layered
            (dotimes (row (grid-rows grid))
              (clx-draw-row view grid row :part :text)))
          (clx-draw-cursor view grid)))
      (clx-draw-images view above))))

(defun clx-draw-images (view records)
  "Draw image RECORDS (see TERMINAL-IMAGE-RENDER-LIST)."
  (dolist (record records)
    (destructuring-bind (placement image x y width height) record
      (let* ((iw (terminal-image-width image)) (ih (terminal-image-height image))
             (rgba (terminal-image-rgba image))
             (sx (terminal-image-placement-source-x placement))
             (sy (terminal-image-placement-source-y placement))
             (sw (or (terminal-image-placement-source-width placement) (- iw sx)))
             (sh (or (terminal-image-placement-source-height placement) (- ih sy)))
             (background (palette-background *palette*))
             (pixels (make-array (list height width)
                                 :element-type '(unsigned-byte 32))))
        (dotimes (dy height)
          (dotimes (dx width)
            (let* ((source-x (+ sx (min (1- sw) (floor (* dx sw) width))))
                   (source-y (+ sy (min (1- sh) (floor (* dy sh) height))))
                   (index (* 4 (+ source-x (* source-y iw))))
                   (alpha (aref rgba (+ index 3)))
                   (inverse (- 255 alpha)))
              (setf (aref pixels dy dx)
                    (logior
                     (ash (floor (+ (* (aref rgba index) alpha)
                                    (* (ldb (byte 8 16) background) inverse)) 255) 16)
                     (ash (floor (+ (* (aref rgba (+ index 1)) alpha)
                                    (* (ldb (byte 8 8) background) inverse)) 255) 8)
                     (floor (+ (* (aref rgba (+ index 2)) alpha)
                               (* (ldb (byte 8 0) background) inverse)) 255))))))
        (xlib:put-image
         (clx-view-window view) (clx-view-gcontext view)
         (xlib:create-image :width width :height height :depth 24
                            :bits-per-pixel 32 :data pixels
                            :red-mask #xff0000 :green-mask #x00ff00 :blue-mask #x0000ff)
         :x x :y y :width width :height height)))))

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
    (#x0020 . "space")
    (#xffe1 . "Shift_L") (#xffe2 . "Shift_R") (#xffe3 . "Control_L")
    (#xffe4 . "Control_R") (#xffe5 . "Caps_Lock") (#xffe9 . "Alt_L")
    (#xffea . "Alt_R") (#xffeb . "Super_L") (#xffec . "Super_R") (#xff7f . "Num_Lock"))
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

(defun clx-autorepeat-p (display code time)
  "True when a key release of CODE at TIME is half of an autorepeat: X sends
a release and a press with the same time for each repeat."
  (and (xlib:event-listen display 0)
       (xlib:process-event display :timeout 0 :peek-p t
                                   :handler (lambda (&key event-key ((:code next-code))
                                                       ((:time next-time)) &allow-other-keys)
                                              (and (eq event-key :key-press)
                                                   (eql next-code code)
                                                   (eql next-time time))))))

(defun clx-key-press (view display code state &optional (kind :press))
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
                                     +mod1-mask+ +mod4-mask+))
               kind))))

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

(defun clx-deliver-mouse (view action cell button state)
  (when (clx-view-mouse-callback view)
    (funcall (clx-view-mouse-callback view)
             action (first cell) (second cell) (clx-view-click-count view)
             button (logand state (logior +shift-mask+ +control-mask+
                                          +mod1-mask+ +mod4-mask+)))))

(defun clx-button (view action code x y state)
  (case code
    ((1 2 3)
     (let ((cell (clx-pointer-cell view x y)))
       (setf (clx-view-button view) (and (eq action :press) code))
       (when (eq action :press)
         (clx-count-click view cell))
       (clx-deliver-mouse view action cell code state)))
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

;;; Drag and drop (XDND, version 5: freedesktop.org's Drag-and-Drop
;;; Protocol for the X Window System).  Files arrive as a text/uri-list.

(defun clx-atom-named-p (atom name)
  (and (symbolp atom) (string= (symbol-name atom) name)))

(defun clx-send-xdnd (display source type &rest data)
  "Send the XDND client message TYPE, with DATA, to the window with id SOURCE."
  (let ((window (xlib::lookup-window display source)))
    (xlib:send-event window :client-message nil
                     :window window :type type :format 32
                     :data (append data (make-list (- 5 (length data)) :initial-element 0)))
    (xlib:display-force-output display)))

(defun clx-handle-xdnd (view display type data)
  "Answer the XDND message TYPE.  Return true when it was one."
  (let ((window-id (xlib:window-id (clx-view-window view)))
        (copy (xlib:intern-atom display :|XdndActionCopy|)))
    (cond
      ((clx-atom-named-p type "XdndEnter")
       (setf (clx-view-drag-source view) (aref data 0)))
      ((clx-atom-named-p type "XdndPosition")
       ;; Accept anywhere in the window, asking to be told of every move.
       (clx-send-xdnd display (aref data 0) :|XdndStatus| window-id 3 0 0 copy))
      ((clx-atom-named-p type "XdndLeave")
       (setf (clx-view-drag-source view) nil)
       t)
      ((clx-atom-named-p type "XdndDrop")
       (setf (clx-view-drag-source view) (aref data 0))
       (xlib:convert-selection :|XdndSelection| :|text/uri-list| (clx-view-window view)
                               :_slt_dnd (aref data 2))
       t))))

(defun clx-receive-drop (view display property)
  "The dropped files' text/uri-list arrived in PROPERTY, or NIL if refused."
  (let ((paths (and property
                    (uri-list-paths
                     (sb-ext:octets-to-string
                      (coerce (xlib:get-property (clx-view-window view) property
                                                 :result-type '(vector (unsigned-byte 8))
                                                 :delete-p t)
                              '(vector (unsigned-byte 8)))
                      :external-format :utf-8))))
        (source (shiftf (clx-view-drag-source view) nil)))
    (when source
      (clx-send-xdnd display source :|XdndFinished| (xlib:window-id (clx-view-window view))
                     (if paths 1 0)
                     (if paths (xlib:intern-atom display :|XdndActionCopy|) 0)))
    (when (and paths (clx-view-drop-callback view))
      (funcall (clx-view-drop-callback view) paths))))

;;; Events

(defun clx-find-view (frontend window)
  (find window (clx-frontend-views frontend) :key #'clx-view-window :test #'xlib:window-equal))

(defun clx-handle-event (frontend &rest event &key event-key window event-window
                                                  &allow-other-keys)
  (let* ((display (clx-frontend-display frontend))
         (view (clx-find-view frontend (or event-window window))))
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
           (clx-key-press view display code state
                          (if (eql code (shiftf (clx-view-repeat-code view) nil))
                              :repeat
                              :press)))
          (:key-release
           (if (clx-autorepeat-p display code time)
               (setf (clx-view-repeat-code view) code)
               (clx-key-press view display code state :release)))
          (:button-press (clx-button view :press code x y state))
          (:button-release (clx-button view :release code x y state))
          (:motion-notify
           (let ((button (clx-view-button view)))
             (clx-deliver-mouse view (if button :drag :motion)
                                (clx-pointer-cell view x y) button state)))
          (:client-message
           (if (and (eq type :wm_protocols)
                    (eq (xlib:atom-name display (aref data 0)) :wm_delete_window))
               (when (clx-view-close-callback view)
                 (funcall (clx-view-close-callback view)))
               (clx-handle-xdnd view display type data)))
          (:selection-request
           (clx-answer-selection-request view display requestor selection target
                                         property time))
          (:selection-clear
           (setf (clx-view-clipboard view) nil))
          (:selection-notify
           (if (eq target :|text/uri-list|)
               (clx-receive-drop view display property)
               (clx-receive-selection view (clx-view-window view) property)))))))
  t)

(defun clx-run-due (frontend)
  "Run the wakeups and the timers that are due; return the seconds until
the next timer, or NIL."
  (let ((now (get-internal-real-time))
        (due '())
        (wakeups '()))
    (sb-thread:with-mutex ((clx-frontend-lock frontend))
      (setf wakeups (reverse (clx-frontend-wakeups frontend))
            (clx-frontend-wakeups frontend) '())
      (setf due (remove-if (lambda (timer) (> (car timer) now)) (clx-frontend-timers frontend))
            (clx-frontend-timers frontend) (set-difference (clx-frontend-timers frontend) due)))
    (mapc #'funcall wakeups)
    (dolist (timer (sort due #'< :key #'car))
      (funcall (cdr timer)))
    (let ((times (sb-thread:with-mutex ((clx-frontend-lock frontend))
                   (mapcar #'car (clx-frontend-timers frontend)))))
      (when times
        (max 0 (/ (- (reduce #'min times) (get-internal-real-time))
                  internal-time-units-per-second))))))

;;; The frontend protocol

(defmethod frontend-call-with-event-loop ((frontend clx-frontend) thunk)
  (funcall thunk)
  (let ((display (clx-frontend-display frontend)))
    (unwind-protect
         (loop until (or (clx-frontend-exit-p frontend) (null display))
               do (let ((timeout (clx-run-due frontend)))
                    (unless (clx-frontend-exit-p frontend)
                      (xlib:display-force-output display)
                      (xlib:process-event display
                                          :timeout (if timeout (min timeout 1) 1)
                                          :discard-p t
                                          :handler (lambda (&rest event)
                                                     (apply #'clx-handle-event frontend event))))))
      (when display
        (ignore-errors (xlib:close-display display))
        (setf (clx-frontend-display frontend) nil)))))

(defmethod frontend-available-font-families ((frontend clx-frontend))
  '())

(defmethod frontend-resolve-font-size ((frontend clx-frontend) font-size)
  (abs (or font-size 13)))

(defun clx-ensure-display (frontend)
  (or (clx-frontend-display frontend)
      (setf (clx-frontend-display frontend) (xlib:open-default-display))))

(defmethod frontend-create-view ((frontend clx-frontend)
                                &key rows columns font-family font-size
                                  cell-width cell-height title)
  (let* ((display (clx-ensure-display frontend))
         (screen (xlib:display-default-screen display))
         (root (xlib:screen-root screen)))
    (multiple-value-bind (fonts unicode-p)
        (clx-open-fonts display font-family (frontend-resolve-font-size frontend font-size))
      (let* ((font (first fonts))
             (cw (or cell-width (max 1 (xlib:max-char-width font))))
             (ch (or cell-height (max 1 (+ (xlib:font-ascent font) (xlib:font-descent font)))))
             (width (* columns cw))
             (height (* rows ch))
             (window (xlib:create-window
                      :parent root :x 0 :y 0 :width width :height height
                      :background (clx-pixel (palette-background *palette*))
                      :event-mask (xlib:make-event-mask
                                   :exposure :structure-notify :key-press :key-release
                                   :button-press :button-release :pointer-motion)))
             (gcontext (xlib:create-gcontext :drawable window :font font
                                             :foreground (clx-pixel (palette-foreground *palette*))
                                             :background (clx-pixel (palette-background *palette*))))
             (view (%make-clx-view :window window :gcontext gcontext :fonts fonts
                                   :unicode-p unicode-p :ascent (xlib:font-ascent font)
                                   :cell-width cw :cell-height ch
                                   :width width :height height)))
        (push view (clx-frontend-views frontend))
        (xlib:set-wm-properties window :name (or title "slt") :icon-name (or title "slt")
                                       :resource-name "slt" :resource-class "Slt"
                                       :width-inc cw :height-inc ch
                                       :base-width 0 :base-height 0)
        (setf (xlib:wm-protocols window) '(:wm_delete_window))
        ;; Accept dropped files.
        (xlib:change-property window :|XdndAware| '(5) :atom 32)
        (xlib:map-window window)
        (xlib:display-force-output display)
        view))))

(defmethod frontend-view-cell-width ((frontend clx-frontend) (view clx-view))
  (clx-view-cell-width view))

(defmethod frontend-view-cell-height ((frontend clx-frontend) (view clx-view))
  (clx-view-cell-height view))

(defmethod frontend-view-window-size ((frontend clx-frontend) (view clx-view))
  (values (clx-view-width view) (clx-view-height view)))

(defmethod frontend-resize-view ((frontend clx-frontend) (view clx-view) rows columns)
  (declare (ignore rows columns))
  view)

(defmethod frontend-render-grid ((frontend clx-frontend) (view clx-view) grid)
  (let ((resized (not (eq grid (clx-view-grid view)))))
    (setf (clx-view-grid view) grid)
    (unless (clx-view-closed-p view)
      ;; Images under the text are redrawn with every change.
      (if (or resized (clx-images-under-text-p view))
          (clx-draw-grid view)
          (with-view-palette (view)
            (dotimes (row (grid-rows grid))
              (when (= 1 (sbit (grid-dirty grid) row))
                (clx-draw-row view grid row)))
            (clx-draw-cursor view grid)))
      (xlib:display-force-output (clx-frontend-display frontend))))
  view)

(defmethod frontend-render-images ((frontend clx-frontend) (view clx-view) store)
  (setf (clx-view-image-store view) store)
  (unless (clx-view-closed-p view)
    (clx-draw-grid view)
    (xlib:display-force-output (clx-frontend-display frontend)))
  view)

(defmethod frontend-set-title ((frontend clx-frontend) (view clx-view) title)
  (unless (clx-view-closed-p view)
    (let ((window (clx-view-window view)))
      (setf (xlib:wm-name window)
            (substitute-if #\? (lambda (c) (> (char-code c) 255)) title))
      (xlib:change-property window :_net_wm_name
                            (sb-ext:string-to-octets title :external-format :utf-8)
                            :utf8_string 8))))

(defmethod frontend-schedule ((frontend clx-frontend) (view clx-view) delay-ms thunk)
  (let ((timer (cons (+ (get-internal-real-time)
                        (round (* (max 0 delay-ms) internal-time-units-per-second) 1000))
                     thunk)))
    (sb-thread:with-mutex ((clx-frontend-lock frontend))
      (push timer (clx-frontend-timers frontend)))
    timer))

(defmethod frontend-cancel-scheduled ((frontend clx-frontend) (view clx-view) token)
  (sb-thread:with-mutex ((clx-frontend-lock frontend))
    (setf (clx-frontend-timers frontend) (remove token (clx-frontend-timers frontend)))))

(defmethod frontend-wakeup ((frontend clx-frontend) view thunk)
  (sb-thread:with-mutex ((clx-frontend-lock frontend))
    (setf (clx-frontend-wakeups frontend)
          (cons thunk (clx-frontend-wakeups frontend))))
  ;; Interrupt the event loop's wait with an event of our own.
  (let ((display (clx-frontend-display frontend))
        (window (and view (clx-view-window view))))
    (when (and display window (not (clx-view-closed-p view)))
      (ignore-errors
        (xlib:send-event window :client-message nil
                         :window window :type :_slt_wakeup :format 32
                         :data '(0 0 0 0 0))
        (xlib:display-force-output display))))
  t)

(defmethod frontend-bind-resize ((frontend clx-frontend) (view clx-view) callback)
  (setf (clx-view-resize-callback view) callback))

(defmethod frontend-bind-drop ((frontend clx-frontend) (view clx-view) callback)
  (setf (clx-view-drop-callback view) callback))

(defmethod frontend-bind-keypress ((frontend clx-frontend) (view clx-view) callback)
  (setf (clx-view-keypress-callback view) callback))

(defmethod frontend-bind-scroll ((frontend clx-frontend) (view clx-view) callback)
  (setf (clx-view-scroll-callback view) callback))

(defmethod frontend-bind-mouse ((frontend clx-frontend) (view clx-view) callback)
  (setf (clx-view-mouse-callback view) callback))

(defmethod frontend-bind-paste ((frontend clx-frontend) (view clx-view) callback)
  (setf (clx-view-paste-callback view) callback))

(defmethod frontend-clipboard-text ((frontend clx-frontend) (view clx-view))
  ;; Our own selection is returned now; another client's arrives through
  ;; the paste callback once it answers.
  (or (clx-view-clipboard view)
      (progn
        (xlib:convert-selection :clipboard :utf8_string (clx-view-window view)
                                :_slt_selection)
        nil)))

(defmethod frontend-set-clipboard-text ((frontend clx-frontend) (view clx-view) text)
  (setf (clx-view-clipboard view) text)
  (setf (xlib:selection-owner (clx-frontend-display frontend) :clipboard)
        (clx-view-window view)))

(defmethod frontend-on-close ((frontend clx-frontend) (view clx-view) callback)
  (setf (clx-view-close-callback view) callback))

(defmethod frontend-focus ((frontend clx-frontend) (view clx-view))
  (let ((window (clx-view-window view)))
    (xlib:map-window window)
    (setf (xlib:window-priority window) :above)))

(defmethod frontend-request-exit ((frontend clx-frontend) (view clx-view))
  (unless (clx-view-closed-p view)
    (setf (clx-view-closed-p view) t)
    (setf (clx-frontend-views frontend) (remove view (clx-frontend-views frontend)))
    (xlib:destroy-window (clx-view-window view))
    (xlib:display-force-output (clx-frontend-display frontend))
    (setf (clx-frontend-exit-p frontend) t)))

(register-frontend :clx (lambda () (make-instance 'clx-frontend)))
