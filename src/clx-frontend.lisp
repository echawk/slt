(in-package #:slt)

;;;; X11 frontend through CLX, the Lisp implementation of the X protocol.
;;;; Glyphs come from slt/glyphs, as in the gl frontend: the same fonts,
;;;; rasterized into the same kind of GLYPH-ATLAS, whose slots are uploaded
;;;; to the server as a RENDER glyph set.  The grid is packed with PACK-GRID,
;;;; so colors, the selection, the cursor and decorations are worked out as
;;;; for the GPU frontends; each row is then drawn in runs with the RENDER
;;;; extension, which every current X server has.
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
  ;; RENDER pictures: the window, and the 1x1 repeating source text and
  ;; fills are drawn with (SOURCE-RGB is its color).
  picture
  source
  (source-rgb nil)
  font
  atlas
  ;; The grid packed by PACK-GRID.
  instances
  ;; The atlas slots uploaded to GLYPH-SET, for the atlas's GENERATION.
  glyph-set
  (glyphs (make-hash-table))
  (atlas-generation -1)
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

;;; Fonts: slt/glyphs, at the scale of the display's Xft.dpi resource.

(defun clx-dpi-scale (display)
  "The scale X clients agree on through the Xft.dpi resource (96 is 1)."
  (let* ((root (xlib:screen-root (xlib:display-default-screen display)))
         (resources (ignore-errors
                     (sb-ext:octets-to-string
                      (coerce (xlib:get-property root :resource_manager
                                                 :result-type '(vector (unsigned-byte 8)))
                              '(vector (unsigned-byte 8)))
                      :external-format :latin-1)))
         (line (and resources
                    (find-if (lambda (line) (eql 0 (search "Xft.dpi:" line)))
                             (uiop:split-string resources :separator '(#\Newline)))))
         (dpi (and line (ignore-errors
                         (let ((*read-eval* nil))
                           (read-from-string (subseq line (length "Xft.dpi:"))))))))
    (if (and (realp dpi) (plusp dpi)) (max 1 (/ dpi 96)) 1)))

;;; Drawing.  Colors are RENDER's 16-bit channels, so no visual is assumed.

(defun clx-render-color (rgb)
  (list (* 257 (ldb (byte 8 16) rgb)) (* 257 (ldb (byte 8 8) rgb))
        (* 257 (ldb (byte 8 0) rgb)) #xFFFF))

(defun clx-pixel (rgb)
  "The core-protocol pixel for #xRRGGBB on a 24-bit TrueColor visual, for
the window's own background only."
  rgb)

(defun clx-fill (view rgb x y width height)
  (when (and (plusp width) (plusp height))
    (xlib:render-fill-rectangle (clx-view-picture view) :src (clx-render-color rgb)
                                x y width height)))

(defun clx-set-source (view rgb)
  (unless (eql rgb (clx-view-source-rgb view))
    (xlib:render-fill-rectangle (clx-view-source view) :src (clx-render-color rgb) 0 0 1 1)
    (setf (clx-view-source-rgb view) rgb)))

(defun clx-ensure-glyphs (view slots)
  "Upload the atlas SLOTS the server does not have yet.  A cleared atlas
reuses slot numbers, so its glyph set is then replaced."
  (let* ((atlas (clx-view-atlas view))
         (glyphs (clx-view-glyphs view))
         (sw (glyph-atlas-slot-width atlas))
         (sh (glyph-atlas-slot-height atlas))
         (per-row (glyph-atlas-slots-per-row atlas))
         (stride (glyph-atlas-width atlas))
         (pixels (glyph-atlas-pixels atlas)))
    (unless (= (clx-view-atlas-generation view) (glyph-atlas-generation atlas))
      (let ((old (clx-view-glyph-set view)))
        (setf (clx-view-glyph-set view)
              (xlib:render-create-glyph-set (xlib::glyph-set-format old)))
        (xlib:render-free-glyph-set old))
      (clrhash glyphs)
      (setf (clx-view-atlas-generation view) (glyph-atlas-generation atlas)))
    (loop for slot across slots
          unless (gethash slot glyphs)
            do (let ((data (make-array (list sh sw) :element-type '(unsigned-byte 8)))
                     (x (* sw (mod slot per-row)))
                     (y (* sh (floor slot per-row))))
                 (dotimes (row sh)
                   (dotimes (column sw)
                     (setf (aref data row column)
                           (aref pixels (+ x column (* stride (+ y row)))))))
                 (xlib:render-add-glyph (clx-view-glyph-set view) slot
                                        :x-origin 0 :y-origin 0 :x-advance sw :y-advance 0
                                        :data data)
                 (setf (gethash slot glyphs) t)))))

(defconstant +clx-underline-flags+
  (logior +gpu-flag-underline+ +gpu-flag-double-underline+ +gpu-flag-curly-underline+
          +gpu-flag-dotted-underline+ +gpu-flag-dashed-underline+))

(defun clx-draw-decorations (view start end word3 rgb y)
  "Underlines, strike-through and bar or underline cursors, as the GPU
shaders draw them."
  (let* ((cw (clx-view-cell-width view))
         (ch (clx-view-cell-height view))
         (x (* start cw))
         (width (* (- end start) cw))
         (flags (ldb (byte 8 0) word3))
         (thickness (decoration-thickness ch)))
    (when (logtest flags +clx-underline-flags+)
      (let ((style (cond ((logtest flags +gpu-flag-double-underline+) :double)
                         ((logtest flags +gpu-flag-curly-underline+) :curly)
                         ((logtest flags +gpu-flag-dotted-underline+) :dotted)
                         ((logtest flags +gpu-flag-dashed-underline+) :dashed)
                         (t :single))))
        (loop for (rx ry rw rh) in (underline-rectangles style x width ch)
              do (clx-fill view (ldb (byte 24 8) word3) rx (+ y ry) rw rh))))
    (when (logtest flags +gpu-flag-strike+)
      (clx-fill view rgb x (+ y (round (- (* ch 0.55) (/ thickness 2)))) width thickness))
    (when (logtest flags +gpu-flag-cursor-bar+)
      (clx-fill view (palette-cursor *palette*) x y (* 2 thickness) ch))
    (when (logtest flags +gpu-flag-cursor-underline+)
      (clx-fill view (palette-cursor *palette*) x (- (+ y ch) (* 2 thickness))
                width (* 2 thickness)))))

(defun clx-draw-row (view row &key (part :all) skip-default-background)
  "Draw packed ROW's backgrounds, text and decorations, or one of them
(PART :BACKGROUNDS or :TEXT).  SKIP-DEFAULT-BACKGROUND leaves cells with
the default background alone, for images under the text."
  (let* ((columns (grid-columns (clx-view-grid view)))
         (cw (clx-view-cell-width view))
         (ch (clx-view-cell-height view))
         (y (* row ch)))
    (multiple-value-bind (backgrounds glyphs decorations)
        (packed-row-runs (clx-view-instances view) columns row)
      (unless (eq part :text)
        (loop for (start end rgb default-p) in backgrounds
              unless (and default-p skip-default-background)
                do (clx-fill view rgb (* start cw) y (* (- end start) cw) ch)))
      (unless (eq part :backgrounds)
        (loop for (start rgb slots) in glyphs
              do (clx-ensure-glyphs view slots)
                 (clx-set-source view rgb)
                 (xlib:render-composite-glyphs (clx-view-picture view) (clx-view-glyph-set view)
                                               (clx-view-source view) (* start cw) y slots))
        (loop for (start end word3 rgb) in decorations
              do (clx-draw-decorations view start end word3 rgb y))))))

(defun clx-pack (view)
  "Pack the dirty rows of the view's grid; packing may clear the atlas and
mark every row dirty."
  (setf (clx-view-instances view)
        (pack-grid (clx-view-grid view) (clx-view-atlas view) *palette*
                   (clx-view-instances view))))

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
        (when grid
          (grid-mark-all-dirty grid)
          (clx-pack view))
        (clx-draw-images view below-backgrounds)
        (when grid
          (dotimes (row (grid-rows grid))
            (clx-draw-row view row :part (if layered :backgrounds :all)
                                   :skip-default-background layered)))
        (clx-draw-images view below-text)
        (when (and grid layered)
          (dotimes (row (grid-rows grid))
            (clx-draw-row view row :part :text))))
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

(defun clx-deliver-mouse (view action cell button state x y)
  "Pass a mouse event at CELL, pixel X, Y, to the view's callback."
  (when (clx-view-mouse-callback view)
    (funcall (clx-view-mouse-callback view)
             action (first cell) (second cell) (clx-view-click-count view)
             button (logand state (logior +shift-mask+ +control-mask+
                                          +mod1-mask+ +mod4-mask+))
             x y)))

(defun clx-button (view action code x y state)
  (case code
    ((1 2 3)
     (let ((cell (clx-pointer-cell view x y)))
       (setf (clx-view-button view) (and (eq action :press) code))
       (when (eq action :press)
         (clx-count-click view cell))
       (clx-deliver-mouse view action cell code state x y)))
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
                                (clx-pointer-cell view x y) button state x y)))
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
  (glyph-font-families))

(defmethod frontend-resolve-font-size ((frontend clx-frontend) font-size)
  (abs (or font-size 15)))

(defun clx-ensure-display (frontend)
  (or (clx-frontend-display frontend)
      (setf (clx-frontend-display frontend) (xlib:open-default-display))))

(defmethod frontend-create-view ((frontend clx-frontend)
                                &key rows columns font-family font-size
                                  cell-width cell-height title)
  (let* ((display (clx-ensure-display frontend))
         (screen (xlib:display-default-screen display))
         (root (xlib:screen-root screen))
         (scale (clx-dpi-scale display))
         (font (open-glyph-font font-family
                                (* (frontend-resolve-font-size frontend font-size) scale))))
    (multiple-value-bind (font-width font-height) (glyph-font-cell-size font)
      (let* ((cw (or (and cell-width (round (* cell-width scale))) font-width))
             (ch (or (and cell-height (round (* cell-height scale))) font-height))
             (width (* columns cw))
             (height (* rows ch))
             (window (xlib:create-window
                      :parent root :x 0 :y 0 :width width :height height
                      :background (clx-pixel (palette-background *palette*))
                      :event-mask (xlib:make-event-mask
                                   :exposure :structure-notify :key-press :key-release
                                   :button-press :button-release :pointer-motion)))
             (gcontext (xlib:create-gcontext :drawable window))
             (source-pixmap (xlib:create-pixmap :width 1 :height 1 :depth 32 :drawable window))
             (view (%make-clx-view
                    :window window :gcontext gcontext
                    :picture (xlib:render-create-picture window)
                    :source (xlib:render-create-picture
                             source-pixmap
                             :format (xlib:find-standard-picture-format display :argb32)
                             :repeat :on)
                    :glyph-set (xlib:render-create-glyph-set
                                (xlib:find-standard-picture-format display :a8))
                    :font font
                    :atlas (make-glyph-atlas cw ch (glyph-font-rasterizer font))
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
            (clx-pack view)
            (dotimes (row (grid-rows grid))
              (when (= 1 (sbit (grid-dirty grid) row))
                (clx-draw-row view row)))))
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
    (ignore-errors (close-glyph-font (clx-view-font view)))
    (setf (clx-frontend-views frontend) (remove view (clx-frontend-views frontend)))
    (xlib:destroy-window (clx-view-window view))
    (xlib:display-force-output (clx-frontend-display frontend))
    (setf (clx-frontend-exit-p frontend) t)))

(register-frontend :clx (lambda () (make-instance 'clx-frontend)))
