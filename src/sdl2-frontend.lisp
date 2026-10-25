(in-package #:slt)

(defclass sdl2-frontend (terminal-frontend)
  ((view
    :initform nil
    :accessor sdl2-frontend-view)
   (timer-seq
    :initform 0
    :accessor sdl2-frontend-timer-seq)
   (timers
    :initform (make-hash-table :test #'eql)
    :accessor sdl2-frontend-timers)
   (ttf-initialized-p
    :initform nil
    :accessor sdl2-frontend-ttf-initialized-p)
   (image-initialized-p
    :initform nil
    :accessor sdl2-frontend-image-initialized-p)
   (timer-event-type
    :initform nil
    :accessor sdl2-frontend-timer-event-type)))

(defstruct (glyph-texture
            (:constructor make-glyph-texture
                (&key texture width height)))
  texture
  width
  height)

(defstruct (sdl2-view
            (:constructor %make-sdl2-view))
  window
  renderer
  font
  font-path
  font-size
  font-family
  cell-width
  cell-height
  rows
  columns
  image-store
  (glyph-cache (make-hash-table :test #'eql))
  ;; Persistent render target holding the whole grid; only dirty rows are
  ;; redrawn into it, then it is copied to the window.
  target
  (target-width 0)
  (target-height 0)
  ;; True while images lie under the text: the target then leaves default
  ;; backgrounds transparent and those images are drawn beneath it.
  (layered nil)
  ;; The grid last drawn into the target.
  grid
  resize-callback
  keypress-callback
  drop-callback
  mouse-callback
  scroll-callback
  close-callback
  ;; The mouse button held down, or NIL.
  (button nil)
  (close-dispatched-p nil)
  (exit-requested-p nil))

(defparameter *sdl-key-name-map*
  '(("Backspace" . "BackSpace")
    ("Delete" . "Delete")
    ("Down" . "Down")
    ("End" . "End")
    ("Enter" . "Return")
    ("Escape" . "Escape")
    ("Home" . "Home")
    ("Insert" . "Insert")
    ("Left" . "Left")
    ("Page Down" . "Next")
    ("Page Up" . "Prior")
    ("PageDown" . "Next")
    ("PageUp" . "Prior")
    ("Return" . "Return")
    ("Right" . "Right")
    ("Space" . "space")
    ("Tab" . "Tab")
    ("Up" . "Up")))

(defparameter *sdl-scancode-keysym-map*
  '((:scancode-lshift . "Shift_L") (:scancode-rshift . "Shift_R")
    (:scancode-lctrl . "Control_L") (:scancode-rctrl . "Control_R")
    (:scancode-lalt . "Alt_L") (:scancode-ralt . "Alt_R")
    (:scancode-lgui . "Super_L") (:scancode-rgui . "Super_R")
    (:scancode-capslock . "Caps_Lock") (:scancode-numlockclear . "Num_Lock")
    (:scancode-kp-0 . "KP_0")
    (:scancode-kp-1 . "KP_1")
    (:scancode-kp-2 . "KP_2")
    (:scancode-kp-3 . "KP_3")
    (:scancode-kp-4 . "KP_4")
    (:scancode-kp-5 . "KP_5")
    (:scancode-kp-6 . "KP_6")
    (:scancode-kp-7 . "KP_7")
    (:scancode-kp-8 . "KP_8")
    (:scancode-kp-9 . "KP_9")
    (:scancode-kp-enter . "KP_Enter")
    (:scancode-kp-period . "KP_Delete")
    (:scancode-kp-down . "KP_Down")
    (:scancode-kp-up . "KP_Up")
    (:scancode-kp-left . "KP_Left")
    (:scancode-kp-right . "KP_Right")
    (:scancode-kp-home . "KP_Home")
    (:scancode-kp-end . "KP_End")
    (:scancode-kp-pageup . "KP_Prior")
    (:scancode-kp-pagedown . "KP_Next")
    (:scancode-kp-insert . "KP_Insert")))

(defun sdl-frontend-use-main-thread-p (&optional
                                        (implementation-type
                                          (uiop:implementation-type))
                                        (operating-system
                                          (uiop:operating-system)))
  (and (member operating-system '(:darwin :macosx))
       (search "SBCL" (string-upcase implementation-type))))

(defun positive-font-size (font-size)
  (abs (or font-size 15)))

(defun hex-pair-integer (string start)
  (parse-integer string :start start :end (+ start 2) :radix 16))

(defun hex-color-rgba (color)
  (check-type color string)
  (unless (and (= (length color) 7)
               (char= (aref color 0) #\#))
    (error "Expected #RRGGBB color string, got ~S." color))
  (values (hex-pair-integer color 1)
          (hex-pair-integer color 3)
          (hex-pair-integer color 5)
          255))

(defun normalize-font-token (string)
  (with-output-to-string (out)
    (loop for char across (string-downcase string)
          when (alphanumericp char)
            do (write-char char out))))

(defun font-pathname-p (pathname)
  (and pathname
       (member (pathname-type pathname)
               '("ttf" "otf" "ttc" "dfont")
               :test #'string-equal)))

(defun default-font-search-roots (&optional (operating-system (uiop:operating-system)))
  (labels ((pathname-if-directory (path)
             (let ((pathname (probe-file path)))
               (and pathname
                    (uiop:directory-exists-p pathname)
                    (uiop:ensure-directory-pathname pathname)))))
    (remove nil
            (append
             (case operating-system
               ((:darwin :macosx)
                (list (pathname-if-directory #P"/System/Library/Fonts/")
                      (pathname-if-directory #P"/Library/Fonts/")
                      (pathname-if-directory (merge-pathnames #P"Library/Fonts/"
                                                              (user-homedir-pathname)))))
               (otherwise
                (list (pathname-if-directory #P"/usr/share/fonts/")
                      (pathname-if-directory #P"/usr/local/share/fonts/")
                      (pathname-if-directory (merge-pathnames #P".fonts/"
                                                              (user-homedir-pathname)))
                      (pathname-if-directory (merge-pathnames #P".local/share/fonts/"
                                                              (user-homedir-pathname))))))
             (list (pathname-if-directory #P"/opt/homebrew/share/fonts/")
                   (pathname-if-directory #P"/opt/local/share/fonts/"))))))

(defun collect-font-files (roots)
  (labels ((walk (directory)
             (append (remove-if-not #'font-pathname-p
                                    (ignore-errors
                                      (uiop:directory-files directory)))
                     (mapcan #'walk
                             (ignore-errors
                               (uiop:subdirectories directory))))))
    (remove-duplicates (mapcan #'walk roots) :test #'equal)))

(defun font-path-score (family pathname)
  (let* ((family-token (normalize-font-token family))
         (pathname-token (normalize-font-token (file-namestring pathname))))
    (cond
      ((string= family-token pathname-token) 3)
      ((search family-token pathname-token) 2)
      ((search "mono" pathname-token) 1)
      (t 0))))

(defun find-font-file (family &key (search-roots (default-font-search-roots)))
  (let ((font-files (collect-font-files search-roots)))
    (or (car (sort (remove-if (lambda (pathname)
                                (zerop (font-path-score family pathname)))
                              font-files)
                   #'>
                   :key (lambda (pathname)
                          (font-path-score family pathname))))
        (find-if (lambda (pathname)
                   (search "mono"
                           (normalize-font-token (file-namestring pathname))))
                 font-files)
        (first font-files))))

(defun resolve-sdl-font-path (font-family &key (search-roots (default-font-search-roots)))
  (let ((explicit-path (and font-family
                            (probe-file font-family))))
    (cond
      ((font-pathname-p explicit-path)
       (namestring explicit-path))
      (t
       (let ((pathname (find-font-file (or font-family
                                           (default-font-family))
                                       :search-roots search-roots)))
         (unless pathname
           (error "Unable to locate a usable font file for SDL2 frontend."))
         (namestring pathname))))))

(defun sdl-modifiers->state (modifiers)
  (let ((state 0))
    (when (intersection modifiers '(:lshift :rshift :shift) :test #'eq)
      (setf state (logior state +shift-mask+)))
    (when (intersection modifiers '(:lctrl :rctrl :ctrl) :test #'eq)
      (setf state (logior state +control-mask+)))
    (when (intersection modifiers '(:lalt :ralt :alt) :test #'eq)
      (setf state (logior state +mod1-mask+)))
    (when (intersection modifiers '(:lgui :rgui :gui) :test #'eq)
      (setf state (logior state +mod4-mask+)))
    state))

(defun normalize-sdl-key-name (name &key shiftp)
  (cond
    ((null name) nil)
    ((assoc name *sdl-key-name-map* :test #'string=)
     (cdr (assoc name *sdl-key-name-map* :test #'string=)))
    ((and (= (length name) 1)
          (alpha-char-p (aref name 0)))
     (if shiftp
         name
         (string-downcase name)))
    (t name)))

(defun sdl-keysym-name (keysym)
  (let* ((modifiers (sdl2:mod-keywords (sdl2:mod-value keysym)))
         (shiftp (intersection modifiers '(:lshift :rshift :shift) :test #'eq))
         (scancode-name (sdl2:scancode keysym))
         (key-name (sdl2:get-key-name (sdl2:sym-value keysym))))
    (or (cdr (assoc scancode-name *sdl-scancode-keysym-map*))
        (normalize-sdl-key-name key-name :shiftp shiftp))))

(defun sdl-keydown-dispatch-data (keysym)
  (let* ((modifiers (sdl2:mod-keywords (sdl2:mod-value keysym)))
         (state (sdl-modifiers->state modifiers))
         (keysym-name (sdl-keysym-name keysym))
         (special-key-p (and keysym-name
                             (keysym->terminal-key keysym-name))))
    (when (or special-key-p
              (logtest state (logior +control-mask+ +mod1-mask+ +mod4-mask+)))
      (values "" keysym-name state))))

(defun destroy-glyph-texture-entry (entry)
  (when (glyph-texture-texture entry)
    (ignore-errors
      (sdl2:destroy-texture (glyph-texture-texture entry)))))

(defun clear-glyph-cache (view)
  (maphash (lambda (key entry)
             (declare (ignore key))
             (destroy-glyph-texture-entry entry))
           (sdl2-view-glyph-cache view))
  (clrhash (sdl2-view-glyph-cache view))
  view)

(defun destroy-sdl-target (view)
  (when (sdl2-view-target view)
    (ignore-errors (sdl2:destroy-texture (sdl2-view-target view)))
    (setf (sdl2-view-target view) nil
          (sdl2-view-target-width view) 0
          (sdl2-view-target-height view) 0))
  view)

(defun destroy-sdl-view (view)
  (when view
    (clear-glyph-cache view)
    (destroy-sdl-target view)
    (ignore-errors
      (when (sdl2-view-font view)
        (sdl2-ttf:close-font (sdl2-view-font view))))
    (ignore-errors
      (when (sdl2-view-renderer view)
        (sdl2:destroy-renderer (sdl2-view-renderer view))))
    (ignore-errors
      (when (sdl2-view-window view)
        (sdl2:destroy-window (sdl2-view-window view)))))
  view)

(defun ensure-sdl-target (view grid)
  "Make sure the render target matches GRID's pixel size.  Return true when
it was (re)created, in which case its contents are undefined."
  (let ((width (* (grid-columns grid) (sdl2-view-cell-width view)))
        (height (* (grid-rows grid) (sdl2-view-cell-height view))))
    (unless (and (sdl2-view-target view)
                 (= width (sdl2-view-target-width view))
                 (= height (sdl2-view-target-height view)))
      (destroy-sdl-target view)
      (setf (sdl2-view-target view)
            (sdl2:create-texture (sdl2-view-renderer view)
                                 :argb8888 :target (max 1 width) (max 1 height))
            (sdl2-view-target-width view) width
            (sdl2-view-target-height view) height)
      t)))

(declaim (inline glyph-cache-key))
(defun glyph-cache-key (code fg attrs)
  (declare (type codepoint code) (type rgb fg) (type attributes attrs))
  (logior (ash code 26)
          (if (logtest attrs +attr-bold+) (ash 1 25) 0)
          (if (logtest attrs +attr-italic+) (ash 1 24) 0)
          fg))

(defun ensure-glyph-texture (view grid index fg attrs)
  (let* ((code (grid-glyph-code grid index))
         (combining (gethash index (grid-combining grid)))
         (cache (sdl2-view-glyph-cache view))
         (key (and (null combining) (glyph-cache-key code fg attrs))))
    (or (and key (gethash key cache))
        (let* ((text (grid-cell-string grid index))
               (font (sdl2-view-font view))
               (style (append (when (logtest attrs +attr-bold+) '(:bold))
                              (when (logtest attrs +attr-italic+) '(:italic)))))
          (when style
            (sdl2-ttf:set-font-style font style))
          (let* ((surface (unwind-protect
                               (sdl2-ttf::render-utf8-blended font text
                                                              (ldb (byte 8 16) fg)
                                                              (ldb (byte 8 8) fg)
                                                              (ldb (byte 8 0) fg)
                                                              255)
                            (when style
                              (sdl2-ttf:set-font-style font '(:normal)))))
                 (texture (sdl2:create-texture-from-surface
                           (sdl2-view-renderer view)
                           surface))
                 (entry (make-glyph-texture :texture texture
                                            :width (sdl2:texture-width texture)
                                            :height (sdl2:texture-height texture))))
            (sdl2:free-surface surface)
            (when key
              (setf (gethash key cache) entry))
            entry)))))

(defun sdl-set-color (renderer rgb &optional (alpha 255))
  (sdl2:set-render-draw-color renderer
                              (ldb (byte 8 16) rgb)
                              (ldb (byte 8 8) rgb)
                              (ldb (byte 8 0) rgb)
                              alpha))

(defun draw-sdl-row (view grid row palette rect)
  (let* ((renderer (sdl2-view-renderer view))
         (cell-width (sdl2-view-cell-width view))
         (cell-height (sdl2-view-cell-height view))
         (columns (grid-columns grid))
         (y (* row cell-height))
         (base (grid-index grid row 0)))
    ;; Backgrounds, merged into runs of equal color.
    (let ((run-start 0)
          (run-color (nth-value 1 (cell-rgb grid base palette))))
      (flet ((flush (end)
               ;; Under images, the default background is see-through.
               (sdl-set-color renderer run-color
                              (if (and (sdl2-view-layered view)
                                       (= run-color (palette-background palette)))
                                  0 255))
               (setf (sdl2:rect-x rect) (* run-start cell-width)
                     (sdl2:rect-y rect) y
                     (sdl2:rect-width rect) (* (- end run-start) cell-width)
                     (sdl2:rect-height rect) cell-height)
               (sdl2:render-fill-rect renderer rect)))
        (loop for column from 1 below columns
              for bg = (nth-value 1 (cell-rgb grid (+ base column) palette))
              unless (= bg run-color)
                do (flush column)
                   (setf run-start column
                         run-color bg))
        (flush columns)))
    ;; Glyphs and decorations.
    (dotimes (column columns)
      (let* ((index (+ base column))
             (code (grid-glyph-code grid index))
             (attrs (aref (grid-attrs grid) index))
             (x (* column cell-width)))
        (multiple-value-bind (fg bg) (cell-rgb grid index palette)
          (declare (ignore bg))
          (unless (or (zerop code)
                      (= code 32)
                      (logtest attrs (logior +attr-wide-spacer+ +attr-invisible+)))
            (let ((glyph (ensure-glyph-texture view grid index fg attrs)))
              (setf (sdl2:rect-x rect) x
                    (sdl2:rect-y rect) (+ y (max 0 (floor (- cell-height
                                                               (glyph-texture-height glyph))
                                                            2)))
                    (sdl2:rect-width rect) (glyph-texture-width glyph)
                    (sdl2:rect-height rect) (glyph-texture-height glyph))
              (sdl2:render-copy renderer (glyph-texture-texture glyph)
                                :source-rect (cffi:null-pointer)
                                :dest-rect rect)))
          (when (logtest attrs (logior +attr-underlines+ +attr-strike+))
            (let ((width (* cell-width (if (logtest attrs +attr-wide+) 2 1))))
              (when (logtest attrs +attr-underlines+)
                (sdl-set-color renderer (cell-underline-rgb grid index palette fg))
                (loop for (rx ry rw rh) in (underline-rectangles (underline-style attrs)
                                                                 x width cell-height)
                      do (setf (sdl2:rect-x rect) rx (sdl2:rect-y rect) (+ y ry)
                               (sdl2:rect-width rect) rw (sdl2:rect-height rect) rh)
                         (sdl2:render-fill-rect renderer rect)))
              (sdl-set-color renderer fg)
              (when (logtest attrs +attr-strike+)
                (let ((mid (+ y (floor cell-height 2))))
                  (sdl2:render-draw-line renderer x mid (+ x width -1) mid))))))))
    ;; Non-block cursors are drawn over the glyph.
    (when (and (grid-cursor-visible-p grid)
               (= row (grid-cursor-row grid))
               (not (eq (grid-cursor-shape grid) :block)))
      (sdl-set-color renderer (palette-cursor palette))
      (let ((x (* (grid-cursor-column grid) cell-width)))
        (if (eq (grid-cursor-shape grid) :bar)
            (setf (sdl2:rect-x rect) x (sdl2:rect-y rect) y
                  (sdl2:rect-width rect) 2 (sdl2:rect-height rect) cell-height)
            (setf (sdl2:rect-x rect) x (sdl2:rect-y rect) (+ y cell-height -2)
                  (sdl2:rect-width rect) cell-width (sdl2:rect-height rect) 2))
        (sdl2:render-fill-rect renderer rect)))))

(defun sdl-image-records (view)
  (let ((store (sdl2-view-image-store view)))
    (and store (terminal-image-render-list store (sdl2-view-cell-width view)
                                           (sdl2-view-cell-height view)))))

(defun draw-sdl-images (view records)
  (let ((renderer (sdl2-view-renderer view)))
    (dolist (record records)
      (destructuring-bind (placement image x y width height) record
        (let ((rgba (terminal-image-rgba image)))
          (sb-sys:with-pinned-objects (rgba)
            (let* ((surface (sdl2:create-rgb-surface-with-format-from
                             (sb-sys:vector-sap rgba)
                             (terminal-image-width image) (terminal-image-height image)
                             32 (* 4 (terminal-image-width image))
                             :format sdl2:+pixelformat-rgba32+))
                   (texture (sdl2:create-texture-from-surface renderer surface)))
              (unwind-protect
                   (sdl2:with-rects
                       ((source (terminal-image-placement-source-x placement)
                                (terminal-image-placement-source-y placement)
                                (or (terminal-image-placement-source-width placement)
                                    (- (terminal-image-width image)
                                       (terminal-image-placement-source-x placement)))
                                (or (terminal-image-placement-source-height placement)
                                    (- (terminal-image-height image)
                                       (terminal-image-placement-source-y placement))))
                        (destination x y width height))
                     (sdl2:render-copy renderer texture
                                       :source-rect source :dest-rect destination))
                (sdl2:destroy-texture texture)
                (sdl2:free-surface surface)))))))))

(defun present-sdl-view (view &optional (palette (view-palette view)))
  (let ((renderer (sdl2-view-renderer view)))
    (sdl2:set-render-target renderer nil)
    (sdl-set-color renderer (palette-background palette))
    (sdl2:render-clear renderer)
    ;; Images with negative z go under the grid.  Unlike the GPU frontends,
    ;; which draw them between backgrounds and text, they are then hidden by
    ;; every background but the default one.
    (multiple-value-bind (below-backgrounds below-text above)
        (image-layers (sdl-image-records view))
      (draw-sdl-images view (append below-backgrounds below-text))
      (sdl2:with-rects ((dest 0 0 (sdl2-view-target-width view)
                              (sdl2-view-target-height view)))
        (sdl2:render-copy renderer (sdl2-view-target view)
                          :source-rect (cffi:null-pointer)
                          :dest-rect dest))
      (draw-sdl-images view above))
    (sdl2:render-present renderer)))

(defun sdl-layered-p (view)
  "True when images lie under the text."
  (and (find-if (lambda (record) (minusp (terminal-image-placement-z-index (first record))))
                (sdl-image-records view))
       t))

(defun render-sdl-grid (view grid &optional (palette (view-palette view)))
  (let ((renderer (sdl2-view-renderer view)))
    (when (ensure-sdl-target view grid)
      (grid-mark-all-dirty grid))
    (setf (sdl2-view-grid view) grid)
    (unless (eq (sdl-layered-p view) (sdl2-view-layered view))
      (setf (sdl2-view-layered view) (sdl-layered-p view))
      (sdl2:set-texture-blend-mode (sdl2-view-target view)
                                   (if (sdl2-view-layered view) :blend :none))
      (grid-mark-all-dirty grid))
    (sdl2:set-render-target renderer (sdl2-view-target view))
    (sdl2:with-rects ((rect 0 0 0 0))
      (dotimes (row (grid-rows grid))
        (when (= 1 (sbit (grid-dirty grid) row))
          (draw-sdl-row view grid row palette rect))))
    (present-sdl-view view palette))
  view)

(defun create-sdl-renderer (window)
  (or (ignore-errors
        (sdl2:create-renderer window nil '(:accelerated :presentvsync :targettexture)))
      (ignore-errors
        (sdl2:create-renderer window nil '(:accelerated :targettexture)))
      (sdl2:create-renderer window nil '(:software :targettexture))))

(defun maybe-dispatch-sdl-close (view)
  (unless (sdl2-view-close-dispatched-p view)
    (setf (sdl2-view-close-dispatched-p view) t)
    (when (sdl2-view-close-callback view)
      (funcall (sdl2-view-close-callback view)))))

(defun ensure-sdl-timer-event-type (frontend)
  (or (sdl2-frontend-timer-event-type frontend)
      (setf (sdl2-frontend-timer-event-type frontend)
            (progn
              (sdl2:register-user-event-type :slt-timer)
              :slt-timer))))

(defun dispatch-sdl-timer (frontend token)
  (let ((thunk (gethash token (sdl2-frontend-timers frontend))))
    (when thunk
      (remhash token (sdl2-frontend-timers frontend))
      (funcall thunk))))

(defvar *sdl-scroll-rows* 3
  "Rows scrolled per mouse wheel notch.")

(defun deliver-sdl-mouse (view action x y clicks button)
  (when (sdl2-view-mouse-callback view)
    (funcall (sdl2-view-mouse-callback view)
             action
             (floor y (sdl2-view-cell-height view))
             (floor x (sdl2-view-cell-width view))
             clicks
             button
             (sdl-modifiers->state
              (sdl2:mod-keywords (sdl2-ffi.functions:sdl-get-mod-state))))))

(defun sdl-button-number (button)
  "slt's number for an SDL button: SDL numbers left, middle, right 1-3."
  (and (<= 1 button 3) button))

(defun run-sdl2-frontend-loop (frontend thunk)
  (sdl2:with-init (:video)
    (setf (sdl2-frontend-ttf-initialized-p frontend) nil
          (sdl2-frontend-image-initialized-p frontend) nil)
    (unwind-protect
         (progn
           (sdl2-ttf:init)
           (setf (sdl2-frontend-ttf-initialized-p frontend) t)
           (when (ignore-errors (sdl2-image:init '(:png)))
             (setf (sdl2-frontend-image-initialized-p frontend) t))
           (ensure-sdl-timer-event-type frontend)
           (funcall thunk)
           (let ((view (sdl2-frontend-view frontend)))
             (when view
               (sdl2:start-text-input)
               (let ((key-event :press))
               (sdl2:with-event-loop (:method :poll)
                 (:keydown (:keysym keysym :repeat repeat)
                           ;; Text input that follows comes from this key.
                           (setf key-event (if (zerop repeat) :press :repeat))
                           (multiple-value-bind (char keysym-name state)
                               (sdl-keydown-dispatch-data keysym)
                             (when (and keysym-name
                                        (sdl2-view-keypress-callback view))
                               (funcall (sdl2-view-keypress-callback view)
                                        char
                                        keysym-name
                                        state
                                        key-event))))
                 (:keyup (:keysym keysym)
                         (let ((name (sdl-keysym-name keysym))
                               (state (sdl-modifiers->state
                                       (sdl2:mod-keywords (sdl2:mod-value keysym)))))
                           (when (and name (sdl2-view-keypress-callback view))
                             (funcall (sdl2-view-keypress-callback view)
                                      (if (= (length name) 1) name "")
                                      name
                                      (logand state (lognot +shift-mask+))
                                      :release))))
                 (:dropfile (:file file)
                            (let ((name (sdl-drop-file-name file)))
                              (when (and name (sdl2-view-drop-callback view))
                                (funcall (sdl2-view-drop-callback view) (list name)))))
                 (:textinput (:text text)
                             (when (and (plusp (length text))
                                        (sdl2-view-keypress-callback view))
                               (funcall (sdl2-view-keypress-callback view)
                                        text
                                        text
                                        0
                                        key-event)))
                 (:mousebuttondown (:button button :x x :y y :clicks clicks)
                                   (let ((number (sdl-button-number button)))
                                     (when number
                                       (setf (sdl2-view-button view) number)
                                       (deliver-sdl-mouse view :press x y clicks number))))
                 (:mousebuttonup (:button button :x x :y y :clicks clicks)
                                 (let ((number (sdl-button-number button)))
                                   (when number
                                     (setf (sdl2-view-button view) nil)
                                     (deliver-sdl-mouse view :release x y clicks number))))
                 (:mousemotion (:x x :y y)
                               (let ((button (sdl2-view-button view)))
                                 (deliver-sdl-mouse view (if button :drag :motion) x y 1 button)))
                 (:mousewheel (:y y)
                              (when (and (/= y 0) (sdl2-view-scroll-callback view))
                                ;; Wheel up (positive) moves back in history.
                                (funcall (sdl2-view-scroll-callback view)
                                         (* (- y) *sdl-scroll-rows*))))
                 (:windowevent (:event event :data1 data1 :data2 data2)
                               (declare (ignore data1 data2))
                               (case event
                                 ((5 6)
                                  (when (sdl2-view-resize-callback view)
                                    (funcall (sdl2-view-resize-callback view))))
                                 (14
                                  (maybe-dispatch-sdl-close view)
                                  (sdl2:push-quit-event))))
                 (:slt-timer (:user-data token)
                             (dispatch-sdl-timer frontend token))
                 (:idle ()
                        (sdl2:delay 1))
                 (:quit ()
                        (maybe-dispatch-sdl-close view)
                        t))))))
      (ignore-errors
        (sdl2:stop-text-input))
      (ignore-errors
        (destroy-sdl-view (sdl2-frontend-view frontend)))
      (setf (sdl2-frontend-view frontend) nil)
      (ignore-errors
        (when (sdl2-frontend-image-initialized-p frontend)
          (sdl2-image:quit)))
      (ignore-errors
        (when (sdl2-frontend-ttf-initialized-p frontend)
          (sdl2-ttf:quit))))))

(defmethod frontend-call-with-event-loop ((frontend sdl2-frontend) thunk)
  (if (sdl-frontend-use-main-thread-p)
      (sdl2:make-this-thread-main
       (lambda ()
         (run-sdl2-frontend-loop frontend thunk)))
      (run-sdl2-frontend-loop frontend thunk)))

(defmethod frontend-available-font-families ((frontend sdl2-frontend))
  (declare (ignore frontend))
  '())

(defmethod frontend-resolve-font-size ((frontend sdl2-frontend) font-size)
  (declare (ignore frontend))
  (positive-font-size font-size))

(defmethod frontend-create-view ((frontend sdl2-frontend)
                                &key rows columns font-family font-size
                                  cell-width cell-height title)
  (let* ((resolved-font-path (resolve-sdl-font-path font-family))
         (font (sdl2-ttf:open-font resolved-font-path font-size))
         (measured-width (multiple-value-bind (width height)
                             (sdl2-ttf::size-utf8 font "M")
                           (declare (ignore height))
                           (max 1 width)))
         (measured-height (multiple-value-bind (width height)
                              (sdl2-ttf::size-utf8 font "M")
                            (declare (ignore width))
                            (max 1 height)))
         (resolved-cell-width (or cell-width measured-width))
         (resolved-cell-height (or cell-height measured-height))
         (window (sdl2:create-window :title title
                                     :w (* columns resolved-cell-width)
                                     :h (* rows resolved-cell-height)
                                     :flags '(:shown :resizable)))
         (renderer (create-sdl-renderer window))
         (view (%make-sdl2-view :window window
                                :renderer renderer
                                :font font
                                :font-path resolved-font-path
                                :font-size font-size
                                :font-family font-family
                                :cell-width resolved-cell-width
                                :cell-height resolved-cell-height
                                :rows rows
                                :columns columns)))
    (setf (sdl2-frontend-view frontend) view)
    view))

(defmethod frontend-view-cell-width ((frontend sdl2-frontend) (view sdl2-view))
  (declare (ignore frontend))
  (sdl2-view-cell-width view))

(defmethod frontend-view-cell-height ((frontend sdl2-frontend) (view sdl2-view))
  (declare (ignore frontend))
  (sdl2-view-cell-height view))

(defmethod frontend-view-window-size ((frontend sdl2-frontend) (view sdl2-view))
  (declare (ignore frontend))
  (sdl2:get-window-size (sdl2-view-window view)))

(defmethod frontend-resize-view ((frontend sdl2-frontend) (view sdl2-view) rows columns)
  (declare (ignore frontend))
  (setf (sdl2-view-rows view) rows
        (sdl2-view-columns view) columns)
  view)

(defmethod frontend-render-grid ((frontend sdl2-frontend) (view sdl2-view) grid)
  (declare (ignore frontend))
  (render-sdl-grid view grid))

(defmethod frontend-render-images ((frontend sdl2-frontend) (view sdl2-view) store)
  (declare (ignore frontend))
  (setf (sdl2-view-image-store view) store)
  (cond ((and (sdl2-view-grid view)
              (not (eq (sdl-layered-p view) (sdl2-view-layered view))))
         ;; The grid must be redrawn with or without its default background.
         (render-sdl-grid view (sdl2-view-grid view)))
        ((sdl2-view-target view)
         (present-sdl-view view))))

(defmethod frontend-set-title ((frontend sdl2-frontend) (view sdl2-view) title)
  (declare (ignore frontend))
  (sdl2:set-window-title (sdl2-view-window view) title))

(defmethod frontend-schedule ((frontend sdl2-frontend) (view sdl2-view) delay-ms thunk)
  (declare (ignore view))
  (let ((token (incf (sdl2-frontend-timer-seq frontend))))
    (setf (gethash token (sdl2-frontend-timers frontend)) thunk)
    (bt:make-thread
     (lambda ()
       (sleep (/ (max 0 delay-ms) 1000.0))
       (when (gethash token (sdl2-frontend-timers frontend))
         (ignore-errors
           (sdl2:push-user-event (ensure-sdl-timer-event-type frontend) token))))
     :name (format nil "slt-sdl2-timer-~d" token))
    token))

(defmethod frontend-cancel-scheduled ((frontend sdl2-frontend) (view sdl2-view) token)
  (declare (ignore view))
  (remhash token (sdl2-frontend-timers frontend))
  nil)

(defmethod frontend-bind-resize ((frontend sdl2-frontend) (view sdl2-view) callback)
  (declare (ignore frontend))
  (setf (sdl2-view-resize-callback view) callback)
  view)

(defun sdl-drop-file-name (file)
  "The name in a drop event's FILE, which SDL allocated for us to free."
  (let ((pointer (typecase file
                   (string nil)
                   (cffi:foreign-pointer file)
                   (t (autowrap:ptr file)))))
    (if pointer
        (unless (cffi:null-pointer-p pointer)
          (prog1 (cffi:foreign-string-to-lisp pointer :encoding :utf-8)
            (sdl2-ffi.functions:sdl-free pointer)))
        file)))

(defmethod frontend-bind-drop ((frontend sdl2-frontend) (view sdl2-view) callback)
  (declare (ignore frontend))
  (setf (sdl2-view-drop-callback view) callback))

(defmethod frontend-bind-keypress ((frontend sdl2-frontend) (view sdl2-view) callback)
  (declare (ignore frontend))
  (setf (sdl2-view-keypress-callback view) callback)
  view)

(defmethod frontend-bind-mouse ((frontend sdl2-frontend) (view sdl2-view) callback)
  (declare (ignore frontend))
  (setf (sdl2-view-mouse-callback view) callback)
  view)

(defmethod frontend-bind-scroll ((frontend sdl2-frontend) (view sdl2-view) callback)
  (declare (ignore frontend))
  (setf (sdl2-view-scroll-callback view) callback)
  view)

(defmethod frontend-on-close ((frontend sdl2-frontend) (view sdl2-view) callback)
  (declare (ignore frontend))
  (setf (sdl2-view-close-callback view) callback)
  view)

(defmethod frontend-focus ((frontend sdl2-frontend) (view sdl2-view))
  (declare (ignore frontend))
  (sdl2:raise-window (sdl2-view-window view)))

(defmethod frontend-request-exit ((frontend sdl2-frontend) (view sdl2-view))
  (declare (ignore frontend))
  (unless (sdl2-view-exit-requested-p view)
    (setf (sdl2-view-exit-requested-p view) t)
    (sdl2:push-quit-event)))

(register-frontend :sdl2 (lambda () (make-instance 'sdl2-frontend)))
