(in-package #:slt)

(defclass sdl2-backend (terminal-backend)
  ((view
    :initform nil
    :accessor sdl2-backend-view)
   (timer-seq
    :initform 0
    :accessor sdl2-backend-timer-seq)
   (timers
    :initform (make-hash-table :test #'eql)
    :accessor sdl2-backend-timers)
   (ttf-initialized-p
    :initform nil
    :accessor sdl2-backend-ttf-initialized-p)
   (image-initialized-p
    :initform nil
    :accessor sdl2-backend-image-initialized-p)
   (timer-event-type
    :initform nil
    :accessor sdl2-backend-timer-event-type)))

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
  (glyph-cache (make-hash-table :test #'equal))
  resize-callback
  keypress-callback
  close-callback
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
  '((:scancode-kp-0 . "KP_0")
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

(defun sdl-backend-use-main-thread-p (&optional
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
           (error "Unable to locate a usable font file for SDL2 backend."))
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

(defun destroy-sdl-view (view)
  (when view
    (clear-glyph-cache view)
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

(defun ensure-glyph-texture (view text foreground)
  (unless (or (null text)
              (zerop (length text))
              (string= text " "))
    (let* ((key (list text foreground))
           (cache (sdl2-view-glyph-cache view))
           (entry (gethash key cache)))
      (or entry
          (multiple-value-bind (red green blue alpha)
              (hex-color-rgba foreground)
            (let* ((surface (sdl2-ttf::render-utf8-blended (sdl2-view-font view)
                                                           text
                                                           red
                                                           green
                                                           blue
                                                           alpha))
                   (texture (sdl2:create-texture-from-surface
                             (sdl2-view-renderer view)
                             surface))
                   (new-entry (make-glyph-texture :texture texture
                                                  :width (sdl2:texture-width texture)
                                                  :height (sdl2:texture-height texture))))
              (sdl2:free-surface surface)
              (setf (gethash key cache) new-entry)
              new-entry))))))

(defun draw-sdl-cell (view renderer cell row column background-rect glyph-rect)
  (multiple-value-bind (red green blue alpha)
      (hex-color-rgba (cell-view-bg cell))
    (setf (sdl2:rect-x background-rect) (* column (sdl2-view-cell-width view))
          (sdl2:rect-y background-rect) (* row (sdl2-view-cell-height view))
          (sdl2:rect-width background-rect) (sdl2-view-cell-width view)
          (sdl2:rect-height background-rect) (sdl2-view-cell-height view))
    (sdl2:set-render-draw-color renderer red green blue alpha)
    (sdl2:render-fill-rect renderer background-rect))
  (let ((glyph (ensure-glyph-texture view
                                     (cell-view-char cell)
                                     (cell-view-fg cell))))
    (when glyph
      (setf (sdl2:rect-x glyph-rect)
            (+ (* column (sdl2-view-cell-width view))
               (max 0 (floor (- (sdl2-view-cell-width view)
                                (glyph-texture-width glyph))
                             2)))
            (sdl2:rect-y glyph-rect)
            (+ (* row (sdl2-view-cell-height view))
               (max 0 (floor (- (sdl2-view-cell-height view)
                                (glyph-texture-height glyph))
                             2)))
            (sdl2:rect-width glyph-rect) (glyph-texture-width glyph)
            (sdl2:rect-height glyph-rect) (glyph-texture-height glyph))
      (sdl2:render-copy renderer
                        (glyph-texture-texture glyph)
                        :source-rect (cffi:null-pointer)
                        :dest-rect glyph-rect))))

(defun render-sdl-term (view term)
  (let ((renderer (sdl2-view-renderer view)))
    (sdl2:set-render-draw-color renderer 0 0 0 255)
    (sdl2:render-clear renderer)
    (sdl2:with-rects ((background-rect 0 0 0 0)
                      (glyph-rect 0 0 0 0))
      (dotimes (row (3bst:rows term))
        (dotimes (column (3bst:columns term))
          (draw-sdl-cell view
                         renderer
                         (term-cell-view term row column)
                         row
                         column
                         background-rect
                         glyph-rect))))
    (sdl2:render-present renderer))
  view)

(defun create-sdl-renderer (window)
  (or (ignore-errors
        (sdl2:create-renderer window nil '(:accelerated :presentvsync)))
      (ignore-errors
        (sdl2:create-renderer window nil '(:accelerated)))
      (sdl2:create-renderer window nil '(:software))))

(defun maybe-dispatch-sdl-close (view)
  (unless (sdl2-view-close-dispatched-p view)
    (setf (sdl2-view-close-dispatched-p view) t)
    (when (sdl2-view-close-callback view)
      (funcall (sdl2-view-close-callback view)))))

(defun ensure-sdl-timer-event-type (backend)
  (or (sdl2-backend-timer-event-type backend)
      (setf (sdl2-backend-timer-event-type backend)
            (progn
              (sdl2:register-user-event-type :slt-timer)
              :slt-timer))))

(defun dispatch-sdl-timer (backend token)
  (let ((thunk (gethash token (sdl2-backend-timers backend))))
    (when thunk
      (remhash token (sdl2-backend-timers backend))
      (funcall thunk))))

(defun run-sdl2-backend-loop (backend thunk)
  (sdl2:with-init (:video)
    (setf (sdl2-backend-ttf-initialized-p backend) nil
          (sdl2-backend-image-initialized-p backend) nil)
    (unwind-protect
         (progn
           (sdl2-ttf:init)
           (setf (sdl2-backend-ttf-initialized-p backend) t)
           (when (ignore-errors (sdl2-image:init '(:png)))
             (setf (sdl2-backend-image-initialized-p backend) t))
           (ensure-sdl-timer-event-type backend)
           (funcall thunk)
           (let ((view (sdl2-backend-view backend)))
             (when view
               (sdl2:start-text-input)
               (sdl2:with-event-loop (:method :poll)
                 (:keydown (:keysym keysym)
                           (multiple-value-bind (char keysym-name state)
                               (sdl-keydown-dispatch-data keysym)
                             (when (and keysym-name
                                        (sdl2-view-keypress-callback view))
                               (funcall (sdl2-view-keypress-callback view)
                                        char
                                        keysym-name
                                        state))))
                 (:textinput (:text text)
                             (when (and (plusp (length text))
                                        (sdl2-view-keypress-callback view))
                               (funcall (sdl2-view-keypress-callback view)
                                        text
                                        text
                                        0)))
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
                             (dispatch-sdl-timer backend token))
                 (:idle ()
                        (sdl2:delay 1))
                 (:quit ()
                        (maybe-dispatch-sdl-close view)
                        t)))))
      (ignore-errors
        (sdl2:stop-text-input))
      (ignore-errors
        (destroy-sdl-view (sdl2-backend-view backend)))
      (setf (sdl2-backend-view backend) nil)
      (ignore-errors
        (when (sdl2-backend-image-initialized-p backend)
          (sdl2-image:quit)))
      (ignore-errors
        (when (sdl2-backend-ttf-initialized-p backend)
          (sdl2-ttf:quit))))))

(defmethod backend-call-with-event-loop ((backend sdl2-backend) thunk)
  (if (sdl-backend-use-main-thread-p)
      (sdl2:make-this-thread-main
       (lambda ()
         (run-sdl2-backend-loop backend thunk)))
      (run-sdl2-backend-loop backend thunk)))

(defmethod backend-available-font-families ((backend sdl2-backend))
  (declare (ignore backend))
  '())

(defmethod backend-resolve-font-size ((backend sdl2-backend) font-size)
  (declare (ignore backend))
  (positive-font-size font-size))

(defmethod backend-create-view ((backend sdl2-backend)
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
    (setf (sdl2-backend-view backend) view)
    view))

(defmethod backend-view-cell-width ((backend sdl2-backend) (view sdl2-view))
  (declare (ignore backend))
  (sdl2-view-cell-width view))

(defmethod backend-view-cell-height ((backend sdl2-backend) (view sdl2-view))
  (declare (ignore backend))
  (sdl2-view-cell-height view))

(defmethod backend-view-window-size ((backend sdl2-backend) (view sdl2-view))
  (declare (ignore backend))
  (sdl2:get-window-size (sdl2-view-window view)))

(defmethod backend-resize-view ((backend sdl2-backend) (view sdl2-view) rows columns)
  (declare (ignore backend))
  (setf (sdl2-view-rows view) rows
        (sdl2-view-columns view) columns)
  view)

(defmethod backend-render-rows ((backend sdl2-backend) (view sdl2-view) term rows)
  (declare (ignore backend rows))
  (render-sdl-term view term))

(defmethod backend-set-title ((backend sdl2-backend) (view sdl2-view) title)
  (declare (ignore backend))
  (sdl2:set-window-title (sdl2-view-window view) title))

(defmethod backend-schedule ((backend sdl2-backend) (view sdl2-view) delay-ms thunk)
  (declare (ignore view))
  (let ((token (incf (sdl2-backend-timer-seq backend))))
    (setf (gethash token (sdl2-backend-timers backend)) thunk)
    (bt:make-thread
     (lambda ()
       (sleep (/ (max 0 delay-ms) 1000.0))
       (when (gethash token (sdl2-backend-timers backend))
         (ignore-errors
           (sdl2:push-user-event (ensure-sdl-timer-event-type backend) token))))
     :name (format nil "slt-sdl2-timer-~d" token))
    token))

(defmethod backend-cancel-scheduled ((backend sdl2-backend) (view sdl2-view) token)
  (declare (ignore view))
  (remhash token (sdl2-backend-timers backend))
  nil)

(defmethod backend-bind-resize ((backend sdl2-backend) (view sdl2-view) callback)
  (declare (ignore backend))
  (setf (sdl2-view-resize-callback view) callback)
  view)

(defmethod backend-bind-keypress ((backend sdl2-backend) (view sdl2-view) callback)
  (declare (ignore backend))
  (setf (sdl2-view-keypress-callback view) callback)
  view)

(defmethod backend-on-close ((backend sdl2-backend) (view sdl2-view) callback)
  (declare (ignore backend))
  (setf (sdl2-view-close-callback view) callback)
  view)

(defmethod backend-focus ((backend sdl2-backend) (view sdl2-view))
  (declare (ignore backend))
  (sdl2:raise-window (sdl2-view-window view)))

(defmethod backend-request-exit ((backend sdl2-backend) (view sdl2-view))
  (declare (ignore backend))
  (unless (sdl2-view-exit-requested-p view)
    (setf (sdl2-view-exit-requested-p view) t)
    (sdl2:push-quit-event)))

(register-backend :sdl2 (lambda () (make-instance 'sdl2-backend)))
