(in-package #:slt)

(defvar *read-buffer-size* (* 64 1024))

(defvar *frame-interval-ms* 8
  "Minimum time between renders; output arriving faster is coalesced.")

(defvar *load-user-config* t
  "When true, LAUNCH-TERMINAL loads USER-CONFIG-PATHNAME if it exists.")

(defstruct (terminal-emulator
            (:constructor %make-terminal-emulator))
  engine
  (grid nil :type (or null grid))
  backend
  view
  (rows 24 :type fixnum)
  (columns 80 :type fixnum)
  process
  (fd nil :type (or null fixnum))
  (read-buffer (make-array *read-buffer-size* :element-type '(unsigned-byte 8))
   :type octets)
  resize-after-id
  (poll-interval 16 :type fixnum)
  (title "slt" :type string)
  (closed-p nil)
  ;; The reader thread parses child output while the UI thread renders.
  ;; LOCK guards the engine; WRITE-LOCK keeps writes to the child whole.
  (lock (bt:make-recursive-lock "slt engine"))
  (write-lock (bt:make-lock "slt pty write"))
  reader-thread
  ;; Work queued for the UI thread by other threads (see INVOKE-ON-UI).
  (ui-lock (bt:make-lock "slt ui queue"))
  (ui-queue '() :type list)
  (render-requested nil)
  (last-render-time 0 :type integer)
  ;; Mouse selection: the cell index where it started, and whether it
  ;; extends by characters, words or lines.
  (selection-anchor nil :type (or null fixnum))
  (selection-unit :char :type (member :char :word :line)))

(defmacro with-engine ((engine emulator) &body body)
  "Run BODY holding EMULATOR's engine lock, with ENGINE bound to the engine."
  (let ((e (gensym "EMULATOR")))
    `(let* ((,e ,emulator)
            (,engine (terminal-emulator-engine ,e)))
       (bt:with-recursive-lock-held ((terminal-emulator-lock ,e))
         ,@body))))

(defun gl-backend-usable-p ()
  "True when the gl backend loads and finds GLFW and a display."
  (and (or (uiop:getenv "DISPLAY") (uiop:getenv "WAYLAND_DISPLAY"))
       (ignore-errors
        (asdf:load-system "slt/gl")
        (uiop:symbol-call '#:slt '#:gl-libraries-available-p))))

(defun default-backend ()
  "Cocoa (Metal) on macOS, the gl backend where GLFW is available, else LTK."
  (cond ((and (uiop:os-macosx-p) (member :cocoa (available-backends))) :cocoa)
        ((and (not (uiop:os-macosx-p)) (gl-backend-usable-p)) :gl)
        (t :ltk)))

(defun user-config-pathname ()
  (uiop:xdg-config-home "slt/init.lisp"))

(defun load-user-config (&optional (pathname (user-config-pathname)))
  (when (and pathname (probe-file pathname))
    (let ((*package* (find-package '#:slt)))
      (load pathname))
    pathname))

(defun send-to-process (emulator string)
  (let ((fd (terminal-emulator-fd emulator)))
    (when (and fd string (plusp (length string)))
      (bt:with-lock-held ((terminal-emulator-write-lock emulator))
        (handler-case (write-octets fd (string->octets string))
          (error () nil))))))

;;; Running work on the UI thread

(defun drain-ui-queue (emulator)
  "Run the work other threads queued for the UI thread.  UI thread only."
  (let ((thunks (bt:with-lock-held ((terminal-emulator-ui-lock emulator))
                  (shiftf (terminal-emulator-ui-queue emulator) '()))))
    (dolist (thunk (nreverse thunks))
      (unless (terminal-emulator-closed-p emulator)
        (funcall thunk)))))

(defun invoke-on-ui (emulator thunk)
  "Run THUNK on the UI thread soon.  Callable from any thread."
  (let ((first-p (bt:with-lock-held ((terminal-emulator-ui-lock emulator))
                   (prog1 (null (terminal-emulator-ui-queue emulator))
                     (push thunk (terminal-emulator-ui-queue emulator))))))
    ;; Backends without a wakeup leave the queue to the poll timer.
    (when first-p
      (backend-wakeup (terminal-emulator-backend emulator)
                      (terminal-emulator-view emulator)
                      (lambda () (drain-ui-queue emulator))))))

(defun request-render (emulator)
  "Ask for a render on the UI thread, merging requests that arrive before
it happens.  Callable from any thread."
  (when (bt:with-lock-held ((terminal-emulator-ui-lock emulator))
          (unless (terminal-emulator-render-requested emulator)
            (setf (terminal-emulator-render-requested emulator) t)))
    (invoke-on-ui emulator (lambda () (render-paced emulator)))))

(defun render-paced (emulator)
  "Render now, or after *FRAME-INTERVAL-MS* since the previous frame."
  (let* ((now (get-internal-real-time))
         (wait-ms (- *frame-interval-ms*
                     (round (* 1000 (- now (terminal-emulator-last-render-time emulator)))
                            internal-time-units-per-second))))
    (flet ((render ()
             (bt:with-lock-held ((terminal-emulator-ui-lock emulator))
               (setf (terminal-emulator-render-requested emulator) nil))
             (setf (terminal-emulator-last-render-time emulator) (get-internal-real-time))
             (unless (terminal-emulator-closed-p emulator)
               (render-emulator emulator))))
      (if (plusp wait-ms)
          (backend-schedule (terminal-emulator-backend emulator)
                            (terminal-emulator-view emulator)
                            wait-ms #'render)
          (render)))))

(defun fit-grid-to-size (width height cell-width cell-height)
  (values (max 1 (floor width (max 1 cell-width)))
          (max 1 (floor height (max 1 cell-height)))))

(defun render-emulator (emulator &key force)
  "Copy the engine's changes into the grid and draw them.  UI thread only;
the engine lock is held only for the copy."
  (let ((grid (terminal-emulator-grid emulator)))
    (with-engine (engine emulator)
      (engine-update-grid engine grid :force force))
    ;; Dirty rows here are the engine's changes, since every render clears
    ;; them; a selection whose text changed no longer means anything.
    (when (grid-selection-intersects-dirty-p grid)
      (clear-selection emulator))
    (when (find 1 (grid-dirty grid))
      (backend-render-grid (terminal-emulator-backend emulator)
                           (terminal-emulator-view emulator)
                           grid)
      (grid-clear-dirty grid))
    emulator))

(defun write-input (emulator string)
  "Send STRING to the child, or echo it locally when there is no child."
  (cond
    ((or (null string) (zerop (length string))))
    ((terminal-emulator-fd emulator)
     (send-to-process emulator string))
    (t
     (with-engine (engine emulator)
       (engine-feed-string engine string))
     (render-emulator emulator)))
  emulator)

(defun destroy-emulator (emulator)
  (unless (terminal-emulator-closed-p emulator)
    (setf (terminal-emulator-closed-p emulator) t)
    (ignore-errors (run-hook '*exit-hook* emulator))
    (when (terminal-emulator-resize-after-id emulator)
      (ignore-errors
        (backend-cancel-scheduled (terminal-emulator-backend emulator)
                                  (terminal-emulator-view emulator)
                                  (terminal-emulator-resize-after-id emulator))))
    (let ((process (terminal-emulator-process emulator)))
      (when process
        (ignore-errors
          (when (process-alive-p process)
            (terminate-process process)))
        (ignore-errors
          (close (process-pty-stream process)))))
    (ignore-errors (engine-close (terminal-emulator-engine emulator))))
  emulator)

(defun emulator-cell-pixels (emulator)
  (let ((backend (terminal-emulator-backend emulator))
        (view (terminal-emulator-view emulator)))
    (values (backend-view-cell-width backend view)
            (backend-view-cell-height backend view))))

(defun resize-emulator (emulator rows columns)
  (clear-selection emulator)
  (setf (terminal-emulator-rows emulator) rows
        (terminal-emulator-columns emulator) columns)
  (with-engine (engine emulator)
    (engine-resize engine rows columns))
  (backend-resize-view (terminal-emulator-backend emulator)
                       (terminal-emulator-view emulator)
                       rows
                       columns)
  (render-emulator emulator :force t))

(defun apply-resize (emulator)
  (setf (terminal-emulator-resize-after-id emulator) nil)
  (multiple-value-bind (width height)
      (backend-view-window-size (terminal-emulator-backend emulator)
                                (terminal-emulator-view emulator))
    (multiple-value-bind (cell-width cell-height) (emulator-cell-pixels emulator)
      (multiple-value-bind (columns rows)
          (fit-grid-to-size width height cell-width cell-height)
        (unless (and (= rows (terminal-emulator-rows emulator))
                     (= columns (terminal-emulator-columns emulator)))
          (resize-emulator emulator rows columns)
          (resize-process-pty (terminal-emulator-process emulator)
                              rows
                              columns
                              :pixel-width (* columns cell-width)
                              :pixel-height (* rows cell-height)))))))

(defun schedule-resize (emulator)
  "Apply the window's size shortly.  Requests arriving while one is pending
join it rather than postponing it, so a steady stream of resize events
(a window being dragged) cannot starve it; APPLY-RESIZE reads the size
current when it runs."
  (unless (terminal-emulator-resize-after-id emulator)
    (setf (terminal-emulator-resize-after-id emulator)
          (backend-schedule (terminal-emulator-backend emulator)
                            (terminal-emulator-view emulator)
                            120
                            (lambda ()
                              (unless (terminal-emulator-closed-p emulator)
                                (apply-resize emulator)))))))

(defun scroll-viewport (emulator how)
  "Scroll EMULATOR's viewport: HOW is :TOP, :BOTTOM, :PAGE-UP, :PAGE-DOWN or
a number of rows (negative scrolls back).  Return true if the engine has
scrollback."
  (let ((how (case how
               (:page-up (- (max 1 (1- (terminal-emulator-rows emulator)))))
               (:page-down (max 1 (1- (terminal-emulator-rows emulator))))
               (t how))))
    (when (with-engine (engine emulator)
            (engine-scroll engine how))
      (request-render emulator)
      t)))

(defun handle-scroll (emulator rows)
  "Mouse wheel: scroll history, or on the alternate screen (vim, less) send
arrow keys so the program scrolls."
  (cond
    ((zerop rows))
    ((with-engine (engine emulator) (engine-mode engine :alternate-screen))
     (let ((sequence (with-engine (engine emulator)
                       (engine-encode-key engine (if (minusp rows) :up :down) '()))))
       (loop repeat (abs rows) do (write-input emulator sequence))))
    (t (scroll-viewport emulator rows))))

(defun scrollback-key-handler (emulator key modifiers)
  "The default scrollback bindings: Shift+PageUp/PageDown by page and
Shift+Home/End to the top or bottom.  On *KEY-HOOK*; remove it to rebind."
  (when (equal modifiers '(:shift))
    (case key
      (:page-up (scroll-viewport emulator :page-up))
      (:page-down (scroll-viewport emulator :page-down))
      (:home (scroll-viewport emulator :top))
      (:end (scroll-viewport emulator :bottom)))))

(add-hook '*key-hook* 'scrollback-key-handler)

(defun handle-keypress (emulator char keysym state)
  (let ((key (event-key char keysym))
        (modifiers (decode-modifier-state state)))
    (when (and key
               (not (run-hook-until-success '*key-hook* emulator key modifiers))
               ;; Super (Command) combinations are shortcuts, not input.
               (not (member :super modifiers)))
      ;; Typing returns the view to the live screen and ends a selection.
      (scroll-viewport emulator :bottom)
      (clear-selection emulator)
      (write-input emulator
                   (with-engine (engine emulator)
                     (engine-encode-key engine key modifiers))))))

;;; Selection and the clipboard

(defvar *selection-word-characters* "-_.,/~:@%+#?&="
  "Characters other than letters and digits that a double click selects as
part of a word, so that paths and URLs are selected whole.")

(defvar *copy-on-select* nil
  "When true, finishing a mouse selection copies it to the clipboard.")

(defun render-selection (emulator)
  "Redraw the rows whose selection changed, without consulting the engine."
  (let ((grid (terminal-emulator-grid emulator)))
    (when (find 1 (grid-dirty grid))
      (backend-render-grid (terminal-emulator-backend emulator)
                           (terminal-emulator-view emulator)
                           grid)
      (grid-clear-dirty grid))))

(defun clear-selection (emulator)
  (let ((grid (terminal-emulator-grid emulator)))
    (setf (terminal-emulator-selection-anchor emulator) nil)
    (when (grid-selection grid)
      (grid-set-selection grid nil nil))))

(defun word-cell-p (grid index)
  (let ((code (aref (grid-codes grid) index)))
    (and (plusp code)
         (let ((char (code-char code)))
           (or (alphanumericp char) (find char *selection-word-characters*))))))

(defun selection-extent (grid anchor point unit)
  "The first and last cell indices selected from ANCHOR to POINT by UNIT."
  (let* ((columns (grid-columns grid))
         (first (min anchor point))
         (last (max anchor point)))
    (ecase unit
      (:char (values first last))
      (:line (values (* columns (floor first columns))
                     (1- (* columns (1+ (floor last columns))))))
      (:word
       (let ((row-start (* columns (floor first columns)))
             (row-end (1- (* columns (1+ (floor last columns))))))
         (when (word-cell-p grid first)
           (loop while (and (> first row-start) (word-cell-p grid (1- first)))
                 do (decf first)))
         (when (word-cell-p grid last)
           (loop while (and (< last row-end) (word-cell-p grid (1+ last)))
                 do (incf last)))
         (values first last))))))

(defun handle-mouse (emulator action row column clicks)
  "Select with the primary button: drag for characters, double-click for
words and triple-click for lines."
  (let* ((grid (terminal-emulator-grid emulator))
         (rows (grid-rows grid))
         (columns (grid-columns grid))
         (index (+ (* columns (max 0 (min row (1- rows))))
                   ;; Past the right edge selects to the end of the row.
                   (max 0 (min column (1- columns))))))
    (when (plusp (* rows columns))
      (ecase action
        (:press
         (setf (terminal-emulator-selection-unit emulator)
               (case clicks (1 :char) (2 :word) (t :line))
               (terminal-emulator-selection-anchor emulator) index)
         (if (= clicks 1)
             (grid-set-selection grid nil nil)
             (multiple-value-call #'grid-set-selection grid
               (selection-extent grid index index (terminal-emulator-selection-unit emulator)))))
        (:drag
         (let ((anchor (terminal-emulator-selection-anchor emulator)))
           (when anchor
             (multiple-value-call #'grid-set-selection grid
               (selection-extent grid anchor index
                                 (terminal-emulator-selection-unit emulator))))))
        (:release
         (setf (terminal-emulator-selection-anchor emulator) nil)
         (when *copy-on-select*
           (copy-selection emulator))))
      (render-selection emulator))))

(defun copy-selection (emulator)
  "Put the selected text on the clipboard; return it, or NIL when nothing
is selected."
  (let ((text (grid-selection-text (terminal-emulator-grid emulator))))
    (when text
      (backend-set-clipboard-text (terminal-emulator-backend emulator)
                                  (terminal-emulator-view emulator)
                                  text)
      text)))

(defun paste-clipboard (emulator)
  (let ((text (backend-clipboard-text (terminal-emulator-backend emulator)
                                      (terminal-emulator-view emulator))))
    (when (and text (plusp (length text)))
      (scroll-viewport emulator :bottom)
      (paste-text emulator text))))

(defun clipboard-key-handler (emulator key modifiers)
  "The default clipboard bindings: Super+C / Super+V (Command on macOS)
and Control+Shift+C / Control+Shift+V.  On *KEY-HOOK*; remove it to rebind."
  (when (characterp key)
    (let ((shortcut (cond ((member :super modifiers) (char-downcase key))
                          ;; Shift is folded into the character.
                          ((and (member :control modifiers) (upper-case-p key))
                           (char-downcase key)))))
      (case shortcut
        (#\c (copy-selection emulator) t)
        (#\v (paste-clipboard emulator) t)))))

(add-hook '*key-hook* 'clipboard-key-handler)

;;; The menu

(defvar *menu-items* '()
  "Items of the Lisp menu, on backends that have one (cocoa): a list of
(LABEL FUNCTION).  LABEL is a string or a function of no arguments
returning one, consulted each time the menu opens; FUNCTION is called with
the emulator when the item is chosen.  Read when the window is created.")

(defun emulator-menu-items (emulator)
  "*MENU-ITEMS* in the form BACKEND-BIND-MENU takes."
  (loop for (label function) in *menu-items*
        collect (list label
                      (let ((function function))
                        (lambda () (funcall function emulator))))))

(defun paste-text (emulator text)
  "Send TEXT to the child as a paste, bracketed when the child asked for it."
  (let ((bracketed (with-engine (engine emulator)
                     (engine-mode engine :bracketed-paste)))
        (esc (code-char 27)))
    (write-input emulator
                 (if bracketed
                     (format nil "~c[200~~~a~c[201~~" esc text esc)
                     text))))

(defun reader-loop (emulator)
  "Body of the reader thread: wait for child output, feed it to the engine
in chunks and request renders, until the child's side closes."
  (let ((fd (terminal-emulator-fd emulator))
        (buffer (terminal-emulator-read-buffer emulator)))
    (loop
      (when (terminal-emulator-closed-p emulator)
        (return))
      ;; Wake periodically so a destroyed emulator's thread exits.
      (when (sb-unix:unix-simple-poll fd :input 100)
        (let ((fed nil))
          (loop
            (let ((count (read-available-octets fd buffer)))
              (cond
                ((eq count :eof)
                 (when fed (request-render emulator))
                 (invoke-on-ui emulator (lambda () (finish-emulator emulator)))
                 (return-from reader-loop))
                ((zerop count) (return))
                (t
                 (with-engine (engine emulator)
                   (engine-feed engine buffer :end count))
                 (setf fed t)
                 (request-render emulator))))))))))

(defun start-reader-thread (emulator)
  (setf (terminal-emulator-reader-thread emulator)
        (bt:make-thread (lambda ()
                          (handler-case (reader-loop emulator)
                            (error (condition)
                              ;; Closing the emulator closes the PTY under
                              ;; the reader; only other failures are news.
                              (unless (terminal-emulator-closed-p emulator)
                                (format *error-output* "~&slt: reader stopped: ~a~%"
                                        condition)))))
                        :name "slt pty reader")))

(defun finish-emulator (emulator)
  (unless (terminal-emulator-closed-p emulator)
    (render-emulator emulator)
    (setf (terminal-emulator-process emulator) nil)
    (destroy-emulator emulator)
    (backend-request-exit (terminal-emulator-backend emulator)
                          (terminal-emulator-view emulator))))

(defun schedule-poll (emulator)
  "Periodic UI-thread work: run queued work for backends without a wakeup,
and notice a child that exited without closing the PTY."
  (labels ((poll ()
             (unless (terminal-emulator-closed-p emulator)
               (drain-ui-queue emulator)
               (let ((process (terminal-emulator-process emulator)))
                 (when (and process (not (process-alive-p process))
                            (not (and (terminal-emulator-reader-thread emulator)
                                      (bt:thread-alive-p
                                       (terminal-emulator-reader-thread emulator)))))
                   (finish-emulator emulator))))
             (unless (terminal-emulator-closed-p emulator)
               (backend-schedule (terminal-emulator-backend emulator)
                                 (terminal-emulator-view emulator)
                                 (terminal-emulator-poll-interval emulator)
                                 #'poll))))
    (backend-schedule (terminal-emulator-backend emulator)
                      (terminal-emulator-view emulator)
                      (terminal-emulator-poll-interval emulator)
                      #'poll)))

(defun make-emulator-engine (engine rows columns emulator-cell)
  "Create ENGINE wired to the emulator held in the cons EMULATOR-CELL."
  (make-engine engine
               :rows rows
               :columns columns
               :output-function (lambda (string)
                                  (when (car emulator-cell)
                                    (send-to-process (car emulator-cell) string)))
               ;; Engines call these from the reader thread.
               :title-function (lambda (title)
                                 (let ((emulator (car emulator-cell)))
                                   (when emulator
                                     (invoke-on-ui
                                      emulator
                                      (lambda ()
                                        (backend-set-title
                                         (terminal-emulator-backend emulator)
                                         (terminal-emulator-view emulator)
                                         (format nil "~a - ~a"
                                                 (terminal-emulator-title emulator)
                                                 title))
                                        (run-hook '*title-change-hook* emulator title))))))
               :bell-function (lambda ()
                                (let ((emulator (car emulator-cell)))
                                  (when emulator
                                    (invoke-on-ui
                                     emulator
                                     (lambda () (run-hook '*bell-hook* emulator))))))))

(defun launch-terminal (&key
                          (backend (or (uiop:getenv "SLT_BACKEND")
                                       (default-backend)))
                          (engine (or (uiop:getenv "SLT_ENGINE")
                                      (default-engine)))
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
                          (title "slt")
                          (load-config *load-user-config*))
  (when load-config
    (load-user-config))
  (let* ((backend-instance (make-backend backend))
         (emulator-cell (list nil)))
    (backend-call-with-event-loop
     backend-instance
     (lambda ()
       (let* ((resolved-font-family
                (resolve-font-family
                 font-family
                 (backend-available-font-families backend-instance)))
              (resolved-font-size (backend-resolve-font-size backend-instance
                                                             font-size))
              (engine-instance (make-emulator-engine engine rows columns emulator-cell))
              (view (backend-create-view backend-instance
                                         :rows rows
                                         :columns columns
                                         :font-family resolved-font-family
                                         :font-size resolved-font-size
                                         :cell-width cell-width
                                         :cell-height cell-height
                                         :title title))
              (process nil)
              (emulator nil))
         (handler-case
             (setf process
                   (launch-shell-process shell
                                         :term-name term-name
                                         :rows rows
                                         :columns columns
                                         :pixel-width (* columns
                                                         (backend-view-cell-width
                                                          backend-instance view))
                                         :pixel-height (* rows
                                                          (backend-view-cell-height
                                                           backend-instance view))))
           (error (condition)
             (engine-feed-string
              engine-instance
              (format nil "Unable to start shell PTY: ~a~c~c~c~cRunning in local echo mode.~c~c"
                      condition #\Return #\Linefeed #\Return #\Linefeed
                      #\Return #\Linefeed))))
         (setf emulator (%make-terminal-emulator
                         :engine engine-instance
                         :grid (make-grid rows columns)
                         :backend backend-instance
                         :view view
                         :rows rows
                         :columns columns
                         :process process
                         :fd (and process (set-fd-nonblocking (process-fd process)))
                         :poll-interval poll-interval
                         :title title)
               (car emulator-cell) emulator)
         (render-emulator emulator :force t)
         (backend-bind-resize backend-instance
                              view
                              ;; Backends pass IMMEDIATE while the user drags
                              ;; a window edge, when timers may not run.
                              (lambda (&optional immediate)
                                (if immediate
                                    (apply-resize emulator)
                                    (schedule-resize emulator))))
         (backend-bind-scroll backend-instance
                              view
                              (lambda (rows)
                                (handle-scroll emulator rows)))
         (backend-bind-keypress backend-instance
                                view
                                (lambda (char keysym state)
                                  (handle-keypress emulator char keysym state)))
         (backend-bind-mouse backend-instance
                             view
                             (lambda (action row column clicks)
                               (handle-mouse emulator action row column clicks)))
         (backend-bind-paste backend-instance
                             view
                             (lambda (text)
                               (paste-text emulator text)))
         (backend-bind-menu backend-instance view (emulator-menu-items emulator))
         (backend-on-close backend-instance
                           view
                           (lambda ()
                             (destroy-emulator emulator)
                             (backend-request-exit backend-instance view)))
         (backend-focus backend-instance view)
         (run-hook '*after-init-hook* emulator)
         (when (terminal-emulator-fd emulator)
           (start-reader-thread emulator))
         (schedule-poll emulator))))))
