(in-package #:slt)

(defvar *read-buffer-size* (* 64 1024))

(defvar *frame-interval-ms* 8
  "Minimum time between renders; output arriving faster is coalesced.")

(defvar *cursor-blink* :program
  "Whether the cursor blinks: T always, NIL never, or :PROGRAM when the
program asks for it (DECSCUSR 1, 3 or 5, or mode 12).")

(defvar *cursor-blink-interval-ms* 600
  "How long a blinking cursor is shown, and then hidden.")

(defvar *load-user-config* t
  "When true, LAUNCH-TERMINAL loads USER-CONFIG-PATHNAME if it exists.")

(defstruct (terminal-emulator
            (:constructor %make-terminal-emulator))
  engine
  (grid nil :type (or null grid))
  (image-store (make-terminal-image-store) :type terminal-image-store)
  (stream-parser (make-terminal-stream-parser) :type terminal-stream-parser)
  ;; State of the control functions slt implements (src/controls.lisp).
  (keyboard (kitty-keyboard:make-keyboard-state))
  ;; Colors a program set (OSC 4, 10 ...), or NIL for *PALETTE*.  The view
  ;; gets a copy on the UI thread.
  (palette nil)
  (working-directory nil)
  (sync-deadline nil)
  (notifications (terminal-osc:make-notification-assembler))
  (recent-notifications '() :type list)
  ;; T or NIL once the frontend reports focus; :UNKNOWN before.
  (focused-p :unknown)
  (hyperlink nil)
  (links '() :type list)
  (link-count 0 :type fixnum)
  (prompt-marks '() :type list)
  ;; Rows the primary screen has scrolled up into the scrollback so far:
  ;; primary-screen spans and marks keep ROW + ROW-ORIGIN, which such
  ;; scrolls leave alone.
  (row-origin 0 :type integer)
  ;; Keys pressed and not yet released, by KEY-IDENTITY, so that releases
  ;; reach the child only for presses it saw.
  (keys-down (make-hash-table :test #'eql) :type hash-table)
  ;; (ROW FIRST LAST): the cells of the hyperlink under the pointer, drawn
  ;; underlined.
  (hover-link nil)
  ;; Smooth scrolling (see smooth-scroll.lisp): whether the frontend does
  ;; it, the pixels the view is between rows, the row offset it is sliding
  ;; to, rows still to pass on to programs, and the grids it draws with.
  (smooth-p nil)
  (smooth-offset 0 :type real)
  (smooth-target nil)
  (smooth-remainder 0 :type real)
  (smooth-overhang nil)
  (smooth-grid nil)
  frontend
  view
  (rows 24 :type fixnum)
  (columns 80 :type fixnum)
  process
  (fd nil :type (or null fixnum))
  ;; Opens a Lisp REPL window (see LAUNCH-TERMINAL), or NIL.
  new-repl-function
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
  (selection-unit :char :type (member :char :word :line))
  ;; Whether the child tracks the mouse, as last told to the frontend.
  (mouse-tracking nil)
  ;; The cell last under the pointer, where wheel reports are placed.
  (pointer-row 0 :type fixnum)
  (pointer-column 0 :type fixnum)
  ;; True once a mouse event has placed the pointer.
  (pointer-known-p nil)
  ;; The pointer's pixel from the grid's top left, for reports in pixels.
  (pointer-x 0 :type fixnum)
  (pointer-y 0 :type fixnum)
  ;; Whether the child asked for mouse reports in pixels (SGR-Pixels, mode
  ;; 1016).  Engines do not implement it, so it is tracked here.
  (mouse-pixels nil)
  ;; Whether a blinking cursor is in its shown half, and the pending timer
  ;; that flips it.
  (cursor-blink-on t)
  (cursor-blink-timer nil)
  ;; The (WIDTH . HEIGHT) in pixels last given to the PTY.
  (pty-pixels nil)
  ;; Whether the child asked to hear of resizes (mode 2048) and of dark or
  ;; light color scheme changes (mode 2031), and the scheme last seen.
  (in-band-resize nil)
  (color-scheme-updates nil)
  (last-color-scheme nil)
  ;; The font size now, and the one the window opened with.
  (font-size 15)
  (initial-font-size 15))

(defmacro with-engine ((engine emulator) &body body)
  "Run BODY holding EMULATOR's engine lock, with ENGINE bound to the engine."
  (let ((e (gensym "EMULATOR")))
    `(let* ((,e ,emulator)
            (,engine (terminal-emulator-engine ,e)))
       (bt:with-recursive-lock-held ((terminal-emulator-lock ,e))
         ,@body))))

(defmacro without-engine-lock ((emulator) &body body)
  "Run BODY with EMULATOR's engine lock released if this thread holds it,
taking it again afterwards.  For slow work on the reader thread that touches
neither the engine nor anything the renderer reads, such as decoding an
image, so that rendering and typing go on meanwhile."
  (let ((lock (gensym "LOCK")))
    `(let ((,lock (terminal-emulator-lock ,emulator)))
       (if #+sbcl (sb-thread:holding-mutex-p ,lock) #-sbcl nil
           (progn
             (bt:release-recursive-lock ,lock)
             (unwind-protect (progn ,@body)
               (bt:acquire-recursive-lock ,lock)))
           (progn ,@body)))))

(defmacro graphics-guard ((what) &body body)
  "Run BODY; report an error instead of letting it stop the terminal."
  `(handler-case (progn ,@body)
     ;; Not only errors: a hostile image may exhaust the heap or stack.
     (serious-condition (condition)
       (format *error-output* "~&slt: error ~a: ~a~%" ,what condition)
       nil)))

(defun gl-frontend-usable-p ()
  "True when the gl frontend loads and finds GLFW and a display."
  (and (or (uiop:getenv "DISPLAY") (uiop:getenv "WAYLAND_DISPLAY"))
       (ignore-errors
        (asdf:load-system "slt/gl")
        (uiop:symbol-call '#:slt '#:gl-libraries-available-p))))

(defun default-frontend ()
  "Cocoa (Metal) on macOS, the gl frontend where GLFW is available, else LTK."
  (cond ((and (uiop:os-macosx-p) (member :cocoa (available-frontends))) :cocoa)
        ((and (not (uiop:os-macosx-p)) (gl-frontend-usable-p)) :gl)
        (t :ltk)))

;;; The init file.  As in StumpWM, it is ordinary Lisp loaded at startup
;;; (in the slt package), so it can do anything: set variables, add hooks,
;;; define commands.  An error in it is reported rather than fatal.

(defvar *config-file* nil
  "The init file to load instead of searching CONFIG-FILE-LOCATIONS; set
by --config.")

(defvar *config-error* nil
  "A description of the error the init file signalled when it was last
loaded, or NIL.  It is shown in the terminal when the window opens.")

(defun default-config-pathname ()
  (uiop:xdg-config-home "slt/init.lisp"))

(defun config-file-locations ()
  "The places an init file is looked for, in order: $SLT_CONFIG,
~/.sltrc, ~/.slt.d/init.lisp and $XDG_CONFIG_HOME/slt/init.lisp."
  (let ((environment (uiop:getenv "SLT_CONFIG"))
        (home (user-homedir-pathname)))
    (remove nil
            (list (and environment (plusp (length environment))
                       (uiop:parse-native-namestring environment))
                  (merge-pathnames ".sltrc" home)
                  (merge-pathnames ".slt.d/init.lisp" home)
                  (default-config-pathname)))))

(defun user-config-pathname ()
  "The init file: *CONFIG-FILE* when set, else the first of
CONFIG-FILE-LOCATIONS that exists, else NIL."
  (if *config-file*
      (uiop:ensure-pathname *config-file* :want-pathname t)
      (find-if #'probe-file (config-file-locations))))

(defun load-user-config (&optional (pathname (user-config-pathname)))
  "Load PATHNAME in the slt package and return it, or NIL when there is no
such file.  An error stops the load but not slt: it is printed, and kept
in *CONFIG-ERROR* to be shown in the terminal."
  (setf *config-error* nil)
  (cond
    ((null pathname) nil)
    ((not (probe-file pathname))
     (when *config-file*
       (setf *config-error* (format nil "~a does not exist"
                                    (uiop:native-namestring pathname))))
     nil)
    (t
     (handler-case
         (let ((*package* (find-package '#:slt)))
           (load pathname))
       (error (condition)
         (setf *config-error* (format nil "error in ~a: ~a"
                                      (uiop:native-namestring pathname) condition))
         (format *error-output* "~&slt: ~a~%" *config-error*)))
     pathname)))

(defparameter *config-template*
  ";;; slt init file, loaded at startup in the slt package.
;;; Reload it with Command+Shift+R (Super+Shift+R elsewhere) or the slt menu.

;; (setf *default-engine* :ghostty)
;; (setf *palette* (make-palette :foreground #xcdd6f4 :background #x1e1e2e))
;; (setf *window-padding* 6)        ; points around the grid (cocoa)
;; (setf *option-as-meta* t)        ; Option sends Meta instead of composing
;; (setf *copy-on-select* t)
;; (setf *audible-bell* nil)
;; (add-hook '*key-hook*            ; return true to consume the key
;;           (lambda (emulator key modifiers)
;;             (when (and (eql key #\d) (member :super modifiers))
;;               (write-input emulator \"date\") t)))
"
  "The contents of a new init file made by EDIT-CONFIG.")

(defun report-message (emulator format-control &rest arguments)
  "Show a message from slt itself in EMULATOR's terminal, in bold red."
  (let ((esc (code-char 27)))
    (with-engine (engine emulator)
      (engine-feed-string engine
                          (format nil "~c[1;31mslt:~c[0m ~?~c~c"
                                  esc esc format-control arguments
                                  #\Return #\Linefeed)))
    (request-render emulator)))

(defun refresh-emulator (emulator)
  "Redraw everything, e.g. after the palette changed."
  (grid-mark-all-dirty (terminal-emulator-grid emulator))
  (render-emulator emulator :force t))

(defun reload-config (emulator)
  "Load the init file again and redraw with whatever it changed."
  (load-user-config)
  (when *config-error*
    (report-message emulator "~a" *config-error*))
  (note-color-scheme emulator)
  (refresh-emulator emulator))

(defun open-external (thing)
  "Open THING (a URL or a file name) with the desktop's default handler."
  (ignore-errors
    (uiop:launch-program (list (if (uiop:os-macosx-p) "open" "xdg-open") thing))))

(defun edit-config (emulator)
  "Open the init file in the default text editor, creating it first."
  (declare (ignore emulator))
  (let ((pathname (or (user-config-pathname) (default-config-pathname))))
    (unless (probe-file pathname)
      (ensure-directories-exist pathname)
      (with-open-file (out pathname :direction :output)
        (write-string *config-template* out)))
    (if (uiop:os-macosx-p)
        (ignore-errors
          (uiop:launch-program (list "open" "-t" (uiop:native-namestring pathname))))
        (open-external (uiop:native-namestring pathname)))))

;;; How the shell starts

(defvar *login-shell* :bundle
  "Whether the shell is started as a login shell: true, false, or :BUNDLE
for only when slt runs as a macOS application (from the Finder or the
Dock, where no login shell has set up the environment).")

(defvar *working-directory* :bundle
  "The shell's working directory: a pathname or string; NIL for slt's own;
or :BUNDLE for the home directory when slt runs as a macOS application
and its own otherwise.")

(defvar *environment-variables* '()
  "An alist of (NAME . VALUE) added to the shell's environment after
slt's own (COLORTERM, TERM_PROGRAM, LANG when unset); a NIL value removes
NAME.")

(defun running-in-app-bundle-p ()
  (and (uiop:os-macosx-p)
       (search ".app/Contents/MacOS/"
               (let ((runtime sb-ext:*runtime-pathname*))
                 (if runtime (namestring runtime) "")))
       t))

(defun resolve-bundle-option (value)
  (if (eq value :bundle) (running-in-app-bundle-p) value))

(defun shell-working-directory (directory)
  (let ((directory (if (eq directory :bundle)
                       (and (running-in-app-bundle-p) (user-homedir-pathname))
                       directory)))
    (and directory (uiop:native-namestring directory))))

(defun send-to-process (emulator string)
  ;; A REPL thread has no process group for the PTY to signal: Control-C
  ;; interrupts it directly.
  (let ((process (terminal-emulator-process emulator)))
    (when (and (repl-process-p process) string (find +interrupt-character+ string))
      (interrupt-repl-process process)
      (setf string (remove +interrupt-character+ string))))
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
    ;; Frontends without a wakeup leave the queue to the poll timer.
    (when first-p
      (frontend-wakeup (terminal-emulator-frontend emulator)
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
  "Render just before the display's next refresh, on frontends that can
tell; elsewhere now, or after *FRAME-INTERVAL-MS* since the previous frame."
  (let* ((now (get-internal-real-time))
         (synchronized (synchronized-wait-ms emulator))
         (wait-ms (max (- *frame-interval-ms*
                          (round (* 1000 (- now (terminal-emulator-last-render-time emulator)))
                                 internal-time-units-per-second))
                       ;; While a program's update is incomplete (mode 2026),
                       ;; look again each frame: its end renders at once.
                       (if synchronized (min synchronized 16) 0))))
    (flet ((render ()
             (if (synchronized-wait-ms emulator)
                 (render-paced emulator)
                 (progn
                   (bt:with-lock-held ((terminal-emulator-ui-lock emulator))
                     (setf (terminal-emulator-render-requested emulator) nil))
                   (setf (terminal-emulator-last-render-time emulator) (get-internal-real-time))
                   (unless (terminal-emulator-closed-p emulator)
                     (render-emulator emulator))))))
      (cond
        ((frontend-request-frame (terminal-emulator-frontend emulator)
                                 (terminal-emulator-view emulator)
                                 #'render))
        ((plusp wait-ms)
         (frontend-schedule (terminal-emulator-frontend emulator)
                            (terminal-emulator-view emulator)
                            wait-ms #'render))
        (t (render))))))

(defun fit-grid-to-size (width height cell-width cell-height)
  (values (max 1 (floor width (max 1 cell-width)))
          (max 1 (floor height (max 1 cell-height)))))

(defun grid-row-fingerprint (grid row)
  (let ((hash 2166136261))
    (loop for column below (grid-columns grid)
          for index = (grid-index grid row column)
          do (setf hash
                   (logand #xffffffff
                           (* 16777619
                              (logxor hash
                                      (aref (grid-codes grid) index)
                                      (aref (grid-fg grid) index)
                                      (aref (grid-bg grid) index)
                                      (aref (grid-attrs grid) index))))))
    hash))

(defun grid-fingerprints (grid)
  (let ((result (make-array (grid-rows grid))))
    (dotimes (row (grid-rows grid) result)
      (setf (aref result row) (grid-row-fingerprint grid row)))))

(defun detect-grid-scroll (before grid)
  "Infer a whole-screen scroll from row fingerprints; positive means up."
  (let* ((rows (grid-rows grid))
         (after (grid-fingerprints grid))
         (best-delta 0)
         ;; A character written into an otherwise repetitive screen must not
         ;; be mistaken for a scroll merely because adjacent blank rows also
         ;; match.  Only accept an offset alignment when it beats the natural
         ;; zero-offset alignment.
         (best-score (loop for row below rows
                           count (= (aref before row) (aref after row)))))
    (when (equalp before after)
      (return-from detect-grid-scroll 0))
    (labels ((consider (delta)
               (let* ((amount (abs delta))
                      (overlap (- rows amount))
                      (matches
                        (loop for row below overlap
                              count (= (if (plusp delta)
                                           (aref before (+ row amount))
                                           (aref before row))
                                       (if (plusp delta)
                                           (aref after row)
                                           (aref after (+ row amount)))))))
                 (when (and (>= matches 2)
                            (>= matches (ceiling (* overlap 3) 4))
                            (> matches best-score))
                   (setf best-score matches
                         best-delta delta)))))
      (loop for amount from 1 below rows
            do (consider amount)
               (consider (- amount))))
    best-delta))

(defun cursor-blinks-p (emulator grid)
  "True when GRID's cursor, as the engine left it, should blink now: it is
shown, the window has the focus and *CURSOR-BLINK* allows it."
  (and (grid-cursor-visible-p grid)
       (terminal-emulator-focused-p emulator)
       (case *cursor-blink*
         ((nil) nil)
         ((t) t)
         (t (eq (grid-cursor-blink grid) t)))))

(defun apply-cursor-blink (emulator grid)
  "Hide the cursor in the hidden half of a blink, and keep the timer that
flips the halves running while it blinks.  UI thread only."
  (if (cursor-blinks-p emulator grid)
      (progn
        (unless (terminal-emulator-cursor-blink-timer emulator)
          (setf (terminal-emulator-cursor-blink-timer emulator)
                (frontend-schedule (terminal-emulator-frontend emulator)
                                   (terminal-emulator-view emulator)
                                   *cursor-blink-interval-ms*
                                   (lambda ()
                                     (setf (terminal-emulator-cursor-blink-timer emulator) nil)
                                     (unless (terminal-emulator-closed-p emulator)
                                       (setf (terminal-emulator-cursor-blink-on emulator)
                                             (not (terminal-emulator-cursor-blink-on emulator)))
                                       (render-emulator emulator))))))
        (unless (terminal-emulator-cursor-blink-on emulator)
          (setf (grid-cursor-visible-p grid) nil
                (sbit (grid-dirty grid) (grid-cursor-row grid)) 1)))
      (setf (terminal-emulator-cursor-blink-on emulator) t)))

(defun restart-cursor-blink (emulator)
  "Show the cursor for a whole blink from now, as after typing."
  (unless (shiftf (terminal-emulator-cursor-blink-on emulator) t)
    (request-render emulator))
  (let ((timer (shiftf (terminal-emulator-cursor-blink-timer emulator) nil)))
    (when timer
      (frontend-cancel-scheduled (terminal-emulator-frontend emulator)
                                 (terminal-emulator-view emulator) timer))))

(defun render-emulator (emulator &key force)
  "Copy the engine's changes into the grid and draw them.  UI thread only;
the engine lock is held only for the copy."
  (let ((grid (terminal-emulator-grid emulator))
        (display nil)
        (tracking nil))
    (step-smooth-snap emulator)
    (with-engine (engine emulator)
      (let* ((store (terminal-emulator-image-store emulator))
             (old-screen (terminal-image-store-active-screen store))
             (reported-p (engine-reports-scrolls-p engine))
             (complete-p (engine-reports-all-scrolls-p engine))
             (same-size-p (and (= (grid-rows grid) (engine-rows engine))
                               (= (grid-columns grid) (engine-columns engine))))
             ;; Scrolls an engine does not report are inferred.
             (fingerprints (and (not complete-p) same-size-p
                                (terminal-image-store-placements store)
                                (grid-fingerprints grid)))
             (force (or force (shiftf (terminal-image-store-force-render-p store) nil))))
        (engine-update-grid engine grid :force force)
        (let* ((screen (if (engine-mode engine :alternate-screen)
                           :alternate :primary))
               (same-screen-p (eq screen old-screen))
               (detected (if (and fingerprints same-screen-p)
                             (detect-grid-scroll fingerprints grid)
                             0))
               (pending (terminal-image-store-pending-scroll store))
               (scroll (- (if (and (plusp pending) (< detected pending)) pending detected)
                          ;; Reported scrolls already moved the placements.
                          (if reported-p (terminal-image-store-reported-scroll store) 0))))
          (unless same-screen-p
            (setf (terminal-image-store-active-screen store) screen
                  (terminal-image-store-dirty-p store) t))
          (when (and (not complete-p) (/= scroll 0)
                     (or (plusp scroll) (zerop (terminal-image-store-reported-scroll store))))
            (scroll-image-placements store scroll))
          ;; Only the live screen shows the cells under SIXEL images.
          (when (and same-screen-p same-size-p (eql 0 (engine-viewport-offset engine)))
            (erase-overwritten-sixel-cells
             store grid
             (frontend-view-cell-width (terminal-emulator-frontend emulator)
                                      (terminal-emulator-view emulator))
             (frontend-view-cell-height (terminal-emulator-frontend emulator)
                                       (terminal-emulator-view emulator))))
          (setf (terminal-image-store-pending-scroll store) 0
                (terminal-image-store-reported-scroll store) 0)))
      (setf tracking (and (engine-mode engine :mouse-tracking) t)))
    (unless (eq tracking (terminal-emulator-mouse-tracking emulator))
      (setf (terminal-emulator-mouse-tracking emulator) tracking)
      (frontend-set-mouse-tracking (terminal-emulator-frontend emulator)
                                  (terminal-emulator-view emulator)
                                  tracking))
    ;; Dirty rows here are the engine's changes, since every render clears
    ;; them; a selection whose text changed no longer means anything.
    (when (grid-selection-intersects-dirty-p grid)
      (clear-selection emulator))
    (apply-cursor-blink emulator grid)
    (with-engine (engine emulator)
      (apply-link-hover emulator engine grid)
      (setf display (smooth-display-grid emulator engine grid)))
    (let ((grid-dirty-p (find 1 (grid-dirty grid))))
      (when grid-dirty-p
        (render-display-grid emulator display)
        (grid-clear-dirty grid))
      (let ((store (terminal-emulator-image-store emulator)))
        (when (or (terminal-image-store-dirty-p store)
                  (and grid-dirty-p (terminal-image-store-placements store)))
          ;; Renderers get a snapshot: the reader thread keeps changing STORE.
          (let ((display (with-engine (engine emulator)
                           (setf (terminal-image-store-dirty-p store) nil)
                           (terminal-image-display-store
                            store grid
                            (frontend-view-cell-width (terminal-emulator-frontend emulator)
                                                     (terminal-emulator-view emulator))
                            (frontend-view-cell-height (terminal-emulator-frontend emulator)
                                                      (terminal-emulator-view emulator))
                            (engine-viewport-offset engine)))))
            (frontend-render-images (terminal-emulator-frontend emulator)
                                   (terminal-emulator-view emulator)
                                   display)))))
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
        (frontend-cancel-scheduled (terminal-emulator-frontend emulator)
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
  (let ((frontend (terminal-emulator-frontend emulator))
        (view (terminal-emulator-view emulator)))
    (values (frontend-view-cell-width frontend view)
            (frontend-view-cell-height frontend view))))

(defun resize-emulator (emulator rows columns)
  (clear-selection emulator)
  (reset-smooth-offset emulator)
  (setf (terminal-emulator-rows emulator) rows
        (terminal-emulator-columns emulator) columns)
  (with-engine (engine emulator)
    (engine-resize engine rows columns))
  (frontend-resize-view (terminal-emulator-frontend emulator)
                       (terminal-emulator-view emulator)
                       rows
                       columns)
  (render-emulator emulator :force t))

(defun apply-resize (emulator)
  (setf (terminal-emulator-resize-after-id emulator) nil)
  (multiple-value-bind (width height)
      (frontend-view-window-size (terminal-emulator-frontend emulator)
                                (terminal-emulator-view emulator))
    (multiple-value-bind (cell-width cell-height) (emulator-cell-pixels emulator)
      (multiple-value-bind (columns rows)
          (fit-grid-to-size width height cell-width cell-height)
        (let* ((pixels (cons (* columns cell-width) (* rows cell-height)))
               (same-pixels-p (equal pixels (terminal-emulator-pty-pixels emulator))))
          (cond ((not (and (= rows (terminal-emulator-rows emulator))
                           (= columns (terminal-emulator-columns emulator))))
                 (resize-emulator emulator rows columns))
                ;; Only the cells changed size: lay the images out again.
                ((not same-pixels-p)
                 (render-emulator emulator :force t)))
          ;; Programs size images by the pixels, so a new cell size alone
          ;; is news to them too.
          (unless same-pixels-p
            (setf (terminal-emulator-pty-pixels emulator) pixels)
            (resize-process-pty (terminal-emulator-process emulator)
                                rows
                                columns
                                :pixel-width (car pixels)
                                :pixel-height (cdr pixels))
            (when (terminal-emulator-in-band-resize emulator)
              (send-to-process emulator
                               (format nil "~c[48;~d;~d;~d;~dt" (code-char 27) rows columns
                                       (cdr pixels) (car pixels))))))))))

(defvar *resize-delay-ms* 16
  "How long a resize waits for the window size to settle.")

(defun schedule-resize (emulator)
  "Apply the window's size shortly.  Requests arriving while one is pending
join it rather than postponing it, so a steady stream of resize events
(a window being dragged) cannot starve it; APPLY-RESIZE reads the size
current when it runs."
  (unless (terminal-emulator-resize-after-id emulator)
    (setf (terminal-emulator-resize-after-id emulator)
          (frontend-schedule (terminal-emulator-frontend emulator)
                            (terminal-emulator-view emulator)
                            *resize-delay-ms*
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
    (reset-smooth-offset emulator)
    (when (with-engine (engine emulator)
            (engine-scroll engine how))
      (request-render emulator)
      t)))

(defun handle-scroll (emulator rows)
  "Mouse wheel: scroll history, or on the alternate screen (vim, less) send
arrow keys so the program scrolls."
  (cond
    ((zerop rows))
    ((with-engine (engine emulator) (engine-mode engine :mouse-tracking))
     (loop repeat (abs rows)
           do (report-mouse emulator :press (if (minusp rows) 4 5)
                            (terminal-emulator-pointer-row emulator)
                            (terminal-emulator-pointer-column emulator)
                            '())))
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

(defun key-identity (key)
  "KEY as the same physical key whatever the modifiers: a lowercase,
unshifted character for characters, so that a release matches its press
when Shift or Control changed in between."
  (if (characterp key)
      (let ((code (char-code key)))
        (cond ((zerop code) #\Space)
              ((<= code 26) (code-char (+ 96 code)))
              ((< code 32) (code-char (+ 64 code)))
              (t (char-downcase (kitty-keyboard:unshifted-key key)))))
      key))

(defun handle-keypress (emulator char keysym state &optional (event :press))
  "A key event from the frontend: EVENT is :PRESS, :REPEAT (held down) or
:RELEASE.  Repeats are presses, and releases and modifier keys are ignored,
unless the child asked the Kitty keyboard protocol for them."
  (let ((key (event-key char keysym))
        (modifiers (decode-modifier-state state)))
    (cond
      ((null key))
      ((or (eq event :release) (modifier-key-p key))
       ;; Only keys whose presses reached the child are released to it.
       (when (or (modifier-key-p key)
                 (gethash (key-identity key) (terminal-emulator-keys-down emulator)))
         (when (eq event :release)
           (remhash (key-identity key) (terminal-emulator-keys-down emulator)))
         (let ((input (bt:with-recursive-lock-held ((terminal-emulator-lock emulator))
                        (kitty-key-input emulator key modifiers event))))
           (unless (eq input :legacy)
             (write-input emulator input)))))
      ((not (run-hook-until-success '*key-hook* emulator key modifiers))
       (let ((input (with-engine (engine emulator)
                      (let ((kitty (kitty-key-input emulator key modifiers event)))
                        (cond ((not (eq kitty :legacy)) kitty)
                              ;; Super (Command) combinations the shortcuts
                              ;; did not take reach only children using the
                              ;; Kitty keyboard protocol: the legacy encoding
                              ;; has no Super.
                              ((member :super modifiers) nil)
                              (t (engine-encode-key engine key modifiers)))))))
         (when input
           (restart-cursor-blink emulator)
           ;; Typing returns the view to the live screen and ends a selection.
           (scroll-viewport emulator :bottom)
           (clear-selection emulator)
           (setf (gethash (key-identity key) (terminal-emulator-keys-down emulator)) t)
           (write-input emulator input)))))))

;;; Selection and the clipboard

(defvar *selection-word-characters* "-_.,/~:@%+#?&="
  "Characters other than letters and digits that a double click selects as
part of a word, so that paths and URLs are selected whole.")

(defvar *copy-on-select* nil
  "When true, finishing a mouse selection copies it to the clipboard.")

(defun render-display-grid (emulator display)
  "Hand DISPLAY (the grid, or the smooth-scrolling one) to the frontend."
  (when (terminal-emulator-smooth-p emulator)
    (frontend-set-smooth-offset (terminal-emulator-frontend emulator)
                                (terminal-emulator-view emulator)
                                (if (eq display (terminal-emulator-grid emulator))
                                    0
                                    (round (terminal-emulator-smooth-offset emulator)))))
  (frontend-render-grid (terminal-emulator-frontend emulator)
                        (terminal-emulator-view emulator)
                        display))

(defun render-selection (emulator)
  "Redraw the rows whose selection changed, without consulting the engine."
  (let ((grid (terminal-emulator-grid emulator)))
    (when (find 1 (grid-dirty grid))
      (if (zerop (terminal-emulator-smooth-offset emulator))
          (render-display-grid emulator grid)
          ;; The smooth-scrolling grid holds the old selection.
          (render-emulator emulator))
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

(defun report-mouse (emulator action button row column modifiers &key (send t))
  "Report a mouse event to the child at the cell ROW, COLUMN, kept inside
the grid, unless SEND is false; in pixels (the pointer's, see HANDLE-MOUSE)
when the child asked for them.  Return true when the child tracks the
mouse."
  (let ((grid (terminal-emulator-grid emulator))
        (tracking nil)
        (report nil))
    (with-engine (engine emulator)
      (setf tracking (engine-mode engine :mouse-tracking))
      (when (and tracking send)
        (setf report
              (if (terminal-emulator-mouse-pixels emulator)
                  (xterm-encode-mouse action button
                                      (terminal-emulator-pointer-y emulator)
                                      (terminal-emulator-pointer-x emulator)
                                      modifiers :tracking tracking :sgr :pixels)
                  (engine-encode-mouse engine action button
                                       (max 0 (min row (1- (grid-rows grid))))
                                       (max 0 (min column (1- (grid-columns grid))))
                                       modifiers)))))
    (write-input emulator report)
    tracking))

(defun note-pointer-pixel (emulator row column x y)
  "Record the pointer's pixel, X and Y from the grid's top left (the middle
of the cell ROW, COLUMN when NIL), kept inside the grid.  Return true when
it changed."
  (multiple-value-bind (cw ch) (emulator-cell-pixels emulator)
    (let* ((grid (terminal-emulator-grid emulator))
           (x (max 0 (min (1- (* (grid-columns grid) cw))
                          (round (or x (+ (* column cw) (floor cw 2)))))))
           (y (max 0 (min (1- (* (grid-rows grid) ch))
                          (round (or y (+ (* row ch) (floor ch 2))))))))
      (prog1 (or (/= x (terminal-emulator-pointer-x emulator))
                 (/= y (terminal-emulator-pointer-y emulator)))
        (setf (terminal-emulator-pointer-x emulator) x
              (terminal-emulator-pointer-y emulator) y)))))

(defun handle-mouse (emulator action row column clicks &optional (button 1) (state 0) x y)
  "Report the event when the child tracks the mouse and Shift is not held;
otherwise select with the primary button: drag for characters,
double-click for words and triple-click for lines.  X and Y are the
pointer's pixels from the grid's top left, which frontends pass when they
know them, for children that ask for reports in pixels."
  (let* ((modifiers (decode-modifier-state state))
         (pixel-moved (note-pointer-pixel emulator row column x y))
         (moved (or (/= row (terminal-emulator-pointer-row emulator))
                    (/= column (terminal-emulator-pointer-column emulator))
                    (and pixel-moved (terminal-emulator-mouse-pixels emulator)))))
    (setf (terminal-emulator-pointer-row emulator) row
          (terminal-emulator-pointer-column emulator) column
          (terminal-emulator-pointer-known-p emulator) t)
    ;; Links under the pointer are underlined.
    (when moved
      (request-render emulator))
    (when (and (eq action :press) (eql button 1) (eql clicks 1)
               (member :super modifiers)
               (open-url-at emulator row column))
      (return-from handle-mouse nil))
    (unless (and (not (member :shift modifiers))
                 (report-mouse emulator (if (eq action :drag) :motion action)
                               (if (eq action :motion) nil button)
                               row column modifiers
                               ;; Motion within the same cell (or pixel, when
                               ;; reported in pixels) is not reported.
                               :send (or moved (not (member action '(:drag :motion))))))
      (when (and (eql button 1) (not (eq action :motion)))
        (select-with-mouse emulator action row column clicks)))))

(defun select-with-mouse (emulator action row column clicks)
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
      (frontend-set-clipboard-text (terminal-emulator-frontend emulator)
                                  (terminal-emulator-view emulator)
                                  text)
      text)))

(defun paste-clipboard (emulator)
  (let ((text (frontend-clipboard-text (terminal-emulator-frontend emulator)
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

;;; Commands: the things a menu or a shortcut does, as functions of the
;;; emulator.

(defun select-all (emulator)
  (let ((grid (terminal-emulator-grid emulator)))
    (grid-set-selection grid 0 (1- (* (grid-rows grid) (grid-columns grid))))
    (render-selection emulator)))

(defun clear-scrollback (emulator)
  "Erase the screen and the history, then ask the shell to redraw its
prompt (Control+L), as Command+K does in Terminal.  Does nothing while a
full-screen program runs."
  (unless (with-engine (engine emulator) (engine-mode engine :alternate-screen))
    (let ((esc (code-char 27)))
      (scroll-viewport emulator :bottom)
      (clear-selection emulator)
      (with-engine (engine emulator)
        (engine-feed-string engine (format nil "~c[H~c[2J~c[3J" esc esc esc)))
      (write-input emulator (string (code-char 12)))
      (request-render emulator))))

(defvar *minimum-font-size* 6)
(defvar *maximum-font-size* 96)

(defun set-font-size (emulator size)
  "Draw with font SIZE, keeping the window's size and refitting the grid."
  (let ((size (max *minimum-font-size* (min *maximum-font-size* size))))
    (when (and (/= size (terminal-emulator-font-size emulator))
               (frontend-set-font-size (terminal-emulator-frontend emulator)
                                      (terminal-emulator-view emulator)
                                      size))
      (setf (terminal-emulator-font-size emulator) size)
      (apply-resize emulator)
      (refresh-emulator emulator))
    size))

(defun increase-font-size (emulator)
  (set-font-size emulator (1+ (terminal-emulator-font-size emulator))))

(defun decrease-font-size (emulator)
  (set-font-size emulator (1- (terminal-emulator-font-size emulator))))

(defun reset-font-size (emulator)
  (set-font-size emulator (terminal-emulator-initial-font-size emulator)))

(defvar *commands*
  '((:copy . copy-selection)
    (:paste . paste-clipboard)
    (:select-all . select-all)
    (:clear-scrollback . clear-scrollback)
    (:bigger . increase-font-size)
    (:smaller . decrease-font-size)
    (:actual-size . reset-font-size)
    (:reload-config . reload-config)
    (:edit-config . edit-config))
  "Commands offered by frontends with application menus (cocoa), as an
alist from a keyword to a function of the emulator.  Change an entry to
change what that menu item does.  Read when the window is created.")

(defun emulator-commands (emulator &optional new-window-function)
  "*COMMANDS* in the form FRONTEND-BIND-COMMANDS takes."
  (append (and new-window-function
               (list (cons :new-window new-window-function)))
          (and emulator (terminal-emulator-new-repl-function emulator)
               (list (cons :new-repl (terminal-emulator-new-repl-function emulator))))
          (loop for (name . function) in *commands*
                collect (cons name (let ((function function))
                                     (lambda () (funcall function emulator)))))))

(defun command-key-handler (emulator key modifiers)
  "Shortcuts for the commands: Super with = or +, -, 0, A and K (Command on
macOS), Super+Shift+R or Control+Shift+R to reload the init file, and
Super+Up/Down or Control+Shift+Up/Down to jump between shell prompts, with
Shift (Meta for Control+Shift) to the prompts of failed commands, and
Super+Shift+O or Control+Shift+O to copy the last command's output (with
shell integration, OSC 133), and Super+Shift+N or Control+Shift+N to open a
Lisp REPL window.  On *KEY-HOOK*; remove it to rebind.  (The
cocoa frontend's menus take these keys first.)"
  (let ((super (member :super modifiers))
        (control-shift (and (member :control modifiers) (member :shift modifiers))))
    (when (and (member key '(:up :down)) (or super control-shift))
      (when (jump-to-prompt emulator key
                            :failed (if super
                                        (member :shift modifiers)
                                        (member :meta modifiers)))
        (request-render emulator))
      (return-from command-key-handler t))
    (when (and (eql key #\O) (or super control-shift))
      (copy-last-command-output emulator)
      (return-from command-key-handler t))
    (when (and (eql key #\N) (or super control-shift)
               (terminal-emulator-new-repl-function emulator))
      (funcall (terminal-emulator-new-repl-function emulator))
      (return-from command-key-handler t)))
  (when (characterp key)
    (let ((super (member :super modifiers)))
      (cond
        ((and super (member key '(#\= #\+))) (increase-font-size emulator) t)
        ((and super (eql key #\-)) (decrease-font-size emulator) t)
        ((and super (eql key #\0)) (reset-font-size emulator) t)
        ((and super (eql key #\a)) (select-all emulator) t)
        ((and super (eql key #\k)) (clear-scrollback emulator) t)
        ((and (or super (member :control modifiers)) (eql key #\R))
         (reload-config emulator) t)))))

(add-hook '*key-hook* 'command-key-handler)

;;; Links

(defvar *url-schemes* '("http" "https" "ftp" "ftps" "sftp" "file" "mailto" "ssh" "git"
                        "gemini" "gopher" "news" "nntp" "irc" "ircs" "xmpp" "magnet" "tel")
  "Schemes of the URLs in plain text that the pointer underlines and
Command-click (Super-click elsewhere) opens.  Text starting www. counts
too, and opens as https.")

(defvar *hyperlink-schemes* '("http" "https" "mailto")
  "Schemes of the OSC 8 hyperlinks Command-click opens.  Their targets are
not shown, so file:, ssh: and the like, which can run programs, are left
out.")

(defun url-cell-p (grid index)
  (let ((code (aref (grid-codes grid) index)))
    (and (> code 32)
         (not (logtest (aref (grid-attrs grid) index) +attr-wide-spacer+))
         (not (find (code-char code) "<>\"'`{}|\\^")))))

(defun url-start (text)
  "Where the first URL in TEXT starts, and whether it is a bare www. one."
  (flet ((boundary-p (position)
           (or (zerop position)
               (not (alphanumericp (char text (1- position)))))))
    (let ((best nil) (www nil))
      (dolist (scheme *url-schemes*)
        (let ((position (search (concatenate 'string scheme ":") text :test #'char-equal)))
          (when (and position (boundary-p position) (or (null best) (< position best)))
            (setf best position))))
      (let ((position (search "www." text :test #'char-equal)))
        (when (and position (boundary-p position)
                   (or (zerop position) (char/= #\. (char text (1- position))))
                   (or (null best) (< position best)))
          (setf best position www t)))
      (values best www))))

(defun url-end (text start)
  "Where the URL starting at START in TEXT ends: punctuation after it is
usually the sentence's, and so is a closing bracket it did not open."
  (let ((end (length text)))
    (flet ((unbalanced-p (open close)
             (> (count close text :start start :end end)
                (count open text :start start :end end))))
      (loop while (> end start)
            do (let ((last (char text (1- end))))
                 (cond ((find last ".,;:!?")
                        (decf end))
                       ((and (char= last #\)) (unbalanced-p #\( #\)))
                        (decf end))
                       ((and (char= last #\]) (unbalanced-p #\[ #\]))
                        (decf end))
                       (t (return))))))
    end))

(defun url-extent (grid row column)
  "The URL shown at ROW, COLUMN of GRID, or NIL; and its cells as a list of
(ROW FIRST-COLUMN LAST-COLUMN), one per row.  A run of URL characters that
reaches the last column goes on at the start of the next row, as a wrapped
URL does."
  (when (and (< -1 row (grid-rows grid)) (< -1 column (grid-columns grid)))
    (let* ((columns (grid-columns grid))
           (total (* columns (grid-rows grid)))
           (index (+ (* row columns) column)))
      (when (url-cell-p grid index)
        (let ((first index)
              (last index))
          (loop while (and (> first 0) (url-cell-p grid (1- first)))
                do (decf first))
          (loop while (and (< last (1- total)) (url-cell-p grid (1+ last)))
                do (incf last))
          ;; The run's text, and the cell each of its characters is in.
          (let* ((cells (make-array 0 :adjustable t :fill-pointer 0))
                 (text (with-output-to-string (out)
                         (loop for cell from first to last
                               do (loop for char across (grid-cell-string grid cell)
                                        do (write-char char out)
                                           (vector-push-extend cell cells))))))
            (multiple-value-bind (start www) (url-start text)
              (when start
                (let* ((end (url-end text start))
                       (url (subseq text start end))
                       (head (if www 4 (1+ (position #\: url)))))
                  (when (and (> (length url) head)
                             (<= (aref cells start) index (aref cells (1- end))))
                    (values (if www (concatenate 'string "https://" url) url)
                            (loop with from = (aref cells start)
                                  with to = (aref cells (1- end))
                                  for r from (floor from columns) to (floor to columns)
                                  collect (list r
                                                (if (= r (floor from columns)) (mod from columns) 0)
                                                (if (= r (floor to columns))
                                                    (mod to columns)
                                                    (1- columns)))))))))))))))

(defun url-at (grid row column)
  "The URL shown at ROW, COLUMN of GRID, or NIL."
  (values (url-extent grid row column)))

(defun open-url-at (emulator row column)
  "Open the link at ROW, COLUMN; return true if there was one."
  (let ((url (or (let ((uri (hyperlink-at emulator row column)))
                    ;; A hyperlink's target is hidden behind its text, so
                    ;; only harmless schemes (*HYPERLINK-SCHEMES*).
                    (and uri (find-if (lambda (scheme)
                                        (and (> (length uri) (length scheme))
                                             (string-equal scheme uri :end2 (length scheme))
                                             (char= #\: (char uri (length scheme)))))
                                      *hyperlink-schemes*)
                         uri))
                  (url-at (terminal-emulator-grid emulator) row column))))
    (when url
      (frontend-open-url (terminal-emulator-frontend emulator)
                         (terminal-emulator-view emulator) url)
      t)))

;;; Focus and the bell

(defun handle-focus (emulator focused)
  "Tell the child about focus changes when it asked (mode 1004)."
  (setf (terminal-emulator-focused-p emulator) (and focused t))
  ;; The cursor blinks only in the focused window.
  (restart-cursor-blink emulator)
  (request-render emulator)
  (when (with-engine (engine emulator) (engine-mode engine :focus-events))
    (write-input emulator (format nil "~c[~c" (code-char 27) (if focused #\I #\O)))))

(defvar *audible-bell* t
  "When true, the bell beeps; otherwise frontends that can only flash or
ask for attention.")

(defun ring-bell (emulator)
  "The default bell: beep, or bounce the Dock icon when slt is in the
background (cocoa).  On *BELL-HOOK*."
  (frontend-bell (terminal-emulator-frontend emulator)
                (terminal-emulator-view emulator)
                *audible-bell*))

(add-hook '*bell-hook* 'ring-bell)

;;; Files dropped on the window are pasted as shell words.

(defun shell-quote (string)
  (if (and (plusp (length string))
           (every (lambda (char)
                    (or (alphanumericp char) (find char "-_./+@%:,=")))
                  string))
      string
      (with-output-to-string (out)
        (write-char #\' out)
        (loop for char across string
              do (if (char= char #\')
                     (write-string "'\\''" out)
                     (write-char char out)))
        (write-char #\' out))))

(defun drop-files (emulator pathnames)
  (paste-text emulator (format nil "~{~a~^ ~} " (mapcar #'shell-quote pathnames))))

;;; The menu

(defvar *menu-items* '()
  "Items of the Lisp menu, on frontends that have one (cocoa): a list of
(LABEL FUNCTION).  LABEL is a string or a function of no arguments
returning one, consulted each time the menu opens; FUNCTION is called with
the emulator when the item is chosen.  Read when the window is created.")

(defun emulator-menu-items (emulator)
  "*MENU-ITEMS* in the form FRONTEND-BIND-MENU takes."
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

(defun install-image-scroll-hook (emulator)
  "Let EMULATOR's engine move image placements as it scrolls."
  (let ((engine (terminal-emulator-engine emulator))
        (store (terminal-emulator-image-store emulator)))
    (setf (engine-scroll-function engine)
          (lambda (top bottom count &optional screen)
            ;; Runs inside the engine's parser: never unwind it.
            (graphics-guard ("moving images")
              (let ((screen (or screen (if (engine-mode engine :alternate-screen)
                                           :alternate :primary))))
                (note-engine-scroll store top bottom count screen)
                (note-annotation-scroll emulator top bottom count screen
                                        (terminal-image-store-rows store))))))))

(defun feed-emulator-output (emulator octets &key (start 0) (end (length octets)))
  "Feed child output through the control-sequence interceptor.  The caller
must hold EMULATOR's engine lock."
  (let* ((engine (terminal-emulator-engine emulator))
         (store (terminal-emulator-image-store emulator))
         (frontend (terminal-emulator-frontend emulator))
         (view (terminal-emulator-view emulator))
         (parser (terminal-emulator-stream-parser emulator)))
    (unless (engine-scroll-function engine)
      (install-image-scroll-hook emulator))
    (setf (terminal-stream-parser-erase-events-p parser)
          (and (find-if #'sixel-placement-p (terminal-image-store-placements store)) t))
    (setf (terminal-image-store-cell-width store) (max 1 (frontend-view-cell-width frontend view))
          (terminal-image-store-cell-height store) (max 1 (frontend-view-cell-height frontend view))
          (terminal-image-store-rows store) (max 1 (engine-rows engine))
          (terminal-image-store-columns store) (max 1 (engine-columns engine)))
    (terminal-stream-feed
     parser octets
     (lambda (ordinary ordinary-start ordinary-end)
       (engine-feed engine ordinary :start ordinary-start :end ordinary-end))
     (lambda (event)
       (graphics-guard ("handling a control sequence")
         (handle-stream-event emulator event)))
     :start start :end end)))

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
                 (bt:with-recursive-lock-held ((terminal-emulator-lock emulator))
                   (feed-emulator-output emulator buffer :end count))
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
    (frontend-request-exit (terminal-emulator-frontend emulator)
                          (terminal-emulator-view emulator))))

(defun schedule-poll (emulator)
  "Periodic UI-thread work: run queued work for frontends without a wakeup,
and notice a child that exited without closing the PTY."
  (labels ((poll ()
             (unless (terminal-emulator-closed-p emulator)
               (drain-ui-queue emulator)
               (when (bt:with-recursive-lock-held ((terminal-emulator-lock emulator))
                       (advance-terminal-image-animations
                        (terminal-emulator-image-store emulator)))
                 (request-render emulator))
               (let ((process (terminal-emulator-process emulator)))
                 (when (and process (not (process-alive-p process))
                            (not (and (terminal-emulator-reader-thread emulator)
                                      (bt:thread-alive-p
                                       (terminal-emulator-reader-thread emulator)))))
                   (finish-emulator emulator))))
             (unless (terminal-emulator-closed-p emulator)
               (frontend-schedule (terminal-emulator-frontend emulator)
                                 (terminal-emulator-view emulator)
                                 (terminal-emulator-poll-interval emulator)
                                 #'poll))))
    (frontend-schedule (terminal-emulator-frontend emulator)
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
                                        (frontend-set-title
                                         (terminal-emulator-frontend emulator)
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
                          (frontend (or (uiop:getenv "SLT_FRONTEND")
                                       (default-frontend)))
                          (engine (or (uiop:getenv "SLT_ENGINE")
                                      (default-engine)))
                          (rows 24)
                          (columns 80)
                          cell-width
                          cell-height
                          font-family
                          (font-size 15)
                          (shell (user-login-shell))
                          (term-name (or (uiop:getenv "SLT_TERM")
                                         "xterm-256color"))
                          (poll-interval 16)
                          (title "slt")
                          (login-shell *login-shell*)
                          (directory *working-directory*)
                          (load-config *load-user-config*)
                          repl)
  "Open a terminal window running SHELL, or with REPL a Lisp REPL in this
image (see OPEN-REPL), and run the frontend's event loop."
  (when load-config
    (load-user-config))
  (let ((frontend-instance (make-frontend frontend)))
    (frontend-call-with-event-loop
     frontend-instance
     (lambda ()
       (labels ((create-terminal (&key from-directory repl)
                  (let* ((emulator-cell (list nil))
                         (resolved-font-family
                           (resolve-font-family
                            font-family
                            (frontend-available-font-families frontend-instance)))
                         (resolved-font-size
                           (frontend-resolve-font-size frontend-instance font-size))
                         (engine-instance
                           (make-emulator-engine engine rows columns emulator-cell))
                         (view (frontend-create-view frontend-instance
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
                              (if repl
                                  (launch-repl-process
                                   :rows rows :columns columns
                                   :pixel-width (* columns (frontend-view-cell-width
                                                            frontend-instance view))
                                   :pixel-height (* rows (frontend-view-cell-height
                                                          frontend-instance view)))
                              (launch-shell-process
                               shell
                               :term-name term-name
                               :rows rows
                               :columns columns
                               :login (resolve-bundle-option login-shell)
                               :directory (or from-directory
                                              (shell-working-directory directory))
                               :variables (append (default-environment-variables)
                                                  *environment-variables*)
                               :pixel-width (* columns
                                               (frontend-view-cell-width
                                                frontend-instance view))
                               :pixel-height (* rows
                                                (frontend-view-cell-height
                                                 frontend-instance view)))))
                      (error (condition)
                        (engine-feed-string
                         engine-instance
                         (format nil "Unable to start shell PTY: ~a~c~c~c~cRunning in local echo mode.~c~c"
                                 condition #\Return #\Linefeed #\Return #\Linefeed
                                 #\Return #\Linefeed))))
                    (setf emulator (%make-terminal-emulator
                                    :engine engine-instance
                                    :grid (make-grid rows columns)
                                    :frontend frontend-instance
                                    :view view
                                    :rows rows
                                    :columns columns
                                    :process process
                                    :fd (and process
                                             (set-fd-nonblocking (process-fd process)))
                                    :poll-interval poll-interval
                                    :title title
                                    :font-size (abs resolved-font-size)
                                    :initial-font-size (abs resolved-font-size))
                          (car emulator-cell) emulator
                          (terminal-emulator-new-repl-function emulator)
                          (lambda () (create-terminal :repl t)))
                    (when (and load-config *config-error*)
                      (report-message emulator "~a" *config-error*))
                    (render-emulator emulator :force t)
                    (frontend-bind-resize
                     frontend-instance view
                     ;; Frontends pass IMMEDIATE while the user drags a
                     ;; window edge, when timers may not run.
                     (lambda (&optional immediate)
                       (if immediate
                           (apply-resize emulator)
                           (schedule-resize emulator))))
                    (frontend-bind-scroll frontend-instance view
                                         (lambda (rows)
                                           (handle-scroll emulator rows)))
                    (when (smooth-scrolling-requested-p)
                      (setf (terminal-emulator-smooth-p emulator)
                            (and (frontend-bind-pixel-scroll
                                  frontend-instance view
                                  (lambda (pixels ending-p)
                                    (handle-pixel-scroll emulator pixels ending-p)))
                                 t)))
                    (frontend-bind-keypress frontend-instance view
                                           (lambda (char keysym state &optional (event :press))
                                             (handle-keypress emulator char keysym state
                                                              event)))
                    (frontend-bind-mouse
                     frontend-instance view
                     (lambda (action row column clicks &optional (button 1) (state 0) x y)
                       (handle-mouse emulator action row column clicks button state x y)))
                    (frontend-bind-paste frontend-instance view
                                        (lambda (text) (paste-text emulator text)))
                    (frontend-bind-focus frontend-instance view
                                        (lambda (focused)
                                          (handle-focus emulator focused)))
                    (frontend-bind-drop frontend-instance view
                                       (lambda (pathnames)
                                         (drop-files emulator pathnames)))
                    (frontend-bind-commands
                     frontend-instance view
                     (emulator-commands
                      emulator
                      ;; New windows start where this one's shell is (OSC 7).
                      (lambda ()
                        (create-terminal
                         :from-directory
                         (let* ((directory (terminal-emulator-working-directory emulator))
                                (pathname (and directory
                                               (ignore-errors
                                                (uiop:parse-native-namestring
                                                 directory :ensure-directory t)))))
                           (and pathname (uiop:directory-exists-p pathname)
                                (uiop:native-namestring pathname)))))))
                    (frontend-bind-menu frontend-instance view
                                       (emulator-menu-items emulator))
                    (frontend-on-close
                     frontend-instance view
                     (lambda ()
                       (destroy-emulator emulator)
                       (frontend-request-exit frontend-instance view)))
                    (frontend-focus frontend-instance view)
                    (run-hook '*after-init-hook* emulator)
                    (when (terminal-emulator-fd emulator)
                      (start-reader-thread emulator))
                    (schedule-poll emulator)
                    emulator)))
         (create-terminal :repl repl))))))
