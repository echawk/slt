(in-package #:slt)

;;;; What slt does with the control functions the interceptor takes out of
;;;; child output (src/sequences.lisp, doc/escape-sequences.md): graphics,
;;;; OSC strings (colors, clipboard, hyperlinks, working directory, prompt
;;;; marks, notifications), the Kitty keyboard protocol, synchronized
;;;; output and a few queries.  HANDLE-STREAM-EVENT runs on the reader
;;;; thread with the engine lock held; work for the UI thread goes through
;;;; INVOKE-ON-UI.

(defvar *clipboard-access* :write
  "What programs may do with the clipboard through OSC 52: NIL nothing,
:WRITE set it, :READ-WRITE also read it.  Reading lets any program you run,
including over SSH, see whatever you last copied.")

(defvar *clipboard-limit* (* 1024 1024)
  "Most characters OSC 52 may put on the clipboard.")

(defvar *notifications* :unfocused
  "When desktop notifications from programs (OSC 9, 777, 99) are shown:
:ALWAYS, :UNFOCUSED (while the window lacks focus) or NIL (never).")

(defvar *synchronized-output-timeout-ms* 150
  "Longest a program may hold back screen updates with mode 2026.")

(defparameter *hyperlink-limit* 4096
  "Most hyperlink spans remembered (OSC 8).")

(defparameter *prompt-mark-limit* 2048
  "Most prompt positions remembered (OSC 133).")

(defstruct link-span
  "Cells showing hyperlinked text: WIDTH cells from COLUMN of ROW on SCREEN,
which held TEXT when written.  ROW is a stored row (see STORED-ROW)."
  (row 0 :type integer)
  (column 0 :type integer)
  (width 0 :type integer)
  (uri "" :type string)
  (text "" :type string)
  (screen :primary :type keyword))

(defun stored-row (emulator row screen)
  "How a screen ROW is kept: primary-screen rows count from the start of the
session, so that scrolling them into the scrollback changes nothing."
  (if (eq screen :primary) (+ row (terminal-emulator-row-origin emulator)) row))

(defun screen-row (emulator stored screen)
  "The screen row of a STORED row; negative rows are in the scrollback."
  (if (eq screen :primary) (- stored (terminal-emulator-row-origin emulator)) stored))

(defstruct command-mark
  "A command run at a shell prompt, from OSC 133 marks: the stored rows
(see STORED-ROW) of its prompt and of the first and last lines of its
output, and its exit status."
  prompt output last status)

(defun prompt-mark-rows (emulator &key failed)
  "The screen rows of the remembered prompts, newest first: only those of
commands that failed, with FAILED."
  (loop for mark in (terminal-emulator-prompt-marks emulator)
        when (or (not failed)
                 (let ((status (command-mark-status mark)))
                   (and status (/= status 0))))
          collect (screen-row emulator (command-mark-prompt mark) :primary)))

(defun stream-screen (engine)
  (if (engine-mode engine :alternate-screen) :alternate :primary))

(defun reply-to-child (emulator reply)
  (when (and reply (plusp (length reply)))
    (send-to-process emulator reply)))

(defun handle-stream-event (emulator event)
  "Act on an event from the interceptor (see CLASSIFY-STREAM-EVENT)."
  (let ((engine (terminal-emulator-engine emulator))
        (store (terminal-emulator-image-store emulator))
        (parser (terminal-emulator-stream-parser emulator)))
    (destructuring-bind (kind &rest arguments) event
      (case kind
        ;; Images are decoded with the engine lock released: a large one
        ;; would otherwise hold up rendering and typing.  Decoding touches
        ;; neither the engine nor what the renderer reads.
        (:sixel
         (graphics-guard ("drawing a SIXEL image")
           (let ((decoded (without-engine-lock (emulator)
                            (decode-sixel-payload (first arguments)))))
             (handle-sixel-command store (first arguments) engine :decoded decoded)))
         ;; Erasures later in this same input may now hit the image.
         (setf (terminal-stream-parser-erase-events-p parser) t))
        (:kitty
         (reply-to-child emulator
                         (graphics-guard ("handling a Kitty graphics command")
                           (multiple-value-bind (command reply decoded)
                               (without-engine-lock (emulator)
                                 (prepare-kitty-command store (first arguments)))
                             (if command
                                 (run-kitty-command store command engine decoded)
                                 reply)))))
        (:osc
         (graphics-guard ("handling an OSC string")
           (destructuring-bind (number text terminator) arguments
             (handle-osc emulator number text terminator))))
        (:keyboard
         (multiple-value-bind (handled reply)
             (kitty-keyboard:keyboard-control-sequence
              (terminal-emulator-keyboard emulator) (stream-screen engine)
              (first arguments) (second arguments))
           (declare (ignore handled))
           (reply-to-child emulator reply)))
        (:version
         (reply-to-child emulator (format nil "~cP>|slt ~a~c\\" (code-char 27)
                                          (or (system-version) "") (code-char 27))))
        (:text (note-hyperlink-text emulator (first arguments) (second arguments)
                                    (third arguments)))
        (:mode
         (destructuring-bind (mode value) arguments
           (case mode
             (2026 (synchronize-output emulator value))
             (1016 (setf (terminal-emulator-mouse-pixels emulator) value))
             (2031 (setf (terminal-emulator-color-scheme-updates emulator) value))
             (2048 (setf (terminal-emulator-in-band-resize emulator) value)
              ;; Enabling it reports the size at once.
              (when value
                (reply-to-child emulator (in-band-resize-report emulator))))
             (t (handle-graphics-control store event engine)))))
        (:report-mode
         (reply-to-child
          emulator
          (flet ((report (mode set)
                   (format nil "~c[?~d;~d$y" (code-char 27) mode (if set 1 2))))
            (case (first arguments)
              (2026 (report 2026 (terminal-emulator-sync-deadline emulator)))
              (1016 (report 1016 (terminal-emulator-mouse-pixels emulator)))
              (2031 (report 2031 (terminal-emulator-color-scheme-updates emulator)))
              (2048 (report 2048 (terminal-emulator-in-band-resize emulator)))
              (t (handle-graphics-control store event engine))))))
        (:window-report
         (reply-to-child emulator (window-report emulator (first arguments))))
        (:color-scheme-query
         (reply-to-child emulator (color-scheme-report emulator)))
        (:xtgettcap
         (reply-to-child emulator (xtgettcap-reply (first arguments))))
        (:decrqss
         (let* ((request (first arguments))
                (value (engine-report-setting engine request)))
           (if (eq value :pass)
               ;; The engine may answer it itself.
               (engine-feed-string engine (format nil "~cP$q~a~c\\" (code-char 27) request
                                                  (code-char 27)))
               (reply-to-child emulator (decrqss-reply request value)))))
        (:enter-alternate-screen
         (kitty-keyboard:keyboard-reset (terminal-emulator-keyboard emulator) :alternate)
         (forget-links emulator :alternate)
         (handle-graphics-control store event engine))
        (:erase-display
         (when (member (first arguments) '(2 3))
           (forget-links emulator (stream-screen engine)
                         (if (= (first arguments) 3) :scrollback :screen)))
         (handle-graphics-control store event engine))
        (:reset
         (kitty-keyboard:keyboard-reset (terminal-emulator-keyboard emulator))
         (synchronize-output emulator nil)
         (setf (terminal-emulator-mouse-pixels emulator) nil
               (terminal-emulator-color-scheme-updates emulator) nil
               (terminal-emulator-in-band-resize emulator) nil
               (terminal-emulator-hyperlink emulator) nil
               (terminal-stream-parser-text-events-p parser) nil
               (terminal-emulator-links emulator) '()
               (terminal-emulator-link-count emulator) 0
               (terminal-emulator-prompt-marks emulator) '())
         (reset-colors emulator :all)
         (handle-graphics-control store event engine))
        (t
         (reply-to-child emulator (handle-graphics-control store event engine)))))))

;;; Queries about the window, the colors and the terminal's capabilities

(defun window-report (emulator what)
  "The XTWINOPS report WHAT asks for: 14 the text area and 16 a cell in
pixels, 18 and 19 the text area in cells."
  (let* ((engine (terminal-emulator-engine emulator))
         (store (terminal-emulator-image-store emulator))
         (rows (engine-rows engine))
         (columns (engine-columns engine))
         (cw (terminal-image-store-cell-width store))
         (ch (terminal-image-store-cell-height store))
         (esc (code-char 27)))
    (case what
      (14 (format nil "~c[4;~d;~dt" esc (* rows ch) (* columns cw)))
      (16 (format nil "~c[6;~d;~dt" esc ch cw))
      (18 (format nil "~c[8;~d;~dt" esc rows columns))
      (19 (format nil "~c[9;~d;~dt" esc rows columns)))))

(defun in-band-resize-report (emulator)
  "The size report of in-band resize notifications (mode 2048): rows and
columns, then the text area in pixels."
  (let* ((engine (terminal-emulator-engine emulator))
         (store (terminal-emulator-image-store emulator))
         (rows (engine-rows engine))
         (columns (engine-columns engine)))
    (format nil "~c[48;~d;~d;~d;~dt" (code-char 27) rows columns
            (* rows (terminal-image-store-cell-height store))
            (* columns (terminal-image-store-cell-width store)))))

(defun dark-color-p (rgb)
  "True when RGB is dark: its relative luminance is under one half."
  (< (+ (* 0.2126 (ldb (byte 8 16) rgb))
        (* 0.7152 (ldb (byte 8 8) rgb))
        (* 0.0722 (ldb (byte 8 0) rgb)))
     128))

(defun color-scheme (emulator)
  "EMULATOR's color scheme, :DARK or :LIGHT, by its background."
  (if (dark-color-p (palette-background (emulator-palette emulator))) :dark :light))

(defun color-scheme-report (emulator)
  "The answer to CSI ? 996 n (mode 2031): CSI ? 997 ; 1 n when dark, 2 when
light."
  (format nil "~c[?997;~dn" (code-char 27) (if (eq (color-scheme emulator) :dark) 1 2)))

(defun note-color-scheme (emulator)
  "Tell a child that asked (mode 2031) when the scheme changed."
  (let ((scheme (color-scheme emulator)))
    (unless (eq scheme (shiftf (terminal-emulator-last-color-scheme emulator) scheme))
      (when (terminal-emulator-color-scheme-updates emulator)
        (reply-to-child emulator (color-scheme-report emulator))))))

(defparameter *terminal-capabilities*
  (let ((esc (string (code-char 27))))
    (flet ((cap (control) (concatenate 'string esc control)))
      `(("TN" . "slt")
        ("Co" . "256")
        ("colors" . "256")
        ("RGB" . "8/8/8")
        ("Tc" . t)
        ("Su" . t)
        ("Smulx" . ,(cap "[4:%p1%dm"))
        ("Setulc" . ,(cap "[58:2::%p1%{65536}%/%d:%p1%{256}%/%{255}%&%d:%p1%{255}%&%d%;m"))
        ("setrgbf" . ,(cap "[38:2::%p1%d:%p2%d:%p3%dm"))
        ("setrgbb" . ,(cap "[48:2::%p1%d:%p2%d:%p3%dm"))
        ("Ms" . ,(concatenate 'string esc "]52;%p1%s;%p2%s" (string (code-char 7))))
        ("Ss" . ,(cap "[%p1%d q"))
        ("Se" . ,(cap "[2 q"))
        ("Sync" . ,(cap "[?2026%?%p1%{1}%-%tl%eh%;"))
        ("BE" . ,(cap "[?2004h"))
        ("BD" . ,(cap "[?2004l"))
        ("PS" . ,(cap "[200~"))
        ("PE" . ,(cap "[201~")))))
  "What XTGETTCAP answers: terminfo names and values (T for booleans).")

(defun hex-string (string)
  (format nil "~{~2,'0X~}" (map 'list #'char-code string)))

(defun unhex-string (hex)
  (ignore-errors
   (coerce (loop for i from 0 below (1- (length hex)) by 2
                 collect (code-char (parse-integer hex :start i :end (+ i 2) :radix 16)))
           'string)))

(defun xtgettcap-reply (request)
  "Answers to XTGETTCAP: DCS 1 + r NAME=VALUE ST for each hex-encoded name
of REQUEST that slt knows (NAME alone for booleans), DCS 0 + r NAME ST for
the others."
  (let ((esc (code-char 27)))
    (with-output-to-string (out)
      (dolist (hex (uiop:split-string request :separator ";"))
        (let* ((name (unhex-string hex))
               (value (and name (cdr (assoc name *terminal-capabilities* :test #'string=)))))
          (cond ((null value) (format out "~cP0+r~a~c\\" esc hex esc))
                ((eq value t) (format out "~cP1+r~a~c\\" esc hex esc))
                (t (format out "~cP1+r~a=~a~c\\" esc hex (hex-string value) esc))))))))

(defun decrqss-reply (request value)
  "DCS 1 $ r VALUE REQUEST ST for a setting the engine reported, else
DCS 0 $ r ST."
  (let ((esc (code-char 27)))
    (if value
        (format nil "~cP1$r~a~a~c\\" esc value request esc)
        (format nil "~cP0$r~c\\" esc esc))))

;;; Synchronized output (mode 2026)

(defun synchronize-output (emulator on)
  "Hold back renders while ON, for at most *SYNCHRONIZED-OUTPUT-TIMEOUT-MS*
(repeating ?2026h does not extend it)."
  (if on
      (unless (terminal-emulator-sync-deadline emulator)
        (setf (terminal-emulator-sync-deadline emulator)
              (+ (get-internal-real-time)
                 (round (* *synchronized-output-timeout-ms* internal-time-units-per-second)
                        1000))))
      (progn (setf (terminal-emulator-sync-deadline emulator) nil)
             (request-render emulator))))

(defun synchronized-wait-ms (emulator)
  "Milliseconds renders must still wait for synchronized output, or NIL."
  (let ((deadline (terminal-emulator-sync-deadline emulator)))
    (when deadline
      (let ((left (round (* 1000 (- deadline (get-internal-real-time)))
                         internal-time-units-per-second)))
        (if (plusp left)
            left
            (progn (setf (terminal-emulator-sync-deadline emulator) nil) nil))))))

;;; OSC

(defun handle-osc (emulator number text terminator)
  (case number
    ((4 10 11 12 17 19 104 110 111 112 117 119)
     (handle-color-osc emulator number text terminator))
    (52 (handle-clipboard-osc emulator text terminator))
    (7 (multiple-value-bind (path host) (terminal-osc:parse-working-directory text)
         ;; A remote shell's directories are not ours to open.
         (when (and path (or (string= host "") (string= host "localhost")
                             (string-equal host (machine-instance))))
           (setf (terminal-emulator-working-directory emulator) path))))
    (8 (multiple-value-bind (uri) (terminal-osc:parse-hyperlink text)
         (let ((uri (and uri (<= (length uri) 2048) uri)))
           (setf (terminal-emulator-hyperlink emulator) uri
                 (terminal-stream-parser-text-events-p (terminal-emulator-stream-parser emulator))
                 (and uri t)))))
    (133 (multiple-value-bind (kind status) (terminal-osc:parse-semantic-prompt text)
           (case kind
             (:prompt-start (note-prompt emulator))
             (:command-start (note-command-output emulator :start))
             (:command-end (note-command-output emulator :end status)))))
    ((9 777) (let ((notification (terminal-osc:parse-notification number text)))
               (cond ((getf notification :progress)
                      (let ((state (getf notification :progress))
                            (value (getf notification :value)))
                        (invoke-on-ui emulator
                                      (lambda ()
                                        (frontend-set-progress
                                         (terminal-emulator-frontend emulator)
                                         (terminal-emulator-view emulator)
                                         state value)))))
                     ((getf notification :body)
                      (notify emulator (getf notification :title)
                              (getf notification :body))))))
    (99 (let ((result (terminal-osc:notification-assembler-feed
                       (terminal-emulator-notifications emulator) text)))
          (case (first result)
            (:query
             ;; The id is echoed into the child's input: only the
             ;; protocol's identifier characters, never controls.
             (when (every (lambda (char) (or (alphanumericp char) (find char "_+.-")))
                          (second result))
               (reply-to-child
                emulator
                (terminal-osc:encode-osc
                 99 (format nil "i=~a:p=?;p=title,body:o=always,unfocused"
                            (second result))
                 terminator))))
            ((:close nil))
            (t (notify emulator (getf result :title) (getf result :body)
                       :occasion (getf result :occasion))))))))

;;; Colors (OSC 4, 10-19, 104, 110-119)

(defun emulator-palette (emulator)
  (or (terminal-emulator-palette emulator) *palette*))

(defun palette-rgb-channels (rgb)
  (values (ldb (byte 8 16) rgb) (ldb (byte 8 8) rgb) (ldb (byte 8 0) rgb)))

(defun set-emulator-palette (emulator palette)
  "Draw EMULATOR with PALETTE (NIL for the default) from now on."
  (setf (terminal-emulator-palette emulator) palette)
  (set-image-store-palette (terminal-emulator-image-store emulator) palette)
  (note-color-scheme emulator)
  (invoke-on-ui emulator
                (lambda ()
                  (setf (view-palette (terminal-emulator-view emulator)) palette)
                  (render-emulator emulator :force t))))

(defun modified-palette (emulator function)
  "A copy of EMULATOR's palette changed by FUNCTION, in use from now on."
  (let* ((old (emulator-palette emulator))
         (new (copy-palette old)))
    (setf (palette-colors new) (copy-seq (palette-colors old)))
    (funcall function new)
    (set-emulator-palette emulator new)
    new))

(defun dynamic-color (palette number)
  (case number
    (10 (palette-foreground palette))
    (11 (palette-background palette))
    (12 (palette-cursor palette))
    (17 (palette-selection palette))
    (t (palette-foreground palette))))

(defun handle-color-osc (emulator number text terminator)
  (flet ((reply (osc-number payload)
           (reply-to-child emulator (terminal-osc:encode-osc osc-number payload terminator)))
         (spec (rgb) (multiple-value-call #'terminal-osc:format-color-spec
                       (palette-rgb-channels rgb))))
    (case number
      (4
       (let ((sets '()))
         (loop for (index . value) in (terminal-osc:parse-palette-request text)
               do (if (string= value "?")
                      (reply 4 (format nil "~d;~a" index
                                       (spec (aref (palette-colors (emulator-palette emulator))
                                                   index))))
                      (multiple-value-bind (r g b) (terminal-osc:parse-color-spec value)
                        (when r (push (cons index (rgb-value r g b)) sets)))))
         (when sets
           (modified-palette emulator
                             (lambda (palette)
                               (loop for (index . rgb) in sets
                                     do (setf (aref (palette-colors palette) index) rgb)))))))
      ((10 11 12 17 19)
       (let ((sets '()))
         (loop for (n . value) in (terminal-osc:parse-dynamic-color-request number text)
               do (if (string= value "?")
                      (reply n (spec (dynamic-color (emulator-palette emulator) n)))
                      (multiple-value-bind (r g b) (terminal-osc:parse-color-spec value)
                        (when r (push (cons n (rgb-value r g b)) sets)))))
         (when sets
           (modified-palette emulator
                             (lambda (palette)
                               (loop for (n . rgb) in sets
                                     do (case n
                                          (10 (setf (palette-foreground palette) rgb))
                                          (11 (setf (palette-background palette) rgb))
                                          (12 (setf (palette-cursor palette) rgb))
                                          (17 (setf (palette-selection palette) rgb)))))))))
      (104 (reset-colors emulator (terminal-osc:parse-palette-reset text)))
      ((110 111 112 117 119) (reset-colors emulator (- number 100))))))

(defun rgb-value (red green blue)
  (logior (ash red 16) (ash green 8) blue))

(defun reset-colors (emulator what)
  "Undo color changes: WHAT is :ALL (every color), a list of palette
indices, or a dynamic color number (10, 11, 12, 17)."
  (let ((default *palette*)
        (current (emulator-palette emulator)))
    (unless (eq current default)
      (if (eq what :all)
          (set-emulator-palette emulator nil)
          (modified-palette emulator
                            (lambda (palette)
                              (if (listp what)
                                  (dolist (index what)
                                    (setf (aref (palette-colors palette) index)
                                          (aref (palette-colors default) index)))
                                  (case what
                                    (10 (setf (palette-foreground palette)
                                              (palette-foreground default)))
                                    (11 (setf (palette-background palette)
                                              (palette-background default)))
                                    (12 (setf (palette-cursor palette)
                                              (palette-cursor default)))
                                    (17 (setf (palette-selection palette)
                                              (palette-selection default)))))))))))

;;; Clipboard (OSC 52)

(defun handle-clipboard-osc (emulator text terminator)
  ;; Base64 is 4/3 the size of its text: refuse oversized data undecoded.
  (when (> (length text) (+ 16 (ceiling (* 4 *clipboard-limit*) 3)))
    (return-from handle-clipboard-osc))
  (multiple-value-bind (selections data) (terminal-osc:parse-clipboard-request text)
    (when selections
      (let ((frontend (terminal-emulator-frontend emulator))
            (view (terminal-emulator-view emulator)))
        (cond
          ((null *clipboard-access*))
          ((eq data :query)
           (when (eq *clipboard-access* :read-write)
             (invoke-on-ui emulator
                           (lambda ()
                             (reply-to-child
                              emulator
                              (terminal-osc:encode-clipboard-reply
                               selections (or (frontend-clipboard-text frontend view) "")
                               terminator))))))
          ((stringp data)
           (when (<= (length data) *clipboard-limit*)
             (invoke-on-ui emulator
                           (lambda () (frontend-set-clipboard-text frontend view data)))))
          ((eq data :clear)
           (invoke-on-ui emulator
                         (lambda () (frontend-set-clipboard-text frontend view "")))))))))

;;; Notifications (OSC 9, 777, 99)

(defun notify (emulator title body &key (occasion :always))
  "Show a desktop notification, as *NOTIFICATIONS*, OCCASION and a rate
limit allow."
  (let ((focused (eq t (terminal-emulator-focused-p emulator)))
        (now (get-internal-real-time)))
    (when (and *notifications*
               (not (and focused (or (eq *notifications* :unfocused)
                                     (member occasion '(:unfocused :invisible))))))
      ;; At most five in ten seconds.
      (let ((recent (remove-if (lambda (time)
                                 (> (- now time) (* 10 internal-time-units-per-second)))
                               (terminal-emulator-recent-notifications emulator))))
        (when (< (length recent) 5)
          (push now recent)
          (invoke-on-ui emulator
                        (lambda ()
                          (frontend-notify (terminal-emulator-frontend emulator)
                                          (terminal-emulator-view emulator)
                                          (and title (subseq title 0 (min 256 (length title))))
                                          (and body (subseq body 0 (min 1024 (length body))))))))
        (setf (terminal-emulator-recent-notifications emulator) recent)))))

;;; Prompt marks (OSC 133)

(defun note-prompt (emulator)
  "Remember the row of a prompt starting at the cursor."
  (let ((engine (terminal-emulator-engine emulator)))
    (when (eq :primary (stream-screen engine))
      (let ((row (stored-row emulator (engine-cursor-position engine) :primary))
            (marks (terminal-emulator-prompt-marks emulator)))
        (unless (and marks (eql row (command-mark-prompt (first marks))))
          (push (make-command-mark :prompt row) marks)
          (when (> (length marks) *prompt-mark-limit*)
            (setf marks (subseq marks 0 *prompt-mark-limit*)))
          (setf (terminal-emulator-prompt-marks emulator) marks))))))

(defun note-command-output (emulator what &optional status)
  "The newest command's output starts (WHAT :START, OSC 133 C) or ends
(:END, OSC 133 D, with its exit STATUS) at the cursor."
  (let ((engine (terminal-emulator-engine emulator))
        (mark (first (terminal-emulator-prompt-marks emulator))))
    (when (and mark (eq :primary (stream-screen engine)))
      (multiple-value-bind (row column) (engine-cursor-position engine)
        (if (eq what :start)
            (setf (command-mark-output mark) (stored-row emulator row :primary)
                  (command-mark-last mark) nil)
            (when (command-mark-output mark)
              ;; D comes at the start of the line after the output.
              (setf (command-mark-last mark)
                    (max (command-mark-output mark)
                         (stored-row emulator (if (zerop column) (1- row) row) :primary))
                    (command-mark-status mark) status)))))))

(defun jump-to-prompt (emulator direction &key failed)
  "Scroll the view so the previous (DIRECTION :UP) or next (:DOWN) prompt
is at the top: only prompts of commands that failed, with FAILED.  Return
true when there was one."
  (with-engine (engine emulator)
    (let* ((offset (engine-viewport-offset engine))
           (top (and offset (- offset)))
           (candidates (and top
                            (remove-if-not (lambda (row)
                                             (if (eq direction :up) (< row top) (> row top)))
                                           (prompt-mark-rows emulator :failed failed))))
           (target (and candidates
                        (reduce (if (eq direction :up) #'max #'min) candidates))))
      (when target
        ;; Rows in the scrollback are negative; one on the live screen
        ;; means following it.
        (if (>= target 0)
            (engine-scroll engine :bottom)
            (engine-scroll engine (+ offset target)))
        t))))

(defun screen-rows-text (emulator engine first last)
  "The text of screen rows FIRST to LAST (negative rows are in the
scrollback), read through the engine's viewport a screenful at a time.
The engine lock must be held.  Rows the engine keeps no more are left out."
  (let* ((rows (engine-rows engine))
         (columns (engine-columns engine))
         (grid (make-grid rows columns))
         (saved (or (engine-viewport-offset engine) 0))
         (lines '()))
    (unwind-protect
         (loop for top from first to last by rows
               do (engine-scroll engine :bottom)
                  (when (minusp top)
                    (engine-scroll engine top))
                  (engine-update-grid engine grid :force t)
                  ;; Where row TOP really is: the engine may not scroll so far.
                  (let ((shown-top (- (or (engine-viewport-offset engine) 0))))
                    (loop for row from (max top shown-top) to (min last (+ top rows -1))
                          for index = (- row shown-top)
                          when (< -1 index rows)
                            do (push (grid-row-text grid index) lines))))
      (engine-scroll engine :bottom)
      (when (plusp saved)
        (engine-scroll engine (- saved)))
      ;; The reads consumed the engine's dirty rows.
      (setf (terminal-image-store-force-render-p (terminal-emulator-image-store emulator)) t))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(defun last-command-output (emulator)
  "The text of the output of the newest command that finished, or NIL."
  (with-engine (engine emulator)
    (let ((mark (find-if #'command-mark-last (terminal-emulator-prompt-marks emulator))))
      (when mark
        (screen-rows-text emulator engine
                          (screen-row emulator (command-mark-output mark) :primary)
                          (screen-row emulator (command-mark-last mark) :primary))))))

(defun copy-last-command-output (emulator)
  "Copy the output of the newest command that finished to the clipboard.
Return true when there was one."
  (let ((text (last-command-output emulator)))
    (when text
      (frontend-set-clipboard-text (terminal-emulator-frontend emulator)
                                  (terminal-emulator-view emulator)
                                  text)
      t)))

;;; Hyperlinks (OSC 8)

(defun note-hyperlink-text (emulator octets start end)
  "Text OCTETS START END is about to be written while a hyperlink is open:
remember the cells it will cover."
  (let* ((engine (terminal-emulator-engine emulator))
         (uri (terminal-emulator-hyperlink emulator))
         (columns (engine-columns engine))
         (text (babel:octets-to-string octets :start start :end end
                                              :encoding :utf-8 :errorp nil)))
    (when uri
      (multiple-value-bind (row column) (engine-cursor-position engine)
        (let ((screen (stream-screen engine))
              (piece-start column)
              (piece (make-string-output-stream))
              (spans '()))
          (flet ((finish-piece (end-column)
                   (let ((piece-text (get-output-stream-string piece)))
                     (when (> end-column piece-start)
                       (push (make-link-span :row (stored-row emulator row screen)
                                             :column piece-start
                                             :width (- end-column piece-start)
                                             :uri uri :text piece-text :screen screen)
                             spans)))))
            (loop for char across text
                  for width = (lispvt:char-width (char-code char))
                  do (when (and (plusp width) (> (+ column width) columns))
                       ;; Wraps to the next line.
                       (finish-piece column)
                       (incf row)
                       (setf column 0 piece-start 0))
                     (write-char char piece)
                     (incf column width))
            (finish-piece column))
          (dolist (span (nreverse spans))
            (add-link-span emulator span)))))))

(defun add-link-span (emulator span)
  "Remember SPAN, joined to the newest span when it continues it."
  (let ((newest (first (terminal-emulator-links emulator))))
    (if (and newest
             (eq (link-span-screen newest) (link-span-screen span))
             (= (link-span-row newest) (link-span-row span))
             (= (+ (link-span-column newest) (link-span-width newest)) (link-span-column span))
             (string= (link-span-uri newest) (link-span-uri span))
             (< (link-span-width newest) 4096))
        (setf (link-span-width newest) (+ (link-span-width newest) (link-span-width span))
              (link-span-text newest) (concatenate 'string (link-span-text newest)
                                                   (link-span-text span)))
        (progn
          (push span (terminal-emulator-links emulator))
          ;; Trim now and then, not on every span.
          (when (> (incf (terminal-emulator-link-count emulator)) (* 2 *hyperlink-limit*))
            (let ((kept (remove-if (lambda (old)
                                     (< (screen-row emulator (link-span-row old)
                                                    (link-span-screen old))
                                        (- *terminal-image-scrollback-rows*)))
                                   (subseq (terminal-emulator-links emulator)
                                           0 *hyperlink-limit*))))
              (setf (terminal-emulator-links emulator) kept
                    (terminal-emulator-link-count emulator) (length kept))))))))

(defun forget-links (emulator screen &optional (where :all))
  "Drop the hyperlink spans of SCREEN: all of them, those on the :SCREEN,
or those in the :SCROLLBACK."
  (setf (terminal-emulator-link-count emulator) 0)
  (setf (terminal-emulator-links emulator)
        (remove-if (lambda (span)
                     (and (eq screen (link-span-screen span))
                          (case where
                            (:screen (>= (screen-row emulator (link-span-row span) screen) 0))
                            (:scrollback (minusp (screen-row emulator (link-span-row span)
                                                             screen)))
                            (t t))))
                   (terminal-emulator-links emulator))))

(defun hyperlink-at (emulator row column)
  "The URI of the hyperlink shown at ROW, COLUMN, if its text is still there."
  (with-engine (engine emulator)
    (hyperlink-at-locked emulator engine row column)))

(defun hyperlink-at-locked (emulator engine row column)
  (let ((span (hyperlink-span-at-locked emulator engine row column)))
    (and span (link-span-uri span))))

(defun hyperlink-span-at-locked (emulator engine row column)
  "The span of the hyperlink shown at ROW, COLUMN, if its text is still
there."
  (let* ((grid (terminal-emulator-grid emulator))
         (offset (engine-viewport-offset engine))
         (screen (stream-screen engine)))
    (when (and offset (< -1 row (grid-rows grid)))
      (let ((absolute (- row offset)))
        (loop for span in (terminal-emulator-links emulator)
              when (and (eq screen (link-span-screen span))
                        (= absolute (screen-row emulator (link-span-row span) screen))
                        (<= (link-span-column span) column
                            (+ (link-span-column span) (link-span-width span) -1))
                        (string= (link-span-text span)
                                 (with-output-to-string (out)
                                   (loop for c from (link-span-column span)
                                           below (min (grid-columns grid)
                                                      (+ (link-span-column span)
                                                         (link-span-width span)))
                                         do (write-string
                                             (grid-cell-string grid (grid-index grid row c))
                                             out)))))
                return span)))))

(defun apply-link-hover (emulator engine grid)
  "Underline the link under the pointer, an OSC 8 hyperlink or a URL in the
text, as kitty does.  Called by RENDER-EMULATOR with the engine lock held,
once GRID is up to date.  Cells underlined here carry +ATTR-HOVER+, which
engines never set, so they can be told from cells the engine underlined
until it rewrites them."
  (let* ((old (terminal-emulator-hover-link emulator))
         (known (terminal-emulator-pointer-known-p emulator))
         (row (terminal-emulator-pointer-row emulator))
         (column (terminal-emulator-pointer-column emulator))
         (span (and known (terminal-emulator-links emulator)
                    (hyperlink-span-at-locked emulator engine row column)))
         ;; A list of (ROW FIRST LAST) extents.
         (new (cond (span
                     (list (list row (link-span-column span)
                                 (min (1- (grid-columns grid))
                                      (+ (link-span-column span) (link-span-width span) -1)))))
                    (known
                     ;; The hover underline itself does not change the text.
                     (nth-value 1 (url-extent grid row column))))))
    (flet ((update (extents function)
             (loop for (row first last) in extents
                   when (< row (grid-rows grid))
                     do (loop for column from first to (min last (1- (grid-columns grid)))
                              for index = (grid-index grid row column)
                              for attrs = (aref (grid-attrs grid) index)
                              for changed = (funcall function attrs)
                              unless (= changed attrs)
                                do (setf (aref (grid-attrs grid) index) changed
                                         (sbit (grid-dirty grid) row) 1)))))
      (when (and old (not (equal old new)))
        (update old (lambda (attrs)
                      (if (logtest attrs +attr-hover+)
                          (logandc2 attrs (logior +attr-hover+ +attr-underline+))
                          attrs))))
      (setf (terminal-emulator-hover-link emulator) new)
      (when new
        (update new (lambda (attrs)
                      (if (logtest attrs +attr-underlines+)
                          attrs
                          (logior attrs +attr-underline+ +attr-hover+))))))))

;;; Scrolling of remembered rows

(defun note-annotation-scroll (emulator top bottom count screen rows)
  "Move hyperlink spans and prompt marks with a scroll of rows TOP..BOTTOM
of SCREEN by COUNT (upward when positive) on a screen of ROWS rows."
  (let ((whole (and (zerop top) (>= bottom (1- rows)))))
    (if (and whole (plusp count) (eq screen :primary))
        ;; The common case costs nothing: stored rows stay put.
        (incf (terminal-emulator-row-origin emulator) count)
        (flet ((moved (row)
                 ;; A screen ROW's new position, or NIL when it leaves.
                 (cond
                   ((and whole (minusp row)) row)
                   ((<= top row bottom)
                    (let ((new (- row count))) (and (<= top new bottom) new)))
                   (t row))))
          (setf (terminal-emulator-links emulator)
                (loop for span in (terminal-emulator-links emulator)
                      for new = (if (eq screen (link-span-screen span))
                                    (moved (screen-row emulator (link-span-row span) screen))
                                    :same)
                      when new
                        collect (progn
                                  (unless (eq new :same)
                                    (setf (link-span-row span) (stored-row emulator new screen)))
                                  span)))
          (setf (terminal-emulator-link-count emulator)
                (length (terminal-emulator-links emulator)))
          (when (eq screen :primary)
            (flet ((move (stored)
                     (let ((new (and stored (moved (screen-row emulator stored :primary)))))
                       (and new (stored-row emulator new :primary)))))
              (setf (terminal-emulator-prompt-marks emulator)
                    (loop for mark in (terminal-emulator-prompt-marks emulator)
                          for prompt = (move (command-mark-prompt mark))
                          when prompt
                            collect (let ((output (move (command-mark-output mark)))
                                          (last (move (command-mark-last mark))))
                                      (setf (command-mark-prompt mark) prompt
                                            (command-mark-output mark) output
                                            (command-mark-last mark) (and output last))
                                      mark)))))))))

;;; Keys

(defparameter *kitty-modifiers* '((:shift . :shift) (:meta . :alt) (:control . :control)
                                  (:super . :super)))

(defun kitty-key-input (emulator key modifiers &optional (event :press))
  "What to send for KEY with MODIFIERS when the child enabled the Kitty
keyboard protocol on this screen, or :LEGACY to encode it the usual way.
EVENT is :PRESS, :REPEAT or :RELEASE."
  (let* ((engine (terminal-emulator-engine emulator))
         (flags (kitty-keyboard:keyboard-flags (terminal-emulator-keyboard emulator)
                                               (stream-screen engine))))
    (if (zerop flags)
        :legacy
        (let* ((modifiers (loop for modifier in modifiers
                                for kitty = (cdr (assoc modifier *kitty-modifiers*))
                                when kitty collect kitty))
               (key (if (and (characterp key) (< (char-code key) 32))
                        ;; A control character: the key typed with Ctrl.
                        (let ((code (char-code key)))
                          (cond ((zerop code) #\Space)
                                ((<= code 26) (code-char (+ 96 code)))
                                (t (code-char (+ 64 code)))))
                        key))
               (text (and (characterp key)
                          (not (eq event :release))
                          (not (intersection modifiers '(:control :alt :super)))
                          (string key)))
               (base key))
          ;; Frontends fold Shift into the character (A, !): recover the
          ;; key and the modifier.
          (when (characterp key)
            (multiple-value-bind (unshifted shifted) (kitty-keyboard:unshifted-key key)
              (setf base unshifted)
              (when shifted (pushnew :shift modifiers))))
          (if (or (characterp base) (kitty-keyboard:functional-key-p base))
              (or (kitty-keyboard:encode-key
                   flags base :modifiers modifiers :text text :event event
                              :shifted-key (and (characterp key) (char/= key base) key)
                              :application-cursor-keys (engine-mode engine :application-cursor))
                  "")
              :legacy)))))
