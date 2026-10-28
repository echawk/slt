(in-package #:slt)

;;;; A Lisp REPL in a terminal window, running in slt's own image: a PTY
;;;; whose far side is a thread reading and evaluating forms instead of a
;;;; shell.  The PTY's line discipline gives the line editing a shell's
;;;; terminal has (erase, kill line and word, end of file with Control-D);
;;;; Control-C interrupts the evaluation into the debugger.  Open one with
;;;; the New Lisp REPL command or OPEN-REPL-WINDOW.

(defvar *repl-package* "CL-USER"
  "The package a new Lisp REPL window starts in.")

(defvar *repl-banner*
  "slt Lisp REPL (~a ~a), in this slt's image.  Control-D closes the window,
Control-C interrupts.  (in-package :slt) to work on slt itself.~%"
  "Printed when a Lisp REPL window opens, given the Lisp's type and version.")

(defconstant +interrupt-character+ (code-char 3)
  "What Control-C sends: ETX.")

(cffi:defcfun ("openpty" %openpty) :int
  (master :pointer) (slave :pointer) (name :pointer) (termios :pointer) (winsize :pointer))

(defun ensure-openpty ()
  "Make openpty callable: it is in libc on macOS and recent glibc, and in
libutil before glibc 2.34."
  (unless (cffi:foreign-symbol-pointer "openpty")
    (cffi:load-foreign-library '(:or "libutil.so.1" "libutil.so"))))

(defstruct (repl-process (:constructor %make-repl-process))
  ;; The terminal's side of the PTY, which slt reads and writes.
  stream
  ;; The REPL's side, and the thread reading it.
  slave-fd
  thread)

(defun open-pty ()
  "A new PTY: its master and slave descriptors."
  (ensure-openpty)
  (cffi:with-foreign-objects ((master :int) (slave :int))
    (unless (zerop (%openpty master slave (cffi:null-pointer) (cffi:null-pointer)
                             (cffi:null-pointer)))
      (error "openpty failed"))
    (values (cffi:mem-ref master :int) (cffi:mem-ref slave :int))))

(defun repl-prompt (stream)
  "Prompt with the current package's shortest name, as CL-USER>."
  (format stream "~&~a> " (reduce (lambda (a b) (if (<= (length a) (length b)) a b))
                                  (package-nicknames *package*)
                                  :initial-value (package-name *package*)))
  (force-output stream))

(defun run-repl (stream)
  "Read, evaluate and print forms from STREAM until its end."
  (let ((*standard-input* stream) (*standard-output* stream) (*error-output* stream)
        (*trace-output* stream) (*query-io* stream) (*debug-io* stream)
        (*terminal-io* stream)
        (*package* (or (find-package *repl-package*) (find-package "CL-USER")))
        ;; bin/slt runs without the debugger; this thread has it.
        (sb-ext:*invoke-debugger-hook* nil)
        (*debugger-hook* nil)
        (* nil) (** nil) (*** nil) (/ nil) (// nil) (/// nil) (+ nil) (++ nil) (+++ nil))
    (format stream *repl-banner* (lisp-implementation-type) (lisp-implementation-version))
    (loop
      (with-simple-restart (abort "Return to the slt REPL.")
        (repl-prompt stream)
        (let ((form (read stream nil stream)))
          (when (eq form stream)
            (return))
          (let ((values (multiple-value-list (eval form))))
            (shiftf +++ ++ + form)
            (shiftf /// // / values)
            (shiftf *** ** * (first values))
            (format stream "~{~s~^~%~}" values)))))))

(defun launch-repl-process (&key rows columns (pixel-width 0) (pixel-height 0))
  "Start a REPL thread on a new PTY, sized ROWS x COLUMNS, and return it as
the process of a terminal."
  (multiple-value-bind (master slave) (open-pty)
    (let* ((stream (sb-sys:make-fd-stream master :input t :output t
                                                 :element-type '(unsigned-byte 8)
                                                 :buffering :none :name "slt REPL PTY"))
           (process (%make-repl-process :stream stream :slave-fd slave)))
      (when (and rows columns)
        (resize-process-pty process rows columns
                            :pixel-width pixel-width :pixel-height pixel-height))
      (setf (repl-process-thread process)
            (bt:make-thread
             (lambda ()
               (let ((repl-stream (sb-sys:make-fd-stream slave :input t :output t
                                                               :external-format :utf-8
                                                               :buffering :line
                                                               :name "slt REPL")))
                 ;; A session of its own: SBCL's debugger otherwise waits for
                 ;; the main thread to give up the console's.
                 (unwind-protect (sb-thread::with-new-session () (run-repl repl-stream))
                   ;; Closing the slave ends the terminal's reads: the
                   ;; window closes as when a shell exits.
                   (ignore-errors (close repl-stream)))))
             :name "slt Lisp REPL"))
      process)))

(defun repl-process-alive-p (process)
  (bt:thread-alive-p (repl-process-thread process)))

(defun terminate-repl-process (process)
  (let ((thread (repl-process-thread process)))
    (when (bt:thread-alive-p thread)
      (ignore-errors
       (sb-thread:interrupt-thread
        thread (lambda () (sb-thread:abort-thread :allow-exit t)))))))

(defun interrupt-repl-process (process)
  "Stop the REPL's evaluation in the debugger, as Control-C does."
  (let ((thread (repl-process-thread process)))
    (when (bt:thread-alive-p thread)
      (sb-thread:interrupt-thread
       thread (lambda ()
                (with-simple-restart (continue "Resume the interrupted evaluation.")
                  (invoke-debugger
                   (make-condition 'simple-condition
                                   :format-control "Interrupted by Control-C."))))))))
