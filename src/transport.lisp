(in-package #:slt)

#+darwin
(defstruct (forkpty-process
            (:constructor %make-forkpty-process))
  pid
  stream
  exit-status
  (reaped-p nil))

#+darwin
(cffi:defcfun ("forkpty" %forkpty) :int
  (amaster :pointer)
  (name :pointer)
  (termp :pointer)
  (winp :pointer))

#+darwin
(cffi:defcfun ("execve" %execve) :int
  (pathname :string)
  (argv :pointer)
  (envp :pointer))

#+darwin
(cffi:defcfun ("_exit" %c-exit) :void
  (status :int))

;;; Newer SBCLs may start children with posix_spawn, which cannot give a
;;; PTY its controlling terminal; older ones (Alpine's 2.5.4) have neither
;;; posix_spawn nor the keyword that turns it off.
#+(and sbcl (not darwin))
(defparameter *run-program-pty-arguments*
  (and (search "USE-POSIX-SPAWN"
               (prin1-to-string (sb-kernel:%fun-lambda-list #'sb-ext:run-program)))
       '(:use-posix-spawn nil)))

;;; struct winsize and TIOCSWINSZ, which sb-posix lacks.  The struct is
;;; four unsigned shorts on every Unix.  TIOCSWINSZ is _IOW('t', 103,
;;; struct winsize) on the BSDs, macOS and Linux on alpha, mips, powerpc and
;;; sparc; other Linux ports use the old 0x5414.

(cffi:defcstruct winsize
  (row :unsigned-short)
  (col :unsigned-short)
  (xpixel :unsigned-short)
  (ypixel :unsigned-short))

(defconstant +tiocswinsz+
  #+(or (not linux) alpha mips ppc ppc64 sparc sparc64) #x80087467
  #-(or (not linux) alpha mips ppc ppc64 sparc sparc64) #x5414)

(defun read-available-output (stream)
  (handler-case
      (with-output-to-string (buffer)
        (loop for character = (read-char-no-hang stream nil :eof)
              until (or (null character) (eq character :eof))
              do (write-char character buffer)))
    (end-of-file ()
      "")
    (stream-error ()
      "")))

(defun shell-search-path-p (shell)
  (not (find #\/ shell)))

(defun environment-entry-name (entry)
  (let ((separator (position #\= entry)))
    (if separator
        (subseq entry 0 separator)
        entry)))

(defun replace-environment-variable (environment name value)
  (let ((entry (format nil "~A=~A" name value)))
    (cons entry
          (loop for existing in environment
                unless (string-equal name (environment-entry-name existing))
                  collect existing))))

(defun terminal-process-environment (&key
                                       (term-name "xterm-256color")
                                       rows
                                       columns
                                       (variables '())
                                     #+sbcl
                                       (base-environment (sb-ext:posix-environ))
                                     #-sbcl
                                       (base-environment '()))
  (declare (ignore rows columns))
  (let ((environment (copy-list base-environment)))
    (when term-name
      (setf environment
            (replace-environment-variable environment "TERM" term-name)))
    ;; The size is the PTY's alone.  curses prefers LINES and COLUMNS to
    ;; the PTY, so set (or inherited) they would pin programs such as htop
    ;; to the size the window had at launch.
    (setf environment
          (remove-if (lambda (entry)
                       (member (environment-entry-name entry) '("LINES" "COLUMNS")
                               :test #'string=))
                     environment))
    ;; VARIABLES is an alist of (NAME . VALUE); a NIL value unsets NAME,
    ;; and (NAME . :DEFAULT) sets NAME only when it is not already set.
    (loop for (name . value) in variables
          do (cond
               ((null value)
                (setf environment
                      (remove name environment :key #'environment-entry-name
                                               :test #'string-equal)))
               ((eq value :default))
               (t
                (setf environment
                      (replace-environment-variable environment name value)))))
    environment))

(defun default-environment-variables (&optional (base-environment
                                                 #+sbcl (sb-ext:posix-environ)
                                                 #-sbcl '()))
  "Variables a terminal sets for its children: truecolor support, the
program's name, and a UTF-8 locale when the environment has none (as for
an application started from the Finder)."
  (flet ((setp (name)
           (find name base-environment :key #'environment-entry-name
                                       :test #'string-equal)))
    (append
     (list (cons "COLORTERM" "truecolor")
           (cons "TERM_PROGRAM" "slt")
           (cons "TERM_PROGRAM_VERSION" (system-version)))
     (unless (or (setp "LANG") (setp "LC_ALL") (setp "LC_CTYPE"))
       (list (cons "LANG" "en_US.UTF-8"))))))

(defun user-login-shell ()
  "The user's shell: $SHELL, else the password database's, else /bin/sh."
  (or (uiop:getenv "SHELL")
      (ignore-errors
        (let ((shell (sb-posix:passwd-shell (sb-posix:getpwuid (sb-posix:getuid)))))
          (and (plusp (length shell)) shell)))
      "/bin/sh"))

(defun shell-arguments (shell-path login)
  "argv for SHELL-PATH: a login shell gets argv[0] starting with -, as
login(1) does; otherwise -i asks for an interactive shell."
  (if login
      (list (concatenate 'string "-" (file-namestring shell-path)))
      (list shell-path "-i")))

(defun native-pty-supported-p ()
  #+sbcl (not (uiop:os-windows-p))
  #-sbcl nil)

(defun process-pty-stream (process)
  (typecase process
    (repl-process (repl-process-stream process))
    #+darwin
    (forkpty-process
     (forkpty-process-stream process))
    #+sbcl
    (sb-impl::process
     (sb-ext:process-pty process))
    (t nil)))

#+darwin
(defun reap-forkpty-process (process)
  (unless (forkpty-process-reaped-p process)
    (multiple-value-bind (pid status)
        (sb-posix:waitpid (forkpty-process-pid process)
                          sb-posix:wnohang)
      (when (plusp pid)
        (setf (forkpty-process-reaped-p process) t
              (forkpty-process-exit-status process) status))))
  process)

(defun process-alive-p (process)
  (typecase process
    (repl-process (repl-process-alive-p process))
    #+darwin
    (forkpty-process
     (not (forkpty-process-reaped-p (reap-forkpty-process process))))
    #+sbcl
    (sb-impl::process
     (and process (sb-ext:process-alive-p process)))
    (t nil)))

(defun terminate-process (process)
  (typecase process
    (repl-process (terminate-repl-process process))
    #+darwin
    (forkpty-process
     (ignore-errors
       (sb-posix:killpg (forkpty-process-pid process) sb-posix:sigterm))
     (ignore-errors
       (sb-posix:kill (forkpty-process-pid process) sb-posix:sigterm)))
    #+sbcl
    (sb-impl::process
     (when process
       (multiple-value-bind (ok errno)
           (sb-ext:process-kill process sb-posix:sigterm :pty-process-group)
         (declare (ignore errno))
         (unless ok
           (sb-ext:process-kill process sb-posix:sigterm))))))
  process)

(defun resolve-shell-path (shell)
  (if (shell-search-path-p shell)
      (or (loop for directory in (uiop:getenv-pathnames "PATH")
                for pathname = (merge-pathnames shell directory)
                when (uiop:file-exists-p pathname)
                  return (namestring pathname))
          (error "Unable to resolve shell executable ~S from PATH." shell))
      shell))

(defun make-foreign-string-vector (strings)
  (let* ((count (length strings))
         (vector (cffi:foreign-alloc :pointer :count (1+ count)))
         (pointers (loop for string in strings
                         for index from 0
                         for pointer = (cffi:foreign-string-alloc string)
                         do (setf (cffi:mem-aref vector :pointer index) pointer)
                         collect pointer)))
    (setf (cffi:mem-aref vector :pointer count) (cffi:null-pointer))
    (values vector pointers)))

(defun free-foreign-string-vector (vector pointers)
  (when vector
    (dolist (pointer pointers)
      (cffi:foreign-string-free pointer))
    (cffi:foreign-free vector)))

#+darwin
(cffi:defcfun ("chdir" %chdir) :int
  (pathname :string))

#+darwin
(defun launch-shell-process/forkpty (shell &key
                                           (term-name "xterm-256color")
                                           rows
                                           columns
                                           (pixel-width 0)
                                           (pixel-height 0)
                                           login
                                           directory
                                           (variables '()))
  (let* ((shell-path (resolve-shell-path shell))
         (argv nil)
         (argv-pointers nil)
         (envp nil)
         (envp-pointers nil))
    (unwind-protect
         (progn
           (multiple-value-setq (argv argv-pointers)
             (make-foreign-string-vector (shell-arguments shell-path login)))
           (multiple-value-setq (envp envp-pointers)
             (make-foreign-string-vector
              (terminal-process-environment :term-name term-name
                                            :rows rows
                                            :columns columns
                                            :variables variables)))
           (cffi:with-foreign-object (master :int)
             (cffi:with-foreign-object (winsize '(:struct winsize))
               (setf (cffi:foreign-slot-value winsize
                                              '(:struct winsize)
                                              'row)
                     (max 1 (or rows 24))
                     (cffi:foreign-slot-value winsize
                                              '(:struct winsize)
                                              'col)
                     (max 1 (or columns 80))
                     (cffi:foreign-slot-value winsize
                                              '(:struct winsize)
                                              'xpixel)
                     (max 0 pixel-width)
                     (cffi:foreign-slot-value winsize
                                              '(:struct winsize)
                                              'ypixel)
                     (max 0 pixel-height))
               (let ((pid (%forkpty master
                                    (cffi:null-pointer)
                                    (cffi:null-pointer)
                                    winsize)))
                 (cond
                   ((minusp pid)
                    (error "forkpty failed."))
                   ((zerop pid)
                    (when directory
                      (%chdir directory))
                    (%execve shell-path argv envp)
                    (%c-exit 127))
                   (t
                    (let* ((fd (cffi:mem-ref master :int))
                           (stream (sb-sys:make-fd-stream fd
                                                          :input t
                                                          :output t
                                                          :element-type 'character
                                                          :external-format :utf-8)))
                      (values (%make-forkpty-process :pid pid
                                                     :stream stream)
                              stream
                              stream))))))))
      (free-foreign-string-vector argv argv-pointers)
      (free-foreign-string-vector envp envp-pointers))))

(defun launch-shell-process (shell &key
                                     (term-name "xterm-256color")
                                     rows
                                     columns
                                     (pixel-width 0)
                                     (pixel-height 0)
                                     login
                                     directory
                                     (variables '()))
  "Start SHELL on a new PTY.  LOGIN makes it a login shell (macOS only;
elsewhere it is started with -i), DIRECTORY is its working directory, and
VARIABLES are added to its environment (see TERMINAL-PROCESS-ENVIRONMENT)."
  #+darwin
  (launch-shell-process/forkpty shell
                                :term-name term-name
                                :rows rows
                                :columns columns
                                :pixel-width pixel-width
                                :pixel-height pixel-height
                                :login login
                                :directory directory
                                :variables variables)
  #-(or darwin (not sbcl))
  (declare (ignore login))
  #-(or darwin (not sbcl))
  (let* ((process (apply #'sb-ext:run-program shell
                                              '("-i")
                                              :directory directory
                                              :environment (terminal-process-environment
                                                             :term-name term-name
                                                             :rows rows
                                                             :columns columns
                                                             :variables variables)
                                              :search (shell-search-path-p shell)
                                              :wait nil
                                              :pty t
                                              :input nil
                                              :output nil
                                              :error :output
                                              *run-program-pty-arguments*))
         (stream (sb-ext:process-pty process)))
    (when (and rows columns)
      (resize-process-pty process
                          rows
                          columns
                          :pixel-width pixel-width
                          :pixel-height pixel-height))
    (values process stream stream))
  #-(or darwin sbcl)
  (declare (ignore shell term-name rows columns pixel-width pixel-height
                   login directory variables))
  #-(or darwin sbcl)
  (error "Native PTY shell transport is only implemented on SBCL."))

(defun resize-process-pty (process rows columns
                           &key (pixel-width 0) (pixel-height 0))
  (when process
    (let ((stream (process-pty-stream process)))
      (when stream
        (cffi:with-foreign-object (winsize '(:struct winsize))
          (setf (cffi:foreign-slot-value winsize
                                         '(:struct winsize)
                                         'row)
                (max 1 rows)
                (cffi:foreign-slot-value winsize
                                         '(:struct winsize)
                                         'col)
                (max 1 columns)
                (cffi:foreign-slot-value winsize
                                         '(:struct winsize)
                                         'xpixel)
                (max 0 pixel-width)
                (cffi:foreign-slot-value winsize
                                         '(:struct winsize)
                                         'ypixel)
                (max 0 pixel-height))
          (multiple-value-bind (ok errno)
              (sb-unix:unix-ioctl (sb-sys:fd-stream-fd stream)
                                  +tiocswinsz+
                                  (sb-sys:int-sap (cffi:pointer-address winsize)))
            (unless ok
              (error "TIOCSWINSZ failed with errno ~A" errno)))))))
  process)

;;; Octet I/O directly on the PTY descriptor.  Reading characters through
;;; an fd-stream decodes one character per call and can stall on split
;;; UTF-8 sequences; engines want raw bytes in large chunks instead.

(defun process-fd (process)
  (let ((stream (process-pty-stream process)))
    (and stream (sb-sys:fd-stream-fd stream))))

(defun set-fd-nonblocking (fd)
  (let ((flags (sb-posix:fcntl fd sb-posix:f-getfl)))
    (sb-posix:fcntl fd sb-posix:f-setfl (logior flags sb-posix:o-nonblock)))
  fd)

(defun read-available-octets (fd buffer &key (start 0))
  "Read whatever is available on non-blocking FD into BUFFER from START.
Return the number of octets read, 0 when nothing is pending, or :EOF once
the other side has closed (Linux reports EIO when the child exits)."
  (declare (type octets buffer)
           (type fixnum start))
  (let ((length (- (length buffer) start)))
    (when (<= length 0)
      (return-from read-available-octets 0))
    (multiple-value-bind (count errno)
        (sb-sys:with-pinned-objects (buffer)
          (sb-unix:unix-read fd
                             (sb-sys:sap+ (sb-sys:vector-sap buffer) start)
                             length))
      (cond
        ((null count)
         (if (or (= errno sb-unix:eagain) (= errno sb-unix:eintr))
             0
             :eof))
        ((zerop count) :eof)
        (t count)))))

(defun write-octets (fd octets &key (start 0) (end (length octets)))
  "Write OCTETS[START, END) to FD, waiting while a non-blocking FD is full."
  (declare (type octets octets))
  (loop while (< start end)
        do (multiple-value-bind (count errno)
               (sb-sys:with-pinned-objects (octets)
                 (sb-unix:unix-write fd octets start (- end start)))
             (cond
               (count (incf start count))
               ((or (= errno sb-unix:eagain) (= errno sb-unix:eintr))
                (sb-sys:wait-until-fd-usable fd :output 0.05))
               (t (error "write to PTY failed: ~a" (sb-int:strerror errno))))))
  end)
