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
                                     #+sbcl
                                       (base-environment (sb-ext:posix-environ))
                                     #-sbcl
                                       (base-environment '()))
  (let ((environment (copy-list base-environment)))
    (when term-name
      (setf environment
            (replace-environment-variable environment "TERM" term-name)))
    (when rows
      (setf environment
            (replace-environment-variable environment "LINES"
                                          (princ-to-string rows))))
    (when columns
      (setf environment
            (replace-environment-variable environment "COLUMNS"
                                          (princ-to-string columns))))
    environment))

(defun native-pty-supported-p ()
  #+sbcl (not (uiop:os-windows-p))
  #-sbcl nil)

(defun process-pty-stream (process)
  (typecase process
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
    #+darwin
    (forkpty-process
     (not (forkpty-process-reaped-p (reap-forkpty-process process))))
    #+sbcl
    (sb-impl::process
     (and process (sb-ext:process-alive-p process)))
    (t nil)))

(defun terminate-process (process)
  (typecase process
    #+darwin
    (forkpty-process
     (ignore-errors
       (sb-posix:killpg (forkpty-process-pid process) osicat-posix:sigterm))
     (ignore-errors
       (sb-posix:kill (forkpty-process-pid process) osicat-posix:sigterm)))
    #+sbcl
    (sb-impl::process
     (when process
       (multiple-value-bind (ok errno)
           (sb-ext:process-kill process osicat-posix:sigterm :pty-process-group)
         (declare (ignore errno))
         (unless ok
           (sb-ext:process-kill process osicat-posix:sigterm))))))
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
(defun launch-shell-process/forkpty (shell &key
                                           (term-name "xterm-256color")
                                           rows
                                           columns
                                           (pixel-width 0)
                                           (pixel-height 0))
  (let* ((shell-path (resolve-shell-path shell))
         (argv nil)
         (argv-pointers nil)
         (envp nil)
         (envp-pointers nil))
    (unwind-protect
         (progn
           (multiple-value-setq (argv argv-pointers)
             (make-foreign-string-vector (list shell-path "-i")))
           (multiple-value-setq (envp envp-pointers)
             (make-foreign-string-vector
              (terminal-process-environment :term-name term-name
                                            :rows rows
                                            :columns columns)))
           (cffi:with-foreign-object (master :int)
             (cffi:with-foreign-object (winsize '(:struct osicat-posix:winsize))
               (setf (cffi:foreign-slot-value winsize
                                              '(:struct osicat-posix:winsize)
                                              'osicat-posix::row)
                     (max 1 (or rows 24))
                     (cffi:foreign-slot-value winsize
                                              '(:struct osicat-posix:winsize)
                                              'osicat-posix::col)
                     (max 1 (or columns 80))
                     (cffi:foreign-slot-value winsize
                                              '(:struct osicat-posix:winsize)
                                              'osicat-posix::xpixel)
                     (max 0 pixel-width)
                     (cffi:foreign-slot-value winsize
                                              '(:struct osicat-posix:winsize)
                                              'osicat-posix::ypixel)
                     (max 0 pixel-height))
               (let ((pid (%forkpty master
                                    (cffi:null-pointer)
                                    (cffi:null-pointer)
                                    winsize)))
                 (cond
                   ((minusp pid)
                    (error "forkpty failed."))
                   ((zerop pid)
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
                                     (pixel-height 0))
  #+darwin
  (launch-shell-process/forkpty shell
                                :term-name term-name
                                :rows rows
                                :columns columns
                                :pixel-width pixel-width
                                :pixel-height pixel-height)
  #-(or darwin (not sbcl))
  (let* ((process (sb-ext:run-program shell
                                      '("-i")
                                      :environment (terminal-process-environment
                                                     :term-name term-name
                                                     :rows rows
                                                     :columns columns)
                                      :search (shell-search-path-p shell)
                                      :wait nil
                                      :pty t
                                      :use-posix-spawn nil
                                      :input nil
                                      :output nil
                                      :error :output))
         (stream (sb-ext:process-pty process)))
    (when (and rows columns)
      (resize-process-pty process
                          rows
                          columns
                          :pixel-width pixel-width
                          :pixel-height pixel-height))
    (values process stream stream))
  #-(or darwin sbcl)
  (declare (ignore shell term-name rows columns pixel-width pixel-height))
  #-(or darwin sbcl)
  (error "Native PTY shell transport is only implemented on SBCL."))

(defun resize-process-pty (process rows columns
                           &key (pixel-width 0) (pixel-height 0))
  (when process
    (let ((stream (process-pty-stream process)))
      (when stream
        (cffi:with-foreign-object (winsize '(:struct osicat-posix:winsize))
          (setf (cffi:foreign-slot-value winsize
                                         '(:struct osicat-posix:winsize)
                                         'osicat-posix::row)
                (max 1 rows)
                (cffi:foreign-slot-value winsize
                                         '(:struct osicat-posix:winsize)
                                         'osicat-posix::col)
                (max 1 columns)
                (cffi:foreign-slot-value winsize
                                         '(:struct osicat-posix:winsize)
                                         'osicat-posix::xpixel)
                (max 0 pixel-width)
                (cffi:foreign-slot-value winsize
                                         '(:struct osicat-posix:winsize)
                                         'osicat-posix::ypixel)
                (max 0 pixel-height))
          (multiple-value-bind (ok errno)
              (sb-unix:unix-ioctl (sb-sys:fd-stream-fd stream)
                                  osicat-posix:tiocswinsz
                                  (sb-sys:int-sap (cffi:pointer-address winsize)))
            (unless ok
              (error "TIOCSWINSZ failed with errno ~A" errno)))))))
  process)
