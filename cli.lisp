(in-package #:slt)

(defparameter +cli-argument-separator+ (string (code-char 31)))

(defun system-version ()
  (or (ignore-errors
        (asdf:component-version (asdf:find-system :slt nil)))
      "0.1.0"))

(defun cli-options ()
  (list
   (clingon:make-option :string
                        :key :backend
                        :short-name #\b
                        :long-name "backend"
                        :description "GUI backend to use.")
   (clingon:make-option :integer
                        :key :rows
                        :long-name "rows"
                        :description "Initial terminal row count.")
   (clingon:make-option :integer
                        :key :columns
                        :long-name "columns"
                        :description "Initial terminal column count.")
   (clingon:make-option :string
                        :key :font-family
                        :long-name "font-family"
                        :description "Font family or path to use.")
   (clingon:make-option :integer
                        :key :font-size
                        :long-name "font-size"
                        :description "Font size for the terminal.")
   (clingon:make-option :integer
                        :key :cell-width
                        :long-name "cell-width"
                        :description "Override the measured cell width.")
   (clingon:make-option :integer
                        :key :cell-height
                        :long-name "cell-height"
                        :description "Override the measured cell height.")
   (clingon:make-option :string
                        :key :shell
                        :long-name "shell"
                        :description "Shell executable to spawn.")
   (clingon:make-option :string
                        :key :term-name
                        :long-name "term"
                        :description "TERM value to expose to the child process.")
   (clingon:make-option :integer
                        :key :poll-interval
                        :long-name "poll-interval"
                        :description "Process poll interval in milliseconds.")
   (clingon:make-option :string
                        :key :title
                        :long-name "title"
                        :description "Window title.")
   (clingon:make-option :flag
                        :key :list-backends
                        :long-name "list-backends"
                        :description "Print the available GUI backends and exit.")))

(defun cli-backend-argument (command)
  (let ((free-arguments (clingon:command-arguments command)))
    (when (= (length free-arguments) 1)
      (first free-arguments))))

(defun %append-keyword-argument (arguments key value)
  (if (null value)
      arguments
      (append arguments (list key value))))

(defun command-launch-arguments (command)
  (let ((arguments '()))
    (setf arguments (%append-keyword-argument arguments
                                              :backend
                                              (or (clingon:getopt command :backend nil)
                                                  (cli-backend-argument command))))
    (setf arguments (%append-keyword-argument arguments
                                              :rows
                                              (clingon:getopt command :rows nil)))
    (setf arguments (%append-keyword-argument arguments
                                              :columns
                                              (clingon:getopt command :columns nil)))
    (setf arguments (%append-keyword-argument arguments
                                              :font-family
                                              (clingon:getopt command :font-family nil)))
    (setf arguments (%append-keyword-argument arguments
                                              :font-size
                                              (clingon:getopt command :font-size nil)))
    (setf arguments (%append-keyword-argument arguments
                                              :cell-width
                                              (clingon:getopt command :cell-width nil)))
    (setf arguments (%append-keyword-argument arguments
                                              :cell-height
                                              (clingon:getopt command :cell-height nil)))
    (setf arguments (%append-keyword-argument arguments
                                              :shell
                                              (clingon:getopt command :shell nil)))
    (setf arguments (%append-keyword-argument arguments
                                              :term-name
                                              (clingon:getopt command :term-name nil)))
    (setf arguments (%append-keyword-argument arguments
                                              :poll-interval
                                              (clingon:getopt command :poll-interval nil)))
    (setf arguments (%append-keyword-argument arguments
                                              :title
                                              (clingon:getopt command :title nil)))
    arguments))

(defun handle-cli-command (command &key (launcher #'launch-terminal)
                                      (stream *standard-output*))
  (cond
    ((clingon:getopt command :list-backends nil)
     (dolist (backend (available-backends))
       (format stream "~(~a~)~%" backend))
     t)
    (t
     (apply launcher (command-launch-arguments command)))))

(defun cli-command-handler (command)
  (handle-cli-command command))

(defun make-cli-command ()
  (clingon:make-command
   :name "slt"
   :description "A small terminal emulator with pluggable GUI backends."
   :version (system-version)
   :authors '("Ethan Hawk <ethhawk@iu.edu>")
   :license "MIT"
   :usage "[OPTIONS] [BACKEND]"
   :options (cli-options)
   :handler #'cli-command-handler))

(defun parse-cli-arguments (arguments)
  (clingon:parse-command-line (make-cli-command) arguments))

(defun environment-cli-arguments (&optional (value (uiop:getenv "SLT_CLI_ARGS")))
  (unless (or (null value) (string= value ""))
    (uiop:split-string value :separator +cli-argument-separator+)))

(defun main (&optional arguments)
  (clingon:run (make-cli-command)
               (or arguments
                   (environment-cli-arguments))))
