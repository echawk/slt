(in-package #:slt)

(defun system-version ()
  (or (ignore-errors
        (asdf:component-version (asdf:find-system :slt nil)))
      "0.1.0"))

(defun web-backend-loaded-p ()
  (boundp '*web-host*))

(defun repl-server-loaded-p ()
  (fboundp 'toggle-repl-server))

(defun cli-options ()
  (remove
   nil
   (list
    (clingon:make-option :string
                         :key :backend
                         :short-name #\b
                         :long-name "backend"
                         :description "GUI backend to use.")
    (clingon:make-option :string
                         :key :engine
                         :short-name #\e
                         :long-name "engine"
                         :description "Terminal engine (VT implementation) to use.")
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
    (when (web-backend-loaded-p)
      (clingon:make-option :string
                           :key :web-host
                           :long-name "web-host"
                           :description "Address the web backend listens on (default 127.0.0.1)."))
    (when (web-backend-loaded-p)
      (clingon:make-option :integer
                           :key :web-port
                           :long-name "web-port"
                           :description "Port the web backend listens on (default 8080)."))
    (when (repl-server-loaded-p)
      (clingon:make-option :choice
                           :key :repl-server
                           :long-name "repl-server"
                           :items '("swank" "slynk")
                           :description "Start a Swank (SLIME) or Slynk (SLY) server."))
    (when (repl-server-loaded-p)
      (clingon:make-option :integer
                           :key :repl-port
                           :long-name "repl-port"
                           :description "Port for the Swank or Slynk server (default 4005)."))
    (clingon:make-option :flag
                         :key :no-config
                         :long-name "no-config"
                         :description "Do not load the user init file.")
    (clingon:make-option :flag
                         :key :list-engines
                         :long-name "list-engines"
                         :description "Print the available terminal engines and exit.")
    (clingon:make-option :flag
                         :key :list-backends
                         :long-name "list-backends"
                         :description "Print the available GUI backends and exit."))))

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
                                              :engine
                                              (clingon:getopt command :engine nil)))
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
    (when (clingon:getopt command :no-config nil)
      (setf arguments (append arguments (list :load-config nil))))
    arguments))

(defun handle-cli-command (command &key (launcher #'launch-terminal)
                                      (stream *standard-output*))
  (cond
    ((clingon:getopt command :list-backends nil)
     (dolist (backend (available-backends :probe t))
       (format stream "~(~a~)~%" backend))
     t)
    ((clingon:getopt command :list-engines nil)
     (dolist (engine (available-engines :probe t))
       (format stream "~(~a~)~%" engine))
     t)
    (t
     ;; The web backend and REPL servers are in the slt system, which a
     ;; lean build (slt/app alone) leaves out; their variables are only
     ;; touched when it is loaded.
     (let ((web-variables (and (web-backend-loaded-p) '(*web-host* *web-port*)))
           (repl-server (clingon:getopt command :repl-server nil)))
       (when (repl-server-loaded-p)
         ;; Set globally so the menu and key binding use them too.
         (setf (symbol-value '*repl-server-port*)
               (clingon:getopt command :repl-port (symbol-value '*repl-server-port*)))
         (when repl-server
           (setf (symbol-value '*repl-server*) (intern (string-upcase repl-server) :keyword))
           (funcall 'toggle-repl-server)))
       (progv web-variables
           (and web-variables
                (list (or (clingon:getopt command :web-host nil) (symbol-value '*web-host*))
                      (or (clingon:getopt command :web-port nil) (symbol-value '*web-port*))))
         (apply launcher (command-launch-arguments command)))))))

(defun cli-command-handler (command)
  (handle-cli-command command))

(defun make-cli-command ()
  (clingon:make-command
   :name "slt"
   :description "A terminal emulator with pluggable engines and GUI backends."
   :version (system-version)
   :authors '("Ethan Hawk <ethhawk@iu.edu>")
   :license "MIT"
   :usage "[OPTIONS] [BACKEND]"
   :options (cli-options)
   :handler #'cli-command-handler))

(defun parse-cli-arguments (arguments)
  (clingon:parse-command-line (make-cli-command) arguments))

(defun main (&optional (arguments (uiop:command-line-arguments)))
  "Entry point of bin/slt.  ARGUMENTS default to the process's command line."
  (clingon:run (make-cli-command) arguments))
