(in-package #:slt)

(defun web-frontend-loaded-p ()
  (boundp '*web-host*))

(defun repl-server-loaded-p ()
  (fboundp 'toggle-repl-server))

(defun cli-options ()
  (remove
   nil
   (list
    (clingon:make-option :string
                         :key :frontend
                         :short-name #\f
                         :long-name "frontend"
                         :description "GUI frontend to use.")
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
    (when (web-frontend-loaded-p)
      (clingon:make-option :string
                           :key :web-host
                           :long-name "web-host"
                           :description "Address the web frontend listens on (default 127.0.0.1)."))
    (when (web-frontend-loaded-p)
      (clingon:make-option :integer
                           :key :web-port
                           :long-name "web-port"
                           :description "Port the web frontend listens on (default 8080)."))
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
    (clingon:make-option :filepath
                         :key :config
                         :short-name #\c
                         :long-name "config"
                         :description "Load this init file instead of searching for one.")
    (clingon:make-option :filepath
                         :key :directory
                         :short-name #\d
                         :long-name "directory"
                         :description "Start the shell in this directory.")
    (clingon:make-option :flag
                         :key :login
                         :short-name #\l
                         :long-name "login"
                         :description "Start the shell as a login shell (macOS).")
    (clingon:make-option :flag
                         :key :smooth-scroll
                         :long-name "smooth-scroll"
                         :description "Scroll the scrollback pixel by pixel with a trackpad (also SLT_SMOOTH_SCROLL=1).")
    (clingon:make-option :flag
                         :key :no-config
                         :long-name "no-config"
                         :description "Do not load the user init file.")
    (clingon:make-option :flag
                         :key :list-engines
                         :long-name "list-engines"
                         :description "Print the available terminal engines and exit.")
    (clingon:make-option :flag
                         :key :list-frontends
                         :long-name "list-frontends"
                         :description "Print the available GUI frontends and exit."))))

(defun cli-frontend-argument (command)
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
                                              :frontend
                                              (or (clingon:getopt command :frontend nil)
                                                  (cli-frontend-argument command))))
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
    (setf arguments (%append-keyword-argument arguments
                                              :directory
                                              (clingon:getopt command :directory nil)))
    (when (clingon:getopt command :login nil)
      (setf arguments (append arguments (list :login-shell t))))
    (when (clingon:getopt command :no-config nil)
      (setf arguments (append arguments (list :load-config nil))))
    arguments))

(defun handle-cli-command (command &key (launcher #'launch-terminal)
                                      (stream *standard-output*))
  (cond
    ((clingon:getopt command :list-frontends nil)
     (dolist (frontend (available-frontends :probe t))
       (format stream "~(~a~)~%" frontend))
     t)
    ((clingon:getopt command :list-engines nil)
     (dolist (engine (available-engines :probe t))
       (format stream "~(~a~)~%" engine))
     t)
    (t
     ;; The web frontend and REPL servers are in the slt system, which a
     ;; lean build (slt/app alone) leaves out; their variables are only
     ;; touched when it is loaded.
     (let ((web-variables (and (web-frontend-loaded-p) '(*web-host* *web-port*)))
           (repl-server (clingon:getopt command :repl-server nil)))
       (when (clingon:getopt command :config nil)
         (setf *config-file* (clingon:getopt command :config nil)))
       (when (clingon:getopt command :smooth-scroll nil)
         (setf *smooth-scrolling* t))
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
   :description "A terminal emulator with pluggable engines and GUI frontends."
   :version (system-version)
   :authors '("Ethan Hawk <ethhawk@iu.edu>")
   :license "MIT"
   :usage "[OPTIONS] [FRONTEND]"
   :options (cli-options)
   :handler #'cli-command-handler))

(defun parse-cli-arguments (arguments)
  (clingon:parse-command-line (make-cli-command) arguments))

(defun main (&optional (arguments (uiop:command-line-arguments)))
  "Entry point of bin/slt.  ARGUMENTS default to the process's command line."
  ;; Older macOS passes -psn_0_NNNN to applications the Finder starts.
  (clingon:run (make-cli-command)
               (remove-if (lambda (argument) (uiop:string-prefix-p "-psn_" argument))
                          arguments)))
