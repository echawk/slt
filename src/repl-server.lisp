(in-package #:slt)

;;;; A Swank or Slynk server, so that SLIME, SLY or another client of those
;;;; protocols can connect to the running terminal.  Start it from the Lisp
;;;; menu (macOS), with Super+Shift+L or Control+Shift+L, with
;;;; --repl-server on the command line, or by calling START-REPL-SERVER from
;;;; the init file.  Both servers and their contribs are loaded in advance,
;;;; since the built executable cannot load systems at run time.

(defvar *repl-server* :swank
  "The server START-REPL-SERVER starts by default: :SWANK for SLIME (and
other clients of its protocol), :SLYNK for SLY.")

(defvar *repl-server-port* 4005
  "The port START-REPL-SERVER listens on by default; 0 picks a free one.")

(defvar *running-repl-server* nil
  "The running server as (KIND . PORT), or NIL.")

(defun repl-server-name (kind)
  (ecase kind (:swank "Swank") (:slynk "Slynk")))

(defun repl-server-running-p ()
  "The kind and port of the running server, or NIL."
  (when *running-repl-server*
    (values (car *running-repl-server*) (cdr *running-repl-server*))))

(defun start-repl-server (&key (kind *repl-server*) (port *repl-server-port*))
  "Start a KIND server (:SWANK or :SLYNK) on localhost PORT and return the
port.  Signals an error when a server is already running or PORT is taken."
  (let ((kind (intern (string-upcase kind) :keyword)))
    (when *running-repl-server*
      (error "A ~a server is already running on port ~d."
             (repl-server-name (car *running-repl-server*))
             (cdr *running-repl-server*)))
    (let ((port (ecase kind
                  (:swank (swank:create-server :port port :dont-close t))
                  (:slynk (slynk:create-server :port port :dont-close t)))))
      (setf *running-repl-server* (cons kind port))
      port)))

(defun stop-repl-server ()
  "Stop listening for connections.  Connected clients stay connected."
  (when *running-repl-server*
    (destructuring-bind (kind . port) *running-repl-server*
      (ecase kind
        (:swank (swank:stop-server port))
        (:slynk (slynk:stop-server port))))
    (setf *running-repl-server* nil)
    t))

(defun toggle-repl-server (&optional emulator)
  "Start the server, or stop the running one, reporting on *ERROR-OUTPUT*.
Return the port of a newly started server."
  (declare (ignore emulator))
  (if *running-repl-server*
      (destructuring-bind (kind . port) *running-repl-server*
        (stop-repl-server)
        (format *error-output* "~&slt: ~a server on port ~d stopped~%"
                (repl-server-name kind) port)
        nil)
      (handler-case
          (let ((port (start-repl-server)))
            (format *error-output* "~&slt: ~a server listening on localhost:~d~%"
                    (repl-server-name *repl-server*) port)
            port)
        (error (condition)
          (format *error-output* "~&slt: could not start the ~a server: ~a~%"
                  (repl-server-name *repl-server*) condition)
          nil))))

(defun repl-server-menu-label ()
  (multiple-value-bind (kind port) (repl-server-running-p)
    (if kind
        (format nil "Stop ~a Server (port ~d)" (repl-server-name kind) port)
        (format nil "Start ~a Server" (repl-server-name *repl-server*)))))

(defun repl-server-key-handler (emulator key modifiers)
  "Toggle the server with Super+Shift+L (Command on macOS) or
Control+Shift+L.  On *KEY-HOOK*; remove it to free the keys for programs
and use the Lisp menu instead."
  (when (and (eql key #\L)              ; Shift is folded into the character.
             (or (member :super modifiers) (member :control modifiers)))
    (toggle-repl-server emulator)
    t))

(add-hook '*key-hook* 'repl-server-key-handler)

(pushnew '(repl-server-menu-label toggle-repl-server) *menu-items* :test #'equal)
