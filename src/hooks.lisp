(in-package #:slt)

;;;; Hooks are the main user extension point.  A hook is a special
;;;; variable holding a list of functions; RUN-HOOK calls each in order.
;;;; Users add to them from their init file, e.g.
;;;;
;;;;   (slt:add-hook 'slt:*title-change-hook*
;;;;                 (lambda (emulator title) (format t "title: ~a~%" title)))

(defmacro define-hook (name &optional documentation)
  `(progn
     (defvar ,name '() ,@(when documentation (list documentation)))
     ',name))

(defun add-hook (hook function &key append)
  "Add FUNCTION to HOOK unless it is already present.  With APPEND, run it
after the existing functions instead of before them."
  (unless (member function (symbol-value hook) :test #'equal)
    (setf (symbol-value hook)
          (if append
              (append (symbol-value hook) (list function))
              (cons function (symbol-value hook)))))
  function)

(defun remove-hook (hook function)
  (setf (symbol-value hook)
        (remove function (symbol-value hook) :test #'equal))
  function)

(defun run-hook (hook &rest arguments)
  (dolist (function (symbol-value hook))
    (apply function arguments)))

(defun run-hook-until-success (hook &rest arguments)
  "Call HOOK's functions in order and return the first non-NIL result."
  (dolist (function (symbol-value hook) nil)
    (let ((result (apply function arguments)))
      (when result
        (return result)))))

(define-hook *after-init-hook*
  "Run with the emulator once its window, engine and process exist.")

(define-hook *title-change-hook*
  "Run with the emulator and the new title string when the child sets it.")

(define-hook *bell-hook*
  "Run with the emulator when the child rings the bell.")

(define-hook *key-hook*
  "Run with the emulator, key and modifier list before a key is encoded.
The first function to return non-NIL consumes the key.")

(define-hook *exit-hook*
  "Run with the emulator when it is being destroyed.")
