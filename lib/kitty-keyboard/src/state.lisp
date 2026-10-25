(in-package #:kitty-keyboard)

;;;; Progressive enhancement state: flags and a bounded stack of earlier
;;;; flags for each of the main and alternate screens.

(defconstant +disambiguate+ 1)
(defconstant +report-events+ 2)
(defconstant +report-alternates+ 4)
(defconstant +report-all-keys+ 8)
(defconstant +report-text+ 16)
(defconstant +all-flags+ 31)

(defparameter *stack-limit* 16)

(defstruct (screen-keyboard (:copier nil))
  (flags 0 :type (integer 0 31))
  (stack '() :type list))

(defstruct (keyboard-state (:constructor make-keyboard-state ()))
  (primary (make-screen-keyboard) :type screen-keyboard)
  (alternate (make-screen-keyboard) :type screen-keyboard))

(defun screen-keyboard (state screen)
  (ecase screen
    (:primary (keyboard-state-primary state))
    (:alternate (keyboard-state-alternate state))))

(defun keyboard-flags (state screen)
  "The enhancement flags in effect on SCREEN (:PRIMARY or :ALTERNATE)."
  (screen-keyboard-flags (screen-keyboard state screen)))

(defun keyboard-set-flags (state screen flags &optional (mode 1))
  "CSI = FLAGS ; MODE u: MODE 1 sets FLAGS exactly, 2 adds them, 3 removes
them."
  (let ((keyboard (screen-keyboard state screen))
        (flags (logand flags +all-flags+)))
    (setf (screen-keyboard-flags keyboard)
          (case mode
            (2 (logior (screen-keyboard-flags keyboard) flags))
            (3 (logandc2 (screen-keyboard-flags keyboard) flags))
            (t flags)))))

(defun keyboard-push (state screen flags)
  "CSI > FLAGS u: save the current flags and use FLAGS; the oldest saved
entry goes when the stack is full."
  (let ((keyboard (screen-keyboard state screen)))
    (push (screen-keyboard-flags keyboard) (screen-keyboard-stack keyboard))
    (when (> (length (screen-keyboard-stack keyboard)) *stack-limit*)
      (setf (screen-keyboard-stack keyboard)
            (subseq (screen-keyboard-stack keyboard) 0 *stack-limit*)))
    (setf (screen-keyboard-flags keyboard) (logand flags +all-flags+))))

(defun keyboard-pop (state screen &optional (count 1))
  "CSI < COUNT u: restore flags saved COUNT pushes ago; emptying the stack
resets them."
  (let ((keyboard (screen-keyboard state screen)))
    (dotimes (i (max 1 count))
      (if (screen-keyboard-stack keyboard)
          (setf (screen-keyboard-flags keyboard) (pop (screen-keyboard-stack keyboard)))
          (progn (setf (screen-keyboard-flags keyboard) 0)
                 (return))))
    (screen-keyboard-flags keyboard)))

(defun keyboard-reset (state &optional screen)
  "Clear the flags and stack of SCREEN, or of both screens (on RIS)."
  (dolist (which (if screen (list screen) '(:primary :alternate)))
    (let ((keyboard (screen-keyboard state which)))
      (setf (screen-keyboard-flags keyboard) 0
            (screen-keyboard-stack keyboard) '()))))

(defun keyboard-control-sequence (state screen private parameters)
  "Act on CSI PRIVATE PARAMETERS u, the protocol's control sequences.
PRIVATE is the marker character (#\\= #\\> #\\< or #\\?) and PARAMETERS a
list of integers, NIL for empty ones.  Return true when the sequence is the
protocol's, and as second value the reply for the application, if any."
  (flet ((parameter (index default)
           (or (nth index parameters) default)))
    (case private
      (#\= (keyboard-set-flags state screen (parameter 0 0) (parameter 1 1))
       (values t nil))
      (#\> (keyboard-push state screen (parameter 0 0))
       (values t nil))
      (#\< (keyboard-pop state screen (parameter 0 1))
       (values t nil))
      (#\? (values t (format nil "~c[?~du" (code-char 27) (keyboard-flags state screen))))
      (t (values nil nil)))))
