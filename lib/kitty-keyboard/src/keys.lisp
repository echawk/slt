(in-package #:kitty-keyboard)

;;;; Keys and modifiers.  A key is a character (the unshifted key, such as
;;;; #\a for both a and A) or a keyword naming a functional key.

(defparameter *functional-keys* (make-hash-table)
  "Keyword -> (CODE FORM LETTER-OR-NUMBER): FORM is :U for CSI CODE u, :TILDE
for CSI NUMBER ~ or :LETTER for CSI 1 LETTER.")

(defmacro define-functional-keys (&body entries)
  `(progn
     ,@(loop for (name code form extra) in entries
             collect `(setf (gethash ,name *functional-keys*) (list ,code ,form ,extra)))))

(define-functional-keys
  (:escape 27 :u) (:enter 13 :u) (:tab 9 :u) (:backspace 127 :u)
  (:insert 2 :tilde 2) (:delete 3 :tilde 3)
  (:page-up 5 :tilde 5) (:page-down 6 :tilde 6)
  (:left 1 :letter #\D) (:right 1 :letter #\C) (:up 1 :letter #\A) (:down 1 :letter #\B)
  (:home 1 :letter #\H) (:end 1 :letter #\F)
  (:f1 1 :letter #\P) (:f2 1 :letter #\Q) (:f3 13 :tilde 13) (:f4 1 :letter #\S)
  (:f5 15 :tilde 15) (:f6 17 :tilde 17) (:f7 18 :tilde 18) (:f8 19 :tilde 19)
  (:f9 20 :tilde 20) (:f10 21 :tilde 21) (:f11 23 :tilde 23) (:f12 24 :tilde 24)
  (:caps-lock 57358 :u) (:scroll-lock 57359 :u) (:num-lock 57360 :u)
  (:print-screen 57361 :u) (:pause 57362 :u) (:menu 57363 :u)
  (:kp-0 57399 :u) (:kp-1 57400 :u) (:kp-2 57401 :u) (:kp-3 57402 :u) (:kp-4 57403 :u)
  (:kp-5 57404 :u) (:kp-6 57405 :u) (:kp-7 57406 :u) (:kp-8 57407 :u) (:kp-9 57408 :u)
  (:kp-decimal 57409 :u) (:kp-divide 57410 :u) (:kp-multiply 57411 :u)
  (:kp-subtract 57412 :u) (:kp-add 57413 :u) (:kp-enter 57414 :u) (:kp-equal 57415 :u)
  (:kp-separator 57416 :u) (:kp-left 57417 :u) (:kp-right 57418 :u) (:kp-up 57419 :u)
  (:kp-down 57420 :u) (:kp-page-up 57421 :u) (:kp-page-down 57422 :u)
  (:kp-home 57423 :u) (:kp-end 57424 :u) (:kp-insert 57425 :u) (:kp-delete 57426 :u)
  (:kp-begin 1 :letter #\E)
  (:media-play 57428 :u) (:media-pause 57429 :u) (:media-play-pause 57430 :u)
  (:media-reverse 57431 :u) (:media-stop 57432 :u) (:media-fast-forward 57433 :u)
  (:media-rewind 57434 :u) (:media-track-next 57435 :u) (:media-track-previous 57436 :u)
  (:media-record 57437 :u) (:lower-volume 57438 :u) (:raise-volume 57439 :u)
  (:mute-volume 57440 :u)
  (:left-shift 57441 :u) (:left-control 57442 :u) (:left-alt 57443 :u)
  (:left-super 57444 :u) (:left-hyper 57445 :u) (:left-meta 57446 :u)
  (:right-shift 57447 :u) (:right-control 57448 :u) (:right-alt 57449 :u)
  (:right-super 57450 :u) (:right-hyper 57451 :u) (:right-meta 57452 :u)
  (:iso-level3-shift 57453 :u) (:iso-level5-shift 57454 :u))

;; F13-F35.
(loop for n from 13 to 35
      do (setf (gethash (intern (format nil "F~d" n) :keyword) *functional-keys*)
               (list (+ 57376 (- n 13)) :u nil)))

(defparameter *modifier-keys*
  '(:left-shift :left-control :left-alt :left-super :left-hyper :left-meta
    :right-shift :right-control :right-alt :right-super :right-hyper :right-meta
    :iso-level3-shift :iso-level5-shift :caps-lock :num-lock :scroll-lock))

(defparameter *keypad-legacy*
  '((:kp-0 . #\0) (:kp-1 . #\1) (:kp-2 . #\2) (:kp-3 . #\3) (:kp-4 . #\4)
    (:kp-5 . #\5) (:kp-6 . #\6) (:kp-7 . #\7) (:kp-8 . #\8) (:kp-9 . #\9)
    (:kp-decimal . #\.) (:kp-divide . #\/) (:kp-multiply . #\*) (:kp-subtract . #\-)
    (:kp-add . #\+) (:kp-equal . #\=) (:kp-separator . #\,)
    (:kp-enter . :enter) (:kp-left . :left) (:kp-right . :right) (:kp-up . :up)
    (:kp-down . :down) (:kp-page-up . :page-up) (:kp-page-down . :page-down)
    (:kp-home . :home) (:kp-end . :end) (:kp-insert . :insert) (:kp-delete . :delete))
  "What keypad keys are in the legacy encoding: their main-keyboard twins.")

(defun functional-key-p (key)
  (and (keywordp key) (gethash key *functional-keys*) t))

(defun key-code (key)
  "The protocol's number for KEY."
  (if (characterp key)
      (char-code (char-downcase key))
      (first (or (gethash key *functional-keys*)
                 (error "Unknown key ~s" key)))))

(defparameter *modifier-values*
  '((:shift . 1) (:alt . 2) (:control . 4) (:super . 8) (:hyper . 16) (:meta . 32)
    (:caps-lock . 64) (:num-lock . 128)))

(defun modifier-bits (modifiers)
  "The protocol's bit field for a list of modifier keywords."
  (loop for modifier in modifiers
        sum (or (cdr (assoc modifier *modifier-values*))
                (error "Unknown modifier ~s" modifier))))

(defconstant +lock-bits+ (logior 64 128))

(defun ctrl-byte (char)
  "The C0 control (or other byte) Ctrl turns CHAR into in the legacy
encoding, or NIL when Ctrl leaves it alone."
  (let ((code (char-code char)))
    (cond
      ((char<= #\a char #\z) (1+ (- code (char-code #\a))))
      ((member char '(#\Space #\2 #\@)) 0)
      ((member char '(#\3 #\[)) 27)
      ((member char '(#\4 #\\)) 28)
      ((member char '(#\5 #\])) 29)
      ((member char '(#\6 #\^ #\~)) 30)
      ((member char '(#\7 #\/ #\_)) 31)
      ((member char '(#\8 #\?)) 127)
      (t nil))))

(defparameter *legacy-text-keys* "abcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',./"
  "Keys the legacy encoding gives Shift, Alt and Ctrl meanings.")

(defparameter *us-shifted* '((#\` . #\~) (#\1 . #\!) (#\2 . #\@) (#\3 . #\#) (#\4 . #\$)
                             (#\5 . #\%) (#\6 . #\^) (#\7 . #\&) (#\8 . #\*) (#\9 . #\()
                             (#\0 . #\)) (#\- . #\_) (#\= . #\+) (#\[ . #\{) (#\] . #\})
                             (#\\ . #\|) (#\; . #\:) (#\' . #\") (#\, . #\<) (#\. . #\>)
                             (#\/ . #\?)))

(defun shifted-char (char)
  (or (cdr (assoc char *us-shifted*)) (char-upcase char)))

(defun unshifted-key (char)
  "The key typing CHAR, and whether Shift was needed: #\\A gives #\\a and
T, #\\! gives #\\1 and T (on a US layout), #\\a gives #\\a and NIL."
  (let ((base (car (rassoc char *us-shifted*))))
    (cond (base (values base t))
          ((char/= char (char-downcase char)) (values (char-downcase char) t))
          (t (values char nil)))))
