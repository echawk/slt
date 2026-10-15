;;;; tests.lisp -- self-contained tests for the libvterm binding.

(defpackage #:libvterm/tests
  (:use #:cl #:libvterm)
  (:export #:run-tests))

(in-package #:libvterm/tests)

(defvar *tests* '())
(defvar *failures* 0)
(defvar *checks* 0)

(defmacro deftest (name &body body)
  `(progn
     (defun ,name () ,@body)
     (setf *tests* (append (remove ',name *tests*) (list ',name)))
     ',name))

(defmacro check (form &optional (expected t expected-p))
  `(let ((got ,form))
     (incf *checks*)
     (unless ,(if expected-p `(equalp got ,expected) 'got)
       (incf *failures*)
       (format t "~&  FAIL ~S~%       got ~S~:[~;~%  expected ~S~]~%"
               ',form got ,expected-p ,expected))))

(defun run-tests ()
  (let ((*failures* 0) (*checks* 0) (failed-tests 0))
    (dolist (test *tests*)
      (let ((before *failures*))
        (handler-case (funcall test)
          (error (e)
            (incf *failures*)
            (format t "~&  ERROR in ~S: ~A~%" test e)))
        (if (= before *failures*)
            (format t "~&ok   ~(~A~)~%" test)
            (progn (incf failed-tests)
                   (format t "~&FAIL ~(~A~)~%" test)))))
    (format t "~&~D tests, ~D checks, ~D failed tests, ~D failed checks~%"
            (length *tests*) *checks* failed-tests *failures*)
    (zerop *failures*)))

(defun esc (&rest parts)
  "Concatenate PARTS, with :esc standing for ESC."
  (format nil "~{~A~}" (mapcar (lambda (p) (if (eq p :esc) (string (code-char 27)) p)) parts)))

(defun octets (string) (map '(vector (unsigned-byte 8)) #'char-code string))

;;; ------------------------------------------------------------------

(deftest plain-text
  (with-vterm (vt :rows 5 :columns 20)
    (check (vterm-feed vt "hello") 5)
    (check (vterm-row-text vt 0) "hello")
    (check (getf (vterm-cell vt 0 0) :codepoints) (list (char-code #\h)))
    (vterm-feed vt (format nil "~C~Cworld" #\Return #\Newline))
    (check (vterm-row-text vt 1) "world")
    (check (vterm-screen-text vt) (format nil "hello~%world~%~%~%"))
    ;; octet vectors with :start/:end
    (vterm-feed vt (coerce (octets "xxABCxx") '(simple-array (unsigned-byte 8) (*)))
                :start 2 :end 5)
    (check (vterm-row-text vt 1) "worldABC")))

(deftest cup-ed-el
  (with-vterm (vt :rows 5 :columns 20)
    (vterm-feed vt (esc :esc "[3;5H" "X"))
    (check (getf (vterm-cell vt 2 4) :codepoints) (list (char-code #\X)))
    (check (vterm-row-text vt 2) "    X")
    (vterm-feed vt (esc :esc "[1;1Habcdef" :esc "[1;4H" :esc "[K"))
    (check (vterm-row-text vt 0) "abc")
    (vterm-feed vt (esc :esc "[1;2H" :esc "[1K"))
    (check (vterm-row-text vt 0) "  c")
    (vterm-feed vt (esc :esc "[2J"))
    (check (vterm-screen-text vt) (format nil "~%~%~%~%"))))

(deftest sgr-palette
  (with-vterm (vt)
    (vterm-feed vt (esc :esc "[31mR" :esc "[0mN" :esc "[44;95mB" :esc "[38;5;200mI"))
    (check (getf (vterm-cell vt 0 0) :fg) '(:palette 1))
    (check (getf (vterm-cell vt 0 0) :bg) '(:default))
    (check (getf (vterm-cell vt 0 1) :fg) '(:default))
    (check (getf (vterm-cell vt 0 2) :fg) '(:palette 13))
    (check (getf (vterm-cell vt 0 2) :bg) '(:palette 4))
    (check (getf (vterm-cell vt 0 3) :fg) '(:palette 200))))

(deftest sgr-truecolor
  (with-vterm (vt)
    (vterm-feed vt (esc :esc "[38;2;10;20;30;48;2;200;100;0mT"))
    (check (getf (vterm-cell vt 0 0) :fg) '(:rgb 10 20 30))
    (check (getf (vterm-cell vt 0 0) :bg) '(:rgb 200 100 0))))

(deftest sgr-attributes
  (with-vterm (vt)
    (vterm-feed vt (esc :esc "[1mB" :esc "[0;7mI" :esc "[0;1;3;4;9mX" :esc "[0;21mD"
                        :esc "[0;8;5mC" :esc "[0mP"))
    (check (getf (vterm-cell vt 0 0) :attrs) '(:bold))
    (check (getf (vterm-cell vt 0 1) :attrs) '(:inverse))
    (check (getf (vterm-cell vt 0 2) :attrs) '(:bold :italic :underline :strike))
    (check (getf (vterm-cell vt 0 3) :attrs) '(:double-underline))
    (check (getf (vterm-cell vt 0 4) :attrs) '(:blink :conceal))
    (check (getf (vterm-cell vt 0 5) :attrs) '())))

(deftest wide-char
  (with-vterm (vt :rows 3 :columns 10)
    (vterm-feed vt (format nil "a~Cb" (code-char #x4E2D)))
    (check (getf (vterm-cell vt 0 1) :codepoints) (list #x4E2D))
    (check (getf (vterm-cell vt 0 1) :width) 2)
    (check (getf (vterm-cell vt 0 2) :width) 0)
    (check (getf (vterm-cell vt 0 3) :codepoints) (list (char-code #\b)))
    (check (nth-value 1 (vterm-cursor vt)) 4)
    (check (vterm-row-text vt 0) (format nil "a~Cb" (code-char #x4E2D)))))

(deftest combining-grapheme
  (with-vterm (vt :rows 3 :columns 10)
    (vterm-feed vt (format nil "e~Cx" (code-char #x301)))
    (check (getf (vterm-cell vt 0 0) :codepoints) (list (char-code #\e) #x301))
    (check (getf (vterm-cell vt 0 0) :width) 1)
    (check (getf (vterm-cell vt 0 1) :codepoints) (list (char-code #\x)))
    (let ((codes (make-row-buffer 10)) (attrs (make-row-buffer 10)))
      (vterm-row-into vt 0 codes nil nil attrs)
      (check (aref codes 0) (char-code #\e))
      (check (attrs-combining-p (aref attrs 0)))
      (check (not (attrs-combining-p (aref attrs 1)))))))

(deftest row-into
  (with-vterm (vt :rows 3 :columns 8)
    (vterm-feed vt (format nil "~A~C" (esc "A" :esc "[1;31mB" :esc "[0;48;2;1;2;3mC" :esc "[0m")
                           (code-char #x4E2D)))
    (let ((codes (make-row-buffer 8)) (fg (make-row-buffer 8))
          (bg (make-row-buffer 8)) (attrs (make-row-buffer 8)))
      (check (vterm-row-into vt 0 codes fg bg attrs) 8)
      (check (subseq codes 0 6) (vector 65 66 67 #x4E2D 0 0))
      (check (aref fg 0) 0)
      (check (aref fg 1) #x01000001)
      (check (aref bg 2) #x02010203)
      (check (decode-color (aref bg 2)) '(:rgb 1 2 3))
      (check (decode-attrs (aref attrs 1)) '(:bold))
      (check (mapcar #'attrs-width (coerce (subseq attrs 0 6) 'list)) '(1 1 1 2 0 1))
      ;; partial row: only columns 1..3 are touched
      (fill codes 99)
      (check (vterm-row-into vt 0 codes nil nil nil :start 1 :end 3) 2)
      (check (subseq codes 0 4) (vector 99 66 67 99)))))

(deftest cursor-position
  (with-vterm (vt :rows 10 :columns 20)
    (check (multiple-value-list (vterm-cursor vt)) '(0 0 t))
    (vterm-feed vt (esc :esc "[5;7H"))
    (check (multiple-value-list (vterm-cursor vt)) '(4 6 t))
    (vterm-feed vt (esc :esc "[?25l"))
    (check (nth-value 2 (vterm-cursor vt)) nil)
    (vterm-feed vt (esc :esc "[?25h" "ab"))
    (check (multiple-value-list (vterm-cursor vt)) '(4 8 t))))

(deftest device-attributes-reply
  (let ((out '()))
    (with-vterm (vt :on-output (lambda (v o) (declare (ignore v)) (push o out)))
      (vterm-feed vt (esc :esc "[c"))
      (let ((reply (map 'string #'code-char (apply #'concatenate 'vector (reverse out)))))
        (check (and (plusp (length reply))
                    (string= (esc :esc "[?") reply :end2 3)
                    (char= (char reply (1- (length reply))) #\c)))
        (setf out '())
        (vterm-feed vt (esc :esc "[3;4H" :esc "[6n"))
        (check (map 'string #'code-char (apply #'concatenate 'vector (reverse out)))
               (esc :esc "[3;4R"))))))

(deftest title-osc
  (let ((seen '()))
    (with-vterm (vt :on-title (lambda (v s) (declare (ignore v)) (push s seen)))
      (check (vterm-title vt) nil)
      (vterm-feed vt (esc :esc "]2;hello world" (code-char 7)))
      (check (vterm-title vt) "hello world")
      ;; split across feeds -> several fragments, assembled
      (vterm-feed vt (esc :esc "]2;sp"))
      (vterm-feed vt (format nil "l~Cit" (code-char #xE9)))
      (vterm-feed vt (esc :esc "\\"))
      (check (vterm-title vt) (format nil "spl~Cit" (code-char #xE9)))
      (check (reverse seen) (list "hello world" (format nil "spl~Cit" (code-char #xE9)))))))

(deftest resize
  (with-vterm (vt :rows 5 :columns 10)
    (vterm-feed vt "abc")
    (vterm-take-damage vt)
    (vterm-resize vt 8 30)
    (check (list (vterm-rows vt) (vterm-columns vt)) '(8 30))
    (check (length (vterm-take-damage vt)) 8)
    (check (vterm-row-text vt 0) "abc")
    (vterm-feed vt (esc :esc "[8;30HZ"))
    (check (getf (vterm-cell vt 7 29) :codepoints) (list (char-code #\Z)))))

(deftest damage
  (with-vterm (vt :rows 6 :columns 10)
    (vterm-take-damage vt)
    (multiple-value-bind (bv any) (vterm-take-damage vt)
      (check bv #*000000)
      (check any nil))
    (vterm-feed vt (esc :esc "[3;1Hxy"))
    (multiple-value-bind (bv any) (vterm-take-damage vt)
      ;; row 0 (old cursor row) and row 2 (written + new cursor)
      (check bv #*101000)
      (check any t))
    (check (vterm-take-damage vt) #*000000)
    ;; scrolling: newline at the bottom moves every row
    (vterm-feed vt (esc :esc "[6;1H"))
    (vterm-take-damage vt)
    (vterm-feed vt (format nil "~C~C" #\Return #\Newline))
    (check (vterm-take-damage vt) #*111111)
    (check (vterm-row-text vt 1) "xy")))

(deftest key-cursor-modes
  (with-vterm (vt)
    (check (vterm-encode-key vt :up) (octets (esc :esc "[A")))
    (vterm-feed vt (esc :esc "[?1h"))
    (check (vterm-encode-key vt :up) (octets (esc :esc "OA")))
    (vterm-feed vt (esc :esc "[?1l"))
    (check (vterm-encode-key vt :up) (octets (esc :esc "[A")))
    (check (vterm-encode-key vt :up :mods '(:ctrl)) (octets (esc :esc "[1;5A")))
    (check (vterm-encode-key vt :f1) (octets (esc :esc "OP")))
    (check (vterm-encode-key vt :f5) (octets (esc :esc "[15~")))
    (check (vterm-encode-key vt :enter) (octets (string #\Return)))
    (check (vterm-encode-char vt #\a) (octets "a"))
    (check (vterm-encode-char vt #\c :mods '(:ctrl)) (vector 3))
    (check (vterm-encode-char vt #x4E2D) (vector #xE4 #xB8 #xAD))
    ;; vterm-key goes through the output closure
    (let ((got nil))
      (setf (vterm-on-output vt) (lambda (v o) (declare (ignore v)) (setf got o)))
      (vterm-key vt :down)
      (check got (octets (esc :esc "[B"))))))

(deftest two-terminals
  (let ((out-a '()) (out-b '()) (bells '()))
    (with-vterm (a :rows 4 :columns 10
                   :on-output (lambda (v o) (declare (ignore v)) (push o out-a))
                   :on-bell (lambda (v) (push v bells)))
      (with-vterm (b :rows 6 :columns 12
                     :on-output (lambda (v o) (declare (ignore v)) (push o out-b))
                     :on-bell (lambda (v) (push v bells)))
        (vterm-feed a (esc "AAA" :esc "]2;ta" (code-char 7)))
        (vterm-feed b (esc "BB" :esc "]2;tb" (code-char 7) (code-char 7)))
        (vterm-feed a (esc :esc "[c"))
        (check (vterm-row-text a 0) "AAA")
        (check (vterm-row-text b 0) "BB")
        (check (vterm-title a) "ta")
        (check (vterm-title b) "tb")
        (check (and out-a t))
        (check out-b '())
        (check (equal bells (list b)))
        (vterm-key b :up)
        (check (length out-b) 1)
        (check (length out-a) 1)))))

(deftest errors-in-callbacks
  (with-vterm (vt :on-bell (lambda (v) (declare (ignore v)) (error "boom")))
    (check (handler-case (progn (vterm-feed vt (string (code-char 7))) :no-error)
             (error () :signalled))
           :signalled)
    ;; terminal is still usable
    (vterm-feed vt "ok")
    (check (vterm-row-text vt 0) "ok")))

(deftest altscreen
  (with-vterm (vt :rows 3 :columns 10)
    (vterm-feed vt "main")
    (vterm-feed vt (esc :esc "[?1049h"))
    (check (vterm-altscreen-p vt))
    (check (vterm-row-text vt 0) "")
    (vterm-feed vt "alt")
    (vterm-feed vt (esc :esc "[?1049l"))
    (check (vterm-altscreen-p vt) nil)
    (check (vterm-row-text vt 0) "main")))

(deftest scrollback
  (let ((lines '()))
    (with-vterm (vt :rows 2 :columns 4
                    :on-scrollback (lambda (v codes fg bg attrs)
                                     (declare (ignore v fg bg attrs))
                                     (push (map 'string #'code-char (remove 0 codes)) lines)))
      (vterm-feed vt (format nil "one~C~Ctwo~C~Cthr" #\Return #\Newline #\Return #\Newline))
      (check lines '("one")))))

(deftest freed
  (let ((vt (make-vterm)))
    (check (vterm-live-p vt))
    (vterm-free vt)
    (vterm-free vt)
    (check (vterm-live-p vt) nil)
    (check (handler-case (progn (vterm-feed vt "x") :no-error) (error () :error)) :error)))
