(defpackage #:lispvt/tests
  (:use #:cl #:lispvt)
  (:export #:run-tests))

(in-package #:lispvt/tests)

;;;; Library-level tests.  Escape-sequence behavior is covered more
;;;; broadly by slt's engine conformance suite; these exercise what it
;;;; cannot see: the scanners, scrollback, and state split across reads.

(defvar *failures*)

(defmacro check (form)
  `(unless ,form
     (push ',form *failures*)
     (format t "~&FAIL: ~s~%" ',form)))

(defun esc (control)
  (format nil "~c~a" #\Esc control))

(defun term (rows columns &rest strings)
  (let ((term (make-terminal :rows rows :columns columns :scrollback 100)))
    (dolist (string strings term)
      (terminal-feed-string term string))))

(defun octets (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun test-scanners ()
  (let ((random (sb-ext:seed-random-state 42)))
    (dotimes (trial 2000)
      (let* ((length (random 70 random))
             (data (make-array length :element-type '(unsigned-byte 8))))
        (dotimes (i length)
          (setf (aref data i) (if (zerop (random 12 random))
                                  (random 256 random)
                                  (+ 32 (random 95 random)))))
        (let ((start (random (1+ length) random)))
          (let ((expected (lispvt::scan-printable-scalar data start length)))
            (check (= expected (scan-printable data start length)))
            (check (= expected (lispvt::scan-printable-swar data start length)))))))))

(defun test-widths ()
  (check (= 1 (char-width (char-code #\a))))
  (check (= 2 (char-width #x4E2D)))
  (check (= 2 (char-width #x1F600)))
  (check (= 0 (char-width #x0301)))
  (check (= 0 (char-width #x200D))))

(defun test-split-reads ()
  (let ((term (make-terminal :rows 2 :columns 10))
        (bytes (octets (format nil "~a31mé~a" (esc "[") (esc "[0mx")))))
    (dotimes (i (length bytes))
      (terminal-feed term bytes :start i :end (1+ i)))
    (check (string= "éx" (terminal-row-text term 0)))
    (check (= (palette-color 1) (aref (line-fg (terminal-line term 0)) 0)))
    (check (= 0 (aref (line-fg (terminal-line term 0)) 1)))))

(defun test-scrollback ()
  (let ((term (term 2 5 (format nil "1~c~c2~c~c3~c~c4" #\Return #\Linefeed
                                #\Return #\Linefeed #\Return #\Linefeed))))
    (check (= 2 (terminal-history-count term)))
    (check (string= "3" (terminal-row-text term 0)))
    (terminal-clear-dirty term)
    (terminal-scroll-viewport term -1)
    (check (terminal-row-dirty-p term 0))
    (check (string= "2" (terminal-row-text term 0)))
    (check (string= "3" (terminal-row-text term 1)))
    (check (not (terminal-cursor-visible-p term)))
    (terminal-scroll-viewport term :top)
    (check (string= "1" (terminal-row-text term 0)))
    (terminal-scroll-viewport term :bottom)
    (check (string= "4" (terminal-row-text term 1)))
    (check (terminal-cursor-visible-p term))))

(defun test-scrollback-limit ()
  (let ((term (make-terminal :rows 1 :columns 4 :scrollback 3)))
    (dotimes (i 10)
      (terminal-feed-string term (format nil "~d~c~c" i #\Return #\Linefeed)))
    (check (= 3 (terminal-history-count term)))
    (terminal-scroll-viewport term :top)
    (check (string= "7" (terminal-row-text term 0)))))

(defun test-alternate-screen-has-no-scrollback ()
  (let ((term (term 2 5 (esc "[?1049h") (format nil "a~c~cb~c~cc" #\Return #\Linefeed
                                                 #\Return #\Linefeed))))
    (check (= 0 (terminal-history-count term)))
    (check (terminal-mode term :alternate-screen))
    (terminal-feed-string term (esc "[?1049l"))
    (check (not (terminal-mode term :alternate-screen)))))

(defun test-wide-overwrite ()
  (let ((term (term 1 6 "中文" (esc "[1;2H") "x")))
    ;; Overwriting the right half of 中 blanks its left half.
    (check (string= " x文" (terminal-row-text term 0)))))

(defun test-shrink-moves-lines-to-scrollback ()
  (let ((term (term 4 5 (format nil "a~c~cb~c~cc~c~cd" #\Return #\Linefeed
                                #\Return #\Linefeed #\Return #\Linefeed))))
    (terminal-resize term 2 5)
    (check (string= "c" (terminal-row-text term 0)))
    (check (string= "d" (terminal-row-text term 1)))
    (check (= 1 (terminal-cursor-row term)))
    (check (= 2 (terminal-history-count term)))))

(defun test-colon-sgr ()
  (let ((term (term 1 4 (esc "[38:2::1:2:3m") "a" (esc "[48:5:9;4:2m") "b")))
    (let ((line (terminal-line term 0)))
      (check (= (rgb-color 1 2 3) (aref (line-fg line) 0)))
      (check (= (palette-color 9) (aref (line-bg line) 1)))
      (check (logtest +attr-double-underline+ (aref (line-attrs line) 1))))))

(defun test-mode-report ()
  (let* ((replies '())
         (term (make-terminal :output-function (lambda (s) (push s replies)))))
    (terminal-feed-string term (format nil "~a~a" (esc "[?2004h") (esc "[?2004$p")))
    (check (terminal-mode term :bracketed-paste))
    (check (equal (list (format nil "~c[?2004;1$y" #\Esc)) replies))))

(defun extent-holds-p (term)
  "True when every cell at or past its line's extent is blank with the
default background, on both screens and in the scrollback."
  (flet ((blank-tail-p (line)
           (loop for i from (lispvt::line-extent line) below (lispvt::line-length line)
                 always (and (zerop (aref (line-codes line) i)) (zerop (aref (line-fg line) i))
                             (zerop (aref (line-bg line) i)) (zerop (aref (line-attrs line) i))
                             (or (null (line-ul line)) (zerop (aref (line-ul line) i)))))))
    (and (every #'blank-tail-p (lispvt::screen-lines (lispvt::terminal-primary term)))
         (every #'blank-tail-p (lispvt::screen-lines (lispvt::terminal-alternate term)))
         (loop for i below (terminal-history-count term)
               always (blank-tail-p (lispvt::history-line (lispvt::terminal-primary term) i))))))

(defun test-line-extents ()
  ;; Clearing stops at each line's extent: random edits must never leave a
  ;; written cell past it.
  (let ((*random-state* (sb-ext:seed-random-state 7))
        (pieces (vector "hello" "wide 中文" (esc "[K") (esc "[1K") (esc "[2K") (esc "[J")
                        (esc "[1J") (esc "[2J") (esc "[3X") (esc "[2@") (esc "[3P")
                        (esc "[2L") (esc "[M") (esc "[S") (esc "[T") (esc "[41m") (esc "[m")
                        (esc "[4:3;58;5;2m") (esc "[44m") (esc "#8") (esc "[?1049h")
                        (esc "[?1049l") (esc "[2;4r") (esc "[r") (esc "[4h") (esc "[4l")
                        (format nil "~c~c" #\Return #\Linefeed) (string #\Linefeed)
                        (esc "[3;7H") (esc "[999C") (esc "D") (esc "M"))))
    (dotimes (trial 50)
      (let ((term (make-terminal :rows 6 :columns 12 :scrollback 20)))
        (dotimes (step 60)
          (terminal-feed-string term (aref pieces (random (length pieces))))
          (when (zerop (random 15))
            (terminal-resize term (+ 3 (random 6)) (+ 6 (random 12)))))
        (check (extent-holds-p term))))))

(defun run-tests ()
  (let ((*failures* '()))
    (test-line-extents)
    (test-scanners)
    (test-widths)
    (test-split-reads)
    (test-scrollback)
    (test-scrollback-limit)
    (test-alternate-screen-has-no-scrollback)
    (test-wide-overwrite)
    (test-shrink-moves-lines-to-scrollback)
    (test-colon-sgr)
    (test-mode-report)
    (format t "~&lispvt: ~:[all tests passed~;~:*~d failure~:p~]~%"
            (and *failures* (length *failures*)))
    (null *failures*)))
