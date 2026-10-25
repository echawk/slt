(defpackage #:terminal-osc/tests
  (:use #:cl #:terminal-osc)
  (:export #:run-tests))

(in-package #:terminal-osc/tests)

(defvar *failures* 0)

(defmacro check (expected form)
  `(let ((actual (multiple-value-list ,form)))
     (unless (equal ,expected actual)
       (incf *failures*)
       (format t "~&FAIL: ~s~%  expected ~s~%  got      ~s~%" ',form ,expected actual))))

(defun octets (string) (babel:string-to-octets string :encoding :utf-8))

(defun run-tests ()
  (setf *failures* 0)
  (check '(52 "c;aGk=") (parse-osc (octets "52;c;aGk=")))
  (check '(nil "x") (parse-osc (octets "abc;x")))
  (check '(2 "") (parse-osc (octets "2")))
  ;; Colors.
  (check '(255 0 128) (parse-color-spec "rgb:ff/00/80"))
  (check '(255 0 128) (parse-color-spec "rgb:ffff/0000/8080"))
  (check '(255 255 255) (parse-color-spec "rgb:f/f/f"))
  (check '(18 52 86) (parse-color-spec "#123456"))
  (check '(16 32 48) (parse-color-spec "#123"))
  (check '(255 0 128) (parse-color-spec "rgbi:1/0/0.5"))
  (check '(nil) (parse-color-spec "rgb:zz/00/00"))
  (check '(nil) (parse-color-spec "rgbi:1/0/(evil)"))
  (check '("rgb:ffff/0000/8080") (format-color-spec 255 0 128))
  (check '(((1 . "?") (2 . "#ffffff"))) (parse-palette-request "1;?;2;#ffffff"))
  (check '(((10 . "?") (11 . "?"))) (parse-dynamic-color-request 10 "?;?"))
  (check '((1 3)) (parse-palette-reset "1;3"))
  (check '(:all) (parse-palette-reset ""))
  ;; Clipboard.
  (check '("c" "hi") (parse-clipboard-request "c;aGk="))
  (check '("s0" :query) (parse-clipboard-request ";?"))
  (check '("c" :clear) (parse-clipboard-request "c;"))
  (check '(nil) (parse-clipboard-request "x;aGk="))
  (check (list (format nil "~c]52;c;aGk=~c\\" (code-char 27) (code-char 27)))
         (encode-clipboard-reply "c" "hi"))
  ;; Hyperlinks.
  (check '("http://x" "a") (parse-hyperlink "id=a;http://x"))
  (check '(nil nil) (parse-hyperlink ";"))
  ;; Working directory.
  (check '("/tmp/a b" "host") (parse-working-directory "file://host/tmp/a%20b"))
  (check '("/" "") (parse-working-directory "file:///"))
  (check '(nil) (parse-working-directory "http://x/y"))
  ;; Shell integration.
  (check '(:prompt-start nil nil) (parse-semantic-prompt "A"))
  (check '(:command-end 3 nil) (parse-semantic-prompt "D;3"))
  (check '(:prompt-start nil (("aid" . "1"))) (parse-semantic-prompt "A;aid=1"))
  ;; Notifications.
  (check '((:title nil :body "done")) (parse-notification 9 "done"))
  (check '((:progress 1 :value 50)) (parse-notification 9 "4;1;50"))
  (check '((:title "T" :body "a;b")) (parse-notification 777 "notify;T;a;b"))
  (let ((assembler (make-notification-assembler)))
    (check '((:id "" :title "Hello" :body nil :urgency nil :occasion :always))
           (notification-assembler-feed assembler ";Hello"))
    (check '(nil) (notification-assembler-feed assembler "i=1:d=0;Hi"))
    (check '((:id "1" :title "Hi" :body "there" :urgency 2 :occasion :unfocused))
           (notification-assembler-feed assembler "i=1:p=body:u=2:o=unfocused;there"))
    (check '((:id "2" :title "é" :body nil :urgency nil :occasion :always))
           (notification-assembler-feed assembler "i=2:e=1;w6k="))
    (check '((:query "q")) (notification-assembler-feed assembler "i=q:p=?;")))
  (format t "~&terminal-osc: ~:[~d failure~:p~;all tests passed~]~%"
          (zerop *failures*) *failures*)
  (zerop *failures*))
