(defpackage #:kitty-graphics/tests
  (:use #:cl #:kitty-graphics)
  (:export #:run-tests))

(in-package #:kitty-graphics/tests)

(defmacro is (form)
  `(assert ,form))

(defun octets (&rest values)
  (make-array (length values) :element-type '(unsigned-byte 8)
                              :initial-contents values))

(defun run-tests ()
  (let* ((data (octets 255 0 0 0 255 0))
         (encoded (encode-graphics-command
                   '((#\a . "T") (#\f . "24") (#\s . "2") (#\v . "1")) data))
         (command (parse-graphics-command encoded)))
    (is (char= #\T (graphics-command-action command)))
    (is (= 24 (graphics-command-integer command #\f)))
    (is (equalp data (graphics-command-data command)))
    (multiple-value-bind (rgba width height) (graphics-command-rgba command)
      (is (= width 2))
      (is (= height 1))
      (is (equalp rgba (octets 255 0 0 255 0 255 0 255)))))
  (let ((decoder (make-graphics-decoder)))
    (is (null (graphics-decoder-feed decoder "Ga=T,f=32,s=1,v=1,m=1;/w")))
    (let ((command (graphics-decoder-feed decoder "Gm=0;A=")))
      (is command)
      (is (equalp (octets 255 0) (graphics-command-data command)))
      (is (null (graphics-command-value command #\m)))))
  (let ((response (encode-graphics-response '((#\i . "42")) "OK")))
    (is (search "_Gi=42;OK" response)))
  t)
