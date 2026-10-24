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
  (let* ((compressed (octets 120 156 227 18 145 251 15 0 1 163 1 60))
         (command (parse-graphics-command
                   (encode-graphics-command
                    '((#\f . "32") (#\s . "1") (#\v . "1") (#\o . "z"))
                    compressed))))
    (multiple-value-bind (rgba width height) (graphics-command-rgba command)
      (is (= 1 width))
      (is (= 1 height))
      (is (equalp rgba (octets 10 20 30 255)))))
  (let* ((png (octets
               137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 1
               0 0 0 1 8 6 0 0 0 31 21 196 137 0 0 0 13 73 68 65 84
               120 156 99 224 18 145 251 15 0 1 164 1 60 147 139 14 183
               0 0 0 0 73 69 78 68 174 66 96 130))
         (command (parse-graphics-command
                   (encode-graphics-command '((#\f . "100")) png))))
    (multiple-value-bind (rgba width height) (graphics-command-rgba command)
      (is (= 1 width))
      (is (= 1 height))
      (is (= 4 (length rgba)))))
  t)
