(defpackage #:simd-base64/tests
  (:use #:cl #:simd-base64)
  (:export #:run-tests))

(in-package #:simd-base64/tests)

(defvar *failures* 0)

(defmacro is (form)
  `(unless ,form
     (incf *failures*)
     (format t "~&FAIL: ~s~%" ',form)))

(defun octets (string)
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code string))

(defun random-octets (count)
  (let ((octets (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (i count octets)
      (setf (aref octets i) (random 256)))))

(defun reference-encode (octets)
  "OCTETS' base64 as cl-base64 writes it."
  (octets (cl-base64:usb8-array-to-base64-string octets)))

(defun rgba (rgb)
  "RGB as RGBA; an incomplete last pixel keeps zeros for what it lacks."
  (let ((pixels (make-array (* 4 (ceiling (length rgb) 3)) :element-type '(unsigned-byte 8)
                                                           :initial-element 0)))
    (dotimes (i (length rgb) pixels)
      (setf (aref pixels (+ i (floor i 3))) (aref rgb i))
      (when (= 2 (mod i 3))
        (setf (aref pixels (+ i (floor i 3) 1)) 255)))))

(defun split (octets sizes)
  "OCTETS in pieces of SIZES, cycled."
  (loop with start = 0
        for i from 0
        for size = (nth (mod i (length sizes)) sizes)
        while (< start (length octets))
        collect (subseq octets start (min (length octets) (+ start size)))
        do (incf start size)))

(defun feed-all (decoder parts)
  (dolist (part parts)
    (decoder-feed decoder part))
  (decoder-finish decoder))

(defun test-round-trips ()
  ;; Lengths around the 16-digit steps, every remainder of 3.
  (loop for length in '(0 1 2 3 4 11 12 13 24 47 48 49 100 1000 4099)
        for data = (random-octets length)
        for encoded = (reference-encode data)
        do (is (equalp data (decode encoded)))
           (is (equalp (rgba data) (decode encoded :layout :rgba)))
           ;; Split anywhere, quartets included.
           (dolist (sizes '((1) (3) (5 17) (16) (4095 1) (7 33 2)))
             (is (equalp data (decode (split encoded sizes))))
             (is (equalp (rgba data) (decode (split encoded sizes) :layout :rgba))))))

(defun test-padding ()
  (is (equalp (octets "ab") (decode (octets "YWI"))))
  (is (equalp (octets "ab") (decode (octets "YWI="))))
  ;; Separately padded pieces, joined.
  (is (equalp (octets "aab") (decode (octets "YQ==YWI="))))
  (is (equalp (octets "aab") (decode (list (octets "YQ=") (octets "=YWI"))))))

(defun test-errors ()
  (flet ((fails (string)
           (handler-case (progn (decode (octets string)) nil)
             (base64-error () t))))
    (is (fails "YW*i"))
    (is (fails "Y"))
    (is (fails "YWJjY"))
    ;; A bad digit within a 16-digit run.
    (is (fails "QUJDREVGR0hJSktM!05PUFFSU1RVVldY"))
    (is (not (fails "")))))

(defun test-output-bounds ()
  ;; Octets past the output are counted, not stored.
  (let* ((data (random-octets 300))
         (output (make-array 100 :element-type '(unsigned-byte 8)))
         (decoder (make-decoder output)))
    (is (= 300 (feed-all decoder (list (reference-encode data)))))
    (is (equalp (subseq data 0 100) output)))
  (let* ((data (random-octets 300))
         (output (make-array 40 :element-type '(unsigned-byte 8)))
         (decoder (make-decoder output :layout :rgba)))
    (is (= 300 (feed-all decoder (list (reference-encode data)))))
    (is (equalp (subseq (rgba data) 0 40) output))))

(defun test-encoding ()
  ;; Byte for byte as cl-base64 writes it, around the 12-octet steps.
  (loop for length in '(0 1 2 3 4 11 12 13 15 16 17 24 47 48 49 100 1000 4099 20000)
        for data = (random-octets length)
        for string = (map 'string #'code-char data)
        do (is (equalp (octets (cl-base64:usb8-array-to-base64-string data)) (encode data)))
           (is (equalp (octets (cl-base64:usb8-array-to-base64-string data :uri t))
                       (encode data :uri t)))
           (is (equalp (decode (encode data :start 1 :end (max 1 (1- length))))
                       (subseq data (min 1 length) (max (min 1 length) (1- length)))))
           (dolist (columns '(0 1 4 10 76))
             (dolist (uri '(nil t))
               (let ((expected (cl-base64:usb8-array-to-base64-string data :uri uri
                                                                            :columns columns)))
                 (is (string= expected (usb8-array-to-base64-string data :uri uri
                                                                        :columns columns)))
                 (is (string= expected (string-to-base64-string string :uri uri
                                                                       :columns columns)))
                 (is (string= expected (with-output-to-string (out)
                                         (usb8-array-to-base64-stream data out :uri uri
                                                                               :columns columns))))
                 (is (string= expected (with-output-to-string (out)
                                         (string-to-base64-stream string out :uri uri
                                                                             :columns columns))))
                 (is (equalp data (base64-string-to-usb8-array expected :uri uri)))
                 (is (string= string (base64-string-to-string expected :uri uri)))
                 (is (string= string (with-output-to-string (out)
                                       (base64-string-to-stream expected :uri uri
                                                                         :stream out))))))))
  (dolist (integer (list 0 1 255 256 65535 (expt 2 64) (random (expt 10 200))))
    (dolist (uri '(nil t))
      (let ((expected (cl-base64:integer-to-base64-string integer :uri uri)))
        (is (string= expected (integer-to-base64-string integer :uri uri)))
        (is (string= expected (with-output-to-string (out)
                                (integer-to-base64-stream integer out :uri uri))))
        (is (= integer (base64-string-to-integer expected :uri uri)))
        (is (= integer (with-input-from-string (in expected)
                         (base64-stream-to-integer in :uri uri)))))))
  ;; A character that is neither a digit, padding nor whitespace.
  (is (handler-case (progn (base64-string-to-usb8-array "YW*i") nil)
        (base64-error () t))))

(defun run-tests ()
  (let ((*failures* 0))
    (test-round-trips)
    (test-padding)
    (test-errors)
    (test-output-bounds)
    (test-encoding)
    (format t "~&simd-base64: ~:[all tests passed~;~:*~d failure~:p~]~%"
            (and (plusp *failures*) *failures*))
    (zerop *failures*)))
