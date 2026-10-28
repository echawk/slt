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

(defun encode (octets)
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
        for encoded = (encode data)
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
    (is (= 300 (feed-all decoder (list (encode data)))))
    (is (equalp (subseq data 0 100) output)))
  (let* ((data (random-octets 300))
         (output (make-array 40 :element-type '(unsigned-byte 8)))
         (decoder (make-decoder output :layout :rgba)))
    (is (= 300 (feed-all decoder (list (encode data)))))
    (is (equalp (subseq (rgba data) 0 40) output))))

(defun run-tests ()
  (let ((*failures* 0))
    (test-round-trips)
    (test-padding)
    (test-errors)
    (test-output-bounds)
    (format t "~&simd-base64: ~:[all tests passed~;~:*~d failure~:p~]~%"
            (and (plusp *failures*) *failures*))
    (zerop *failures*)))
