(in-package #:lispvt)

;;;; What joins codepoints into one grapheme cluster (one cell) beyond the
;;;; zero-width characters CHAR-WIDTH already reports (marks, ZWJ,
;;;; variation selectors, emoji modifiers, tags): the parts of emoji
;;;; sequences, after Unicode's grapheme cluster rules GB11, GB12 and GB13
;;;; (UAX #29).  The ranges are Extended_Pictographic from Unicode 16.0's
;;;; emoji-data.txt.

(defparameter *extended-pictographic-ranges*
  #(#x00A9 #x00A9  #x00AE #x00AE  #x203C #x203C  #x2049 #x2049
    #x2122 #x2122  #x2139 #x2139  #x2194 #x2199  #x21A9 #x21AA
    #x231A #x231B  #x2328 #x2328  #x2388 #x2388  #x23CF #x23CF
    #x23E9 #x23F3  #x23F8 #x23FA  #x24C2 #x24C2  #x25AA #x25AB
    #x25B6 #x25B6  #x25C0 #x25C0  #x25FB #x25FE  #x2600 #x2605
    #x2607 #x2612  #x2614 #x2685  #x2690 #x2705  #x2708 #x2712
    #x2714 #x2714  #x2716 #x2716  #x271D #x271D  #x2721 #x2721
    #x2728 #x2728  #x2733 #x2734  #x2744 #x2744  #x2747 #x2747
    #x274C #x274C  #x274E #x274E  #x2753 #x2755  #x2757 #x2757
    #x2763 #x2767  #x2795 #x2797  #x27A1 #x27A1  #x27B0 #x27B0
    #x27BF #x27BF  #x2934 #x2935  #x2B05 #x2B07  #x2B1B #x2B1C
    #x2B50 #x2B50  #x2B55 #x2B55  #x3030 #x3030  #x303D #x303D
    #x3297 #x3297  #x3299 #x3299  #x1F000 #x1F0FF  #x1F10D #x1F10F
    #x1F12F #x1F12F  #x1F16C #x1F171  #x1F17E #x1F17F  #x1F18E #x1F18E
    #x1F191 #x1F19A  #x1F1AD #x1F1E5  #x1F201 #x1F20F  #x1F21A #x1F21A
    #x1F22F #x1F22F  #x1F232 #x1F23A  #x1F23C #x1F23F  #x1F249 #x1F3FA
    #x1F400 #x1F53D  #x1F546 #x1F64F  #x1F680 #x1F6FF  #x1F774 #x1F77F
    #x1F7D5 #x1F7FF  #x1F80C #x1F80F  #x1F848 #x1F84F  #x1F85A #x1F85F
    #x1F888 #x1F88F  #x1F8AE #x1F8FF  #x1F90C #x1F93A  #x1F93C #x1F945
    #x1F947 #x1FAFF  #x1FC00 #x1FFFD)
  "Extended_Pictographic codepoint ranges, as start and end pairs.")

(defconstant +zero-width-joiner+ #x200D)
(defconstant +text-presentation+ #xFE0E)
(defconstant +emoji-presentation+ #xFE0F)

(defun extended-pictographic-p (code)
  (declare (type (unsigned-byte 32) code))
  (let ((ranges *extended-pictographic-ranges*))
    (declare (type simple-vector ranges))
    ;; Binary search over the start of each pair.
    (let ((low 0) (high (1- (floor (length ranges) 2))))
      (declare (type fixnum low high))
      (loop while (<= low high)
            do (let* ((middle (floor (+ low high) 2))
                      (start (svref ranges (* 2 middle)))
                      (end (svref ranges (1+ (* 2 middle)))))
                 (cond ((< code start) (setf high (1- middle)))
                       ((> code end) (setf low (1+ middle)))
                       (t (return t))))))))

(defun regional-indicator-p (code)
  "A regional indicator letter: two of them make a flag."
  (<= #x1F1E6 code #x1F1FF))
