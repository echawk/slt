(in-package #:ecma48)

;;;; Finding the end of a text run.  In UTF-8 terminal output everything but
;;;; the C0 controls (00-1F) and DEL (7F) is text, and text is most of the
;;;; input, so this scan is the scanner's innermost loop.  It tests 16 bytes
;;;; per step when SBCL has the sb-simd contrib: with NEON on arm64 and SSE2,
;;;; which every x86-64 processor has, on x86-64.  Otherwise it tests 8 bytes
;;;; per step within a 64-bit word.

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(and sbcl arm64)
  (when (and (ignore-errors (require "sb-simd") t)
             (find-package "SB-SIMD-NEON"))
    (pushnew :ecma48-neon *features*))
  #+(and sbcl x86-64)
  (when (and (ignore-errors (require "sb-simd") t)
             (find-package "SB-SIMD-SSE2"))
    (pushnew :ecma48-sse2 *features*)))

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(deftype index () `(integer 0 ,array-dimension-limit))

(declaim (inline text-byte-p))
(defun text-byte-p (byte)
  (declare (type (unsigned-byte 8) byte))
  (and (>= byte #x20) (/= byte #x7F)))

(declaim (inline scan-text-scalar))
(defun scan-text-scalar (octets start end)
  (declare (type octets octets)
           (type index start end)
           (optimize speed (safety 0)))
  (loop for i of-type index from start below end
        unless (text-byte-p (aref octets i))
          do (return i)
        finally (return end)))

#+sbcl
(defun scan-text-swar (octets start end)
  "Word-at-a-time scan: a word holds a control when some byte is below #x20
or equal to #x7F (zero once XORed with #x7F)."
  (declare (type octets octets)
           (type index start end)
           (optimize speed (safety 0)))
  (let ((i start))
    (declare (type index i))
    (sb-sys:with-pinned-objects (octets)
      (let ((sap (sb-sys:vector-sap octets)))
        (loop while (<= (+ i 8) end)
              do (let* ((word (sb-sys:sap-ref-64 sap i))
                        (del (logxor word #x7F7F7F7F7F7F7F7F)))
                   (declare (type (unsigned-byte 64) word del))
                   (if (zerop (logand
                               (logior
                                ;; Some byte < #x20.
                                (logand (ldb (byte 64 0) (- word #x2020202020202020))
                                        (lognot word))
                                ;; Some byte = #x7F.
                                (logand (ldb (byte 64 0) (- del #x0101010101010101))
                                        (lognot del)))
                               #x8080808080808080))
                       (incf i 8)
                       (return))))))
    (scan-text-scalar octets i end)))

#+ecma48-neon
(defun scan-text-neon (octets start end)
  (declare (type octets octets)
           (type index start end)
           (optimize speed (safety 0)))
  (let ((i start)
        (space (sb-simd-neon:u8.16 #x20))
        (del (sb-simd-neon:u8.16 #x7F)))
    (declare (type index i))
    (loop while (<= (+ i 16) end)
          do (let ((v (sb-simd-neon:u8.16-aref octets i)))
               (if (zerop (sb-simd-neon:u8.16-horizontal-max
                           (sb-simd-neon:u8.16-or (sb-simd-neon:u8.16< v space)
                                                  (sb-simd-neon:u8.16= v del))))
                   (incf i 16)
                   (return))))
    (scan-text-scalar octets i end)))

#+ecma48-sse2
(defun scan-text-sse2 (octets start end)
  (declare (type octets octets)
           (type index start end)
           (optimize speed (safety 0)))
  (let ((i start)
        (space (sb-simd-sse2:u8.16 #x20))
        (del (sb-simd-sse2:u8.16 #x7F)))
    (declare (type index i))
    (loop while (<= (+ i 16) end)
          do (let ((v (sb-simd-sse2:u8.16-aref octets i)))
               (if (zerop (sb-simd-sse2:u8.16-movemask
                           (sb-simd-sse2:u8.16-or (sb-simd-sse2:u8.16< v space)
                                                  (sb-simd-sse2:u8.16= v del))))
                   (incf i 16)
                   (return))))
    (scan-text-scalar octets i end)))

(declaim (inline scan-text))
(defun scan-text (octets start end)
  "The index of the first C0 control or DEL in OCTETS from START to END, or
END."
  #+ecma48-neon (scan-text-neon octets start end)
  #+ecma48-sse2 (scan-text-sse2 octets start end)
  #-(or ecma48-neon ecma48-sse2) #+sbcl (scan-text-swar octets start end)
  #-(or ecma48-neon ecma48-sse2 sbcl) (scan-text-scalar octets start end))
