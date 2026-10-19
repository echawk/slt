(in-package #:lispvt)

;;;; Finding runs of printable ASCII.  Most terminal output is plain
;;;; text, and the parser hands each run to a bulk printer, so the scan
;;;; for the next control or non-ASCII byte is the innermost loop.  On
;;;; arm64 it tests 16 bytes per step with NEON (sb-simd); elsewhere it
;;;; tests 8 bytes per step within a 64-bit word.

(deftype octets () '(simple-array (unsigned-byte 8) (*)))
(deftype index () `(integer 0 ,array-dimension-limit))

(declaim (inline printable-byte-p))
(defun printable-byte-p (byte)
  (declare (type (unsigned-byte 8) byte))
  (<= #x20 byte #x7E))

(declaim (inline scan-printable-scalar))
(defun scan-printable-scalar (octets start end)
  (declare (type octets octets)
           (type index start end)
           (optimize speed (safety 0)))
  (loop for i of-type index from start below end
        unless (printable-byte-p (aref octets i))
          do (return i)
        finally (return end)))

(defun scan-printable-swar (octets start end)
  "Portable word-at-a-time scan: a word holds a non-printable byte when some
byte is below #x20 or above #x7E."
  (declare (type octets octets)
           (type index start end)
           (optimize speed (safety 0)))
  (let ((i start))
    (declare (type index i))
    (sb-sys:with-pinned-objects (octets)
      (let ((sap (sb-sys:vector-sap octets)))
        (loop while (<= (+ i 8) end)
              do (let ((word (sb-sys:sap-ref-64 sap i)))
                   (declare (type (unsigned-byte 64) word))
                   (if (zerop (logand
                               (logior
                                ;; Some byte < #x20.
                                (logand (ldb (byte 64 0) (- word #x2020202020202020))
                                        (lognot word))
                                ;; Some byte > #x7E.
                                (logior (ldb (byte 64 0) (+ word #x0101010101010101))
                                        word))
                               #x8080808080808080))
                       (incf i 8)
                       (return))))))
    (scan-printable-scalar octets i end)))

#+(and sbcl arm64)
(defun scan-printable-neon (octets start end)
  (declare (type octets octets)
           (type index start end)
           (optimize speed (safety 0)))
  (let ((i start)
        (low (sb-simd-neon:u8.16 #x20))
        (high (sb-simd-neon:u8.16 #x7E)))
    (declare (type index i))
    (loop while (<= (+ i 16) end)
          do (let ((v (sb-simd-neon:u8.16-aref octets i)))
               (if (zerop (sb-simd-neon:u8.16-horizontal-max
                           (sb-simd-neon:u8.16-or (sb-simd-neon:u8.16< v low)
                                                  (sb-simd-neon:u8.16> v high))))
                   (incf i 16)
                   (return))))
    (scan-printable-scalar octets i end)))

(declaim (inline scan-printable))
(defun scan-printable (octets start end)
  "The index of the first octet of OCTETS in [START, END) outside printable
ASCII (#x20-#x7E), or END."
  #+(and sbcl arm64) (scan-printable-neon octets start end)
  #-(and sbcl arm64) (scan-printable-swar octets start end))
