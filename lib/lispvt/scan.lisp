(in-package #:lispvt)

;;;; Finding runs of printable ASCII.  Most terminal output is plain
;;;; text, and the parser hands each run to a bulk printer, so the scan
;;;; for the next control or non-ASCII byte is the innermost loop.  It
;;;; tests 16 bytes per step when SBCL has the sb-simd contrib (some builds,
;;;; such as Alpine's, do not): with NEON on arm64 and SSE2, which every
;;;; x86-64 processor has, on x86-64, where processors with AVX2 test 32.
;;;; Otherwise it tests 8 bytes per step within a 64-bit word.

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(and sbcl arm64)
  (when (and (ignore-errors (require "sb-simd") t)
             (find-package "SB-SIMD-NEON"))
    (pushnew :lispvt-neon *features*))
  #+(and sbcl x86-64)
  (when (and (ignore-errors (require "sb-simd") t)
             (find-package "SB-SIMD-SSE2"))
    (pushnew :lispvt-sse2 *features*)))

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

#+lispvt-neon
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


;;; AVX2 is not part of the x86-64 baseline, so it is chosen at run time,
;;; and again whenever a saved image starts, perhaps on another processor.
;;; sb-simd's own INSTRUCTION-SET-CASE is not enough: an sb-simd built where
;;; AVX2 was missing has AVX2 functions that only signal an error.

#+lispvt-sse2
(progn
  (sb-ext:defglobal **avx2** nil)

  (defun avx2-usable-p ()
    "True when the processor has AVX2 and sb-simd's AVX2 functions work."
    (and (sb-simd-internals:avx2-supported-p)
         (handler-case (progn (sb-simd-avx2:u8.32-movemask (sb-simd-avx2:u8.32 0)) t)
           (error () nil))))

  (defun note-avx2 ()
    (setf **avx2** (avx2-usable-p)))

  (note-avx2)
  (pushnew 'note-avx2 sb-ext:*init-hooks*))

#+lispvt-sse2
(defun scan-printable-sse2 (octets start end)
  (declare (type octets octets)
           (type index start end)
           (optimize speed (safety 0)))
  (let ((i start)
        (low (sb-simd-sse2:u8.16 #x20))
        (high (sb-simd-sse2:u8.16 #x7E)))
    (declare (type index i))
    (loop while (<= (+ i 16) end)
          do (let ((v (sb-simd-sse2:u8.16-aref octets i)))
               (if (zerop (sb-simd-sse2:u8.16-movemask
                           (sb-simd-sse2:u8.16-or (sb-simd-sse2:u8.16< v low)
                                                  (sb-simd-sse2:u8.16> v high))))
                   (incf i 16)
                   (return))))
    (scan-printable-scalar octets i end)))

#+lispvt-sse2
(defun scan-printable-avx2 (octets start end)
  (declare (type octets octets)
           (type index start end)
           (optimize speed (safety 0)))
  (let ((i start)
        (low (sb-simd-avx2:u8.32 #x20))
        (high (sb-simd-avx2:u8.32 #x7E)))
    (declare (type index i))
    (loop while (<= (+ i 32) end)
          do (let ((v (sb-simd-avx2:u8.32-aref octets i)))
               (if (zerop (sb-simd-avx2:u8.32-movemask
                           (sb-simd-avx2:u8.32-or (sb-simd-avx2:u8.32< v low)
                                                  (sb-simd-avx2:u8.32> v high))))
                   (incf i 32)
                   (return))))
    (scan-printable-sse2 octets i end)))

(declaim (inline scan-printable))
(defun scan-printable (octets start end)
  "The index of the first octet of OCTETS in [START, END) outside printable
ASCII (#x20-#x7E), or END."
  #+lispvt-neon (scan-printable-neon octets start end)
  #+lispvt-sse2 (if **avx2**
                    (scan-printable-avx2 octets start end)
                    (scan-printable-sse2 octets start end))
  #-(or lispvt-neon lispvt-sse2) (scan-printable-swar octets start end))
