(in-package #:simd-base64)

;;;; Base64 decoding, incremental and into a vector the caller provides.
;;;; Input may arrive in pieces split anywhere, even within a quartet of
;;;; digits, and padding may be missing or appear between separately
;;;; padded pieces.  The output layout :RGBA decodes RGB pixels straight
;;;; into RGBA, every third octet followed by an opaque alpha, which is
;;;; how a terminal wants an image sent as base64 RGB.
;;;;
;;;; Whole runs of 16 digits are decoded in one step with SIMD when SBCL
;;;; has the sb-simd contrib: NEON on arm64 (both layouts) and SSE2 on
;;;; x86-64 (RGBA only: packing 12 octets out of 16 needs a byte shuffle,
;;;; which SSE2 lacks).  Everything else goes a quartet at a time.

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(and sbcl arm64)
  (when (and (ignore-errors (require "sb-simd") t)
             (find-package "SB-SIMD-NEON"))
    (pushnew :simd-base64-neon *features*))
  #+(and sbcl x86-64)
  (when (and (ignore-errors (require "sb-simd") t)
             (find-package "SB-SIMD-SSE2"))
    (pushnew :simd-base64-sse2 *features*)))

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(deftype index () `(integer 0 ,array-dimension-limit))

(define-condition base64-error (error)
  ((position :initarg :position :initform nil :reader base64-error-position))
  (:report (lambda (condition stream)
             (format stream "Invalid base64~@[ at octet ~d~]"
                     (base64-error-position condition)))))

(defparameter +digit-values+
  (let ((table (make-array 256 :element-type '(signed-byte 8) :initial-element -1)))
    (loop for char across "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
          for value from 0
          do (setf (aref table (char-code char)) value))
    (setf (aref table (char-code #\=)) -2)
    table)
  "Each octet's digit value: -1 for an octet that is not a digit, -2 for
the padding =.")

(defstruct (decoder (:constructor make-decoder (output &key (layout :octets))))
  "Decodes base64 into OUTPUT.  With LAYOUT :OCTETS the decoded octets go
there in order; with :RGBA each three are a pixel's RGB, stored as its four
RGBA octets with alpha 255.  Octets beyond OUTPUT are counted, not kept."
  (output nil :type octets)
  (layout :octets :type (member :octets :rgba))
  ;; Octets decoded so far, those past the output's end included.
  (fill 0 :type index)
  ;; The digits (up to three) of an unfinished quartet, and their bits.
  (bits 0 :type fixnum)
  (digits 0 :type fixnum))

(declaim (inline output-capacity))
(defun output-capacity (output layout)
  "How many decoded octets OUTPUT holds in LAYOUT."
  (if (eq layout :rgba)
      (* 3 (floor (length output) 4))
      (length output)))

;;; 16 digits at a time.  Each 32-bit lane holds a quartet, its first digit
;;; in the low octet; the sextets of digits a b c d make the octets
;;;   a << 2 | b >> 4,  (b & 15) << 4 | c >> 2,  (c & 3) << 6 | d,
;;; placed in the lane's low three octets (with alpha 255 above for RGBA).
;;; A lane holds one RGBA pixel, so for :RGBA the lanes are stored as they
;;; are; for :OCTETS their low three octets are packed into 12.

(defmacro define-block-decoder (name package &key all-true shuffle)
  "Define (NAME OCTETS INDEX END OUTPUT FILL CAPACITY RGBA-P), which decodes
runs of 16 digits from INDEX while they and their output fit, and returns
the index and fill after them.  It stops before a run holding anything but
digits.  With no SHUFFLE it decodes only for RGBA."
  (flet ((op (name) (intern (string name) package)))
    `(progn
       (declaim (inline ,name))
       (defun ,name (octets index end output fill capacity rgba-p)
         (declare (type octets octets output)
                  (type index index end fill capacity)
                  (optimize speed (safety 0)))
         (let ((output-length (length output)))
           (declare (type index output-length))
           (loop while (and (<= (+ index 16) end)
                            (<= (+ fill 12) capacity)
                            ,(if shuffle
                                 `(or rgba-p (<= (+ fill 16) output-length))
                                 'rgba-p))
                 do (let* ((v (,(op "U8.16-AREF") octets index))
                           (upper (,(op "U8.16-AND") (,(op "U8.16>=") v (,(op "U8.16") 65))
                                   (,(op "U8.16<=") v (,(op "U8.16") 90))))
                           (lower (,(op "U8.16-AND") (,(op "U8.16>=") v (,(op "U8.16") 97))
                                   (,(op "U8.16<=") v (,(op "U8.16") 122))))
                           (digit (,(op "U8.16-AND") (,(op "U8.16>=") v (,(op "U8.16") 48))
                                   (,(op "U8.16<=") v (,(op "U8.16") 57))))
                           (plus (,(op "U8.16=") v (,(op "U8.16") 43)))
                           (slash (,(op "U8.16=") v (,(op "U8.16") 47)))
                           (valid (,(op "U8.16-OR") (,(op "U8.16-OR") upper lower)
                                   (,(op "U8.16-OR") digit (,(op "U8.16-OR") plus slash)))))
                      (unless (,all-true valid)
                        (return))
                      ;; Each class's distance to its sextets, modulo 256.
                      (let* ((sextets
                               (,(op "U8.16+")
                                v
                                (,(op "U8.16-OR")
                                 (,(op "U8.16-OR")
                                  (,(op "U8.16-AND") upper (,(op "U8.16") 191))
                                  (,(op "U8.16-AND") lower (,(op "U8.16") 185)))
                                 (,(op "U8.16-OR")
                                  (,(op "U8.16-AND") digit (,(op "U8.16") 4))
                                  (,(op "U8.16-OR")
                                   (,(op "U8.16-AND") plus (,(op "U8.16") 19))
                                   (,(op "U8.16-AND") slash (,(op "U8.16") 16)))))))
                             (w (,(op "U32.4!") sextets))
                             (m6 (,(op "U32.4") #x3f))
                             (a (,(op "U32.4-AND") w m6))
                             (b (,(op "U32.4-AND") (,(op "U32.4-SHIFTR") w 8) m6))
                             (c (,(op "U32.4-AND") (,(op "U32.4-SHIFTR") w 16) m6))
                             (d (,(op "U32.4-SHIFTR") w 24))
                             (rgb (,(op "U32.4-OR")
                                   (,(op "U32.4-OR")
                                    (,(op "U32.4-SHIFTL") a 2)
                                    (,(op "U32.4-SHIFTR") b 4))
                                   (,(op "U32.4-OR")
                                    (,(op "U32.4-OR")
                                     (,(op "U32.4-SHIFTL") (,(op "U32.4-AND") b (,(op "U32.4") 15)) 12)
                                     (,(op "U32.4-SHIFTL") (,(op "U32.4-SHIFTR") c 2) 8))
                                    (,(op "U32.4-OR")
                                     (,(op "U32.4-SHIFTL") (,(op "U32.4-AND") c (,(op "U32.4") 3)) 22)
                                     (,(op "U32.4-SHIFTL") d 16))))))
                        (if rgba-p
                            (setf (,(op "U8.16-AREF") output (* 4 (floor fill 3)))
                                  (,(op "U8.16!") (,(op "U32.4-OR") rgb (,(op "U32.4") #xff000000))))
                            ,(if shuffle
                                 `(setf (,(op "U8.16-AREF") output fill)
                                        (,(op "U8.16-SHUFFLE")
                                         (,(op "U8.16!") rgb)
                                         (,(op "MAKE-U8.16") 0 1 2 4 5 6 8 9 10 12 13 14
                                          255 255 255 255)))
                                 nil))
                        (incf index 16)
                        (incf fill 12))))
           (values index fill))))))

#+simd-base64-neon
(define-block-decoder decode-blocks "SB-SIMD-NEON"
  :all-true (lambda (mask) (/= 0 (sb-simd-neon:u8.16-horizontal-min mask)))
  :shuffle t)

#+simd-base64-sse2
(define-block-decoder decode-blocks "SB-SIMD-SSE2"
  :all-true (lambda (mask) (= #xffff (sb-simd-sse2:u8.16-movemask mask))))

#-(or simd-base64-neon simd-base64-sse2)
(progn
  (declaim (inline decode-blocks))
  (defun decode-blocks (octets index end output fill capacity rgba-p)
    (declare (ignore octets end output capacity rgba-p))
    (values index fill)))

(defun decoder-feed (decoder octets &key (start 0) (end (length octets)))
  "Decode the base64 digits in OCTETS from START to END, continuing the
quartet the last call ended in.  Signal BASE64-ERROR at an octet that is
not a digit or padding; DECODER is then of no further use."
  (declare (type octets octets) (type index start end) (optimize speed))
  (let* ((output (decoder-output decoder))
         (rgba-p (eq (decoder-layout decoder) :rgba))
         (capacity (output-capacity output (decoder-layout decoder)))
         (fill (decoder-fill decoder))
         (bits (decoder-bits decoder))
         (digits (decoder-digits decoder))
         (table +digit-values+)
         (index start))
    (declare (type octets output) (type index capacity fill index)
             (type fixnum bits digits)
             (type (simple-array (signed-byte 8) (256)) table))
    (when (> end (length octets))
      (error "END ~d is beyond the ~d octets" end (length octets)))
    (labels ((emit (octet)
               (declare (type (unsigned-byte 8) octet))
               (when (< fill capacity)
                 (if rgba-p
                     (let ((target (+ fill (floor fill 3))))
                       (setf (aref output target) octet)
                       (when (= 2 (mod fill 3))
                         (setf (aref output (1+ target)) 255)))
                     (setf (aref output fill) octet)))
               (incf fill))
             (finish-group ()
               ;; A lone digit holds nothing whole and is malformed.
               (case digits
                 (1 (error 'base64-error :position index))
                 (2 (emit (ldb (byte 8 4) bits)))
                 (3 (emit (ldb (byte 8 10) bits))
                  (emit (ldb (byte 8 2) bits))))
               (setf bits 0 digits 0)))
      (loop while (< index end)
            do (when (and (zerop digits) (or (not rgba-p) (zerop (mod fill 3))))
                 (multiple-value-setq (index fill)
                   (decode-blocks octets index end output fill capacity rgba-p)))
               (let ((a 0) (b 0) (c 0) (d 0))
                 (declare (type (signed-byte 8) a b c d))
                 (cond
                   ((>= index end))
                   ((and (zerop digits)
                         (<= (+ index 4) end)
                         (progn (setf a (aref table (aref octets index))
                                      b (aref table (aref octets (+ index 1)))
                                      c (aref table (aref octets (+ index 2)))
                                      d (aref table (aref octets (+ index 3))))
                                ;; Negative when any is not a digit.
                                (>= (logior a b c d) 0)))
                    (let ((group (logior (ash a 18) (ash b 12) (ash c 6) d)))
                      (emit (ldb (byte 8 16) group))
                      (emit (ldb (byte 8 8) group))
                      (emit (ldb (byte 8 0) group))
                      (incf index 4)))
                   (t
                    (let ((value (aref table (aref octets index))))
                      (cond
                        ((>= value 0)
                         (setf bits (logior (ash (logand bits #x3ffff) 6) value))
                         (incf digits)
                         (when (= digits 4)
                           (emit (ldb (byte 8 16) bits))
                           (emit (ldb (byte 8 8) bits))
                           (emit (ldb (byte 8 0) bits))
                           (setf bits 0 digits 0)))
                        ((= value -2) (finish-group))
                        (t (error 'base64-error :position index)))
                      (incf index)))))))
    (setf (decoder-fill decoder) fill
          (decoder-bits decoder) bits
          (decoder-digits decoder) digits)
    decoder))

(defun decoder-finish (decoder)
  "Decode the digits of an unpadded last quartet; return how many octets
were decoded in all, those that did not fit the output included."
  (unless (zerop (decoder-digits decoder))
    ;; The padding the input left out.
    (decoder-feed decoder (make-array 1 :element-type '(unsigned-byte 8)
                                        :initial-element (char-code #\=))))
  (decoder-fill decoder))

(defun decoded-length (parts)
  "How many octets the base64 in PARTS, an octet vector or a list of them,
decodes to at most: exactly, unless padding comes before its end."
  (let* ((parts (if (listp parts) parts (list parts)))
         (total (reduce #'+ parts :key #'length))
         (padding (loop for part in (reverse parts)
                        for trailing = (or (position 61 part :from-end t :test #'/=) -1)
                        sum (- (length part) trailing 1)
                        while (minusp trailing))))
    (floor (* 3 (- total padding)) 4)))

(defun decode (parts &key (layout :octets))
  "Decode the base64 in PARTS, an octet vector or a list of them split
anywhere, into a new octet vector in LAYOUT.  Signal BASE64-ERROR when it
is malformed."
  (let* ((parts (if (listp parts) parts (list parts)))
         (length (decoded-length parts))
         (output (make-array (if (eq layout :rgba) (* 4 (ceiling length 3)) length)
                             :element-type '(unsigned-byte 8)))
         (decoder (make-decoder output :layout layout)))
    (dolist (part parts)
      (decoder-feed decoder (coerce part 'octets)))
    (let* ((fill (decoder-finish decoder))
           (used (if (eq layout :rgba) (* 4 (ceiling fill 3)) fill)))
      (if (= used (length output)) output (subseq output 0 used)))))
