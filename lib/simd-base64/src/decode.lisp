(in-package #:simd-base64)

;;;; Base64 decoding, incremental and into a vector the caller provides.
;;;; Input may arrive in pieces split anywhere, even within a group of
;;;; digits, and padding may be missing or appear between separately
;;;; padded pieces.  The output layout :RGBA decodes RGB pixels straight
;;;; into RGBA, every pixel's three octets followed by an opaque alpha,
;;;; which is how a terminal wants an image sent as base64 RGB.
;;;;
;;;; A block of groups is decoded in one step with SIMD when SBCL has the
;;;; sb-simd contrib: NEON on arm64 (both layouts) and SSE2 on x86-64 (RGBA
;;;; only: packing the octets of a block needs a byte shuffle, which SSE2
;;;; lacks).  Everything else goes a group at a time.

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

;;; The encoding: each digit holds BITS-PER-DIGIT bits, so a group of
;;; DIGITS-PER-GROUP digits holds OCTETS-PER-GROUP octets.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +octet-bits+ 8)
  (defconstant +bits-per-digit+ 6)
  (defconstant +digits-per-group+ 4)
  (defconstant +octets-per-group+ (/ (* +bits-per-digit+ +digits-per-group+) +octet-bits+))
  (defconstant +group-bits+ (* +bits-per-digit+ +digits-per-group+))
  (defconstant +octet-mask+ (ldb (byte +octet-bits+ 0) -1))
  (defconstant +digit-mask+ (ldb (byte +bits-per-digit+ 0) -1))

  (defparameter +alphabet+
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    "The base64 digits, in order of value.")

  (defconstant +padding+ (char-code #\=))

  ;; The digits are five runs of consecutive octets, each mapped to its
  ;; values by adding one offset (modulo 256): (FIRST LAST OFFSET).
  (defparameter +digit-runs+
    (loop for (first last) in '((#\A #\Z) (#\a #\z) (#\0 #\9) (#\+ #\+) (#\/ #\/))
          collect (list (char-code first) (char-code last)
                        (ldb (byte +octet-bits+ 0)
                             (- (position first +alphabet+) (char-code first)))))))

;;; :RGBA output: each pixel's RGB octets, then its alpha.

(defconstant +rgb-octets+ 3)
(defconstant +rgba-octets+ 4)
(defconstant +opaque+ +octet-mask+ "The alpha of an opaque pixel.")

(defconstant +not-a-digit+ -1)
(defconstant +padding-value+ -2)

(defparameter +digit-values+
  (let ((table (make-array (1+ +octet-mask+) :element-type '(signed-byte 8)
                                             :initial-element +not-a-digit+)))
    (loop for char across +alphabet+
          for value from 0
          do (setf (aref table (char-code char)) value))
    (setf (aref table +padding+) +padding-value+)
    table)
  "Each octet's digit value, +NOT-A-DIGIT+ or +PADDING-VALUE+.")

(defstruct (decoder (:constructor make-decoder (output &key (layout :octets))))
  "Decodes base64 into OUTPUT.  With LAYOUT :OCTETS the decoded octets go
there in order; with :RGBA each three are a pixel's RGB, stored as its four
RGBA octets with an opaque alpha.  Octets beyond OUTPUT are counted, not
kept."
  (output nil :type octets)
  (layout :octets :type (member :octets :rgba))
  ;; Octets decoded so far, those past the output's end included.
  (fill 0 :type index)
  ;; The digits of an unfinished group, and their bits.
  (bits 0 :type fixnum)
  (digits 0 :type fixnum))

(declaim (inline output-capacity rgba-index))
(defun output-capacity (output layout)
  "How many decoded octets OUTPUT holds in LAYOUT."
  (if (eq layout :rgba)
      (* +rgb-octets+ (floor (length output) +rgba-octets+))
      (length output)))

(defun rgba-index (fill)
  "Where the FILLth decoded octet goes in :RGBA output."
  (multiple-value-bind (pixel channel) (floor fill +rgb-octets+)
    (+ (* pixel +rgba-octets+) channel)))

;;; A block at a time.  A 128-bit vector holds LANES 32-bit lanes, each a
;;; group of digits, its first digit in the low octet.  The digits become
;;; their values, which are joined into the group's bits and split into
;;; its octets, placed in the lane's low octets (with an opaque alpha
;;; above for RGBA).  A lane then holds one RGBA pixel, so for :RGBA the
;;; lanes are stored as they are; for :OCTETS their octets are packed.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +vector-octets+ 16)
  ;; A lane holds a group, a digit in each of its octets.
  (defconstant +lane-octets+ +digits-per-group+)
  (defconstant +lanes+ (/ +vector-octets+ +lane-octets+))
  (defconstant +block-digits+ (* +lanes+ +digits-per-group+))
  (defconstant +block-octets+ (* +lanes+ +octets-per-group+))
  ;; A shuffle index past the vector's end yields a zero octet.
  (defconstant +shuffle-zero+ +octet-mask+))

(defmacro define-block-decoder (name package &key all-true shuffle)
  "Define (NAME OCTETS INDEX END OUTPUT FILL CAPACITY RGBA-P), which decodes
blocks from INDEX while they and their output fit, and returns the index
and fill after them.  It stops before a block holding anything but digits.
ALL-TRUE tests that a mask has every octet set; with no SHUFFLE it decodes
only for RGBA."
  (flet ((op (name) (intern (string name) package)))
    (let* ((broadcast (op "U8.16"))
           (word (op "U32.4"))
           (runs (loop for (first last offset) in +digit-runs+
                       collect (list (gensym "RUN")
                                     (if (= first last)
                                         `(,(op "U8.16=") v (,broadcast ,first))
                                         `(,(op "U8.16-AND")
                                           (,(op "U8.16>=") v (,broadcast ,first))
                                           (,(op "U8.16<=") v (,broadcast ,last))))
                                     offset)))
           (any (lambda (forms)
                  (reduce (lambda (a b) `(,(op "U8.16-OR") ,a ,b)) forms)))
           (word-or (lambda (forms)
                      (reduce (lambda (a b) `(,(op "U32.4-OR") ,a ,b)) forms)))
           ;; NEON cannot encode a right shift by zero.
           (shift (lambda (direction form count)
                    (if (zerop count) form `(,(op direction) ,form ,count)))))
      `(progn
         (declaim (inline ,name))
         (defun ,name (octets index end output fill capacity rgba-p)
           (declare (type octets octets output)
                    (type index index end fill capacity)
                    (optimize speed (safety 0)))
           (let ((output-length (length output)))
             (declare (type index output-length))
             (loop while (and (<= (+ index +block-digits+) end)
                              (<= (+ fill +block-octets+) capacity)
                              ,(if shuffle
                                   `(or rgba-p (<= (+ fill +vector-octets+) output-length))
                                   'rgba-p))
                   do (let* ((v (,(op "U8.16-AREF") octets index))
                             ,@(loop for (run test) in runs collect `(,run ,test)))
                        (unless (,all-true ,(funcall any (mapcar #'first runs)))
                          (return))
                        (let* ((values
                                 (,(op "U8.16+")
                                  v ,(funcall any (loop for (run nil offset) in runs
                                                        collect `(,(op "U8.16-AND") ,run
                                                                  (,broadcast ,offset))))))
                               (lanes (,(op "U32.4!") values))
                               (group
                                 ,(funcall
                                   word-or
                                   (loop for digit below +digits-per-group+
                                         collect (funcall
                                                  shift "U32.4-SHIFTL"
                                                  `(,(op "U32.4-AND")
                                                    ,(funcall shift "U32.4-SHIFTR" 'lanes
                                                              (* digit +octet-bits+))
                                                    (,word ,+digit-mask+))
                                                  (* +bits-per-digit+
                                                     (- +digits-per-group+ 1 digit))))))
                               (decoded
                                 ,(funcall
                                   word-or
                                   (loop for octet below +octets-per-group+
                                         collect (funcall
                                                  shift "U32.4-SHIFTL"
                                                  `(,(op "U32.4-AND")
                                                    ,(funcall shift "U32.4-SHIFTR" 'group
                                                              (* +octet-bits+
                                                                 (- +octets-per-group+ 1 octet)))
                                                    (,word ,+octet-mask+))
                                                  (* octet +octet-bits+))))))
                          (if rgba-p
                              (setf (,(op "U8.16-AREF") output (rgba-index fill))
                                    (,(op "U8.16!")
                                     (,(op "U32.4-OR") decoded
                                      (,word ,(ash +opaque+ (* +octet-bits+ +rgb-octets+))))))
                              ,(if shuffle
                                   `(setf (,(op "U8.16-AREF") output fill)
                                          (,(op "U8.16-SHUFFLE")
                                           (,(op "U8.16!") decoded)
                                           (,(op "MAKE-U8.16")
                                            ,@(loop for i below +vector-octets+
                                                    collect (multiple-value-bind (lane octet)
                                                                (floor i +octets-per-group+)
                                                              (if (< lane +lanes+)
                                                                  (+ (* lane +lane-octets+)
                                                                     octet)
                                                                  +shuffle-zero+))))))
                                   nil))
                          (incf index +block-digits+)
                          (incf fill +block-octets+))))
             (values index fill)))))))

#+simd-base64-neon
(define-block-decoder decode-blocks "SB-SIMD-NEON"
  :all-true (lambda (mask) (/= 0 (sb-simd-neon:u8.16-horizontal-min mask)))
  :shuffle t)

#+simd-base64-sse2
(define-block-decoder decode-blocks "SB-SIMD-SSE2"
  :all-true (lambda (mask)
              (= (ldb (byte +vector-octets+ 0) -1) (sb-simd-sse2:u8.16-movemask mask))))

#-(or simd-base64-neon simd-base64-sse2)
(progn
  (declaim (inline decode-blocks))
  (defun decode-blocks (octets index end output fill capacity rgba-p)
    (declare (ignore octets end output capacity rgba-p))
    (values index fill)))

(defun decoder-feed (decoder octets &key (start 0) (end (length octets)))
  "Decode the base64 digits in OCTETS from START to END, continuing the
group the last call ended in.  Signal BASE64-ERROR at an octet that is not
a digit or padding; DECODER is then of no further use."
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
             (type (simple-array (signed-byte 8) (*)) table))
    (when (> end (length octets))
      (error "END ~d is beyond the ~d octets" end (length octets)))
    (labels ((emit (octet)
               (declare (type (unsigned-byte 8) octet))
               (when (< fill capacity)
                 (if rgba-p
                     (let ((target (rgba-index fill)))
                       (setf (aref output target) octet)
                       (when (= (1- +rgb-octets+) (mod fill +rgb-octets+))
                         (setf (aref output (1+ target)) +opaque+)))
                     (setf (aref output fill) octet)))
               (incf fill))
             (emit-group (group digits)
               ;; The whole octets in the bits of DIGITS digits.
               (let ((size (* digits +bits-per-digit+)))
                 (loop for octet from 1 to (floor size +octet-bits+)
                       do (emit (ldb (byte +octet-bits+ (- size (* octet +octet-bits+)))
                                     group)))))
             (finish-group ()
               ;; A group too short to hold an octet is malformed.
               (when (plusp digits)
                 (when (< (* digits +bits-per-digit+) +octet-bits+)
                   (error 'base64-error :position index))
                 (emit-group bits digits))
               (setf bits 0 digits 0)))
      (loop while (< index end)
            do (when (and (zerop digits) (or (not rgba-p) (zerop (mod fill +rgb-octets+))))
                 (multiple-value-setq (index fill)
                   (decode-blocks octets index end output fill capacity rgba-p)))
               (cond
                 ((>= index end))
                 ;; A whole group of digits, the common case.
                 ((and (zerop digits)
                       (<= (+ index +digits-per-group+) end)
                       (let ((group 0))
                         (declare (type fixnum group))
                         (loop for i of-type index from index
                                 below (+ index +digits-per-group+)
                               for value of-type fixnum = (aref table (aref octets i))
                               always (>= value 0)
                               do (setf group (logior (ash group +bits-per-digit+) value))
                               finally (emit-group group +digits-per-group+)
                                       (incf index +digits-per-group+)
                                       (return t)))))
                 (t
                  (let ((value (aref table (aref octets index))))
                    (cond
                      ((>= value 0)
                       (setf bits (logior (ash (ldb (byte (- +group-bits+ +bits-per-digit+) 0)
                                                    bits)
                                               +bits-per-digit+)
                                          value))
                       (when (= (incf digits) +digits-per-group+)
                         (emit-group bits digits)
                         (setf bits 0 digits 0)))
                      ((= value +padding-value+) (finish-group))
                      (t (error 'base64-error :position index)))
                    (incf index))))))
    (setf (decoder-fill decoder) fill
          (decoder-bits decoder) bits
          (decoder-digits decoder) digits)
    decoder))

(defun decoder-finish (decoder)
  "Decode the digits of an unpadded last group; return how many octets
were decoded in all, those that did not fit the output included."
  (unless (zerop (decoder-digits decoder))
    ;; The padding the input left out.
    (decoder-feed decoder (make-array 1 :element-type '(unsigned-byte 8)
                                        :initial-element +padding+)))
  (decoder-fill decoder))

(defun decoded-length (parts)
  "How many octets the base64 in PARTS, an octet vector or a list of them,
decodes to at most: exactly, unless padding comes before its end."
  (let* ((parts (if (listp parts) parts (list parts)))
         (total (reduce #'+ parts :key #'length))
         (padding (loop for part in (reverse parts)
                        for trailing = (or (position +padding+ part :from-end t :test #'/=) -1)
                        sum (- (length part) trailing 1)
                        while (minusp trailing))))
    (floor (* +octets-per-group+ (- total padding)) +digits-per-group+)))

(defun decode (parts &key (layout :octets))
  "Decode the base64 in PARTS, an octet vector or a list of them split
anywhere, into a new octet vector in LAYOUT.  Signal BASE64-ERROR when it
is malformed."
  (flet ((size (octets)
           (if (eq layout :rgba)
               (* +rgba-octets+ (ceiling octets +rgb-octets+))
               octets)))
    (let* ((parts (if (listp parts) parts (list parts)))
           (output (make-array (size (decoded-length parts)) :element-type '(unsigned-byte 8)))
           (decoder (make-decoder output :layout layout)))
      (dolist (part parts)
        (decoder-feed decoder (coerce part 'octets)))
      (let ((used (size (decoder-finish decoder))))
        (if (= used (length output)) output (subseq output 0 used))))))
