(in-package #:lispvt)

;;;; Each codepoint's width and whether it is Extended_Pictographic, packed
;;;; into one octet and looked up in a two-stage table: the codepoint's
;;;; high bits pick a block of BLOCK-SIZE octets, and identical blocks are
;;;; stored once, so all of Unicode takes a few dozen kilobytes.  Built at
;;;; load time from the ranges in width.lisp and grapheme.lisp.

(defconstant +codepoint-limit+ #x110000)
(defconstant +block-bits+ 8)
(defconstant +block-size+ (ash 1 +block-bits+))
(defconstant +block-count+ (ceiling +codepoint-limit+ +block-size+))

;;; A property octet: the width in its low bits, then the flag.
(defconstant +width-field+ 2 "Bits holding the width.")
(defconstant +pictographic-bit+ +width-field+)
(defconstant +default-properties+ 1 "Width 1, not pictographic.")

(defun compute-char-properties (code)
  (logior (compute-char-width code)
          (if (compute-extended-pictographic-p code) (ash 1 +pictographic-bit+) 0)))

(defun build-property-table ()
  "The two stages: each block's offset in the octets, and the octets."
  (let ((blocks (make-hash-table :test #'equalp))
        (index (make-array +block-count+ :element-type '(unsigned-byte 32)))
        (octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dotimes (block +block-count+)
      (let ((properties (make-array +block-size+ :element-type '(unsigned-byte 8))))
        (dotimes (i +block-size+)
          (setf (aref properties i)
                (compute-char-properties (+ i (ash block +block-bits+)))))
        (setf (aref index block)
              (or (gethash properties blocks)
                  (setf (gethash properties blocks)
                        (prog1 (fill-pointer octets)
                          (loop for octet across properties
                                do (vector-push-extend octet octets))))))))
    (values index (coerce octets '(simple-array (unsigned-byte 8) (*))))))

(declaim (type (simple-array (unsigned-byte 32) (*)) *property-index*)
         (type (simple-array (unsigned-byte 8) (*)) *property-blocks*))
(multiple-value-bind (index blocks) (build-property-table)
  (defparameter *property-index* index)
  (defparameter *property-blocks* blocks))

(declaim (inline char-properties char-width extended-pictographic-p))
(defun char-properties (code)
  (declare (type (unsigned-byte 32) code) (optimize speed))
  (if (< code +codepoint-limit+)
      (aref *property-blocks*
            (+ (aref *property-index* (ash code (- +block-bits+)))
               (ldb (byte +block-bits+ 0) code)))
      +default-properties+))

(defun char-width (code)
  "Cells occupied by codepoint CODE: 0, 1 or 2."
  (ldb (byte +width-field+ 0) (char-properties code)))

(defun extended-pictographic-p (code)
  (logbitp +pictographic-bit+ (char-properties code)))
