(in-package #:simd-base64)

;;;; Base64 encoding into octets.  A block of groups is encoded in one step
;;;; with NEON on arm64, for the standard and URI alphabets; spreading a
;;;; block's octets into its groups needs a byte shuffle, which SSE2 lacks,
;;;; so other machines and alphabets go a group at a time.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +uri-alphabet+
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
    "The base64 digits of the URI-safe alphabet, in order of value."))

;;; The alphabets and padding as cl-base64 names them, which may be bound
;;; to others.

(defvar *encode-table* +alphabet+)
(defvar *uri-encode-table* +uri-alphabet+)
(defvar *pad-char* (code-char +padding+))
(defvar *uri-pad-char* #\.)

(deftype alphabet () `(simple-array (unsigned-byte 8) (,(length +alphabet+))))

(defun alphabet-octets (alphabet)
  "ALPHABET, a string of the 64 digits in order of value, as their octets."
  (unless (= (length alphabet) (length +alphabet+))
    (error "A base64 alphabet has ~d digits, not ~d"
           (length +alphabet+) (length alphabet)))
  (map 'alphabet (lambda (char)
                   (let ((code (char-code char)))
                     (unless (<= code +octet-mask+)
                       (error "The base64 digit ~s is not an octet" char))
                     code))
       alphabet))

(defun encoded-length (count &key (columns 0))
  "How many digits COUNT octets encode to, padded, with a line break
between lines of COLUMNS digits when COLUMNS is positive."
  (let ((digits (* +digits-per-group+ (ceiling count +octets-per-group+))))
    (if (and (plusp columns) (plusp digits))
        (+ digits (floor (1- digits) columns))
        digits)))

;;; A block at a time.  The block's octets are spread over the LANES
;;; 32-bit lanes of a 128-bit vector, a group's octets in each lane as one
;;; big-endian number, which is split into the group's digit values, the
;;; first in the lane's low octet.  Each value then becomes its digit by
;;; adding the offset of the alphabet's run of consecutive octets it falls
;;; in.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun alphabet-runs (alphabet)
    "ALPHABET's runs of digits with consecutive octets, as (FIRST LAST
OFFSET): the values from FIRST to LAST are digits by adding OFFSET, modulo
256."
    (let ((runs '()))
      (loop for value from 0
            for char across alphabet
            for offset = (ldb (byte +octet-bits+ 0) (- (char-code char) value))
            for run = (first runs)
            do (if (and run (= offset (third run)))
                   (setf (second run) value)
                   (push (list value value offset) runs)))
      (nreverse runs))))

(defmacro define-block-encoder (name package alphabet)
  "Define (NAME INPUT START END OUTPUT POSITION), which encodes blocks of
INPUT from START, in the alphabet named ALPHABET, into OUTPUT from
POSITION while they fit, and returns START and POSITION after them."
  (flet ((op (name) (intern (string name) package)))
    (let* ((broadcast (op "U8.16"))
           (word (op "U32.4"))
           (runs (alphabet-runs (symbol-value alphabet)))
           (any (lambda (operator forms)
                  (reduce (lambda (a b) `(,(op operator) ,a ,b)) forms)))
           ;; NEON cannot encode a right shift by zero.
           (shift (lambda (direction form count)
                    (if (zerop count) form `(,(op direction) ,form ,count))))
           (in-run (lambda (first last)
                     (cond ((= first last) `(,(op "U8.16=") digits (,broadcast ,first)))
                           ((zerop first) `(,(op "U8.16<=") digits (,broadcast ,last)))
                           (t `(,(op "U8.16-AND")
                                (,(op "U8.16>=") digits (,broadcast ,first))
                                (,(op "U8.16<=") digits (,broadcast ,last))))))))
      `(progn
         (declaim (inline ,name))
         (defun ,name (input start end output position)
           (declare (type octets input output)
                    (type index start end position)
                    (optimize speed (safety 0)))
           (let ((input-length (length input))
                 (output-length (length output)))
             (declare (type index input-length output-length))
             ;; A whole vector is loaded, of which a block is used.
             (loop while (and (<= (+ start +block-octets+) end)
                              (<= (+ start +vector-octets+) input-length)
                              (<= (+ position +block-digits+) output-length))
                   do (let* ((groups
                               (,(op "U32.4!")
                                (,(op "U8.16-SHUFFLE")
                                 (,(op "U8.16-AREF") input start)
                                 (,(op "MAKE-U8.16")
                                  ,@(loop for i below +vector-octets+
                                          collect (multiple-value-bind (lane octet)
                                                      (floor i +lane-octets+)
                                                    (if (< octet +octets-per-group+)
                                                        (+ (* lane +octets-per-group+)
                                                           (- +octets-per-group+ 1 octet))
                                                        +shuffle-zero+)))))))
                             (digits
                               (,(op "U8.16!")
                                ,(funcall
                                  any "U32.4-OR"
                                  (loop for digit below +digits-per-group+
                                        collect (funcall
                                                 shift "U32.4-SHIFTL"
                                                 `(,(op "U32.4-AND")
                                                   ,(funcall shift "U32.4-SHIFTR" 'groups
                                                             (* +bits-per-digit+
                                                                (- +digits-per-group+ 1 digit)))
                                                   (,word ,+digit-mask+))
                                                 (* digit +octet-bits+)))))))
                        (setf (,(op "U8.16-AREF") output position)
                              (,(op "U8.16+")
                               digits
                               ,(funcall any "U8.16-OR"
                                         (loop for (first last offset) in runs
                                               collect `(,(op "U8.16-AND")
                                                         ,(funcall in-run first last)
                                                         (,broadcast ,offset))))))
                        (incf start +block-octets+)
                        (incf position +block-digits+)))
             (values start position)))))))

#+simd-base64-neon
(progn
  (define-block-encoder encode-blocks "SB-SIMD-NEON" +alphabet+)
  (define-block-encoder encode-uri-blocks "SB-SIMD-NEON" +uri-alphabet+))

#-simd-base64-neon
(progn
  (declaim (inline encode-blocks encode-uri-blocks))
  (defun encode-blocks (input start end output position)
    (declare (ignore input end output))
    (values start position))
  (defun encode-uri-blocks (input start end output position)
    (declare (ignore input end output))
    (values start position)))

(defparameter +alphabet-octets+ (alphabet-octets +alphabet+))
(defparameter +uri-alphabet-octets+ (alphabet-octets +uri-alphabet+))

(defun encode-octets (input start end output position alphabet padding)
  "Write the base64 of INPUT from START to END, in ALPHABET (its digits'
octets) padded with PADDING, into OUTPUT from POSITION; return the
position after it."
  (declare (type octets input output) (type alphabet alphabet)
           (type index start end position) (type (unsigned-byte 8) padding)
           (optimize speed))
  (multiple-value-setq (start position)
    (cond ((equalp alphabet +alphabet-octets+)
           (encode-blocks input start end output position))
          ((equalp alphabet +uri-alphabet-octets+)
           (encode-uri-blocks input start end output position))
          (t (values start position))))
  (flet ((encode-group (octets)
           ;; OCTETS octets from START as a group, padded when short.
           (let ((group 0)
                 (digits (ceiling (* octets +octet-bits+) +bits-per-digit+)))
             (declare (type (unsigned-byte #.+group-bits+) group))
             (dotimes (i +octets-per-group+)
               (setf group (logior (ash group +octet-bits+)
                                   (if (< i octets) (aref input (+ start i)) 0))))
             (dotimes (digit +digits-per-group+)
               (setf (aref output (+ position digit))
                     (if (< digit digits)
                         (aref alphabet (ldb (byte +bits-per-digit+
                                                   (* +bits-per-digit+
                                                      (- +digits-per-group+ 1 digit)))
                                             group))
                         padding)))
             (incf start octets)
             (incf position +digits-per-group+))))
    (loop while (<= (+ start +octets-per-group+) end)
          do (encode-group +octets-per-group+))
    (when (< start end)
      (encode-group (- end start))))
  position)

(defun encode (octets &key (start 0) (end (length octets)) uri)
  "The base64 of OCTETS from START to END, padded, as a new octet vector:
in the standard alphabet, or with URI in *URI-ENCODE-TABLE* padded with
*URI-PAD-CHAR*."
  (let* ((octets (coerce octets 'octets))
         (output (make-array (encoded-length (- end start)) :element-type '(unsigned-byte 8))))
    (encode-octets octets start end output 0
                   (alphabet-octets (if uri *uri-encode-table* *encode-table*))
                   (char-code (if uri *uri-pad-char* *pad-char*)))
    output))
