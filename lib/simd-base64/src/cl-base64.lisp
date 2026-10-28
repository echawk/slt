(in-package #:simd-base64)

;;;; cl-base64's functions, by the same names and arguments, so a program
;;;; using cl-base64 can use these instead.  Digits are encoded and decoded
;;;; as above; what is added is the strings, streams and integers, line
;;;; breaks every COLUMNS digits, and leaving out whitespace when decoding.
;;;; Unlike cl-base64, a character that is neither a digit, padding nor
;;;; whitespace signals BASE64-ERROR rather than a warning.

(defun make-decode-table (encode-table)
  "A table of each octet's digit value in ENCODE-TABLE's alphabet, -1 for
octets that are not its digits."
  (let ((table (make-array (1+ +octet-mask+) :element-type 'fixnum
                                             :initial-element +not-a-digit+)))
    (loop for char across encode-table
          for value from 0
          do (setf (aref table (char-code char)) value))
    table))

(defvar *decode-table* (make-decode-table *encode-table*))
(defvar *uri-decode-table* (make-decode-table *uri-encode-table*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +whitespace+ '(#\Newline #\Return #\Space #\Tab)
    "What decoding leaves out between digits."))

(defconstant +last-whitespace-code+ (reduce #'max +whitespace+ :key #'char-code))

(defparameter +standard-decode-table+ (make-decode-table +alphabet+))

(defconstant +chunk-groups+ 4096
  "How many groups are encoded at a time on the way to a stream or string.")

(defconstant +chunk-octets+ (* +chunk-groups+ +octets-per-group+))

;;; Encoding

(defun string-octets (string)
  "STRING's characters as octets."
  (map 'octets (lambda (char)
                 (let ((code (char-code char)))
                   (unless (<= code +octet-mask+)
                     (error "~s is not the character of an octet" char))
                   code))
       string))

(defun integer-octets (integer)
  "INTEGER's octets, most significant first, as few as hold it."
  (check-type integer (integer 0))
  (let* ((count (ceiling (integer-length integer) +octet-bits+))
         (octets (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (i count octets)
      (setf (aref octets i)
            (ldb (byte +octet-bits+ (* +octet-bits+ (- count 1 i))) integer)))))

(defun map-encoded-chunks (function octets uri)
  "Call FUNCTION with each chunk of the base64 of OCTETS in turn, as an
octet vector and how many digits of it are the chunk's."
  (let* ((octets (coerce octets 'octets))
         (alphabet (alphabet-octets (if uri *uri-encode-table* *encode-table*)))
         (padding (char-code (if uri *uri-pad-char* *pad-char*)))
         (buffer (make-array (encoded-length (min (length octets) +chunk-octets+))
                             :element-type '(unsigned-byte 8))))
    (loop for start from 0 below (length octets) by +chunk-octets+
          for end = (min (length octets) (+ start +chunk-octets+))
          do (funcall function buffer (encode-octets octets start end buffer 0 alphabet padding)))))

(defun copy-digits (digits count string position column columns)
  "Copy COUNT DIGITS octets into STRING from POSITION, the line there
being COLUMN digits long, with a line break before a digit that would
make it longer than a positive COLUMNS; return the position and column
after them."
  (declare (type octets digits) (type index count position column)
           (type (simple-array character (*)) string) (type fixnum columns)
           (optimize speed))
  (flet ((copy (start end)
           (loop for i of-type index from start below end
                 do (setf (schar string position) (code-char (aref digits i)))
                    (incf position))))
    (if (plusp columns)
        ;; A line, or what of it is left, at a time.
        (let ((start 0))
          (declare (type index start))
          (loop while (< start count)
                do (when (= column columns)
                     (setf (schar string position) #\Newline
                           column 0)
                     (incf position))
                   (let ((end (min count (+ start (- columns column)))))
                     (copy start end)
                     (incf column (- end start))
                     (setf start end))))
        (copy 0 count)))
  (values position column))

(defun octets-to-base64-string (octets uri columns)
  (let ((string (make-string (encoded-length (length octets) :columns columns)))
        (position 0)
        (column 0))
    (map-encoded-chunks (lambda (digits count)
                          (multiple-value-setq (position column)
                            (copy-digits digits count string position column columns)))
                        octets uri)
    string))

(defun octets-to-base64-stream (octets stream uri columns)
  (let* ((digits (encoded-length (min (length octets) +chunk-octets+)))
         ;; Room for a line break before every digit.
         (string (make-string (if (plusp columns) (* 2 digits) digits)))
         (column 0))
    (map-encoded-chunks (lambda (digits count)
                          (multiple-value-bind (end next-column)
                              (copy-digits digits count string 0 column columns)
                            (write-string string stream :end end)
                            (setf column next-column)))
                        octets uri)
    stream))

(defun usb8-array-to-base64-string (input &key uri (columns 0))
  "The base64 of the octets INPUT, with a line break between lines of
COLUMNS digits when COLUMNS is positive."
  (octets-to-base64-string input uri columns))

(defun usb8-array-to-base64-stream (input output &key uri (columns 0))
  "Write the base64 of the octets INPUT to OUTPUT, as
USB8-ARRAY-TO-BASE64-STRING makes it; return OUTPUT."
  (octets-to-base64-stream input output uri columns))

(defun string-to-base64-string (input &key uri (columns 0))
  "The base64 of the string INPUT, a character to an octet, with a line
break between lines of COLUMNS digits when COLUMNS is positive."
  (octets-to-base64-string (string-octets input) uri columns))

(defun string-to-base64-stream (input output &key uri (columns 0))
  "Write the base64 of the string INPUT to OUTPUT, as
STRING-TO-BASE64-STRING makes it; return OUTPUT."
  (octets-to-base64-stream (string-octets input) output uri columns))

(defun integer-to-base64-string (input &key uri (columns 0))
  "The base64 of the non-negative integer INPUT's octets, most significant
first, with a line break between lines of COLUMNS digits when COLUMNS is
positive."
  (octets-to-base64-string (integer-octets input) uri columns))

(defun integer-to-base64-stream (input stream &key uri (columns 0))
  "Write the base64 of INPUT to STREAM, as INTEGER-TO-BASE64-STRING makes
it; return STREAM."
  (octets-to-base64-stream (integer-octets input) stream uri columns))

;;; Decoding

(defun digit-octets (string uri)
  "The digits and padding in STRING, in the alphabet URI chooses, as the
octets of the standard alphabet's, leaving out whitespace; and the
position in STRING of each octet, or NIL when that is its own."
  (declare (type string string))
  (let* ((string (if (typep string '(simple-array character (*)))
                     string
                     (coerce string '(simple-array character (*)))))
         (table (if uri *uri-decode-table* *decode-table*))
         (pad (if uri *uri-pad-char* *pad-char*))
         ;; The standard alphabet's digits and padding are their octets,
         ;; which decoding checks.
         (standard-p (and (equalp table +standard-decode-table+)
                          (char= pad (code-char +padding+))))
         (alphabet +alphabet-octets+)
         (octets (make-array (length string) :element-type '(unsigned-byte 8)))
         (count 0))
    (declare (type (simple-array character (*)) string)
             (type (simple-array fixnum (*)) table) (type character pad)
             (type alphabet alphabet) (type index count) (optimize speed))
    (flet ((skip-or-fail (char position)
             (unless (member char +whitespace+)
               (error 'base64-error :position position))))
      (if standard-p
          (loop for position of-type index from 0 below (length string)
                for char = (schar string position)
                for code = (char-code char)
                do (if (and (> code +last-whitespace-code+) (<= code +octet-mask+))
                       (progn (setf (aref octets count) code)
                              (incf count))
                       (skip-or-fail char position)))
          (loop for position of-type index from 0 below (length string)
                for char = (schar string position)
                for code = (char-code char)
                for value of-type fixnum = (if (< code (length table))
                                               (aref table code)
                                               +not-a-digit+)
                do (cond ((>= value 0)
                          (setf (aref octets count) (aref alphabet value))
                          (incf count))
                         ((char= char pad)
                          (setf (aref octets count) +padding+)
                          (incf count))
                         (t (skip-or-fail char position))))))
    (if (= count (length octets))
        (values octets nil)
        (values (subseq octets 0 count)
                (let ((positions (make-array count))
                      (index 0))
                  (dotimes (position (length string) positions)
                    (unless (member (schar string position) +whitespace+)
                      (setf (aref positions index) position)
                      (incf index))))))))

(defun octets-string (octets)
  "OCTETS as a string, a character to an octet."
  (map 'string #'code-char octets))

(defun base64-string-to-usb8-array (input &key uri)
  "The octets the base64 string INPUT encodes."
  (multiple-value-bind (octets positions) (digit-octets input uri)
    (handler-case (decode octets)
      (base64-error (condition)
        ;; Where in INPUT, rather than in its digits.
        (let ((position (base64-error-position condition)))
          (error 'base64-error :position (if (and positions position)
                                             (aref positions position)
                                             position)))))))

(defun base64-string-to-string (input &key uri)
  "The string the base64 string INPUT encodes, a character to an octet."
  (octets-string (base64-string-to-usb8-array input :uri uri)))

(defun base64-string-to-stream (input &key uri stream)
  "Write the string the base64 string INPUT encodes to STREAM; return
STREAM."
  (write-string (base64-string-to-string input :uri uri) stream)
  stream)

(defun base64-string-to-integer (string &key uri)
  "The integer whose octets, most significant first, the base64 STRING
encodes."
  (reduce (lambda (integer octet) (logior (ash integer +octet-bits+) octet))
          (base64-string-to-usb8-array string :uri uri)
          :initial-value 0))

(defun base64-stream-to-integer (stream &key uri)
  "The integer the base64 read from STREAM to its end encodes, as
BASE64-STRING-TO-INTEGER decodes it."
  (base64-string-to-integer
   (with-output-to-string (string)
     (loop for char = (read-char stream nil)
           while char
           do (write-char char string)))
   :uri uri))
