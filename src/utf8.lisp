(in-package #:slt)

;;;; Incremental UTF-8 decoding.  PTY reads split multi-byte sequences
;;;; arbitrarily, so the decoder carries an incomplete tail between calls.

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defstruct (utf8-decoder (:constructor make-utf8-decoder ()))
  ;; Codepoint bits accumulated so far and continuation bytes still needed.
  (partial 0 :type (unsigned-byte 21))
  (needed 0 :type (integer 0 3))
  (minimum 0 :type (unsigned-byte 21)))

(defconstant +replacement-character+ #xFFFD)

(defun utf8-decode (decoder octets &key (start 0) (end (length octets)))
  "Decode OCTETS[START, END) and return a fresh simple string.  Bytes that
end mid-sequence are retained in DECODER for the next call."
  (declare (type utf8-decoder decoder)
           (type octets octets)
           (type fixnum start end)
           (optimize (speed 3) (safety 0)))
  (let ((out (make-string (+ (- end start) 1)))
        (fill 0)
        (partial (utf8-decoder-partial decoder))
        (needed (utf8-decoder-needed decoder))
        (minimum (utf8-decoder-minimum decoder)))
    (declare (type fixnum fill)
             (type (unsigned-byte 21) partial minimum)
             (type (integer 0 3) needed))
    (flet ((emit (code)
             (declare (type (unsigned-byte 21) code))
             (setf (schar out fill)
                   (code-char (if (or (< #xD7FF code #xE000) (> code #x10FFFF))
                                  +replacement-character+
                                  code)))
             (incf fill)))
      (declare (inline emit))
      (loop for index of-type fixnum from start below end
            for byte of-type (unsigned-byte 8) = (aref octets index)
            do (cond
                 ((and (plusp needed) (= (logand byte #xC0) #x80))
                  (setf partial (logior (ash partial 6) (logand byte #x3F)))
                  (decf needed)
                  (when (zerop needed)
                    (emit (if (< partial minimum) +replacement-character+ partial))))
                 (t
                  (when (plusp needed)
                    ;; Truncated sequence: replace it and reprocess this byte.
                    (emit +replacement-character+)
                    (setf needed 0))
                  (cond
                    ((< byte #x80) (emit byte))
                    ((= (logand byte #xE0) #xC0)
                     (setf partial (logand byte #x1F) needed 1 minimum #x80))
                    ((= (logand byte #xF0) #xE0)
                     (setf partial (logand byte #x0F) needed 2 minimum #x800))
                    ((= (logand byte #xF8) #xF0)
                     (setf partial (logand byte #x07) needed 3 minimum #x10000))
                    (t (emit +replacement-character+)))))))
    (setf (utf8-decoder-partial decoder) partial
          (utf8-decoder-needed decoder) needed
          (utf8-decoder-minimum decoder) minimum)
    (subseq out 0 fill)))

(defun string->octets (string)
  (coerce (babel:string-to-octets string :encoding :utf-8)
          'octets))
