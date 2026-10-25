(in-package #:ecma48)

;;;; An incremental scanner for ECMA-48 (ISO 6429) control functions in a
;;;; UTF-8 byte stream, following the DEC/xterm conventions terminals share
;;;; (Paul Williams' parser state machine).  SCAN calls a function with one
;;;; EVENT per text run, C0 control, escape sequence, control sequence (CSI)
;;;; or control string (OSC, DCS, APC, PM, SOS), however the input is split
;;;; into chunks.  Every event carries its raw bytes, so a caller can pass
;;;; on what it does not handle unchanged: the raw bytes of all events are
;;;; the input in order, except that a C0 control met inside an escape or
;;;; control sequence is reported (and executed, as terminals do) before the
;;;; sequence that contains it.
;;;;
;;;; In UTF-8 the 8-bit C1 controls are not recognized: bytes 80-FF are text.

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defconstant +max-parameters+ 32)

(defconstant +max-sequence-length+ 1024
  "Escape and control sequences longer than this (in octets) are passed on
as :RAW events instead of being reported.")

(defconstant +esc+ 27)
(defconstant +bel+ 7)
(defconstant +can+ 24)
(defconstant +sub+ 26)
(defconstant +del+ 127)

(defvar *empty-octets* (make-array 0 :element-type '(unsigned-byte 8)))

(defstruct (event (:copier nil))
  "What SCAN reports.  KIND is :TEXT, :CONTROL (a C0 control or DEL, the
FINAL octet), :ESCAPE (ESC, INTERMEDIATES, FINAL), :CSI, :OSC, :DCS, :APC,
:PM, :SOS or :RAW (octets passed on without interpretation: aborted, overlong
or declined sequences).  The raw octets are OCTETS from START to END; for
control strings the payload is DATA-START to DATA-END.  The scanner reuses
one event: copy it (COPY-EVENT) to keep it."
  (kind :text :type keyword)
  (final 0 :type (unsigned-byte 8))
  ;; CSI and DCS: the private-use marker (< = > ?), or 0.
  (private 0 :type (unsigned-byte 8))
  ;; Up to two intermediate octets, the first in the low byte.
  (intermediates 0 :type (unsigned-byte 16))
  (intermediate-count 0 :type fixnum)
  (parameter-values (make-array +max-parameters+ :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (#.+max-parameters+)))
  (parameter-count 0 :type fixnum)
  ;; Bit I: parameter I was empty (DEFAULT-P); parameter I followed a colon
  ;; (a sub-parameter of the one before).
  (defaults 0 :type (unsigned-byte 32))
  (colons 0 :type (unsigned-byte 32))
  ;; False for a sequence that broke the syntax (parameters after
  ;; intermediates, a misplaced private marker, too many intermediates).
  (valid-p t)
  ;; Control strings: :ST, :BEL (OSC only) or :INTERRUPTED (by another ESC).
  (terminator nil)
  (octets *empty-octets* :type octets)
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (data-start 0 :type fixnum)
  (data-end 0 :type fixnum))

(defun copy-event (event)
  "A copy of EVENT that owns its octets and parameters."
  (let ((copy (copy-structure event)))
    (setf (event-parameter-values copy) (copy-seq (event-parameter-values event))
          (event-octets copy) (subseq (event-octets event) (event-start event)
                                      (event-end event))
          (event-data-start copy) (- (event-data-start event) (event-start event))
          (event-data-end copy) (- (event-data-end event) (event-start event))
          (event-end copy) (- (event-end event) (event-start event))
          (event-start copy) 0)
    copy))

(defun event-parameter (event index &optional (default 0))
  "Parameter INDEX of a CSI or DCS event, or DEFAULT when absent or empty."
  (if (and (< index (event-parameter-count event))
           (not (logbitp index (event-defaults event))))
      (aref (event-parameter-values event) index)
      default))

(defun event-parameter-default-p (event index)
  (or (>= index (event-parameter-count event))
      (logbitp index (event-defaults event))))

(defun event-subparameter-p (event index)
  "True when parameter INDEX followed a colon: a sub-parameter."
  (logbitp index (event-colons event)))

(defun event-parameters (event &optional (default 0))
  "The parameters as a list, empty ones as DEFAULT."
  (loop for index below (event-parameter-count event)
        collect (event-parameter event index default)))

(defun event-intermediate (event &optional (index 0))
  "Intermediate octet INDEX, or 0."
  (if (< index (event-intermediate-count event))
      (ldb (byte 8 (* 8 index)) (event-intermediates event))
      0))

(defun event-data (event)
  "A control string's payload, as a fresh octet vector."
  (subseq (event-octets event) (event-data-start event) (event-data-end event)))

(defun event-raw (event)
  "The event's raw octets, as a fresh octet vector."
  (subseq (event-octets event) (event-start event) (event-end event)))

;;; The scanner

(defstruct (scanner (:constructor make-scanner
                        (&key (max-string-length (* 64 1024 1024)) string-policy)))
  "MAX-STRING-LENGTH bounds a collected control string; longer ones are
passed on as :RAW.  STRING-POLICY, if given, is called with the string's
kind, the (partly filled) event and its first payload octet (a DCS's final
octet) once that is known, and returns :COLLECT to have the string reported
whole or :PASS to have it passed on as :RAW events as it arrives; with
:COLLECT, an integer second value lowers the length limit for that string."
  (state :ground :type keyword)
  (string-kind :osc :type keyword)
  (max-string-length (* 64 1024 1024) :type fixnum)
  (string-policy nil)
  ;; Octets of the current sequence from earlier chunks (or before a C0
  ;; control inside it): BUFFER below FILL.  The rest lie in the current
  ;; chunk from SEGMENT.
  (buffer (make-array 256 :element-type '(unsigned-byte 8)) :type octets)
  (fill 0 :type fixnum)
  (segment 0 :type fixnum)
  ;; The parameter being read, or -1 before its first digit.
  (parameter -1 :type fixnum)
  (parameter-started nil)
  ;; Offset of a control string's payload from the start of its octets,
  ;; and the length of its terminator.
  (data-offset 0 :type fixnum)
  (data-trim 0 :type fixnum)
  ;; The limit for the string being collected.
  (string-limit 0 :type fixnum)
  (event (make-event) :type event))

(defun scanner-reset (scanner)
  "Forget any partial sequence."
  (setf (scanner-state scanner) :ground
        (scanner-fill scanner) 0)
  (when (> (length (scanner-buffer scanner)) (* 1024 1024))
    (setf (scanner-buffer scanner) (make-array 256 :element-type '(unsigned-byte 8))))
  scanner)

(defun scanner-in-sequence-p (scanner)
  "True when the scanner holds the beginning of an unfinished sequence."
  (not (eq (scanner-state scanner) :ground)))

(declaim (inline buffer-append))
(defun buffer-append (scanner octets start end)
  (declare (type scanner scanner) (type octets octets) (type fixnum start end))
  (when (< start end)
    (let* ((fill (scanner-fill scanner))
           (needed (+ fill (- end start)))
           (buffer (scanner-buffer scanner)))
      (when (< (length buffer) needed)
        (let ((bigger (make-array (max needed (* 2 (length buffer)))
                                  :element-type '(unsigned-byte 8))))
          (replace bigger buffer :end2 fill)
          (setf buffer bigger
                (scanner-buffer scanner) bigger)))
      (replace buffer octets :start1 fill :start2 start :end2 end)
      (setf (scanner-fill scanner) needed))))

(defun scan (scanner octets function &key (start 0) (end (length octets)))
  "Scan OCTETS from START to END, calling FUNCTION with an EVENT for each
complete item.  A sequence left unfinished at END is completed by later
calls."
  (declare (type scanner scanner) (type fixnum start end)
           (type function function))
  (let ((octets (if (typep octets 'octets) octets (coerce octets 'octets)))
        (event (scanner-event scanner))
        (index start))
    (declare (type octets octets) (type fixnum index))
    (setf (scanner-segment scanner) start)
    (labels ((state! (state) (setf (scanner-state scanner) state))
             (emit (kind from to)
               (setf (event-kind event) kind
                     (event-octets event) octets
                     (event-start event) from
                     (event-end event) to)
               (funcall function event))
             (length-so-far (at)
               (+ (scanner-fill scanner) (- at (scanner-segment scanner))))
             (begin (state at)
               ;; A sequence starts with the ESC at AT.
               (setf (scanner-fill scanner) 0
                     (scanner-segment scanner) at
                     (scanner-parameter scanner) -1
                     (scanner-parameter-started scanner) nil
                     (event-final event) 0
                     (event-private event) 0
                     (event-intermediates event) 0
                     (event-intermediate-count event) 0
                     (event-parameter-count event) 0
                     (event-defaults event) 0
                     (event-colons event) 0
                     (event-valid-p event) t
                     (event-terminator event) nil)
               (state! state))
             (finish (kind at)
               ;; The sequence ends before AT: report it.
               (if (zerop (scanner-fill scanner))
                   (setf (event-octets event) octets
                         (event-start event) (scanner-segment scanner)
                         (event-end event) at)
                   (progn
                     (buffer-append scanner octets (scanner-segment scanner) at)
                     (setf (event-octets event) (scanner-buffer scanner)
                           (event-start event) 0
                           (event-end event) (scanner-fill scanner))))
               (setf (event-kind event) kind
                     (scanner-fill scanner) 0
                     (scanner-segment scanner) at)
               (when (member kind '(:osc :dcs :apc :pm :sos))
                 (let ((data-start (+ (event-start event) (scanner-data-offset scanner))))
                   (setf (event-data-start event) data-start
                         (event-data-end event) (max data-start
                                                     (- (event-end event)
                                                        (scanner-data-trim scanner))))))
               (state! :ground)
               (funcall function event)
               ;; Let a large string's buffer go.
               (when (> (length (scanner-buffer scanner)) (* 1024 1024))
                 (setf (scanner-buffer scanner)
                       (make-array 256 :element-type '(unsigned-byte 8)))))
             (pass-partial (at)
               ;; Give up on the sequence so far (before AT): pass it on.
               (finish :raw at))
             (execute (byte-index)
               ;; A C0 control inside a sequence: report it now, keep the
               ;; sequence's octets apart from it.
               (buffer-append scanner octets (scanner-segment scanner) byte-index)
               (let ((state (scanner-state scanner)))
                 (setf (scanner-segment scanner) (1+ byte-index))
                 (emit :control byte-index (1+ byte-index))
                 (setf (event-final event) (aref octets byte-index))
                 (state! state)))
             (end-parameter ()
               (let ((count (event-parameter-count event))
                     (value (scanner-parameter scanner)))
                 (when (< count +max-parameters+)
                   (setf (aref (event-parameter-values event) count) (max value 0))
                   (when (minusp value)
                     (setf (event-defaults event) (logior (event-defaults event)
                                                          (ash 1 count))))
                   (setf (event-parameter-count event) (1+ count)))
                 (setf (scanner-parameter scanner) -1)))
             (add-intermediate (byte)
               (let ((count (event-intermediate-count event)))
                 (if (< count 2)
                     (setf (event-intermediates event)
                           (logior (event-intermediates event) (ash byte (* 8 count)))
                           (event-intermediate-count event) (1+ count))
                     (setf (event-valid-p event) nil))))
             (parameter-byte (byte)
               ;; 0-9 : ; in a CSI or DCS header; return NIL for others.
               (cond
                 ((<= 48 byte 57)
                  (setf (scanner-parameter scanner)
                        (min 999999 (+ (* 10 (max 0 (scanner-parameter scanner)))
                                       (- byte 48)))
                        (scanner-parameter-started scanner) t)
                  t)
                 ((or (= byte 59) (= byte 58))
                  (end-parameter)
                  (when (and (= byte 58) (< (event-parameter-count event) +max-parameters+))
                    (setf (event-colons event)
                          (logior (event-colons event)
                                  (ash 1 (event-parameter-count event)))))
                  (setf (scanner-parameter-started scanner) t)
                  t)))
             (end-parameters ()
               (when (or (scanner-parameter-started scanner)
                         (>= (scanner-parameter scanner) 0))
                 (end-parameter)
                 (setf (scanner-parameter-started scanner) nil)))
             (start-string (kind)
               (setf (scanner-string-kind scanner) kind)
               (state! :string-pending))
             (decide-string (first-byte at)
               ;; The string's identity is known: collect it or pass it on.
               (let ((policy (scanner-string-policy scanner)))
                 (setf (event-kind event) (scanner-string-kind scanner))
                 (multiple-value-bind (decision limit)
                     (if policy
                         (funcall policy (scanner-string-kind scanner) event first-byte)
                         :collect)
                   (if (eq decision :pass)
                       (progn (pass-partial at) (state! :string-pass))
                       (progn (setf (scanner-data-offset scanner) (length-so-far at)
                                    (scanner-string-limit scanner)
                                    (min (scanner-max-string-length scanner)
                                         (if (integerp limit) limit most-positive-fixnum)))
                              (state! :string))))))
             (finish-string (terminator data-end-from-end at)
               ;; The string ends before AT; its payload ends
               ;; DATA-END-FROM-END octets before that.
               (setf (event-terminator event) terminator
                     (scanner-data-trim scanner) data-end-from-end)
               (finish (scanner-string-kind scanner) at))
             (string-terminator-p (byte)
               (or (= byte +esc+) (= byte +can+) (= byte +sub+)
                   (and (= byte +bel+) (eq (scanner-string-kind scanner) :osc)))))
      (declare (inline state! emit length-so-far end-parameter parameter-byte))
      (loop while (< index end)
            do (let ((byte (aref octets index)))
                 (ecase (scanner-state scanner)
                   (:ground
                    (let ((stop (or (position-if (lambda (b) (or (< b 32) (= b +del+)))
                                                 octets :start index :end end)
                                    end)))
                      (when (< index stop)
                        (emit :text index stop))
                      (setf index stop)
                      (when (< index end)
                        (let ((control (aref octets index)))
                          (if (= control +esc+)
                              (begin :escape index)
                              (progn
                                (setf (event-final event) control)
                                (emit :control index (1+ index)))))
                        (incf index))))
                   ((:escape :escape-intermediate)
                    (cond
                      ((or (= byte +can+) (= byte +sub+))
                       (pass-partial index)
                       (setf (event-final event) byte)
                       (emit :control index (1+ index)))
                      ((= byte +esc+)
                       (pass-partial index)
                       (begin :escape index))
                      ((< byte 32) (execute index))
                      ((<= 32 byte 47)
                       (add-intermediate byte)
                       (state! :escape-intermediate))
                      ((and (eq (scanner-state scanner) :escape) (= byte 91)) ; [
                       (state! :csi))
                      ((and (eq (scanner-state scanner) :escape) (= byte 80)) ; P
                       (state! :dcs-header))
                      ((and (eq (scanner-state scanner) :escape) (= byte 93)) ; ]
                       (start-string :osc))
                      ((and (eq (scanner-state scanner) :escape) (= byte 95)) ; _
                       (start-string :apc))
                      ((and (eq (scanner-state scanner) :escape) (= byte 94)) ; ^
                       (start-string :pm))
                      ((and (eq (scanner-state scanner) :escape) (= byte 88)) ; X
                       (start-string :sos))
                      ((<= 48 byte 126)
                       (setf (event-final event) byte)
                       (finish :escape (1+ index)))
                      ((= byte +del+))
                      (t
                       ;; Not an escape sequence: pass the ESC on, then BYTE
                       ;; as text.
                       (pass-partial index)
                       (decf index)))
                    (incf index))
                   ((:csi :csi-intermediate :csi-ignore :dcs-header :dcs-intermediate
                     :dcs-ignore)
                    (let* ((state (scanner-state scanner))
                           (dcs (member state '(:dcs-header :dcs-intermediate :dcs-ignore))))
                      (cond
                        ((or (= byte +can+) (= byte +sub+))
                         (pass-partial index)
                         (setf (event-final event) byte)
                         (emit :control index (1+ index)))
                        ((= byte +esc+)
                         (pass-partial index)
                         (begin :escape index))
                        ((< byte 32)
                         (if dcs
                             ;; C0 controls in a DCS header are ignored.
                             nil
                             (execute index)))
                        ((<= 64 byte 126)
                         (end-parameters)
                         (setf (event-final event) byte)
                         (when (member state '(:csi-ignore :dcs-ignore))
                           (setf (event-valid-p event) nil))
                         (if dcs
                             (progn
                               (setf (scanner-string-kind scanner) :dcs)
                               (decide-string byte (1+ index)))
                             (finish :csi (1+ index))))
                        ((member state '(:csi-ignore :dcs-ignore)))
                        ((<= 32 byte 47)
                         (end-parameters)
                         (add-intermediate byte)
                         (state! (if dcs :dcs-intermediate :csi-intermediate)))
                        ((member state '(:csi-intermediate :dcs-intermediate))
                         ;; Parameters after intermediates.
                         (state! (if dcs :dcs-ignore :csi-ignore)))
                        ((parameter-byte byte))
                        ((<= 60 byte 63)
                         (if (and (zerop (event-private event))
                                  (not (scanner-parameter-started scanner))
                                  (zerop (event-parameter-count event)))
                             (setf (event-private event) byte)
                             (state! (if dcs :dcs-ignore :csi-ignore))))
                        ((= byte +del+))
                        (t (state! (if dcs :dcs-ignore :csi-ignore))))
                      (when (and (member (scanner-state scanner)
                                         '(:csi :csi-intermediate :csi-ignore :dcs-header
                                           :dcs-intermediate :dcs-ignore))
                                 (> (length-so-far (1+ index)) +max-sequence-length+))
                        ;; Too long to be real: pass it on.
                        (pass-partial (1+ index))
                        (state! :sequence-pass)))
                    (incf index))
                   (:sequence-pass
                    ;; Pass octets on through the final octet of an overlong
                    ;; sequence; CAN, SUB and ESC end it as in any sequence.
                    (let* ((stop (position-if (lambda (b) (or (<= 64 b 126) (= b +can+)
                                                              (= b +sub+) (= b +esc+)))
                                              octets :start index :end end))
                           (final (and stop (<= 64 (aref octets stop) 126))))
                      (emit :raw index (if final (1+ stop) (or stop end)))
                      (setf index (if final (1+ stop) (or stop end))
                            (scanner-segment scanner) index)
                      (when stop (state! :ground))))
                   (:string-pending
                    (if (string-terminator-p byte)
                        (progn (decide-string byte index) ; an empty string
                               (setf (event-kind event) (scanner-string-kind scanner)))
                        (progn (decide-string byte index)
                               ;; A passed string's first octet goes out
                               ;; with the rest of it.
                               (unless (eq (scanner-state scanner) :string-pass)
                                 (incf index)))))
                   (:string
                    (let ((stop (position-if #'string-terminator-p octets
                                             :start index :end end)))
                      (setf index (or stop end))
                      (when (> (length-so-far index) (scanner-string-limit scanner))
                        ;; Too long to collect: pass it on as it comes.
                        (pass-partial index)
                        (state! :string-pass))
                      (when (and stop (eq (scanner-state scanner) :string))
                        (let ((terminator (aref octets stop)))
                          (cond
                            ((= terminator +esc+) (state! :string-escape) (incf index))
                            ((= terminator +bel+)
                             (finish-string :bel 1 (1+ stop))
                             (incf index))
                            (t
                             (pass-partial stop)
                             (setf (event-final event) terminator)
                             (emit :control stop (1+ stop))
                             (incf index)))))))
                   (:string-escape
                    (if (= byte 92)    ; \
                        (progn (finish-string :st 2 (1+ index)) (incf index))
                        ;; ESC ends the string early and begins a sequence.
                        (progn
                          ;; The ESC is not the string's: it begins the new
                          ;; sequence.
                          (if (< (scanner-segment scanner) index)
                              (finish-string :interrupted 0 (1- index))
                              (progn (decf (scanner-fill scanner))
                                     (finish-string :interrupted 0 index)))
                          (begin :escape index)
                          (buffer-append scanner (make-array 1 :element-type '(unsigned-byte 8)
                                                               :initial-element +esc+)
                                         0 1))))
                   (:string-pass
                    (let ((stop (position-if #'string-terminator-p octets
                                             :start index :end end)))
                      (when (< index (or stop end))
                        (emit :raw index (or stop end)))
                      (setf index (or stop end)
                            (scanner-segment scanner) index)
                      (when stop
                        (let ((terminator (aref octets stop)))
                          (cond
                            ((= terminator +esc+)
                             (setf (scanner-fill scanner) 0)
                             (buffer-append scanner octets stop (1+ stop))
                             (setf (scanner-segment scanner) (1+ stop))
                             (state! :string-pass-escape))
                            ((= terminator +bel+)
                             (emit :raw stop (1+ stop))
                             (state! :ground))
                            (t
                             (setf (event-final event) terminator)
                             (emit :control stop (1+ stop))
                             (state! :ground))))
                        (incf index)
                        (setf (scanner-segment scanner) index))))
                   (:string-pass-escape
                    (if (= byte 92)
                        (progn
                          (buffer-append scanner octets index (1+ index))
                          (setf (event-kind event) :raw
                                (event-octets event) (scanner-buffer scanner)
                                (event-start event) 0
                                (event-end event) (scanner-fill scanner)
                                (scanner-fill scanner) 0)
                          (state! :ground)
                          (funcall function event)
                          (incf index)
                          (setf (scanner-segment scanner) index))
                        ;; The held ESC begins a new sequence.
                        (progn
                          (state! :escape)
                          (setf (scanner-segment scanner) index
                                (scanner-parameter scanner) -1
                                (scanner-parameter-started scanner) nil
                                (event-final event) 0 (event-private event) 0
                                (event-intermediates event) 0
                                (event-intermediate-count event) 0
                                (event-parameter-count event) 0
                                (event-defaults event) 0 (event-colons event) 0
                                (event-valid-p event) t (event-terminator event) nil))))))))
    ;; Keep an unfinished sequence's octets for the next call.
    (unless (member (scanner-state scanner) '(:ground :sequence-pass :string-pass))
      (buffer-append scanner octets (scanner-segment scanner) end))
    (setf (scanner-segment scanner) end)
    scanner))

(defun scanner-flush (scanner function)
  "Pass on the octets of an unfinished sequence as a :RAW event (at the end
of the input, say) and return to the ground state."
  (when (and (plusp (scanner-fill scanner))
             (not (member (scanner-state scanner) '(:ground :sequence-pass :string-pass))))
    (let ((event (scanner-event scanner)))
      (setf (event-kind event) :raw
            (event-octets event) (scanner-buffer scanner)
            (event-start event) 0
            (event-end event) (scanner-fill scanner))
      (funcall function event)))
  (scanner-reset scanner))
