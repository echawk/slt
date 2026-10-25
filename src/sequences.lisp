(in-package #:slt)

;;;; The control-sequence interceptor.  Child output is scanned with ecma48
;;;; before any engine sees it.  The few control functions slt implements
;;;; itself, the same way whichever engine runs, are taken out here: SIXEL
;;;; and Kitty graphics, the OSC strings of doc/escape-sequences.md, the
;;;; Kitty keyboard protocol, and some queries.  Others are observed on
;;;; their way through (modes, erasures, RIS).  Everything else reaches the
;;;; engine unchanged and in order, as large slices of the input.
;;;;
;;;; TERMINAL-STREAM-FEED calls a handler with events, lists such as
;;;; (:SIXEL octets), (:OSC number text terminator) or (:MODE 2026 T); see
;;;; CLASSIFY-STREAM-EVENT for all of them.  Events about state the engine
;;;; keeps (modes, erasures, RIS, text) come after the engine has seen the
;;;; sequence, so its cursor and screen are current; :TEXT events come
;;;; before.

(defstruct (terminal-stream-parser (:constructor %make-terminal-stream-parser))
  (scanner nil :type (or null ecma48:scanner))
  ;; Report erasures (EL, ECH, ED 0/1) and text, which split the engine's
  ;; input: only while something depends on them.
  (erase-events-p nil)
  (text-events-p nil))

(defparameter *osc-numbers-handled*
  '(4 7 8 9 10 11 12 17 19 52 99 104 110 111 112 117 119 133 777)
  "OSC numbers slt implements; others (the title among them) are left to
the engine.")

(defun stream-string-policy (kind event first-octet)
  "Collect the control strings slt may act on; pass the rest on at once."
  (case kind
    (:dcs (if (and *sixel-images*
                   (= first-octet (char-code #\q))
                   (zerop (ecma48:event-private event))
                   (zerop (ecma48:event-intermediates event)))
              :collect :pass))
    (:apc (if (and *kitty-graphics-images* (= first-octet (char-code #\G)))
              :collect :pass))
    ;; OSC strings are small but for OSC 52; bound them well below images.
    (:osc (values :collect (* 4 1024 1024)))
    (t :pass)))

(defun make-terminal-stream-parser (&key (max-control-bytes (* 70 1024 1024)))
  (%make-terminal-stream-parser
   :scanner (ecma48:make-scanner :max-string-length max-control-bytes
                                 :string-policy #'stream-string-policy)))

(defun classify-csi (parser event)
  (let ((private (ecma48:event-private event))
        (intermediate (ecma48:event-intermediate event))
        (final (code-char (ecma48:event-final event)))
        (first (ecma48:event-parameter event 0)))
    (flet ((private-p (char) (= private (char-code char)))
           (plain-p () (and (zerop private) (zerop intermediate))))
      (cond
        ((and (private-p #\?) (zerop intermediate) (member final '(#\h #\l)))
         (let ((value (char= final #\h))
               (events '()))
           (dolist (mode (ecma48:event-parameters event))
             (case mode
               ((80 8452 2026) (push (list :mode mode value) events))
               ((1047 1049) (when value (push (list :enter-alternate-screen) events)))))
           (if events (values :pass-then (nreverse events)) :pass)))
        ((and (plain-p) (char= final #\J)
              (or (member first '(2 3))
                  (and (terminal-stream-parser-erase-events-p parser) (member first '(0 1)))))
         (values :pass-then (list (list :erase-display first))))
        ((and (plain-p) (terminal-stream-parser-erase-events-p parser)
              (member final '(#\K #\X)))
         (values :pass-then (list (list (if (char= final #\K) :erase-line :erase-characters)
                                        first))))
        ((and (private-p #\?) (= intermediate (char-code #\$)) (char= final #\p)
              (member first '(80 8452 2026)))
         (values :consume (list (list :report-mode first))))
        ((and (private-p #\?) (zerop intermediate) (char= final #\S) *sixel-images*
              (member first '(1 2)))
         (values :consume (list (list :graphics-attributes first
                                      (ecma48:event-parameter event 1)))))
        ((and (plain-p) (char= final #\c) (zerop first) *sixel-images*)
         (values :consume (list (list :primary-device-attributes))))
        ((and (zerop intermediate) (char= final #\u)
              (member private (mapcar #'char-code '(#\= #\> #\< #\?))))
         (values :consume
                 (list (list :keyboard (code-char private)
                             (loop for index below (ecma48:event-parameter-count event)
                                   collect (and (not (ecma48:event-parameter-default-p
                                                      event index))
                                                (ecma48:event-parameter event index)))))))
        ((and (private-p #\>) (zerop intermediate) (char= final #\q) (zerop first))
         (values :consume (list (list :version))))
        (t :pass)))))

(defun classify-stream-event (parser event)
  "What to do with a scanner EVENT: :PASS it to the engine, :CONSUME it,
:PASS-THEN act on it, or act on it BEFORE passing it; and the events to
act on."
  (case (ecma48:event-kind event)
    (:text (if (terminal-stream-parser-text-events-p parser)
               (values :before (list (list :text (ecma48:event-octets event)
                                           (ecma48:event-start event)
                                           (ecma48:event-end event))))
               :pass))
    ((:control :raw) :pass)
    (:escape (if (and (= (ecma48:event-final event) (char-code #\c))
                      (zerop (ecma48:event-intermediates event)))
                 (values :pass-then (list (list :reset)))
                 :pass))
    (:csi (if (ecma48:event-valid-p event) (classify-csi parser event) :pass))
    (:osc
     (multiple-value-bind (number text)
         (terminal-osc:parse-osc (ecma48:event-octets event)
                                 :start (ecma48:event-data-start event)
                                 :end (ecma48:event-data-end event))
       (if (and number (member number *osc-numbers-handled*)
                (not (eq (ecma48:event-terminator event) :interrupted)))
           (values :consume (list (list :osc number text
                                        (if (eq (ecma48:event-terminator event) :bel)
                                            :bel :st))))
           :pass)))
    (:dcs (if (eq (ecma48:event-terminator event) :interrupted)
              (values :consume '())   ; an unfinished image
              (values :consume (list (list :sixel (ecma48:event-raw event))))))
    (:apc (if (eq (ecma48:event-terminator event) :interrupted)
              (values :consume '())
              (values :consume (list (list :kitty (ecma48:event-data event))))))
    (t :pass)))

(defun terminal-stream-feed (parser octets feed-normal handler
                             &key (start 0) (end (length octets)))
  "Scan child output OCTETS from START to END.  FEED-NORMAL is called with
(OCTETS START END) for what the engine should process, HANDLER with each
event (see CLASSIFY-STREAM-EVENT)."
  (unless (typep octets '(simple-array (unsigned-byte 8) (*)))
    (setf octets (coerce (subseq octets start end) '(simple-array (unsigned-byte 8) (*)))
          start 0
          end (length octets)))
  (let ((pending nil)
        (pending-start 0)
        (pending-end 0))
    (labels ((flush ()
               (when pending
                 (let ((vector pending))
                   (setf pending nil)
                   (funcall feed-normal vector pending-start pending-end))))
             (pass (event)
               (let ((vector (ecma48:event-octets event))
                     (from (ecma48:event-start event))
                     (to (ecma48:event-end event)))
                 (cond
                   ((>= from to))
                   ((and (eq vector pending) (= from pending-end))
                    (setf pending-end to))
                   ((eq vector octets)
                    (flush)
                    (setf pending vector pending-start from pending-end to))
                   (t
                    ;; The scanner's own buffer, reused: feed it now.
                    (flush)
                    (funcall feed-normal vector from to)))))
             (on-event (event)
               (multiple-value-bind (action events) (classify-stream-event parser event)
                 (ecase action
                   (:pass (pass event))
                   (:consume (flush) (mapc handler events))
                   (:pass-then (pass event) (flush) (mapc handler events))
                   (:before (flush) (mapc handler events) (pass event))))))
      (ecma48:scan (terminal-stream-parser-scanner parser) octets #'on-event
                   :start start :end end)
      (flush))
    parser))

;;; The graphics-only interface the tests and older callers use.

(defun make-terminal-graphics-parser (&rest arguments)
  (apply #'make-terminal-stream-parser arguments))

(defun terminal-graphics-feed (parser octets feed-normal on-sixel on-kitty
                               &key (start 0) (end (length octets)) on-control)
  (terminal-stream-feed parser octets feed-normal
                        (lambda (event)
                          (case (first event)
                            (:sixel (funcall on-sixel (second event)))
                            (:kitty (funcall on-kitty (second event)))
                            (t (when on-control (funcall on-control event)))))
                        :start start :end end))
