(defpackage #:ecma48/tests
  (:use #:cl #:ecma48)
  (:export #:run-tests))

(in-package #:ecma48/tests)

(defvar *failures* 0)

(defmacro is (form)
  `(unless ,form
     (incf *failures*)
     (format t "~&FAIL: ~s~%" ',form)))

(defun octets (string)
  (map '(vector (unsigned-byte 8)) #'char-code string))

(defun scan-all (input &key (chunk-sizes nil) policy (max-string-length (* 1024 1024)))
  "Scan INPUT (a string of octets), split into chunks of CHUNK-SIZES (cycled),
and return copies of the events."
  (let* ((bytes (coerce (octets input) '(simple-array (unsigned-byte 8) (*))))
         (scanner (make-scanner :string-policy policy :max-string-length max-string-length))
         (events '()))
    (flet ((collect (event)
             (if (and events (member (event-kind event) '(:text :raw))
                      (eq (event-kind (first events)) (event-kind event)))
                 ;; Text (or raw octets) split by chunking is the same.
                 (setf (first events)
                       (let ((joined (concatenate '(vector (unsigned-byte 8))
                                                  (event-raw (first events)) (event-raw event)))
                             (copy (copy-event event)))
                         (setf (event-octets copy) (coerce joined '(simple-array (unsigned-byte 8) (*)))
                               (event-start copy) 0 (event-end copy) (length joined))
                         copy))
                 (push (copy-event event) events))))
      (if chunk-sizes
          (loop with start = 0
                for sizes = chunk-sizes then (or (rest sizes) chunk-sizes)
                while (< start (length bytes))
                do (let ((end (min (length bytes) (+ start (first sizes)))))
                     (scan scanner bytes #'collect :start start :end end)
                     (setf start end)))
          (scan scanner bytes #'collect))
      (scanner-flush scanner #'collect))
    (nreverse events)))

(defun describe-event (event)
  (list* (event-kind event)
         (map 'string #'code-char (event-raw event))
         (case (event-kind event)
           (:csi (list (code-char (event-final event)) (event-private event)
                       (event-parameters event -1) (event-valid-p event)))
           (:escape (list (code-char (event-final event)) (event-intermediate event)))
           ((:osc :dcs :apc :pm :sos)
            (list (map 'string #'code-char (event-data event)) (event-terminator event)))
           (:control (list (event-final event))))))

(defun kinds (input &rest arguments)
  (mapcar #'event-kind (apply #'scan-all input arguments)))

(defun esc (&rest parts)
  (apply #'concatenate 'string
         (mapcar (lambda (part) (if (characterp part) (string part) part))
                 (substitute (code-char 27) :esc parts))))

(defun test-basics ()
  (let ((events (mapcar #'describe-event
                        (scan-all (esc "ab" :esc "[1;;3:4m" (code-char 10) "c")))))
    (is (equal (first events) '(:text "ab")))
    (is (equal (third (second events)) #\m))
    (is (equal (fifth (second events)) '(1 -1 3 4)))
    (is (equal (third events) (list :control (string (code-char 10)) 10)))
    (is (equal (fourth events) '(:text "c"))))
  ;; Private markers, intermediates, sub-parameters.
  (let ((event (first (scan-all (esc :esc "[?2026$p")))))
    (is (eql (char-code #\?) (event-private event)))
    (is (eql (char-code #\$) (event-intermediate event)))
    (is (equal (event-parameters event) '(2026))))
  (let ((event (first (scan-all (esc :esc "[4:3m")))))
    (is (event-subparameter-p event 1))
    (is (not (event-subparameter-p event 0))))
  ;; Escape sequences with intermediates.
  (let ((event (first (scan-all (esc :esc "(B")))))
    (is (eq :escape (event-kind event)))
    (is (eql (char-code #\() (event-intermediate event)))
    (is (eql (char-code #\B) (event-final event))))
  ;; Invalid sequences are reported as such.
  (is (not (event-valid-p (first (scan-all (esc :esc "[1?2m"))))))
  (is (not (event-valid-p (first (scan-all (esc :esc "[1$2m")))))))

(defun test-strings ()
  (let ((osc (first (scan-all (esc :esc "]8;;http://x" (code-char 7))))))
    (is (eq :osc (event-kind osc)))
    (is (eq :bel (event-terminator osc)))
    (is (equal "8;;http://x" (map 'string #'code-char (event-data osc)))))
  (let ((apc (first (scan-all (esc :esc "_Ga=t;AAAA" :esc "\\")))))
    (is (eq :apc (event-kind apc)))
    (is (eq :st (event-terminator apc)))
    (is (equal "Ga=t;AAAA" (map 'string #'code-char (event-data apc)))))
  ;; BEL ends only OSC strings.
  (let ((apc (first (scan-all (esc :esc "_a" (code-char 7) "b" :esc "\\")))))
    (is (equal (format nil "a~cb" (code-char 7)) (map 'string #'code-char (event-data apc)))))
  ;; DCS headers are parsed like CSI.
  (let ((dcs (first (scan-all (esc :esc "P0;1q#0~" :esc "\\")))))
    (is (eq :dcs (event-kind dcs)))
    (is (eql (char-code #\q) (event-final dcs)))
    (is (equal '(0 1) (event-parameters dcs)))
    (is (equal "#0~" (map 'string #'code-char (event-data dcs)))))
  ;; ESC inside a string ends it and starts a sequence.
  (let ((events (scan-all (esc :esc "]2;t" :esc "[m"))))
    (is (equal '(:osc :csi) (mapcar #'event-kind events)))
    (is (eq :interrupted (event-terminator (first events))))
    (is (equal (esc :esc "[m") (map 'string #'code-char (event-raw (second events))))))
  ;; CAN aborts: the partial string is passed on raw.
  (is (equal '(:raw :control :text) (kinds (esc :esc "]2;ab" (code-char 24) "z"))))
  ;; Declined strings are passed on raw, as they come.
  (let ((events (scan-all (esc "x" :esc "P+q544e" :esc "\\y")
                          :policy (lambda (kind event first)
                                    (declare (ignore event first))
                                    (if (eq kind :dcs) :pass :collect)))))
    (is (equal (esc "x" :esc "P+q544e" :esc "\\y")
               (apply #'concatenate 'string
                      (mapcar (lambda (e) (map 'string #'code-char (event-raw e))) events))))
    (is (not (member :dcs (mapcar #'event-kind events)))))
  ;; Including a declined string's first octet.
  (let ((events (scan-all (esc :esc "_" (code-char #x90) "x" :esc "\\")
                          :policy (constantly :pass))))
    (is (equal (esc :esc "_" (code-char #x90) "x" :esc "\\")
               (apply #'concatenate 'string
                      (mapcar (lambda (e) (map 'string #'code-char (event-raw e))) events)))))
  ;; Overlong strings are passed on raw.
  (is (equal '(:raw)
             (remove :text (kinds (esc :esc "]2;" (make-string 50 :initial-element #\a)
                                       (code-char 7))
                                  :max-string-length 10)))))

(defun test-limits ()
  ;; An overlong CSI still ends at CAN or ESC, keeping the scanner in step.
  (let ((events (scan-all (esc :esc "[" (make-string 1100 :initial-element #\1)
                               (code-char 24) :esc "]52;c;aGk=" (code-char 7)))))
    (is (eq :osc (event-kind (car (last events))))))
  ;; A policy may lower the length limit of the strings it collects.
  (is (equal '(:raw)
             (kinds (esc :esc "]2;" (make-string 40 :initial-element #\a) (code-char 7))
                    :policy (lambda (kind event first)
                              (declare (ignore kind event first))
                              (values :collect 10))))))

(defun test-utf-8-and-c1 ()
  ;; 9B, 9D, 90 and 9F are UTF-8 continuation bytes, never C1 controls.
  (let ((text (map 'string #'code-char '(#xf0 #x9f #x98 #x80 #xc2 #x9b #xe2 #x90 #x9d))))
    (is (equal '(:text) (kinds text)))))

(defun test-c0-inside-sequences ()
  ;; Executed at once, before the sequence, which keeps its own octets.
  (let ((events (scan-all (esc :esc "[1" (code-char 10) "2m"))))
    (is (equal '(:control :csi) (mapcar #'event-kind events)))
    (is (equal (esc :esc "[12m") (map 'string #'code-char (event-raw (second events)))))
    (is (equal '(12) (event-parameters (second events))))))

(defun test-chunking ()
  ;; Any split gives the same events.
  (let* ((input (esc "hello" :esc "[38:2:1:2:3m" :esc "]52;c;aGk=" :esc "\\" :esc "P1$r0m"
                     :esc "\\" :esc "_Gi=1;QUJD" :esc "\\" :esc "7" (code-char 13) "world"
                     :esc "[?1049h" :esc "]0;t" (code-char 7) "é" :esc))
         (whole (mapcar #'describe-event (scan-all input))))
    (dolist (sizes '((1) (2) (3) (1 4) (7) (5 1 2)))
      (is (equal whole (mapcar #'describe-event (scan-all input :chunk-sizes sizes)))))))

(defun test-random-streams ()
  ;; Raw octets reproduce the input (no C0 inside sequences here), under any
  ;; chunking.
  (let ((state (sb-ext:seed-random-state 5))
        (alphabet (esc "ab09;:?$ []P_^X\\Gq" :esc :esc (code-char 7) (code-char 24)
                       (map 'string #'code-char '(#x9b #x9c #xc3)))))
    (dotimes (trial 2000)
      (let* ((input (coerce (loop repeat (random 80 state)
                                  collect (char alphabet (random (length alphabet) state)))
                            'string))
             (sizes (list (1+ (random 9 state)) (1+ (random 9 state))))
             (events (scan-all input :chunk-sizes sizes :max-string-length 16))
             (raw (apply #'concatenate 'string
                         (mapcar (lambda (e) (map 'string #'code-char (event-raw e)))
                                 events))))
        (is (equal (mapcar #'describe-event (scan-all input :max-string-length 16))
                   (mapcar #'describe-event events)))
        (unless (find-if (lambda (c) (member (char-code c) '(7 24))) input)
          (is (equal input raw)))
        ;; Again with some strings declined (passed on as they come).
        (let* ((policy (lambda (kind event first)
                         (declare (ignore event first))
                         (if (member kind '(:dcs :apc :pm)) :pass :collect)))
               (events (scan-all input :chunk-sizes sizes :max-string-length 16
                                       :policy policy))
               (raw (apply #'concatenate 'string
                           (mapcar (lambda (e) (map 'string #'code-char (event-raw e)))
                                   events))))
          (is (equal (mapcar #'describe-event
                             (scan-all input :max-string-length 16 :policy policy))
                     (mapcar #'describe-event events)))
          (unless (find-if (lambda (c) (member (char-code c) '(7 24))) input)
            (is (equal input raw))))))))

(defun test-text-scans ()
  ;; The vector scans agree with the byte-at-a-time one wherever the
  ;; control falls and however the run is aligned.
  (let ((*random-state* (sb-ext:seed-random-state 48)))
    (dotimes (trial 2000)
      (let* ((length (random 70))
             (octets (make-array length :element-type '(unsigned-byte 8)))
             (start (random (1+ length))))
        (dotimes (i length)
          (setf (aref octets i) (+ 32 (random 224))))
        (when (and (plusp length) (zerop (random 3)))
          (setf (aref octets (random length)) (if (zerop (random 2)) 127 (random 32))))
        (let ((expected (ecma48::scan-text-scalar octets start length)))
          #+sbcl (is (= expected (ecma48::scan-text-swar octets start length)))
          #+ecma48-neon (is (= expected (ecma48::scan-text-neon octets start length)))
          #+ecma48-sse2 (is (= expected (ecma48::scan-text-sse2 octets start length))))))))

(defun run-tests ()
  (setf *failures* 0)
  (test-text-scans)
  (test-basics)
  (test-strings)
  (test-utf-8-and-c1)
  (test-limits)
  (test-c0-inside-sequences)
  (test-chunking)
  (test-random-streams)
  (format t "~&ecma48: ~:[~d failure~:p~;all tests passed~]~%"
          (zerop *failures*) *failures*)
  (zerop *failures*))
