(uiop:define-package #:slt/conformance
  (:use #:cl #:slt)
  (:export #:define-vt-case
           #:*cases*
           #:run-case
           #:run-conformance
           #:print-report
           #:compare-engines
           #:conformance-main))

(in-package #:slt/conformance)

;;;; A case is a byte stream fed to a fresh engine plus a list of
;;;; expectations checked against the resulting grid:
;;;;
;;;;   (:screen "row0" "row1" ...)   every row, right-trimmed
;;;;   (:row N "text")               one row, right-trimmed
;;;;   (:cursor ROW COLUMN)
;;;;   (:cursor-visible BOOLEAN)
;;;;   (:fg ROW COLUMN COLOR)        COLOR is :default, (:palette N), (:rgb R G B)
;;;;   (:bg ROW COLUMN COLOR)
;;;;   (:attrs ROW COLUMN (KEYWORD ...))  exactly these style attributes
;;;;   (:has-attrs ROW COLUMN (KEYWORD ...))
;;;;   (:reply "string")             concatenated output-function data
;;;;   (:reply-prefix "string")
;;;;   (:title "string")
;;;;
;;;; Input is a list of parts: strings, :esc, :csi (ESC [), :osc (ESC ]),
;;;; :bel, :st (ESC \), (:bytes n ...) for raw octets, or (:resize ROWS
;;;; COLUMNS) to resize the engine at that point.  A part of
;;;; :split makes the runner feed everything before it separately, to
;;;; exercise state carried across reads.

(defvar *cases* '())

(defstruct vt-case
  name
  description
  (rows 5)
  (columns 10)
  input
  expectations
  ;; Engines known not to implement the behavior: reported as XFAIL.
  known-failures)

(defmacro define-vt-case (name (&key (rows 5) (columns 10) known-failures description)
                          input &body expectations)
  `(progn
     (setf *cases*
           (append (remove ',name *cases* :key #'vt-case-name)
                   (list (make-vt-case :name ',name
                                       :description ,description
                                       :rows ,rows
                                       :columns ,columns
                                       :input ',input
                                       :expectations ',expectations
                                       :known-failures ',known-failures))))
     ',name))

(defun part-octets (part)
  (flet ((str (s) (slt::string->octets s)))
    (etypecase part
      (string (str part))
      (character (str (string part)))
      ((eql :esc) (str (string (code-char 27))))
      ((eql :csi) (str (format nil "~c[" (code-char 27))))
      ((eql :osc) (str (format nil "~c]" (code-char 27))))
      ((eql :bel) (str (string (code-char 7))))
      ((eql :st) (str (format nil "~c\\" (code-char 27))))
      (cons (ecase (first part)
              (:bytes (coerce (rest part) 'slt::octets))
              (:repeat (let ((one (part-octets (third part))))
                         (apply #'concatenate 'slt::octets
                                (make-list (second part) :initial-element one)))))))))

(defun input-chunks (input)
  "Split INPUT into a list of steps: octet vectors to feed, separated at
:SPLIT markers, and (:RESIZE ROWS COLUMNS) steps."
  (let ((steps '()) (current '()))
    (flet ((flush ()
             (when current
               (push (apply #'concatenate 'slt::octets
                            (mapcar #'part-octets (reverse current)))
                     steps)
               (setf current '()))))
      (dolist (part input)
        (cond
          ((eq part :split) (flush))
          ((and (consp part) (eq (first part) :resize))
           (flush)
           (push part steps))
          (t (push part current))))
      (flush))
    (nreverse steps)))

(defun color-designator (color)
  (cond
    ((slt::color-default-p color) :default)
    ((slt::color-palette-p color) (list :palette (ldb (byte 8 0) color)))
    (t (let ((value (slt::color-value color)))
         (list :rgb (ldb (byte 8 16) value) (ldb (byte 8 8) value)
               (ldb (byte 8 0) value))))))

(defparameter *style-attributes*
  '(:bold :faint :italic :underline :double-underline :blink :inverse
    :invisible :strike))

(defun style-attributes (attrs)
  (remove-if-not (lambda (name) (member name *style-attributes*))
                 (attributes->list attrs)))

(defun same-set-p (a b)
  (and (subsetp a b) (subsetp b a)))

(defun check-expectation (expectation grid replies title)
  "Return NIL when EXPECTATION holds, otherwise a string describing why not."
  (flet ((index (row column) (grid-index grid row column))
         (complain (format-control &rest arguments)
           (apply #'format nil format-control arguments)))
    (destructuring-bind (kind &rest arguments) expectation
      (ecase kind
        (:screen
         (let ((actual (grid-text grid)))
           (unless (equal arguments actual)
             (complain "screen ~s, expected ~s" actual arguments))))
        (:row
         (destructuring-bind (row text) arguments
           (let ((actual (grid-row-text grid row)))
             (unless (string= text actual)
               (complain "row ~d ~s, expected ~s" row actual text)))))
        (:cursor
         (destructuring-bind (row column) arguments
           (unless (and (= row (grid-cursor-row grid))
                        (= column (grid-cursor-column grid)))
             (complain "cursor (~d ~d), expected (~d ~d)"
                       (grid-cursor-row grid) (grid-cursor-column grid) row column))))
        (:cursor-visible
         (unless (eq (first arguments) (grid-cursor-visible-p grid))
           (complain "cursor visible ~s, expected ~s"
                     (grid-cursor-visible-p grid) (first arguments))))
        ((:fg :bg)
         (destructuring-bind (row column expected) arguments
           (let ((actual (color-designator
                          (aref (if (eq kind :fg) (grid-fg grid) (grid-bg grid))
                                (index row column)))))
             (unless (equal expected actual)
               (complain "~(~a~) at (~d ~d) ~s, expected ~s"
                         kind row column actual expected)))))
        ((:attrs :has-attrs)
         (destructuring-bind (row column expected) arguments
           (let ((actual (style-attributes (aref (grid-attrs grid) (index row column)))))
             (unless (if (eq kind :attrs)
                         (same-set-p expected actual)
                         (subsetp expected actual))
               (complain "attrs at (~d ~d) ~s, expected ~:[~;at least ~]~s"
                         row column actual (eq kind :has-attrs) expected)))))
        (:reply
         (unless (string= (first arguments) replies)
           (complain "reply ~s, expected ~s" replies (first arguments))))
        (:reply-prefix
         (unless (eql 0 (search (first arguments) replies))
           (complain "reply ~s, expected prefix ~s" replies (first arguments))))
        (:title
         (unless (equal (first arguments) title)
           (complain "title ~s, expected ~s" title (first arguments))))))))

(defun run-case (case engine-name)
  "Run CASE on a fresh ENGINE-NAME engine.  Return :PASS, :XFAIL, :XPASS or
:FAIL, and a list of failure descriptions."
  (let* ((replies (make-string-output-stream))
         (title nil)
         (failures
           (handler-case
               (let ((engine (make-engine engine-name
                                          :rows (vt-case-rows case)
                                          :columns (vt-case-columns case)
                                          :output-function
                                          (lambda (string) (write-string string replies))
                                          :title-function
                                          (lambda (string) (setf title string)))))
                 (unwind-protect
                      (progn
                        (dolist (step (input-chunks (vt-case-input case)))
                          (if (consp step)
                              (engine-resize engine (second step) (third step))
                              (engine-feed engine step)))
                        (let ((grid (engine-update-grid engine (make-grid 1 1)))
                              (reply-string (get-output-stream-string replies)))
                          (loop for expectation in (vt-case-expectations case)
                                for failure = (check-expectation expectation grid
                                                                 reply-string title)
                                when failure collect failure)))
                   (engine-close engine)))
             (error (condition)
               (list (format nil "error: ~a" condition)))))
         (expected-failure-p (member engine-name (vt-case-known-failures case))))
    (values (cond
              ((and failures expected-failure-p) :xfail)
              (failures :fail)
              (expected-failure-p :xpass)
              (t :pass))
            failures)))

(defun run-conformance (&key (engines (available-engines))
                          (cases *cases*))
  "Run CASES on ENGINES.  Return a list of (CASE-NAME ENGINE STATUS FAILURES)."
  (loop for case in cases
        nconc (loop for engine in engines
                    collect (multiple-value-bind (status failures)
                                (run-case case engine)
                              (list (vt-case-name case) engine status failures)))))

(defun print-report (results &key (stream *standard-output*) (verbose t))
  (let* ((engines (remove-duplicates (mapcar #'second results) :from-end t))
         (cases (remove-duplicates (mapcar #'first results) :from-end t))
         (width (reduce #'max cases :key (lambda (c) (length (string c)))
                                    :initial-value 4)))
    (format stream "~&~vA" (+ width 2) "case")
    (dolist (engine engines)
      (format stream "~10@A" (string-downcase engine)))
    (terpri stream)
    (dolist (case cases)
      (format stream "~(~vA~)" (+ width 2) case)
      (dolist (engine engines)
        (let ((result (find-if (lambda (r) (and (eq (first r) case) (eq (second r) engine)))
                               results)))
          (format stream "~10@A" (string-downcase (third result)))))
      (terpri stream))
    (terpri stream)
    (dolist (engine engines)
      (let ((own (remove engine results :key #'second :test-not #'eq)))
        (format stream "~(~a~): ~d pass, ~d fail, ~d xfail, ~d xpass~%"
                engine
                (count :pass own :key #'third)
                (count :fail own :key #'third)
                (count :xfail own :key #'third)
                (count :xpass own :key #'third))))
    (when verbose
      (dolist (result results)
        (when (member (third result) '(:fail :xpass))
          (format stream "~&~(~a~) [~(~a~)] ~(~a~)~{~%    ~a~}~%"
                  (first result) (second result) (third result) (fourth result)))))
    results))

(defun compare-engines (input engine-a engine-b &key (rows 24) (columns 80))
  "Differential check: feed INPUT (octets or string) to two engines and
return a list of (ROW COLUMN WHAT A B) differences in text, colors,
attributes and cursor."
  (let* ((octets (if (stringp input) (slt::string->octets input) input))
         (grids (mapcar (lambda (name)
                          (let ((engine (make-engine name :rows rows :columns columns)))
                            (unwind-protect
                                 (progn (engine-feed engine octets)
                                        (engine-update-grid engine (make-grid 1 1)))
                              (engine-close engine))))
                        (list engine-a engine-b)))
         (a (first grids))
         (b (second grids))
         (differences '()))
    (dotimes (row rows)
      (dotimes (column columns)
        (let ((index (grid-index a row column)))
          (flet ((compare (what x y)
                   (unless (equal x y)
                     (push (list row column what x y) differences))))
            (compare :text (grid-cell-string a index) (grid-cell-string b index))
            (compare :fg (color-designator (aref (grid-fg a) index))
                     (color-designator (aref (grid-fg b) index)))
            (compare :bg (color-designator (aref (grid-bg a) index))
                     (color-designator (aref (grid-bg b) index)))
            (compare :attrs (style-attributes (aref (grid-attrs a) index))
                     (style-attributes (aref (grid-attrs b) index)))))))
    (unless (and (= (grid-cursor-row a) (grid-cursor-row b))
                 (= (grid-cursor-column a) (grid-cursor-column b)))
      (push (list (grid-cursor-row a) (grid-cursor-column a) :cursor
                  (list (grid-cursor-row a) (grid-cursor-column a))
                  (list (grid-cursor-row b) (grid-cursor-column b)))
            differences))
    (nreverse differences)))

(defun conformance-main (&key (engines (available-engines)))
  "Run the suite, print the report, and return true when nothing failed."
  (let ((results (run-conformance :engines engines)))
    (print-report results)
    (notany (lambda (r) (member (third r) '(:fail :xpass))) results)))
