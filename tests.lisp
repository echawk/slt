(uiop:define-package #:slt/tests
  (:use #:cl)
  (:import-from #:slt
                #:cell-char-string
                #:cell-view-bg
                #:cell-view-char
                #:cell-view-cursor-p
                #:cell-view-fg
                #:clear-dirty-rows
                #:color->hex
                #:decode-modifier-state
                #:default-font-family
                #:dirty-rows
                #:escape-tcl-text
                #:fit-grid-to-size
                #:handle-term-input
                #:keysym->terminal-key
                #:make-script-command
                #:mark-all-dirty
                #:normalize-event-state
                #:pick-font-family
                #:render-row-command-string
                #:resolve-cell-dimensions
                #:resolve-font-family
                #:render-text-command
                #:resize-term
                #:tcl-bool
                #:terminal-font-size
                #:term-cell-view
                #:translate-key-event)
  (:export #:run-tests))

(in-package #:slt/tests)

(defvar *tests* '())

(defmacro deftest (name (&rest args) &body body)
  `(progn
     (defun ,name ,args
       ,@body)
     (pushnew ',name *tests* :test #'eq)))

(defmacro is (condition &optional description)
  `(unless ,condition
     (error "Assertion failed~@[ (~a)~]: ~s" ,description ',condition)))

(defmacro is-equal (expected actual &optional description)
  `(let ((expected-value ,expected)
         (actual-value ,actual))
     (unless (equal expected-value actual-value)
       (error "Assertion failed~@[ (~a)~]: expected ~s, got ~s"
              ,description
              expected-value
              actual-value))))

(defun make-term (&key (rows 3) (columns 6))
  (make-instance '3bst:term :rows rows :columns columns))

(deftest color->hex-converts-basic-palette ()
  (is-equal "#CD0000" (color->hex 1)))

(deftest cell-char-string-hides-nulls-and-invisible-cells ()
  (is-equal " " (cell-char-string (code-char 0) '()))
  (is-equal " " (cell-char-string #\X '(:invisible)))
  (is-equal "A" (cell-char-string #\A '())))

(deftest decode-modifier-state-reads-known-bits ()
  (is-equal '(:shift :control :mod1) (decode-modifier-state #x0D)))

(deftest tcl-bool-converts-lisp-booleans-to-tcl-integers ()
  (is-equal 1 (tcl-bool t))
  (is-equal 0 (tcl-bool nil)))

(deftest normalize-event-state-accepts-strings-and-integers ()
  (is-equal 0 (normalize-event-state 0))
  (is-equal 13 (normalize-event-state "13"))
  (is-equal 0 (normalize-event-state ""))
  (is-equal 0 (normalize-event-state nil)))

(deftest default-font-family-picks-platform-appropriate-monospaced-fonts ()
  (is-equal "Menlo" (default-font-family :darwin))
  (is-equal "Menlo" (default-font-family :macosx))
  (is-equal "DejaVu Sans Mono" (default-font-family :linux))
  (is-equal "Monospace" (default-font-family :freebsd)))

(deftest pick-font-family-prefers-available-better-monospaced-families ()
  (is-equal "JetBrains Mono"
            (pick-font-family '("Courier" "JetBrains Mono" "Menlo")
                              '("SF Mono" "JetBrains Mono" "Menlo")
                              "Menlo"))
  (is-equal "Menlo"
            (pick-font-family '("Courier")
                              '("SF Mono" "JetBrains Mono")
                              "Menlo")))

(deftest resolve-font-family-respects-explicit-values-and-falls-back-cleanly ()
  (is-equal "Monaco"
            (resolve-font-family "Monaco" '("Menlo" "Monaco") :macosx))
  (is-equal "Menlo"
            (resolve-font-family nil '("Courier" "Menlo") :macosx))
  (is-equal "DejaVu Sans Mono"
            (resolve-font-family nil '("Courier") :linux)))

(deftest terminal-font-size-uses-pixel-sized-fonts-for-crisper-rendering ()
  (is-equal -15 (terminal-font-size 15))
  (is-equal -13 (terminal-font-size -13))
  (is-equal -15 (terminal-font-size nil)))

(deftest resolve-cell-dimensions-prefers-measured-font-metrics ()
  (multiple-value-bind (width height)
      (resolve-cell-dimensions 8 17)
    (is-equal 8 width)
    (is-equal 17 height))
  (multiple-value-bind (width height)
      (resolve-cell-dimensions 8 17 :cell-width 10 :cell-height 20)
    (is-equal 10 width)
    (is-equal 20 height)))

(deftest escape-tcl-text-protects-braces-backslashes-and-command-characters ()
  (is-equal "\\{\\}" (escape-tcl-text "{}"))
  (is-equal "\\\\"
            (escape-tcl-text "\\"))
  (is-equal "\\$\\[\\]\\\""
            (escape-tcl-text "$[]\"")))

(deftest render-text-command-escapes-canvas-cell-text-for-tcl ()
  (is-equal ".c itemconfigure 7 -fill {#FFFFFF} -font {mono} -text {\\{}"
            (render-text-command ".c" 7 "mono" "#FFFFFF" "{")))

(deftest fit-grid-to-size-computes-terminal-dimensions-from-pixels ()
  (multiple-value-bind (columns rows)
      (fit-grid-to-size 800 432 10 18)
    (is-equal 80 columns)
    (is-equal 24 rows))
  (multiple-value-bind (columns rows)
      (fit-grid-to-size 5 5 10 18)
    (is-equal 1 columns)
    (is-equal 1 rows)))

(deftest keysym->terminal-key-maps-special-and-function-keys ()
  (is-equal :prior (keysym->terminal-key "Prior"))
  (is-equal :kp_3 (keysym->terminal-key "KP_3"))
  (is-equal :f12 (keysym->terminal-key "F12"))
  (is-equal nil (keysym->terminal-key "NoSuchKey")))

(deftest translate-key-event-handles-characters-and-control-sequences ()
  (let ((term (make-term)))
    (is-equal "a" (translate-key-event term "a" "a" 0))
    (is-equal (string (code-char 3))
              (translate-key-event term "c" "c" #x04))
    (is-equal (string (code-char 24))
              (translate-key-event term "" "x" #x04))
    (is-equal (format nil "~Cz" (code-char 27))
              (translate-key-event term "z" "z" #x08))
    (is-equal (format nil "~C[A" (code-char 27))
              (translate-key-event term "" "Up" 0))
    (is-equal (format nil "~C[6~~" (code-char 27))
              (translate-key-event term "" "Next" 0))
    (is-equal (string #\Return)
              (translate-key-event term "" "Return" 0))))

(deftest dirty-row-helpers-track-render-work ()
  (let ((term (make-term :rows 2 :columns 4)))
    (3bst:handle-input "hi" :term term)
    (is-equal '(0) (dirty-rows term))
    (clear-dirty-rows term)
    (is-equal '() (dirty-rows term))
    (mark-all-dirty term)
    (is-equal '(0 1) (dirty-rows term))))

(deftest handle-term-input-marks-cursor-motion-dirty-even-without-glyph-changes ()
  (let ((term (make-term :rows 2 :columns 4)))
    (clear-dirty-rows term)
    (handle-term-input term (format nil "~C[C" (code-char 27)))
    (is-equal '(0) (dirty-rows term))
    (is (= 1 (3bst::x (3bst::cursor term))))))

(deftest resize-term-updates-dimensions-and-dirties-everything ()
  (let ((term (make-term :rows 2 :columns 3)))
    (resize-term term 4 5)
    (is (= 4 (3bst:rows term)))
    (is (= 5 (3bst:columns term)))
    (is-equal '(0 1 2 3) (dirty-rows term))))

(deftest term-cell-view-exposes-character-and-colors ()
  (let ((term (make-term :rows 1 :columns 4)))
    (3bst:handle-input (format nil "~c[31mR" (code-char 27)) :term term)
    (let ((cell (term-cell-view term 0 0)))
      (is-equal "R" (cell-view-char cell))
      (is-equal "#CD0000" (cell-view-fg cell))
      (is-equal "#000000" (cell-view-bg cell))
      (is (not (cell-view-cursor-p cell))))
    (is (cell-view-cursor-p (term-cell-view term 0 1)))))

(deftest make-script-command-supports-gnu-and-bsd-script ()
  (is-equal '("script" "-q" "/dev/null" "/bin/sh" "-i")
            (make-script-command "/bin/sh" :gnu-script-p nil))
  (is-equal '("script" "-qefc" "exec /bin/sh -i" "/dev/null")
            (make-script-command "/bin/sh" :gnu-script-p t)))

(defun run-tests ()
  (let ((failures '()))
    (dolist (test (reverse *tests*))
      (handler-case
          (funcall test)
        (error (condition)
          (push (cons test condition) failures))))
    (if failures
        (progn
          (dolist (failure (reverse failures))
            (format t "~&[FAIL] ~a~%  ~a~%" (car failure) (cdr failure)))
          (error "~d test(s) failed." (length failures)))
        (format t "~&~d tests passed.~%" (length *tests*)))))
