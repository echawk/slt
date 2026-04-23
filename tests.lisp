(uiop:define-package #:slt/tests
  (:use #:cl)
  (:import-from #:slt
                #:available-backends
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
                #:launch-shell-process
                #:make-backend
                #:mark-all-dirty
                #:native-pty-supported-p
                #:normalize-event-state
                #:pick-font-family
                #:process-alive-p
                #:read-available-output
                #:render-emulator
                #:render-row-command-string
                #:replace-environment-variable
                #:resolve-cell-dimensions
                #:resolve-font-family
                #:render-text-command
                #:resize-process-pty
                #:resize-term
                #:strip-unsupported-control-strings
                #:tcl-bool
                #:terminal-process-environment
                #:terminal-font-size
                #:terminate-process
                #:term-cell-view
                #:translate-key-event)
  (:export #:run-tests))

(in-package #:slt/tests)

(defvar *tests* '())

(defclass mock-backend (slt::terminal-backend) ())

(defstruct (mock-view
            (:constructor make-mock-view
                (&key (cell-width 10)
                      (cell-height 20)
                      (window-width 80)
                      (window-height 24))))
  cell-width
  cell-height
  window-width
  window-height
  resized-to
  rendered-rows
  last-title
  scheduled-callback
  cancelled-token
  resize-callback
  keypress-callback
  close-callback
  focused-p
  exit-requested-p)

(defmethod slt::backend-call-with-event-loop ((backend mock-backend) thunk)
  (declare (ignore backend))
  (funcall thunk))

(defmethod slt::backend-available-font-families ((backend mock-backend))
  (declare (ignore backend))
  '("Mock Mono"))

(defmethod slt::backend-create-view ((backend mock-backend)
                                     &key rows columns font-family font-size
                                       cell-width cell-height title)
  (declare (ignore backend rows columns font-family font-size title))
  (make-mock-view :cell-width (or cell-width 10)
                  :cell-height (or cell-height 20)))

(defmethod slt::backend-view-cell-width ((backend mock-backend) (view mock-view))
  (declare (ignore backend))
  (mock-view-cell-width view))

(defmethod slt::backend-view-cell-height ((backend mock-backend) (view mock-view))
  (declare (ignore backend))
  (mock-view-cell-height view))

(defmethod slt::backend-view-window-size ((backend mock-backend) (view mock-view))
  (declare (ignore backend))
  (values (mock-view-window-width view)
          (mock-view-window-height view)))

(defmethod slt::backend-resize-view ((backend mock-backend) (view mock-view) rows columns)
  (declare (ignore backend))
  (setf (mock-view-resized-to view) (list rows columns))
  view)

(defmethod slt::backend-render-rows ((backend mock-backend) (view mock-view) term rows)
  (declare (ignore backend term))
  (setf (mock-view-rendered-rows view) rows)
  view)

(defmethod slt::backend-set-title ((backend mock-backend) (view mock-view) title)
  (declare (ignore backend))
  (setf (mock-view-last-title view) title))

(defmethod slt::backend-schedule ((backend mock-backend) (view mock-view) delay-ms thunk)
  (declare (ignore backend delay-ms))
  (setf (mock-view-scheduled-callback view) thunk)
  :mock-token)

(defmethod slt::backend-cancel-scheduled ((backend mock-backend) (view mock-view) token)
  (declare (ignore backend))
  (setf (mock-view-cancelled-token view) token))

(defmethod slt::backend-bind-resize ((backend mock-backend) (view mock-view) callback)
  (declare (ignore backend))
  (setf (mock-view-resize-callback view) callback))

(defmethod slt::backend-bind-keypress ((backend mock-backend) (view mock-view) callback)
  (declare (ignore backend))
  (setf (mock-view-keypress-callback view) callback))

(defmethod slt::backend-on-close ((backend mock-backend) (view mock-view) callback)
  (declare (ignore backend))
  (setf (mock-view-close-callback view) callback))

(defmethod slt::backend-focus ((backend mock-backend) (view mock-view))
  (declare (ignore backend))
  (setf (mock-view-focused-p view) t))

(defmethod slt::backend-request-exit ((backend mock-backend) (view mock-view))
  (declare (ignore backend))
  (setf (mock-view-exit-requested-p view) t))

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

(defun make-mock-emulator (&key (rows 3)
                                (columns 6)
                                (window-width 80)
                                (window-height 60)
                                (cell-width 10)
                                (cell-height 20))
  (let* ((backend (make-instance 'mock-backend))
         (view (make-mock-view :cell-width cell-width
                               :cell-height cell-height
                               :window-width window-width
                               :window-height window-height))
         (term (make-term :rows rows :columns columns)))
    (values (slt::%make-terminal-emulator
             :term term
             :backend backend
             :view view
             :rows rows
             :columns columns
             :poll-interval 16)
            term
            view)))

(defun collect-process-output (process stream &key (timeout-seconds 2.0))
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-seconds internal-time-units-per-second)))))
    (with-output-to-string (out)
      (loop
        do (let ((chunk (read-available-output stream)))
             (when (plusp (length chunk))
               (write-string chunk out)))
        until (or (> (get-internal-real-time) deadline)
                  (not (process-alive-p process)))
        do (sleep 0.05))
      (write-string (read-available-output stream) out))))

(defun wait-for-process-output (stream &key (timeout-seconds 1.0))
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-seconds internal-time-units-per-second)))))
    (loop
      for chunk = (read-available-output stream)
      when (plusp (length chunk))
        return chunk
      when (> (get-internal-real-time) deadline)
        return ""
      do (sleep 0.05))))

(defun collect-output-until (process stream substring &key (timeout-seconds 2.0))
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-seconds internal-time-units-per-second))))
        (output ""))
    (loop
      do (let ((chunk (read-available-output stream)))
           (when (plusp (length chunk))
             (setf output (concatenate 'string output chunk))))
      when (search substring output)
        return output
      when (or (> (get-internal-real-time) deadline)
               (not (process-alive-p process)))
        return output
      do (sleep 0.05))))

(defun make-temp-directory ()
  (let ((directory (merge-pathnames
                    (format nil "slt-tests-~d-~d/"
                            (get-universal-time)
                            (random 1000000))
                    (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames "keep" directory))
    directory))

(defun write-test-file (pathname &optional (contents ""))
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream))
  pathname)

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

(deftest replace-environment-variable-updates-existing-values-cleanly ()
  (is-equal '("TERM=xterm-256color" "HOME=/tmp")
            (replace-environment-variable '("TERM=dumb" "HOME=/tmp")
                                          "TERM"
                                          "xterm-256color"))
  (is-equal '("TERM=xterm-256color" "HOME=/tmp")
            (replace-environment-variable '("HOME=/tmp")
                                          "TERM"
                                          "xterm-256color")))

(deftest terminal-process-environment-forces-a-terminal-type ()
  (is-equal '("COLUMNS=80" "LINES=24" "TERM=xterm-256color" "HOME=/tmp")
            (terminal-process-environment :term-name "xterm-256color"
                                          :rows 24
                                          :columns 80
                                          :base-environment '("HOME=/tmp")))
  (is-equal '("HOME=/tmp")
            (terminal-process-environment :term-name nil
                                          :base-environment '("HOME=/tmp"))))

(deftest backend-registry-exposes-the-ltk-and-sdl2-backends ()
  (is (member :ltk (available-backends)))
  (is (member :sdl2 (available-backends)))
  (is (typep (make-backend :ltk) 'slt::terminal-backend))
  (is (typep (make-backend :sdl2) 'slt::terminal-backend))
  (is (typep (make-backend "sdl2") 'slt::terminal-backend)))

(deftest sdl-hex-color-rgba-parses-rgb-strings ()
  (multiple-value-bind (red green blue alpha)
      (slt::hex-color-rgba "#A1B2C3")
    (is-equal '(161 178 195 255)
              (list red green blue alpha))))

(deftest sdl-modifiers->state-translates-common-sdl-keywords ()
  (is-equal #x4D
            (slt::sdl-modifiers->state '(:lshift :rctrl :alt :lgui))))

(deftest normalize-sdl-key-name-maps-special-keys-and-letter-case ()
  (is-equal "BackSpace"
            (slt::normalize-sdl-key-name "Backspace" :shiftp nil))
  (is-equal "space"
            (slt::normalize-sdl-key-name "Space" :shiftp nil))
  (is-equal "a"
            (slt::normalize-sdl-key-name "A" :shiftp nil))
  (is-equal "A"
            (slt::normalize-sdl-key-name "A" :shiftp t)))

(deftest sdl-backend-resolves-font-size-to-positive-point-size ()
  (let ((backend (make-backend :sdl2)))
    (is-equal 15 (slt::backend-resolve-font-size backend nil))
    (is-equal 13 (slt::backend-resolve-font-size backend -13))
    (is-equal 11 (slt::backend-resolve-font-size backend 11))))

(deftest sdl-backend-main-thread-policy-matches-sbcl-on-macos ()
  (is (slt::sdl-backend-use-main-thread-p "SBCL" :darwin))
  (is (slt::sdl-backend-use-main-thread-p "SBCL" :macosx))
  (is (not (slt::sdl-backend-use-main-thread-p "CCL" :darwin)))
  (is (not (slt::sdl-backend-use-main-thread-p "SBCL" :linux))))

(deftest dispatch-sdl-timer-runs-callback-once-and-clears-it ()
  (let* ((backend (make-instance 'slt::sdl2-backend))
         (calls 0))
    (setf (gethash 7 (slt::sdl2-backend-timers backend))
          (lambda ()
            (incf calls)))
    (slt::dispatch-sdl-timer backend 7)
    (slt::dispatch-sdl-timer backend 7)
    (is-equal 1 calls)
    (is (null (gethash 7 (slt::sdl2-backend-timers backend))))))

(deftest find-font-file-prefers-the-closest-family-match ()
  (let* ((root (make-temp-directory))
         (nested (merge-pathnames "nested/" root))
         (fallback-font (merge-pathnames "SomeMono.ttf" root))
         (target-font (merge-pathnames "JetBrainsMono-Regular.ttf" nested)))
    (unwind-protect
         (progn
           (write-test-file fallback-font)
           (write-test-file target-font)
           (is-equal (truename target-font)
                     (truename (slt::find-font-file "JetBrains Mono"
                                                   :search-roots (list root)))))
      (ignore-errors
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))

(deftest resolve-sdl-font-path-uses-explicit-paths-and-search-roots ()
  (let* ((root (make-temp-directory))
         (fonts (merge-pathnames "fonts/" root))
         (target-font (merge-pathnames "IosevkaTerm-Regular.otf" fonts))
         (target-path nil))
    (unwind-protect
         (progn
           (write-test-file target-font)
           (setf target-path (namestring (truename target-font)))
           (is-equal (truename target-font)
                     (truename (slt::resolve-sdl-font-path target-path
                                                           :search-roots (list root))))
           (is-equal (truename target-font)
                     (truename (slt::resolve-sdl-font-path "Iosevka Term"
                                                           :search-roots (list root)))))
      (ignore-errors
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))

(deftest strip-unsupported-control-strings-removes-dcs-and-keeps-text ()
  (let* ((escape (string (code-char 27)))
         (dcs-sequence (concatenate 'string escape "Pzz" escape "\\"))
         (c1-dcs-sequence (concatenate 'string
                                       (string (code-char #x90))
                                       "ignored"
                                       (string (code-char #x9C)))))
    (is-equal "ab"
              (strip-unsupported-control-strings
               (concatenate 'string "a" dcs-sequence "b")))
    (is-equal "xy"
              (strip-unsupported-control-strings
               (concatenate 'string "x" c1-dcs-sequence "y")))))

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

(deftest handle-term-input-ignores-unsupported-dcs-sequences ()
  (let* ((term (make-term :rows 1 :columns 4))
         (escape (string (code-char 27)))
         (input (concatenate 'string "A" escape "Pzz" escape "\\" "B")))
    (handle-term-input term input)
    (is-equal "A" (cell-view-char (term-cell-view term 0 0)))
    (is-equal "B" (cell-view-char (term-cell-view term 0 1)))))

(deftest render-emulator-delegates-rendering-through-the-backend-interface ()
  (multiple-value-bind (emulator term view)
      (make-mock-emulator :rows 2 :columns 4)
    (3bst:handle-input "hi" :term term)
    (render-emulator emulator :force nil)
    (is-equal '(0) (mock-view-rendered-rows view))
    (is-equal '() (dirty-rows term))))

(deftest apply-resize-uses-backend-window-metrics-and-resizes-the-view ()
  (multiple-value-bind (emulator term view)
      (make-mock-emulator :rows 1
                          :columns 1
                          :window-width 95
                          :window-height 41
                          :cell-width 10
                          :cell-height 20)
    (slt::apply-resize emulator)
    (is (= 2 (3bst:rows term)))
    (is (= 9 (3bst:columns term)))
    (is-equal '(2 9) (mock-view-resized-to view))
    (is-equal '() (dirty-rows term))))

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

(deftest native-pty-transport-runs-shell-commands-through-a-single-stream ()
  (is (native-pty-supported-p))
  (multiple-value-bind (process input output)
      (launch-shell-process "/bin/sh"
                            :term-name "xterm-256color"
                            :rows 24
                            :columns 80)
    (unwind-protect
         (progn
           (is (eq input output))
           (wait-for-process-output output)
           (write-line "printf \"slt-pty-ok\\n\"" input)
           (finish-output input)
           (is (search "slt-pty-ok"
                       (collect-output-until process output "slt-pty-ok")))
           (write-line "exit" input)
           (finish-output input)
           (collect-process-output process output))
      (ignore-errors
        (when (process-alive-p process)
          (terminate-process process)))
      (ignore-errors
        (when input
          (close input))))))

(deftest native-pty-transport-provides-a-controlling-tty ()
  (is (native-pty-supported-p))
  (multiple-value-bind (process input output)
      (launch-shell-process "/bin/sh"
                            :term-name "xterm-256color"
                            :rows 24
                            :columns 80)
    (unwind-protect
         (progn
           (wait-for-process-output output)
           (write-line ": </dev/tty && printf \"tty-open-ok\\n\"" input)
           (finish-output input)
           (is (search "tty-open-ok"
                       (collect-output-until process output "tty-open-ok"
                                             :timeout-seconds 3.0)))
           (write-line "exit" input)
           (finish-output input)
           (collect-process-output process output))
      (ignore-errors
        (when (process-alive-p process)
          (terminate-process process)))
      (ignore-errors
        (when input
          (close input))))))

(deftest resize-process-pty-updates-the-child-terminal-size ()
  (is (native-pty-supported-p))
  (multiple-value-bind (process input output)
      (launch-shell-process "/bin/sh"
                            :term-name "xterm-256color"
                            :rows 24
                            :columns 80)
    (unwind-protect
         (progn
           (wait-for-process-output output)
           (resize-process-pty process 12 34)
           (write-line "stty size" input)
           (finish-output input)
           (is (search "12 34"
                       (collect-output-until process output "12 34"
                                             :timeout-seconds 3.0)))
           (write-line "exit" input)
           (finish-output input)
           (collect-process-output process output))
      (ignore-errors
        (when (process-alive-p process)
          (terminate-process process)))
      (ignore-errors
        (when input
          (close input))))))

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
