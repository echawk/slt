(uiop:define-package #:slt/tests
  (:use #:cl #:slt)
  (:import-from #:slt
                #:escape-tcl-text
                #:normalize-event-state
                #:render-text-command
                #:replace-environment-variable
                #:strip-unsupported-control-strings
                #:tcl-bool
                #:terminal-process-environment)
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

(defmethod slt::backend-render-grid ((backend mock-backend) (view mock-view) grid)
  (declare (ignore backend))
  (setf (mock-view-rendered-rows view) (grid-dirty-rows grid))
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

(defun make-test-engine (&key (rows 3) (columns 6) (engine :3bst))
  (make-engine engine :rows rows :columns columns))

(defun feed (engine &rest parts)
  "Feed PARTS to ENGINE; characters and strings are concatenated, with
:ESC standing for the escape character."
  (engine-feed-string engine
                      (apply #'concatenate 'string
                             (mapcar (lambda (part)
                                       (case part
                                         (:esc (string (code-char 27)))
                                         (t (string part))))
                                     parts)))
  engine)

(defun engine-grid (engine)
  (engine-update-grid engine (make-grid 1 1)))

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
         (engine (make-test-engine :rows rows :columns columns))
         (grid (make-grid rows columns)))
    (engine-update-grid engine grid)
    (grid-clear-dirty grid)
    (values (slt::%make-terminal-emulator
             :engine engine
             :grid grid
             :backend backend
             :view view
             :rows rows
             :columns columns
             :poll-interval 16)
            engine
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

(defvar *accumulated-output* (make-hash-table :test #'eq))

(defun get-output-stream-string* (stream)
  "Like GET-OUTPUT-STREAM-STRING, but cumulative across calls."
  (setf (gethash stream *accumulated-output*)
        (concatenate 'string (gethash stream *accumulated-output* "")
                     (get-output-stream-string stream))))

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

(deftest decode-modifier-state-reads-known-bits ()
  (is-equal '(:shift :control :meta) (decode-modifier-state #x0D))
  (is-equal '(:super) (decode-modifier-state #x40)))

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

(deftest hex->rgb-and-rgb->hex-round-trip ()
  (is-equal #xA1B2C3 (hex->rgb "#A1B2C3"))
  (is-equal "#A1B2C3" (rgb->hex #xA1B2C3)))

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

(deftest strip-unsupported-control-strings-keeps-plain-introducer-letters ()
  ;; Regression: a bare P, _ or ^ used to start a control string and
  ;; swallow the rest of the chunk.
  (is-equal "echo Python ^_^" (strip-unsupported-control-strings "echo Python ^_^")))

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
  (is-equal :page-up (keysym->terminal-key "Prior"))
  (is-equal #\3 (keysym->terminal-key "KP_3"))
  (is-equal :enter (keysym->terminal-key "KP_Enter"))
  (is-equal :f12 (keysym->terminal-key "F12"))
  (is-equal nil (keysym->terminal-key "NoSuchKey")))

(deftest translate-key-event-handles-characters-and-control-sequences ()
  (let ((engine (make-test-engine))
        (esc (code-char 27)))
    (is-equal "a" (translate-key-event engine "a" "a" 0))
    (is-equal (string (code-char 3)) (translate-key-event engine "c" "c" #x04))
    (is-equal (string (code-char 24)) (translate-key-event engine "" "x" #x04))
    (is-equal (format nil "~Cz" esc) (translate-key-event engine "z" "z" #x08))
    (is-equal (format nil "~C[A" esc) (translate-key-event engine "" "Up" 0))
    (is-equal (format nil "~C[1;5A" esc) (translate-key-event engine "" "Up" #x04))
    (is-equal (format nil "~C[6~~" esc) (translate-key-event engine "" "Next" 0))
    (is-equal (format nil "~COP" esc) (translate-key-event engine "" "F1" 0))
    (is-equal (format nil "~C[15;2~~" esc) (translate-key-event engine "" "F5" #x01))
    (is-equal (format nil "~C[Z" esc) (translate-key-event engine "" "Tab" #x01))
    (is-equal (string #\Return) (translate-key-event engine "" "Return" 0))))

(deftest application-cursor-mode-changes-arrow-encoding ()
  (let ((engine (make-test-engine)))
    (feed engine :esc "[?1h")
    (is-equal (format nil "~COA" (code-char 27))
              (engine-encode-key engine :up '()))))

(deftest utf8-decoder-handles-sequences-split-across-reads ()
  (let ((decoder (slt::make-utf8-decoder))
        (octets (slt::string->octets "a→b")))
    (is-equal 5 (length octets))
    (is-equal "a" (slt::utf8-decode decoder octets :end 2))
    (is-equal "→b" (slt::utf8-decode decoder octets :start 2))
    (is-equal (string (code-char #xFFFD))
              (slt::utf8-decode decoder (coerce #(#xFF) 'slt::octets)))))

(deftest hooks-run-in-order-and-can-be-removed ()
  (let ((slt::*bell-hook* '())
        (calls '()))
    (flet ((first-hook (x) (push (list :first x) calls))
           (second-hook (x) (push (list :second x) calls)))
      (add-hook 'slt::*bell-hook* #'second-hook)
      (add-hook 'slt::*bell-hook* #'first-hook)
      (run-hook 'slt::*bell-hook* 1)
      (is-equal '((:second 1) (:first 1)) calls)
      (remove-hook 'slt::*bell-hook* #'first-hook)
      (is-equal 1 (length slt::*bell-hook*)))))

(deftest engine-registry-exposes-3bst ()
  (is (member :3bst (available-engines)))
  (is (typep (make-engine :3bst :rows 2 :columns 2) 'engine)))

(deftest engine-update-grid-copies-text-colors-and-cursor ()
  (let* ((engine (feed (make-test-engine :rows 2 :columns 6)
                       "a" :esc "[31mR" :esc "[1;7mX"))
         (grid (engine-grid engine)))
    (is-equal '("aRX" "") (grid-text grid))
    (is-equal +color-default+ (aref (grid-fg grid) 0))
    (is-equal (palette-color 1) (aref (grid-fg grid) 1))
    (is-equal '(:bold :inverse) (attributes->list (aref (grid-attrs grid) 2)))
    (is-equal 0 (grid-cursor-row grid))
    (is-equal 3 (grid-cursor-column grid))))

(deftest engine-update-grid-tracks-dirty-rows-and-cursor-motion ()
  (let* ((engine (make-test-engine :rows 3 :columns 4))
         (grid (engine-grid engine)))
    (grid-clear-dirty grid)
    (feed engine :esc "[3;1H")
    (engine-update-grid engine grid)
    (is-equal '(0 2) (grid-dirty-rows grid))
    (grid-clear-dirty grid)
    (feed engine "x")
    (engine-update-grid engine grid)
    (is-equal '(2) (grid-dirty-rows grid))))

(deftest engine-ignores-unsupported-dcs-sequences ()
  (let ((grid (engine-grid (feed (make-test-engine :rows 1 :columns 4)
                                 "A" :esc "Pzz" :esc "\\" "B"))))
    (is-equal "AB" (grid-row-text grid 0))))

(deftest engine-replies-to-device-attributes-through-output-function ()
  (let* ((replies '())
         (engine (make-engine :3bst :rows 2 :columns 4
                                    :output-function (lambda (s) (push s replies)))))
    (feed engine :esc "[c")
    (is replies)
    (is (char= (code-char 27) (char (first replies) 0)))))

(deftest engine-reports-title-changes ()
  (let* ((title nil)
         (engine (make-engine :3bst :rows 2 :columns 4
                                    :title-function (lambda (s) (setf title s)))))
    (feed engine :esc "]2;hello" (code-char 7))
    (is-equal "hello" title)))

(deftest cell-rgb-resolves-palette-inverse-and-cursor ()
  (let* ((palette (make-palette :ansi :xterm :foreground #xAAAAAA :background #x111111
                                :cursor #x00FF00))
         (grid (engine-grid (feed (make-test-engine :rows 1 :columns 4)
                                  :esc "[31mR" :esc "[0;7mI" :esc "[0m"))))
    (is-equal (list #xCD0000 #x111111) (multiple-value-list (cell-rgb grid 0 palette)))
    (is-equal (list #x111111 #xAAAAAA) (multiple-value-list (cell-rgb grid 1 palette)))
    (is-equal #x00FF00 (nth-value 1 (cell-rgb grid 2 palette)))))

(deftest default-palette-is-the-soft-theme ()
  (is-equal #x1C1F24 (palette-background (make-palette)))
  (is-equal #xD77A7A (aref (palette-colors (make-palette)) 1))
  (is-equal #xCD0000 (aref (palette-colors (make-palette :ansi :xterm)) 1))
  (is-equal #x123456 (aref (palette-colors (make-palette :overrides '((1 . #x123456)))) 1)))

(deftest cell-rgb-dims-faint-text-toward-the-background ()
  (let ((palette (make-palette :foreground #xFFFFFF :background #x000000))
        (grid (engine-grid (feed (make-test-engine :rows 1 :columns 2) :esc "[2mf"))))
    (is-equal #x7F7F7F (nth-value 0 (cell-rgb grid 0 palette)))))

(deftest make-engine-loads-engine-systems-on-demand ()
  (is (null (slt::load-engine-system :no-such-engine)))
  (handler-case (progn (make-engine :no-such-engine) (is nil "unknown engine accepted"))
    (error () t)))

(deftest grid-handles-wide-characters ()
  (let ((grid (engine-grid (feed (make-test-engine :rows 1 :columns 4)
                                 (string (code-char #x4E2D)) "x"))))
    (is-equal (format nil "~Cx" (code-char #x4E2D)) (grid-row-text grid 0))
    (is (logtest slt::+attr-wide+ (aref (grid-attrs grid) 0)))
    (is (logtest slt::+attr-wide-spacer+ (aref (grid-attrs grid) 1)))))

(deftest render-emulator-delegates-rendering-through-the-backend-interface ()
  (multiple-value-bind (emulator engine view)
      (make-mock-emulator :rows 2 :columns 4)
    (feed engine "hi")
    (render-emulator emulator)
    (is-equal '(0) (mock-view-rendered-rows view))
    (is-equal '() (grid-dirty-rows (slt::terminal-emulator-grid emulator)))))

(deftest apply-resize-uses-backend-window-metrics-and-resizes-the-view ()
  (multiple-value-bind (emulator engine view)
      (make-mock-emulator :rows 1
                          :columns 1
                          :window-width 95
                          :window-height 41
                          :cell-width 10
                          :cell-height 20)
    (slt::apply-resize emulator)
    (is (= 2 (engine-rows engine)))
    (is (= 9 (engine-columns engine)))
    (is-equal '(2 9) (mock-view-resized-to view))
    (is-equal 2 (grid-rows (slt::terminal-emulator-grid emulator)))
    (is-equal '() (grid-dirty-rows (slt::terminal-emulator-grid emulator)))))

(deftest key-hook-can-consume-keys ()
  (multiple-value-bind (emulator engine) (make-mock-emulator :rows 1 :columns 8)
    (let ((slt::*key-hook* (list (lambda (emulator key modifiers)
                                   (declare (ignore emulator modifiers))
                                   (eql key #\q)))))
      (slt::handle-keypress emulator "q" "q" 0)
      (slt::handle-keypress emulator "w" "w" 0)
      (is-equal "w" (grid-row-text (engine-grid engine) 0)))))

(deftest octet-transport-round-trips-through-a-pty ()
  (multiple-value-bind (process) (launch-shell-process "/bin/sh" :rows 24 :columns 80)
    (unwind-protect
         (let ((fd (slt::set-fd-nonblocking (slt::process-fd process)))
               (buffer (make-array 4096 :element-type '(unsigned-byte 8)))
               (output (make-string-output-stream)))
           (write-octets fd (slt::string->octets (format nil "printf 'oct%sok\\n' -~%")))
           (loop with deadline = (+ (get-internal-real-time)
                                    (* 3 internal-time-units-per-second))
                 for count = (read-available-octets fd buffer)
                 do (when (and (integerp count) (plusp count))
                      (write-string (babel:octets-to-string buffer :end count) output))
                 until (or (search "oct-ok" (get-output-stream-string* output))
                           (eq count :eof)
                           (> (get-internal-real-time) deadline))
                 do (sleep 0.02))
           (is (search "oct-ok" (get-output-stream-string* output))))
      (ignore-errors (terminate-process process)))))

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

(deftest parse-cli-arguments-translates-runtime-flags-into-launch-keywords ()
  (let* ((command (parse-cli-arguments '("--backend" "sdl2"
                                         "--engine" "3bst"
                                         "--rows" "30"
                                         "--columns" "120"
                                         "--font-family" "JetBrains Mono"
                                         "--font-size" "18"
                                         "--cell-width" "11"
                                         "--cell-height" "22"
                                         "--shell" "/bin/zsh"
                                         "--term" "xterm-direct"
                                         "--poll-interval" "25"
                                         "--title" "scratch"
                                         "--no-config")))
         (arguments (command-launch-arguments command)))
    (is-equal '(:backend "sdl2"
                :engine "3bst"
                :rows 30
                :columns 120
                :font-family "JetBrains Mono"
                :font-size 18
                :cell-width 11
                :cell-height 22
                :shell "/bin/zsh"
                :term-name "xterm-direct"
                :poll-interval 25
                :title "scratch"
                :load-config nil)
              arguments)))

(deftest parse-cli-arguments-keeps-bare-backend-compatibility ()
  (let* ((command (parse-cli-arguments '("ltk")))
         (arguments (command-launch-arguments command)))
    (is-equal '(:backend "ltk")
              arguments)))

(deftest environment-cli-arguments-splits-wrapper-encoded-arguments ()
  (is-equal '("--backend" "sdl2" "--title" "scratch pad")
            (slt::environment-cli-arguments
             (format nil "--backend~Csdl2~C--title~Cscratch pad"
                     (code-char 31)
                     (code-char 31)
                     (code-char 31)))))

(deftest handle-cli-command-can-list-backends-without-launching-the-gui ()
  (let* ((command (parse-cli-arguments '("--list-backends")))
         (output (with-output-to-string (stream)
                  (slt::handle-cli-command command :stream stream))))
    (is (search "ltk" output))
    (is (search "sdl2" output))))

(deftest default-executable-name-matches-platform-conventions ()
  (is-equal "slt" (default-executable-name :darwin))
  (is-equal "slt.exe" (default-executable-name :windows)))

(deftest normalize-executable-output-uses-build-directory-by-default-and-respects-directories ()
  (let* ((root #P"/tmp/slt-root/")
         (default-output (default-executable-output root :linux))
         (normalized-default (normalize-executable-output nil
                                                          :root root
                                                          :operating-system :linux))
         (directory-output (normalize-executable-output #P"artifacts/"
                                                        :root root
                                                        :operating-system :linux))
         (relative-output (normalize-executable-output "dist/custom-slt"
                                                       :root root
                                                       :operating-system :linux)))
    (is-equal (namestring default-output)
              (namestring normalized-default))
    (is-equal "/tmp/slt-root/artifacts/slt"
              (namestring directory-output))
    (is-equal "/tmp/slt-root/dist/custom-slt"
              (namestring relative-output))))

(deftest conformance-suite-passes-on-3bst ()
  (let ((results (slt/conformance:run-conformance :engines '(:3bst))))
    (dolist (result results)
      (is (member (third result) '(:pass :xfail))
          (format nil "~(~a~) ~(~a~): ~{~a~^; ~}"
                  (first result) (third result) (fourth result))))))

(defun fake-rasterizer (string style pixels stride x y width height)
  "Mark each rasterized slot with the first character's code in its corner."
  (declare (ignore style width height))
  (setf (aref pixels (+ x (* stride y))) (mod (char-code (char string 0)) 256)))

(deftest pack-grid-assigns-atlas-slots-and-colors ()
  (let* ((grid (engine-grid (feed (make-test-engine :rows 1 :columns 4)
                                  :esc "[31;4mA" :esc "[0mA" (string (code-char #x4E2D)))))
         (atlas (slt::make-glyph-atlas 2 2 #'fake-rasterizer :width 16 :height 16))
         (instances (slt::pack-grid grid atlas (make-palette :ansi :xterm) nil)))
    ;; A (red, underlined), A again (same glyph, same slot), then a wide glyph.
    (is-equal (list #xCD0000 1 slt::+gpu-flag-underline+)
              (list (aref instances 0) (aref instances 2) (aref instances 3)))
    (is-equal 1 (aref instances 6) "repeated glyph reuses its slot")
    (is-equal 2 (aref instances 10) "wide glyph takes two fresh slots")
    (is-equal 3 (aref instances 14) "spacer draws the wide glyph's right half")
    (is-equal (char-code #\A) (aref (slt::glyph-atlas-pixels atlas) 2))))

(deftest pack-grid-recovers-when-the-atlas-fills ()
  (let* ((grid (engine-grid (feed (make-test-engine :rows 1 :columns 8) "abcdefgh")))
         ;; Room for three glyphs besides the blank slot.
         (atlas (slt::make-glyph-atlas 4 4 #'fake-rasterizer :width 8 :height 8))
         (generation (slt::glyph-atlas-generation atlas))
         (instances (slt::pack-grid grid atlas (make-palette) nil)))
    (is (> (slt::glyph-atlas-generation atlas) generation))
    (is (> (slt::glyph-atlas-height atlas) 8) "atlas grew to fit the screen")
    (is (loop for cell below 8 always (plusp (aref instances (+ 2 (* 4 cell)))))
        "every glyph has a slot")))

(defun wait-until (predicate &key (timeout 5))
  (loop with deadline = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
        until (funcall predicate)
        when (> (get-internal-real-time) deadline)
          return nil
        do (sleep 0.02)
        finally (return t)))

(defun headless-screen-text (view)
  (let ((grid (headless-view-grid view)))
    (if grid (format nil "~{~a~%~}" (grid-text grid)) "")))

(deftest headless-backend-runs-a-shell-end-to-end ()
  (let* ((backend (make-instance 'headless-backend))
         (thread (bt:make-thread
                  (lambda ()
                    (launch-terminal :backend backend :shell "/bin/sh"
                                     :rows 6 :columns 40 :load-config nil))
                  :name "slt headless test")))
    (unwind-protect
         (progn
           (is (wait-until (lambda () (slt::headless-backend-view backend))))
           (let ((view (slt::headless-backend-view backend)))
             (is (wait-until (lambda () (search "$" (headless-screen-text view)))))
             (loop for char across "echo hel"
                   do (headless-key view (string char) (string char) 0))
             (headless-paste view "lo-headless")
             (headless-key view "" "Return" 0)
             (is (wait-until (lambda ()
                               (search (format nil "~%hello-headless")
                                       (headless-screen-text view))))
                 "typed and pasted input reaches the shell")
             (headless-resize-cells view 10 50)
             (is (wait-until (lambda ()
                               (let ((grid (headless-view-grid view)))
                                 (and grid (= 10 (grid-rows grid)) (= 50 (grid-columns grid))))))
                 "window resize reaches the grid")
             (headless-close view)
             (is (wait-until (lambda () (not (bt:thread-alive-p thread)))))))
      (when (bt:thread-alive-p thread)
        (setf (slt::headless-backend-quit-p backend) t)
        (bt:join-thread thread)))))

(deftest headless-scrollback-with-ghostty ()
  (when (slt::engine-available-p :ghostty)
    (let* ((backend (make-instance 'headless-backend))
           (thread (bt:make-thread
                    (lambda ()
                      (launch-terminal :backend backend :engine :ghostty :shell "/bin/sh"
                                       :rows 6 :columns 30 :load-config nil))
                    :name "slt scrollback test")))
      (unwind-protect
           (progn
             (is (wait-until (lambda () (slt::headless-backend-view backend))))
             (let ((view (slt::headless-backend-view backend)))
               (is (wait-until (lambda () (search "$" (headless-screen-text view)))))
               (headless-paste view (format nil "for i in $(seq 1 60); do echo line$i; done~%"))
               (is (wait-until (lambda () (search "line60" (headless-screen-text view))))
                   "output reaches the screen")
               (headless-scroll view -20)
               (is (wait-until (lambda () (and (search "line4" (headless-screen-text view))
                                               (not (search "line60" (headless-screen-text view))))))
                   "scrolling back shows earlier lines")
               (headless-key view "" "End" slt::+shift-mask+)
               (is (wait-until (lambda () (search "line60" (headless-screen-text view))))
                   "Shift+End returns to the live screen")
               (headless-close view)
               (is (wait-until (lambda () (not (bt:thread-alive-p thread)))))))
        (when (bt:thread-alive-p thread)
          (setf (slt::headless-backend-quit-p backend) t)
          (bt:join-thread thread))))))

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
