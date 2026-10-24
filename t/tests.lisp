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

(deftest emulator-commands-includes-an-independent-new-window-command ()
  (let* ((calls 0)
         (commands (slt::emulator-commands
                    nil (lambda () (incf calls)))))
    (is (functionp (cdr (assoc :new-window commands))))
    (funcall (cdr (assoc :new-window commands)))
    (is-equal 1 calls)))

(defun graphics-test-feed (parser engine store chunks &optional responses)
  (dolist (chunk chunks)
    (let ((octets (slt::string->octets chunk)))
      (slt::terminal-graphics-feed
       parser octets
       (lambda (bytes start end) (engine-feed engine bytes :start start :end end))
       (lambda (payload) (slt::handle-sixel-command store payload engine :cell-height 16))
       (lambda (payload)
         (let ((response (slt::handle-kitty-command store payload engine)))
           (when (and response responses)
             (vector-push-extend response responses)))))))
  store)

(deftest graphics-stream-parser-intercepts-fragmented-kitty-commands ()
  (let* ((engine (make-engine :lispvt :rows 3 :columns 10))
         (store (slt::make-terminal-image-store))
         (parser (slt::make-terminal-graphics-parser))
         (command (kitty-graphics:encode-graphics-command
                   '((#\a . "T") (#\f . "32") (#\s . "1") (#\v . "1")
                     (#\i . "7") (#\c . "1") (#\r . "1"))
                   (make-array 4 :element-type '(unsigned-byte 8)
                                 :initial-contents '(10 20 30 255)))))
    (graphics-test-feed parser engine store
                        (list "A" (subseq command 0 5) (subseq command 5) "B"))
    (let ((image (gethash 7 (terminal-image-store-images store)))
          (placement (first (terminal-image-store-placements store))))
      (is image)
      (is-equal 1 (terminal-image-width image))
      (is-equal '(10 20 30 255) (coerce (terminal-image-rgba image) 'list))
      (is-equal 0 (terminal-image-placement-row placement))
      (is-equal 1 (terminal-image-placement-column placement)))
    (let ((grid (make-grid 3 10)))
      (engine-update-grid engine grid :force t)
      (is-equal "AB" (grid-row-text grid 0)))))

(deftest graphics-stream-parser-decodes-sixel-and-preserves-surrounding-text ()
  (when (cl-libsixel:libsixel-available-p)
    (let* ((engine (make-engine :lispvt :rows 8 :columns 10))
           (store (slt::make-terminal-image-store))
           (parser (slt::make-terminal-graphics-parser))
           (esc (string (code-char 27)))
           (sixel (concatenate 'string esc "Pq#0;2;100;0;0#0~" esc "\\")))
      (graphics-test-feed parser engine store
                          (list "X" (subseq sixel 0 4) (subseq sixel 4) "Y"))
      (let* ((placement (first (terminal-image-store-placements store)))
             (image (gethash (terminal-image-placement-image-id placement)
                             (terminal-image-store-images store))))
        (is-equal 1 (terminal-image-width image))
        (is-equal 6 (terminal-image-height image))
        (is-equal 0 (terminal-image-placement-row placement))
        (is-equal 1 (terminal-image-placement-column placement)))
      (let ((grid (make-grid 8 10)))
        (engine-update-grid engine grid :force t)
        (is-equal "X" (grid-row-text grid 0))
        (is-equal "Y" (grid-row-text grid 1))))))

(deftest sixel-at-bottom-scrolls-inline-with-terminal-content ()
  (when (cl-libsixel:libsixel-available-p)
    (let* ((engine (make-engine :lispvt :rows 4 :columns 10))
           (grid (make-grid 4 10))
           (store (slt::make-terminal-image-store))
           (esc (string (code-char 27)))
           (sixel (concatenate 'string esc "Pq#0;2;100;0;0#0~" esc "\\")))
      (engine-feed-string engine
                          (format nil "zero~c~cone~c~ctwo~c~cthree"
                                  #\Return #\Linefeed #\Return #\Linefeed
                                  #\Return #\Linefeed))
      (engine-update-grid engine grid :force t)
      (let ((before (slt::grid-fingerprints grid)))
        (slt::handle-sixel-command store sixel engine :cell-height 16)
        (engine-update-grid engine grid)
        (let* ((detected (slt::detect-grid-scroll before grid))
               (scroll (max detected (slt::terminal-image-store-pending-scroll store))))
          (slt::scroll-image-placements store scroll)
          (setf (slt::terminal-image-store-pending-scroll store) 0)
          (is-equal 1 scroll)
          (is-equal 2 (terminal-image-placement-row
                       (first (terminal-image-store-placements store))))
          (multiple-value-bind (row column) (slt::engine-cursor-position engine)
            (is-equal 3 row)
            (is-equal 0 column)))))))

(deftest unchanged-grid-does-not-drift-image-placements ()
  (let ((grid (make-grid 4 10)))
    (is-equal 0 (slt::detect-grid-scroll (slt::grid-fingerprints grid) grid))))

(deftest writing-on-repetitive-grid-is-not-mistaken-for-scroll ()
  (let* ((grid (make-grid 4 10))
         (before (slt::grid-fingerprints grid)))
    (setf (aref (grid-codes grid) (grid-index grid 0 0)) (char-code #\X))
    (is-equal 0 (slt::detect-grid-scroll before grid))))

(deftest sixel-cells-are-erased-by-later-terminal-writes ()
  (let* ((grid (make-grid 2 4))
         (snapshot (slt::snapshot-image-grid grid))
         (store (slt::make-terminal-image-store))
         (rgba (make-array (* 16 16 4) :element-type '(unsigned-byte 8)
                                         :initial-element 255))
         (placement (slt::make-terminal-image-placement
                     :image-id 1 :row 0 :column 0 :columns 2 :rows 1
                     :erase-on-write-p t)))
    (slt::store-image store 1 0 16 16 rgba)
    (push placement (terminal-image-store-placements store))
    (setf (aref (grid-codes grid) (grid-index grid 0 0)) (char-code #\X))
    (slt::erase-written-image-cells store snapshot grid 0 8 16)
    (is (gethash 0 (slt::terminal-image-placement-hidden-cells placement)))
    (let ((records (terminal-image-render-list store 8 16)))
      (is-equal 1 (length records))
      (is-equal 8 (third (first records))))))

(deftest primary-screen-images-are-hidden-on-the-alternate-screen ()
  (let* ((store (slt::make-terminal-image-store))
         (rgba (make-array 4 :element-type '(unsigned-byte 8)
                             :initial-contents '(1 2 3 255))))
    (slt::store-image store 1 0 1 1 rgba)
    (push (slt::make-terminal-image-placement
           :image-id 1 :screen :primary :row 0 :column 0)
          (terminal-image-store-placements store))
    (is-equal 1 (length (terminal-image-render-list store 8 16)))
    (setf (slt::terminal-image-store-active-screen store) :alternate)
    (is (null (terminal-image-render-list store 8 16)))))

(deftest render-emulator-erases-sixel-cells-written-by-the-engine ()
  (multiple-value-bind (emulator engine)
      (make-mock-emulator :rows 2 :columns 4 :cell-width 8 :cell-height 16)
    (let* ((store (slt::terminal-emulator-image-store emulator))
           (rgba (make-array (* 8 16 4) :element-type '(unsigned-byte 8)
                                          :initial-element 255))
           (placement (slt::make-terminal-image-placement
                       :image-id 1 :row 0 :column 0 :columns 1 :rows 1
                       :erase-on-write-p t)))
      (slt::store-image store 1 0 8 16 rgba)
      (push placement (terminal-image-store-placements store))
      (feed engine "X")
      (render-emulator emulator)
      (is (gethash 0 (slt::terminal-image-placement-hidden-cells placement)))
      (is (null (terminal-image-render-list store 8 16))))))

(deftest render-emulator-hides-primary-images-during-alternate-screen-programs ()
  (multiple-value-bind (emulator engine)
      (make-mock-emulator :rows 2 :columns 4 :cell-width 8 :cell-height 16)
    (let* ((store (slt::terminal-emulator-image-store emulator))
           (rgba (make-array 4 :element-type '(unsigned-byte 8)
                               :initial-contents '(1 2 3 255))))
      (slt::store-image store 1 0 1 1 rgba)
      (push (slt::make-terminal-image-placement
             :image-id 1 :screen :primary :row 0 :column 0)
            (terminal-image-store-placements store))
      (feed engine :esc "[?1049h")
      (render-emulator emulator)
      (is (eq :alternate (slt::terminal-image-store-active-screen store)))
      (is (null (terminal-image-render-list store 8 16)))
      (feed engine :esc "[?1049l")
      (render-emulator emulator)
      (is (eq :primary (slt::terminal-image-store-active-screen store)))
      (is-equal 1 (length (terminal-image-render-list store 8 16))))))

(deftest kitty-image-placement-and-deletion-use-ids ()
  (let* ((engine (make-engine :lispvt :rows 3 :columns 10))
         (store (slt::make-terminal-image-store))
         (transmit (kitty-graphics:encode-graphics-command
                    '((#\a . "T") (#\f . "24") (#\s . "1") (#\v . "1")
                      (#\i . "91") (#\p . "4"))
                    (make-array 3 :element-type '(unsigned-byte 8)
                                  :initial-contents '(1 2 3))))
         (delete (kitty-graphics:encode-graphics-command
                  '((#\a . "d") (#\d . "I") (#\i . "91") (#\p . "4")))))
    (graphics-test-feed (slt::make-terminal-graphics-parser) engine store (list transmit))
    (is (gethash 91 (terminal-image-store-images store)))
    (is-equal 1 (length (terminal-image-store-placements store)))
    (graphics-test-feed (slt::make-terminal-graphics-parser) engine store (list delete))
    (is (null (gethash 91 (terminal-image-store-images store))))
    (is (null (terminal-image-store-placements store)))))

(deftest kitty-query-validates-without-storing-or-displaying ()
  (let* ((engine (make-engine :lispvt :rows 3 :columns 10))
         (store (slt::make-terminal-image-store))
         (query (kitty-graphics:encode-graphics-command
                 '((#\a . "q") (#\f . "32") (#\s . "1") (#\v . "1") (#\i . "8"))
                 (make-array 4 :element-type '(unsigned-byte 8)
                               :initial-contents '(20 30 40 255))))
         (response (slt::handle-kitty-command store
                                               (subseq query 2 (- (length query) 2))
                                               engine)))
    (is (search ";OK" response))
    (is-equal 0 (hash-table-count (terminal-image-store-images store)))
    (is (null (terminal-image-store-placements store)))))

(deftest kitty-render-list-honors-crop-offset-and-cell-scaling ()
  (let* ((store (slt::make-terminal-image-store))
         (rgba (make-array (* 40 30 4) :element-type '(unsigned-byte 8)))
         (image (slt::store-image store 9 0 40 30 rgba))
         (command (kitty-graphics:parse-graphics-command
                   "Ga=p,i=9,x=4,y=2,w=20,h=10,c=3,r=2,X=1,Y=2,z=-1")))
    (declare (ignore image))
    (slt::place-image store 9 5 6 command)
    (destructuring-bind (placement rendered-image x y width height)
        (first (terminal-image-render-list store 8 16))
      (declare (ignore placement rendered-image))
      (is-equal 49 x)
      (is-equal 82 y)
      (is-equal 24 width)
      (is-equal 32 height))))

(deftest kitty-uppercase-delete-frees-unplaced-image-data ()
  (let* ((engine (make-engine :lispvt :rows 2 :columns 10))
         (store (slt::make-terminal-image-store))
         (transmit (kitty-graphics:encode-graphics-command
                    '((#\a . "t") (#\f . "32") (#\s . "1") (#\v . "1")
                      (#\i . "77"))
                    (make-array 4 :element-type '(unsigned-byte 8)
                                  :initial-contents '(1 2 3 255))))
         (delete (kitty-graphics:encode-graphics-command
                  '((#\a . "d") (#\d . "I") (#\i . "77")))))
    (graphics-test-feed (slt::make-terminal-graphics-parser) engine store (list transmit))
    (is (gethash 77 (terminal-image-store-images store)))
    (graphics-test-feed (slt::make-terminal-graphics-parser) engine store (list delete))
    (is (null (gethash 77 (terminal-image-store-images store))))))

(deftest oversized-graphics-control-is-discarded-and-stream-recovers ()
  (let* ((engine (make-engine :lispvt :rows 2 :columns 10))
         (store (slt::make-terminal-image-store))
         (parser (slt::make-terminal-graphics-parser :max-control-bytes 1024))
         (esc (string (code-char 27)))
         (payload (concatenate 'string esc "_Ga=T,m=0;"
                               (make-string 1200 :initial-element #\A) esc "\\Z")))
    (graphics-test-feed parser engine store (list payload))
    (let ((grid (make-grid 2 10)))
      (engine-update-grid engine grid :force t)
      (is-equal "Z" (grid-row-text grid 0)))
    (is-equal 0 (hash-table-count (terminal-image-store-images store)))))

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

(defun wait-for-repl-server-thread ()
  ;; Swank registers a new server from the thread it spawns, so stopping
  ;; one immediately after starting it would find nothing to stop.
  (sleep 0.2))

(deftest repl-servers-start-and-stop ()
  (dolist (kind '(:swank :slynk))
    (let ((slt:*repl-server* kind))
      (is (not (repl-server-running-p)))
      (let ((port (start-repl-server :port 0)))
        (unwind-protect
             (progn
               (is (plusp port))
               (is-equal (list kind port)
                         (multiple-value-list (repl-server-running-p)))
               (is (search (format nil "port ~d" port) (slt::repl-server-menu-label)))
               (is (null (ignore-errors (start-repl-server :port 0)))
                   "only one server runs at a time"))
          (wait-for-repl-server-thread)
          (stop-repl-server)))
      (is (not (repl-server-running-p)))
      (is (search "Start" (slt::repl-server-menu-label))))))

(deftest repl-server-key-handler-toggles-the-server ()
  (multiple-value-bind (emulator engine) (make-mock-emulator :rows 1 :columns 8)
    (let ((slt:*repl-server-port* 0)
          (*error-output* (make-broadcast-stream)))
      (unwind-protect
           (progn
             (slt::handle-keypress emulator "L" "L" slt::+control-mask+)
             (is (repl-server-running-p) "Control+Shift+L starts the server")
             (wait-for-repl-server-thread)
             (slt::handle-keypress emulator "L" "L" slt::+mod4-mask+)
             (is (not (repl-server-running-p)) "Super+Shift+L stops it")
             (slt::handle-keypress emulator "l" "l" slt::+mod4-mask+)
             (is (not (repl-server-running-p)) "Super+L is not the binding")
             (slt::handle-keypress emulator "L" "L" 0)
             (is (not (repl-server-running-p)))
             (is-equal "L" (grid-row-text (engine-grid engine) 0)
                       "only the binding is consumed"))
        (stop-repl-server)))))

(deftest menu-items-call-their-function-with-the-emulator ()
  (let* ((chosen nil)
         (slt:*menu-items* (list (list "Plain" (lambda (emulator) (setf chosen emulator)))
                                 '(slt::repl-server-menu-label slt:toggle-repl-server)))
         (items (slt::emulator-menu-items :the-emulator)))
    (is-equal "Plain" (first (first items)))
    (funcall (second (first items)))
    (is-equal :the-emulator chosen)
    (is (stringp (funcall (first (second items)))))))

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

(deftest parse-cli-arguments-reads-repl-server-options ()
  (let ((command (parse-cli-arguments '("--repl-server" "slynk" "--repl-port" "4123"))))
    (is-equal "slynk" (clingon:getopt command :repl-server))
    (is-equal 4123 (clingon:getopt command :repl-port))
    (is (null (ignore-errors (parse-cli-arguments '("--repl-server" "nrepl"))))
        "only swank and slynk are accepted")))

(deftest parse-cli-arguments-keeps-bare-backend-compatibility ()
  (let* ((command (parse-cli-arguments '("ltk")))
         (arguments (command-launch-arguments command)))
    (is-equal '(:backend "ltk")
              arguments)))

(deftest handle-cli-command-can-list-backends-without-launching-the-gui ()
  (let* ((command (parse-cli-arguments '("--list-backends")))
         (output (with-output-to-string (stream)
                  (slt::handle-cli-command command :stream stream))))
    (is (search "ltk" output))
    (is (search "sdl2" output))))

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

(deftest selection-text-spans-rows-and-trims-blanks ()
  (let ((grid (engine-grid (feed (make-test-engine :rows 3 :columns 10)
                                 "one two" #\Return #\Newline "three"))))
    (is (null (grid-selection-text grid)))
    (grid-set-selection grid 4 19)
    (is-equal (format nil "two~%three") (grid-selection-text grid))
    (grid-set-selection grid 12 1)
    (is-equal (format nil "ne two~%thr") (grid-selection-text grid) "either order")
    (grid-set-selection grid nil nil)
    (is (null (grid-selection grid)))))

(deftest selection-extent-grows-to-words-and-lines ()
  (let ((grid (engine-grid (feed (make-test-engine :rows 2 :columns 20)
                                 "cd ~/src/slt; ls"))))
    (multiple-value-bind (first last) (slt::selection-extent grid 6 6 :word)
      (is-equal "~/src/slt" (slt::grid-range-text grid first last) "paths are words"))
    (multiple-value-bind (first last) (slt::selection-extent grid 2 2 :word)
      (is-equal 2 first "a blank selects only itself")
      (is-equal 2 last))
    (multiple-value-bind (first last) (slt::selection-extent grid 5 25 :line)
      (is-equal 0 first)
      (is-equal 39 last))))

(deftest pack-grid-highlights-the-selection ()
  (let* ((grid (engine-grid (feed (make-test-engine :rows 1 :columns 4) "ab")))
         (palette (make-palette :selection #x123456))
         (atlas (slt::make-glyph-atlas 4 4 #'fake-rasterizer))
         (instances nil))
    (grid-set-selection grid 1 2)
    (setf instances (slt::pack-grid grid atlas palette instances))
    (flet ((bg (cell) (aref instances (+ 1 (* 4 cell)))))
      (is (/= #x123456 (bg 0)))
      (is-equal #x123456 (bg 1))
      (is-equal #x123456 (bg 2))
      (is (/= #x123456 (bg 3))))))

;;; Mouse reporting

(deftest xterm-encode-mouse-follows-tracking-modes ()
  (let ((esc (code-char 27)))
    (is (null (xterm-encode-mouse :press 1 0 0 '())) "no tracking, no report")
    (is-equal (format nil "~c[<0;3;2M" esc)
              (xterm-encode-mouse :press 1 1 2 '() :tracking :normal :sgr t))
    (is-equal (format nil "~c[<2;3;2m" esc)
              (xterm-encode-mouse :release 3 1 2 '() :tracking :normal :sgr t))
    (is-equal (format nil "~c[<20;1;1M" esc)
              (xterm-encode-mouse :press 1 0 0 '(:shift :control) :tracking :normal :sgr t))
    (is-equal (format nil "~c[<65;1;1M" esc)
              (xterm-encode-mouse :press 5 0 0 '() :tracking :normal :sgr t)
              "wheel down")
    (is-equal (format nil "~c[M ##" esc)
              (xterm-encode-mouse :press 1 2 2 '() :tracking :normal)
              "legacy encoding")
    (is-equal (format nil "~c[M#!!" esc)
              (xterm-encode-mouse :release 1 0 0 '() :tracking :normal)
              "legacy release is button 3")
    (is (null (xterm-encode-mouse :motion 1 0 0 '() :tracking :normal)))
    (is-equal (format nil "~c[<32;1;1M" esc)
              (xterm-encode-mouse :motion 1 0 0 '() :tracking :button :sgr t))
    (is (null (xterm-encode-mouse :motion nil 0 0 '() :tracking :button :sgr t)))
    (is-equal (format nil "~c[<35;1;1M" esc)
              (xterm-encode-mouse :motion nil 0 0 '() :tracking :any :sgr t))
    (is (null (xterm-encode-mouse :release 1 0 0 '() :tracking :x10)))
    (is (null (xterm-encode-mouse :press 1 0 200 '() :tracking :normal))
        "legacy coordinates past one octet are dropped")))

(deftest engines-report-the-mouse-once-the-child-asks ()
  (dolist (name '(:lispvt :3bst :libvterm :ghostty))
    (when (slt::engine-available-p name)
      (let ((engine (make-test-engine :engine name :rows 4 :columns 10))
            (esc (code-char 27)))
        (is (null (engine-mode engine :mouse-tracking)) name)
        (feed engine :esc "[?1002h" :esc "[?1006h")
        (is-equal :button (engine-mode engine :mouse-tracking) name)
        (is-equal (format nil "~c[<0;3;2M" esc)
                  (engine-encode-mouse engine :press 1 1 2 '()) name)
        (is-equal (format nil "~c[<32;4;2M" esc)
                  (engine-encode-mouse engine :motion 1 1 3 '()) name)
        (is-equal (format nil "~c[<0;4;2m" esc)
                  (engine-encode-mouse engine :release 1 1 3 '()) name)
        (feed engine :esc "[?1002l")
        (is (null (engine-mode engine :mouse-tracking)) name)))))

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

(deftest headless-mouse-reaches-a-tracking-child ()
  (let* ((backend (make-instance 'headless-backend))
         (thread (bt:make-thread
                  (lambda ()
                    (launch-terminal :backend backend :engine :lispvt :shell "/bin/sh"
                                     :rows 6 :columns 40 :load-config nil))
                  :name "slt mouse test")))
    (unwind-protect
         (progn
           (is (wait-until (lambda () (slt::headless-backend-view backend))))
           (let ((view (slt::headless-backend-view backend)))
             (is (wait-until (lambda () (search "$" (headless-screen-text view)))))
             ;; READY, split on the command line, shows the modes are set.
             (headless-paste view (format nil "printf '\\033[?1000h\\033[?1006hRE''ADY'; cat -v~%"))
             (is (wait-until (lambda () (search "READY" (headless-screen-text view)))))
             (headless-mouse view :press 2 4 1 3 slt::+control-mask+)
             (headless-mouse view :release 2 4 1 3 0)
             (headless-key view "" "Return" 0)
             (is (wait-until (lambda ()
                               (search "^[[<18;5;3M^[[<2;5;3m" (headless-screen-text view))))
                 "the click reaches the child as SGR reports")
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

(defun find-screen-cell (view text)
  "The (ROW COLUMN) where TEXT starts on VIEW's screen, or NIL."
  (let ((grid (headless-view-grid view)))
    (when grid
      (loop for line in (grid-text grid)
            for row from 0
            for column = (search text line)
            when column return (list row column)))))

(deftest headless-selection-copies-and-pastes ()
  (let* ((backend (make-instance 'headless-backend))
         (thread (bt:make-thread
                  (lambda ()
                    (launch-terminal :backend backend :shell "/bin/sh"
                                     :rows 8 :columns 40 :load-config nil))
                  :name "slt selection test")))
    (unwind-protect
         (progn
           (is (wait-until (lambda () (slt::headless-backend-view backend))))
           (let ((view (slt::headless-backend-view backend)))
             (is (wait-until (lambda () (search "$" (headless-screen-text view)))))
             (headless-paste view (format nil "printf 'alpha %s\\n' beta/gamma~%"))
             (is (wait-until (lambda () (find-screen-cell view "alpha beta/gamma"))))
             (destructuring-bind (row column) (find-screen-cell view "alpha beta/gamma")
               ;; Drag over "alpha", then copy with Super+C.
               (headless-mouse view :press row column)
               (headless-mouse view :drag row (+ column 4))
               (headless-mouse view :release row (+ column 4))
               (headless-key view "c" "c" slt::+mod4-mask+)
               (is (wait-until (lambda () (equal "alpha" (headless-view-clipboard view))))
                   "a drag selects characters and Super+C copies them")
               ;; A double click selects the path-like word.
               (headless-mouse view :press row (+ column 8) 2)
               (headless-mouse view :release row (+ column 8) 2)
               (headless-key view "C" "C" slt::+control-mask+)
               (is (wait-until (lambda () (equal "beta/gamma" (headless-view-clipboard view))))
                   "a double click selects a word and Control+Shift+C copies it"))
             ;; Paste the clipboard back into the shell.
             (headless-key view "e" "e" 0)
             (headless-key view "c" "c" 0)
             (headless-key view "h" "h" 0)
             (headless-key view "o" "o" 0)
             (headless-key view " " " " 0)
             (headless-key view "v" "v" slt::+mod4-mask+)
             (headless-key view "" "Return" 0)
             (is (wait-until (lambda ()
                               (search (format nil "~%beta/gamma") (headless-screen-text view))))
                 "Super+V pastes the clipboard")
             (is (null (grid-selection (headless-view-grid view)))
                 "typing clears the selection")
             (headless-close view)
             (is (wait-until (lambda () (not (bt:thread-alive-p thread)))))))
      (when (bt:thread-alive-p thread)
        (setf (slt::headless-backend-quit-p backend) t)
        (bt:join-thread thread)))))

;;; Links, quoting, the shell's environment and the init file

(defun grid-showing (string &key (rows 2) (columns 60))
  (let ((engine (make-engine :lispvt :rows rows :columns columns))
        (grid (make-grid rows columns)))
    (engine-feed-string engine string)
    (engine-update-grid engine grid)
    grid))

(deftest url-at-finds-the-link-under-a-cell ()
  (let ((grid (grid-showing "see https://example.com/a?b=1, ok")))
    (is-equal "https://example.com/a?b=1" (url-at grid 0 10))
    (is-equal "https://example.com/a?b=1" (url-at grid 0 4)
              "the scheme's first letter counts")
    (is (null (url-at grid 0 1)) "plain words are not links")
    (is (null (url-at grid 0 30)) "nor are trailing words")
    (is (null (url-at grid 5 0)) "cells outside the grid have none")))

(deftest shell-quote-leaves-plain-words-alone ()
  (is-equal "/tmp/a-b_c.txt" (shell-quote "/tmp/a-b_c.txt"))
  (is-equal "'/tmp/two words'" (shell-quote "/tmp/two words"))
  (is-equal "'it'\\''s'" (shell-quote "it's")))

(deftest terminal-process-environment-adds-and-removes-variables ()
  (is-equal '("COLORTERM=truecolor" "HOME=/tmp")
            (terminal-process-environment :term-name nil
                                          :variables '(("COLORTERM" . "truecolor")
                                                       ("LANG" . nil))
                                          :base-environment '("LANG=C" "HOME=/tmp"))))

(deftest shell-arguments-mark-login-shells-with-a-dash ()
  (is-equal '("-zsh") (slt::shell-arguments "/bin/zsh" t))
  (is-equal '("/bin/zsh" "-i") (slt::shell-arguments "/bin/zsh" nil)))

(deftest load-user-config-reports-errors-instead-of-signalling ()
  (let ((pathname (uiop:with-temporary-file (:stream out :pathname p :keep t)
                    (write-string "(defparameter cl-user::*slt-test-loaded* t) (error \"boom\")" out)
                    p)))
    (unwind-protect
         (progn
           (is-equal pathname (load-user-config pathname))
           (is (symbol-value 'cl-user::*slt-test-loaded*) "forms before the error ran")
           (is (search "boom" *config-error*)))
      (delete-file pathname)
      (setf *config-error* nil))))

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
