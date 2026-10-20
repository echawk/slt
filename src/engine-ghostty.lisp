(in-package #:slt)

;;;; Engine backed by Ghostty's libghostty-vt through the ghostty-vt
;;;; binding (github.com/echawk/cl-ghostty-vt).
;;;;
;;;; Screen updates go through a small C helper (src/native/slt-ghostty.c)
;;;; that walks Ghostty's render state and writes only dirty rows straight
;;;; into the grid's arrays.  It is compiled on first load; without a C
;;;; compiler the engine falls back to reading cells through the binding.

(defun ghostty-helper-pathname ()
  (asdf:system-relative-pathname
   :slt (format nil "build/native/libslt-ghostty.~a"
                (if (uiop:os-macosx-p) "dylib" "so"))))

(defun build-ghostty-helper ()
  "Compile the C helper when it is missing or older than its source."
  (let ((source (asdf:system-relative-pathname :slt "src/native/slt-ghostty.c"))
        (output (ghostty-helper-pathname))
        (include (asdf:system-relative-pathname :ghostty-vt "build/include/")))
    (when (or (not (probe-file output))
              (> (file-write-date source) (file-write-date output)))
      (ensure-directories-exist output)
      (uiop:run-program
       (append (list (or (uiop:getenv "CC") "cc") "-O2" "-std=c11" "-shared" "-fPIC")
               (if (uiop:os-macosx-p) (list "-undefined" "dynamic_lookup") '())
               (list "-I" (namestring include)
                     (namestring source) "-o" (namestring output)))
       :error-output :output :output :interactive))
    output))

(defvar *ghostty-helper-loaded* nil)

(defun ensure-ghostty-helper ()
  (unless *ghostty-helper-loaded*
    (setf *ghostty-helper-loaded*
          (handler-case
              (progn (cffi:load-foreign-library (build-ghostty-helper)) t)
            (error (condition)
              (warn "slt: ghostty fast path unavailable (~a); reading cells individually."
                    condition)
              :unavailable))))
  (eq *ghostty-helper-loaded* t))

(ensure-ghostty-helper)

(cffi:defcfun ("slt_ghostty_new" %slt-ghostty-new) :pointer)
(cffi:defcfun ("slt_ghostty_free" %slt-ghostty-free) :void (helper :pointer))
(cffi:defcfun ("slt_ghostty_copy" %slt-ghostty-copy) :int
  (helper :pointer) (terminal :pointer) (rows :int) (columns :int) (force :int)
  (codes :pointer) (fg :pointer) (bg :pointer) (attrs :pointer) (dirty :pointer)
  (extra :pointer) (extra-capacity :int) (extra-length :pointer) (cursor :pointer))
(cffi:defcfun ("slt_ghostty_scroll" %slt-ghostty-scroll) :void
  (terminal :pointer) (mode :int) (delta :long))

(defconstant +ghostty-extra-capacity+ 8192)
(defconstant +attr-combining+ (ash 1 15))

(defclass ghostty-engine (engine)
  ((terminal :reader ghostty-engine-terminal)
   (helper :initform nil :accessor ghostty-engine-helper)
   (row-dirty :initform (make-array 0 :element-type '(unsigned-byte 8))
              :accessor ghostty-engine-row-dirty)
   (extra :initform (make-array +ghostty-extra-capacity+ :element-type '(unsigned-byte 32))
          :reader ghostty-engine-extra)
   ;; Set by resizes and scrolling: the next update must copy every row.
   (force-next :initform t :accessor ghostty-engine-force-next)
   (scrolled-p :initform nil :accessor ghostty-engine-scrolled-p)))

(defmethod initialize-instance :after ((engine ghostty-engine) &key (rows 24) (columns 80))
  (setf (slot-value engine 'terminal)
        (ghostty-vt:make-terminal
         :rows rows
         :columns columns
         :on-pty-write (lambda (octets)
                         (engine-emit-output engine
                                             (babel:octets-to-string octets :encoding :utf-8
                                                                            :errorp nil)))
         :on-bell (lambda () (engine-emit-bell engine))
         :on-title (lambda (title) (engine-emit-title engine title))))
  (when (ensure-ghostty-helper)
    (setf (ghostty-engine-helper engine) (%slt-ghostty-new))))

(defmethod engine-rows ((engine ghostty-engine))
  (ghostty-vt:terminal-rows (ghostty-engine-terminal engine)))

(defmethod engine-columns ((engine ghostty-engine))
  (ghostty-vt:terminal-columns (ghostty-engine-terminal engine)))

(defmethod engine-feed ((engine ghostty-engine) octets &key (start 0) (end (length octets)))
  (ghostty-vt:terminal-feed (ghostty-engine-terminal engine) octets :start start :end end)
  engine)

(defmethod engine-resize ((engine ghostty-engine) rows columns)
  (ghostty-vt:terminal-resize (ghostty-engine-terminal engine) rows columns)
  (setf (ghostty-engine-force-next engine) t)
  engine)

(defmethod engine-close ((engine ghostty-engine))
  (when (ghostty-engine-helper engine)
    (%slt-ghostty-free (ghostty-engine-helper engine))
    (setf (ghostty-engine-helper engine) nil))
  (ghostty-vt:terminal-free (ghostty-engine-terminal engine)))

(defmethod engine-scroll ((engine ghostty-engine) how)
  (when (and (ghostty-engine-helper engine)
             ;; Nothing to do when already following the live screen.
             (or (ghostty-engine-scrolled-p engine) (not (eq how :bottom))))
    (setf (ghostty-engine-scrolled-p engine) (not (eq how :bottom)))
    (multiple-value-bind (mode delta)
        (etypecase how
          ((eql :top) (values 0 0))
          ((eql :bottom) (values 1 0))
          (integer (values 2 how)))
      (%slt-ghostty-scroll (ghostty-vt:terminal-handle (ghostty-engine-terminal engine))
                           mode delta)
      (setf (ghostty-engine-force-next engine) t)
      t)))

(defun ghostty-color->color (designator)
  (ecase (first designator)
    (:default +color-default+)
    (:palette (palette-color (second designator)))
    (:rgb (destructuring-bind (red green blue) (rest designator)
            (rgb-color red green blue)))))

(defun ghostty-cell-attributes (cell)
  (logior (loop for attribute in (ghostty-vt:cell-attrs cell)
                sum (case attribute
                      (:bold +attr-bold+)
                      (:faint +attr-faint+)
                      (:italic +attr-italic+)
                      ((:underline :curly-underline :dotted-underline :dashed-underline)
                       +attr-underline+)
                      (:double-underline +attr-double-underline+)
                      (:blink +attr-blink+)
                      (:inverse +attr-inverse+)
                      (:invisible +attr-invisible+)
                      (:strike +attr-strike+)
                      (t 0)))
          (case (ghostty-vt:cell-width cell)
            (:wide +attr-wide+)
            (:spacer-tail +attr-wide-spacer+)
            (t 0))))

(defun copy-ghostty-row (terminal grid row)
  "Copy ROW into GRID.  Return true when anything in the row changed."
  (let ((base (grid-index grid row 0))
        (changed nil))
    (dotimes (column (grid-columns grid) changed)
      (let* ((cell (ghostty-vt:terminal-cell terminal row column))
             (codepoints (ghostty-vt:cell-codepoints cell))
             (index (+ base column))
             (code (if codepoints (first codepoints) 0))
             (fg (ghostty-color->color (ghostty-vt:cell-fg cell)))
             (bg (ghostty-color->color (ghostty-vt:cell-bg cell)))
             (attrs (ghostty-cell-attributes cell))
             (combining (and (rest codepoints) (map 'string #'code-char (rest codepoints)))))
        (unless (and (= code (aref (grid-codes grid) index))
                     (= fg (aref (grid-fg grid) index))
                     (= bg (aref (grid-bg grid) index))
                     (= attrs (aref (grid-attrs grid) index))
                     (equal combining (gethash index (grid-combining grid))))
          (setf changed t)
          (grid-set-cell grid index code fg bg attrs)
          (grid-set-combining grid index combining))))))

(defun ghostty-update-grid-fast (engine grid force)
  (let* ((terminal (ghostty-engine-terminal engine))
         (rows (ghostty-vt:terminal-rows terminal))
         (columns (ghostty-vt:terminal-columns terminal)))
    (unless (and (= rows (grid-rows grid)) (= columns (grid-columns grid)))
      (grid-resize grid rows columns)
      (setf force t))
    (when (ghostty-engine-force-next engine)
      (setf force t
            (ghostty-engine-force-next engine) nil))
    (unless (= (length (ghostty-engine-row-dirty engine)) rows)
      (setf (ghostty-engine-row-dirty engine)
            (make-array rows :element-type '(unsigned-byte 8))))
    (let ((row-dirty (ghostty-engine-row-dirty engine))
          (extra (ghostty-engine-extra engine))
          (codes (grid-codes grid))
          (fg (grid-fg grid))
          (bg (grid-bg grid))
          (attrs (grid-attrs grid)))
      (fill row-dirty 0)
      (cffi:with-foreign-objects ((extra-length :int) (cursor :int32 4))
        (sb-sys:with-pinned-objects (codes fg bg attrs row-dirty extra)
          (when (minusp (%slt-ghostty-copy
                         (ghostty-engine-helper engine)
                         (ghostty-vt:terminal-handle terminal)
                         rows columns (if force 1 0)
                         (sb-sys:vector-sap codes) (sb-sys:vector-sap fg)
                         (sb-sys:vector-sap bg) (sb-sys:vector-sap attrs)
                         (sb-sys:vector-sap row-dirty) (sb-sys:vector-sap extra)
                         +ghostty-extra-capacity+ extra-length cursor))
            (error "Ghostty render state update failed.")))
        ;; Combining marks of the rows just written.
        (let ((combining (grid-combining grid)))
          (when (plusp (hash-table-count combining))
            (dotimes (row rows)
              (when (= 1 (aref row-dirty row))
                (let ((base (* row columns)))
                  (dotimes (column columns)
                    (remhash (+ base column) combining)))))))
        (loop with length = (cffi:mem-ref extra-length :int)
              with i = 0
              while (< i length)
              do (let ((index (aref extra i))
                       (count (aref extra (1+ i))))
                   (grid-set-combining grid index
                                       (map 'string #'code-char
                                            (subseq extra (+ i 2) (+ i 2 count))))
                   (incf i (+ 2 count))))
        (dotimes (row rows)
          (when (= 1 (aref row-dirty row))
            (setf (sbit (grid-dirty grid) row) 1)))
        (let ((cursor-row (min (max 0 (cffi:mem-aref cursor :int32 0)) (1- rows)))
              (cursor-column (min (max 0 (cffi:mem-aref cursor :int32 1)) (1- columns)))
              (visiblep (/= 0 (cffi:mem-aref cursor :int32 2)))
              (shape (case (cffi:mem-aref cursor :int32 3)
                       (0 :bar) (2 :underline) (t :block))))
          (when (or (/= cursor-row (grid-cursor-row grid))
                    (/= cursor-column (grid-cursor-column grid))
                    (not (eq visiblep (grid-cursor-visible-p grid)))
                    (not (eq shape (grid-cursor-shape grid))))
            (setf (sbit (grid-dirty grid) (grid-cursor-row grid)) 1
                  (sbit (grid-dirty grid) cursor-row) 1))
          (setf (grid-cursor-row grid) cursor-row
                (grid-cursor-column grid) cursor-column
                (grid-cursor-visible-p grid) visiblep
                (grid-cursor-shape grid) shape))))
    grid))

(defmethod engine-update-grid ((engine ghostty-engine) grid &key force)
  (if (ghostty-engine-helper engine)
      (ghostty-update-grid-fast engine grid force)
      (ghostty-update-grid-slow engine grid force)))

(defun ghostty-update-grid-slow (engine grid force)
  (let* ((terminal (ghostty-engine-terminal engine))
         (rows (ghostty-vt:terminal-rows terminal))
         (columns (ghostty-vt:terminal-columns terminal)))
    (unless (and (= rows (grid-rows grid)) (= columns (grid-columns grid)))
      (grid-resize grid rows columns)
      (setf force t))
    (dotimes (row rows)
      (when (or (copy-ghostty-row terminal grid row) force)
        (setf (sbit (grid-dirty grid) row) 1)))
    (multiple-value-bind (cursor-row cursor-column visiblep)
        (ghostty-vt:terminal-cursor terminal)
      (let ((cursor-row (min cursor-row (1- rows)))
            (cursor-column (min cursor-column (1- columns))))
        (when (or (/= cursor-row (grid-cursor-row grid))
                  (/= cursor-column (grid-cursor-column grid))
                  (not (eq visiblep (grid-cursor-visible-p grid))))
          (setf (sbit (grid-dirty grid) (grid-cursor-row grid)) 1
                (sbit (grid-dirty grid) cursor-row) 1))
        (setf (grid-cursor-row grid) cursor-row
              (grid-cursor-column grid) cursor-column
              (grid-cursor-visible-p grid) visiblep)))
    grid))

(defmethod engine-mode ((engine ghostty-engine) mode)
  (let ((terminal (ghostty-engine-terminal engine)))
    (case mode
      (:application-cursor (ghostty-vt:terminal-mode terminal 1))
      (:application-keypad (ghostty-vt:terminal-mode terminal 66))
      (:bracketed-paste (ghostty-vt:terminal-mode terminal 2004))
      (:alternate-screen (eq (ghostty-vt:terminal-active-screen terminal) :alternate))
      (:mouse-tracking (cond ((ghostty-vt:terminal-mode terminal 1003) :any)
                             ((ghostty-vt:terminal-mode terminal 1002) :button)
                             ((ghostty-vt:terminal-mode terminal 1000) :normal)
                             ((ghostty-vt:terminal-mode terminal 9) :x10)))
      (:mouse-sgr (ghostty-vt:terminal-mode terminal 1006))
      (t nil))))

(defmethod engine-encode-key ((engine ghostty-engine) key modifiers)
  (let ((octets (ghostty-vt:encode-key
                 (ghostty-engine-terminal engine)
                 key
                 :mods (loop for modifier in modifiers
                             for mapped = (case modifier
                                            (:shift :shift)
                                            (:control :ctrl)
                                            (:meta :alt)
                                            (:super :super))
                             when mapped collect mapped))))
    (and octets (plusp (length octets))
         (babel:octets-to-string octets :encoding :utf-8 :errorp nil))))

(register-engine :ghostty
                 (lambda (&rest arguments)
                   (apply #'make-instance 'ghostty-engine arguments)))
