(in-package #:slt)

;;;; Engine backed by 3bst, a pure-Lisp port of suckless st.  It is the
;;;; portable default: no foreign libraries are required.  3bst stores
;;;; one CLOS glyph per cell and a single codepoint per cell, so it has
;;;; no combining characters, and it cannot distinguish the default colors
;;;; from explicit palette entries 7 (foreground) and 0 (background).

(defclass 3bst-engine (engine)
  ((term :reader 3bst-engine-term)
   (decoder :initform (make-utf8-decoder) :reader 3bst-engine-decoder)
   ;; Cursor position last copied into a grid, so motion dirties both rows.
   (last-cursor-row :initform 0 :accessor 3bst-engine-last-cursor-row)
   (last-cursor-visible-p :initform t :accessor 3bst-engine-last-cursor-visible-p)))

(defmethod initialize-instance :after ((engine 3bst-engine) &key (rows 24) (columns 80))
  (let ((term (make-instance '3bst:term :rows rows :columns columns)))
    (setf (slot-value engine 'term) term
          (3bst::on-set-title term)
          (lambda (term title)
            (declare (ignore term))
            (engine-emit-title engine title)))))

(defmethod engine-rows ((engine 3bst-engine))
  (3bst:rows (3bst-engine-term engine)))

(defmethod engine-columns ((engine 3bst-engine))
  (3bst:columns (3bst-engine-term engine)))

(defun string-control-sequence-introducer-p (char)
  (and char
       (member char (list #\P #\_ #\^
                          (code-char #x90)
                          (code-char #x9E)
                          (code-char #x9F))
               :test #'char=)))

(defun skip-string-control-sequence (string start)
  (loop with length = (length string)
        for index from start below length
        for char = (aref string index)
        do (cond
             ((char= char +escape-char+)
              (when (< (1+ index) length)
                (let ((next (aref string (1+ index))))
                  (when (char= next #\\)
                    (return (+ index 2))))))
             ((char= char (code-char #x9C))
              (return (1+ index))))
        finally (return length)))

(defun strip-unsupported-control-strings (string)
  "Remove DCS, PM and APC strings, which 3bst would otherwise print."
  (if (not (find-if #'string-control-sequence-introducer-p string))
      string
      (with-output-to-string (out)
        (loop with length = (length string)
              for index from 0 below length
              for char = (aref string index)
              do (cond
                   ((and (char= char +escape-char+)
                         (< (1+ index) length)
                         (string-control-sequence-introducer-p
                          (aref string (1+ index))))
                    (setf index (1- (skip-string-control-sequence string (+ index 2)))))
                   ((and (char/= char #\P) (char/= char #\_) (char/= char #\^)
                         (string-control-sequence-introducer-p char))
                    (setf index (1- (skip-string-control-sequence string (1+ index)))))
                   (t
                    (write-char char out)))))))

(defmethod engine-feed ((engine 3bst-engine) octets &key (start 0) (end (length octets)))
  (let ((string (strip-unsupported-control-strings
                 (utf8-decode (3bst-engine-decoder engine) octets :start start :end end)))
        (3bst::*write-to-child-hook*
          (lambda (term string)
            (declare (ignore term))
            (engine-emit-output engine string))))
    (3bst:handle-input string :term (3bst-engine-term engine))
    engine))

(defmethod engine-resize ((engine 3bst-engine) rows columns)
  (let ((term (3bst-engine-term engine)))
    (3bst::tresize columns rows :term term)
    (fill (3bst:dirty term) 1)
    engine))

(defmethod engine-mode ((engine 3bst-engine) mode)
  (let ((term-mode (3bst::mode (3bst-engine-term engine))))
    (case mode
      (:application-cursor (logtest 3bst::+mode-appcursor+ term-mode))
      (:application-keypad (logtest 3bst::+mode-appkeypad+ term-mode))
      (:bracketed-paste (logtest 3bst::+mode-brcktpaste+ term-mode))
      (:alternate-screen (logtest 3bst::+mode-altscreen+ term-mode))
      (t nil))))

(declaim (inline 3bst-color->color))
(defun 3bst-color->color (value default)
  (declare (type (unsigned-byte 32) value))
  (cond
    ((logbitp 24 value)
     (rgb-color (ldb (byte 8 16) value) (ldb (byte 8 8) value) (ldb (byte 8 0) value)))
    ((= value default) +color-default+)
    (t (palette-color (ldb (byte 8 0) value)))))

(declaim (inline 3bst-mode->attributes))
(defun 3bst-mode->attributes (mode)
  (declare (type (unsigned-byte 16) mode))
  ;; Bits 0-7 (bold .. struck) coincide with ours; wide flags differ.
  (logior (ldb (byte 8 0) mode)
          (if (logtest mode 3bst::+attr-wide+) +attr-wide+ 0)
          (if (logtest mode 3bst::+attr-wdummy+) +attr-wide-spacer+ 0)))

(defun copy-3bst-row (term grid row)
  (declare (type grid grid)
           (type grid-dimension row)
           (optimize (speed 3)))
  (let ((line (aref (the simple-vector (3bst::screen term)) row))
        (base (grid-index grid row 0))
        (default-fg 3bst::*default-foreground*)
        (default-bg 3bst::*default-background*))
    (dotimes (column (grid-columns grid))
      (let* ((glyph (aref line column))
             (char (3bst::c glyph))
             (code (if char (char-code char) 0)))
        (grid-set-cell grid (+ base column)
                       (if (= code 32) 0 code)
                       (3bst-color->color (3bst::fg glyph) default-fg)
                       (3bst-color->color (3bst::bg glyph) default-bg)
                       (3bst-mode->attributes (3bst::mode glyph)))))))

(defmethod engine-update-grid ((engine 3bst-engine) grid &key force)
  (let* ((term (3bst-engine-term engine))
         (rows (3bst:rows term))
         (columns (3bst:columns term))
         (dirty (3bst:dirty term))
         (cursor (3bst::cursor term))
         (cursor-row (min (3bst::y cursor) (1- rows)))
         (cursor-column (min (3bst::x cursor) (1- columns)))
         (visiblep (not (logtest 3bst::+mode-hide+ (3bst::mode term)))))
    (unless (and (= rows (grid-rows grid)) (= columns (grid-columns grid)))
      (grid-resize grid rows columns)
      (setf force t))
    (when (or (/= cursor-row (grid-cursor-row grid))
              (/= cursor-column (grid-cursor-column grid))
              (not (eq visiblep (grid-cursor-visible-p grid))))
      (setf (sbit (grid-dirty grid) (grid-cursor-row grid)) 1
            (sbit (grid-dirty grid) cursor-row) 1))
    (dotimes (row rows)
      (when (or force (plusp (aref dirty row)))
        (copy-3bst-row term grid row)
        (setf (sbit (grid-dirty grid) row) 1
              (aref dirty row) 0)))
    (setf (grid-cursor-row grid) cursor-row
          (grid-cursor-column grid) cursor-column
          (grid-cursor-visible-p grid) visiblep)
    grid))

(register-engine :3bst
                 (lambda (&rest arguments)
                   (apply #'make-instance '3bst-engine arguments)))
