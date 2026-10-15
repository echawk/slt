(in-package #:slt)

;;;; Engine backed by Ghostty's libghostty-vt through the ghostty-vt
;;;; binding (github.com/echawk/cl-ghostty-vt).  The binding has no damage
;;;; tracking yet, so every update reads the whole screen and diffs it
;;;; against the grid to decide which rows are dirty.

(defclass ghostty-engine (engine)
  ((terminal :reader ghostty-engine-terminal)))

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
         :on-title (lambda (title) (engine-emit-title engine title)))))

(defmethod engine-rows ((engine ghostty-engine))
  (ghostty-vt:terminal-rows (ghostty-engine-terminal engine)))

(defmethod engine-columns ((engine ghostty-engine))
  (ghostty-vt:terminal-columns (ghostty-engine-terminal engine)))

(defmethod engine-feed ((engine ghostty-engine) octets &key (start 0) (end (length octets)))
  (ghostty-vt:terminal-feed (ghostty-engine-terminal engine) octets :start start :end end)
  engine)

(defmethod engine-resize ((engine ghostty-engine) rows columns)
  (ghostty-vt:terminal-resize (ghostty-engine-terminal engine) rows columns)
  engine)

(defmethod engine-close ((engine ghostty-engine))
  (ghostty-vt:terminal-free (ghostty-engine-terminal engine)))

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

(defmethod engine-update-grid ((engine ghostty-engine) grid &key force)
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
