(in-package #:lispvt)

;;;; Cells are stored as a struct of arrays per line: codepoints, colors
;;;; and attributes in parallel typed vectors, so printing a run is a
;;;; handful of REPLACE and FILL calls and a consumer can copy a line with
;;;; REPLACE.  A screen is a vector of lines; scrolling rotates line
;;;; objects rather than moving cells, and lines leaving the top enter a
;;;; scrollback ring whose oldest line is recycled as the new blank one.

;;; Colors are tagged 32-bit values:
;;;   tag 0 : the default foreground or background
;;;   tag 1 : palette index in the low 8 bits
;;;   tag 2 : direct RGB in the low 24 bits

(defconstant +color-default+ 0)

(declaim (inline palette-color rgb-color))
(defun palette-color (index)
  (declare (type (unsigned-byte 8) index))
  (logior (ash 1 24) index))

(defun rgb-color (red green blue)
  (declare (type (unsigned-byte 8) red green blue))
  (logior (ash 2 24) (ash red 16) (ash green 8) blue))

(defconstant +attr-bold+ (ash 1 0))
(defconstant +attr-faint+ (ash 1 1))
(defconstant +attr-italic+ (ash 1 2))
(defconstant +attr-underline+ (ash 1 3))
(defconstant +attr-blink+ (ash 1 4))
(defconstant +attr-inverse+ (ash 1 5))
(defconstant +attr-invisible+ (ash 1 6))
(defconstant +attr-strike+ (ash 1 7))
(defconstant +attr-wide+ (ash 1 8))
(defconstant +attr-wide-spacer+ (ash 1 9))
(defconstant +attr-double-underline+ (ash 1 10))
(defconstant +attr-curly-underline+ (ash 1 11))
(defconstant +attr-dotted-underline+ (ash 1 12))
(defconstant +attr-dashed-underline+ (ash 1 13))
(defconstant +attr-underlines+
  (logior +attr-underline+ +attr-double-underline+ +attr-curly-underline+
          +attr-dotted-underline+ +attr-dashed-underline+)
  "The underline styles; a cell has at most one.")

(deftype code-vector () '(simple-array (unsigned-byte 32) (*)))
(deftype color-vector () '(simple-array (unsigned-byte 32) (*)))
(deftype attr-vector () '(simple-array (unsigned-byte 16) (*)))
(deftype dimension () '(integer 0 65535))

(defstruct (line (:constructor %make-line (codes fg bg attrs)))
  "One row of cells.  A code of 0 is an erased cell; COMBINING holds extra
codepoints as an alist of (COLUMN . STRING); WRAPPED is true when the text
continues on the next line because of autowrap.  UL holds underline colors
(SGR 58), 0 where the underline takes the foreground; it is allocated only
once one is printed on the line.  Cells from EXTENT on are known to be
blank with the default background, so clearing the line stops there."
  (codes nil :type code-vector)
  (fg nil :type color-vector)
  (bg nil :type color-vector)
  (attrs nil :type attr-vector)
  (ul nil :type (or null color-vector))
  (combining '() :type list)
  (wrapped nil :type boolean)
  (extent 0 :type fixnum))

(defun make-line (columns &optional (bg +color-default+))
  (let ((line (%make-line (make-array columns :element-type '(unsigned-byte 32)
                                              :initial-element 0)
                          (make-array columns :element-type '(unsigned-byte 32)
                                              :initial-element 0)
                          (make-array columns :element-type '(unsigned-byte 32)
                                              :initial-element bg)
                          (make-array columns :element-type '(unsigned-byte 16)
                                              :initial-element 0))))
    (unless (= bg +color-default+)
      (setf (line-extent line) columns))
    line))

(declaim (inline note-extent))
(defun note-extent (line end)
  "Cells before END of LINE may no longer be blank."
  (declare (type line line) (type fixnum end))
  (when (> end (line-extent line))
    (setf (line-extent line) end)))

(defun ensure-line-ul (line)
  "LINE's underline colors, allocated on first use."
  (or (line-ul line)
      (setf (line-ul line) (make-array (length (line-codes line))
                                       :element-type '(unsigned-byte 32)
                                       :initial-element 0))))

(declaim (inline line-length))
(defun line-length (line)
  (length (line-codes line)))

(defun line-remove-combining (line start end)
  (declare (type line line) (type fixnum start end))
  (when (line-combining line)
    (setf (line-combining line)
          (delete-if (lambda (entry)
                       (let ((column (car entry)))
                         (declare (type fixnum column))
                         (and (<= start column) (< column end))))
                     (line-combining line)))))

(defun erase-cells (line start end bg)
  "Blank cells [START, END) of LINE with background BG."
  (declare (type line line)
           (type fixnum start end)
           (type (unsigned-byte 32) bg)
           (optimize speed (safety 0)))
  (let* ((end (min end (line-length line)))
         (extent (line-extent line))
         ;; Past the extent, cells are blank already, unless the new
         ;; background is not the default.
         (fill-end (if (= bg +color-default+) (min end extent) end)))
    (declare (type fixnum end extent fill-end))
    (when (< start fill-end)
      (fill (line-codes line) 0 :start start :end fill-end)
      (fill (line-fg line) 0 :start start :end fill-end)
      (fill (line-bg line) bg :start start :end fill-end)
      (fill (line-attrs line) 0 :start start :end fill-end)
      (when (line-ul line)
        (fill (line-ul line) 0 :start start :end fill-end)))
    (cond ((/= bg +color-default+) (note-extent line end))
          ((and (< start extent) (>= end extent)) (setf (line-extent line) start)))
    (when (< start end)
      (line-remove-combining line start end))))

(defun clear-line (line bg)
  (declare (type line line))
  (erase-cells line 0 (line-length line) bg)
  (setf (line-combining line) '()
        (line-wrapped line) nil)
  line)

(defun resize-line (line columns)
  "LINE with exactly COLUMNS cells, truncated or blank-extended."
  (declare (type line line) (type dimension columns))
  (let ((old (line-length line)))
    (if (= old columns)
        line
        (flet ((resized (vector type)
                 (let ((new (make-array columns :element-type type :initial-element 0)))
                   (replace new vector)
                   new)))
          (let ((new (%make-line (resized (line-codes line) '(unsigned-byte 32))
                                 (resized (line-fg line) '(unsigned-byte 32))
                                 (resized (line-bg line) '(unsigned-byte 32))
                                 (resized (line-attrs line) '(unsigned-byte 16)))))
            (when (line-ul line)
              (setf (line-ul new) (resized (line-ul line) '(unsigned-byte 32))))
            (setf (line-extent new) (min columns (line-extent line)))
            (setf (line-combining new)
                  (remove-if (lambda (entry) (>= (car entry) columns))
                             (line-combining line)))
            ;; A wide character cut in half by the new edge goes away.
            (when (and (< columns old)
                       (plusp columns)
                       (logtest (aref (line-attrs new) (1- columns)) +attr-wide+))
              (erase-cells new (1- columns) columns (aref (line-bg new) (1- columns))))
            new)))))

(defstruct (screen (:constructor %make-screen))
  (lines #() :type simple-vector)
  ;; Scrollback ring: HISTORY-COUNT lines starting at HISTORY-START.
  (history #() :type simple-vector)
  (history-start 0 :type fixnum)
  (history-count 0 :type fixnum)
  ;; DECSC state, or NIL.
  (saved-cursor nil))

(defun make-screen (rows columns history-limit)
  (let ((lines (make-array rows)))
    (dotimes (row rows)
      (setf (svref lines row) (make-line columns)))
    (%make-screen :lines lines :history (make-array history-limit :initial-element nil))))

(declaim (inline history-line))
(defun history-line (screen index)
  "The INDEXth line of scrollback, 0 being the oldest."
  (let ((history (screen-history screen)))
    (svref history (mod (+ (screen-history-start screen) index) (length history)))))

(defun history-push (screen line)
  "Append LINE to the scrollback of SCREEN.  Return a line no longer
referenced anywhere (the evicted oldest, or LINE itself when SCREEN keeps no
scrollback), or NIL."
  (declare (type screen screen)
           (optimize speed))
  (let* ((history (screen-history screen))
         (limit (length history))
         (count (screen-history-count screen))
         (start (screen-history-start screen)))
    (declare (type fixnum count start))
    (cond ((zerop limit) line)
          ((< count limit)
           (setf (svref history (mod (+ start count) limit)) line
                 (screen-history-count screen) (1+ count))
           nil)
          (t
           (let ((old (svref history start)))
             (setf (svref history start) line
                   (screen-history-start screen) (mod (1+ start) limit))
             old)))))

(defun clear-history (screen)
  (fill (screen-history screen) nil)
  (setf (screen-history-start screen) 0
        (screen-history-count screen) 0))
