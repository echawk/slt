(in-package #:slt)

;;;; The grid is the engine-neutral snapshot of a screen that renderers
;;;; consume.  It is stored as parallel typed vectors (struct of arrays)
;;;; indexed by ROW * COLUMNS + COLUMN so that renderers can walk it
;;;; without allocating and SBCL can open-code every access.

(deftype codepoint () '(unsigned-byte 32))
(deftype color () '(unsigned-byte 32))
(deftype attributes () '(unsigned-byte 16))
(deftype grid-index () '(integer 0 #.(1- array-total-size-limit)))
(deftype grid-dimension () '(integer 0 65535))

;;; Colors are tagged so engines need not agree on a palette: the frontend
;;; resolves them with a user-configurable PALETTE at render time.
;;;   tag 0 : the default foreground or background
;;;   tag 1 : palette index in the low 8 bits
;;;   tag 2 : direct RGB in the low 24 bits

(defconstant +color-default+ 0)
(defconstant +color-tag-palette+ 1)
(defconstant +color-tag-rgb+ 2)

(declaim (inline palette-color rgb-color color-tag color-value
                 color-default-p color-palette-p color-rgb-p))

(defun palette-color (index)
  (declare (type (unsigned-byte 8) index))
  (logior (ash +color-tag-palette+ 24) index))

(defun rgb-color (red green blue)
  (declare (type (unsigned-byte 8) red green blue))
  (logior (ash +color-tag-rgb+ 24) (ash red 16) (ash green 8) blue))

(defun color-tag (color)
  (declare (type color color))
  (ldb (byte 8 24) color))

(defun color-value (color)
  (declare (type color color))
  (ldb (byte 24 0) color))

(defun color-default-p (color) (= (color-tag color) 0))
(defun color-palette-p (color) (= (color-tag color) +color-tag-palette+))
(defun color-rgb-p (color) (= (color-tag color) +color-tag-rgb+))

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

(defparameter *attribute-names*
  `((:bold . ,+attr-bold+)
    (:faint . ,+attr-faint+)
    (:italic . ,+attr-italic+)
    (:underline . ,+attr-underline+)
    (:blink . ,+attr-blink+)
    (:inverse . ,+attr-inverse+)
    (:invisible . ,+attr-invisible+)
    (:strike . ,+attr-strike+)
    (:wide . ,+attr-wide+)
    (:wide-spacer . ,+attr-wide-spacer+)
    (:double-underline . ,+attr-double-underline+)))

(defun attributes->list (attributes)
  (loop for (name . mask) in *attribute-names*
        when (logtest mask attributes)
          collect name))

(defun list->attributes (names)
  (loop with result = 0
        for name in names
        for mask = (or (cdr (assoc name *attribute-names*))
                       (error "Unknown cell attribute ~S." name))
        do (setf result (logior result mask))
        finally (return result)))

(defstruct (grid (:constructor %make-grid))
  (rows 0 :type grid-dimension)
  (columns 0 :type grid-dimension)
  (codes (make-array 0 :element-type 'codepoint)
   :type (simple-array codepoint (*)))
  (fg (make-array 0 :element-type 'color)
   :type (simple-array color (*)))
  (bg (make-array 0 :element-type 'color)
   :type (simple-array color (*)))
  (attrs (make-array 0 :element-type 'attributes)
   :type (simple-array attributes (*)))
  ;; Extra codepoints (combining marks, ZWJ sequences) keyed by cell index.
  (combining (make-hash-table) :type hash-table)
  (dirty (make-array 0 :element-type 'bit) :type simple-bit-vector)
  (cursor-row 0 :type grid-dimension)
  (cursor-column 0 :type grid-dimension)
  (cursor-visible-p t :type boolean)
  (cursor-shape :block :type (member :block :underline :bar))
  ;; Selected cells as (FIRST . LAST) cell indices in reading order,
  ;; inclusive, or NIL.  Set with GRID-SET-SELECTION.
  (selection nil :type (or null (cons grid-index grid-index))))

(defun make-grid (rows columns)
  (let ((grid (%make-grid)))
    (grid-resize grid rows columns)))

(defun grid-resize (grid rows columns)
  "Reallocate GRID for ROWS x COLUMNS, blank and fully dirty."
  (let ((size (* rows columns)))
    (setf (grid-rows grid) rows
          (grid-columns grid) columns
          (grid-codes grid) (make-array size :element-type 'codepoint
                                             :initial-element 0)
          (grid-fg grid) (make-array size :element-type 'color
                                          :initial-element +color-default+)
          (grid-bg grid) (make-array size :element-type 'color
                                          :initial-element +color-default+)
          (grid-attrs grid) (make-array size :element-type 'attributes
                                             :initial-element 0)
          (grid-dirty grid) (make-array rows :element-type 'bit
                                             :initial-element 1)
          (grid-cursor-row grid) (min (grid-cursor-row grid) (max 0 (1- rows)))
          (grid-cursor-column grid) (min (grid-cursor-column grid)
                                         (max 0 (1- columns))))
    (clrhash (grid-combining grid))
    (setf (grid-selection grid) nil)
    grid))

(declaim (inline grid-index))
(defun grid-index (grid row column)
  (declare (type grid grid)
           (type grid-dimension row column))
  (the grid-index (+ (* row (grid-columns grid)) column)))

(declaim (inline grid-set-cell))
(defun grid-set-cell (grid index code fg bg attrs)
  (declare (type grid grid)
           (type grid-index index)
           (type codepoint code)
           (type color fg bg)
           (type attributes attrs)
           (optimize (speed 3) (safety 0)))
  (setf (aref (grid-codes grid) index) code
        (aref (grid-fg grid) index) fg
        (aref (grid-bg grid) index) bg
        (aref (grid-attrs grid) index) attrs)
  (values))

(defun grid-set-combining (grid index string)
  (if (and string (plusp (length string)))
      (setf (gethash index (grid-combining grid)) string)
      (remhash index (grid-combining grid))))

(defun grid-mark-all-dirty (grid)
  (fill (grid-dirty grid) 1)
  grid)

(defun grid-clear-dirty (grid)
  (fill (grid-dirty grid) 0)
  grid)

(declaim (inline grid-selected-p))
(defun grid-selected-p (grid index)
  (let ((selection (grid-selection grid)))
    (and selection (<= (car selection) index (cdr selection)))))

(defun grid-mark-selection-dirty (grid)
  (let ((selection (grid-selection grid))
        (columns (grid-columns grid)))
    (when selection
      (fill (grid-dirty grid) 1 :start (floor (car selection) columns)
                                :end (1+ (floor (cdr selection) columns))))))

(defun grid-set-selection (grid first last)
  "Select the cells from index FIRST to LAST (in either order), or nothing
when FIRST is NIL, marking the rows that change dirty."
  (grid-mark-selection-dirty grid)
  (setf (grid-selection grid) (and first (cons (min first last) (max first last))))
  (grid-mark-selection-dirty grid)
  grid)

(defun grid-selection-intersects-dirty-p (grid)
  "True when a selected row is dirty, i.e. its text may have changed."
  (let ((selection (grid-selection grid))
        (columns (grid-columns grid)))
    (and selection
         (find 1 (grid-dirty grid) :start (floor (car selection) columns)
                                   :end (1+ (floor (cdr selection) columns))))))

(defun grid-range-text (grid first last)
  "The text of cells FIRST to LAST: rows end in newlines, trailing blanks
are dropped, and the right halves of wide characters are skipped."
  (let ((columns (grid-columns grid)))
    (with-output-to-string (out)
      (loop for row from (floor first columns) to (floor last columns)
            for start = (max first (* row columns))
            for end = (min last (1- (* (1+ row) columns)))
            do (let ((line (with-output-to-string (line)
                             (loop for index from start to end
                                   for code = (aref (grid-codes grid) index)
                                   unless (logtest (aref (grid-attrs grid) index)
                                                   +attr-wide-spacer+)
                                     do (write-char (if (zerop code) #\Space (code-char code))
                                                    line)
                                        (let ((extra (gethash index (grid-combining grid))))
                                          (when extra (write-string extra line)))))))
                 (when (> row (floor first columns))
                   (terpri out))
                 (write-string (string-right-trim " " line) out))))))

(defun grid-selection-text (grid)
  "The selected text, or NIL when nothing is selected."
  (let ((selection (grid-selection grid)))
    (and selection (grid-range-text grid (car selection) (cdr selection)))))

(defun grid-dirty-rows (grid)
  (loop for bit across (grid-dirty grid)
        for row from 0
        when (= bit 1)
          collect row))

(defun grid-cell-string (grid index)
  "The text displayed at INDEX: empty for wide spacers, a space for blanks."
  (let ((code (aref (grid-codes grid) index))
        (attrs (aref (grid-attrs grid) index)))
    (cond
      ((logtest attrs +attr-wide-spacer+) "")
      ((or (zerop code) (logtest attrs +attr-invisible+)) " ")
      (t
       (let ((extra (gethash index (grid-combining grid))))
         (if extra
             (concatenate 'string (string (code-char code)) extra)
             (string (code-char code))))))))

(defun grid-row-text (grid row &key (trim t))
  "Row ROW as a string, one character per column (wide spacers omitted)."
  (let ((text (with-output-to-string (out)
                (dotimes (column (grid-columns grid))
                  (write-string (grid-cell-string grid (grid-index grid row column))
                                out)))))
    (if trim
        (string-right-trim " " text)
        text)))

(defun grid-text (grid &key (trim t))
  (loop for row below (grid-rows grid)
        collect (grid-row-text grid row :trim trim)))

;;; Palette

(deftype rgb () '(unsigned-byte 24))

(defun make-rgb (red green blue)
  (logior (ash red 16) (ash green 8) blue))

(defun rgb->hex (rgb)
  (format nil "#~6,'0X" rgb))

(defun hex->rgb (string)
  (let ((start (if (char= (char string 0) #\#) 1 0)))
    (parse-integer string :start start :radix 16)))

(defun xterm-palette-entry (index)
  "The xterm default color for palette INDEX."
  (cond
    ((< index 16)
     (aref #(#x000000 #xCD0000 #x00CD00 #xCDCD00 #x0000EE #xCD00CD #x00CDCD #xE5E5E5
             #x7F7F7F #xFF0000 #x00FF00 #xFFFF00 #x5C5CFF #xFF00FF #x00FFFF #xFFFFFF)
           index))
    ((< index 232)
     (flet ((level (n) (if (zerop n) 0 (+ 55 (* n 40)))))
       (let ((n (- index 16)))
         (make-rgb (level (floor n 36))
                   (level (mod (floor n 6) 6))
                   (level (mod n 6))))))
    (t
     (let ((level (+ 8 (* 10 (- index 232)))))
       (make-rgb level level level)))))

(defparameter *ansi-colors*
  '(:slt #(#x2B2F37 #xD77A7A #x9FBF7F #xE0C285 #x7AA6DA #xC397D8 #x70C0B1 #xC8CCD4
           #x5C6370 #xE88F8F #xB5D495 #xECD39B #x93B8E3 #xD2AEE3 #x8FD3C8 #xE6E9EF)
    :xterm #(#x000000 #xCD0000 #x00CD00 #xCDCD00 #x0000EE #xCD00CD #x00CDCD #xE5E5E5
             #x7F7F7F #xFF0000 #x00FF00 #xFFFF00 #x5C5CFF #xFF00FF #x00FFFF #xFFFFFF))
  "Named sets of the 16 ANSI colors.  :SLT is a soft, low-glare default.")

(defstruct (palette (:constructor %make-palette))
  (foreground #xC8CCD4 :type rgb)
  (background #x1C1F24 :type rgb)
  (cursor #xD8DEE9 :type rgb)
  ;; Background of selected cells.
  (selection #x3B4252 :type rgb)
  (colors (make-array 256 :element-type 'rgb) :type (simple-array rgb (256))))

(defun make-palette (&key (ansi :slt)
                       (foreground (if (eq ansi :xterm) #xE5E5E5 #xC8CCD4))
                       (background (if (eq ansi :xterm) #x000000 #x1C1F24))
                       (cursor (if (eq ansi :xterm) foreground #xD8DEE9))
                       (selection (if (eq ansi :xterm) #x4A4A4A #x3B4252))
                       overrides)
  "A palette.  ANSI names a set of the 16 base colors in *ANSI-COLORS* (or
is a vector of 16 RGB values); colors 16-255 are the xterm cube and gray
ramp.  OVERRIDES is an alist of (INDEX . RGB) applied last."
  (let ((colors (make-array 256 :element-type 'rgb))
        (base (if (vectorp ansi) ansi (getf *ansi-colors* ansi))))
    (unless (and base (= (length base) 16))
      (error "Unknown ANSI color set ~s." ansi))
    (dotimes (index 256)
      (setf (aref colors index)
            (if (< index 16) (aref base index) (xterm-palette-entry index))))
    (loop for (index . rgb) in overrides
          do (setf (aref colors index) rgb))
    (%make-palette :foreground foreground :background background
                   :cursor cursor :selection selection :colors colors)))

(defvar *palette* (make-palette)
  "The palette used to resolve grid colors.  Rebind or SETF to theme.")

(declaim (inline resolve-color))
(defun resolve-color (color palette defaultp-foreground)
  (declare (type color color)
           (type palette palette)
           (optimize (speed 3) (safety 0)))
  (case (color-tag color)
    (#.+color-tag-palette+ (aref (palette-colors palette) (ldb (byte 8 0) color)))
    (#.+color-tag-rgb+ (color-value color))
    (t (if defaultp-foreground
           (palette-foreground palette)
           (palette-background palette)))))

(declaim (inline blend-rgb))
(defun blend-rgb (a b)
  "The color halfway between RGB values A and B."
  (declare (type rgb a b))
  (flet ((channel (shift)
           (ash (floor (+ (ldb (byte 8 shift) a) (ldb (byte 8 shift) b)) 2) shift)))
    (logior (channel 16) (channel 8) (channel 0))))

(defun cell-rgb (grid index palette)
  "Return the resolved foreground and background RGB for the cell at INDEX,
taking bold-brightening, faint, inverse video and the cursor into account."
  (declare (type grid grid)
           (type grid-index index)
           (type palette palette)
           (optimize (speed 3)))
  (let* ((attrs (aref (grid-attrs grid) index))
         (fg-color (aref (grid-fg grid) index))
         (fg-color (if (and (logtest attrs +attr-bold+)
                            (color-palette-p fg-color)
                            (< (ldb (byte 8 0) fg-color) 8))
                       (palette-color (+ 8 (ldb (byte 8 0) fg-color)))
                       fg-color))
         (fg (resolve-color fg-color palette t))
         (bg (resolve-color (aref (grid-bg grid) index) palette nil)))
    (declare (type rgb fg bg))
    (when (logtest attrs +attr-faint+)
      (setf fg (blend-rgb fg bg)))
    (when (logtest attrs +attr-inverse+)
      (rotatef fg bg))
    (when (and (grid-cursor-visible-p grid)
               (eq (grid-cursor-shape grid) :block)
               (= index (grid-index grid (grid-cursor-row grid)
                                    (grid-cursor-column grid))))
      (setf bg (palette-cursor palette)
            fg (resolve-color (aref (grid-bg grid) index) palette nil)))
    (values fg bg)))
