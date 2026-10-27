(in-package #:lispvt)

;;;; The terminal: an xterm-compatible VT state machine (after Paul
;;;; Williams' DEC parser) driving a primary screen with scrollback and an
;;;; alternate screen.  Parser state lives in the terminal, so input may be
;;;; split anywhere, including inside UTF-8 sequences.  In the ground state
;;;; printable ASCII is consumed in whole runs (see scan.lisp).

(defconstant +ground+ 0)
(defconstant +escape+ 1)
(defconstant +escape-intermediate+ 2)
(defconstant +csi+ 3)
(defconstant +csi-intermediate+ 4)
(defconstant +csi-ignore+ 5)
(defconstant +osc+ 6)
(defconstant +control-string+ 7)          ; DCS, SOS, PM, APC: ignored

(defconstant +max-parameters+ 32)
(defconstant +max-osc-length+ 4096)

(defstruct (terminal (:constructor %make-terminal))
  (rows 24 :type dimension)
  (columns 80 :type dimension)
  (primary nil :type screen)
  (alternate nil :type screen)
  (screen nil :type screen)
  (alternate-p nil :type boolean)
  (scrollback 10000 :type fixnum)
  ;; Cursor and pen.
  (cursor-row 0 :type fixnum)
  (cursor-column 0 :type fixnum)
  (pending-wrap nil :type boolean)
  (fg 0 :type (unsigned-byte 32))
  (bg 0 :type (unsigned-byte 32))
  (attrs 0 :type (unsigned-byte 16))
  ;; Underline color (SGR 58), 0 for the foreground's.
  (ul 0 :type (unsigned-byte 32))
  (cursor-shape :block :type (member :block :underline :bar))
  ;; Whether the cursor blinks: T, NIL, or :DEFAULT where the program has
  ;; not said (DECSCUSR 0 or never), leaving it to the terminal's setting.
  (cursor-blink :default :type (member t nil :default))
  ;; Modes with behavior of their own; the rest live in MODES.
  (autowrap t :type boolean)
  (origin nil :type boolean)
  (insert nil :type boolean)
  (newline nil :type boolean)
  (cursor-shown t :type boolean)
  (modes (make-hash-table) :type hash-table)
  (scroll-top 0 :type fixnum)
  (scroll-bottom 23 :type fixnum)
  (tabs (make-array 0 :element-type 'bit) :type simple-bit-vector)
  ;; Character sets: G0, G1 (:ascii or :graphics) and which one is GL.
  (g0 :ascii :type keyword)
  (g1 :ascii :type keyword)
  (gl 0 :type bit)
  (last-char 32 :type (unsigned-byte 32))
  ;; Grapheme clusters (mode 2027, on by default): emoji sequences share a
  ;; cell.  The cluster last printed: its cell (row -1 when there is none,
  ;; or a control or escape sequence came since), where printing it left
  ;; the cursor, and what decides whether the next codepoint joins it.
  (graphemes t :type boolean)
  (cluster-row -1 :type fixnum)
  (cluster-column 0 :type fixnum)
  (cluster-after-row 0 :type fixnum)
  (cluster-after-column 0 :type fixnum)
  (cluster-after-wrap nil :type boolean)
  ;; An Extended_Pictographic codepoint and Extend characters so far, then
  ;; a ZWJ after them; and how many regional indicators.
  (cluster-pictographic nil :type boolean)
  (cluster-zwj nil :type boolean)
  (cluster-regional 0 :type fixnum)
  ;; One bit per displayed row whose contents changed.
  (dirty (make-array 0 :element-type 'bit) :type simple-bit-vector)
  ;; Rows of scrollback shown above the screen.
  (viewport-offset 0 :type fixnum)
  ;; Parser.
  (state +ground+ :type fixnum)
  (params (make-array +max-parameters+ :element-type 'fixnum)
   :type (simple-array fixnum (#.+max-parameters+)))
  (colons (make-array +max-parameters+ :element-type 'bit)
   :type (simple-bit-vector #.+max-parameters+))
  (param-count 0 :type fixnum)
  (param-value 0 :type fixnum)
  (param-digits nil :type boolean)
  (colon-next nil :type boolean)
  (private 0 :type (unsigned-byte 8))
  (intermediate 0 :type (unsigned-byte 8))
  (intermediate-count 0 :type fixnum)
  (osc (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (utf8-code 0 :type (unsigned-byte 32))
  (utf8-remaining 0 :type fixnum)
  ;; Callbacks and reported state.
  (output-function nil)
  (title-function nil)
  (bell-function nil)
  ;; Called with TOP, BOTTOM and COUNT whenever rows TOP..BOTTOM of the
  ;; current screen move: COUNT is positive for upward scrolls.  Resizing
  ;; also passes the screen, :PRIMARY or :ALTERNATE.
  (scroll-function nil)
  (title "" :type string))

(defun default-tabs (columns)
  (let ((tabs (make-array columns :element-type 'bit :initial-element 0)))
    (loop for column from 8 below columns by 8
          do (setf (sbit tabs column) 1))
    tabs))

(defun make-terminal (&key (rows 24) (columns 80) (scrollback 10000)
                        output-function title-function bell-function scroll-function)
  "A terminal of ROWS x COLUMNS keeping up to SCROLLBACK lines of history.
OUTPUT-FUNCTION is called with strings to send back to the application
(replies to queries), TITLE-FUNCTION with a new window title,
BELL-FUNCTION with no arguments, and SCROLL-FUNCTION with the TOP and
BOTTOM rows and COUNT (positive upward) of every scroll."
  (let* ((primary (make-screen rows columns scrollback))
         (term (%make-terminal :rows rows :columns columns :scrollback scrollback
                               :primary primary
                               :alternate (make-screen rows columns 0)
                               :screen primary
                               :output-function output-function
                               :title-function title-function
                               :bell-function bell-function
                               :scroll-function scroll-function)))
    (reset-state term)
    term))

(defun reset-state (term)
  (let ((rows (terminal-rows term))
        (columns (terminal-columns term)))
    (setf (terminal-primary term) (make-screen rows columns (terminal-scrollback term))
          (terminal-alternate term) (make-screen rows columns 0)
          (terminal-screen term) (terminal-primary term)
          (terminal-alternate-p term) nil
          (terminal-cursor-row term) 0
          (terminal-cursor-column term) 0
          (terminal-pending-wrap term) nil
          (terminal-fg term) 0
          (terminal-bg term) 0
          (terminal-attrs term) 0
          (terminal-ul term) 0
          (terminal-cursor-shape term) :block
          (terminal-autowrap term) t
          (terminal-origin term) nil
          (terminal-insert term) nil
          (terminal-newline term) nil
          (terminal-cursor-shown term) t
          (terminal-scroll-top term) 0
          (terminal-scroll-bottom term) (1- rows)
          (terminal-tabs term) (default-tabs columns)
          (terminal-g0 term) :ascii
          (terminal-g1 term) :ascii
          (terminal-gl term) 0
          (terminal-last-char term) 32
          (terminal-dirty term) (make-array rows :element-type 'bit :initial-element 1)
          (terminal-viewport-offset term) 0
          (terminal-state term) +ground+
          (terminal-utf8-remaining term) 0)
    (clrhash (terminal-modes term))
    (setf (terminal-graphemes term) t
          (terminal-cluster-row term) -1)
    term))

(defun terminal-reset (term)
  "Full reset (RIS)."
  (reset-state term))

;;; Callbacks

(defun emit-output (term string)
  (let ((function (terminal-output-function term)))
    (when function
      (funcall function string))))

;;; Dirty tracking.  Rows are dirtied in screen coordinates and shifted by
;;; the viewport offset when scrollback is shown.

(declaim (inline mark-row-dirty))
(defun mark-row-dirty (term row)
  (declare (type terminal term) (type fixnum row))
  (let ((shown (+ row (terminal-viewport-offset term))))
    (when (< shown (terminal-rows term))
      (setf (sbit (terminal-dirty term) shown) 1))))

(defun mark-rows-dirty (term top bottom)
  (declare (type terminal term) (type fixnum top bottom))
  (if (plusp (terminal-viewport-offset term))
      (fill (terminal-dirty term) 1)
      (fill (terminal-dirty term) 1 :start top :end (1+ bottom))))

(defun terminal-row-dirty-p (term row)
  (= 1 (sbit (terminal-dirty term) row)))

(defun terminal-clear-dirty (term)
  (fill (terminal-dirty term) 0)
  term)

;;; Lines

(declaim (inline current-line screen-line))
(defun screen-line (term row)
  (svref (screen-lines (terminal-screen term)) row))

(defun current-line (term)
  (screen-line term (terminal-cursor-row term)))

(defun terminal-line (term row)
  "The line displayed at ROW, taking the scrollback viewport into account."
  (let* ((screen (terminal-screen term))
         (offset (terminal-viewport-offset term)))
    (if (< row offset)
        (let* ((index (+ (- (screen-history-count screen) offset) row))
               (line (history-line screen index))
               (columns (terminal-columns term)))
          ;; History keeps the width it was written at; adjust on demand.
          (if (= (line-length line) columns)
              line
              (setf (svref (screen-history screen)
                           (mod (+ (screen-history-start screen) index)
                                (length (screen-history screen))))
                    (resize-line line columns))))
        (screen-line term (- row offset)))))

(defun terminal-history-count (term)
  (screen-history-count (terminal-screen term)))

;;; Scrolling

(defun note-scroll (term top bottom count &optional screen)
  (let ((function (terminal-scroll-function term)))
    (when (and function (/= count 0))
      (if screen
          (funcall function top bottom count screen)
          (funcall function top bottom count)))))

(defun scroll-up (term top bottom count &optional history)
  "Scroll rows TOP..BOTTOM up by COUNT.  With HISTORY, lines leaving the top
of the primary screen go to scrollback."
  (declare (type terminal term) (type fixnum top bottom count)
           (optimize speed))
  (let* ((screen (terminal-screen term))
         (lines (screen-lines screen))
         (columns (terminal-columns term))
         (bg (terminal-bg term))
         (keep (and history (zerop top) (not (terminal-alternate-p term))))
         (count (min count (1+ (- bottom top)))))
    (declare (type fixnum count))
    (dotimes (i count)
      (let ((line (svref lines top)))
        (replace lines lines :start1 top :start2 (1+ top) :end2 (1+ bottom))
        (let ((recycled (if keep
                            (let ((old (history-push screen line)))
                              (and old (= (line-length old) columns) old))
                            line)))
          (setf (svref lines bottom)
                (if recycled (clear-line recycled bg) (make-line columns bg))))))
    (note-scroll term top bottom count)
    (when (and keep (plusp (terminal-viewport-offset term)))
      (setf (terminal-viewport-offset term)
            (min (+ (terminal-viewport-offset term) count)
                 (screen-history-count screen))))
    (mark-rows-dirty term top bottom)))

(defun scroll-down (term top bottom count)
  (declare (type terminal term) (type fixnum top bottom count))
  (let ((lines (screen-lines (terminal-screen term)))
        (bg (terminal-bg term))
        (count (min count (1+ (- bottom top)))))
    (dotimes (i count)
      (let ((line (svref lines bottom)))
        (replace lines lines :start1 (1+ top) :start2 top :end2 bottom)
        (setf (svref lines top) (clear-line line bg))))
    (note-scroll term top bottom (- count))
    (mark-rows-dirty term top bottom)))

(defun index (term)
  "Move down a line, scrolling at the bottom margin (IND, LF)."
  (declare (type terminal term))
  (let ((row (terminal-cursor-row term)))
    (setf (terminal-pending-wrap term) nil)
    (cond ((= row (terminal-scroll-bottom term))
           (scroll-up term (terminal-scroll-top term) row 1 t))
          ((< row (1- (terminal-rows term)))
           (setf (terminal-cursor-row term) (1+ row))))))

(defun reverse-index (term)
  (let ((row (terminal-cursor-row term)))
    (setf (terminal-pending-wrap term) nil)
    (cond ((= row (terminal-scroll-top term))
           (scroll-down term row (terminal-scroll-bottom term) 1))
          ((plusp row)
           (setf (terminal-cursor-row term) (1- row))))))

(defun wrap (term)
  (setf (line-wrapped (current-line term)) t
        (terminal-cursor-column term) 0)
  (index term))

;;; Cursor motion

(defun set-cursor (term row column)
  (setf (terminal-cursor-row term) (max 0 (min row (1- (terminal-rows term))))
        (terminal-cursor-column term) (max 0 (min column (1- (terminal-columns term))))
        (terminal-pending-wrap term) nil))

(defun set-column (term column)
  (set-cursor term (terminal-cursor-row term) column))

(defun cursor-to (term row column)
  "CUP: ROW and COLUMN are 0-based, relative to the scroll region in
origin mode."
  (if (terminal-origin term)
      (set-cursor term
                  (min (+ row (terminal-scroll-top term)) (terminal-scroll-bottom term))
                  column)
      (set-cursor term row column)))

(defun cursor-up (term count)
  (let* ((row (terminal-cursor-row term))
         (top (if (>= row (terminal-scroll-top term)) (terminal-scroll-top term) 0)))
    (set-cursor term (max top (- row count)) (terminal-cursor-column term))))

(defun cursor-down (term count)
  (let* ((row (terminal-cursor-row term))
         (bottom (if (<= row (terminal-scroll-bottom term))
                     (terminal-scroll-bottom term)
                     (1- (terminal-rows term)))))
    (set-cursor term (min bottom (+ row count)) (terminal-cursor-column term))))

(defun tab-forward (term count)
  (let ((tabs (terminal-tabs term))
        (last (1- (terminal-columns term)))
        (column (terminal-cursor-column term)))
    (dotimes (i count)
      (setf column (or (position 1 tabs :start (min (1+ column) last)) last)))
    (setf (terminal-cursor-column term) column
          (terminal-pending-wrap term) nil)))

(defun tab-backward (term count)
  (let ((tabs (terminal-tabs term))
        (column (terminal-cursor-column term)))
    (dotimes (i count)
      (setf column (if (plusp column)
                       (or (position 1 tabs :end column :from-end t) 0)
                       0)))
    (set-column term column)))

;;; Writing cells

(declaim (inline clear-cut-wide-characters))
(defun clear-cut-wide-characters (line start end)
  "Before cells [START, END) of LINE are overwritten, blank the other half
of any wide character the range cuts."
  (declare (type line line) (type fixnum start end))
  (let ((attrs (line-attrs line))
        (codes (line-codes line)))
    (when (and (plusp start) (logtest (aref attrs start) +attr-wide-spacer+))
      (setf (aref codes (1- start)) 0
            (aref attrs (1- start)) 0))
    (when (and (< end (length attrs)) (logtest (aref attrs (1- end)) +attr-wide+))
      (setf (aref codes end) 0
            (aref attrs end) 0))))

(defun erase (term line start end)
  "Erase cells [START, END) of LINE with the current background, widened
to whole wide characters."
  (let* ((attrs (line-attrs line))
         (length (length attrs))
         (start (max 0 start))
         (end (min end length)))
    (when (< start end)
      (when (and (plusp start) (logtest (aref attrs start) +attr-wide-spacer+))
        (decf start))
      (when (and (< end length) (logtest (aref attrs end) +attr-wide-spacer+))
        (incf end))
      (erase-cells line start end (terminal-bg term)))))

(defun print-ascii (term octets start end)
  "Print the printable ASCII run OCTETS[START, END)."
  (declare (type terminal term)
           (type octets octets)
           (type index start end)
           (optimize speed (safety 0)))
  (when (or (terminal-insert term)
            (eq :graphics (if (zerop (terminal-gl term)) (terminal-g0 term) (terminal-g1 term))))
    (loop for i of-type index from start below end
          do (print-char term (aref octets i)))
    (return-from print-ascii))
  (let ((i start)
        (columns (terminal-columns term))
        (fg (terminal-fg term))
        (bg (terminal-bg term))
        (attrs (terminal-attrs term)))
    (declare (type index i) (type fixnum columns))
    (loop while (< i end)
          do (when (terminal-pending-wrap term)
               (wrap term))
             (let* ((column (terminal-cursor-column term))
                    (row (terminal-cursor-row term))
                    (line (screen-line term row))
                    (count (min (- columns column) (- end i)))
                    (stop (+ column count))
                    (codes (line-codes line)))
               (declare (type fixnum column row count stop))
               (clear-cut-wide-characters line column stop)
               (when (line-combining line)
                 (line-remove-combining line column stop))
               (loop for k of-type fixnum from column below stop
                     for j of-type index from i
                     do (setf (aref codes k) (aref octets j)))
               (fill (line-fg line) fg :start column :end stop)
               (fill (line-bg line) bg :start column :end stop)
               (fill (line-attrs line) attrs :start column :end stop)
               (note-extent line stop)
               (let ((ul (terminal-ul term)))
                 (when (or (/= ul 0) (line-ul line))
                   (fill (the color-vector (ensure-line-ul line)) ul :start column :end stop)))
               (mark-row-dirty term row)
               (incf i count)
               (if (>= stop columns)
                   (setf (terminal-cursor-column term) (1- columns)
                         (terminal-pending-wrap term) (terminal-autowrap term))
                   (setf (terminal-cursor-column term) stop))))
    (setf (terminal-last-char term) (aref octets (1- end)))
    ;; The last character can take marks and selectors (a keycap, 1 FE0F).
    (note-cluster term (terminal-cursor-row term)
                  (if (terminal-pending-wrap term)
                      (terminal-cursor-column term)
                      (1- (terminal-cursor-column term)))
                  (aref octets (1- end)))))

(defparameter *dec-special-graphics*
  #(#x00A0 #x25C6 #x2592 #x2409 #x240C #x240D #x240A #x00B0 #x00B1 #x2424
    #x240B #x2518 #x2510 #x250C #x2514 #x253C #x23BA #x23BB #x2500 #x23BC
    #x23BD #x251C #x2524 #x2534 #x252C #x2502 #x2264 #x2265 #x03C0 #x2260
    #x00A3 #x00B7)
  "Codepoints for #x5F through #x7E in the DEC special graphics set.")

(defun attach-combining (term code)
  (let* ((line (current-line term))
         (column (if (terminal-pending-wrap term)
                     (terminal-cursor-column term)
                     (1- (terminal-cursor-column term)))))
    (when (and (plusp column) (logtest (aref (line-attrs line) column) +attr-wide-spacer+))
      (decf column))
    (when (>= column 0)
      (let ((entry (assoc column (line-combining line)))
            (string (string (code-char code))))
        (if entry
            (setf (cdr entry) (concatenate 'string (cdr entry) string))
            (push (cons column string) (line-combining line))))
      (mark-row-dirty term (terminal-cursor-row term)))))

;;; Grapheme clusters

(defun note-cluster-end (term)
  (setf (terminal-cluster-after-row term) (terminal-cursor-row term)
        (terminal-cluster-after-column term) (terminal-cursor-column term)
        (terminal-cluster-after-wrap term) (terminal-pending-wrap term)))

(defun note-cluster (term row column code)
  "Remember that a cluster starting with CODE was printed at ROW, COLUMN."
  (setf (terminal-cluster-row term) row
        (terminal-cluster-column term) column
        (terminal-cluster-pictographic term) (extended-pictographic-p code)
        (terminal-cluster-zwj term) nil
        (terminal-cluster-regional term) (if (regional-indicator-p code) 1 0))
  (note-cluster-end term))

(defun joins-cluster-p (term code width)
  "Whether CODE, of WIDTH, extends the cluster just printed: Extend
characters (zero width), a pictograph after a ZWJ following one (GB11),
or a second regional indicator (GB12, GB13)."
  (and (terminal-graphemes term)
       (>= (terminal-cluster-row term) 0)
       (= (terminal-cluster-after-row term) (terminal-cursor-row term))
       (= (terminal-cluster-after-column term) (terminal-cursor-column term))
       (eq (terminal-cluster-after-wrap term) (terminal-pending-wrap term))
       (cond ((zerop width) t)
             ((extended-pictographic-p code) (terminal-cluster-zwj term))
             ((regional-indicator-p code) (= 1 (terminal-cluster-regional term)))
             (t nil))))

(defun widen-cluster (term line column)
  "Make the one-cell cluster at COLUMN of the cursor's LINE two cells wide,
as an emoji presentation selector or a flag asks, when the row has room."
  (let ((columns (terminal-columns term)))
    (when (< (1+ column) columns)
      (let ((next (1+ column)))
        (clear-cut-wide-characters line next (1+ next))
        (line-remove-combining line next (1+ next))
        (setf (aref (line-attrs line) column)
              (logior (aref (line-attrs line) column) +attr-wide+)
              (aref (line-codes line) next) 0
              (aref (line-fg line) next) (aref (line-fg line) column)
              (aref (line-bg line) next) (aref (line-bg line) column)
              (aref (line-attrs line) next)
              (logior (logandc2 (aref (line-attrs line) column) +attr-wide+)
                      +attr-wide-spacer+))
        (note-extent line (1+ next))
        (if (>= (1+ next) columns)
            (setf (terminal-cursor-column term) (1- columns)
                  (terminal-pending-wrap term) (terminal-autowrap term))
            (setf (terminal-cursor-column term) (1+ next)
                  (terminal-pending-wrap term) nil))))))

(defun join-cluster (term code)
  "Add CODE to the cluster just printed."
  (let* ((row (terminal-cluster-row term))
         (column (terminal-cluster-column term))
         (line (screen-line term row))
         (entry (assoc column (line-combining line)))
         (string (string (code-char code))))
    (if entry
        (setf (cdr entry) (concatenate 'string (cdr entry) string))
        (push (cons column string) (line-combining line)))
    (mark-row-dirty term row)
    (cond ((= code +zero-width-joiner+)
           (setf (terminal-cluster-zwj term) (terminal-cluster-pictographic term)))
          ((extended-pictographic-p code)
           (setf (terminal-cluster-pictographic term) t
                 (terminal-cluster-zwj term) nil))
          ((regional-indicator-p code)
           (incf (terminal-cluster-regional term)))
          (t (setf (terminal-cluster-zwj term) nil)))
    (when (and (or (= code +emoji-presentation+)
                   (= 2 (terminal-cluster-regional term)))
               (not (logtest (aref (line-attrs line) column) +attr-wide+)))
      (widen-cluster term line column))
    (note-cluster-end term)))

(defun print-char (term code)
  (declare (type terminal term) (type (unsigned-byte 32) code))
  (let ((width (char-width code))
        (columns (terminal-columns term)))
    (when (joins-cluster-p term code width)
      (join-cluster term code)
      (return-from print-char))
    (when (zerop width)
      (attach-combining term code)
      (return-from print-char))
    (when (and (<= #x5F code #x7E)
               (eq :graphics (if (zerop (terminal-gl term)) (terminal-g0 term) (terminal-g1 term))))
      (setf code (svref *dec-special-graphics* (- code #x5F))))
    (when (< columns 2)
      (setf width 1))
    (when (terminal-pending-wrap term)
      (wrap term))
    (when (and (= width 2) (= (terminal-cursor-column term) (1- columns)))
      (if (terminal-autowrap term)
          (progn (erase term (current-line term) (1- columns) columns)
                 (wrap term))
          (setf (terminal-cursor-column term) (- columns 2))))
    (let* ((column (terminal-cursor-column term))
           (line (current-line term)))
      (when (terminal-insert term)
        (insert-blanks term line column width))
      (clear-cut-wide-characters line column (+ column width))
      (line-remove-combining line column (+ column width))
      (let ((fg (terminal-fg term))
            (bg (terminal-bg term))
            (attrs (terminal-attrs term)))
        (setf (aref (line-codes line) column) code
              (aref (line-fg line) column) fg
              (aref (line-bg line) column) bg
              (aref (line-attrs line) column) (if (= width 2) (logior attrs +attr-wide+) attrs))
        (when (= width 2)
          (setf (aref (line-codes line) (1+ column)) 0
                (aref (line-fg line) (1+ column)) fg
                (aref (line-bg line) (1+ column)) bg
                (aref (line-attrs line) (1+ column)) (logior attrs +attr-wide-spacer+)))
        (note-extent line (+ column width))
        (let ((ul (terminal-ul term)))
          (when (or (/= ul 0) (line-ul line))
            (fill (ensure-line-ul line) ul :start column :end (+ column width)))))
      (mark-row-dirty term (terminal-cursor-row term))
      (setf (terminal-last-char term) code)
      (let ((next (+ column width)))
        (if (>= next columns)
            (setf (terminal-cursor-column term) (1- columns)
                  (terminal-pending-wrap term) (terminal-autowrap term))
            (setf (terminal-cursor-column term) next)))
      (note-cluster term (terminal-cursor-row term) column code))))

(defun shift-combining (line start delta limit)
  "Move combining marks at columns >= START by DELTA, dropping those that
end up outside [START, LIMIT)."
  (when (line-combining line)
    (setf (line-combining line)
          (loop for (column . string) in (line-combining line)
                for moved = (if (>= column start) (+ column delta) column)
                when (or (< column start) (and (>= moved start) (< moved limit)))
                  collect (cons moved string)))))

(defun insert-blanks (term line column count)
  "ICH: shift cells from COLUMN right by COUNT, blanking the gap."
  (let* ((columns (length (line-codes line)))
         (count (min count (- columns column))))
    (when (and (plusp column) (logtest (aref (line-attrs line) column) +attr-wide-spacer+))
      (erase-cells line (1- column) (1+ column) (terminal-bg term)))
    (shift-combining line column count columns)
    (note-extent line columns)
    (dolist (vector (list* (line-codes line) (line-fg line) (line-bg line) (line-attrs line)
                           (and (line-ul line) (list (line-ul line)))))
      (replace vector vector :start1 (+ column count) :start2 column :end2 (- columns count)))
    (when (logtest (aref (line-attrs line) (1- columns)) +attr-wide+)
      (erase-cells line (1- columns) columns (terminal-bg term)))
    (erase-cells line column (+ column count) (terminal-bg term))))

(defun delete-chars (term line column count)
  "DCH: delete COUNT cells at COLUMN, shifting the rest left."
  (let* ((columns (length (line-codes line)))
         (count (min count (- columns column))))
    (when (and (plusp column) (logtest (aref (line-attrs line) column) +attr-wide-spacer+))
      (erase-cells line (1- column) (1+ column) (terminal-bg term)))
    (line-remove-combining line column (+ column count))
    (shift-combining line (+ column count) (- count) columns)
    (dolist (vector (list* (line-codes line) (line-fg line) (line-bg line) (line-attrs line)
                           (and (line-ul line) (list (line-ul line)))))
      (replace vector vector :start1 column :start2 (+ column count)))
    (when (logtest (aref (line-attrs line) column) +attr-wide-spacer+)
      (erase-cells line column (1+ column) (terminal-bg term)))
    (erase-cells line (- columns count) columns (terminal-bg term))))

;;; Control characters

(defun execute-control (term byte)
  (declare (type terminal term) (type (unsigned-byte 8) byte))
  (setf (terminal-cluster-row term) -1)
  (case byte
    (7 (let ((function (terminal-bell-function term)))
         (when function (funcall function))))
    (8 (setf (terminal-pending-wrap term) nil)
       (when (plusp (terminal-cursor-column term))
         (decf (terminal-cursor-column term))))
    (9 (tab-forward term 1))
    ((10 11 12)
     (index term)
     (when (terminal-newline term)
       (setf (terminal-cursor-column term) 0)))
    (13 (setf (terminal-cursor-column term) 0
              (terminal-pending-wrap term) nil))
    (14 (setf (terminal-gl term) 1))
    (15 (setf (terminal-gl term) 0))))

;;; Saving the cursor

(defun save-cursor (term)
  (setf (screen-saved-cursor (terminal-screen term))
        (vector (terminal-cursor-row term) (terminal-cursor-column term)
                (terminal-pending-wrap term)
                (terminal-fg term) (terminal-bg term) (terminal-attrs term)
                (terminal-origin term)
                (terminal-g0 term) (terminal-g1 term) (terminal-gl term)
                (terminal-ul term))))

(defun restore-cursor (term)
  (let ((saved (screen-saved-cursor (terminal-screen term))))
    (if saved
        (progn
          (set-cursor term (svref saved 0) (svref saved 1))
          (setf (terminal-pending-wrap term) (and (svref saved 2)
                                                  (= (terminal-cursor-column term)
                                                     (svref saved 1)))
                (terminal-fg term) (svref saved 3)
                (terminal-bg term) (svref saved 4)
                (terminal-attrs term) (svref saved 5)
                (terminal-origin term) (svref saved 6)
                (terminal-g0 term) (svref saved 7)
                (terminal-g1 term) (svref saved 8)
                (terminal-gl term) (svref saved 9)
                (terminal-ul term) (svref saved 10)))
        (progn
          (set-cursor term 0 0)
          (setf (terminal-fg term) 0 (terminal-bg term) 0 (terminal-attrs term) 0
                (terminal-ul term) 0 (terminal-origin term) nil)))))

;;; Escape sequences

(defun esc-dispatch (term byte)
  (declare (type terminal term) (type (unsigned-byte 8) byte))
  (setf (terminal-state term) +ground+
        (terminal-cluster-row term) -1)
  (if (zerop (terminal-intermediate-count term))
      (case (code-char byte)
        (#\[ (setf (terminal-state term) +csi+
                   (terminal-param-count term) 0
                   (terminal-param-value term) 0
                   (terminal-param-digits term) nil
                   (terminal-colon-next term) nil
                   (terminal-private term) 0
                   (terminal-intermediate term) 0
                   (terminal-intermediate-count term) 0))
        (#\] (setf (terminal-state term) +osc+
                   (fill-pointer (terminal-osc term)) 0))
        ((#\P #\X #\^ #\_) (setf (terminal-state term) +control-string+))
        (#\7 (save-cursor term))
        (#\8 (restore-cursor term))
        (#\D (index term))
        (#\E (index term)
         (setf (terminal-cursor-column term) 0))
        (#\H (setf (sbit (terminal-tabs term) (terminal-cursor-column term)) 1))
        (#\M (reverse-index term))
        (#\c (reset-state term))
        (#\= (setf (gethash 66 (terminal-modes term)) t))
        (#\> (remhash 66 (terminal-modes term))))
      (let ((intermediate (code-char (terminal-intermediate term)))
            (final (code-char byte)))
        (case intermediate
          (#\# (when (char= final #\8) (screen-alignment term)))
          (#\( (setf (terminal-g0 term) (if (char= final #\0) :graphics :ascii)))
          (#\) (setf (terminal-g1 term) (if (char= final #\0) :graphics :ascii)))))))

(defun screen-alignment (term)
  "DECALN: fill the screen with E."
  (loop for line across (screen-lines (terminal-screen term))
        do (clear-line line 0)
           (fill (line-codes line) (char-code #\E))
           (note-extent line (line-length line)))
  (setf (terminal-scroll-top term) 0
        (terminal-scroll-bottom term) (1- (terminal-rows term)))
  (set-cursor term 0 0)
  (mark-rows-dirty term 0 (1- (terminal-rows term))))

;;; CSI

(declaim (inline param))
(defun param (term index default)
  (declare (type terminal term) (type fixnum index default))
  (if (< index (terminal-param-count term))
      (let ((value (aref (terminal-params term) index)))
        (if (zerop value) default value))
      default))

(defun push-param (term)
  (declare (type terminal term))
  (let ((count (terminal-param-count term)))
    (when (< count +max-parameters+)
      (setf (aref (terminal-params term) count) (terminal-param-value term)
            (sbit (terminal-colons term) count) (if (terminal-colon-next term) 1 0)
            (terminal-param-count term) (1+ count))))
  (setf (terminal-param-value term) 0
        (terminal-param-digits term) nil
        (terminal-colon-next term) nil))

(defun erase-display (term how)
  (let* ((row (terminal-cursor-row term))
         (column (terminal-cursor-column term))
         (rows (terminal-rows term))
         (bg (terminal-bg term)))
    (setf (terminal-pending-wrap term) nil)
    (flet ((clear-rows (start end)
             (loop for r from start below end
                   do (clear-line (screen-line term r) bg))))
      (case how
        (0 (erase term (current-line term) column (terminal-columns term))
         (setf (line-wrapped (current-line term)) nil)
         (clear-rows (1+ row) rows)
         (mark-rows-dirty term row (1- rows)))
        (1 (erase term (current-line term) 0 (1+ column))
         (clear-rows 0 row)
         (mark-rows-dirty term 0 row))
        (2 (clear-rows 0 rows)
         (mark-rows-dirty term 0 (1- rows)))
        (3 (unless (terminal-alternate-p term)
             (clear-history (terminal-primary term))
             (when (plusp (terminal-viewport-offset term))
               (setf (terminal-viewport-offset term) 0)
               (mark-rows-dirty term 0 (1- rows)))))))))

(defun erase-line (term how)
  (let ((line (current-line term))
        (column (terminal-cursor-column term)))
    (setf (terminal-pending-wrap term) nil)
    (case how
      (0 (erase term line column (terminal-columns term))
       (setf (line-wrapped line) nil))
      (1 (erase term line 0 (1+ column)))
      (2 (erase term line 0 (terminal-columns term))
       (setf (line-wrapped line) nil)))
    (mark-row-dirty term (terminal-cursor-row term))))

(defun extended-color (term index)
  "Parse the color starting at parameter INDEX (the 38, 48 or 58).
Return the color or NIL, and the number of parameters consumed."
  (let* ((params (terminal-params term))
         (colons (terminal-colons term))
         (count (terminal-param-count term))
         (next (1+ index)))
    (flet ((byte-at (i) (if (< i count) (min 255 (aref params i)) 0)))
      (cond
        ((>= next count) (values nil 1))
        ((= 1 (sbit colons next))
         ;; Colon form: 38:5:N or 38:2:[colorspace:]R:G:B
         (let ((end next))
           (loop while (and (< end count) (= 1 (sbit colons end)))
                 do (incf end))
           (let ((kind (aref params next))
                 (subparams (- end next)))
             (values (case kind
                       (5 (palette-color (byte-at (+ next 1))))
                       (2 (let ((base (if (>= subparams 5) (+ next 2) (+ next 1))))
                            (rgb-color (byte-at base) (byte-at (+ base 1)) (byte-at (+ base 2))))))
                     (- end index)))))
        ((= (aref params next) 5)
         (values (palette-color (byte-at (+ next 1))) 3))
        ((= (aref params next) 2)
         (values (rgb-color (byte-at (+ next 1)) (byte-at (+ next 2)) (byte-at (+ next 3)))
                 5))
        (t (values nil 2))))))

(defun select-graphic-rendition (term)
  (declare (type terminal term))
  (let ((params (terminal-params term))
        (count (terminal-param-count term))
        (i 0))
    (declare (type fixnum count i))
    (when (zerop count)
      (setf (terminal-fg term) 0 (terminal-bg term) 0 (terminal-attrs term) 0
            (terminal-ul term) 0))
    (macrolet ((set-attr (mask) `(setf (terminal-attrs term) (logior (terminal-attrs term) ,mask)))
               (clear-attr (mask) `(setf (terminal-attrs term)
                                         (logandc2 (terminal-attrs term) ,mask))))
      (loop while (< i count)
            do (let ((p (aref params i)))
                 (incf i)
                 (case p
                   (0 (setf (terminal-fg term) 0 (terminal-bg term) 0 (terminal-attrs term) 0
                            (terminal-ul term) 0))
                   (1 (set-attr +attr-bold+))
                   (2 (set-attr +attr-faint+))
                   (3 (set-attr +attr-italic+))
                   (4 (if (and (< i count) (= 1 (sbit (terminal-colons term) i)))
                          (let ((style (aref params i)))
                            (incf i)
                            (clear-attr +attr-underlines+)
                            (case style
                              (0)
                              (2 (set-attr +attr-double-underline+))
                              (3 (set-attr +attr-curly-underline+))
                              (4 (set-attr +attr-dotted-underline+))
                              (5 (set-attr +attr-dashed-underline+))
                              (t (set-attr +attr-underline+))))
                          (progn (clear-attr +attr-underlines+)
                                 (set-attr +attr-underline+))))
                   ((5 6) (set-attr +attr-blink+))
                   (7 (set-attr +attr-inverse+))
                   (8 (set-attr +attr-invisible+))
                   (9 (set-attr +attr-strike+))
                   (21 (clear-attr +attr-underlines+)
                    (set-attr +attr-double-underline+))
                   (22 (clear-attr (logior +attr-bold+ +attr-faint+)))
                   (23 (clear-attr +attr-italic+))
                   (24 (clear-attr +attr-underlines+))
                   (25 (clear-attr +attr-blink+))
                   (27 (clear-attr +attr-inverse+))
                   (28 (clear-attr +attr-invisible+))
                   (29 (clear-attr +attr-strike+))
                   ((30 31 32 33 34 35 36 37) (setf (terminal-fg term) (palette-color (- p 30))))
                   ((40 41 42 43 44 45 46 47) (setf (terminal-bg term) (palette-color (- p 40))))
                   ((90 91 92 93 94 95 96 97) (setf (terminal-fg term) (palette-color (- p 82))))
                   ((100 101 102 103 104 105 106 107)
                    (setf (terminal-bg term) (palette-color (- p 92))))
                   (39 (setf (terminal-fg term) 0))
                   (49 (setf (terminal-bg term) 0))
                   (59 (setf (terminal-ul term) 0))
                   ((38 48 58)
                    (multiple-value-bind (color consumed) (extended-color term (1- i))
                      (incf i (1- consumed))
                      (when color
                        (case p
                          (38 (setf (terminal-fg term) color))
                          (48 (setf (terminal-bg term) color))
                          (58 (setf (terminal-ul term) color)))))))))))
  (values))

(defun set-alternate-screen (term on &key clear)
  (unless (eq on (terminal-alternate-p term))
    (setf (terminal-alternate-p term) on
          (terminal-screen term) (if on (terminal-alternate term) (terminal-primary term))
          (terminal-viewport-offset term) 0)
    (when (and on clear)
      (loop for line across (screen-lines (terminal-alternate term))
            do (clear-line line (terminal-bg term))))
    (mark-rows-dirty term 0 (1- (terminal-rows term)))))

(defun set-dec-mode (term mode on)
  (case mode
    (12 (setf (terminal-cursor-blink term) on))
    (2027 (setf (terminal-graphemes term) on))
    (6 (setf (terminal-origin term) on)
     (cursor-to term 0 0))
    (7 (setf (terminal-autowrap term) on))
    (25 (setf (terminal-cursor-shown term) on))
    ((47 1047) (set-alternate-screen term on :clear t))
    (1048 (if on (save-cursor term) (restore-cursor term)))
    (1049 (if on
              (progn (save-cursor term)
                     (set-alternate-screen term t :clear t))
              (progn (set-alternate-screen term nil)
                     (restore-cursor term)))))
  (if on
      (setf (gethash mode (terminal-modes term)) t)
      (remhash mode (terminal-modes term))))

(defun dec-mode-set-p (term mode)
  (case mode
    (6 (terminal-origin term))
    (7 (terminal-autowrap term))
    (25 (terminal-cursor-shown term))
    ((47 1047 1049) (terminal-alternate-p term))
    (2027 (terminal-graphemes term))
    (t (values (gethash mode (terminal-modes term))))))

(defun sgr-color-parameters (color base)
  "SGR parameters selecting COLOR (see PALETTE-COLOR) as the foreground
(BASE 30), background (40) or underline color (50), or NIL for the
default."
  (let ((tag (ldb (byte 8 24) color))
        (value (ldb (byte 24 0) color)))
    (case tag
      (1 (cond ((and (< value 8) (/= base 50)) (list (+ base value)))
               ((and (< value 16) (/= base 50)) (list (+ base 60 (- value 8))))
               (t (list (+ base 8) 5 value))))
      (2 (list (+ base 8) 2 (ldb (byte 8 16) value) (ldb (byte 8 8) value)
               (ldb (byte 8 0) value))))))

(defun terminal-report-setting (term request)
  "The value of the setting DECRQSS REQUEST names, as it goes before
REQUEST in the answer: \"m\" the SGR attributes, \"r\" the scroll
margins, \" q\" the cursor style.  NIL for others."
  (cond
    ((string= request "m")
     (let ((attrs (terminal-attrs term)))
       (format nil "~{~a~^;~}"
               (append
                '(0)
                (loop for (mask code) in `((,+attr-bold+ 1) (,+attr-faint+ 2) (,+attr-italic+ 3)
                                           (,+attr-blink+ 5) (,+attr-inverse+ 7)
                                           (,+attr-invisible+ 8) (,+attr-strike+ 9))
                      when (logtest attrs mask) collect code)
                (cond ((logtest attrs +attr-double-underline+) '("4:2"))
                      ((logtest attrs +attr-curly-underline+) '("4:3"))
                      ((logtest attrs +attr-dotted-underline+) '("4:4"))
                      ((logtest attrs +attr-dashed-underline+) '("4:5"))
                      ((logtest attrs +attr-underline+) '(4)))
                (sgr-color-parameters (terminal-fg term) 30)
                (sgr-color-parameters (terminal-bg term) 40)
                (sgr-color-parameters (terminal-ul term) 50)))))
    ((string= request "r")
     (format nil "~d;~d" (1+ (terminal-scroll-top term)) (1+ (terminal-scroll-bottom term))))
    ((string= request " q")
     (format nil "~d"
             (+ (ecase (terminal-cursor-shape term) (:block 1) (:underline 3) (:bar 5))
                (if (eq (terminal-cursor-blink term) t) 0 1))))))

(defun soft-reset (term)
  "DECSTR."
  (setf (terminal-cursor-shown term) t
        (terminal-origin term) nil
        (terminal-autowrap term) t
        (terminal-insert term) nil
        (terminal-scroll-top term) 0
        (terminal-scroll-bottom term) (1- (terminal-rows term))
        (terminal-fg term) 0 (terminal-bg term) 0 (terminal-attrs term) 0 (terminal-ul term) 0
        (terminal-g0 term) :ascii (terminal-g1 term) :ascii (terminal-gl term) 0
        (screen-saved-cursor (terminal-screen term)) nil
        (terminal-pending-wrap term) nil)
  (remhash 1 (terminal-modes term))
  (remhash 66 (terminal-modes term)))

(defun report-cursor (term prefix)
  (emit-output term (format nil "~c[~a~d;~dR" #\Esc prefix
                            (1+ (- (terminal-cursor-row term)
                                   (if (terminal-origin term) (terminal-scroll-top term) 0)))
                            (1+ (terminal-cursor-column term)))))

(defun csi-dispatch (term byte)
  (declare (type terminal term) (type (unsigned-byte 8) byte))
  (setf (terminal-cluster-row term) -1)
  (when (or (terminal-param-digits term) (plusp (terminal-param-count term)))
    (push-param term))
  (let ((final (code-char byte))
        (private (code-char (terminal-private term)))
        (intermediate (if (zerop (terminal-intermediate-count term))
                          nil
                          (code-char (terminal-intermediate term))))
        (row (terminal-cursor-row term))
        (column (terminal-cursor-column term)))
    (cond
      ((> (terminal-intermediate-count term) 1))
      ((and intermediate (char/= private #\Nul))
       (when (and (char= private #\?) (char= intermediate #\$) (char= final #\p))
         (let ((mode (param term 0 0)))
           (emit-output term (format nil "~c[?~d;~d$y" #\Esc mode
                                     (if (dec-mode-set-p term mode) 1 2))))))
      (intermediate
       (case intermediate
         (#\Space (when (char= final #\q)
                    (let ((style (param term 0 0)))
                      (setf (terminal-cursor-shape term)
                            (case style
                              ((3 4) :underline)
                              ((5 6) :bar)
                              (t :block))
                            (terminal-cursor-blink term)
                            (cond ((zerop style) :default)
                                  ((oddp style) t)
                                  (t nil))))))
         (#\! (when (char= final #\p) (soft-reset term)))))
      ((char= private #\?)
       (case final
         ((#\h #\l)
          (dotimes (i (terminal-param-count term))
            (set-dec-mode term (aref (terminal-params term) i) (char= final #\h))))
         (#\n (when (= (param term 0 0) 6) (report-cursor term "?")))
         (#\J (erase-display term (param term 0 0)))
         (#\K (erase-line term (param term 0 0)))))
      ((char= private #\>)
       (when (char= final #\c)
         (emit-output term (format nil "~c[>1;10;0c" #\Esc))))
      ((char/= private #\Nul))
      (t
       (case final
         (#\@ (insert-blanks term (current-line term) column (param term 0 1))
          (setf (terminal-pending-wrap term) nil)
          (mark-row-dirty term row))
         (#\A (cursor-up term (param term 0 1)))
         ((#\B #\e) (cursor-down term (param term 0 1)))
         ((#\C #\a) (set-column term (+ column (param term 0 1))))
         (#\D (set-column term (- column (param term 0 1))))
         (#\E (cursor-down term (param term 0 1)) (set-column term 0))
         (#\F (cursor-up term (param term 0 1)) (set-column term 0))
         ((#\G #\`) (set-column term (1- (param term 0 1))))
         ((#\H #\f) (cursor-to term (1- (param term 0 1)) (1- (param term 1 1))))
         (#\I (tab-forward term (param term 0 1)))
         (#\J (erase-display term (param term 0 0)))
         (#\K (erase-line term (param term 0 0)))
         (#\L (when (<= (terminal-scroll-top term) row (terminal-scroll-bottom term))
                (scroll-down term row (terminal-scroll-bottom term) (param term 0 1))
                (set-column term 0)))
         (#\M (when (<= (terminal-scroll-top term) row (terminal-scroll-bottom term))
                (scroll-up term row (terminal-scroll-bottom term) (param term 0 1))
                (set-column term 0)))
         (#\P (delete-chars term (current-line term) column (param term 0 1))
          (setf (terminal-pending-wrap term) nil)
          (mark-row-dirty term row))
         (#\S (scroll-up term (terminal-scroll-top term) (terminal-scroll-bottom term)
                         (param term 0 1)))
         (#\T (scroll-down term (terminal-scroll-top term) (terminal-scroll-bottom term)
                           (param term 0 1)))
         (#\X (erase term (current-line term) column (+ column (param term 0 1)))
          (setf (terminal-pending-wrap term) nil)
          (mark-row-dirty term row))
         (#\Z (tab-backward term (param term 0 1)))
         (#\b (let ((char (terminal-last-char term)))
                (dotimes (i (min (param term 0 1) 65535))
                  (print-char term char))))
         (#\c (when (zerop (param term 0 0))
                (emit-output term (format nil "~c[?62;22c" #\Esc))))
         (#\d (cursor-to term (1- (param term 0 1)) column))
         (#\g (case (param term 0 0)
                (0 (setf (sbit (terminal-tabs term) column) 0))
                (3 (fill (terminal-tabs term) 0))))
         ((#\h #\l)
          (let ((on (char= final #\h)))
            (dotimes (i (terminal-param-count term))
              (case (aref (terminal-params term) i)
                (4 (setf (terminal-insert term) on))
                (20 (setf (terminal-newline term) on))))))
         (#\m (select-graphic-rendition term))
         (#\n (case (param term 0 0)
                (5 (emit-output term (format nil "~c[0n" #\Esc)))
                (6 (report-cursor term ""))))
         (#\r (let ((top (1- (param term 0 1)))
                    (bottom (1- (min (param term 1 (terminal-rows term)) (terminal-rows term)))))
                (when (< top bottom)
                  (setf (terminal-scroll-top term) top
                        (terminal-scroll-bottom term) bottom)
                  (cursor-to term 0 0))))
         (#\s (save-cursor term))
         (#\u (restore-cursor term)))))))

;;; OSC

(defun osc-dispatch (term)
  (let* ((data (terminal-osc term))
         (separator (position (char-code #\;) data)))
    (when separator
      (let ((command (parse-integer (map 'string #'code-char (subseq data 0 separator))
                                    :junk-allowed t)))
        (when (member command '(0 2))
          (let ((title (sb-ext:octets-to-string
                        (coerce (subseq data (1+ separator)) 'octets)
                        :external-format '(:utf-8 :replacement #\?))))
            (setf (terminal-title term) title)
            (let ((function (terminal-title-function term)))
              (when function (funcall function title)))))))))

;;; The state machine

(defun process-byte (term byte)
  (declare (type terminal term)
           (type (unsigned-byte 8) byte)
           (optimize speed))
  ;; Finish or abandon a UTF-8 sequence.
  (let ((remaining (terminal-utf8-remaining term)))
    (when (plusp remaining)
      (if (= (logand byte #xC0) #x80)
          (let ((code (logior (ash (terminal-utf8-code term) 6) (logand byte #x3F))))
            (setf (terminal-utf8-code term) code
                  (terminal-utf8-remaining term) (1- remaining))
            (when (= remaining 1)
              (print-char term (if (or (<= #xD800 code #xDFFF) (> code #x10FFFF))
                                   #xFFFD
                                   code)))
            (return-from process-byte))
          (progn (setf (terminal-utf8-remaining term) 0)
                 (print-char term #xFFFD)))))
  (let ((state (terminal-state term)))
    (declare (type fixnum state))
    ;; Controls valid in every state.
    (when (< byte #x20)
      (case byte
        ((#x18 #x1A) (setf (terminal-state term) +ground+))
        (#x1B (when (= state +osc+) (osc-dispatch term))
         (setf (terminal-state term) +escape+
               (terminal-intermediate-count term) 0))
        (#x07 (if (= state +osc+)
                  (progn (osc-dispatch term)
                         (setf (terminal-state term) +ground+))
                  (unless (= state +control-string+)
                    (execute-control term byte))))
        (t (unless (or (= state +osc+) (= state +control-string+))
             (execute-control term byte))))
      (return-from process-byte))
    (cond
      ((= state +ground+)
       (cond ((< byte #x7F) (print-char term byte))
             ((= byte #x7F))
             ((<= #xC2 byte #xDF)
              (setf (terminal-utf8-code term) (logand byte #x1F)
                    (terminal-utf8-remaining term) 1))
             ((<= #xE0 byte #xEF)
              (setf (terminal-utf8-code term) (logand byte #x0F)
                    (terminal-utf8-remaining term) 2))
             ((<= #xF0 byte #xF4)
              (setf (terminal-utf8-code term) (logand byte #x07)
                    (terminal-utf8-remaining term) 3))
             (t (print-char term #xFFFD))))
      ((or (= state +escape+) (= state +escape-intermediate+))
       (cond ((<= #x20 byte #x2F)
              (when (zerop (terminal-intermediate-count term))
                (setf (terminal-intermediate term) byte))
              (incf (terminal-intermediate-count term))
              (setf (terminal-state term) +escape-intermediate+))
             ((<= #x30 byte #x7E) (esc-dispatch term byte))
             ((> byte #x7F) (setf (terminal-state term) +ground+))))
      ((= state +csi+)
       (cond ((<= #x30 byte #x39)
              (setf (terminal-param-value term)
                    (min 65535 (+ (* 10 (terminal-param-value term)) (- byte #x30)))
                    (terminal-param-digits term) t))
             ((= byte #x3B) (push-param term))
             ((= byte #x3A) (push-param term)
              (setf (terminal-colon-next term) t))
             ((<= #x3C byte #x3F)
              (if (and (zerop (terminal-param-count term))
                       (not (terminal-param-digits term))
                       (zerop (terminal-private term)))
                  (setf (terminal-private term) byte)
                  (setf (terminal-state term) +csi-ignore+)))
             ((<= #x20 byte #x2F)
              (setf (terminal-intermediate term) byte
                    (terminal-intermediate-count term) 1
                    (terminal-state term) +csi-intermediate+))
             ((<= #x40 byte #x7E)
              (setf (terminal-state term) +ground+)
              (csi-dispatch term byte))))
      ((= state +csi-intermediate+)
       (cond ((<= #x20 byte #x2F) (incf (terminal-intermediate-count term)))
             ((<= #x30 byte #x3F) (setf (terminal-state term) +csi-ignore+))
             ((<= #x40 byte #x7E)
              (setf (terminal-state term) +ground+)
              (csi-dispatch term byte))))
      ((= state +csi-ignore+)
       (when (<= #x40 byte #x7E)
         (setf (terminal-state term) +ground+)))
      ((= state +osc+)
       (let ((data (terminal-osc term)))
         (when (< (fill-pointer data) +max-osc-length+)
           (vector-push-extend byte data))))))
  (values))

(defun terminal-feed (term octets &key (start 0) (end (length octets)))
  "Process OCTETS[START, END), output from the application."
  (declare (type terminal term))
  (unless (typep octets 'octets)
    (setf octets (coerce (subseq octets start end) 'octets)
          end (- end start)
          start 0))
  (let ((i start))
    (declare (type index i end)
             (type octets octets)
             (optimize speed (safety 0)))
    (loop while (< i end)
          do (if (and (= (terminal-state term) +ground+)
                      (zerop (terminal-utf8-remaining term)))
                 (let ((run (scan-printable octets i end)))
                   (declare (type index run))
                   (if (< i run)
                       (progn (print-ascii term octets i run)
                              (setf i run))
                       (progn (process-byte term (aref octets i))
                              (incf i))))
                 (progn (process-byte term (aref octets i))
                        (incf i)))))
  term)

(defun terminal-feed-string (term string)
  (terminal-feed term (sb-ext:string-to-octets string :external-format :utf-8)))

;;; Resizing

(defun resize-screen (screen rows columns shift keep-history)
  "Resize SCREEN to ROWS x COLUMNS, first scrolling SHIFT lines off the top
(into scrollback with KEEP-HISTORY)."
  (let* ((old (screen-lines screen))
         (new (make-array rows)))
    (dotimes (i shift)
      (when keep-history
        (history-push screen (svref old i))))
    (dotimes (row rows)
      (let ((source (+ row shift)))
        (setf (svref new row)
              (if (< source (length old))
                  (resize-line (svref old source) columns)
                  (make-line columns)))))
    (setf (screen-lines screen) new)))

;;; Reflow.  When the width changes, the main screen's text is rewrapped:
;;; rows that autowrap joined (LINE-WRAPPED) are logical lines again, laid
;;; out at the new width, scrollback included.  Full-screen programs on the
;;; alternate screen redraw themselves, so it is only resized.

(defun wrap-segments (segments columns cursor)
  "Lay out a logical line, SEGMENTS of (LINE START END) cells in order, in
lines COLUMNS wide, all but the last marked wrapped; runs of cells are
copied whole.  A wide character that would straddle the edge starts the next
line.  CURSOR is an offset into the logical line, or NIL.  Return the lines
and, for CURSOR, (LINE-INDEX COLUMN WIDTH) of its cell, or of the place after
the text it is past."
  (let* ((lines (list (make-line columns)))
         (column 0)
         (offset 0)
         (place nil))
    (flet ((new-line ()
             (setf (line-wrapped (first lines)) t)
             (push (make-line columns) lines)
             (setf column 0)))
      (loop for (source start end) in segments
            do (let ((i start))
                 (loop while (< i end)
                       do (let ((wide (logtest (aref (line-attrs source) i) +attr-wide+)))
                            (when (or (>= column columns)
                                      (and wide (> (+ column 2) columns) (> columns 1)))
                              (new-line))
                            (let* ((line (first lines))
                                   (n (min (- columns column) (- end i))))
                              ;; Keep a wide character with its right half.
                              (when (and (> n 1)
                                         (logtest (aref (line-attrs source) (+ i n -1))
                                                  +attr-wide+))
                                (decf n))
                              (when (and cursor (<= offset cursor) (< cursor (+ offset n)))
                                (setf place (list (1- (length lines)) (+ column (- cursor offset))
                                                  (if (logtest (aref (line-attrs source)
                                                                     (+ i (- cursor offset)))
                                                               +attr-wide+)
                                                      2 1))))
                              (replace (line-codes line) (line-codes source)
                                       :start1 column :start2 i :end2 (+ i n))
                              (replace (line-fg line) (line-fg source)
                                       :start1 column :start2 i :end2 (+ i n))
                              (replace (line-bg line) (line-bg source)
                                       :start1 column :start2 i :end2 (+ i n))
                              (replace (line-attrs line) (line-attrs source)
                                       :start1 column :start2 i :end2 (+ i n))
                              (when (line-ul source)
                                (replace (ensure-line-ul line) (line-ul source)
                                         :start1 column :start2 i :end2 (+ i n)))
                              (loop for (at . string) in (line-combining source)
                                    when (and (<= i at) (< at (+ i n)))
                                      do (push (cons (+ column (- at i)) string)
                                               (line-combining line)))
                              ;; A wide character on a one-column screen is
                              ;; cut to its left half.
                              (when (and (= columns 1) wide)
                                (setf (aref (line-attrs line) column)
                                      (logandc2 (aref (line-attrs line) column) +attr-wide+))
                                (when (and (< (1+ i) end)
                                           (logtest (aref (line-attrs source) (1+ i))
                                                    +attr-wide-spacer+))
                                  (incf n)))
                              (incf column (min n (- columns column)))
                              (note-extent line column)
                              (incf offset n)
                              (incf i n)))))))
    (when (and cursor (null place))
      ;; Past the text: as far after it, on its last line.
      (setf place (list (1- (length lines))
                        (min (1- columns) (+ column (- cursor offset)))
                        1)))
    (values (coerce (nreverse lines) 'simple-vector) place)))

(defun reflow-screen (screen rows columns cursor)
  "Rewrap SCREEN's lines and scrollback at COLUMNS and keep ROWS of them on
screen.  CURSOR, (ROW COLUMN PENDING-WRAP) or NIL, is placed on the same
cell of text; return its new (ROW COLUMN PENDING-WRAP) and how many rows the
text at the cursor moved up on the screen."
  (let* ((old-lines (screen-lines screen))
         (history-count (screen-history-count screen))
         (physical (concatenate 'simple-vector
                                (loop for i below history-count
                                      collect (history-line screen i))
                                old-lines))
         (cursor-index (and cursor (+ history-count (first cursor))))
         ;; Logical lines: lists of segments (LINE START END) to rewrap, or a
         ;; line that fits as it is (most of them), kept to save copying.
         (logicals '())
         (segments '())
         (logical-length 0)
         (cursor-logical nil)
         (cursor-offset 0)
         (cursor-pending (third cursor)))
    (loop for index below (length physical)
          for line = (svref physical index)
          for next = (and (< (1+ index) (length physical)) (svref physical (1+ index)))
          do (if (and (null segments) (not (line-wrapped line))
                      (<= (line-extent line) columns)
                      (not (eql index cursor-index)))
                 (push line logicals)
                 (let ((count (if (line-wrapped line)
                                  (let ((length (line-length line)))
                                    ;; The blank left at the edge by a wide
                                    ;; character that did not fit.
                                    (if (and next (zerop (aref (line-codes line) (1- length)))
                                             (not (logtest (aref (line-attrs line) (1- length))
                                                           +attr-wide-spacer+))
                                             (logtest (aref (line-attrs next) 0) +attr-wide+))
                                        (1- length)
                                        length))
                                  (line-extent line))))
                   (when (eql index cursor-index)
                     (setf cursor-logical (length logicals)
                           cursor-offset (+ logical-length (second cursor))))
                   (push (list line 0 count) segments)
                   (incf logical-length count)
                   (unless (line-wrapped line)
                     ;; A cursor just after its line's text is pending after
                     ;; the last character, so that a line ending at the new
                     ;; edge does not leave the cursor alone on a row of its own.
                     (when (and (eql cursor-logical (length logicals)) (not cursor-pending)
                                (plusp logical-length) (= cursor-offset logical-length))
                       (setf cursor-offset (1- logical-length)
                             cursor-pending t)
                       (let ((last (first segments)))
                         (when (and (> (third last) 1)
                                    (logtest (aref (line-attrs (first last)) (1- (third last)))
                                             +attr-wide-spacer+))
                           (decf cursor-offset))))
                     (push (nreverse segments) logicals)
                     (setf segments '()
                           logical-length 0)))))
    (when segments
      (push (nreverse segments) logicals))
    (setf logicals (nreverse logicals))
    ;; Lay them out again, finding the cursor and the last line of text.
    (let ((new '())
          (count 0)
          (new-cursor nil)
          (last-text -1))
      (loop for logical in logicals
            for index from 0
            do (multiple-value-bind (lines place)
                   (if (line-p logical)
                       (values (vector logical) nil)
                       (wrap-segments logical columns
                                      (and (eql index cursor-logical) cursor-offset)))
                 (when place
                   (destructuring-bind (line column width) place
                     (setf new-cursor
                           (if cursor-pending
                               ;; Pending wrap: the cursor is after its cell.
                               (if (>= (+ column width) columns)
                                   (list (+ count line) (1- columns) t)
                                   (list (+ count line) (+ column width) nil))
                               (list (+ count line) column nil)))))
                 (loop for line across lines
                       do (push line new)
                          (when (or (plusp (line-extent line)) (line-wrapped line))
                            (setf last-text count))
                          (incf count))))
      (let* ((new (coerce (nreverse new) 'simple-vector))
             (end (max (1+ last-text) (if new-cursor (1+ (first new-cursor)) 0)))
             (start (max 0 (- end rows)))
             (lines (make-array rows)))
        ;; Scrollback keeps lines at the width they have; the screen's are
        ;; exactly COLUMNS wide.
        (dotimes (row rows)
          (setf (svref lines row)
                (if (< (+ start row) (length new))
                    (resize-line (svref new (+ start row)) columns)
                    (make-line columns))))
        (clear-history screen)
        (dotimes (i start)
          (history-push screen (svref new i)))
        (setf (screen-lines screen) lines)
        (values (and new-cursor
                     (list (- (first new-cursor) start) (second new-cursor) (third new-cursor)))
                (if (and cursor new-cursor)
                    (- (first cursor) (- (first new-cursor) start))
                    0))))))

(defun terminal-resize (term rows columns)
  "Resize to ROWS x COLUMNS.  When the width changes, the main screen's text
is rewrapped (see REFLOW-SCREEN); otherwise, and on the alternate screen,
lines are truncated or extended, and when the screen gets shorter, lines
above the cursor move to scrollback so that the cursor stays on screen."
  (let ((rows (max 1 rows))
        (columns (max 1 columns)))
    (unless (and (= rows (terminal-rows term)) (= columns (terminal-columns term)))
      (let* ((shift (max 0 (- (terminal-cursor-row term) (1- rows))))
             (old-tabs (terminal-tabs term))
             (reflow (/= columns (terminal-columns term)))
             (primary (terminal-primary term))
             (alternate-p (terminal-alternate-p term))
             ;; The main screen's cursor: the cursor, or while the alternate
             ;; screen is shown, the one saved on entering it.
             (saved (screen-saved-cursor primary))
             (primary-cursor (cond ((not alternate-p)
                                    (list (terminal-cursor-row term)
                                          (terminal-cursor-column term)
                                          (terminal-pending-wrap term)))
                                   (saved
                                    (list (min (svref saved 0) (1- (terminal-rows term)))
                                          (min (svref saved 1) (1- (terminal-columns term)))
                                          (svref saved 2)))))
             (new-primary-cursor nil))
        (if reflow
            (multiple-value-bind (cursor moved)
                (reflow-screen primary rows columns primary-cursor)
              (setf new-primary-cursor cursor)
              (note-scroll term 0 (1- (terminal-rows term)) moved :primary)
              (when (and alternate-p saved cursor)
                (setf (svref saved 0) (first cursor)
                      (svref saved 1) (second cursor)
                      (svref saved 2) (third cursor))))
            (progn
              (note-scroll term 0 (1- (terminal-rows term)) shift :primary)
              (resize-screen primary rows columns shift t)))
        (note-scroll term 0 (1- (terminal-rows term)) shift :alternate)
        (resize-screen (terminal-alternate term) rows columns shift nil)
        (setf (terminal-rows term) rows
              (terminal-columns term) columns
              (terminal-scroll-top term) 0
              (terminal-scroll-bottom term) (1- rows)
              (terminal-tabs term) (let ((tabs (default-tabs columns)))
                                     (replace tabs old-tabs)
                                     tabs)
              (terminal-dirty term) (make-array rows :element-type 'bit :initial-element 1)
              (terminal-viewport-offset term) (if reflow
                                                  0
                                                  (min (terminal-viewport-offset term)
                                                       (terminal-history-count term)))
              (terminal-cluster-row term) -1)
        (if (and reflow (not alternate-p) new-primary-cursor)
            (progn
              (set-cursor term (first new-primary-cursor) (second new-primary-cursor))
              (setf (terminal-pending-wrap term) (third new-primary-cursor)))
            (set-cursor term (- (terminal-cursor-row term) shift)
                        (terminal-cursor-column term))))))
  term)

;;; Queries

(defun terminal-cursor-visible-p (term)
  "True when the cursor should be drawn: it is enabled (DECTCEM) and the
viewport is not scrolled back."
  (and (terminal-cursor-shown term)
       (zerop (terminal-viewport-offset term))))

(defun terminal-mode (term mode)
  "Whether MODE is set.  MODE is a DEC private mode number or one of
:APPLICATION-CURSOR, :APPLICATION-KEYPAD, :BRACKETED-PASTE,
:ALTERNATE-SCREEN, :AUTOWRAP, :ORIGIN, :INSERT, :CURSOR-VISIBLE,
:FOCUS-EVENTS, :MOUSE-X10, :MOUSE-NORMAL, :MOUSE-BUTTON, :MOUSE-ANY,
:MOUSE-SGR or :SYNCHRONIZED-OUTPUT."
  (if (integerp mode)
      (dec-mode-set-p term mode)
      (case mode
        (:application-cursor (dec-mode-set-p term 1))
        (:application-keypad (dec-mode-set-p term 66))
        (:bracketed-paste (dec-mode-set-p term 2004))
        (:alternate-screen (terminal-alternate-p term))
        (:autowrap (terminal-autowrap term))
        (:origin (terminal-origin term))
        (:insert (terminal-insert term))
        (:cursor-visible (terminal-cursor-shown term))
        (:focus-events (dec-mode-set-p term 1004))
        (:mouse-x10 (dec-mode-set-p term 9))
        (:mouse-normal (dec-mode-set-p term 1000))
        (:mouse-button (dec-mode-set-p term 1002))
        (:mouse-any (dec-mode-set-p term 1003))
        (:mouse-sgr (dec-mode-set-p term 1006))
        (:synchronized-output (dec-mode-set-p term 2026))
        (t nil))))

(defun terminal-scroll-viewport (term how)
  "Scroll the view through scrollback: HOW is :TOP, :BOTTOM or a number of
rows (negative scrolls back).  Return the new offset."
  (let* ((limit (terminal-history-count term))
         (old (terminal-viewport-offset term))
         (new (if (terminal-alternate-p term)
                  0
                  (case how
                    (:top limit)
                    (:bottom 0)
                    (t (max 0 (min limit (- old how))))))))
    (unless (= old new)
      (setf (terminal-viewport-offset term) new)
      (fill (terminal-dirty term) 1))
    new))

(defun line-text (line)
  (with-output-to-string (out)
    (let ((codes (line-codes line))
          (attrs (line-attrs line)))
      (dotimes (column (length codes))
        (unless (logtest (aref attrs column) +attr-wide-spacer+)
          (let ((code (aref codes column)))
            (write-char (if (zerop code) #\Space (code-char code)) out)
            (let ((extra (cdr (assoc column (line-combining line)))))
              (when extra (write-string extra out)))))))))

(defun terminal-row-text (term row &key (trim t))
  "The text displayed at ROW."
  (let ((text (line-text (terminal-line term row))))
    (if trim (string-right-trim " " text) text)))

(defun terminal-text (term &key (trim t))
  "The displayed text, rows separated by newlines."
  (format nil "~{~a~^~%~}"
          (loop for row below (terminal-rows term)
                collect (terminal-row-text term row :trim trim))))
