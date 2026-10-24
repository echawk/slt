(in-package #:slt)

;;;; Engine backed by lispvt (lib/lispvt), a VT emulator in pure Lisp.
;;;; Its lines use the grid's cell layout (tagged colors, the same
;;;; attribute bits), so a dirty row is copied with REPLACE.

(defclass lispvt-engine (engine)
  ((terminal :reader lispvt-engine-terminal)))

(defmethod initialize-instance :after ((engine lispvt-engine) &key (rows 24) (columns 80))
  (setf (slot-value engine 'terminal)
        (lispvt:make-terminal
         :rows rows :columns columns
         :output-function (lambda (string) (engine-emit-output engine string))
         :title-function (lambda (title) (engine-emit-title engine title))
         :bell-function (lambda () (engine-emit-bell engine)))))

(defmethod engine-rows ((engine lispvt-engine))
  (lispvt:terminal-rows (lispvt-engine-terminal engine)))

(defmethod engine-columns ((engine lispvt-engine))
  (lispvt:terminal-columns (lispvt-engine-terminal engine)))

(defmethod engine-cursor-position ((engine lispvt-engine))
  (let ((terminal (lispvt-engine-terminal engine)))
    (values (lispvt:terminal-cursor-row terminal)
            (lispvt:terminal-cursor-column terminal))))

(defmethod engine-feed ((engine lispvt-engine) octets &key (start 0) (end (length octets)))
  (lispvt:terminal-feed (lispvt-engine-terminal engine) octets :start start :end end)
  engine)

(defmethod engine-resize ((engine lispvt-engine) rows columns)
  (lispvt:terminal-resize (lispvt-engine-terminal engine) rows columns)
  engine)

(defmethod engine-mode ((engine lispvt-engine) mode)
  (let ((terminal (lispvt-engine-terminal engine)))
    (if (eq mode :mouse-tracking)
        (cond ((lispvt:terminal-mode terminal :mouse-any) :any)
              ((lispvt:terminal-mode terminal :mouse-button) :button)
              ((lispvt:terminal-mode terminal :mouse-normal) :normal)
              ((lispvt:terminal-mode terminal :mouse-x10) :x10))
        (lispvt:terminal-mode terminal mode))))

(defmethod engine-scroll ((engine lispvt-engine) how)
  (lispvt:terminal-scroll-viewport (lispvt-engine-terminal engine) how)
  t)

(defun copy-lispvt-line (line grid row)
  (declare (type lispvt:line line)
           (type grid grid)
           (type grid-dimension row)
           (optimize (speed 3)))
  (let* ((columns (grid-columns grid))
         (start (grid-index grid row 0))
         (end (+ start columns))
         (combining (grid-combining grid)))
    (replace (grid-codes grid) (lispvt:line-codes line) :start1 start :end1 end)
    (replace (grid-fg grid) (lispvt:line-fg line) :start1 start :end1 end)
    (replace (grid-bg grid) (lispvt:line-bg line) :start1 start :end1 end)
    (replace (grid-attrs grid) (lispvt:line-attrs line) :start1 start :end1 end)
    (when (plusp (hash-table-count combining))
      (loop for index from start below end
            do (remhash index combining)))
    (loop for (column . string) in (lispvt:line-combining line)
          when (< column columns)
            do (grid-set-combining grid (+ start column) string))))

(defmethod engine-update-grid ((engine lispvt-engine) grid &key force)
  (let* ((terminal (lispvt-engine-terminal engine))
         (rows (lispvt:terminal-rows terminal))
         (columns (lispvt:terminal-columns terminal))
         (cursor-row (lispvt:terminal-cursor-row terminal))
         (cursor-column (lispvt:terminal-cursor-column terminal))
         (visiblep (lispvt:terminal-cursor-visible-p terminal))
         (shape (lispvt:terminal-cursor-shape terminal)))
    (unless (and (= rows (grid-rows grid)) (= columns (grid-columns grid)))
      (grid-resize grid rows columns)
      (setf force t))
    (dotimes (row rows)
      (when (or force (lispvt:terminal-row-dirty-p terminal row))
        (copy-lispvt-line (lispvt:terminal-line terminal row) grid row)
        (setf (sbit (grid-dirty grid) row) 1)))
    (lispvt:terminal-clear-dirty terminal)
    (when (or (/= cursor-row (grid-cursor-row grid))
              (/= cursor-column (grid-cursor-column grid))
              (not (eq visiblep (grid-cursor-visible-p grid)))
              (not (eq shape (grid-cursor-shape grid))))
      (setf (sbit (grid-dirty grid) (grid-cursor-row grid)) 1
            (sbit (grid-dirty grid) cursor-row) 1))
    (setf (grid-cursor-row grid) cursor-row
          (grid-cursor-column grid) cursor-column
          (grid-cursor-visible-p grid) visiblep
          (grid-cursor-shape grid) shape)
    grid))

(register-engine :lispvt
                 (lambda (&rest arguments)
                   (apply #'make-instance 'lispvt-engine arguments)))
