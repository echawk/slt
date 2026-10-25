(in-package #:slt)

;;;; Pixel-smooth scrolling through the scrollback.  Trackpads report
;;;; scrolling in pixels; normally they move the view a whole row at a time.
;;;; With smooth scrolling the view also stops between rows: the engine's
;;;; viewport moves by whole rows, and the remainder, SMOOTH-OFFSET (0 up to
;;;; a cell height, in device pixels), moves the drawing down so that part
;;;; of the row above shows at the top.  That row comes from the scrollback
;;;; and is drawn as an extra first row of the grid the frontend gets.
;;;;
;;;; When the gesture ends the view slides to the nearest whole row, so it
;;;; stays between rows only while it moves.  Programs that track the mouse
;;;; or use the alternate screen still get whole rows.  Frontends opt in
;;;; with FRONTEND-BIND-PIXEL-SCROLL and FRONTEND-SET-SMOOTH-OFFSET.

(defvar *smooth-scrolling* nil
  "When true, trackpads scroll the scrollback pixel by pixel on frontends
that can.  Also turned on by the SLT_SMOOTH_SCROLL environment variable or
the --smooth-scroll option.")

(defun smooth-scrolling-requested-p ()
  "*SMOOTH-SCROLLING*, or the SLT_SMOOTH_SCROLL environment variable, read
when a window opens (not when slt is built)."
  (or *smooth-scrolling*
      (let ((value (uiop:getenv "SLT_SMOOTH_SCROLL")))
        (and value (not (member value '("" "0" "no" "false" "off") :test #'string-equal))))))

(defparameter *smooth-snap-rate* 0.35
  "The share of the distance to the nearest row the view slides each frame
once a gesture ends.")

(defun emulator-cell-height (emulator)
  (max 1 (frontend-view-cell-height (terminal-emulator-frontend emulator)
                                    (terminal-emulator-view emulator))))

(defun reset-smooth-offset (emulator)
  "Drop any partial row: the view is on a whole row again."
  (when (or (plusp (terminal-emulator-smooth-offset emulator))
            (terminal-emulator-smooth-target emulator))
    (setf (terminal-emulator-smooth-offset emulator) 0
          (terminal-emulator-smooth-target emulator) nil)
    (grid-mark-all-dirty (terminal-emulator-grid emulator))))

(defun move-smooth-offset (emulator offset)
  "Show the view OFFSET pixels further back than the engine's viewport,
moving the viewport by whole rows to keep the remainder within a row."
  (let* ((height (emulator-cell-height emulator))
         (rows (floor offset height))
         (remainder (- offset (* rows height))))
    (unless (zerop rows)
      (with-engine (engine emulator)
        (let ((before (or (engine-viewport-offset engine) 0)))
          (engine-scroll engine (- rows))
          ;; At the top of the scrollback or on the live screen the view
          ;; stops on the row it reached.
          (unless (= rows (- (or (engine-viewport-offset engine) 0) before))
            (setf remainder 0)))))
    (setf (terminal-emulator-smooth-offset emulator) remainder)
    (grid-mark-all-dirty (terminal-emulator-grid emulator))
    (request-render emulator)))

(defun handle-pixel-scroll (emulator pixels ending-p)
  "PIXELS (device pixels) of trackpad scrolling, positive toward the
scrollback.  ENDING-P marks the end of a gesture or of its momentum."
  (multiple-value-bind (tracked alternate)
      (with-engine (engine emulator)
        (values (engine-mode engine :mouse-tracking) (engine-mode engine :alternate-screen)))
    (if (or tracked alternate)
        ;; Programs get whole rows, as from a wheel.
        (let* ((rows (+ (terminal-emulator-smooth-remainder emulator)
                        (/ pixels (emulator-cell-height emulator))))
               (whole (truncate rows)))
          (setf (terminal-emulator-smooth-remainder emulator) (- rows whole))
          (unless (zerop whole)
            (handle-scroll emulator (- whole))))
        (progn
          ;; New movement, such as momentum after the fingers lift, stops a
          ;; slide to the nearest row.
          (setf (terminal-emulator-smooth-target emulator) nil)
          (unless (zerop pixels)
            (move-smooth-offset emulator (+ (terminal-emulator-smooth-offset emulator) pixels)))
          (when ending-p
            (let ((offset (terminal-emulator-smooth-offset emulator))
                  (height (emulator-cell-height emulator)))
              (when (plusp offset)
                (setf (terminal-emulator-smooth-target emulator)
                      (if (< offset (/ height 2)) 0 height))
                (request-render emulator))))))))

(defun step-smooth-snap (emulator)
  "Slide the view one frame toward the row it is settling on.  Called by
RENDER-EMULATOR."
  (let ((target (terminal-emulator-smooth-target emulator)))
    (when target
      (let* ((offset (terminal-emulator-smooth-offset emulator))
             (next (+ offset (* *smooth-snap-rate* (- target offset)))))
        (if (< (abs (- target next)) 0.75)
            (progn
              (setf (terminal-emulator-smooth-target emulator) nil)
              (move-smooth-offset emulator target))
            (progn
              (setf (terminal-emulator-smooth-offset emulator) next)
              (grid-mark-all-dirty (terminal-emulator-grid emulator))
              (request-render emulator)))))))

(defun compose-smooth-grid (display overhang grid)
  "Fill DISPLAY with OVERHANG's first row above GRID's rows."
  (let* ((columns (grid-columns grid))
         (size (* columns (grid-rows grid))))
    (unless (and (= (grid-columns display) columns)
                 (= (grid-rows display) (1+ (grid-rows grid))))
      (grid-resize display (1+ (grid-rows grid)) columns))
    (flet ((copy (accessor)
             (let ((target (funcall accessor display)))
               (replace target (funcall accessor overhang) :end1 columns)
               (replace target (funcall accessor grid) :start1 columns :end2 size))))
      (copy #'grid-codes)
      (copy #'grid-fg)
      (copy #'grid-bg)
      (copy #'grid-attrs)
      (copy #'grid-ul))
    (let ((combining (grid-combining display)))
      (clrhash combining)
      (maphash (lambda (index string)
                 (when (< index columns) (setf (gethash index combining) string)))
               (grid-combining overhang))
      (maphash (lambda (index string) (setf (gethash (+ index columns) combining) string))
               (grid-combining grid)))
    (setf (grid-cursor-row display) (1+ (grid-cursor-row grid))
          (grid-cursor-column display) (grid-cursor-column grid)
          (grid-cursor-visible-p display) (grid-cursor-visible-p grid)
          (grid-cursor-shape display) (grid-cursor-shape grid)
          (grid-selection display) (let ((selection (grid-selection grid)))
                                     (and selection (cons (+ (car selection) columns)
                                                          (+ (cdr selection) columns)))))
    (grid-mark-all-dirty display)
    display))

(defun smooth-display-grid (emulator engine grid)
  "The grid to draw: GRID itself, or while the view is between rows, GRID
under the scrollback row above it.  The engine lock must be held."
  (if (zerop (terminal-emulator-smooth-offset emulator))
      grid
      (let ((before (or (engine-viewport-offset engine) 0)))
        (engine-scroll engine -1)
        (if (= before (or (engine-viewport-offset engine) 0))
            ;; Nothing above: the top of the scrollback.
            (progn (setf (terminal-emulator-smooth-offset emulator) 0
                         (terminal-emulator-smooth-target emulator) nil)
                   grid)
            (let ((overhang (or (terminal-emulator-smooth-overhang emulator)
                                (setf (terminal-emulator-smooth-overhang emulator)
                                      (make-grid 1 1)))))
              (engine-update-grid engine overhang :force t)
              (engine-scroll engine 1)
              ;; The reads consumed the engine's dirty rows.
              (setf (terminal-image-store-force-render-p
                     (terminal-emulator-image-store emulator))
                    t)
              (compose-smooth-grid (or (terminal-emulator-smooth-grid emulator)
                                       (setf (terminal-emulator-smooth-grid emulator)
                                             (make-grid 1 1)))
                                   overhang grid))))))
