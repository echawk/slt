(in-package #:slt)

;;;; Engine-independent terminal graphics. The stream parser consumes only
;;;; SIXEL DCS and Kitty APC strings, and observes (without consuming) the
;;;; few CSI sequences that affect graphics; every other byte reaches the
;;;; selected VT engine unchanged and in its original order.
;;;;
;;;; Placements are anchored to screen cells.  Engines that report their
;;;; scrolls (ENGINE-REPORTS-SCROLLS-P) move them exactly through
;;;; NOTE-ENGINE-SCROLL; for the others the emulator infers whole-screen
;;;; scrolls from the grid.

(defvar *sixel-images* t)
(defvar *kitty-graphics-images* t)
(defvar *terminal-image-storage-limit* (* 320 1024 1024))

(defvar *terminal-image-scrollback-rows* 10000
  "Rows above the screen a placement may scroll into before it is dropped.")

(defvar *kitty-max-relative-depth* 8
  "Longest chain of relative Kitty placements accepted (the spec's minimum).")

(defparameter +kitty-default-frame-gap+ 40)

(defparameter +max-placement-cells+ 2000
  "Most rows or columns a placement may span.")

(defstruct terminal-image-frame
  (rgba #() :type (simple-array (unsigned-byte 8) (*)))
  ;; Milliseconds until the next frame; zero or negative frames are skipped.
  (gap +kitty-default-frame-gap+ :type integer))

(defstruct terminal-image
  ;; Ids above 32 bits are internal (SIXEL and anonymous Kitty images), out
  ;; of reach of clients.
  (id 0 :type (integer 0))
  (number 0 :type (unsigned-byte 32))
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  ;; The frame shown now.  A new vector whenever the frame changes, so
  ;; renderers may cache textures by its identity.
  (rgba #() :type (simple-array (unsigned-byte 8) (*)))
  (access 0 :type integer)
  ;; Transmitted without an id or number: freed with its last placement.
  (anonymous-p nil)
  (frames (make-array 1 :adjustable t :fill-pointer 0) :type vector)
  (current-frame 0 :type fixnum)
  (animation :stopped :type (member :stopped :loading :running))
  ;; Wraps to the first frame allowed before stopping; zero is unlimited.
  (max-loops 0 :type integer)
  (loops 0 :type integer)
  ;; Internal real time, in milliseconds, at which the current frame ends.
  (frame-deadline nil)
  ;; The transmission of an indexed-color (f=8 or f=16) image, whose colors
  ;; follow the terminal's palette; NIL otherwise, or once it has frames.
  (indexed nil))

(defstruct terminal-image-placement
  (image-id 0 :type (integer 0))
  (placement-id 0 :type (unsigned-byte 32))
  (row 0 :type integer)
  (column 0 :type integer)
  ;; Requested size in cells, or NIL for the image's own size.
  columns rows
  (source-x 0 :type integer)
  (source-y 0 :type integer)
  source-width source-height
  (x-offset 0 :type integer)
  (y-offset 0 :type integer)
  (z-index 0 :type integer)
  (virtual-p nil)
  (screen :primary :type (member :primary :alternate))
  (erase-on-write-p nil)
  ;; Cells (DC + DR * COLUMNS) no longer showing the image.
  (hidden-cells (make-hash-table :test #'eql) :type hash-table)
  ;; (FIRST . LAST): the only rows still shown after being clipped by a
  ;; scroll region, or NIL for all.
  (row-clip nil)
  ;; Relative placements: the parent's (IMAGE-ID . PLACEMENT-ID) and the
  ;; offset in cells from its top left cell.
  parent
  (h-offset 0 :type integer)
  (v-offset 0 :type integer)
  ;; Set on display copies only: the drawn rectangle in pixels relative to
  ;; the placement's cell, overriding the computed layout.
  draw-rect
  ;; SIXEL: each cell's (CODE FG BG ATTRS) when the image was drawn, or NIL
  ;; until known.  A cell whose text later differs no longer shows the image.
  baseline)

(defstruct (terminal-image-store (:constructor make-terminal-image-store))
  (images (make-hash-table) :type hash-table)
  (placements '() :type list)
  (numbers (make-hash-table) :type hash-table)
  (next-id 1 :type (integer 1 #xffffffff))
  (next-internal-id #x100000000 :type integer)
  (clock 0 :type integer)
  (bytes 0 :type integer)
  (limit *terminal-image-storage-limit* :type integer)
  (dirty-p nil)
  (pending-scroll 0 :type integer)
  ;; Whole-screen scrolls reported by the engine since the last render.
  (reported-scroll 0 :type integer)
  (active-screen :primary :type (member :primary :alternate))
  (kitty-decoder (kitty-graphics:make-graphics-decoder))
  ;; The palette indexed-color images are drawn with; NIL for *PALETTE*.
  (palette nil)
  ;; Geometry, kept current by the emulator.
  (cell-width 8 :type (integer 1 *))
  (cell-height 16 :type (integer 1 *))
  (rows 24 :type (integer 1 *))
  (columns 80 :type (integer 1 *))
  ;; SIXEL modes: DECSDM (?80) and sixel scrolling leaving the cursor to
  ;; the right of the image (?8452).
  (sixel-display-mode nil)
  (sixel-scrolls-right nil)
  ;; (IMAGE-ID . PLACEMENT-ID) of virtual placements -> (ROW . COLUMN), the
  ;; top left of their Unicode placeholders at the last render.
  (virtual-origins (make-hash-table :test #'equal) :type hash-table)
  ;; Grid the screen under new SIXEL images is read into; reading consumes
  ;; the engine's dirty rows, so the next render must copy everything.
  (scratch-grid nil)
  (force-render-p nil))

;;; Image storage

(defun allocate-image-id (store &key internal)
  "A free image id: a 32-bit one a client may use (for images created by
number), or with INTERNAL one no client can name."
  (if internal
      (incf (terminal-image-store-next-internal-id store))
      (loop for id = (terminal-image-store-next-id store)
            do (setf (terminal-image-store-next-id store)
                     (if (= id #xffffffff) 1 (1+ id)))
            unless (gethash id (terminal-image-store-images store))
              return id)))

(defun image-store-id (store requested-id number)
  (cond ((plusp requested-id) requested-id)
        ((plusp number) (allocate-image-id store))
        (t (allocate-image-id store :internal t))))

(defun image-bytes (image)
  (* 4 (terminal-image-width image) (terminal-image-height image)
     (max 1 (length (terminal-image-frames image)))))

(defun image-placed-p (store id)
  (find id (terminal-image-store-placements store)
        :key #'terminal-image-placement-image-id))

(defun remove-image (store id)
  (let ((image (gethash id (terminal-image-store-images store))))
    (when image
      (decf (terminal-image-store-bytes store) (image-bytes image))
      (remhash id (terminal-image-store-images store))
      (delete-placements-if store (lambda (placement)
                                    (= id (terminal-image-placement-image-id placement))))
      (when (and (plusp (terminal-image-number image))
                 (= id (gethash (terminal-image-number image)
                                (terminal-image-store-numbers store) 0)))
        (remhash (terminal-image-number image) (terminal-image-store-numbers store)))
      (setf (terminal-image-store-dirty-p store) t))))

(defun evict-images-to-fit (store bytes)
  "Free images, unplaced and least recently used first, until BYTES fit."
  (loop until (<= (+ bytes (terminal-image-store-bytes store))
                  (terminal-image-store-limit store))
        do (let ((victim (let ((best nil))
                       (loop for image being the hash-values
                               of (terminal-image-store-images store)
                             for key = (list (if (image-placed-p store (terminal-image-id image))
                                                 1 0)
                                             (terminal-image-access image))
                             when (or (null best)
                                      (< (first key) (first (car best)))
                                      (and (= (first key) (first (car best)))
                                           (< (second key) (second (car best)))))
                               do (setf best (cons key image)))
                       (cdr best))))
             (if victim
                 (remove-image store (terminal-image-id victim))
                 (error 'kitty-command-error :code "ENOSPC"
                                             :message "image exceeds the storage quota")))))

(defun store-image (store id number width height rgba &key anonymous-p)
  (remove-image store id)
  (evict-images-to-fit store (length rgba))
  (let ((image (make-terminal-image :id id :number number :width width :height height
                                    :rgba rgba :anonymous-p anonymous-p
                                    :access (incf (terminal-image-store-clock store)))))
    (vector-push-extend (make-terminal-image-frame :rgba rgba :gap 0)
                        (terminal-image-frames image))
    (setf (gethash id (terminal-image-store-images store)) image)
    (when (plusp number)
      (setf (gethash number (terminal-image-store-numbers store)) id))
    (incf (terminal-image-store-bytes store) (length rgba))
    (setf (terminal-image-store-dirty-p store) t)
    image))

;;; Placement geometry

(defun placement-source-rect (placement image)
  "The displayed part of IMAGE: the requested rectangle clipped to it."
  (let* ((iw (terminal-image-width image))
         (ih (terminal-image-height image))
         (sx (min (max 0 (terminal-image-placement-source-x placement)) iw))
         (sy (min (max 0 (terminal-image-placement-source-y placement)) ih))
         (sw (min (or (terminal-image-placement-source-width placement) iw) (- iw sx)))
         (sh (min (or (terminal-image-placement-source-height placement) ih) (- ih sy))))
    (values sx sy sw sh)))

(defun fit-rectangle (width height box-width box-height)
  "Scale WIDTH x HEIGHT to fit the box, centered: X Y WIDTH HEIGHT."
  (let* ((scale (min (/ box-width (max 1 width)) (/ box-height (max 1 height))))
         (w (max 1 (round (* width scale))))
         (h (max 1 (round (* height scale)))))
    (values (floor (- box-width w) 2) (floor (- box-height h) 2) w h)))

(defun placement-layout (placement image cell-width cell-height)
  "Return the placement's COLUMNS and ROWS and the drawn rectangle X Y WIDTH
HEIGHT in pixels relative to its top left cell.  With only one of the cell
counts given the other follows the aspect ratio; with both, the image is
letterboxed into them; with neither it keeps its own pixel size."
  (multiple-value-bind (sx sy sw sh) (placement-source-rect placement image)
    (declare (ignore sx sy))
    (let ((columns (terminal-image-placement-columns placement))
          (rows (terminal-image-placement-rows placement))
          (xo (terminal-image-placement-x-offset placement))
          (yo (terminal-image-placement-y-offset placement))
          (sw (max 1 sw))
          (sh (max 1 sh)))
      (cond
        ((and columns rows)
         (multiple-value-bind (x y w h)
             (fit-rectangle sw sh (* columns cell-width) (* rows cell-height))
           (values columns rows (+ xo x) (+ yo y) w h)))
        (columns
         (let* ((w (* columns cell-width))
                (h (max 1 (round (* w sh) sw))))
           (values columns (ceiling (+ yo h) cell-height) xo yo w h)))
        (rows
         (let* ((h (* rows cell-height))
                (w (max 1 (round (* h sw) sh))))
           (values (ceiling (+ xo w) cell-width) rows xo yo w h)))
        (t
         (values (max 1 (ceiling (+ xo sw) cell-width))
                 (max 1 (ceiling (+ yo sh) cell-height))
                 xo yo sw sh))))))

(defun placement-cell-size (placement image cell-width cell-height)
  "Return a placement's width and height in cells."
  (multiple-value-bind (columns rows)
      (placement-layout placement image cell-width cell-height)
    (values columns rows)))

(defun find-placement (store image-id placement-id)
  "The placement (IMAGE-ID, PLACEMENT-ID); with PLACEMENT-ID 0 the newest one
of the image."
  (find-if (lambda (placement)
             (and (= image-id (terminal-image-placement-image-id placement))
                  (or (zerop placement-id)
                      (= placement-id (terminal-image-placement-placement-id placement)))))
           (terminal-image-store-placements store)))

(defun placement-origin (store placement &optional (depth 0))
  "The top left cell of PLACEMENT as (VALUES ROW COLUMN), following relative
placements to their parents, or NIL when it has no position."
  (let ((parent-key (terminal-image-placement-parent placement)))
    (cond
      ((null parent-key)
       (if (terminal-image-placement-virtual-p placement)
           (let ((origin (gethash (cons (terminal-image-placement-image-id placement)
                                        (terminal-image-placement-placement-id placement))
                                  (terminal-image-store-virtual-origins store))))
             (and origin (values (car origin) (cdr origin))))
           (values (terminal-image-placement-row placement)
                   (terminal-image-placement-column placement))))
      ((> depth *kitty-max-relative-depth*) nil)
      (t
       (let ((parent (find-placement store (car parent-key) (cdr parent-key))))
         (when parent
           (multiple-value-bind (row column) (placement-origin store parent (1+ depth))
             (and row
                  (values (+ row (terminal-image-placement-v-offset placement))
                          (+ column (terminal-image-placement-h-offset placement)))))))))))

(defun placement-screen (store placement &optional (depth 0))
  "The screen a placement shows on: its own, or its root parent's."
  (let ((parent-key (terminal-image-placement-parent placement)))
    (if (and parent-key (<= depth *kitty-max-relative-depth*))
        (let ((parent (find-placement store (car parent-key) (cdr parent-key))))
          (if parent
              (placement-screen store parent (1+ depth))
              (terminal-image-placement-screen placement)))
        (terminal-image-placement-screen placement))))

(defun visible-cell-rectangles (hidden columns rows &optional row-clip)
  "The cells of a COLUMNS x ROWS placement not in HIDDEN, as rectangles
(TOP BOTTOM LEFT RIGHT) of cell offsets, exclusive at the end: runs of
cells in a row, joined with the rows below that have the same runs."
  (let ((done '())
        (open '()))                     ; ((left . right) . top)
    (dotimes (dr (1+ rows))
      (let ((runs (and (< dr rows)
                       (or (null row-clip) (<= (car row-clip) dr (cdr row-clip)))
                       (loop with start = nil
                             for dc from 0 to columns
                             for visible = (and (< dc columns)
                                                (not (gethash (+ dc (* dr columns)) hidden)))
                             when (and visible (null start))
                               do (setf start dc)
                             when (and (not visible) start)
                               collect (cons start dc)
                               and do (setf start nil)))))
        (dolist (entry open)
          (unless (member (car entry) runs :test #'equal)
            (push (list (cdr entry) dr (car (car entry)) (cdr (car entry))) done)))
        (setf open (loop for run in runs
                         for continuing = (assoc run open :test #'equal)
                         collect (cons run (if continuing (cdr continuing) dr))))))
    (nreverse done)))

(defun placement-render-records (placement image cell-width cell-height)
  (let ((row (terminal-image-placement-row placement))
        (column (terminal-image-placement-column placement))
        (draw (terminal-image-placement-draw-rect placement)))
    (if draw
        (destructuring-bind (x y w h) draw
          (list (list placement image
                      (+ (* column cell-width) x) (+ (* row cell-height) y) w h)))
        (multiple-value-bind (columns rows dx dy dw dh)
            (placement-layout placement image cell-width cell-height)
          (let ((hidden (terminal-image-placement-hidden-cells placement))
                (x0 (+ (* column cell-width) dx))
                (y0 (+ (* row cell-height) dy)))
            (if (and (zerop (hash-table-count hidden))
                     (null (terminal-image-placement-row-clip placement)))
                (list (list placement image x0 y0 dw dh))
                ;; Draw only the visible cells' share of the image.
                (multiple-value-bind (sx sy sw sh) (placement-source-rect placement image)
                  (loop for (top bottom left-cell right-cell)
                          in (visible-cell-rectangles
                              hidden columns rows (terminal-image-placement-row-clip placement))
                        for left = (max (* (+ column left-cell) cell-width) x0)
                        for right = (min (* (+ column right-cell) cell-width) (+ x0 dw))
                        for upper = (max (* (+ row top) cell-height) y0)
                        for lower = (min (* (+ row bottom) cell-height) (+ y0 dh))
                        when (and (< left right) (< upper lower))
                          collect (let ((fragment (copy-terminal-image-placement placement))
                                        (fx0 (+ sx (floor (* (- left x0) sw) dw)))
                                        (fy0 (+ sy (floor (* (- upper y0) sh) dh)))
                                        (fx1 (+ sx (ceiling (* (- right x0) sw) dw)))
                                        (fy1 (+ sy (ceiling (* (- lower y0) sh) dh))))
                                    (setf (terminal-image-placement-source-x fragment) fx0
                                          (terminal-image-placement-source-y fragment) fy0
                                          (terminal-image-placement-source-width fragment)
                                          (max 1 (- fx1 fx0))
                                          (terminal-image-placement-source-height fragment)
                                          (max 1 (- fy1 fy0)))
                                    (list fragment image left upper
                                          (- right left) (- lower upper)))))))))))

(defun terminal-image-render-list (store cell-width cell-height)
  "Return visible (PLACEMENT IMAGE X Y WIDTH HEIGHT) records in z order, lower
image ids first among equal z-indices."
  (sort
   (loop for placement in (terminal-image-store-placements store)
         for image = (gethash (terminal-image-placement-image-id placement)
                              (terminal-image-store-images store))
         when (and image
                   (eq (terminal-image-placement-screen placement)
                       (terminal-image-store-active-screen store))
                   (not (terminal-image-placement-virtual-p placement))
                   (null (terminal-image-placement-parent placement)))
           append (placement-render-records placement image cell-width cell-height))
   (lambda (a b)
     (let ((za (terminal-image-placement-z-index (first a)))
           (zb (terminal-image-placement-z-index (first b))))
       (or (< za zb)
           (and (= za zb)
                (< (terminal-image-id (second a)) (terminal-image-id (second b)))))))))

;;; Deleting placements

(defun delete-placements-if (store predicate &key free-p)
  "Delete the placements satisfying PREDICATE and, transitively, placements
relative to them.  Images left without placements are freed when FREE-P,
when anonymous, or when a relative placement of theirs went with its
parent.  Return true when anything was deleted."
  (let ((doomed '())
        (relative-ids '()))
    (setf (terminal-image-store-placements store)
          (delete-if (lambda (placement)
                       (when (funcall predicate placement)
                         (push placement doomed)
                         t))
                     (terminal-image-store-placements store)))
    ;; Relative placements die with their parents.
    (loop while doomed
          do (let ((keys (mapcar (lambda (placement)
                                   (cons (terminal-image-placement-image-id placement)
                                         (terminal-image-placement-placement-id placement)))
                                 doomed))
                   (affected (mapcar #'terminal-image-placement-image-id doomed)))
               (setf doomed '())
               (dolist (id affected)
                 (let ((image (gethash id (terminal-image-store-images store))))
                   (when (and image
                              (or free-p (terminal-image-anonymous-p image)
                                  (member id relative-ids))
                              (not (image-placed-p store id)))
                     (remove-image store id))))
               (setf (terminal-image-store-placements store)
                     (delete-if (lambda (placement)
                                  (let ((parent (terminal-image-placement-parent placement)))
                                    (when (and parent
                                               (find-if (lambda (key)
                                                          (and (= (car key) (car parent))
                                                               (or (zerop (cdr parent))
                                                                   (= (cdr key) (cdr parent)))))
                                                        keys)
                                               (not (find-placement store (car parent)
                                                                    (cdr parent))))
                                      (push placement doomed)
                                      (pushnew (terminal-image-placement-image-id placement)
                                               relative-ids)
                                      t)))
                                (terminal-image-store-placements store)))))
    (setf (terminal-image-store-dirty-p store) t)))

(defun placement-visible-rows (store placement)
  (let ((image (gethash (terminal-image-placement-image-id placement)
                        (terminal-image-store-images store))))
    (if image
        (nth-value 1 (placement-cell-size placement image
                                          (terminal-image-store-cell-width store)
                                          (terminal-image-store-cell-height store)))
        1)))

(defun clear-screen-images (store how screen)
  "Erase in display: HOW 2 clears placements on SCREEN, 3 those scrolled
above it, :ALL every placement on SCREEN."
  (let ((rows (terminal-image-store-rows store)))
    (delete-placements-if
     store
     (lambda (placement)
       (and (not (terminal-image-placement-virtual-p placement))
            (null (terminal-image-placement-parent placement))
            (eq screen (terminal-image-placement-screen placement))
            (let* ((top (terminal-image-placement-row placement))
                   (bottom (+ top (placement-visible-rows store placement))))
              (case how
                (2 (and (< top rows) (> bottom 0)))
                (3 (<= bottom 0))
                (t t))))))))

;;; Scrolling

(defun clip-placement-rows (placement first last)
  "Show only rows FIRST..LAST (offsets into PLACEMENT) from now on."
  (let ((clip (terminal-image-placement-row-clip placement)))
    (setf (terminal-image-placement-row-clip placement)
          (if clip
              (cons (max first (car clip)) (min last (cdr clip)))
              (cons first last)))))

(defun note-engine-scroll (store top bottom count screen)
  "Rows TOP..BOTTOM of SCREEN moved up by COUNT (down when negative).
Placements move with the text.  A scroll of the whole screen moves them all
(the primary screen's upward ones into the scrollback), clipping what leaves
the screen; within a scroll region only placements entirely inside it move,
clipped to it.  Placements left with nothing to show are dropped."
  (let* ((rows (terminal-image-store-rows store))
         (whole (and (zerop top) (>= bottom (1- rows))))
         (doomed '()))
    (when (and whole (eq screen (terminal-image-store-active-screen store)))
      (incf (terminal-image-store-reported-scroll store) count))
    (dolist (placement (terminal-image-store-placements store))
      (when (and (eq screen (terminal-image-placement-screen placement))
                 (not (terminal-image-placement-virtual-p placement))
                 (null (terminal-image-placement-parent placement)))
        (let* ((first-row (terminal-image-placement-row placement))
               (height (placement-visible-rows store placement))
               (last-row (+ first-row height -1))
               (new-first (- first-row count))
               (new-last (- last-row count)))
          (flet ((move (low high)
                   ;; Move, keeping only rows LOW..HIGH of the screen.
                   (setf (terminal-image-placement-row placement) new-first)
                   (if (or (and low (< new-last low)) (and high (> new-first high)))
                       (push placement doomed)
                       (clip-placement-rows placement
                                            (if low (max 0 (- low new-first)) 0)
                                            (if high (- high new-first) (1- height))))))
            (cond
              ((and whole (plusp count) (eq screen :primary))
               (setf (terminal-image-placement-row placement) new-first)
               (when (< new-last (- *terminal-image-scrollback-rows*))
                 (push placement doomed)))
              ((and whole (plusp count))
               (move 0 nil))
              ;; Downward: what is in the scrollback stays there.
              ((and whole (>= last-row 0))
               (move nil bottom))
              ((and (not whole) (<= top first-row) (<= last-row bottom))
               (move top bottom)))))))
    (when doomed
      (delete-placements-if store (lambda (placement) (member placement doomed))))
    (setf (terminal-image-store-dirty-p store) t)
    store))

(defun scroll-image-placements (store rows)
  "Move screen-anchored placements when terminal contents scroll up ROWS."
  (unless (zerop rows)
    (dolist (placement (terminal-image-store-placements store))
      (when (and (eq (terminal-image-placement-screen placement)
                     (terminal-image-store-active-screen store))
                 (null (terminal-image-placement-parent placement)))
        (decf (terminal-image-placement-row placement) rows)))
    (setf (terminal-image-store-dirty-p store) t))
  store)

;;; SIXEL cells
;;;
;;; As in xterm and foot, text written over a SIXEL image replaces the image
;;; in those cells, and erasing cells erases it there too.

(defun grid-cell-signature (grid row column)
  (let ((index (grid-index grid row column)))
    (list (aref (grid-codes grid) index) (aref (grid-fg grid) index)
          (aref (grid-bg grid) index) (aref (grid-attrs grid) index))))

(defun sixel-placement-p (placement)
  (terminal-image-placement-erase-on-write-p placement))

(defun capture-sixel-baseline (store placement image grid &optional (row-offset 0))
  "Record the cells under PLACEMENT, ROW-OFFSET rows higher in GRID, as GRID
holds them now."
  (multiple-value-bind (columns rows)
      (placement-cell-size placement image (terminal-image-store-cell-width store)
                           (terminal-image-store-cell-height store))
    (let ((baseline (make-array (* columns rows) :initial-element nil))
          (top (- (terminal-image-placement-row placement) row-offset))
          (left (terminal-image-placement-column placement)))
      (dotimes (dr rows)
        (dotimes (dc columns)
          (let ((row (+ top dr)) (column (+ left dc)))
            (when (and (< -1 row (grid-rows grid)) (< -1 column (grid-columns grid)))
              (setf (svref baseline (+ dc (* dr columns)))
                    (grid-cell-signature grid row column))))))
      (setf (terminal-image-placement-baseline placement) baseline))))

(defun drop-hidden-sixels (store)
  "Delete SIXEL placements no cell of which still shows the image."
  (delete-placements-if
   store
   (lambda (placement)
     (and (sixel-placement-p placement)
          (let ((image (gethash (terminal-image-placement-image-id placement)
                                (terminal-image-store-images store))))
            (or (null image)
                (multiple-value-bind (columns rows)
                    (placement-cell-size placement image
                                         (terminal-image-store-cell-width store)
                                         (terminal-image-store-cell-height store))
                  (null (visible-cell-rectangles
                         (terminal-image-placement-hidden-cells placement) columns rows
                         (terminal-image-placement-row-clip placement))))))))))

(defun hide-sixel-cells (store screen top left bottom right &key except)
  "Hide SIXEL image cells on SCREEN within rows TOP..BOTTOM and columns
LEFT..RIGHT, inclusive."
  (let ((changed nil))
    (dolist (placement (terminal-image-store-placements store))
      (when (and (sixel-placement-p placement)
                 (not (eq placement except))
                 (eq screen (terminal-image-placement-screen placement)))
        (let ((image (gethash (terminal-image-placement-image-id placement)
                              (terminal-image-store-images store)))
              (row0 (terminal-image-placement-row placement))
              (column0 (terminal-image-placement-column placement)))
          (when image
            (multiple-value-bind (columns rows)
                (placement-cell-size placement image
                                     (terminal-image-store-cell-width store)
                                     (terminal-image-store-cell-height store))
              (loop for dr from (max 0 (- top row0)) to (min (1- rows) (- bottom row0))
                    do (loop for dc from (max 0 (- left column0))
                               to (min (1- columns) (- right column0))
                             do (setf (gethash (+ dc (* dr columns))
                                               (terminal-image-placement-hidden-cells
                                                placement))
                                      t
                                      changed t))))))))
    (when changed
      (drop-hidden-sixels store)
      (setf (terminal-image-store-dirty-p store) t))
    changed))

(defun erase-overwritten-sixel-cells (store grid cell-width cell-height)
  "Hide SIXEL cells whose text in GRID differs from when the image was drawn.
Cells first seen now (scrolled in from below) take their current text."
  (let ((changed nil))
    (dolist (placement (terminal-image-store-placements store))
      (when (and (sixel-placement-p placement)
                 (eq (terminal-image-placement-screen placement)
                     (terminal-image-store-active-screen store)))
        (let ((image (gethash (terminal-image-placement-image-id placement)
                              (terminal-image-store-images store))))
          (when image
            (multiple-value-bind (columns rows)
                (placement-cell-size placement image cell-width cell-height)
              (let ((baseline (terminal-image-placement-baseline placement))
                    (hidden (terminal-image-placement-hidden-cells placement))
                    (top (terminal-image-placement-row placement))
                    (left (terminal-image-placement-column placement)))
                (unless (and baseline (= (length baseline) (* columns rows)))
                  (setf baseline (make-array (* columns rows) :initial-element nil)
                        (terminal-image-placement-baseline placement) baseline))
                ;; Only cells on screen can have changed.
                (loop for dr from (max 0 (- top)) below (min rows (- (grid-rows grid) top))
                      for row = (+ top dr)
                      do (loop for dc from (max 0 (- left))
                                 below (min columns (- (grid-columns grid) left))
                               for key = (+ dc (* dr columns))
                               unless (gethash key hidden)
                                 do (let ((now (grid-cell-signature grid row (+ left dc)))
                                          (then (svref baseline key)))
                                      (cond ((null then) (setf (svref baseline key) now))
                                            ((not (equal now then))
                                             (setf (gethash key hidden) t
                                                   changed t))))))))))))
    (when changed
      (drop-hidden-sixels store)
      (setf (terminal-image-store-dirty-p store) t))
    changed))

;;; Kitty graphics

(define-condition kitty-command-error (error)
  ((code :initarg :code :reader kitty-command-error-code)
   (message :initarg :message :reader kitty-command-error-message))
  (:report (lambda (condition stream)
             (format stream "~a:~a" (kitty-command-error-code condition)
                     (kitty-command-error-message condition)))))

(defun kitty-fail (code control &rest arguments)
  (error 'kitty-command-error :code code :message (apply #'format nil control arguments)))

(defun kitty-int (command key &optional (default 0))
  (kitty-graphics:graphics-command-integer command key default))

(defun kitty-command-image-id (store command)
  (let ((id (kitty-int command #\i))
        (number (kitty-int command #\I)))
    (values (if (plusp id) id (gethash number (terminal-image-store-numbers store) 0))
            number)))

(defun kitty-printable (message)
  (map 'string (lambda (char)
                 (if (and (<= 32 (char-code char) 126) (char/= char #\;)) char #\Space))
       message))

(defun kitty-response (command status &optional message id)
  "The reply to COMMAND, or NIL: replies need an image id or number and q=1
silences successes, q=2 everything."
  (let ((quiet (or (ignore-errors (kitty-int command #\q)) 0))
        (number (or (ignore-errors (kitty-int command #\I)) 0))
        (requested (or (ignore-errors (kitty-int command #\i)) 0))
        (placement (or (ignore-errors (kitty-int command #\p)) 0))
        (ok (string= status "OK")))
    (unless (or (and ok (>= quiet 1))
                (>= quiet 2)
                (and (zerop requested) (zerop number)))
      (let ((controls '())
            (id (if (plusp (or id 0)) id requested)))
        (when (plusp id) (push (cons #\i (princ-to-string id)) controls))
        (when (plusp number) (push (cons #\I (princ-to-string number)) controls))
        (when (plusp placement) (push (cons #\p (princ-to-string placement)) controls))
        (kitty-graphics:encode-graphics-response
         (nreverse controls) status (and message (kitty-printable message)))))))

(defun kitty-palette-lookup (palette)
  "Kitty's indexed-color lookup through PALETTE: its 256 colors, but for
248-255, which are the background, foreground and other theme colors."
  (lambda (index)
    (let ((rgb (case index
                 ((248 253) (palette-background palette))
                 ((249 251 255) (palette-foreground palette))
                 (250 (palette-selection palette))
                 (252 (palette-cursor palette))
                 (254 #x0087BD)
                 (t (aref (palette-colors palette) index)))))
      (values (ldb (byte 8 16) rgb) (ldb (byte 8 8) rgb) (ldb (byte 8 0) rgb)))))

(defun image-store-palette (store)
  (or (terminal-image-store-palette store) *palette*))

(defun kitty-indexed-p (command)
  (member (kitty-int command #\f) '(8 16)))

(defvar *kitty-decoded* nil
  "(COMMAND . RESULT) for a command whose pixels were decoded beforehand by
PREPARE-KITTY-COMMAND: RESULT is (RGBA WIDTH HEIGHT), or the condition
decoding signalled.")

(defun kitty-command-rgba (command &key width height (palette *palette*))
  "Decode COMMAND's pixels.  WIDTH and HEIGHT stand in for missing s and v;
PALETTE colors indexed images."
  (let ((decoded *kitty-decoded*))
    (when (and decoded (eq (car decoded) command))
      (let ((result (cdr decoded)))
        (if (typep result 'condition)
            (error result)
            (return-from kitty-command-rgba (values-list result))))))
  (when (and width (zerop (kitty-int command #\s)))
    (push (cons #\s (princ-to-string width)) (kitty-graphics:graphics-command-controls command)))
  (when (and height (zerop (kitty-int command #\v)))
    (push (cons #\v (princ-to-string height)) (kitty-graphics:graphics-command-controls command)))
  (kitty-graphics:graphics-command-rgba command :palette (kitty-palette-lookup palette)))

(defun set-image-store-palette (store palette)
  "Draw STORE's indexed-color images with PALETTE (NIL for *PALETTE*)."
  (unless (eq palette (terminal-image-store-palette store))
    (setf (terminal-image-store-palette store) palette)
    (loop for image being the hash-values of (terminal-image-store-images store)
          for command = (terminal-image-indexed image)
          when command
            do (let ((rgba (kitty-command-rgba command :palette (image-store-palette store))))
                 (setf (terminal-image-rgba image) rgba
                       (terminal-image-frame-rgba (aref (terminal-image-frames image) 0)) rgba
                       (terminal-image-store-dirty-p store) t))))
  store)

(defun kitty-move-cursor (engine columns rows)
  "Move the cursor past an image: down ROWS - 1 lines, scrolling at the
bottom margin, then right COLUMNS cells (each at most a screenful)."
  (setf rows (min rows (engine-rows engine))
        columns (min columns (engine-columns engine)))
  (engine-feed-string
   engine
   (with-output-to-string (out)
     (dotimes (i (max 0 (1- rows)))
       (write-char (code-char 27) out)
       (write-char #\D out))
     (when (plusp columns)
       (format out "~c[~dC" (code-char 27) columns)))))

(defun place-image (store id row column command &key (screen :primary) image)
  "Create the placement COMMAND describes for image ID at ROW, COLUMN."
  (let* ((image (or image
                    (gethash id (terminal-image-store-images store))
                    (kitty-fail "ENOENT" "image ~d not found" id)))
         (placement-id (if (terminal-image-anonymous-p image) 0 (kitty-int command #\p)))
         (parent-image (kitty-int command #\P))
         (parent-placement (kitty-int command #\Q))
         (virtual-p (= 1 (kitty-int command #\U)))
         (placement (make-terminal-image-placement
                     :image-id id :placement-id placement-id :row row :column column
                     :columns (let ((n (kitty-int command #\c))) (and (plusp n) n))
                     :rows (let ((n (kitty-int command #\r))) (and (plusp n) n))
                     :source-x (kitty-int command #\x)
                     :source-y (kitty-int command #\y)
                     :source-width (let ((n (kitty-int command #\w))) (and (plusp n) n))
                     :source-height (let ((n (kitty-int command #\h))) (and (plusp n) n))
                     ;; Offsets lie within the first cell.
                     :x-offset (min (max 0 (kitty-int command #\X))
                                    (1- (terminal-image-store-cell-width store)))
                     :y-offset (min (max 0 (kitty-int command #\Y))
                                    (1- (terminal-image-store-cell-height store)))
                     :z-index (kitty-int command #\z)
                     :virtual-p virtual-p
                     :screen screen
                     :parent (and (plusp parent-image) (cons parent-image parent-placement))
                     :h-offset (kitty-int command #\H)
                     :v-offset (kitty-int command #\V))))
    (multiple-value-bind (sx sy sw sh) (placement-source-rect placement image)
      (declare (ignore sx sy))
      (unless (and (plusp sw) (plusp sh))
        (kitty-fail "EINVAL" "source rectangle is outside the image")))
    (multiple-value-bind (columns rows)
        (placement-cell-size placement image (terminal-image-store-cell-width store)
                             (terminal-image-store-cell-height store))
      (unless (and (<= columns +max-placement-cells+) (<= rows +max-placement-cells+))
        (kitty-fail "EINVAL" "placement of ~dx~d cells is too large" columns rows)))
    (when (terminal-image-placement-parent placement)
      (when virtual-p
        (kitty-fail "EINVAL" "virtual placements cannot be relative"))
      (let ((parent (find-placement store parent-image parent-placement)))
        (unless parent
          (kitty-fail "ENOPARENT" "parent placement not found"))
        ;; Walk up from the parent: meeting this placement is a cycle.
        (loop for depth from 1
              for node = parent
                then (let ((key (terminal-image-placement-parent node)))
                       (and key (find-placement store (car key) (cdr key))))
              while node
              do (when (and (plusp placement-id)
                            (= id (terminal-image-placement-image-id node))
                            (= placement-id (terminal-image-placement-placement-id node)))
                   (kitty-fail "ECYCLE" "relative placement would form a cycle"))
                 (when (> depth *kitty-max-relative-depth*)
                   (kitty-fail "ETOODEEP" "relative placement chain too deep")))
        (setf (terminal-image-placement-screen placement)
              (placement-screen store parent))))
    (when (plusp placement-id)
      ;; Replacing a placement moves it; its relative placements follow.
      (setf (terminal-image-store-placements store)
            (delete-if (lambda (old)
                         (and (= id (terminal-image-placement-image-id old))
                              (= placement-id (terminal-image-placement-placement-id old))))
                       (terminal-image-store-placements store))))
    (push placement (terminal-image-store-placements store))
    (setf (terminal-image-access image) (incf (terminal-image-store-clock store)))
    (setf (terminal-image-store-dirty-p store) t)
    placement))

(defun kitty-put (store command engine id image)
  (multiple-value-bind (row column) (engine-cursor-position engine)
    (let ((placement (place-image store id row column command
                                  :screen (if (engine-mode engine :alternate-screen)
                                              :alternate :primary)
                                  :image image)))
      (unless (or (terminal-image-placement-virtual-p placement)
                  (terminal-image-placement-parent placement)
                  (= 1 (kitty-int command #\C)))
        (multiple-value-bind (columns rows)
            (placement-cell-size placement image
                                 (terminal-image-store-cell-width store)
                                 (terminal-image-store-cell-height store))
          (kitty-move-cursor engine columns rows)))
      placement)))

(defun kitty-transmit (store command engine)
  (let* ((requested (kitty-int command #\i))
         (number (kitty-int command #\I))
         (anonymous-p (and (zerop requested) (zerop number)))
         (display-p (char= #\T (kitty-graphics:graphics-command-action command))))
    (multiple-value-bind (rgba width height)
        (kitty-command-rgba command :palette (image-store-palette store))
      (when (and anonymous-p (not display-p))
        ;; Nothing could ever refer to it.
        (return-from kitty-transmit (kitty-response command "OK")))
      (let* ((id (image-store-id store requested number))
             (image (store-image store id number width height rgba
                                 :anonymous-p anonymous-p)))
        (when (kitty-indexed-p command)
          (setf (terminal-image-indexed image) command))
        (when display-p
          (kitty-put store command engine id image))
        (kitty-response command "OK" nil id)))))

(defun kitty-find-image (store command)
  (let* ((id (kitty-command-image-id store command))
         (image (and (plusp id) (gethash id (terminal-image-store-images store)))))
    (unless image
      (kitty-fail "ENOENT" "image not found"))
    (values image id)))

(defun delete-kitty-images (store command row column
                            &key (screen (terminal-image-store-active-screen store))
                              (cell-width (terminal-image-store-cell-width store))
                              (cell-height (terminal-image-store-cell-height store)))
  (kitty-graphics:graphics-decoder-abort (terminal-image-store-kitty-decoder store))
  (let* ((selector (char (kitty-graphics:graphics-command-value command #\d "a") 0))
         (free-p (upper-case-p selector))
         (screen-rows (terminal-image-store-rows store)))
    (labels ((extent (placement)
               ;; (VALUES TOP LEFT ROWS COLUMNS) or NIL.
               (let ((image (gethash (terminal-image-placement-image-id placement)
                                     (terminal-image-store-images store))))
                 (when (and image (not (terminal-image-placement-virtual-p placement)))
                   (multiple-value-bind (top left) (placement-origin store placement)
                     (when top
                       (multiple-value-bind (columns rows)
                           (placement-cell-size placement image cell-width cell-height)
                         (values top left rows columns)))))))
             (on-screen-p (placement)
               (eq screen (placement-screen store placement)))
             (intersects (placement target-row target-column)
               (and (on-screen-p placement)
                    (multiple-value-bind (top left rows columns) (extent placement)
                      (and top
                           (or (null target-row)
                               (and (<= top target-row) (< target-row (+ top rows))))
                           (or (null target-column)
                               (and (<= left target-column)
                                    (< target-column (+ left columns))))))))
             (by-id (id pid)
               (delete-placements-if
                store
                (lambda (placement)
                  (and (= id (terminal-image-placement-image-id placement))
                       (or (zerop pid)
                           (= pid (terminal-image-placement-placement-id placement)))))
                :free-p free-p)
               (when (and free-p (plusp id) (not (image-placed-p store id)))
                 (remove-image store id)))
             (target (key) (1- (kitty-int command key 1))))
      (case (char-downcase selector)
        (#\a (delete-placements-if
              store
              (lambda (placement)
                (and (on-screen-p placement)
                     (multiple-value-bind (top left rows) (extent placement)
                       (declare (ignore left))
                       (and top (< top screen-rows) (> (+ top rows) 0)))))
              :free-p free-p))
        (#\i (let ((id (kitty-int command #\i)))
               (when (plusp id) (by-id id (kitty-int command #\p)))))
        (#\n (let ((id (gethash (kitty-int command #\I)
                                (terminal-image-store-numbers store) 0)))
               (when (plusp id) (by-id id (kitty-int command #\p)))))
        (#\c (delete-placements-if store (lambda (p) (intersects p row column))
                                   :free-p free-p))
        (#\p (let ((r (target #\y)) (c (target #\x)))
               (delete-placements-if store (lambda (p) (intersects p r c)) :free-p free-p)))
        (#\q (let ((r (target #\y)) (c (target #\x)) (z (kitty-int command #\z)))
               (delete-placements-if
                store (lambda (p) (and (intersects p r c)
                                       (= z (terminal-image-placement-z-index p))))
                :free-p free-p)))
        (#\r (let ((low (kitty-int command #\x)) (high (kitty-int command #\y)))
               (dolist (id (loop for id being the hash-keys
                                   of (terminal-image-store-images store)
                                 when (<= low id high) collect id))
                 (by-id id 0))))
        (#\x (let ((c (target #\x)))
               (delete-placements-if store (lambda (p) (intersects p nil c)) :free-p free-p)))
        (#\y (let ((r (target #\y)))
               (delete-placements-if store (lambda (p) (intersects p r nil)) :free-p free-p)))
        (#\z (let ((z (kitty-int command #\z)))
               (delete-placements-if
                store (lambda (p) (and (on-screen-p p)
                                       (not (terminal-image-placement-virtual-p p))
                                       (= z (terminal-image-placement-z-index p))))
                :free-p free-p)))
        (#\f (let ((image (gethash (kitty-command-image-id store command)
                                   (terminal-image-store-images store))))
               (when image (delete-animation-frames store image))))
        (t nil))
      (setf (terminal-image-store-dirty-p store) t))))

;;; Kitty animation

(defun now-ms ()
  (values (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second)))

(defun image-frame (image number)
  "The frame with 1-based NUMBER, or NIL."
  (let ((frames (terminal-image-frames image)))
    (and (<= 1 number (length frames)) (aref frames (1- number)))))

(defun show-image-frame (store image index)
  (setf (terminal-image-current-frame image) index
        (terminal-image-rgba image)
        (terminal-image-frame-rgba (aref (terminal-image-frames image) index))
        (terminal-image-store-dirty-p store) t))

(defun delete-animation-frames (store image)
  (let ((frames (terminal-image-frames image)))
    (decf (terminal-image-store-bytes store)
          (* 4 (terminal-image-width image) (terminal-image-height image)
             (max 0 (1- (length frames)))))
    (setf (fill-pointer frames) (min 1 (length frames))
          (terminal-image-animation image) :stopped
          (terminal-image-frame-deadline image) nil
          (terminal-image-loops image) 0)
    (show-image-frame store image 0)))

(defun rgba-color-pixel (color)
  "Kitty's 32-bit RGBA color as four octets."
  (list (ldb (byte 8 24) color) (ldb (byte 8 16) color)
        (ldb (byte 8 8) color) (ldb (byte 8 0) color)))

(defun compose-pixels (target target-width source source-width
                       source-x source-y target-x target-y width height replace-p)
  "Draw WIDTH x HEIGHT pixels of SOURCE at (SOURCE-X, SOURCE-Y) onto TARGET at
(TARGET-X, TARGET-Y), replacing or alpha blending (source over)."
  (declare (type (simple-array (unsigned-byte 8) (*)) target source)
           (type fixnum target-width source-width source-x source-y
                 target-x target-y width height))
  (dotimes (y height target)
    (let ((s (* 4 (+ source-x (* source-width (+ source-y y)))))
          (d (* 4 (+ target-x (* target-width (+ target-y y))))))
      (declare (type fixnum s d))
      (if replace-p
          (replace target source :start1 d :start2 s :end2 (+ s (* 4 width)))
          (dotimes (x width)
            (let* ((sa (aref source (+ s 3)))
                   (da (aref target (+ d 3)))
                   (out-a (+ sa (floor (* da (- 255 sa)) 255))))
              (declare (type fixnum sa da out-a))
              (cond
                ((= sa 255) (replace target source :start1 d :end1 (+ d 4) :start2 s))
                ((zerop sa))
                (t
                 (dotimes (c 3)
                   (setf (aref target (+ d c))
                         (if (zerop out-a)
                             0
                             (min 255
                                  (round (+ (* (aref source (+ s c)) sa)
                                            (floor (* (aref target (+ d c)) da (- 255 sa)) 255))
                                         out-a)))))
                 (setf (aref target (+ d 3)) out-a)))
              (incf s 4)
              (incf d 4)))))))

(defun kitty-frame (store command)
  "a=f: add a frame to an image or edit one."
  (multiple-value-bind (image id) (kitty-find-image store command)
    ;; Frames are kept as RGBA: the image no longer follows the palette.
    (setf (terminal-image-indexed image) nil)
    (let* ((width (terminal-image-width image))
           (height (terminal-image-height image))
           (frames (terminal-image-frames image))
           (edit (kitty-int command #\r))
           (base (kitty-int command #\c))
           (gap (kitty-int command #\z))
           (x (kitty-int command #\x))
           (y (kitty-int command #\y))
           (replace-p (= 1 (kitty-int command #\X)))
           (has-data (plusp (length (kitty-graphics:graphics-command-data command)))))
      (when (and (plusp edit) (null (image-frame image edit)))
        (kitty-fail "ENOENT" "frame ~d not found" edit))
      (when (and (plusp base) (null (image-frame image base)))
        (kitty-fail "ENOENT" "frame ~d not found" base))
      (multiple-value-bind (data data-width data-height)
          (if has-data
              (kitty-command-rgba command :width (- width x) :height (- height y)
                                          :palette (image-store-palette store))
              (values nil 0 0))
        (unless (and (<= 0 x) (<= 0 y)
                     (<= (+ x data-width) width) (<= (+ y data-height) height))
          (kitty-fail "EINVAL" "frame rectangle is outside the image"))
        (let ((canvas (cond
                        ((plusp edit)
                         (copy-seq (terminal-image-frame-rgba (image-frame image edit))))
                        ((plusp base)
                         (copy-seq (terminal-image-frame-rgba (image-frame image base))))
                        (t
                         (let ((canvas (make-array (* 4 width height)
                                                   :element-type '(unsigned-byte 8)))
                               (pixel (rgba-color-pixel (kitty-int command #\Y))))
                           (unless (every #'zerop pixel)
                             (loop for i from 0 below (length canvas) by 4
                                   do (replace canvas pixel :start1 i)))
                           canvas)))))
          (when data
            (compose-pixels canvas width data data-width 0 0 x y data-width data-height
                            replace-p))
          (if (plusp edit)
              (let ((frame (image-frame image edit)))
                (setf (terminal-image-frame-rgba frame) canvas)
                (unless (zerop gap) (setf (terminal-image-frame-gap frame) gap))
                (when (= (1- edit) (terminal-image-current-frame image))
                  (show-image-frame store image (1- edit))))
              (progn
                (evict-images-to-fit store (length canvas))
                (unless (gethash id (terminal-image-store-images store))
                  (kitty-fail "ENOSPC" "image evicted for lack of space"))
                (vector-push-extend
                 (make-terminal-image-frame
                  :rgba canvas :gap (if (zerop gap) +kitty-default-frame-gap+ gap))
                 frames)
                (incf (terminal-image-store-bytes store) (length canvas))
                ;; A loading animation waiting on the last frame resumes.
                (when (and (eq :loading (terminal-image-animation image))
                           (null (terminal-image-frame-deadline image)))
                  (setf (terminal-image-frame-deadline image) (now-ms)))))
          (kitty-response command "OK" nil id))))))

(defun kitty-animate (store command)
  "a=a: control an image's animation."
  (multiple-value-bind (image id) (kitty-find-image store command)
    (let ((state (kitty-int command #\s))
          (loops (kitty-int command #\v))
          (current (kitty-int command #\c))
          (frame-number (kitty-int command #\r))
          (gap (kitty-int command #\z)))
      (when (plusp frame-number)
        (let ((frame (or (image-frame image frame-number)
                         (kitty-fail "ENOENT" "frame ~d not found" frame-number))))
          (unless (zerop gap) (setf (terminal-image-frame-gap frame) gap))))
      (when (plusp loops)
        (setf (terminal-image-max-loops image) (1- loops)))
      (when (plusp current)
        (unless (image-frame image current)
          (kitty-fail "ENOENT" "frame ~d not found" current))
        (show-image-frame store image (1- current))
        (when (terminal-image-frame-deadline image)
          (setf (terminal-image-frame-deadline image)
                (+ (now-ms) (max 0 (terminal-image-frame-gap
                                     (image-frame image current)))))))
      (case state
        (1 (setf (terminal-image-animation image) :stopped
                 (terminal-image-frame-deadline image) nil
                 (terminal-image-loops image) 0))
        ((2 3)
         (let ((was (terminal-image-animation image)))
           (setf (terminal-image-animation image) (if (= state 2) :loading :running))
           (when (or (eq was :stopped) (null (terminal-image-frame-deadline image)))
             (setf (terminal-image-frame-deadline image)
                   (+ (now-ms)
                      (max 0 (terminal-image-frame-gap
                              (aref (terminal-image-frames image)
                                    (terminal-image-current-frame image)))))))))
        (t nil))
      (kitty-response command "OK" nil id))))

(defun kitty-compose (store command)
  "a=c: compose a rectangle of one frame onto another."
  (multiple-value-bind (image id) (kitty-find-image store command)
    (setf (terminal-image-indexed image) nil)
    (let* ((width (terminal-image-width image))
           (height (terminal-image-height image))
           (source-number (max 1 (kitty-int command #\r 1)))
           (target-number (max 1 (kitty-int command #\c 1)))
           (source (or (image-frame image source-number)
                       (kitty-fail "ENOENT" "frame ~d not found" source-number)))
           (target (or (image-frame image target-number)
                       (kitty-fail "ENOENT" "frame ~d not found" target-number)))
           (w (let ((n (kitty-int command #\w))) (if (plusp n) n width)))
           (h (let ((n (kitty-int command #\h))) (if (plusp n) n height)))
           (sx (kitty-int command #\X)) (sy (kitty-int command #\Y))
           (dx (kitty-int command #\x)) (dy (kitty-int command #\y)))
      (unless (and (<= 0 sx) (<= 0 sy) (<= 0 dx) (<= 0 dy)
                   (<= (+ sx w) width) (<= (+ sy h) height)
                   (<= (+ dx w) width) (<= (+ dy h) height))
        (kitty-fail "EINVAL" "rectangle is outside the image"))
      (when (and (eq source target)
                 (< sx (+ dx w)) (< dx (+ sx w)) (< sy (+ dy h)) (< dy (+ sy h)))
        (kitty-fail "EINVAL" "source and destination overlap"))
      (let ((canvas (copy-seq (terminal-image-frame-rgba target))))
        (compose-pixels canvas width (terminal-image-frame-rgba source) width
                        sx sy dx dy w h (= 1 (kitty-int command #\C)))
        (setf (terminal-image-frame-rgba target) canvas)
        (when (= (1- target-number) (terminal-image-current-frame image))
          (show-image-frame store image (1- target-number))))
      (kitty-response command "OK" nil id))))

(defun advance-image-animation (store image now)
  "Step IMAGE's animation to NOW.  Return true when its frame changed."
  (let* ((frames (terminal-image-frames image))
         (count (length frames))
         (start (terminal-image-current-frame image))
         (index start)
         (instant 0))
    (loop
      (let ((deadline (terminal-image-frame-deadline image)))
        (unless (and deadline (>= now deadline)
                     (not (eq :stopped (terminal-image-animation image))))
          (return))
        (when (< count 2)
          (setf (terminal-image-frame-deadline image) nil)
          (return))
        (let ((next (1+ index)))
          (when (>= next count)
            (when (eq :loading (terminal-image-animation image))
              ;; Wait at the last frame for more to arrive.
              (setf (terminal-image-frame-deadline image) nil)
              (return))
            (incf (terminal-image-loops image))
            (when (and (plusp (terminal-image-max-loops image))
                       (>= (terminal-image-loops image) (terminal-image-max-loops image)))
              (setf (terminal-image-animation image) :stopped
                    (terminal-image-frame-deadline image) nil
                    (terminal-image-loops image) 0)
              (return))
            (setf next 0))
          (setf index next)
          (let ((gap (terminal-image-frame-gap (aref frames index))))
            ;; Frames without a gap are skipped; a cycle of them stops.
            (if (plusp gap) (setf instant 0) (incf instant))
            (when (> instant count)
              (setf (terminal-image-animation image) :stopped
                    (terminal-image-frame-deadline image) nil)
              (return))
            (setf (terminal-image-frame-deadline image)
                  ;; Keep the pace, unless far behind.
                  (+ (if (> (- now deadline) 1000) now deadline) (max 0 gap)))))))
    (unless (= index start)
      (show-image-frame store image index)
      t)))

(defun advance-terminal-image-animations (store &optional (now (now-ms)))
  "Step every running animation.  Return true when a shown frame changed."
  (let ((changed nil))
    (loop for image being the hash-values of (terminal-image-store-images store)
          when (and (terminal-image-frame-deadline image)
                    (advance-image-animation store image now)
                    (image-placed-p store (terminal-image-id image)))
            do (setf changed t))
    changed))

(defun handle-kitty-command (store payload engine)
  "Execute the Kitty graphics command PAYLOAD (an APC body).  Return the reply
for the child, or NIL."
  (multiple-value-bind (command reply decoded) (prepare-kitty-command store payload)
    (if command
        (run-kitty-command store command engine decoded)
        reply)))

(defun predecode-kitty-pixels (store command)
  "Decode the pixels COMMAND transmits, as RUN-KITTY-COMMAND will want them:
a list of the RGBA, width and height, the condition decoding signalled, or
NIL for a command that transmits nothing.  Frames (a=f) are left to
RUN-KITTY-COMMAND, as their size depends on the image they belong to."
  (case (kitty-graphics:graphics-command-action command)
    ((#\t #\T #\q)
     (handler-case (multiple-value-list
                    (kitty-command-rgba command :palette (image-store-palette store)))
       (error (condition) condition)))))

(defun prepare-kitty-command (store payload)
  "The slow half of HANDLE-KITTY-COMMAND, which may run without the engine
lock: it uses only STORE's chunk decoder and palette, which the renderer
never touches.  Join PAYLOAD to the chunks before it and decode the image.  Return the complete command (or NIL while chunks
are still due), the reply when PAYLOAD failed, and the decoded pixels to
pass to RUN-KITTY-COMMAND."
  (let* ((decoder (terminal-image-store-kitty-decoder store))
         (header (ignore-errors (kitty-graphics:parse-graphics-command payload
                                                                       :decode-data nil)))
         (command nil))
    ;; A delete aborts a chunked upload instead of continuing it.
    (when (and header (kitty-graphics:graphics-decoder-pending-p decoder)
               (equal "d" (kitty-graphics:graphics-command-value header #\a)))
      (kitty-graphics:graphics-decoder-abort decoder))
    (flet ((fail (code message)
             (and header (kitty-response header code message))))
      (handler-case
          (setf command (kitty-graphics:graphics-decoder-feed decoder payload))
        (kitty-graphics:graphics-protocol-error (condition)
          (return-from prepare-kitty-command
            (values nil (fail (kitty-graphics:graphics-protocol-error-code condition)
                              (kitty-graphics:graphics-protocol-error-message condition)))))
        (error (condition)
          (return-from prepare-kitty-command
            (values nil (fail "EINVAL" (princ-to-string condition))))))
      (when command
        (let ((decoded (predecode-kitty-pixels store command)))
          (values command nil (and decoded (cons command decoded))))))))

(defun run-kitty-command (store command engine &optional decoded)
  "Execute COMMAND, from PREPARE-KITTY-COMMAND with its DECODED pixels.
Return the reply for the child, or NIL."
  (let ((*kitty-decoded* decoded))
    (handler-case
        (progn
          (when (and (plusp (kitty-int command #\i)) (plusp (kitty-int command #\I)))
            (kitty-fail "EINVAL" "i and I are mutually exclusive"))
          (case (kitty-graphics:graphics-command-action command)
            ((#\t #\T) (kitty-transmit store command engine))
            (#\p (multiple-value-bind (image id) (kitty-find-image store command)
                   (kitty-put store command engine id image)
                   (kitty-response command "OK" nil id)))
            (#\q (kitty-command-rgba command)
             (kitty-response command "OK"))
            (#\d (multiple-value-bind (row column) (engine-cursor-position engine)
                   (delete-kitty-images store command row column
                                        :screen (if (engine-mode engine :alternate-screen)
                                                    :alternate :primary)))
             nil)
            (#\f (kitty-frame store command))
            (#\a (kitty-animate store command))
            (#\c (kitty-compose store command))
            (t (kitty-fail "EINVAL" "unknown action"))))
      (kitty-command-error (condition)
        (kitty-response command (kitty-command-error-code condition)
                        (kitty-command-error-message condition)))
      (kitty-graphics:graphics-protocol-error (condition)
        (kitty-response command (kitty-graphics:graphics-protocol-error-code condition)
                        (kitty-graphics:graphics-protocol-error-message condition)))
      (error (condition)
        (kitty-response command "EINVAL" (princ-to-string condition))))))

;;; Unicode placeholders

;;; The row, column and image id diacritics, from kitty's
;;; gen/rowcolumn-diacritics.txt (github.com/kovidgoyal/kitty at
;;; a79dd3996ac55e1c89c8cc916e3a598739761455): the Nth entry encodes N.
(defparameter +placeholder-diacritics+
  #(#x0305 #x030D #x030E #x0310 #x0312 #x033D #x033E #x033F #x0346 #x034A
  #x034B #x034C #x0350 #x0351 #x0352 #x0357 #x035B #x0363 #x0364 #x0365
  #x0366 #x0367 #x0368 #x0369 #x036A #x036B #x036C #x036D #x036E #x036F
  #x0483 #x0484 #x0485 #x0486 #x0487 #x0592 #x0593 #x0594 #x0595 #x0597
  #x0598 #x0599 #x059C #x059D #x059E #x059F #x05A0 #x05A1 #x05A8 #x05A9
  #x05AB #x05AC #x05AF #x05C4 #x0610 #x0611 #x0612 #x0613 #x0614 #x0615
  #x0616 #x0617 #x0657 #x0658 #x0659 #x065A #x065B #x065D #x065E #x06D6
  #x06D7 #x06D8 #x06D9 #x06DA #x06DB #x06DC #x06DF #x06E0 #x06E1 #x06E2
  #x06E4 #x06E7 #x06E8 #x06EB #x06EC #x0730 #x0732 #x0733 #x0735 #x0736
  #x073A #x073D #x073F #x0740 #x0741 #x0743 #x0745 #x0747 #x0749 #x074A
  #x07EB #x07EC #x07ED #x07EE #x07EF #x07F0 #x07F1 #x07F3 #x0816 #x0817
  #x0818 #x0819 #x081B #x081C #x081D #x081E #x081F #x0820 #x0821 #x0822
  #x0823 #x0825 #x0826 #x0827 #x0829 #x082A #x082B #x082C #x082D #x0951
  #x0953 #x0954 #x0F82 #x0F83 #x0F86 #x0F87 #x135D #x135E #x135F #x17DD
  #x193A #x1A17 #x1A75 #x1A76 #x1A77 #x1A78 #x1A79 #x1A7A #x1A7B #x1A7C
  #x1B6B #x1B6D #x1B6E #x1B6F #x1B70 #x1B71 #x1B72 #x1B73 #x1CD0 #x1CD1
  #x1CD2 #x1CDA #x1CDB #x1CE0 #x1DC0 #x1DC1 #x1DC3 #x1DC4 #x1DC5 #x1DC6
  #x1DC7 #x1DC8 #x1DC9 #x1DCB #x1DCC #x1DD1 #x1DD2 #x1DD3 #x1DD4 #x1DD5
  #x1DD6 #x1DD7 #x1DD8 #x1DD9 #x1DDA #x1DDB #x1DDC #x1DDD #x1DDE #x1DDF
  #x1DE0 #x1DE1 #x1DE2 #x1DE3 #x1DE4 #x1DE5 #x1DE6 #x1DFE #x20D0 #x20D1
  #x20D4 #x20D5 #x20D6 #x20D7 #x20DB #x20DC #x20E1 #x20E7 #x20E9 #x20F0
  #x2CEF #x2CF0 #x2CF1 #x2DE0 #x2DE1 #x2DE2 #x2DE3 #x2DE4 #x2DE5 #x2DE6
  #x2DE7 #x2DE8 #x2DE9 #x2DEA #x2DEB #x2DEC #x2DED #x2DEE #x2DEF #x2DF0
  #x2DF1 #x2DF2 #x2DF3 #x2DF4 #x2DF5 #x2DF6 #x2DF7 #x2DF8 #x2DF9 #x2DFA
  #x2DFB #x2DFC #x2DFD #x2DFE #x2DFF #xA66F #xA67C #xA67D #xA6F0 #xA6F1
  #xA8E0 #xA8E1 #xA8E2 #xA8E3 #xA8E4 #xA8E5 #xA8E6 #xA8E7 #xA8E8 #xA8E9
  #xA8EA #xA8EB #xA8EC #xA8ED #xA8EE #xA8EF #xA8F0 #xA8F1 #xAAB0 #xAAB2
  #xAAB3 #xAAB7 #xAAB8 #xAABE #xAABF #xAAC1 #xFE20 #xFE21 #xFE22 #xFE23
  #xFE24 #xFE25 #xFE26 #x10A0F #x10A38 #x1D185 #x1D186 #x1D187 #x1D188
  #x1D189 #x1D1AA #x1D1AB #x1D1AC #x1D1AD #x1D242 #x1D243 #x1D244))

(defvar *placeholder-diacritic-values*
  (let ((table (make-hash-table)))
    (loop for code across +placeholder-diacritics+
          for value from 0
          do (setf (gethash code table) value))
    table))

(defun placeholder-image-id (color)
  (cond ((color-palette-p color) (color-value color))
        ((color-rgb-p color) (color-value color))
        (t 0)))

(defun scan-image-placeholders (store grid)
  "Return (ROW COLUMN IMAGE-ID PLACEMENT-ID IMAGE-ROW IMAGE-COLUMN) for every
Unicode placeholder cell of GRID, filling in omitted diacritics from the
cell to the left as the protocol prescribes.  The foreground color holds
the image id's low 24 bits and the underline color the placement id."
  (declare (ignore store))
  (let ((cells '())
        (codes (grid-codes grid))
        (columns (grid-columns grid)))
    (dotimes (row (grid-rows grid))
      (let ((previous nil))
        (dotimes (column columns)
          (let* ((index (+ column (* row columns)))
                 (code (aref codes index)))
            (if (/= code +image-placeholder+)
                (setf previous nil)
                (let* ((fg (aref (grid-fg grid) index))
                       (ul (aref (grid-ul grid) index))
                       (marks (loop for char across (or (gethash index (grid-combining grid)) "")
                                    for value = (gethash (char-code char)
                                                         *placeholder-diacritic-values*)
                                    when value collect value))
                       (same (and previous (= fg (getf previous :fg))
                                  (= ul (getf previous :ul))))
                       (image-row (first marks))
                       (image-column (second marks))
                       (msb (third marks)))
                  (cond
                    ((null marks)
                     (setf image-row (if same (getf previous :row) 0)
                           image-column (if same (1+ (getf previous :column)) 0)
                           msb (if same (getf previous :msb) 0)))
                    ((null (cdr marks))
                     (let ((continues (and same (= image-row (getf previous :row)))))
                       (setf image-column (if continues (1+ (getf previous :column)) 0)
                             msb (if continues (getf previous :msb) 0))))
                    ((null (cddr marks))
                     (setf msb (if (and same (= image-row (getf previous :row))
                                        (= image-column (1+ (getf previous :column))))
                                   (getf previous :msb)
                                   0))))
                  (let ((id (logior (placeholder-image-id fg) (ash msb 24))))
                    (setf previous (list :fg fg :ul ul :row image-row :column image-column
                                         :msb msb))
                    (when (plusp id)
                      (push (list row column id (placeholder-image-id ul) image-row image-column)
                            cells)))))))))
    (nreverse cells)))

(defun placeholder-display-placements (store grid cell-width cell-height)
  "Display placements drawing Unicode placeholder cells, and record the
origins of their virtual placements."
  (clrhash (terminal-image-store-virtual-origins store))
  (when (find-if #'terminal-image-placement-virtual-p (terminal-image-store-placements store))
    (loop for (row column id pid image-row image-column)
            in (scan-image-placeholders store grid)
          for virtual = (find-if (lambda (placement)
                                   (and (terminal-image-placement-virtual-p placement)
                                        (= id (terminal-image-placement-image-id placement))
                                        (or (zerop pid)
                                            (= pid (terminal-image-placement-placement-id
                                                    placement)))))
                                 (terminal-image-store-placements store))
          for image = (and virtual (gethash id (terminal-image-store-images store)))
          when image
            append
            (let* ((key (cons id (terminal-image-placement-placement-id virtual)))
                   (origin (gethash key (terminal-image-store-virtual-origins store)))
                   (top (- row image-row))
                   (left (- column image-column)))
              (setf (gethash key (terminal-image-store-virtual-origins store))
                    (if origin
                        (cons (min top (car origin)) (min left (cdr origin)))
                        (cons top left)))
              (multiple-value-bind (sx sy sw sh) (placement-source-rect virtual image)
                (multiple-value-bind (columns rows)
                    (placement-cell-size virtual image cell-width cell-height)
                  (multiple-value-bind (bx by bw bh)
                      (fit-rectangle sw sh (* columns cell-width) (* rows cell-height))
                    ;; This cell's share of the fitted image.
                    (let* ((cx (* image-column cell-width))
                           (cy (* image-row cell-height))
                           (left (max cx bx))
                           (top (max cy by))
                           (right (min (+ cx cell-width) (+ bx bw)))
                           (bottom (min (+ cy cell-height) (+ by bh))))
                      (when (and (< left right) (< top bottom))
                        (let ((fx0 (+ sx (floor (* (- left bx) sw) bw)))
                              (fy0 (+ sy (floor (* (- top by) sh) bh)))
                              (fx1 (+ sx (ceiling (* (- right bx) sw) bw)))
                              (fy1 (+ sy (ceiling (* (- bottom by) sh) bh))))
                          (list (make-terminal-image-placement
                                 :image-id id :row row :column column
                                 :source-x fx0 :source-y fy0
                                 :source-width (max 1 (- fx1 fx0))
                                 :source-height (max 1 (- fy1 fy0))
                                 :z-index (terminal-image-placement-z-index virtual)
                                 :screen (terminal-image-store-active-screen store)
                                 :draw-rect (list (- left cx) (- top cy)
                                                  (- right left) (- bottom top))))))))))))))

;;; Image layers

(defconstant +kitty-z-below-backgrounds+ (- (expt 2 30))
  "Kitty draws images with a z-index below this under cell backgrounds that
are not the default; other negative z-indexes are drawn over backgrounds
but under text.")

(defun image-layers (records)
  "Split render RECORDS (see TERMINAL-IMAGE-RENDER-LIST) by where they are
drawn: under cell backgrounds, between backgrounds and text, and over text.
Each list keeps the records' order."
  (let ((below-backgrounds '()) (below-text '()) (above '()))
    (dolist (record records)
      (let ((z (terminal-image-placement-z-index (first record))))
        (cond ((< z +kitty-z-below-backgrounds+) (push record below-backgrounds))
              ((minusp z) (push record below-text))
              (t (push record above)))))
    (values (nreverse below-backgrounds) (nreverse below-text) (nreverse above))))

;;; Display snapshots

(defun terminal-image-display-store (store grid cell-width cell-height viewport-offset)
  "A copy of STORE holding only what is visible, for renderers on another
thread: relative placements resolved, Unicode placeholders turned into
placements, and rows shifted down by VIEWPORT-OFFSET rows of scrollback
being shown (NIL hides every image)."
  (let ((display (make-terminal-image-store))
        (placements '()))
    (setf (terminal-image-store-active-screen display) (terminal-image-store-active-screen store)
          (terminal-image-store-cell-width display) cell-width
          (terminal-image-store-cell-height display) cell-height)
    (when viewport-offset
      (dolist (placement (append (and grid (zerop viewport-offset)
                                      (placeholder-display-placements
                                       store grid cell-width cell-height))
                                 (terminal-image-store-placements store)))
        (unless (terminal-image-placement-virtual-p placement)
          (multiple-value-bind (row column) (placement-origin store placement)
            (when (and row
                       (eq (placement-screen store placement)
                           (terminal-image-store-active-screen store)))
              (let ((copy (copy-terminal-image-placement placement))
                    (hidden (terminal-image-placement-hidden-cells placement)))
                (setf (terminal-image-placement-row copy) (+ row viewport-offset)
                      (terminal-image-placement-column copy) column
                      (terminal-image-placement-parent copy) nil
                      (terminal-image-placement-screen copy)
                      (terminal-image-store-active-screen store))
                (when (plusp (hash-table-count hidden))
                  (setf (terminal-image-placement-hidden-cells copy)
                        (let ((table (make-hash-table)))
                          (maphash (lambda (k v) (setf (gethash k table) v)) hidden)
                          table)))
                (push copy placements)
                (let* ((id (terminal-image-placement-image-id placement))
                       (image (gethash id (terminal-image-store-images store))))
                  (when (and image (not (gethash id (terminal-image-store-images display))))
                    (setf (gethash id (terminal-image-store-images display))
                          (copy-terminal-image image))))))))))
    (setf (terminal-image-store-placements display) (nreverse placements))
    display))

;;; SIXEL

(defun sixel-header-parameters (payload)
  "The numeric parameters of the DCS header ESC P P1;P2;P3 q."
  (let* ((end (or (position (char-code #\q) payload) 2))
         (text (map 'string #'code-char (subseq payload (min 2 end) end))))
    (loop with start = 0
          for semicolon = (position #\; text :start start)
          collect (or (parse-integer text :start start :end semicolon :junk-allowed t) 0)
          while semicolon
          do (setf start (1+ semicolon)))))

(defparameter +max-sixel-octets+ (* 256 1024 1024)
  "Largest RGBA a stretched SIXEL image may take.")

(defun sixel-pixel-aspect (payload)
  "The pixel aspect ratio, height over width, that PAYLOAD's raster
attributes (\"Pan;Pad;Ph;Pv right after the q) declare, or 1.  Like xterm
and foot, the aspect ratio P1 selects is ignored: programs send P1 = 0,
which means 2:1, for square pixels."
  (let ((start (position (char-code #\q) payload)))
    (if (and start (< (1+ start) (length payload))
             (= (aref payload (1+ start)) (char-code #\")))
        (let* ((text (map 'string #'code-char
                          (subseq payload (+ 2 start) (min (length payload) (+ 2 start 64)))))
               (semicolon (position #\; text))
               (pan (parse-integer text :end semicolon :junk-allowed t))
               (pad (and semicolon (parse-integer text :start (1+ semicolon) :junk-allowed t))))
          (if (and pan pad (plusp pan) (plusp pad))
              (max 1/8 (min 8 (/ pan pad)))
              1))
        1)))

(defun scale-rgba (rgba width height new-width new-height)
  "RGBA (WIDTH x HEIGHT) resized to NEW-WIDTH x NEW-HEIGHT, nearest pixel."
  (let ((result (make-array (* 4 new-width new-height) :element-type '(unsigned-byte 8))))
    (dotimes (y new-height result)
      (let ((source-row (* width (floor (* y height) new-height))))
        (dotimes (x new-width)
          (replace result rgba :start1 (* 4 (+ x (* y new-width)))
                               :start2 (* 4 (+ source-row (floor (* x width) new-width)))
                               :end2 (+ 4 (* 4 (+ source-row (floor (* x width) new-width))))))))))

(defun decode-sixel-payload (payload)
  "Decode the SIXEL image PAYLOAD (a whole DCS string) into a list of its
RGBA, width and height, or return the condition decoding signalled.  Pixels
that are not square, by the raster attributes, are stretched to squares.
This touches nothing the renderer reads, so it may run without the engine
lock."
  (handler-case
      (let* ((image (cl-libsixel:decode-sixel payload))
             (transparent-p (eql 1 (second (sixel-header-parameters payload))))
             (rgba (cl-libsixel:sixel-image-rgba image
                                                 :background (if transparent-p :transparent 0)))
             (width (cl-libsixel:sixel-image-width image))
             (height (cl-libsixel:sixel-image-height image))
             (aspect (sixel-pixel-aspect payload))
             (new-width (max 1 (round (* width (max 1 (/ aspect))))))
             (new-height (max 1 (round (* height (max 1 aspect))))))
        (when (> (* 4 new-width new-height) +max-sixel-octets+)
          (error "SIXEL image too large at ~dx~d" new-width new-height))
        (if (and (= new-width width) (= new-height height))
            (list rgba width height)
            (list (scale-rgba rgba width height new-width new-height) new-width new-height)))
    (error (condition) condition)))

(defun handle-sixel-command (store payload engine
                             &key (cell-width (terminal-image-store-cell-width store))
                               (cell-height (terminal-image-store-cell-height store))
                               (decoded (decode-sixel-payload payload)))
  "Decode and place the SIXEL image PAYLOAD (a whole DCS string).  With sixel
scrolling (DECSDM reset) the image starts at the cursor, which then ends on
the line below the image, at its left column (or on its last text row, after
it, with ?8452), scrolling as needed, like mlterm, konsole and libvte; with
DECSDM set it is drawn at the top left and the cursor does not move.  An
opaque image replaces older SIXEL images in the cells it covers entirely.  DECODED is the image from DECODE-SIXEL-PAYLOAD."
  (when (typep decoded 'condition)
    (warn "Could not decode SIXEL image: ~a" decoded)
    (return-from handle-sixel-command nil))
  (handler-case
      (let* ((transparent-p (eql 1 (second (sixel-header-parameters payload))))
             (rgba (first decoded))
             (width (second decoded))
             (height (third decoded))
             (display-mode (terminal-image-store-sixel-display-mode store))
             (screen (if (engine-mode engine :alternate-screen) :alternate :primary))
             (cell-width (max 1 cell-width))
             (cell-height (max 1 cell-height)))
        (setf (terminal-image-store-cell-width store) cell-width
              (terminal-image-store-cell-height store) cell-height)
        (multiple-value-bind (row column)
            (if display-mode (values 0 0) (engine-cursor-position engine))
          (let* ((image-rows (max 1 (ceiling height cell-height)))
                 (image-columns (max 1 (ceiling width cell-width)))
                 (next-line (if (terminal-image-store-sixel-scrolls-right store) 0 1))
                 (row-before 0)
                 (id (allocate-image-id store :internal t))
                 (stored (store-image store id 0 width height rgba :anonymous-p t))
                 (placement (make-terminal-image-placement
                             :image-id id :row row :column column :screen screen
                             :erase-on-write-p t)))
            (unless transparent-p
              (let ((full-rows (floor height cell-height))
                    (full-columns (floor width cell-width)))
                (when (and (plusp full-rows) (plusp full-columns))
                  (hide-sixel-cells store screen row column
                                    (+ row full-rows -1) (+ column full-columns -1)))))
            (push placement (terminal-image-store-placements store))
            (setf row-before (terminal-image-placement-row placement))
            (unless display-mode
              (incf (terminal-image-store-pending-scroll store)
                    (max 0 (- (+ row image-rows next-line) (engine-rows engine))))
              (engine-feed-string
               engine
               (with-output-to-string (out)
                 (dotimes (i (+ image-rows next-line -1))
                   (format out "~cD" (code-char 27)))
                 (when (terminal-image-store-sixel-scrolls-right store)
                   (if (>= (+ column image-columns) (engine-columns engine))
                       (format out "~c~cD" #\Return (code-char 27))
                       (format out "~c[~dC" (code-char 27) image-columns))))))
            ;; What the image covers once it is on screen (scrolled into
            ;; view if need be); later changes there erase it.
            (let ((grid (terminal-image-store-scratch-grid store))
                  ;; Scrolling the engine did not report has not moved
                  ;; the placement yet.
                  (scrolled (if display-mode
                                0
                                (max 0 (- (max 0 (- (+ row image-rows next-line)
                                                    (engine-rows engine)))
                                          (- row-before
                                             (terminal-image-placement-row placement)))))))
              (unless (and grid (= (grid-rows grid) (engine-rows engine))
                           (= (grid-columns grid) (engine-columns engine)))
                (setf grid (make-grid (engine-rows engine) (engine-columns engine))
                      (terminal-image-store-scratch-grid store) grid))
              (engine-update-grid engine grid :force t)
              (setf (terminal-image-store-force-render-p store) t)
              (capture-sixel-baseline store placement stored grid scrolled))
            (setf (terminal-image-store-dirty-p store) t))))
    (error (condition)
      (warn "Could not decode SIXEL image: ~a" condition))))

;;; Other terminal actions

(defun handle-graphics-control (store event engine)
  "React to EVENT, a control sequence the graphics parser observed (see
TERMINAL-GRAPHICS-FEED).  Return a reply for the child, or NIL."
  (let ((esc (code-char 27)))
    (destructuring-bind (kind &rest arguments) event
      (case kind
        (:mode
         (destructuring-bind (mode value) arguments
           (case mode
             (80 (setf (terminal-image-store-sixel-display-mode store) value))
             (8452 (setf (terminal-image-store-sixel-scrolls-right store) value)))
           nil))
        (:report-mode
         (let ((mode (first arguments)))
           (format nil "~c[?~d;~d$y" esc mode
                   (if (case mode
                         (80 (terminal-image-store-sixel-display-mode store))
                         (8452 (terminal-image-store-sixel-scrolls-right store)))
                       1 2))))
        (:enter-alternate-screen
         (clear-screen-images store :all :alternate)
         nil)
        (:erase-display
         (let ((how (first arguments))
               (screen (if (engine-mode engine :alternate-screen) :alternate :primary))
               (last-row (1- (engine-rows engine)))
               (last-column (1- (engine-columns engine))))
           (if (member how '(2 3))
               (clear-screen-images store how screen)
               (multiple-value-bind (row column) (engine-cursor-position engine)
                 (if (eql how 1)
                     (progn (hide-sixel-cells store screen 0 0 (1- row) last-column)
                            (hide-sixel-cells store screen row 0 row column))
                     (progn (hide-sixel-cells store screen row column row last-column)
                            (hide-sixel-cells store screen (1+ row) 0
                                              last-row last-column))))))
         nil)
        (:erase-line
         (let ((screen (if (engine-mode engine :alternate-screen) :alternate :primary))
               (last-column (1- (engine-columns engine))))
           (multiple-value-bind (row column) (engine-cursor-position engine)
             (case (first arguments)
               (1 (hide-sixel-cells store screen row 0 row column))
               (2 (hide-sixel-cells store screen row 0 row last-column))
               (t (hide-sixel-cells store screen row column row last-column)))))
         nil)
        (:erase-characters
         (let ((screen (if (engine-mode engine :alternate-screen) :alternate :primary)))
           (multiple-value-bind (row column) (engine-cursor-position engine)
             (hide-sixel-cells store screen row column row
                               (min (1- (engine-columns engine))
                                    (+ column (max 1 (first arguments)) -1)))))
         nil)
        (:reset
         (kitty-graphics:graphics-decoder-abort (terminal-image-store-kitty-decoder store))
         (setf (terminal-image-store-sixel-display-mode store) nil
               (terminal-image-store-sixel-scrolls-right store) nil)
         (delete-placements-if store (lambda (placement)
                                       (not (terminal-image-placement-virtual-p placement))))
         nil)
        (:primary-device-attributes
         ;; VT220-level, with sixel graphics (4) and ANSI color (22).
         (format nil "~c[?62;4;22c" esc))
        (:graphics-attributes
         ;; XTSMGRAPHICS: item 1 color registers, 2 sixel geometry; action
         ;; 1 reads, 4 reads the maximum.
         (destructuring-bind (item action) arguments
           (cond
             ((not (member action '(1 4))) (format nil "~c[?~d;3;0S" esc item))
             ((= item 1) (format nil "~c[?1;0;256S" esc))
             ((= item 2)
              (format nil "~c[?2;0;~d;~dS" esc
                      (min 4096 (* (terminal-image-store-columns store)
                                   (terminal-image-store-cell-width store)))
                      (min 4096 (* (terminal-image-store-rows store)
                                   (terminal-image-store-cell-height store)))))
             (t (format nil "~c[?~d;1;0S" esc item)))))))))
