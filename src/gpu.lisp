(in-package #:slt)

;;;; Renderer-agnostic support for GPU frontends (Metal, OpenGL, Vulkan).
;;;;
;;;; Glyphs are rasterized once into an alpha-only atlas of cell-sized
;;;; slots.  A wide glyph takes two adjacent slots so that its spacer cell
;;;; simply draws the right half; every cell is then one fixed-size quad.
;;;; Color glyphs (emoji) go into the same slots of a second, RGBA atlas,
;;;; and their slot numbers carry +GLYPH-COLOR+.
;;;;
;;;; The grid is packed into an instance array of four 32-bit words per
;;;; cell -- foreground RGB, background RGB, atlas slot, and decoration
;;;; flags in the low 8 bits under the underline's RGB --
;;;; which a frontend uploads and draws with a single instanced call.  Only
;;;; dirty rows are repacked.

(deftype u32-vector () '(simple-array (unsigned-byte 32) (*)))
(deftype u8-vector () '(simple-array (unsigned-byte 8) (*)))

(defconstant +gpu-flag-underline+ 1)
(defconstant +gpu-flag-double-underline+ 2)
(defconstant +gpu-flag-strike+ 4)
(defconstant +gpu-flag-cursor-bar+ 8)
(defconstant +gpu-flag-cursor-underline+ 16)
(defconstant +gpu-flag-curly-underline+ 32)
(defconstant +gpu-flag-dotted-underline+ 64)
(defconstant +gpu-flag-dashed-underline+ 128)
(defconstant +gpu-words-per-cell+ 4)
;; Set on the atlas slot of cells with the default background, which the
;; background layer leaves transparent for images under the text.
(defconstant +gpu-slot-default-background+ (ash 1 31))
;; Set on the slots of color glyphs, which come from the color atlas.
(defconstant +glyph-color+ (ash 1 30))

(defstruct (glyph-atlas (:constructor %make-glyph-atlas))
  (slot-width 1 :type (integer 1 4096))
  (slot-height 1 :type (integer 1 4096))
  (width 1 :type (integer 1 16384))
  (height 1 :type (integer 1 16384))
  (slots-per-row 1 :type (integer 1 16384))
  (slot-rows 1 :type (integer 1 16384))
  (pixels (make-array 1 :element-type '(unsigned-byte 8)) :type u8-vector)
  ;; Premultiplied RGBA pixels of the color glyphs, four bytes per pixel of
  ;; PIXELS, made when the first one is rasterized; and their dirty rows.
  (color-pixels nil :type (or null u8-vector))
  (color-dirty-start nil :type (or null fixnum))
  (color-dirty-end 0 :type fixnum)
  ;; Slot 0 is the blank glyph.
  (next-slot 1 :type fixnum)
  (table (make-hash-table) :type hash-table)
  (cluster-table (make-hash-table :test #'equal) :type hash-table)
  ;; Rows of pixels that changed since the frontend last uploaded them.
  (dirty-start nil :type (or null fixnum))
  (dirty-end 0 :type fixnum)
  ;; (FUNCTION STRING STYLE): true when the font draws STRING, a run of
  ;; cells, otherwise than its characters one by one (ligatures); NIL when
  ;; the rasterizer cannot shape.
  (shaper nil :type (or null function))
  ;; (STRING . STYLE-KEY) -> the first of the slots holding STRING's run,
  ;; or :NONE when it has no ligature.
  (ligatures (make-hash-table :test #'equal) :type hash-table)
  ;; (FUNCTION STRING STYLE PIXELS STRIDE X Y WIDTH HEIGHT): draw STRING's
  ;; coverage into PIXELS within the box; STYLE is a list of :bold :italic.
  ;; For a color glyph it may instead return the box's premultiplied RGBA
  ;; pixels, WIDTH x HEIGHT x 4 octets, top row first.
  (rasterizer nil :type (or null function))
  ;; Incremented whenever the atlas is cleared, invalidating packed slots.
  (generation 0 :type fixnum))

(defparameter +glyph-atlas-full-height+ 2048
  "Atlases start small and grow, keeping their glyphs, up to this height;
past it a full atlas is cleared first, and grows only when one screen of
glyphs does not fit.")

(defun make-glyph-atlas (slot-width slot-height rasterizer
                         &key (width 2048) (height 256) shaper)
  (let ((slots-per-row (max 2 (floor width slot-width)))
        (slot-rows (max 1 (floor height slot-height))))
    (%make-glyph-atlas :slot-width slot-width
                       :slot-height slot-height
                       :width width
                       :height height
                       :slots-per-row slots-per-row
                       :slot-rows slot-rows
                       :pixels (make-array (* width height) :element-type '(unsigned-byte 8)
                                                            :initial-element 0)
                       :rasterizer rasterizer
                       :shaper shaper)))

(defun glyph-atlas-clear (atlas)
  (fill (glyph-atlas-pixels atlas) 0)
  (when (glyph-atlas-color-pixels atlas)
    (fill (glyph-atlas-color-pixels atlas) 0)
    (setf (glyph-atlas-color-dirty-start atlas) 0
          (glyph-atlas-color-dirty-end atlas) (glyph-atlas-height atlas)))
  (clrhash (glyph-atlas-table atlas))
  (clrhash (glyph-atlas-cluster-table atlas))
  (clrhash (glyph-atlas-ligatures atlas))
  (setf (glyph-atlas-next-slot atlas) 1
        (glyph-atlas-dirty-start atlas) 0
        (glyph-atlas-dirty-end atlas) (glyph-atlas-height atlas))
  (incf (glyph-atlas-generation atlas))
  atlas)

(defun glyph-atlas-grow (atlas &key keep)
  "Double the atlas height (the texture must then be recreated) and clear
it, or with KEEP keep its glyphs in their slots."
  (let ((height (min 16384 (* 2 (glyph-atlas-height atlas)))))
    (when (= height (glyph-atlas-height atlas))
      (error "The glyph atlas cannot hold one screen of glyphs."))
    (flet ((grown (pixels octets-per-pixel)
             (and pixels
                  (let ((new (make-array (* octets-per-pixel (glyph-atlas-width atlas) height)
                                         :element-type '(unsigned-byte 8)
                                         :initial-element 0)))
                    (when keep (replace new pixels))
                    new))))
      (setf (glyph-atlas-height atlas) height
            (glyph-atlas-slot-rows atlas) (max 1 (floor height (glyph-atlas-slot-height atlas)))
            (glyph-atlas-pixels atlas) (grown (glyph-atlas-pixels atlas) 1)
            (glyph-atlas-color-pixels atlas) (grown (glyph-atlas-color-pixels atlas) 4)))
    (if keep
        ;; Same slots, larger textures: renderers recreate and fill them.
        (progn
          (setf (glyph-atlas-dirty-start atlas) 0
                (glyph-atlas-dirty-end atlas) height)
          (when (glyph-atlas-color-pixels atlas)
            (setf (glyph-atlas-color-dirty-start atlas) 0
                  (glyph-atlas-color-dirty-end atlas) height))
          (incf (glyph-atlas-generation atlas))
          atlas)
        (glyph-atlas-clear atlas))))

(defun glyph-atlas-take-color-dirty (atlas)
  "Return the dirty row range [START, END) of the color atlas and reset
it, or NIL."
  (let ((start (glyph-atlas-color-dirty-start atlas)))
    (when start
      (setf (glyph-atlas-color-dirty-start atlas) nil)
      (values start (glyph-atlas-color-dirty-end atlas)))))

(defun glyph-atlas-take-dirty (atlas)
  "Return the dirty pixel row range [START, END) and reset it, or NIL."
  (let ((start (glyph-atlas-dirty-start atlas)))
    (when start
      (setf (glyph-atlas-dirty-start atlas) nil)
      (values start (glyph-atlas-dirty-end atlas)))))

(defun allocate-glyph-slots (atlas count)
  "Reserve COUNT adjacent slots on one atlas row, or return NIL when full."
  (let* ((per-row (glyph-atlas-slots-per-row atlas))
         (slot (glyph-atlas-next-slot atlas)))
    (when (> (+ (mod slot per-row) count) per-row)
      (setf slot (* per-row (ceiling slot per-row))))
    (when (<= (+ slot count) (* per-row (glyph-atlas-slot-rows atlas)))
      (setf (glyph-atlas-next-slot atlas) (+ slot count))
      slot)))

(defun rasterize-into-slot (atlas slot string style slot-count)
  "Draw STRING into SLOT (and the slots after it, SLOT-COUNT in all); return
true when it is a color glyph."
  (let* ((per-row (glyph-atlas-slots-per-row atlas))
         (x (* (mod slot per-row) (glyph-atlas-slot-width atlas)))
         (y (* (floor slot per-row) (glyph-atlas-slot-height atlas)))
         (width (* slot-count (glyph-atlas-slot-width atlas)))
         (height (glyph-atlas-slot-height atlas))
         (rgba (funcall (glyph-atlas-rasterizer atlas)
                        string style (glyph-atlas-pixels atlas) (glyph-atlas-width atlas)
                        x y width height)))
    (setf (glyph-atlas-dirty-start atlas) (min y (or (glyph-atlas-dirty-start atlas) y))
          (glyph-atlas-dirty-end atlas) (max (+ y height) (glyph-atlas-dirty-end atlas)))
    (when (typep rgba 'u8-vector)
      (let ((stride (* 4 (glyph-atlas-width atlas)))
            (color (or (glyph-atlas-color-pixels atlas)
                       (setf (glyph-atlas-color-pixels atlas)
                             (make-array (* 4 (glyph-atlas-width atlas) (glyph-atlas-height atlas))
                                         :element-type '(unsigned-byte 8)
                                         :initial-element 0)))))
        (dotimes (row height)
          (replace color rgba :start1 (+ (* 4 x) (* stride (+ y row)))
                              :start2 (* 4 width row) :end2 (* 4 width (1+ row))))
        (setf (glyph-atlas-color-dirty-start atlas)
              (min y (or (glyph-atlas-color-dirty-start atlas) y))
              (glyph-atlas-color-dirty-end atlas)
              (max (+ y height) (glyph-atlas-color-dirty-end atlas))))
      t)))

(declaim (inline glyph-key))
(defun glyph-key (code bold italic wide)
  (logior (ash code 3) (if bold 1 0) (if italic 2 0) (if wide 4 0)))

(defun glyph-atlas-slot (atlas code combining attrs)
  "The slot holding CODE (plus COMBINING marks) in ATTRS's style, rasterizing
it on first use.  Return NIL when the atlas is full."
  (let* ((bold (logtest attrs +attr-bold+))
         (italic (logtest attrs +attr-italic+))
         (wide (logtest attrs +attr-wide+))
         (key (if combining
                  (list code combining bold italic wide)
                  (glyph-key code bold italic wide)))
         (table (if combining
                    (glyph-atlas-cluster-table atlas)
                    (glyph-atlas-table atlas))))
    (or (gethash key table)
        (let* ((count (if wide 2 1))
               (slot (allocate-glyph-slots atlas count)))
          (when slot
            (setf (gethash key table)
                  (if (rasterize-into-slot atlas slot
                                           (if combining
                                               (concatenate 'string (string (code-char code))
                                                            combining)
                                               (string (code-char code)))
                                           (append (and bold '(:bold)) (and italic '(:italic)))
                                           count)
                      (logior slot +glyph-color+)
                      slot)))))))

;;; Ligatures.  A run of cells holding ASCII punctuation in one style, as
;;; ->, != or ===, is offered to the atlas's shaper; when the font draws it
;;; otherwise than character by character, the run is drawn once over as
;;; many adjacent slots as it has cells, the way a wide character takes two.

(defparameter +max-ligature-cells+ 16
  "The longest run of cells shaped together; longer ones are split.")

(defconstant +ascii-code-limit+ 128)

(defun ligature-character-p (code)
  "True for the characters runs of which are shaped: ASCII punctuation."
  (and (< code +ascii-code-limit+)
       (let ((char (code-char code)))
         (and (graphic-char-p char) (not (alphanumericp char)) (char/= char #\Space)))))

(defun glyph-atlas-ligature (atlas text attrs)
  "The first of the slots holding the run TEXT in ATTRS's style when the
font draws it with ligatures; NIL when it has none; :FULL when it has one
but the atlas is full."
  (let* ((bold (logtest attrs +attr-bold+))
         (italic (logtest attrs +attr-italic+))
         (style (append (and bold '(:bold)) (and italic '(:italic))))
         (key (cons text (glyph-key 0 bold italic nil)))
         (known (gethash key (glyph-atlas-ligatures atlas))))
    (cond ((eq known :none) nil)
          (known)
          ((not (funcall (glyph-atlas-shaper atlas) text style))
           (setf (gethash key (glyph-atlas-ligatures atlas)) :none)
           nil)
          (t (let ((slot (allocate-glyph-slots atlas (length text))))
               (if slot
                   (progn (rasterize-into-slot atlas slot text style (length text))
                          (setf (gethash key (glyph-atlas-ligatures atlas)) slot))
                   :full))))))

(defun row-ligature-slots (grid row atlas)
  "For ROW of GRID, a vector of the slots ligatures give its cells (NIL for
other cells), NIL when it has none, or :FULL when the atlas filled up."
  (when (and *ligatures* (glyph-atlas-shaper atlas))
    (let ((columns (grid-columns grid))
          (slots nil))
      (flet ((candidate-p (column)
               (let ((index (grid-index grid row column)))
                 (and (ligature-character-p (grid-glyph-code grid index))
                      (not (gethash index (grid-combining grid)))
                      (not (logtest (aref (grid-attrs grid) index)
                                    (logior +attr-wide+ +attr-wide-spacer+ +attr-invisible+))))))
             (style (column)
               (logand (aref (grid-attrs grid) (grid-index grid row column))
                       (logior +attr-bold+ +attr-italic+))))
        (loop with column = 0
              while (< column columns)
              do (if (not (candidate-p column))
                     (incf column)
                     (let ((end (loop for next from (1+ column)
                                      while (and (< next columns)
                                                 (< (- next column) +max-ligature-cells+)
                                                 (candidate-p next)
                                                 (= (style next) (style column)))
                                      finally (return next))))
                       (when (> (- end column) 1)
                         (let* ((text (map 'string
                                           (lambda (c)
                                             (code-char (grid-glyph-code
                                                         grid (grid-index grid row c))))
                                           (loop for c from column below end collect c)))
                                (slot (glyph-atlas-ligature
                                       atlas text
                                       (aref (grid-attrs grid) (grid-index grid row column)))))
                           (cond ((eq slot :full) (return-from row-ligature-slots :full))
                                 (slot
                                  (unless slots
                                    (setf slots (make-array columns :initial-element nil)))
                                  (loop for c from column below end
                                        for s from slot
                                        do (setf (aref slots c) s))))))
                       (setf column end))))
        slots))))

(defun make-cell-instances (grid)
  (make-array (* +gpu-words-per-cell+ (grid-rows grid) (grid-columns grid))
              :element-type '(unsigned-byte 32) :initial-element 0))

(defun pack-grid-row (grid row atlas palette instances)
  "Pack ROW of GRID into INSTANCES.  Return NIL if the atlas filled up."
  (declare (type grid grid)
           (type u32-vector instances)
           (type palette palette))
  (let ((columns (grid-columns grid))
        (cursor-column (and (grid-cursor-visible-p grid)
                            (= row (grid-cursor-row grid))
                            (grid-cursor-column grid)))
        (wide-slot nil)
        (wide-fg 0)
        (wide-bg 0)
        (ligature-slots (row-ligature-slots grid row atlas)))
    (when (eq ligature-slots :full)
      (return-from pack-grid-row nil))
    (dotimes (column columns t)
      (let* ((index (grid-index grid row column))
             (code (grid-glyph-code grid index))
             (attrs (aref (grid-attrs grid) index))
             (offset (* +gpu-words-per-cell+ index))
             (flags (logior (case (underline-style attrs)
                              ((nil) 0)
                              (:single +gpu-flag-underline+)
                              (:double +gpu-flag-double-underline+)
                              (:curly +gpu-flag-curly-underline+)
                              (:dotted +gpu-flag-dotted-underline+)
                              (:dashed +gpu-flag-dashed-underline+))
                            (if (logtest attrs +attr-strike+) +gpu-flag-strike+ 0)
                            (if (eql column cursor-column)
                                (case (grid-cursor-shape grid)
                                  (:bar +gpu-flag-cursor-bar+)
                                  (:underline +gpu-flag-cursor-underline+)
                                  (t 0))
                                0))))
        (multiple-value-bind (fg bg) (cell-rgb grid index palette)
          (when (grid-selected-p grid index)
            (setf bg (palette-selection palette)))
          (let ((slot
                  (cond
                    ((and ligature-slots (aref ligature-slots column)))
                    ((logtest attrs +attr-wide-spacer+)
                     (prog1 (if wide-slot (1+ wide-slot) 0)
                       (when wide-slot
                         (setf fg wide-fg bg wide-bg))))
                    ((or (zerop code) (= code 32) (logtest attrs +attr-invisible+)) 0)
                    (t (or (glyph-atlas-slot atlas code
                                             (gethash index (grid-combining grid))
                                             attrs)
                           (return nil))))))
            (if (and (logtest attrs +attr-wide+) (plusp slot))
                (setf wide-slot slot wide-fg fg wide-bg bg)
                (setf wide-slot nil))
            (setf (aref instances offset) fg
                  (aref instances (+ offset 1)) bg
                  (aref instances (+ offset 2))
                  (if (= bg (palette-background palette))
                      (logior slot +gpu-slot-default-background+)
                      slot)
                  (aref instances (+ offset 3))
                  (logior flags (ash (cell-underline-rgb grid index palette fg) 8)))))))))

(defun pack-grid (grid atlas palette instances)
  "Repack the dirty rows of GRID into INSTANCES (reallocating it when the
grid changed size) and return INSTANCES.  When the atlas overflows it is
cleared and every row repacked."
  (let ((size (* +gpu-words-per-cell+ (grid-rows grid) (grid-columns grid))))
    (unless (and instances (= (length instances) size))
      (setf instances (make-cell-instances grid))
      (grid-mark-all-dirty grid))
    (flet ((pack-dirty ()
             (dotimes (row (grid-rows grid) t)
               (when (= 1 (sbit (grid-dirty grid) row))
                 (unless (pack-grid-row grid row atlas palette instances)
                   (return nil))))))
      ;; A small atlas grows keeping its glyphs.  A full-size one is
      ;; cleared, evicting glyphs from earlier screens; if one screen alone
      ;; does not fit, it grows until it does.
      (loop for attempt from 0
            until (pack-dirty)
            do (cond ((< (glyph-atlas-height atlas) +glyph-atlas-full-height+)
                      (glyph-atlas-grow atlas :keep t))
                     ((zerop attempt)
                      (glyph-atlas-clear atlas)
                      (grid-mark-all-dirty grid))
                     (t
                      (glyph-atlas-grow atlas)
                      (grid-mark-all-dirty grid)))))
    instances))

(defun packed-row-runs (instances columns row)
  "Split ROW of INSTANCES, a grid packed by PACK-GRID, into the runs that
draw it, for frontends that draw with rectangles and glyph runs (clx).
Return three lists, left to right: backgrounds (START END RGB DEFAULT-P),
glyphs in one color (START RGB SLOTS) and decorations (START END WORD3 RGB),
WORD3 being the cells' flags and underline color and RGB their foreground.
Color glyphs (slots with +GLYPH-COLOR+) are in runs of their own."
  (let ((base (* row columns))
        (backgrounds '()) (glyphs '()) (decorations '()))
    (flet ((word (column i)
             (aref instances (+ (* +gpu-words-per-cell+ (+ base column)) i))))
      (flet ((slot (column) (ldb (byte 31 0) (word column 2))))
        (loop with start = 0
              for column from 1 to columns
              when (or (= column columns) (/= (word column 1) (word start 1)))
                do (push (list start column (word start 1)
                               (logtest (word start 2) +gpu-slot-default-background+))
                         backgrounds)
                   (setf start column))
        (let ((column 0))
          (loop while (< column columns)
                do (if (zerop (slot column))
                       (incf column)
                       (let ((end (1+ column)))
                         (loop while (and (< end columns) (plusp (slot end))
                                          (= (word end 0) (word column 0))
                                          (eq (logtest (slot end) +glyph-color+)
                                              (logtest (slot column) +glyph-color+)))
                               do (incf end))
                         (push (list column (word column 0)
                                     (coerce (loop for c from column below end collect (slot c))
                                             '(simple-array (unsigned-byte 32) (*))))
                               glyphs)
                         (setf column end)))))
        (let ((column 0))
          (loop while (< column columns)
                do (if (zerop (ldb (byte 8 0) (word column 3)))
                       (incf column)
                       (let ((end (1+ column)))
                         (loop while (and (< end columns)
                                          (= (word end 3) (word column 3))
                                          (= (word end 0) (word column 0)))
                               do (incf end))
                         (push (list column end (word column 3) (word column 0)) decorations)
                         (setf column end)))))))
    (values (nreverse backgrounds) (nreverse glyphs) (nreverse decorations))))

(defun write-ppm (pathname rgb width height)
  (with-open-file (out pathname :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
    (write-sequence (babel:string-to-octets (format nil "P6~%~d ~d~%255~%" width height)) out)
    (write-sequence rgb out))
  pathname)
