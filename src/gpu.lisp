(in-package #:slt)

;;;; Renderer-agnostic support for GPU backends (Metal, OpenGL, Vulkan).
;;;;
;;;; Glyphs are rasterized once into an alpha-only atlas of cell-sized
;;;; slots.  A wide glyph takes two adjacent slots so that its spacer cell
;;;; simply draws the right half; every cell is then one fixed-size quad.
;;;;
;;;; The grid is packed into an instance array of four 32-bit words per
;;;; cell -- foreground RGB, background RGB, atlas slot, decoration flags --
;;;; which a backend uploads and draws with a single instanced call.  Only
;;;; dirty rows are repacked.

(deftype u32-vector () '(simple-array (unsigned-byte 32) (*)))
(deftype u8-vector () '(simple-array (unsigned-byte 8) (*)))

(defconstant +gpu-flag-underline+ 1)
(defconstant +gpu-flag-double-underline+ 2)
(defconstant +gpu-flag-strike+ 4)
(defconstant +gpu-flag-cursor-bar+ 8)
(defconstant +gpu-flag-cursor-underline+ 16)
(defconstant +gpu-words-per-cell+ 4)

(defstruct (glyph-atlas (:constructor %make-glyph-atlas))
  (slot-width 1 :type (integer 1 4096))
  (slot-height 1 :type (integer 1 4096))
  (width 1 :type (integer 1 16384))
  (height 1 :type (integer 1 16384))
  (slots-per-row 1 :type (integer 1 16384))
  (slot-rows 1 :type (integer 1 16384))
  (pixels (make-array 1 :element-type '(unsigned-byte 8)) :type u8-vector)
  ;; Slot 0 is the blank glyph.
  (next-slot 1 :type fixnum)
  (table (make-hash-table) :type hash-table)
  (cluster-table (make-hash-table :test #'equal) :type hash-table)
  ;; Rows of pixels that changed since the backend last uploaded them.
  (dirty-start nil :type (or null fixnum))
  (dirty-end 0 :type fixnum)
  ;; (FUNCTION STRING STYLE PIXELS STRIDE X Y WIDTH HEIGHT): draw STRING's
  ;; coverage into PIXELS within the box; STYLE is a list of :bold :italic.
  (rasterizer nil :type (or null function))
  ;; Incremented whenever the atlas is cleared, invalidating packed slots.
  (generation 0 :type fixnum))

(defun make-glyph-atlas (slot-width slot-height rasterizer &key (width 2048) (height 2048))
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
                       :rasterizer rasterizer)))

(defun glyph-atlas-clear (atlas)
  (fill (glyph-atlas-pixels atlas) 0)
  (clrhash (glyph-atlas-table atlas))
  (clrhash (glyph-atlas-cluster-table atlas))
  (setf (glyph-atlas-next-slot atlas) 1
        (glyph-atlas-dirty-start atlas) 0
        (glyph-atlas-dirty-end atlas) (glyph-atlas-height atlas))
  (incf (glyph-atlas-generation atlas))
  atlas)

(defun glyph-atlas-grow (atlas)
  "Double the atlas height (the texture must then be recreated) and clear it."
  (let ((height (min 16384 (* 2 (glyph-atlas-height atlas)))))
    (when (= height (glyph-atlas-height atlas))
      (error "The glyph atlas cannot hold one screen of glyphs."))
    (setf (glyph-atlas-height atlas) height
          (glyph-atlas-slot-rows atlas) (max 1 (floor height (glyph-atlas-slot-height atlas)))
          (glyph-atlas-pixels atlas) (make-array (* (glyph-atlas-width atlas) height)
                                                 :element-type '(unsigned-byte 8)))
    (glyph-atlas-clear atlas)))

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
  (let* ((per-row (glyph-atlas-slots-per-row atlas))
         (x (* (mod slot per-row) (glyph-atlas-slot-width atlas)))
         (y (* (floor slot per-row) (glyph-atlas-slot-height atlas)))
         (height (glyph-atlas-slot-height atlas)))
    (funcall (glyph-atlas-rasterizer atlas)
             string style (glyph-atlas-pixels atlas) (glyph-atlas-width atlas)
             x y (* slot-count (glyph-atlas-slot-width atlas)) height)
    (setf (glyph-atlas-dirty-start atlas) (min y (or (glyph-atlas-dirty-start atlas) y))
          (glyph-atlas-dirty-end atlas) (max (+ y height) (glyph-atlas-dirty-end atlas)))))

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
            (rasterize-into-slot atlas slot
                                 (if combining
                                     (concatenate 'string (string (code-char code)) combining)
                                     (string (code-char code)))
                                 (append (and bold '(:bold)) (and italic '(:italic)))
                                 count)
            (setf (gethash key table) slot))))))

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
        (wide-bg 0))
    (dotimes (column columns t)
      (let* ((index (grid-index grid row column))
             (code (aref (grid-codes grid) index))
             (attrs (aref (grid-attrs grid) index))
             (offset (* +gpu-words-per-cell+ index))
             (flags (logior (if (logtest attrs +attr-underline+) +gpu-flag-underline+ 0)
                            (if (logtest attrs +attr-double-underline+)
                                +gpu-flag-double-underline+ 0)
                            (if (logtest attrs +attr-strike+) +gpu-flag-strike+ 0)
                            (if (eql column cursor-column)
                                (case (grid-cursor-shape grid)
                                  (:bar +gpu-flag-cursor-bar+)
                                  (:underline +gpu-flag-cursor-underline+)
                                  (t 0))
                                0))))
        (multiple-value-bind (fg bg) (cell-rgb grid index palette)
          (let ((slot
                  (cond
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
                  (aref instances (+ offset 2)) slot
                  (aref instances (+ offset 3)) flags)))))))

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
      ;; Clearing evicts glyphs from earlier screens; if one screen alone
      ;; does not fit, grow the atlas until it does.
      (loop for attempt from 0
            until (pack-dirty)
            do (if (zerop attempt)
                   (glyph-atlas-clear atlas)
                   (glyph-atlas-grow atlas))
               (grid-mark-all-dirty grid)))
    instances))

(defun write-ppm (pathname rgb width height)
  (with-open-file (out pathname :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
    (write-sequence (babel:string-to-octets (format nil "P6~%~d ~d~%255~%" width height)) out)
    (write-sequence rgb out))
  pathname)
