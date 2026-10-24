(in-package #:slt)

;;;; Engine backed by libvterm (the library used by Neovim and Lem) through
;;;; the cl-libvterm binding in lib/cl-libvterm.  Its packed row encoding
;;;; uses the same color tags as the grid, so rows copy without decoding.

(defclass libvterm-engine (engine)
  ((vterm :reader libvterm-engine-vterm)
   (codes :accessor libvterm-engine-codes)
   (fg :accessor libvterm-engine-fg)
   (bg :accessor libvterm-engine-bg)
   (attrs :accessor libvterm-engine-attrs)))

(defun allocate-libvterm-rows (engine columns)
  (flet ((row () (make-array columns :element-type '(unsigned-byte 32))))
    (setf (libvterm-engine-codes engine) (row)
          (libvterm-engine-fg engine) (row)
          (libvterm-engine-bg engine) (row)
          (libvterm-engine-attrs engine) (row))))

(defmethod initialize-instance :after ((engine libvterm-engine) &key (rows 24) (columns 80))
  (setf (slot-value engine 'vterm)
        (libvterm:make-vterm
         :rows rows
         :columns columns
         :on-output (lambda (vt octets)
                      (declare (ignore vt))
                      (engine-emit-output engine
                                          (babel:octets-to-string octets :encoding :utf-8
                                                                         :errorp nil)))
         :on-bell (lambda (vt)
                    (declare (ignore vt))
                    (engine-emit-bell engine))
         :on-title (lambda (vt title)
                     (declare (ignore vt))
                     (engine-emit-title engine title))
         ;; Full-width row moves are scrolls (and line insertion/deletion).
         :on-moverect (lambda (vt dsr der dsc dec ssr ser ssc sec)
                        (when (and (zerop dsc) (zerop ssc) (= dec sec)
                                   (= dec (libvterm:vterm-columns vt))
                                   (/= dsr ssr))
                          (engine-emit-scroll engine (min dsr ssr) (1- (max der ser))
                                              (- ssr dsr))))))
  (allocate-libvterm-rows engine columns))

(defmethod engine-reports-scrolls-p ((engine libvterm-engine))
  t)

(defmethod engine-rows ((engine libvterm-engine))
  (libvterm:vterm-rows (libvterm-engine-vterm engine)))

(defmethod engine-columns ((engine libvterm-engine))
  (libvterm:vterm-columns (libvterm-engine-vterm engine)))

(defmethod engine-cursor-position ((engine libvterm-engine))
  (libvterm:vterm-cursor (libvterm-engine-vterm engine)))

(defmethod engine-feed ((engine libvterm-engine) octets &key (start 0) (end (length octets)))
  (libvterm:vterm-feed (libvterm-engine-vterm engine) octets :start start :end end)
  engine)

(defmethod engine-resize ((engine libvterm-engine) rows columns)
  ;; Shrinking pushes the rows above the cursor into the scrollback.
  (let* ((vt (libvterm-engine-vterm engine))
         (old-rows (libvterm:vterm-rows vt))
         (pushed 0))
    (setf (libvterm:vterm-on-scrollback vt) (lambda (&rest line)
                                              (declare (ignore line))
                                              (incf pushed)))
    (unwind-protect (libvterm:vterm-resize vt rows columns)
      (setf (libvterm:vterm-on-scrollback vt) nil))
    (when (plusp pushed)
      (engine-emit-scroll engine 0 (1- old-rows) pushed :primary)))
  (allocate-libvterm-rows engine columns)
  engine)

(defmethod engine-close ((engine libvterm-engine))
  (libvterm:vterm-free (libvterm-engine-vterm engine)))

(declaim (inline libvterm-attrs->attributes))
(defun libvterm-attrs->attributes (word)
  (declare (type (unsigned-byte 32) word))
  (let ((underline (ldb (byte 2 2) word))
        (width (ldb (byte 2 9) word)))
    (logior (if (logbitp 0 word) +attr-bold+ 0)
            (if (logbitp 1 word) +attr-italic+ 0)
            (case underline
              (0 0)
              (2 +attr-double-underline+)
              (t +attr-underline+))
            (if (logbitp 4 word) +attr-blink+ 0)
            (if (logbitp 5 word) +attr-inverse+ 0)
            (if (logbitp 6 word) +attr-invisible+ 0)
            (if (logbitp 7 word) +attr-strike+ 0)
            (case width
              (2 +attr-wide+)
              (0 +attr-wide-spacer+)
              (t 0)))))

(defun copy-libvterm-row (engine grid row)
  (declare (type grid grid)
           (optimize (speed 3)))
  (let* ((vt (libvterm-engine-vterm engine))
         (columns (grid-columns grid))
         (codes (libvterm-engine-codes engine))
         (fg (libvterm-engine-fg engine))
         (bg (libvterm-engine-bg engine))
         (attrs (libvterm-engine-attrs engine))
         (base (grid-index grid row 0)))
    (declare (type (simple-array (unsigned-byte 32) (*)) codes fg bg attrs))
    (libvterm:vterm-row-into vt row codes fg bg attrs :end columns)
    (dotimes (column columns)
      (let ((index (+ base column))
            (word (aref attrs column)))
        (grid-set-cell grid index
                       (aref codes column)
                       (aref fg column)
                       (aref bg column)
                       (libvterm-attrs->attributes word))
        (if (logbitp 8 word)
            (grid-set-combining
             grid index
             (map 'string #'code-char
                  (rest (getf (libvterm:vterm-cell vt row column) :codepoints))))
            (remhash index (grid-combining grid)))))))

(defmethod engine-update-grid ((engine libvterm-engine) grid &key force)
  (let* ((vt (libvterm-engine-vterm engine))
         (rows (libvterm:vterm-rows vt))
         (columns (libvterm:vterm-columns vt)))
    (unless (and (= rows (grid-rows grid)) (= columns (grid-columns grid)))
      (grid-resize grid rows columns)
      (setf force t))
    (let ((damage (libvterm:vterm-take-damage vt)))
      (dotimes (row rows)
        (when (or force (= 1 (sbit damage row)))
          (copy-libvterm-row engine grid row)
          (setf (sbit (grid-dirty grid) row) 1))))
    (multiple-value-bind (cursor-row cursor-column visiblep) (libvterm:vterm-cursor vt)
      (let ((cursor-row (min cursor-row (1- rows)))
            (cursor-column (min cursor-column (1- columns)))
            (visiblep (and visiblep t)))
        (when (or (/= cursor-row (grid-cursor-row grid))
                  (/= cursor-column (grid-cursor-column grid))
                  (not (eq visiblep (grid-cursor-visible-p grid))))
          (setf (sbit (grid-dirty grid) (grid-cursor-row grid)) 1
                (sbit (grid-dirty grid) cursor-row) 1))
        (setf (grid-cursor-row grid) cursor-row
              (grid-cursor-column grid) cursor-column
              (grid-cursor-visible-p grid) visiblep)))
    grid))

(defmethod engine-mode ((engine libvterm-engine) mode)
  (case mode
    (:alternate-screen (libvterm:vterm-altscreen-p (libvterm-engine-vterm engine)))
    (:mouse-tracking (case (libvterm:vterm-property (libvterm-engine-vterm engine) :mouse)
                       (1 :normal)
                       (2 :button)
                       (3 :any)))
    (t nil)))

(defmethod engine-encode-mouse ((engine libvterm-engine) action button row column modifiers)
  (let ((octets (libvterm:vterm-encode-mouse
                 (libvterm-engine-vterm engine) row column
                 :button (and (not (eq action :motion)) button)
                 :pressed (eq action :press)
                 :mods (remove :super modifiers))))
    (when (plusp (length octets))
      (babel:octets-to-string octets :encoding :utf-8 :errorp nil))))

(defmethod engine-encode-key ((engine libvterm-engine) key modifiers)
  (let ((vt (libvterm-engine-vterm engine))
        (mods (loop for modifier in modifiers
                    for mapped = (case modifier
                                   (:shift :shift)
                                   (:control :ctrl)
                                   (:meta :alt))
                    when mapped collect mapped)))
    (let ((octets (if (characterp key)
                      (libvterm:vterm-encode-char vt key :mods mods)
                      (libvterm:vterm-encode-key vt key :mods mods))))
      (and octets (plusp (length octets))
           (babel:octets-to-string octets :encoding :utf-8 :errorp nil)))))

(register-engine :libvterm
                 (lambda (&rest arguments)
                   (apply #'make-instance 'libvterm-engine arguments)))
