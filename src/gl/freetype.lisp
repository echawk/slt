(in-package #:slt)

;;;; Glyph rasterization for the gl frontend on Linux and the BSDs (macOS
;;;; uses CoreText, src/macos/coretext.lisp).  font-discovery (fontconfig)
;;;; finds the family's files; characters the family lacks come from the
;;;; first font in *FONT-FALLBACK-FAMILIES*, then any installed font, that
;;;; has them.  Combining marks are placed without shaping (no HarfBuzz yet).
;;;;
;;;; The structure offsets below are FreeType's public ABI on LP64
;;;; platforms (every 64-bit Linux and BSD).

(defun load-named-library (name candidates message)
  "Load the first loadable of CANDIDATES as the foreign library NAME."
  (unless (ignore-errors (cffi:foreign-library-loaded-p name))
    (eval `(cffi:define-foreign-library ,name (t (:or ,@candidates))))
    (handler-case (cffi:load-foreign-library name)
      (cffi:load-foreign-library-error ()
        (error "~a (tried ~{~a~^, ~})." message candidates)))))

(defun load-freetype ()
  (load-named-library 'libfreetype '("libfreetype.so.6" "libfreetype.so")
                      "FreeType was not found; install it (libfreetype6 / freetype)"))

(defvar *font-fallback-families*
  '("DejaVu Sans Mono" "Noto Sans Mono" "Noto Sans Symbols" "Noto Sans Symbols 2"
    "Noto Sans CJK SC" "Noto Sans Math" "DejaVu Sans" "Symbola" "Unifont")
  "Families to try, in order, for characters the terminal font lacks,
before searching every installed font.")

(defun font-monospace-families ()
  (sort (remove-duplicates
         (loop for font in (font-discovery:list-fonts :spacing :monospace)
               for family = (font-discovery:family font)
               when (and family (plusp (length family)))
                 collect family)
         :test #'string=)
        #'string<))

;;; FreeType

(cffi:defcfun ("FT_Init_FreeType" %ft-init-freetype) :int (library :pointer))
(cffi:defcfun ("FT_Done_FreeType" %ft-done-freetype) :int (library :pointer))
(cffi:defcfun ("FT_New_Face" %ft-new-face) :int
  (library :pointer) (path :string) (index :long) (face :pointer))
(cffi:defcfun ("FT_Done_Face" %ft-done-face) :int (face :pointer))
(cffi:defcfun ("FT_Set_Pixel_Sizes" %ft-set-pixel-sizes) :int
  (face :pointer) (width :uint) (height :uint))
(cffi:defcfun ("FT_Get_Char_Index" %ft-get-char-index) :uint (face :pointer) (code :ulong))
(cffi:defcfun ("FT_Load_Glyph" %ft-load-glyph) :int (face :pointer) (index :uint) (flags :int32))
(cffi:defcfun ("FT_Render_Glyph" %ft-render-glyph) :int (slot :pointer) (mode :int))
(cffi:defcfun ("FT_GlyphSlot_Embolden" %ft-glyph-slot-embolden) :void (slot :pointer))
(cffi:defcfun ("FT_GlyphSlot_Oblique" %ft-glyph-slot-oblique) :void (slot :pointer))

(defconstant +ft-face-flag-scalable+ 1)
(defconstant +ft-style-flag-italic+ 1)
(defconstant +ft-style-flag-bold+ 2)
(defconstant +ft-load-target-light+ #x10000)
(defconstant +ft-render-mode-light+ 1)
(defconstant +ft-pixel-mode-mono+ 1)
(defconstant +ft-pixel-mode-gray+ 2)

;; FT_FaceRec
(defconstant +ft-face-count-offset+ 0)
(defconstant +ft-face-flags-offset+ 16)
(defconstant +ft-face-style-flags-offset+ 24)
(defconstant +ft-face-family-name-offset+ 40)
(defconstant +ft-face-glyph-offset+ 152)
(defconstant +ft-face-size-offset+ 160)
;; FT_SizeRec.metrics (FT_Size_Metrics) and its fields
(defconstant +ft-size-ascender-offset+ 48)
(defconstant +ft-size-descender-offset+ 56)
(defconstant +ft-size-height-offset+ 64)
;; FT_GlyphSlotRec
(defconstant +ft-slot-advance-x-offset+ 128)
(defconstant +ft-slot-bitmap-rows-offset+ 152)
(defconstant +ft-slot-bitmap-width-offset+ 156)
(defconstant +ft-slot-bitmap-pitch-offset+ 160)
(defconstant +ft-slot-bitmap-buffer-offset+ 168)
(defconstant +ft-slot-bitmap-pixel-mode-offset+ 178)
(defconstant +ft-slot-bitmap-left-offset+ 192)
(defconstant +ft-slot-bitmap-top-offset+ 196)

(defstruct ft-face
  (pointer (cffi:null-pointer))
  (synthetic-bold nil)
  (synthetic-italic nil))

(defstruct (ft-font (:constructor %make-ft-font))
  (library (cffi:null-pointer))
  (family "" :type string)
  (pixel-size 15 :type fixnum)
  ;; "file#index" -> FT_Face pointer, so each file is opened once.
  (open-faces (make-hash-table :test #'equal) :type hash-table)
  ;; Style list -> FT-FACE for the primary family.
  (styles (make-hash-table :test #'equal) :type hash-table)
  ;; (code . style) -> FT-FACE that has the character, or :none.
  (fallbacks (make-hash-table :test #'equal) :type hash-table)
  ;; Font files to search for fallbacks, most preferred first.
  (fallback-files nil :type list)
  (cell-width 1 :type fixnum)
  (cell-height 1 :type fixnum)
  ;; Pixels from the top of a cell to the baseline.
  (baseline 0 :type fixnum))

(defun ft-new-face (font file index)
  "Open face INDEX of FILE, or return NIL."
  (cffi:with-foreign-object (face :pointer)
    (when (zerop (%ft-new-face (ft-font-library font) file index face))
      (cffi:mem-ref face :pointer))))

(defun ft-open-face (font file index)
  "Face INDEX of FILE at the font's size; each is opened once."
  (let ((key (format nil "~a#~d" file index)))
    (or (gethash key (ft-font-open-faces font))
        (let ((pointer (or (ft-new-face font file index)
                           (error "FreeType could not open ~a." file))))
          (when (ft-scalable-p pointer)
            (%ft-set-pixel-sizes pointer 0 (ft-font-pixel-size font)))
          (setf (gethash key (ft-font-open-faces font)) pointer)))))

(defun ft-scalable-p (pointer)
  (logtest (cffi:mem-ref pointer :long +ft-face-flags-offset+) +ft-face-flag-scalable+))

(defun ft-style-flags (pointer)
  (cffi:mem-ref pointer :long +ft-face-style-flags-offset+))

(defun ft-family-name (pointer)
  (let ((name (cffi:mem-ref pointer :pointer +ft-face-family-name-offset+)))
    (unless (cffi:null-pointer-p name)
      (cffi:foreign-string-to-lisp name :encoding :utf-8))))

(defun ft-style-wanted (style)
  (logior (if (member :bold style) +ft-style-flag-bold+ 0)
          (if (member :italic style) +ft-style-flag-italic+ 0)))

(defun ft-pick-face-index (font file family style)
  "The face in FILE (a collection may hold several) that best matches
FAMILY and STYLE."
  (let ((first (ft-new-face font file 0))
        (wanted (ft-style-wanted style)))
    (unless first
      (return-from ft-pick-face-index 0))
    (let ((count (cffi:mem-ref first :long +ft-face-count-offset+))
          (best 0)
          (best-score -1))
      (%ft-done-face first)
      (dotimes (index count best)
        (let ((face (ft-new-face font file index)))
          (when face
            (let ((score (+ (if (and family (string-equal family (ft-family-name face))) 4 0)
                            (if (= wanted (logand 3 (ft-style-flags face))) 2 0))))
              (when (> score best-score)
                (setf best index best-score score)))
            (%ft-done-face face)))))))

(defun ft-face-from-file (font file family style)
  "An FT-FACE for FILE in STYLE, noting the traits FreeType must synthesize."
  (let* ((pointer (ft-open-face font file (ft-pick-face-index font file family style)))
         (missing (logandc2 (ft-style-wanted style) (ft-style-flags pointer))))
    (make-ft-face :pointer pointer
                  :synthetic-bold (logtest missing +ft-style-flag-bold+)
                  :synthetic-italic (logtest missing +ft-style-flag-italic+))))

(defun find-font-file (&rest query)
  (let ((font (ignore-errors (apply #'font-discovery:find-font query))))
    (and font (font-discovery:file font) (uiop:native-namestring (font-discovery:file font)))))

(defun ft-font-style-face (font style)
  (let ((key (list (and (member :bold style) t) (and (member :italic style) t))))
    (or (gethash key (ft-font-styles font))
        (let* ((family (ft-font-family font))
               (file (or (find-font-file :family family
                                         :weight (if (member :bold style) :bold :regular)
                                         :slant (if (member :italic style) :italic :roman))
                         (find-font-file :family family)
                         (find-font-file :spacing :monospace)
                         (error "No font file was found for ~s." family))))
          (setf (gethash key (ft-font-styles font))
                (ft-face-from-file font file family style))))))

(defun ft-has-char-p (face code)
  (plusp (%ft-get-char-index (ft-face-pointer face) code)))

(defun ft-fallback-files (font)
  "Font files to search for missing characters: the preferred fallback
families first, then every other installed font."
  (or (ft-font-fallback-files font)
      (setf (ft-font-fallback-files font)
            (remove-duplicates
             (append (loop for family in *font-fallback-families*
                           for file = (find-font-file :family family)
                           when file collect (cons file family))
                     (loop for font in (ignore-errors (font-discovery:list-fonts))
                           for file = (font-discovery:file font)
                           when file
                             collect (cons (uiop:native-namestring file)
                                           (font-discovery:family font))))
             :key #'car :test #'string= :from-end t))))

(defun ft-file-has-char-p (font file code)
  "True when the first face of FILE is scalable and has CODE, checked
without keeping the face open."
  (let ((face (ft-new-face font file 0)))
    (when face
      (prog1 (and (ft-scalable-p face) (plusp (%ft-get-char-index face code)))
        (%ft-done-face face)))))

(defun ft-find-fallback (font code style)
  (loop for (file . family) in (ft-fallback-files font)
        when (ft-file-has-char-p font file code)
          do (let ((face (ft-face-from-file font file family style)))
               (when (ft-has-char-p face code)
                 (return face)))))

(defun ft-face-for (font code style)
  "The face to draw CODE with in STYLE: the family's own, else a fallback."
  (let ((face (ft-font-style-face font style)))
    (if (ft-has-char-p face code)
        face
        (let* ((key (cons code style))
               (fallback (or (gethash key (ft-font-fallbacks font))
                             (setf (gethash key (ft-font-fallbacks font))
                                   (or (ft-find-fallback font code style) :none)))))
          (if (eq fallback :none) face fallback)))))

(defun ft-load-glyph (face code)
  "Load and render CODE in FACE; return the glyph slot, or NIL."
  (let* ((pointer (ft-face-pointer face))
         (index (%ft-get-char-index pointer code)))
    (when (zerop (%ft-load-glyph pointer index +ft-load-target-light+))
      (let ((slot (cffi:mem-ref pointer :pointer +ft-face-glyph-offset+)))
        (when (ft-face-synthetic-bold face) (%ft-glyph-slot-embolden slot))
        (when (ft-face-synthetic-italic face) (%ft-glyph-slot-oblique slot))
        (when (zerop (%ft-render-glyph slot +ft-render-mode-light+))
          slot)))))

(defun make-ft-font (family pixel-size)
  "Open FAMILY at PIXEL-SIZE and measure its cell."
  (load-freetype)
  (let ((font (%make-ft-font :family family :pixel-size (max 1 (round pixel-size)))))
    (cffi:with-foreign-object (library :pointer)
      (unless (zerop (%ft-init-freetype library))
        (error "FreeType could not be initialized."))
      (setf (ft-font-library font) (cffi:mem-ref library :pointer)))
    (let* ((face (ft-font-style-face font '()))
           (size (cffi:mem-ref (ft-face-pointer face) :pointer +ft-face-size-offset+))
           (ascent (ceiling (cffi:mem-ref size :long +ft-size-ascender-offset+) 64))
           (descent (ceiling (- (cffi:mem-ref size :long +ft-size-descender-offset+)) 64))
           (line (round (cffi:mem-ref size :long +ft-size-height-offset+) 64))
           (height (max 1 line (+ ascent descent)))
           (slot (ft-load-glyph face (char-code #\M))))
      (setf (ft-font-cell-width font)
            (max 1 (if slot
                       (round (cffi:mem-ref slot :long +ft-slot-advance-x-offset+) 64)
                       (ceiling (ft-font-pixel-size font) 2)))
            (ft-font-cell-height font) height
            (ft-font-baseline font) (+ ascent (floor (- height ascent descent) 2))))
    font))

(defun free-ft-font (font)
  (loop for pointer being the hash-values of (ft-font-open-faces font)
        do (%ft-done-face pointer))
  (clrhash (ft-font-open-faces font))
  (clrhash (ft-font-styles font))
  (clrhash (ft-font-fallbacks font))
  (unless (cffi:null-pointer-p (ft-font-library font))
    (%ft-done-freetype (ft-font-library font))
    (setf (ft-font-library font) (cffi:null-pointer))))

(defun ft-blit (slot pen pixels stride x y width height baseline)
  "Combine SLOT's bitmap, drawn with its origin PEN pixels into the box at
X, Y (WIDTH x HEIGHT), into PIXELS."
  (declare (type u8-vector pixels)
           (type fixnum pen stride x y width height baseline))
  (let* ((rows (cffi:mem-ref slot :uint +ft-slot-bitmap-rows-offset+))
         (columns (cffi:mem-ref slot :uint +ft-slot-bitmap-width-offset+))
         (pitch (cffi:mem-ref slot :int +ft-slot-bitmap-pitch-offset+))
         (buffer (cffi:mem-ref slot :pointer +ft-slot-bitmap-buffer-offset+))
         (mode (cffi:mem-ref slot :uint8 +ft-slot-bitmap-pixel-mode-offset+))
         (left (+ pen (cffi:mem-ref slot :int +ft-slot-bitmap-left-offset+)))
         (top (- baseline (cffi:mem-ref slot :int +ft-slot-bitmap-top-offset+))))
    (unless (or (cffi:null-pointer-p buffer)
                (not (member mode (list +ft-pixel-mode-gray+ +ft-pixel-mode-mono+))))
      (loop for row from (max 0 (- top)) below (min rows (- height top))
            for source-row = (if (minusp pitch)
                                 (* (- rows row 1) (- pitch))
                                 (* row pitch))
            for target-row = (* stride (+ y top row))
            do (loop for column from (max 0 (- left)) below (min columns (- width left))
                     for value = (if (= mode +ft-pixel-mode-gray+)
                                     (cffi:mem-aref buffer :uint8 (+ source-row column))
                                     (if (logbitp (- 7 (mod column 8))
                                                  (cffi:mem-aref buffer :uint8
                                                                 (+ source-row (floor column 8))))
                                         255 0))
                     for target = (+ target-row x left column)
                     do (setf (aref pixels target) (max (aref pixels target) value)))))))

(defun freetype-rasterizer (font)
  "A GLYPH-ATLAS rasterizer drawing with FONT."
  (lambda (string style pixels stride x y width height)
    (declare (type u8-vector pixels))
    ;; Clear the box: slots are reused after the atlas is cleared.
    (dotimes (row height)
      (fill pixels 0 :start (+ x (* stride (+ y row))) :end (+ x width (* stride (+ y row)))))
    ;; Without shaping, a non-spacing mark is placed by its advance: a
    ;; zero-advance mark is designed to follow its base, while one with an
    ;; advance (as in Menlo) is drawn over the base's origin.
    (let ((pen 0)
          (base-pen 0))
      (loop for char across string
            for code = (char-code char)
            for mark = (member (sb-unicode:general-category char) '(:mn :me))
            for face = (ft-face-for font code style)
            for slot = (ft-load-glyph face code)
            when slot
              do (let ((advance (cffi:mem-ref slot :long +ft-slot-advance-x-offset+)))
                   (ft-blit slot (round (if (and mark (plusp advance)) base-pen pen) 64)
                            pixels stride x y width height (ft-font-baseline font))
                   (unless mark
                     (setf base-pen pen)
                     (incf pen advance)))))))
