(in-package #:slt)

;;;; Glyph rasterization with CoreText.  Each grapheme is laid out as a
;;;; CTLine, so CoreText supplies fallback fonts (CJK, symbols, emoji),
;;;; positions combining marks and shapes emoji sequences; the line is drawn
;;;; into an alpha-only bitmap whose coverage is copied into the glyph
;;;; atlas.  A line set in a color font (Apple Color Emoji) is drawn in color
;;;; instead, scaled to fit its cells, and handed to the atlas as RGBA.

(defconstant +cf-string-encoding-utf8+ #x08000100)
(defconstant +cg-image-alpha-only+ 7)
;; kCGImageAlphaPremultipliedLast | kCGBitmapByteOrder32Big: R, G, B, A octets.
(defconstant +cg-rgba-premultiplied+ (logior 1 (ash 4 12)))
(defconstant +ct-font-color-glyphs-trait+ (ash 1 13))
(defconstant +ct-font-bold-trait+ 2)
(defconstant +ct-font-italic-trait+ 1)

(cffi:defcfun ("CFRelease" cf-release) :void (object :pointer))

(cffi:defcfun ("CFStringCreateWithCString" %cf-string-create) :pointer
  (allocator :pointer) (string :string) (encoding :uint32))

(defun cf-string (string)
  "A CFString holding STRING; release with CF-RELEASE."
  (%cf-string-create (cffi:null-pointer) string +cf-string-encoding-utf8+))

(cffi:defcfun ("CTFontCreateWithName" %ct-font-create-with-name) :pointer
  (name :pointer) (size :double) (matrix :pointer))

(cffi:defcfun ("CTFontCreateCopyWithSymbolicTraits" %ct-font-create-copy-with-traits) :pointer
  (font :pointer) (size :double) (matrix :pointer) (value :uint32) (mask :uint32))

(cffi:defcfun ("CTFontGetAscent" ct-font-ascent) :double (font :pointer))
(cffi:defcfun ("CTFontGetDescent" ct-font-descent) :double (font :pointer))
(cffi:defcfun ("CTFontGetLeading" ct-font-leading) :double (font :pointer))

(cffi:defcfun ("CTFontGetGlyphsForCharacters" %ct-font-glyphs) :bool
  (font :pointer) (characters :pointer) (glyphs :pointer) (count :long))

(cffi:defcfun ("CTFontGetAdvancesForGlyphs" %ct-font-advances) :double
  (font :pointer) (orientation :uint32) (glyphs :pointer) (advances :pointer) (count :long))

(cffi:defcfun ("CFDictionaryCreate" %cf-dictionary-create) :pointer
  (allocator :pointer) (keys :pointer) (values :pointer) (count :long)
  (key-callbacks :pointer) (value-callbacks :pointer))

(cffi:defcfun ("CFAttributedStringCreate" %cf-attributed-string-create) :pointer
  (allocator :pointer) (string :pointer) (attributes :pointer))

(cffi:defcfun ("CTLineCreateWithAttributedString" %ct-line-create) :pointer
  (string :pointer))

(cffi:defcfun ("CTLineDraw" %ct-line-draw) :void (line :pointer) (context :pointer))
(cffi:defcfun ("CTLineGetGlyphRuns" %ct-line-glyph-runs) :pointer (line :pointer))
(cffi:defcfun ("CTLineGetTypographicBounds" %ct-line-typographic-bounds) :double
  (line :pointer) (ascent :pointer) (descent :pointer) (leading :pointer))
(cffi:defcfun ("CTRunGetAttributes" %ct-run-attributes) :pointer (run :pointer))
(cffi:defcfun ("CTFontGetSymbolicTraits" %ct-font-symbolic-traits) :uint32 (font :pointer))
(cffi:defcfun ("CFArrayGetCount" %cf-array-count) :long (array :pointer))
(cffi:defcfun ("CFArrayGetValueAtIndex" %cf-array-value) :pointer (array :pointer) (index :long))
(cffi:defcfun ("CFDictionaryGetValue" %cf-dictionary-value) :pointer
  (dictionary :pointer) (key :pointer))
(cffi:defcfun ("CGColorSpaceCreateDeviceRGB" %cg-color-space-create-device-rgb) :pointer)
(cffi:defcfun ("CGColorSpaceRelease" %cg-color-space-release) :void (space :pointer))
(cffi:defcfun ("CGContextSaveGState" %cg-context-save-gstate) :void (context :pointer))
(cffi:defcfun ("CGContextRestoreGState" %cg-context-restore-gstate) :void (context :pointer))
(cffi:defcfun ("CGContextTranslateCTM" %cg-context-translate) :void
  (context :pointer) (x :double) (y :double))
(cffi:defcfun ("CGContextScaleCTM" %cg-context-scale) :void
  (context :pointer) (x :double) (y :double))

(cffi:defcfun ("CGBitmapContextCreate" %cg-bitmap-context-create) :pointer
  (data :pointer) (width :size) (height :size) (bits-per-component :size)
  (bytes-per-row :size) (color-space :pointer) (bitmap-info :uint32))

(cffi:defcfun ("CGContextRelease" cg-context-release) :void (context :pointer))
(cffi:defcfun ("CGContextClearRect" %cg-context-clear-rect) :void
  (context :pointer) (x :double) (y :double) (width :double) (height :double))
(cffi:defcfun ("CGContextSetTextPosition" %cg-context-set-text-position) :void
  (context :pointer) (x :double) (y :double))
(cffi:defcfun ("CGContextSetShouldSmoothFonts" %cg-context-set-should-smooth-fonts) :void
  (context :pointer) (smooth :bool))

(defun foreign-global (name)
  (cffi:mem-ref (cffi:foreign-symbol-pointer name) :pointer))

(defstruct (coretext-font (:constructor %make-coretext-font))
  (regular (cffi:null-pointer))
  (bold (cffi:null-pointer))
  (italic (cffi:null-pointer))
  (bold-italic (cffi:null-pointer))
  (cell-width 1 :type fixnum)
  (cell-height 1 :type fixnum)
  (baseline 0 :type fixnum)
  ;; Scratch bitmap two cells wide, reused for every glyph.
  (bitmap (cffi:null-pointer))
  (context (cffi:null-pointer))
  ;; The same for color glyphs, RGBA, made when the first one is drawn.
  (color-bitmap (cffi:null-pointer))
  (color-context (cffi:null-pointer)))

(defun ct-font-variant (font traits)
  (let ((variant (%ct-font-create-copy-with-traits font 0d0 (cffi:null-pointer) traits traits)))
    (if (cffi:null-pointer-p variant) font variant)))

(defun ct-advance-of (font char)
  (cffi:with-foreign-objects ((unichar :uint16) (glyph :uint16))
    (setf (cffi:mem-ref unichar :uint16) (char-code char))
    (%ct-font-glyphs font unichar glyph 1)
    (%ct-font-advances font 0 glyph (cffi:null-pointer) 1)))

(defun make-coretext-font (family size)
  "Open FAMILY at SIZE device pixels and measure its cell."
  (let* ((name (cf-string family))
         (regular (prog1 (%ct-font-create-with-name name (coerce size 'double-float)
                                                    (cffi:null-pointer))
                    (cf-release name)))
         (ascent (ct-font-ascent regular))
         (descent (ct-font-descent regular))
         (leading (ct-font-leading regular))
         (cell-width (max 1 (round (ct-advance-of regular #\M))))
         (cell-height (max 1 (ceiling (+ ascent descent leading))))
         (bitmap (cffi:foreign-alloc :uint8 :count (* 2 cell-width cell-height)
                                            :initial-element 0))
         (context (%cg-bitmap-context-create bitmap (* 2 cell-width) cell-height 8
                                             (* 2 cell-width) (cffi:null-pointer)
                                             +cg-image-alpha-only+)))
    (%cg-context-set-should-smooth-fonts context nil)
    (%make-coretext-font
     :regular regular
     :bold (ct-font-variant regular +ct-font-bold-trait+)
     :italic (ct-font-variant regular +ct-font-italic-trait+)
     :bold-italic (ct-font-variant regular (logior +ct-font-bold-trait+
                                                   +ct-font-italic-trait+))
     :cell-width cell-width
     :cell-height cell-height
     :baseline (+ (ceiling descent) (floor leading 2))
     :bitmap bitmap
     :context context)))

(defun free-coretext-font (font)
  (let ((released '()))
    (dolist (handle (list (coretext-font-regular font) (coretext-font-bold font)
                          (coretext-font-italic font) (coretext-font-bold-italic font)))
      (unless (or (cffi:null-pointer-p handle)
                  (member handle released :test #'cffi:pointer-eq))
        (push handle released)
        (cf-release handle))))
  (dolist (context (list (coretext-font-context font) (coretext-font-color-context font)))
    (unless (cffi:null-pointer-p context)
      (cg-context-release context)))
  (dolist (bitmap (list (coretext-font-bitmap font) (coretext-font-color-bitmap font)))
    (unless (cffi:null-pointer-p bitmap)
      (cffi:foreign-free bitmap)))
  (setf (coretext-font-context font) (cffi:null-pointer)
        (coretext-font-bitmap font) (cffi:null-pointer)
        (coretext-font-color-context font) (cffi:null-pointer)
        (coretext-font-color-bitmap font) (cffi:null-pointer)))

(defun coretext-font-for-style (font style)
  (let ((bold (member :bold style))
        (italic (member :italic style)))
    (cond ((and bold italic) (coretext-font-bold-italic font))
          (bold (coretext-font-bold font))
          (italic (coretext-font-italic font))
          (t (coretext-font-regular font)))))

(defun call-with-ct-line (ct-font string function)
  "Call FUNCTION with a CTLine of STRING set in CT-FONT, released after."
  (let ((text (cf-string string))
        (key (foreign-global "kCTFontAttributeName")))
    (cffi:with-foreign-objects ((keys :pointer) (values :pointer))
      (setf (cffi:mem-ref keys :pointer) key
            (cffi:mem-ref values :pointer) ct-font)
      (let* ((attributes (%cf-dictionary-create
                          (cffi:null-pointer) keys values 1
                          (cffi:foreign-symbol-pointer "kCFTypeDictionaryKeyCallBacks")
                          (cffi:foreign-symbol-pointer "kCFTypeDictionaryValueCallBacks")))
             (attributed (%cf-attributed-string-create (cffi:null-pointer) text attributes))
             (line (%ct-line-create attributed)))
        (unwind-protect (funcall function line)
          (cf-release line)
          (cf-release attributed)
          (cf-release attributes)
          (cf-release text))))))

(defun ct-line-color-p (line)
  "True when CoreText set some of LINE in a color font, as emoji are."
  (let ((runs (%ct-line-glyph-runs line))
        (key (foreign-global "kCTFontAttributeName")))
    (loop for i below (%cf-array-count runs)
          for font = (%cf-dictionary-value (%ct-run-attributes (%cf-array-value runs i)) key)
            thereis (and (not (cffi:null-pointer-p font))
                         (logtest (%ct-font-symbolic-traits font)
                                  +ct-font-color-glyphs-trait+)))))

(defun coretext-color-context (font)
  (when (cffi:null-pointer-p (coretext-font-color-context font))
    (let* ((width (* 2 (coretext-font-cell-width font)))
           (height (coretext-font-cell-height font))
           (bitmap (cffi:foreign-alloc :uint8 :count (* 4 width height) :initial-element 0))
           (space (%cg-color-space-create-device-rgb)))
      (setf (coretext-font-color-bitmap font) bitmap
            (coretext-font-color-context font)
            (%cg-bitmap-context-create bitmap width height 8 (* 4 width) space
                                       +cg-rgba-premultiplied+))
      (%cg-color-space-release space)))
  (coretext-font-color-context font))

(defun coretext-draw-color-line (font line width height)
  "Draw LINE in color, scaled to fit WIDTH x HEIGHT pixels and centered;
return the box's premultiplied RGBA pixels."
  (let* ((context (coretext-color-context font))
         (bitmap (coretext-font-color-bitmap font))
         (bitmap-width (* 2 (coretext-font-cell-width font)))
         (bitmap-height (coretext-font-cell-height font))
         (rgba (make-array (* 4 width height) :element-type '(unsigned-byte 8)
                                              :initial-element 0)))
    (%cg-context-clear-rect context 0d0 0d0
                            (coerce bitmap-width 'double-float)
                            (coerce bitmap-height 'double-float))
    (cffi:with-foreign-objects ((ascent :double) (descent :double) (leading :double))
      (let* ((line-width (max 1d0 (%ct-line-typographic-bounds line ascent descent leading)))
             (line-height (max 1d0 (+ (cffi:mem-ref ascent :double)
                                      (cffi:mem-ref descent :double))))
             (scale (min 1d0 (/ width line-width) (/ height line-height))))
        (%cg-context-save-gstate context)
        ;; CoreGraphics counts y up from the bottom.
        (%cg-context-translate context
                               (/ (- width (* line-width scale)) 2)
                               (+ (/ (- height (* line-height scale)) 2)
                                  (* (cffi:mem-ref descent :double) scale)))
        (%cg-context-scale context scale scale)
        (%cg-context-set-text-position context 0d0 0d0)
        (%ct-line-draw line context)
        (%cg-context-restore-gstate context)))
    ;; Bitmap rows are stored top to bottom, like the atlas.
    (dotimes (row (min height bitmap-height) rgba)
      (dotimes (byte (* 4 (min width bitmap-width)))
        (setf (aref rgba (+ byte (* 4 width row)))
              (cffi:mem-aref bitmap :uint8 (+ byte (* 4 bitmap-width row))))))))

(defun coretext-rasterizer (font)
  "A GLYPH-ATLAS rasterizer drawing with FONT."
  (lambda (string style pixels stride x y width height)
    (declare (type u8-vector pixels))
    (let* ((context (coretext-font-context font))
           (bitmap (coretext-font-bitmap font))
           (bitmap-width (* 2 (coretext-font-cell-width font)))
           (bitmap-height (coretext-font-cell-height font)))
      (call-with-ct-line
       (coretext-font-for-style font style) string
       (lambda (line)
         (if (ct-line-color-p line)
             (coretext-draw-color-line font line width height)
             (progn
               (%cg-context-clear-rect context 0d0 0d0
                                       (coerce bitmap-width 'double-float)
                                       (coerce bitmap-height 'double-float))
               (%cg-context-set-text-position context 0d0
                                              (coerce (coretext-font-baseline font)
                                                      'double-float))
               (%ct-line-draw line context)
               ;; Bitmap rows are stored top to bottom, like the atlas.
               (dotimes (row (min height bitmap-height))
                 (let ((source (* row bitmap-width))
                       (target (+ x (* stride (+ y row)))))
                   (dotimes (column (min width bitmap-width))
                     (setf (aref pixels (+ target column))
                           (cffi:mem-aref bitmap :uint8 (+ source column))))))
               nil)))))))
