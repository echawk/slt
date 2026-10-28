(in-package #:slt)

;;;; Glyph rasterization with CoreText.  Each grapheme is laid out as a
;;;; CTLine, so CoreText supplies fallback fonts (CJK, symbols, emoji),
;;;; positions combining marks and shapes emoji sequences; the line is drawn
;;;; into an alpha-only bitmap whose coverage is copied into the glyph
;;;; atlas.  A line set in a color font (Apple Color Emoji) is drawn in color
;;;; instead, scaled to fit its cells, and handed to the atlas as RGBA.
;;;;
;;;; CoreText's own fallback picks from a fixed list of Apple fonts, so the
;;;; icons programs print from the private use area (Nerd Font symbols) come
;;;; out as LastResort's placeholder.  Every font falls back first to
;;;; *FONT-FALLBACK-FAMILIES* (its cascade list), which by default names the
;;;; Symbols Nerd Font Mono slt ships, and a character that still ends up in
;;;; LastResort is looked for among the installed fonts.

(defconstant +cf-string-encoding-utf8+ #x08000100)
(defconstant +cg-image-alpha-only+ 7)
(defconstant +bits-per-component+ 8)
(defconstant +rgba-octets+ 4)
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

(cffi:defcfun ("CFArrayCreate" %cf-array-create) :pointer
  (allocator :pointer) (values :pointer) (count :long) (callbacks :pointer))
(cffi:defcfun ("CFCharacterSetCreateWithCharactersInRange" %cf-charset-create-with-range)
    :pointer
  (allocator :pointer) (location :long) (length :long))
(cffi:defcfun ("CFStringGetCString" %cf-string-get-c-string) :bool
  (string :pointer) (buffer :pointer) (size :long) (encoding :uint32))
(cffi:defcfun ("CFStringGetLength" %cf-string-length) :long (string :pointer))
(cffi:defcfun ("CFStringGetMaximumSizeForEncoding" %cf-string-maximum-size) :long
  (length :long) (encoding :uint32))
(cffi:defcfun ("CTFontDescriptorCreateWithAttributes" %ct-font-descriptor-create) :pointer
  (attributes :pointer))
(cffi:defcfun ("CTFontCreateWithFontDescriptor" %ct-font-create-with-descriptor) :pointer
  (descriptor :pointer) (size :double) (matrix :pointer))
(cffi:defcfun ("CTFontCreateCopyWithAttributes" %ct-font-create-copy-with-attributes) :pointer
  (font :pointer) (size :double) (matrix :pointer) (attributes :pointer))
(cffi:defcfun ("CTFontGetSize" %ct-font-size) :double (font :pointer))
(cffi:defcfun ("CTFontCopyPostScriptName" %ct-font-copy-postscript-name) :pointer
  (font :pointer))
(cffi:defcfun ("CTFontCollectionCreateWithFontDescriptors" %ct-font-collection-create) :pointer
  (descriptors :pointer) (options :pointer))
(cffi:defcfun ("CTFontCollectionCreateMatchingFontDescriptors"
               %ct-font-collection-matching-descriptors)
    :pointer
  (collection :pointer))

(defun cf-dictionary (&rest keys-and-values)
  "A CFDictionary of the CF objects KEYS-AND-VALUES; release with CF-RELEASE."
  (let ((count (floor (length keys-and-values) 2)))
    (cffi:with-foreign-objects ((keys :pointer count) (values :pointer count))
      (loop for (key value) on keys-and-values by #'cddr
            for i from 0
            do (setf (cffi:mem-aref keys :pointer i) key
                     (cffi:mem-aref values :pointer i) value))
      (%cf-dictionary-create (cffi:null-pointer) keys values count
                             (cffi:foreign-symbol-pointer "kCFTypeDictionaryKeyCallBacks")
                             (cffi:foreign-symbol-pointer "kCFTypeDictionaryValueCallBacks")))))

(defun cf-array (&rest objects)
  "A CFArray of the CF OBJECTS; release with CF-RELEASE."
  (cffi:with-foreign-object (values :pointer (max 1 (length objects)))
    (loop for object in objects
          for i from 0
          do (setf (cffi:mem-aref values :pointer i) object))
    (%cf-array-create (cffi:null-pointer) values (length objects)
                      (cffi:foreign-symbol-pointer "kCFTypeArrayCallBacks"))))

(defun cf-string-lisp (string)
  "The Lisp string of the CFString STRING."
  ;; Room for the encoded characters and the terminating NUL.
  (let ((size (1+ (%cf-string-maximum-size (%cf-string-length string)
                                           +cf-string-encoding-utf8+))))
    (cffi:with-foreign-object (buffer :char size)
      (if (%cf-string-get-c-string string buffer size +cf-string-encoding-utf8+)
          (cffi:foreign-string-to-lisp buffer)
          ""))))

;;; Fallback fonts.

(cffi:defcfun ("CFURLCreateFromFileSystemRepresentation" %cf-url-create) :pointer
  (allocator :pointer) (path :string) (length :long) (directory-p :bool))
(cffi:defcfun ("CTFontManagerRegisterFontsForURL" %ct-font-manager-register-url) :bool
  (url :pointer) (scope :uint32) (error :pointer))

(defconstant +ct-font-manager-scope-process+ 1)

(defvar *bundled-fonts-registered* nil)

(defun forget-bundled-fonts ()
  (setf *bundled-fonts-registered* nil))

;; Registration belongs to a process: a saved image registers again.
(pushnew 'forget-bundled-fonts sb-ext:*save-hooks*)

(defun register-bundled-fonts ()
  "Make the fonts slt ships available to this process by family name."
  (unless *bundled-fonts-registered*
    (setf *bundled-fonts-registered* t)
    (dolist (file (bundled-font-files))
      (let* ((path (uiop:native-namestring file))
             (url (%cf-url-create (cffi:null-pointer) path
                                  (length (sb-ext:string-to-octets path :external-format :utf-8))
                                  nil)))
        (%ct-font-manager-register-url url +ct-font-manager-scope-process+ (cffi:null-pointer))
        (cf-release url)))))

(defun ct-font-with-fallbacks (font)
  "A copy of FONT that falls back to *FONT-FALLBACK-FAMILIES* before the
system's fallbacks; FONT is released."
  (register-bundled-fonts)
  (let* ((names (mapcar #'cf-string *font-fallback-families*))
         (descriptors (loop for name in names
                            collect (let ((attributes (cf-dictionary
                                                       (foreign-global "kCTFontFamilyNameAttribute")
                                                       name)))
                                      (prog1 (%ct-font-descriptor-create attributes)
                                        (cf-release attributes)))))
         (cascade (apply #'cf-array descriptors))
         (attributes (cf-dictionary (foreign-global "kCTFontCascadeListAttribute") cascade))
         (descriptor (%ct-font-descriptor-create attributes))
         (copy (%ct-font-create-copy-with-attributes font 0d0 (cffi:null-pointer) descriptor)))
    (mapc #'cf-release (list* descriptor attributes cascade (append descriptors names)))
    (if (cffi:null-pointer-p copy)
        font
        (progn (cf-release font) copy))))

(defparameter +last-resort-font-name+ "LastResort"
  "The PostScript name of the font CoreText draws characters no font has
with, as placeholders.")

(defun ct-font-last-resort-p (font)
  "True for the LastResort font."
  (let ((name (%ct-font-copy-postscript-name font)))
    (prog1 (string= +last-resort-font-name+ (cf-string-lisp name))
      (cf-release name))))

(defun installed-font-for (code size)
  "A new CTFont of SIZE from some installed font with the character CODE,
or a null pointer."
  (let* ((set (%cf-charset-create-with-range (cffi:null-pointer) code 1))
         (attributes (cf-dictionary (foreign-global "kCTFontCharacterSetAttribute") set))
         (descriptor (%ct-font-descriptor-create attributes))
         (descriptors (cf-array descriptor))
         (collection (%ct-font-collection-create descriptors (cffi:null-pointer)))
         (matches (%ct-font-collection-matching-descriptors collection)))
    (unwind-protect
         (if (or (cffi:null-pointer-p matches) (zerop (%cf-array-count matches)))
             (cffi:null-pointer)
             (loop for i below (%cf-array-count matches)
                   for font = (%ct-font-create-with-descriptor (%cf-array-value matches i)
                                                               size (cffi:null-pointer))
                   do (if (ct-font-last-resort-p font)
                          (cf-release font)
                          (return font))
                   finally (return (cffi:null-pointer))))
      (dolist (object (list matches collection descriptors descriptor attributes set))
        (unless (cffi:null-pointer-p object)
          (cf-release object))))))

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
  (color-context (cffi:null-pointer))
  ;; Codepoints CoreText could only draw with LastResort -> the installed
  ;; font found to have them, or a null pointer.
  (fallbacks (make-hash-table) :type hash-table))

(defun ct-font-variant (font traits)
  (let ((variant (%ct-font-create-copy-with-traits font 0d0 (cffi:null-pointer) traits traits)))
    (if (cffi:null-pointer-p variant) font (ct-font-with-fallbacks variant))))

(defun ct-advance-of (font char)
  (cffi:with-foreign-objects ((unichar :uint16) (glyph :uint16))
    (setf (cffi:mem-ref unichar :uint16) (char-code char))
    (%ct-font-glyphs font unichar glyph 1)
    (%ct-font-advances font 0 glyph (cffi:null-pointer) 1)))

(defun make-coretext-font (family size)
  "Open FAMILY at SIZE device pixels and measure its cell."
  (let* ((name (cf-string family))
         (regular (ct-font-with-fallbacks
                   (prog1 (%ct-font-create-with-name name (coerce size 'double-float)
                                                     (cffi:null-pointer))
                     (cf-release name))))
         (ascent (ct-font-ascent regular))
         (descent (ct-font-descent regular))
         (leading (ct-font-leading regular))
         (cell-width (max 1 (round (ct-advance-of regular #\M))))
         (cell-height (max 1 (ceiling (+ ascent descent leading))))
         (bitmap (cffi:foreign-alloc :uint8 :count (* 2 cell-width cell-height)
                                            :initial-element 0))
         (context (%cg-bitmap-context-create bitmap (* 2 cell-width) cell-height
                                             +bits-per-component+
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
        (cf-release handle)))
    (maphash (lambda (code handle)
               (declare (ignore code))
               (unless (cffi:null-pointer-p handle)
                 (cf-release handle)))
             (coretext-font-fallbacks font))
    (clrhash (coretext-font-fallbacks font)))
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
           (bitmap (cffi:foreign-alloc :uint8 :count (* +rgba-octets+ width height)
                                             :initial-element 0))
           (space (%cg-color-space-create-device-rgb)))
      (setf (coretext-font-color-bitmap font) bitmap
            (coretext-font-color-context font)
            (%cg-bitmap-context-create bitmap width height +bits-per-component+
                                       (* +rgba-octets+ width) space
                                       +cg-rgba-premultiplied+))
      (%cg-color-space-release space)))
  (coretext-font-color-context font))

(defparameter +ink-margin+ 1/4
  "Room left around a color line when measuring its ink, as a fraction of
its height: glyphs may reach beyond their typographic bounds.")

(defun call-with-rgba-bitmap (width height function)
  "Call FUNCTION with a cleared RGBA bitmap context of WIDTH x HEIGHT pixels
and its pixels, both freed after."
  (let* ((bitmap (cffi:foreign-alloc :uint8 :count (* +rgba-octets+ width height)
                                            :initial-element 0))
         (space (%cg-color-space-create-device-rgb))
         (context (%cg-bitmap-context-create bitmap width height +bits-per-component+
                                             (* +rgba-octets+ width)
                                             space +cg-rgba-premultiplied+)))
    (%cg-color-space-release space)
    (unwind-protect (funcall function context bitmap)
      (cg-context-release context)
      (cffi:foreign-free bitmap))))

(defun rgba-ink-box (bitmap width height)
  "The box of BITMAP's pixels that are not transparent, as (values LEFT
TOP RIGHT BOTTOM) with rows counted from the top, or NIL."
  (let ((left width) (top height) (right 0) (bottom 0))
    (dotimes (row height)
      (dotimes (column width)
        (when (plusp (cffi:mem-aref bitmap :uint8
                                    (+ (1- +rgba-octets+)
                                       (* +rgba-octets+ (+ column (* row width))))))
          (setf left (min left column) right (max right (1+ column))
                top (min top row) bottom (max bottom (1+ row))))))
    (and (< left right) (values left top right bottom))))

(defun ct-line-ink (line)
  "Where LINE's ink lies relative to its pen position, in pixels with y
up: (values LEFT BOTTOM WIDTH HEIGHT), or NIL when it draws nothing."
  (cffi:with-foreign-objects ((ascent :double) (descent :double) (leading :double))
    (let* ((line-width (%ct-line-typographic-bounds line ascent descent leading))
           (line-height (+ (cffi:mem-ref ascent :double) (cffi:mem-ref descent :double)))
           (margin (ceiling (* +ink-margin+ (max line-width line-height))))
           (width (+ (ceiling line-width) (* 2 margin)))
           (height (+ (ceiling line-height) (* 2 margin)))
           (pen-x margin)
           (pen-y (+ margin (cffi:mem-ref descent :double))))
      (call-with-rgba-bitmap
       width height
       (lambda (context bitmap)
         (%cg-context-set-text-position context (coerce pen-x 'double-float)
                                        (coerce pen-y 'double-float))
         (%ct-line-draw line context)
         (multiple-value-bind (left top right bottom) (rgba-ink-box bitmap width height)
           (and left
                ;; Bitmap rows run top down; CoreGraphics' y runs up.
                (values (- left pen-x) (- height bottom pen-y)
                        (- right left) (- bottom top)))))))))

(defun coretext-draw-color-line (font line width height)
  "Draw LINE in color with its ink scaled to fill WIDTH x HEIGHT pixels,
keeping its proportions, and centered; return the box's premultiplied RGBA
pixels."
  (let* ((context (coretext-color-context font))
         (bitmap (coretext-font-color-bitmap font))
         (bitmap-width (* 2 (coretext-font-cell-width font)))
         (bitmap-height (coretext-font-cell-height font))
         (rgba (make-array (* +rgba-octets+ width height) :element-type '(unsigned-byte 8)
                                                         :initial-element 0)))
    (%cg-context-clear-rect context 0d0 0d0
                            (coerce bitmap-width 'double-float)
                            (coerce bitmap-height 'double-float))
    (multiple-value-bind (ink-x ink-y ink-width ink-height) (ct-line-ink line)
      (when ink-x
        (let ((scale (coerce (min (/ width ink-width) (/ height ink-height)) 'double-float)))
          (%cg-context-save-gstate context)
          ;; Put the scaled ink box in the middle of the cells.
          (%cg-context-translate context
                                 (- (/ (- width (* ink-width scale)) 2) (* ink-x scale))
                                 (- (/ (- height (* ink-height scale)) 2) (* ink-y scale)))
          (%cg-context-scale context scale scale)
          (%cg-context-set-text-position context 0d0 0d0)
          (%ct-line-draw line context)
          (%cg-context-restore-gstate context))))
    ;; Bitmap rows are stored top to bottom, like the atlas.
    (dotimes (row (min height bitmap-height) rgba)
      (dotimes (byte (* +rgba-octets+ (min width bitmap-width)))
        (setf (aref rgba (+ byte (* +rgba-octets+ width row)))
              (cffi:mem-aref bitmap :uint8 (+ byte (* +rgba-octets+ bitmap-width row))))))))

(defun ct-line-last-resort-p (line)
  "True when CoreText found no font but LastResort for some of LINE."
  (let ((runs (%ct-line-glyph-runs line))
        (key (foreign-global "kCTFontAttributeName")))
    (loop for i below (%cf-array-count runs)
          for run-font = (%cf-dictionary-value (%ct-run-attributes (%cf-array-value runs i)) key)
            thereis (and (not (cffi:null-pointer-p run-font))
                         (ct-font-last-resort-p run-font)))))

(defun coretext-fallback-font (font code)
  "An installed font with the character CODE, at FONT's size, or NIL; the
answer is kept."
  (let ((found (multiple-value-bind (handle known)
                   (gethash code (coretext-font-fallbacks font))
                 (if known
                     handle
                     (setf (gethash code (coretext-font-fallbacks font))
                           (installed-font-for code (%ct-font-size
                                                     (coretext-font-regular font))))))))
    (and (not (cffi:null-pointer-p found)) found)))

(defun call-with-coretext-line (font style string function)
  "Call FUNCTION with a CTLine of STRING in FONT's STYLE, or, when CoreText
would draw its first character with LastResort, in an installed font that
has it."
  (let* ((fallback nil)
         (result (call-with-ct-line
                  (coretext-font-for-style font style) string
                  (lambda (line)
                    (unless (and (ct-line-last-resort-p line)
                                 (setf fallback (coretext-fallback-font
                                                 font (char-code (char string 0)))))
                      (funcall function line))))))
    (if fallback
        (call-with-ct-line fallback string function)
        result)))

(defun coretext-rasterizer (font)
  "A GLYPH-ATLAS rasterizer drawing with FONT."
  (lambda (string style pixels stride x y width height)
    (declare (type u8-vector pixels))
    (let* ((context (coretext-font-context font))
           (bitmap (coretext-font-bitmap font))
           (bitmap-width (* 2 (coretext-font-cell-width font)))
           (bitmap-height (coretext-font-cell-height font)))
      (call-with-coretext-line
       font style string
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
