(in-package #:slt)

;;;; Glyph fonts for the frontends that rasterize their own glyphs into a
;;;; GLYPH-ATLAS (gl and clx): CoreText on macOS, FreeType elsewhere with
;;;; families found through Shinmera's font-discovery (fontconfig).  Sizes
;;;; are pixels.

#-darwin
(sb-ext:add-package-local-nickname '#:font-discovery '#:org.shirakumo.font-discovery '#:slt)

(defun open-glyph-font (family pixel-size)
  #+darwin (make-coretext-font family pixel-size)
  #-darwin (make-ft-font family pixel-size))

(defun glyph-font-cell-size (font)
  #+darwin (values (coretext-font-cell-width font) (coretext-font-cell-height font))
  #-darwin (values (ft-font-cell-width font) (ft-font-cell-height font)))

(defun glyph-font-rasterizer (font)
  #+darwin (coretext-rasterizer font)
  #-darwin (freetype-rasterizer font))

(defun glyph-font-shaper (font)
  "A GLYPH-ATLAS shaper for FONT, or NIL where text is not shaped yet."
  #+darwin (coretext-shaper font)
  #-darwin (progn font nil))

(defun close-glyph-font (font)
  #+darwin (free-coretext-font font)
  #-darwin (free-ft-font font))

(defun glyph-font-families ()
  "The installed monospace families, for choosing a default."
  #+darwin '()
  #-darwin (or (ignore-errors (font-monospace-families)) '()))

(defun glyph-libraries-available-p ()
  "True when the rasterizer's libraries load."
  #+darwin (progn (ensure-objc) t)
  #-darwin (ignore-errors (load-freetype) t))
