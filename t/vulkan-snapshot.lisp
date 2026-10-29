;;;; Smoke test for the Vulkan renderer: render styled text and an image
;;;; offscreen, with no window, and check the pixels as t/gl-snapshot.lisp
;;;; does.  Run with scripts/vulkan-test; needs a Vulkan loader and driver
;;;; (MoltenVK on macOS; on Linux CI, lavapipe).  Leaves
;;;; build/vulkan-test/snapshot.ppm for inspection.

(asdf:load-system "slt")
(asdf:load-system "slt/glyphs")
(asdf:load-system "slt/vulkan")
(in-package #:slt)

(defparameter *output* (merge-pathnames "build/vulkan-test/" (asdf:system-source-directory "slt")))
(ensure-directories-exist *output*)

(defparameter *rows* 6)
(defparameter *columns* 40)
(defparameter *font-pixels* 30)

;; As t/gl-snapshot.lisp's shell prints it, so the same cells are checked.
(defparameter *text*
  (let ((esc (string (code-char 27)))
        (newline (format nil "~c~c" #\Return #\Newline)))
    (concatenate 'string
                 esc "[1mbold" esc "[0m plain " esc "[3mitalic" esc "[0m " esc "[4munderline" esc "[0m"
                 newline esc "[41m  red  " esc "[0m " esc "[32mgreen" esc "[0m 中文 e" (string (code-char #x301)) " ──"
                 newline "😀 -> !=")))

(defvar *failures* '())

(defun check (ok description)
  (format t "~&~:[FAIL~;ok  ~] ~a~%" ok description)
  (unless ok (push description *failures*)))

(defun pixel (rgb width x y)
  (let ((i (* 3 (+ x (* y width)))))
    (logior (ash (aref rgb i) 16) (ash (aref rgb (+ i 1)) 8) (aref rgb (+ i 2)))))

(defun red-image (store)
  "A one-pixel red image placed on row 3, column 1, over the text."
  (store-image store 1 0 1 1 (make-array 4 :element-type '(unsigned-byte 8)
                                           :initial-contents '(255 0 0 255)))
  (push (make-terminal-image-placement :image-id 1 :row 3 :column 1 :columns 1 :rows 1
                                       :z-index 1)
        (terminal-image-store-placements store)))

(defun check-snapshot (renderer)
  (let* ((engine (make-engine :lispvt :rows *rows* :columns *columns*))
         (palette (make-palette))
         (font (open-glyph-font (default-font-family) *font-pixels*))
         (store (make-terminal-image-store)))
    (engine-feed-string engine *text*)
    (red-image store)
    (multiple-value-bind (cw ch) (glyph-font-cell-size font)
      (let* ((grid (engine-update-grid engine (make-grid 1 1)))
             (atlas (make-glyph-atlas cw ch (glyph-font-rasterizer font)
                                      :shaper (glyph-font-shaper font)))
             (instances (pack-grid grid atlas palette nil))
             (width (* cw *columns*))
             (height (* ch *rows*))
             (rgb (vulkan-snapshot renderer instances *columns* (* *rows* *columns*) cw ch
                                   width height atlas palette
                                   (terminal-image-render-list store cw ch))))
        (write-ppm (merge-pathnames "snapshot.ppm" *output*) rgb width height)
        (format t "~&frame ~dx~d, cell ~dx~d~%" width height cw ch)
        (check (= (pixel rgb width (1- width) (1- height)) (palette-background palette))
               "the bottom-right corner is the background")
        (check (> (loop for y below ch
                        count (loop for x below (* 4 cw)
                                    thereis (/= (pixel rgb width x y)
                                                (palette-background palette))))
                  (floor ch 3))
               "\"bold\" in the first row draws glyph pixels")
        (check (= (pixel rgb width (floor cw 2) (+ ch 2)) (aref (palette-colors palette) 1))
               "SGR 41 fills its cells with palette red")
        (check (= (pixel rgb width (+ cw (floor cw 2)) (+ (* 3 ch) (floor ch 2))) #xff0000)
               "RGBA terminal images are composited over the grid")
        (check (find-if (lambda (y)
                          (= (pixel rgb width (+ (* 21 cw) (floor cw 2)) y)
                             (palette-foreground palette)))
                        (loop for y from ch below (* 2 ch) collect y))
               "box drawing characters reach the cell centre")
        (check (loop for y from (* 2 ch) below (* 3 ch)
                     thereis (loop for x below (* 2 cw)
                                   for p = (pixel rgb width x y)
                                   thereis (and (> (ldb (byte 8 16) p) 180)
                                                (> (ldb (byte 8 8) p) 120)
                                                (< (ldb (byte 8 0) p) 90))))
               "a color emoji is drawn in color")))
    (close-glyph-font font)))

(glyph-libraries-available-p)
(float-features:with-float-traps-masked t
  (let ((renderer (make-vulkan-renderer)))
    (unwind-protect
         (handler-case (check-snapshot renderer)
           (error (condition)
             (check nil (format nil "snapshot: ~a" condition))))
      (free-vulkan-renderer renderer))))
(uiop:quit (if *failures* 1 0))
