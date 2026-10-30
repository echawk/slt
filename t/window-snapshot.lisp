;;;; Smoke test for a window frontend: run a shell that prints styled text,
;;;; render the window offscreen and check the pixels, as t/gl-snapshot.lisp
;;;; does for the gl frontend.  SLT_FRONTEND names the frontend (x11,
;;;; wayland, gl) and SLT_GL_API what it draws with (gl, gles, vulkan).
;;;; Run with scripts/window-test, which starts Xvfb or Weston as the
;;;; frontend needs.  Leaves build/window-test/FRONTEND-API/snapshot.ppm.
;;;; The wayland frontend's input is checked too (t/wayland-input.lisp).

(asdf:load-system "slt")
(defpackage #:slt/window-test (:use #:cl))
(in-package #:slt/window-test)

(defun environment-keyword (name default)
  (let ((value (uiop:getenv name)))
    (if (and value (plusp (length value)))
        (intern (string-upcase value) :keyword)
        default)))

(defparameter *frontend* (environment-keyword "SLT_FRONTEND" :x11))
(defparameter *api* (environment-keyword "SLT_GL_API" :auto))

(asdf:load-system (format nil "slt/~(~a~)" *frontend*))
(in-package #:slt)

(defparameter *output*
  (merge-pathnames (format nil "build/window-test/~(~a-~a~)/"
                           slt/window-test::*frontend* slt/window-test::*api*)
                   (asdf:system-source-directory "slt")))
(ensure-directories-exist *output*)

(defparameter *script*
  (let ((path (merge-pathnames "shell.sh" *output*)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "#!/bin/sh~%printf '\\033[1mbold\\033[0m plain \\033[3mitalic\\033[0m \\033[4munderline\\033[0m\\n'~%printf '\\033[41m  red  \\033[0m \\033[32mgreen\\033[0m \\344\\270\\255\\346\\226\\207 e\\314\\201 \\342\\224\\200\\342\\224\\200\\n'~%printf '\\360\\237\\230\\200\\n'~%exec sleep 30~%"))
    (uiop:run-program (list "chmod" "+x" (uiop:native-namestring path)))
    (uiop:native-namestring path)))

(defvar *failures* '())

(defun check (ok description)
  (format t "~&~:[FAIL~;ok  ~] ~a~%" ok description)
  (unless ok (push description *failures*)))

(defun pixel (rgb width x y)
  (let ((i (* 3 (+ x (* y width)))))
    (logior (ash (aref rgb i) 16) (ash (aref rgb (+ i 1)) 8) (aref rgb (+ i 2)))))

(defconstant +rgb-channels+ 3)
(defconstant +channel-bits+ 8)

(when (eq slt/window-test::*frontend* :wayland)
  (load (merge-pathnames "wayland-input.lisp" *load-truename*)))

(defun channel-distance (a b)
  "The sum of the differences between the #xRRGGBB colors A and B's channels."
  (loop for channel below +rgb-channels+
        for field = (byte +channel-bits+ (* channel +channel-bits+))
        sum (abs (- (ldb field a) (ldb field b)))))

(defun inked-p (rgb)
  "True when RGB is nearer the foreground than the background: a glyph's
pixel at least half covered."
  (< (channel-distance rgb (palette-foreground *palette*))
     (channel-distance rgb (palette-background *palette*))))

(defun check-snapshot (frontend view)
  (let ((cw (frontend-view-cell-width frontend view))
        (ch (frontend-view-cell-height frontend view)))
    (multiple-value-bind (rgb width height) (frontend-snapshot frontend view)
      (write-ppm (merge-pathnames "snapshot.ppm" *output*) rgb width height)
      (format t "~&~a with ~a: window ~dx~d, cell ~dx~d~%"
              slt/window-test::*frontend* slt/window-test::*api* width height cw ch)
      (check (= (pixel rgb width (1- width) (1- height)) (palette-background *palette*))
             "the bottom-right corner is the background")
      (check (> (loop for y below ch
                      count (loop for x below (* 4 cw)
                                  thereis (/= (pixel rgb width x y)
                                              (palette-background *palette*))))
                (floor ch 3))
             "\"bold\" in the first row draws glyph pixels")
      (check (= (pixel rgb width (floor cw 2) (+ ch 2)) (aref (palette-colors *palette*) 1))
             "SGR 41 fills its cells with palette red")
      (check (= (pixel rgb width (+ cw (floor cw 2)) (+ (* 3 ch) (floor ch 2))) #xff0000)
             "RGBA terminal images are composited over the grid")
      ;; The line may fall between pixel rows and be antialiased over two.
      (check (find-if (lambda (y)
                        (inked-p (pixel rgb width (+ (* 21 cw) (floor cw 2)) y)))
                      (loop for y from ch below (* 2 ch) collect y))
             "box drawing characters reach the cell centre"))))

(setf *gl-api* slt/window-test::*api*)
(add-hook '*after-init-hook*
          (lambda (emulator)
            (let ((frontend (terminal-emulator-frontend emulator))
                  (view (terminal-emulator-view emulator))
                  (store (terminal-emulator-image-store emulator)))
              (store-image store 1 0 1 1
                           (make-array 4 :element-type '(unsigned-byte 8)
                                         :initial-contents '(255 0 0 255)))
              (push (make-terminal-image-placement :image-id 1 :row 3 :column 1
                                                   :columns 1 :rows 1 :z-index 1)
                    (terminal-image-store-placements store))
              (setf (terminal-image-store-dirty-p store) t)
              (frontend-schedule frontend view 1500
                                (lambda ()
                                  (render-emulator emulator :force t)
                                  (handler-case (check-snapshot frontend view)
                                    (error (condition)
                                      (check nil (format nil "snapshot: ~a" condition))))
                                  (when (fboundp 'check-wayland-input)
                                    (handler-case (check-wayland-input frontend view)
                                      (error (condition)
                                        (check nil (format nil "input: ~a" condition)))))
                                  (destroy-emulator emulator)
                                  (frontend-request-exit frontend view))))))

(launch-terminal :frontend slt/window-test::*frontend*
                 :engine (or (uiop:getenv "SLT_ENGINE") :lispvt)
                 :shell *script* :rows 10 :columns 50 :load-config nil)
(uiop:quit (if *failures* 1 0))
