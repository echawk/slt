;;;; Smoke test for the gl backend: run a shell that prints styled text in
;;;; a hidden GLFW window, render it offscreen and check the pixels.  Run
;;;; with scripts/gl-test; on Linux CI, under Xvfb or a headless Wayland
;;;; compositor.  Leaves build/gl-test/snapshot.ppm for inspection.

(asdf:load-system "slt")
(asdf:load-system "slt/gl")
(in-package #:slt)

(defparameter *output* (merge-pathnames "build/gl-test/" (asdf:system-source-directory "slt")))
(ensure-directories-exist *output*)

(defparameter *script*
  (let ((path (merge-pathnames "shell.sh" *output*)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "#!/bin/sh~%printf '\\033[1mbold\\033[0m plain \\033[3mitalic\\033[0m \\033[4munderline\\033[0m\\n'~%printf '\\033[41m  red  \\033[0m \\033[32mgreen\\033[0m \\344\\270\\255\\346\\226\\207 e\\314\\201 \\342\\224\\200\\342\\224\\200\\n'~%exec sleep 30~%"))
    (uiop:run-program (list "chmod" "+x" (uiop:native-namestring path)))
    (uiop:native-namestring path)))

(defvar *failures* '())

(defun check (ok description)
  (format t "~&~:[FAIL~;ok  ~] ~a~%" ok description)
  (unless ok (push description *failures*)))

(defun pixel (rgb width x y)
  (let ((i (* 3 (+ x (* y width)))))
    (logior (ash (aref rgb i) 16) (ash (aref rgb (+ i 1)) 8) (aref rgb (+ i 2)))))

(defun check-snapshot (view)
  (let* ((grid (gl-view-grid view))
         (cw (gl-view-cell-width view))
         (ch (gl-view-cell-height view)))
    (multiple-value-bind (rgb width height)
        (snapshot-gl-view view (merge-pathnames "snapshot.ppm" *output*))
      (format t "~&framebuffer ~dx~d, cell ~dx~d, grid ~dx~d, platform ~a, GL ~a~%"
              width height cw ch (grid-columns grid) (grid-rows grid)
              (glfw:platform) (gl:get-string :version))
      (check (= (pixel rgb width (1- width) (1- height)) (palette-background *palette*))
             "the bottom-right corner is the background")
      (check (> (loop for y below ch
                      count (loop for x below (* 4 cw)
                                  thereis (/= (pixel rgb width x y)
                                              (palette-background *palette*))))
                (floor ch 3))
             "\"bold\" in the first row draws glyph pixels")
      ;; Row 2 starts with seven cells of red background.
      (check (= (pixel rgb width (floor cw 2) (+ ch 2))
                (palette-ansi-rgb *palette* 1))
             "SGR 41 fills its cells with palette red")
      (check (= (pixel rgb width (+ cw (floor cw 2)) (+ (* 3 ch) (floor ch 2)))
                #xff0000)
             "RGBA terminal images are composited over the grid")
      (check (find-if (lambda (y)
                        (= (pixel rgb width (+ (* 21 cw) (floor cw 2)) y)
                           (palette-foreground *palette*)))
                      (loop for y from ch below (* 2 ch) collect y))
             "box drawing characters reach the cell centre"))))

(defun palette-ansi-rgb (palette index)
  (aref (palette-colors palette) index))

(setf *gl-window-visible* nil)
(add-hook '*after-init-hook*
          (lambda (emulator)
            (let ((backend (terminal-emulator-backend emulator))
                  (view (terminal-emulator-view emulator)))
              (let ((store (terminal-emulator-image-store emulator)))
                (store-image store 1 0 1 1
                             (make-array 4 :element-type '(unsigned-byte 8)
                                           :initial-contents '(255 0 0 255)))
                (push (make-terminal-image-placement :image-id 1 :row 3 :column 1
                                                     :columns 1 :rows 1 :z-index 1)
                      (terminal-image-store-placements store))
                (setf (terminal-image-store-dirty-p store) t))
              (backend-schedule backend view 1500
                                (lambda ()
                                  (render-emulator emulator :force t)
                                  (handler-case (check-snapshot view)
                                    (error (condition)
                                      (check nil (format nil "snapshot: ~a" condition))))
                                  (destroy-emulator emulator)
                                  (backend-request-exit backend view))))))

(launch-terminal :backend :gl :engine (or (uiop:getenv "SLT_ENGINE") :3bst)
                 :shell *script* :rows 10 :columns 50 :load-config nil)
(uiop:quit (if *failures* 1 0))
