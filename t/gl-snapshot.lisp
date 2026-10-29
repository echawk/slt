;;;; Smoke test for the gl frontend: run a shell that prints styled text in
;;;; a hidden GLFW window, render it offscreen and check the pixels.  Run
;;;; with scripts/gl-test; on Linux CI, under Xvfb or a headless Wayland
;;;; compositor.  SLT_GL_API=vulkan draws the window with Vulkan instead.
;;;; Leaves build/gl-test/API/snapshot.ppm for inspection.

(asdf:load-system "slt")
(asdf:load-system "slt/gl")
(defparameter cl-user::*api*
  (let ((api (uiop:getenv "SLT_GL_API")))
    (if (and api (plusp (length api))) (intern (string-upcase api) :keyword) :auto)))
(when (eq cl-user::*api* :vulkan)
  (asdf:load-system "slt/glfw-vulkan"))
(in-package #:slt)

(defparameter *output* (merge-pathnames (format nil "build/gl-test/~(~a~)/" cl-user::*api*)
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

(defun check-snapshot (view)
  (let* ((grid (gl-view-grid view))
         (cw (gl-view-cell-width view))
         (ch (gl-view-cell-height view)))
    (multiple-value-bind (rgb width height)
        (snapshot-gl-view view (merge-pathnames "snapshot.ppm" *output*))
      (format t "~&framebuffer ~dx~d, cell ~dx~d, grid ~dx~d, GLFW ~a, ~a~%"
              width height cw ch (grid-columns grid) (grid-rows grid)
              (%glfw:get-version-string)
              (if (typep (gl-view-renderer view) 'gl-renderer)
                  (format nil "GL ~a" (gl:get-string :version))
                  (type-of (gl-view-renderer view))))
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
             "box drawing characters reach the cell centre")
      ;; Row 3 is an emoji, drawn in its own colors: the face is yellow.
      (check (loop for y from (* 2 ch) below (* 3 ch)
                   thereis (loop for x below (* 2 cw)
                                 for p = (pixel rgb width x y)
                                 thereis (and (> (ldb (byte 8 16) p) 180)
                                              (> (ldb (byte 8 8) p) 120)
                                              (< (ldb (byte 8 0) p) 90))))
             "a color emoji is drawn in color"))))

(defun check-replaced-image (emulator view)
  "Put a blue image over the red one, as a video's next frame comes: it is
drawn, and it takes over the red one's texture."
  (let* ((store (terminal-emulator-image-store emulator))
         (renderer (gl-view-renderer view))
         (red (image-texture renderer 1)))
    (store-image store 2 0 1 1
                 (make-array 4 :element-type '(unsigned-byte 8)
                               :initial-contents '(0 0 255 255))
                 :opaque-p t)
    (push (make-terminal-image-placement :image-id 2 :row 3 :column 1
                                         :columns 1 :rows 1 :z-index 1)
          (terminal-image-store-placements store))
    (setf (terminal-image-store-dirty-p store) t)
    (render-emulator emulator :force t)
    (multiple-value-bind (rgb width) (snapshot-gl-view view (merge-pathnames "replaced.ppm" *output*))
      (let ((cw (gl-view-cell-width view)) (ch (gl-view-cell-height view)))
        (check (= (pixel rgb width (+ cw (floor cw 2)) (+ (* 3 ch) (floor ch 2))) #x0000ff)
               "an image over another is drawn in its place")))
    (check (and red
                (null (image-texture renderer 1))
                (eql red (image-texture renderer 2)))
           "the covered image's texture is reused for the new one")))

(defun image-texture (renderer id)
  "The texture RENDERER holds image ID's pixels in, or NIL."
  (second (gethash id (if (typep renderer 'gl-renderer)
                          (gl-renderer-image-textures renderer)
                          (uiop:symbol-call :slt :vulkan-renderer-image-textures renderer)))))

(defun palette-ansi-rgb (palette index)
  (aref (palette-colors palette) index))

(setf *gl-window-visible* nil
      *gl-api* cl-user::*api*)
(add-hook '*after-init-hook*
          (lambda (emulator)
            (let ((frontend (terminal-emulator-frontend emulator))
                  (view (terminal-emulator-view emulator)))
              (let ((store (terminal-emulator-image-store emulator)))
                (store-image store 1 0 1 1
                             (make-array 4 :element-type '(unsigned-byte 8)
                                           :initial-contents '(255 0 0 255)))
                (push (make-terminal-image-placement :image-id 1 :row 3 :column 1
                                                     :columns 1 :rows 1 :z-index 1)
                      (terminal-image-store-placements store))
                (setf (terminal-image-store-dirty-p store) t))
              (frontend-schedule frontend view 1500
                                (lambda ()
                                  (render-emulator emulator :force t)
                                  (handler-case (progn (check-snapshot view)
                                                       (check-replaced-image emulator view))
                                    (error (condition)
                                      (check nil (format nil "snapshot: ~a" condition))))
                                  (destroy-emulator emulator)
                                  (frontend-request-exit frontend view))))))

(launch-terminal :frontend :gl :engine (or (uiop:getenv "SLT_ENGINE") :lispvt)
                 :shell *script* :rows 10 :columns 50 :load-config nil)
(uiop:quit (if *failures* 1 0))
