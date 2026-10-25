;;;; Smoke test for the clx frontend: run a shell that prints styled text,
;;;; read the window's pixels back from the X server and check them.  Run
;;;; with scripts/clx-test; on Linux CI, under Xvfb (24-bit depth).  Leaves
;;;; build/clx-test/snapshot.ppm for inspection.

(asdf:load-system "slt")
(asdf:load-system "slt/clx")
(in-package #:slt)

(defparameter *output* (merge-pathnames "build/clx-test/" (asdf:system-source-directory "slt")))
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

(defun snapshot-clx-view (view pathname)
  "Read VIEW's window back; return its pixels as a (simple-array
(unsigned-byte 32) (* *)) of #xRRGGBB, and write a PPM."
  (let* ((grid (clx-view-grid view))
         (width (* (grid-columns grid) (clx-view-cell-width view)))
         (height (* (grid-rows grid) (clx-view-cell-height view)))
         (image (xlib:get-image (clx-view-window view) :x 0 :y 0
                                :width width :height height :format :z-pixmap))
         (data (xlib:image-z-pixarray image))
         (pixels (make-array (list height width) :element-type '(unsigned-byte 32))))
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref pixels y x) (logand #xFFFFFF (aref data y x)))))
    (with-open-file (out pathname :direction :output :if-exists :supersede
                                  :element-type '(unsigned-byte 8))
      (write-sequence (sb-ext:string-to-octets (format nil "P6~%~d ~d~%255~%" width height)) out)
      (dotimes (y height)
        (dotimes (x width)
          (let ((p (aref pixels y x)))
            (write-byte (ldb (byte 8 16) p) out)
            (write-byte (ldb (byte 8 8) p) out)
            (write-byte (ldb (byte 8 0) p) out)))))
    pixels))

(defun check-snapshot (view)
  (let* ((cw (clx-view-cell-width view))
         (ch (clx-view-cell-height view))
         (pixels (snapshot-clx-view view (merge-pathnames "snapshot.ppm" *output*)))
         (height (array-dimension pixels 0))
         (width (array-dimension pixels 1))
         (background (palette-background *palette*)))
    (format t "~&image ~dx~d, cell ~dx~d, unicode fonts ~:[no~;yes~]~%"
            width height cw ch (clx-view-unicode-p view))
    (check (= (aref pixels (1- height) (1- width)) background)
           "the bottom-right corner is the background")
    (check (> (loop for y below ch
                    count (loop for x below (* 4 cw)
                                thereis (/= (aref pixels y x) background)))
              (floor ch 3))
           "\"bold\" in the first row draws glyph pixels")
    ;; Row 2 starts with seven cells of red background.
    (check (= (aref pixels (+ ch 2) (floor cw 2))
              (aref (palette-colors *palette*) 1))
           "SGR 41 fills its cells with palette red")
    (check (find-if (lambda (y)
                      (= (aref pixels y (+ (* 21 cw) (floor cw 2)))
                         (palette-foreground *palette*)))
                    (loop for y from ch below (* 2 ch) collect y))
           "box drawing characters reach the cell centre")
    ;; Column 19, after the spacers of two wide characters, holds an e.
    (check (loop for y from ch below (* 2 ch)
                 thereis (loop for x from (* 19 cw) below (* 20 cw)
                               thereis (/= (aref pixels y x) background)))
           "ASCII after a wide character is drawn")))

(add-hook '*after-init-hook*
          (lambda (emulator)
            (let ((frontend (terminal-emulator-frontend emulator))
                  (view (terminal-emulator-view emulator)))
              (frontend-schedule frontend view 1500
                                (lambda ()
                                  (render-emulator emulator :force t)
                                  (handler-case (check-snapshot view)
                                    (error (condition)
                                      (check nil (format nil "snapshot: ~a" condition))))
                                  (destroy-emulator emulator)
                                  (frontend-request-exit frontend view))))))

(launch-terminal :frontend :clx :engine (or (uiop:getenv "SLT_ENGINE") :3bst)
                 :shell *script* :rows 10 :columns 50 :load-config nil)
(uiop:quit (if *failures* 1 0))
