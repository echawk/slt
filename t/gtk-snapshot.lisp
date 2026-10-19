;;;; Smoke test for the gtk backend: run a shell that prints styled text,
;;;; draw the window's contents into an offscreen Cairo surface through the
;;;; backend's draw function and check the pixels.  Run with
;;;; scripts/gtk-test; on Linux CI, under Xvfb.  Leaves
;;;; build/gtk-test/snapshot.ppm for inspection.

(asdf:load-system "slt")
(asdf:load-system "slt/gtk")
(in-package #:slt)

(defparameter *output* (merge-pathnames "build/gtk-test/" (asdf:system-source-directory "slt")))
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

(defun snapshot-gtk-view (view pathname)
  "Draw VIEW into an RGB24 image surface; return its pixels as a
(simple-array (unsigned-byte 32) (* *)) of #xRRGGBB, and write a PPM."
  (let* ((grid (gtk-view-grid view))
         (width (* (grid-columns grid) (gtk-view-cell-width view)))
         (height (* (grid-rows grid) (gtk-view-cell-height view)))
         (surface (cairo:cairo-image-surface-create :rgb24 width height))
         (cr (cairo:cairo-create surface))
         (pixels (make-array (list height width) :element-type '(unsigned-byte 32))))
    (gtk-draw view nil cr)
    (cairo:cairo-surface-flush surface)
    (let ((data (cairo:cairo-image-surface-get-data surface))
          (stride (cairo:cairo-image-surface-get-stride surface)))
      (dotimes (y height)
        (dotimes (x width)
          (setf (aref pixels y x)
                (logand #xFFFFFF (cffi:mem-ref data :uint32 (+ (* y stride) (* 4 x))))))))
    (cairo:cairo-destroy cr)
    (cairo:cairo-surface-destroy surface)
    (with-open-file (out pathname :direction :output :if-exists :supersede
                                  :element-type '(unsigned-byte 8))
      (write-sequence (babel:string-to-octets (format nil "P6~%~d ~d~%255~%" width height)) out)
      (dotimes (y height)
        (dotimes (x width)
          (let ((p (aref pixels y x)))
            (write-byte (ldb (byte 8 16) p) out)
            (write-byte (ldb (byte 8 8) p) out)
            (write-byte (ldb (byte 8 0) p) out)))))
    pixels))

(defun check-snapshot (view)
  (let* ((cw (gtk-view-cell-width view))
         (ch (gtk-view-cell-height view))
         (pixels (snapshot-gtk-view view (merge-pathnames "snapshot.ppm" *output*)))
         (height (array-dimension pixels 0))
         (width (array-dimension pixels 1))
         (background (palette-background *palette*)))
    (format t "~&surface ~dx~d, cell ~dx~d, GTK ~d.~d~%" width height cw ch
            (gtk:gtk-get-major-version) (gtk:gtk-get-minor-version))
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
           "box drawing characters reach the cell centre")))

(add-hook '*after-init-hook*
          (lambda (emulator)
            (let ((backend (terminal-emulator-backend emulator))
                  (view (terminal-emulator-view emulator)))
              (backend-schedule backend view 1500
                                (lambda ()
                                  (render-emulator emulator :force t)
                                  (handler-case (check-snapshot view)
                                    (error (condition)
                                      (check nil (format nil "snapshot: ~a" condition))))
                                  (destroy-emulator emulator)
                                  (backend-request-exit backend view))))))

(launch-terminal :backend :gtk :engine (or (uiop:getenv "SLT_ENGINE") :3bst)
                 :shell *script* :rows 10 :columns 50 :load-config nil)
(uiop:quit (if *failures* 1 0))
