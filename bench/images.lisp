(in-package #:slt/bench)

;;;; Image throughput: video played as Kitty graphics, as `mpv --vo=kitty`
;;;; sends it.  Each frame moves the cursor home and transmits a new RGB
;;;; image (no id, so a fresh one each frame) in 4096-octet base64 chunks,
;;;; displayed over the last.  Frames go through the emulator as child
;;;; output does -- the interceptor, decoding, the image store -- and each
;;;; is followed by a render, which snapshots the images for the frontend.

(defun video-frame (width height seed)
  "One frame of mpv's Kitty output: WIDTH x HEIGHT RGB pixels from SEED."
  (let* ((rgb (make-array (* 3 width height) :element-type '(unsigned-byte 8)))
         (esc (code-char 27)))
    (dotimes (i (length rgb))
      (setf (aref rgb i) (logand #xff (+ seed (* i 7)))))
    (let ((base64 (cl-base64:usb8-array-to-base64-string rgb)))
      (slt::string->octets
       (with-output-to-string (out)
         (format out "~c[1;1H~c_Ga=T,f=24,s=~d,v=~d,C=1,q=2,m=1;" esc esc width height)
         (loop for start from 0 below (length base64) by 4096
               for end = (min (length base64) (+ start 4096))
               do (when (plusp start)
                    (format out "~c_Gm=~d;" esc (if (< end (length base64)) 1 0)))
                  (write-string base64 out :start start :end end)
                  (format out "~c\\" esc)))))))

(defun make-bench-emulator (engine rows columns)
  (let* ((frontend (make-instance 'slt::headless-frontend :cell-width 8 :cell-height 16))
         (view (slt::frontend-create-view frontend :rows rows :columns columns)))
    (slt::%make-terminal-emulator :engine (make-engine engine :rows rows :columns columns)
                                  :grid (make-grid rows columns)
                                  :frontend frontend :view view
                                  :rows rows :columns columns :poll-interval 16)))

(defun run-image-stream (&key (engine :lispvt) (frames 200) (width 1280) (height 720)
                              (stream *standard-output*))
  "Play FRAMES video frames of WIDTH x HEIGHT through an emulator; report
frames per second, the time spent feeding and rendering, garbage
collection, and how many images are left to draw."
  (let* ((emulator (make-bench-emulator engine 50 160))
         (data (loop for seed below 4 collect (video-frame width height seed)))
         (feed 0)
         (render 0)
         (milliseconds (/ internal-time-units-per-second 1000d0)))
    (sb-ext:gc :full t)
    (let ((gc-start sb-ext:*gc-run-time*)
          (consed-start (sb-ext:get-bytes-consed))
          (start (get-internal-real-time)))
      (dotimes (frame frames)
        (let ((octets (nth (mod frame 4) data))
              (before (get-internal-real-time)))
          ;; In pieces the size of the reader's buffer.
          (loop for offset from 0 below (length octets) by 65536
                do (bt:with-recursive-lock-held ((slt::terminal-emulator-lock emulator))
                     (slt::feed-emulator-output emulator octets
                                                :start offset
                                                :end (min (length octets) (+ offset 65536)))))
          (let ((fed (get-internal-real-time)))
            (render-emulator emulator)
            (incf feed (- fed before))
            (incf render (- (get-internal-real-time) fed)))))
      (let* ((seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second 1d0))
             (display (slt::headless-view-image-store (slt::terminal-emulator-view emulator))))
        (format stream "~&~d frames of ~dx~d: ~,1f frames/s; per frame ~,2f ms feeding, ~
                        ~,2f ms rendering, ~,1f MB consed~%"
                frames width height (/ frames seconds)
                (/ feed frames milliseconds) (/ render frames milliseconds)
                (/ (- (sb-ext:get-bytes-consed) consed-start) frames 1d6))
        (format stream "GC ~,1f% of the time; ~d images stored, ~d to draw~%"
                (* 100 (/ (- sb-ext:*gc-run-time* gc-start) internal-time-units-per-second seconds))
                (hash-table-count (terminal-image-store-images
                                   (slt::terminal-emulator-image-store emulator)))
                (length (terminal-image-render-list display 8 16)))))
    (engine-close (slt::terminal-emulator-engine emulator))))
