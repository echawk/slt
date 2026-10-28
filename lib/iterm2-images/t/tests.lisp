(defpackage #:iterm2-images/tests
  (:use #:cl #:iterm2-images)
  (:export #:run-tests))

(in-package #:iterm2-images/tests)

(defvar *failures* 0)

(defmacro is (form)
  `(unless ,form
     (incf *failures*)
     (format t "~&FAIL: ~s~%" ',form)))

(defun octets (string)
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code string))

(defun gif (width height colors &key (delay 10))
  "A GIF of WIDTH x HEIGHT pixels with a frame in each of COLORS, (R G B)."
  (let* ((table (skippy:make-color-table))
         (stream (skippy:make-data-stream :width width :height height :color-table table
                                          :loopingp t)))
    (dolist (color colors)
      (skippy:add-image (skippy:make-image
                         :width width :height height :delay-time delay
                         :image-data (skippy:make-image-data
                                      width height
                                      :initial-element (skippy:ensure-color
                                                        (apply #'skippy:rgb-color color) table)))
                        stream))
    (flexi-streams:with-output-to-sequence (out)
      (skippy:write-data-stream stream out))))

(defun jpeg (width height)
  "A green JPEG (cl-jpeg's encoder takes pixels as B G R)."
  (let ((pixels (make-array (* 3 width height) :element-type '(unsigned-byte 8))))
    (dotimes (pixel (* width height))
      (setf (aref pixels (1+ (* 3 pixel))) 255))
    (flexi-streams:with-output-to-sequence (out)
      (jpeg:encode-image-stream out pixels 3 height width))))

(defun osc (text)
  "TEXT as the payload of an OSC string."
  (octets (format nil "1337;~a" text)))

(defun feed (decoder text)
  (let* ((payload (osc text))
         (start (file-command-start payload 0 (length payload))))
    (and start (decoder-feed decoder payload start (length payload)))))

(defun base64 (octets)
  (cl-base64:usb8-array-to-base64-string octets))

(defun test-commands ()
  (let ((payload (osc "File=inline=1:AAAA")))
    (is (= 5 (file-command-start payload 0 (length payload)))))
  (dolist (text '("SetMark" "1337" "Files=x" ""))
    (let ((payload (octets text)))
      (is (null (file-command-start payload 0 (length payload)))))))

(defun test-files ()
  (let ((decoder (make-decoder)))
    (is (null (feed decoder (format nil "File=inline=0:~a" (base64 (gif 2 2 '((1 2 3))))))))
    (let ((image (feed decoder (format nil "File=name=eA==;inline=1:~a"
                                       (base64 (gif 4 2 '((255 0 0))))))))
      (is (equal '(4 2) (list (image-width image) (image-height image))))
      (is (= 1 (length (image-frames image))))
      (is (equalp #(255 0 0 255) (subseq (frame-rgba (first (image-frames image))) 0 4)))
      (is (image-moves-cursor-p image)))
    (let* ((image (feed decoder (format nil "File=inline=1:~a" (base64 (jpeg 16 8)))))
           (pixel (subseq (frame-rgba (first (image-frames image))) 0 4)))
      (is (and (< (aref pixel 0) 8) (> (aref pixel 1) 247) (< (aref pixel 2) 8))))
    (is (handler-case (progn (feed decoder "File=inline=1:!!!!") nil)
          (image-error () t)))
    (is (handler-case (progn (feed decoder (format nil "File=inline=1:~a"
                                                   (base64 (octets "not an image"))))
                             nil)
          (image-error () t)))))

(defun test-parts ()
  (let* ((decoder (make-decoder))
         (data (base64 (gif 2 2 '((255 0 0) (0 0 255)) :delay 5)))
         (half (floor (length data) 2)))
    (is (null (feed decoder "MultipartFile=inline=1;doNotMoveCursor=1")))
    (is (null (feed decoder (format nil "FilePart=~a" (subseq data 0 half)))))
    (is (null (feed decoder (format nil "FilePart=~a" (subseq data half)))))
    (let ((image (feed decoder "FileEnd")))
      (is (= 2 (length (image-frames image))))
      (is (= 50 (frame-delay-ms (first (image-frames image)))))
      (is (image-loops-p image))
      (is (not (image-moves-cursor-p image))))
    (is (null (feed decoder "FileEnd")))
    ;; Too large: the parts are dropped.
    (let ((small (make-decoder :max-encoded-bytes 4)))
      (feed small "MultipartFile=inline=1")
      (is (handler-case (progn (feed small "FilePart=AAAAAAAA") nil)
            (image-error () t)))
      (is (null (feed small "FileEnd"))))))

(defun test-sizes ()
  (flet ((size (arguments width height)
           (multiple-value-list
            (image-display-size (make-image arguments '() width height)
                                :cell-width 10 :cell-height 20
                                :screen-width 800 :screen-height 400))))
    (is (equal '(40 20 nil) (size '() 40 20)))
    (is (equal '(800 400 nil) (size '() 1600 800)))
    (is (equal '(30 15 nil) (size '(("width" . "3")) 40 20)))
    (is (equal '(80 40 nil) (size '(("height" . "40px")) 40 20)))
    (is (equal '(400 200 nil) (size '(("width" . "50%")) 40 20)))
    (is (equal '(50 25 nil) (size '(("width" . "5") ("height" . "5")) 40 20)))
    (is (equal '(50 100 t) (size '(("width" . "5") ("height" . "5")
                                   ("preserveAspectRatio" . "0"))
                                 40 20)))))

(defun run-tests ()
  (let ((*failures* 0))
    (test-commands)
    (test-files)
    (test-parts)
    (test-sizes)
    (format t "~&iterm2-images: ~:[all tests passed~;~:*~d failure~:p~]~%"
            (and (plusp *failures*) *failures*))
    (zerop *failures*)))
