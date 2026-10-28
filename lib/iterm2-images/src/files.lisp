(in-package #:iterm2-images)

;;;; Image files sent whole, as iTerm2's inline images are: PNG (through
;;;; kitty-graphics' libpng binding), JPEG (cl-jpeg, which decodes baseline
;;;; but not progressive JPEG) and GIF (skippy), decoded into RGBA frames.
;;;; The format is told by the file's signature.

(define-condition image-error (error)
  ((message :initarg :message :reader image-error-message))
  (:report (lambda (condition stream)
             (write-string (image-error-message condition) stream))))

(defun image-error (control &rest arguments)
  (error 'image-error :message (apply #'format nil control arguments)))

(defparameter *max-image-octets* (* 320 1024 1024)
  "The largest image, in RGBA octets, decoded.")

(defparameter +image-file-signatures+
  `((:png . #(#x89 ,(char-code #\P) ,(char-code #\N) ,(char-code #\G)))
    (:jpeg . #(#xff #xd8 #xff))
    (:gif . ,(map 'vector #'char-code "GIF8")))
  "The octets each format's files start with.")

(defun image-file-format (octets)
  "The format of the image file OCTETS (:PNG, :JPEG or :GIF), or NIL."
  (car (find-if (lambda (signature)
                  (and (>= (length octets) (length signature))
                       (every #'= signature octets)))
                +image-file-signatures+
                :key #'cdr)))

(defconstant +rgb-octets+ 3)
(defconstant +rgba-octets+ 4)
(defconstant +alpha-offset+ +rgb-octets+ "Where a pixel's alpha is among its RGBA octets.")
(defconstant +opaque+ #xff)

(defstruct (frame (:constructor make-frame (rgba delay-ms)))
  "One picture of an image file, as RGBA octets."
  (rgba nil :type (simple-array (unsigned-byte 8) (*)))
  ;; How long the frame shows in an animation; NIL for a still image.
  (delay-ms nil))

(defun check-image-size (width height)
  (unless (and (plusp width) (plusp height)
               (<= (* width height +rgba-octets+) *max-image-octets*))
    (image-error "Refusing image dimensions ~dx~d" width height)))

(defun decode-png-file (octets)
  (multiple-value-bind (rgba width height)
      (handler-case (kitty-graphics:decode-png octets)
        (kitty-graphics:graphics-protocol-error (condition)
          (image-error "~a" condition)))
    (values (list (make-frame rgba nil)) width height)))

;;; cl-jpeg gives the components of each pixel in reverse: B G R.
(defconstant +jpeg-gray-components+ 1)
(defconstant +jpeg-color-components+ 3)
(defconstant +jpeg-cmyk-components+ 4)

(defun decode-jpeg-file (octets)
  (multiple-value-bind (buffer height width components)
      (handler-case (jpeg:decode-stream (flexi-streams:make-in-memory-input-stream octets))
        (error (condition) (image-error "Invalid JPEG image: ~a" condition)))
    (check-image-size width height)
    (when (= components +jpeg-cmyk-components+)
      (setf buffer (jpeg:convert-cmyk-to-rgb buffer height width)
            components +jpeg-color-components+))
    (let ((rgba (make-array (* +rgba-octets+ width height) :element-type '(unsigned-byte 8))))
      (dotimes (pixel (* width height))
        (let ((source (* pixel components))
              (target (* pixel +rgba-octets+)))
          (dotimes (channel +rgb-octets+)
            (setf (aref rgba (+ target channel))
                  (aref buffer (if (= components +jpeg-gray-components+)
                                   source
                                   (+ source (- +rgb-octets+ 1 channel))))))
          (setf (aref rgba (+ target +alpha-offset+)) +opaque+)))
      (values (list (make-frame rgba nil)) width height))))

(defparameter +gif-centiseconds-ms+ 10
  "GIF delays count hundredths of a second.")

(defparameter +gif-minimum-delay-ms+ 20
  "Shorter GIF delays, zero included, are shown as +GIF-DEFAULT-DELAY-MS+,
as browsers do: many GIFs rely on it.")

(defparameter +gif-default-delay-ms+ 100)

(defun decode-gif-file (octets)
  "The frames of the GIF OCTETS, each composed over the frames before it
as the images' disposal methods say."
  (let* ((stream (handler-case
                     (skippy:read-data-stream (flexi-streams:make-in-memory-input-stream octets))
                   (error (condition) (image-error "Invalid GIF image: ~a" condition))))
         (width (skippy:width stream))
         (height (skippy:height stream))
         (canvas (progn (check-image-size width height)
                        (make-array (* +rgba-octets+ width height)
                                    :element-type '(unsigned-byte 8) :initial-element 0)))
         (frames '()))
    (loop for image across (skippy:images stream)
          for table = (or (skippy:color-table image) (skippy:color-table stream))
          for transparent = (skippy:transparency-index image)
          for left = (skippy:left-position image)
          for top = (skippy:top-position image)
          for image-width = (skippy:width image)
          for image-height = (skippy:height image)
          for data = (skippy:image-data image)
          for previous = (and (eq (skippy:disposal-method image) :restore-previous)
                              (copy-seq canvas))
          do (unless table
               (image-error "GIF image without a color table"))
             (dotimes (row image-height)
               (dotimes (column image-width)
                 (let ((index (aref data (+ column (* row image-width))))
                       (x (+ left column))
                       (y (+ top row)))
                   (when (and (< x width) (< y height) (not (eql index transparent)))
                     (let ((target (* +rgba-octets+ (+ x (* y width)))))
                       (multiple-value-bind (red green blue)
                           (skippy:color-rgb (skippy:color-table-entry table index))
                         (replace canvas (list red green blue +opaque+) :start1 target)))))))
             (let ((delay (* +gif-centiseconds-ms+ (skippy:delay-time image))))
               (push (make-frame (copy-seq canvas)
                                            (if (< delay +gif-minimum-delay-ms+)
                                                +gif-default-delay-ms+
                                                delay))
                     frames))
             (case (skippy:disposal-method image)
               (:restore-background
                (dotimes (row image-height)
                  (let ((y (+ top row)))
                    (when (< y height)
                      (fill canvas 0 :start (* +rgba-octets+ (+ left (* y width)))
                                     :end (* +rgba-octets+ (+ (min width (+ left image-width))
                                                              (* y width))))))))
               (:restore-previous (setf canvas previous))))
    (when (null frames)
      (image-error "GIF without images"))
    (setf frames (nreverse frames))
    ;; A single image is still.
    (when (null (rest frames))
      (setf (frame-delay-ms (first frames)) nil))
    (values frames width height (skippy:loopingp stream))))

(defun decode-image-file (octets)
  "Decode the image file OCTETS.  Return its frames (FRAMEs),
width and height, and for an animation whether it loops.  Signal
IMAGE-FILE-ERROR for an unknown format or a malformed file."
  (case (image-file-format octets)
    (:png (decode-png-file octets))
    (:jpeg (decode-jpeg-file octets))
    (:gif (decode-gif-file octets))
    (t (image-error "Unknown image format"))))
