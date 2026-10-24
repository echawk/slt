(in-package #:cl-libsixel)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defstruct sixel-image
  (width 0 :type (integer 0 #.most-positive-fixnum))
  (height 0 :type (integer 0 #.most-positive-fixnum))
  ;; One palette index per pixel.
  (pixels #() :type (simple-array (unsigned-byte 8) (*)))
  ;; Four bytes per color; the wrapper expands libsixel's RGB888 palette.
  (palette #() :type (simple-array (unsigned-byte 8) (*)))
  (color-count 0 :type (integer 0 256)))

(defun ensure-octets (data)
  (etypecase data
    ((simple-array (unsigned-byte 8) (*)) data)
    ((array (unsigned-byte 8) (*))
     (coerce data '(simple-array (unsigned-byte 8) (*))))
    (string
     (let ((result (make-array (length data) :element-type '(unsigned-byte 8))))
       (loop for char across data
             for index from 0
             do (setf (aref result index) (char-code char)))
       result))))

(defun copy-foreign-octets (pointer length)
  (let ((result (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (index length result)
      (setf (aref result index) (cffi:mem-aref pointer :uint8 index)))))

(defun sixel-dcs-p (data)
  (or (and (>= (length data) 2)
           (= (aref data 0) 27)
           (= (aref data 1) (char-code #\P)))
      (and (plusp (length data)) (= (aref data 0) #x90))))

(defun ensure-sixel-dcs (data)
  "libsixel expects the DCS envelope. Add it when DATA is only its payload."
  (if (sixel-dcs-p data)
      data
      (let* ((has-final-q (and (plusp (length data))
                               (= (aref data 0) (char-code #\q))))
             (prefix (if has-final-q 2 3))
             (result (make-array (+ prefix (length data) 2)
                                 :element-type '(unsigned-byte 8))))
        (setf (aref result 0) 27
              (aref result 1) (char-code #\P))
        (unless has-final-q
          (setf (aref result 2) (char-code #\q)))
        (replace result data :start1 prefix)
        (setf (aref result (+ prefix (length data))) 27
              (aref result (+ prefix (length data) 1)) (char-code #\\))
        result)))

(defun rgb-palette-to-rgba (pointer colors)
  (let ((result (make-array (* colors 4) :element-type '(unsigned-byte 8))))
    (dotimes (color colors result)
      (let ((source (* color 3))
            (target (* color 4)))
        (setf (aref result target) (cffi:mem-aref pointer :uint8 source)
              (aref result (+ target 1)) (cffi:mem-aref pointer :uint8 (+ source 1))
              (aref result (+ target 2)) (cffi:mem-aref pointer :uint8 (+ source 2))
              (aref result (+ target 3)) 255)))))

(defun decode-sixel (data &key (max-pixels (* 4096 4096)))
  "Decode a SIXEL payload or complete DCS sequence with libsixel.

Return a SIXEL-IMAGE containing palette indices and an RGBA palette. DATA may
be an octet vector or an ASCII string. MAX-PIXELS limits the accepted decoded
image size; it defaults to 4096 by 4096."
  (load-libsixel)
  (let ((data (ensure-sixel-dcs (ensure-octets data))))
    (cffi:with-foreign-objects ((allocator-out :pointer)
                                (pixels-out :pointer)
                                (palette-out :pointer)
                                (width-out :int)
                                (height-out :int)
                                (colors-out :int))
      (setf (cffi:mem-ref allocator-out :pointer) (cffi:null-pointer)
            (cffi:mem-ref pixels-out :pointer) (cffi:null-pointer)
            (cffi:mem-ref palette-out :pointer) (cffi:null-pointer))
      (check-status
       (%allocator-new allocator-out
                       (cffi:null-pointer) (cffi:null-pointer)
                       (cffi:null-pointer) (cffi:null-pointer)))
      (let ((allocator (cffi:mem-ref allocator-out :pointer)))
        (unwind-protect
             (cffi:with-pointer-to-vector-data (input data)
               (check-status
                (%decode-raw input (length data) pixels-out width-out height-out
                             palette-out colors-out allocator))
               (let* ((width (cffi:mem-ref width-out :int))
                      (height (cffi:mem-ref height-out :int))
                      (colors (cffi:mem-ref colors-out :int))
                      (pixel-count (* width height)))
                 (unless (and (plusp width) (plusp height)
                              (<= pixel-count max-pixels)
                              (<= 0 colors 256))
                   (error "Refusing decoded SIXEL image ~dx~d with ~d colors"
                          width height colors))
                 (make-sixel-image
                  :width width
                  :height height
                  :pixels (copy-foreign-octets
                           (cffi:mem-ref pixels-out :pointer) pixel-count)
                  :palette (rgb-palette-to-rgba
                            (cffi:mem-ref palette-out :pointer) colors)
                  :color-count colors)))
          (let ((pixels (cffi:mem-ref pixels-out :pointer))
                (palette (cffi:mem-ref palette-out :pointer)))
            (unless (cffi:null-pointer-p pixels)
              (%allocator-free allocator pixels))
            (unless (cffi:null-pointer-p palette)
              (%allocator-free allocator palette)))
          (%allocator-unref allocator))))))

(defun sixel-image-rgba (image &key (background :transparent))
  "Expand IMAGE's indexed pixels into an RGBA octet vector.

libsixel leaves pixels the image never paints at an index past the palette.
BACKGROUND decides their color: :TRANSPARENT (the default, as for P2=1), or
a palette index, which SIXEL's opaque background modes use (register 0)."
  (let* ((pixels (sixel-image-pixels image))
         (palette (sixel-image-palette image))
         (colors (sixel-image-color-count image))
         (fill (and (integerp background) (< -1 background colors) background))
         (result (make-array (* 4 (length pixels))
                             :element-type '(unsigned-byte 8))))
    (dotimes (index (length pixels) result)
      (let ((color (aref pixels index))
            (target (* 4 index)))
        (cond
          ((< color colors)
           (replace result palette :start1 target :end1 (+ target 4)
                                   :start2 (* 4 color)))
          (fill
           (replace result palette :start1 target :end1 (+ target 4)
                                   :start2 (* 4 fill))))))))
