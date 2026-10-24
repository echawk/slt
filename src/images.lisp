(in-package #:slt)

;;;; Engine-independent terminal graphics. The stream parser consumes only
;;;; SIXEL DCS and Kitty APC strings; every other byte reaches the selected VT
;;;; engine unchanged and in its original order.

(defvar *sixel-images* t)
(defvar *kitty-graphics-images* t)
(defvar *terminal-image-storage-limit* (* 320 1024 1024))

(defstruct terminal-image
  (id 0 :type (unsigned-byte 32))
  (number 0 :type (unsigned-byte 32))
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (rgba #() :type (simple-array (unsigned-byte 8) (*)))
  (access 0 :type integer))

(defstruct terminal-image-placement
  (image-id 0 :type (unsigned-byte 32))
  (placement-id 0 :type (unsigned-byte 32))
  (row 0 :type integer)
  (column 0 :type integer)
  columns rows
  (source-x 0 :type integer)
  (source-y 0 :type integer)
  source-width source-height
  (x-offset 0 :type integer)
  (y-offset 0 :type integer)
  (z-index 0 :type integer)
  (virtual-p nil))

(defstruct (terminal-image-store (:constructor make-terminal-image-store))
  (images (make-hash-table) :type hash-table)
  (placements '() :type list)
  (numbers (make-hash-table) :type hash-table)
  (next-id 1 :type (integer 1 #xffffffff))
  (clock 0 :type integer)
  (bytes 0 :type integer)
  (limit *terminal-image-storage-limit* :type integer)
  (dirty-p nil)
  (kitty-decoder (kitty-graphics:make-graphics-decoder)))

(defun image-store-id (store requested-id number)
  (cond
    ((plusp requested-id) requested-id)
    ((plusp number)
     (prog1 (terminal-image-store-next-id store)
       (setf (terminal-image-store-next-id store)
             (if (= (terminal-image-store-next-id store) #xffffffff)
                 1
                 (1+ (terminal-image-store-next-id store))))))
    (t
     (prog1 (terminal-image-store-next-id store)
       (setf (terminal-image-store-next-id store)
             (if (= (terminal-image-store-next-id store) #xffffffff)
                 1
                 (1+ (terminal-image-store-next-id store))))))))

(defun remove-image (store id)
  (let ((image (gethash id (terminal-image-store-images store))))
    (when image
      (decf (terminal-image-store-bytes store) (length (terminal-image-rgba image)))
      (remhash id (terminal-image-store-images store))
      (setf (terminal-image-store-placements store)
            (delete id (terminal-image-store-placements store)
                    :key #'terminal-image-placement-image-id))
      (when (and (plusp (terminal-image-number image))
                 (= id (gethash (terminal-image-number image)
                                (terminal-image-store-numbers store) 0)))
        (remhash (terminal-image-number image) (terminal-image-store-numbers store)))
      (setf (terminal-image-store-dirty-p store) t))))

(defun evict-images-to-fit (store bytes)
  (loop while (> (+ bytes (terminal-image-store-bytes store))
                 (terminal-image-store-limit store))
        for victim = (loop for image being the hash-values
                             of (terminal-image-store-images store)
                           minimize (terminal-image-access image) into oldest
                           finally (return
                                     (find oldest
                                           (loop for value being the hash-values
                                                   of (terminal-image-store-images store)
                                                 collect value)
                                           :key #'terminal-image-access)))
        do (if victim
               (remove-image store (terminal-image-id victim))
               (error "Terminal image exceeds the storage quota"))))

(defun store-image (store id number width height rgba)
  (remove-image store id)
  (evict-images-to-fit store (length rgba))
  (let ((image (make-terminal-image :id id :number number :width width :height height
                                    :rgba rgba :access (incf (terminal-image-store-clock store)))))
    (setf (gethash id (terminal-image-store-images store)) image)
    (when (plusp number)
      (setf (gethash number (terminal-image-store-numbers store)) id))
    (incf (terminal-image-store-bytes store) (length rgba))
    (setf (terminal-image-store-dirty-p store) t)
    image))

(defun place-image (store id row column command)
  (let* ((image (or (gethash id (terminal-image-store-images store))
                    (error "Image ~d does not exist" id)))
         (placement-id (kitty-graphics:graphics-command-integer command #\p 0))
         (placement (make-terminal-image-placement
                     :image-id id :placement-id placement-id :row row :column column
                     :columns (let ((n (kitty-graphics:graphics-command-integer command #\c 0)))
                                (and (plusp n) n))
                     :rows (let ((n (kitty-graphics:graphics-command-integer command #\r 0)))
                             (and (plusp n) n))
                     :source-x (kitty-graphics:graphics-command-integer command #\x 0)
                     :source-y (kitty-graphics:graphics-command-integer command #\y 0)
                     :source-width (let ((n (kitty-graphics:graphics-command-integer command #\w 0)))
                                     (and (plusp n) n))
                     :source-height (let ((n (kitty-graphics:graphics-command-integer command #\h 0)))
                                      (and (plusp n) n))
                     :x-offset (kitty-graphics:graphics-command-integer command #\X 0)
                     :y-offset (kitty-graphics:graphics-command-integer command #\Y 0)
                     :z-index (kitty-graphics:graphics-command-integer command #\z 0)
                     :virtual-p (= 1 (kitty-graphics:graphics-command-integer command #\U 0)))))
    (unless (and (< -1 (terminal-image-placement-source-x placement)
                    (terminal-image-width image))
                 (< -1 (terminal-image-placement-source-y placement)
                    (terminal-image-height image))
                 (or (null (terminal-image-placement-source-width placement))
                     (<= (+ (terminal-image-placement-source-x placement)
                            (terminal-image-placement-source-width placement))
                         (terminal-image-width image)))
                 (or (null (terminal-image-placement-source-height placement))
                     (<= (+ (terminal-image-placement-source-y placement)
                            (terminal-image-placement-source-height placement))
                         (terminal-image-height image))))
      (error "Image source rectangle is outside the image"))
    (when (plusp placement-id)
      (setf (terminal-image-store-placements store)
            (delete-if (lambda (old)
                         (and (= id (terminal-image-placement-image-id old))
                              (= placement-id
                                 (terminal-image-placement-placement-id old))))
                       (terminal-image-store-placements store))))
    (push placement (terminal-image-store-placements store))
    (setf (terminal-image-access image) (incf (terminal-image-store-clock store)))
    (setf (terminal-image-store-dirty-p store) t)
    placement))

(defun placement-cell-size (placement image cell-width cell-height)
  "Return a placement's width and height in cells."
  (let ((source-width (or (terminal-image-placement-source-width placement)
                          (- (terminal-image-width image)
                             (terminal-image-placement-source-x placement))))
        (source-height (or (terminal-image-placement-source-height placement)
                           (- (terminal-image-height image)
                              (terminal-image-placement-source-y placement)))))
    (values (or (terminal-image-placement-columns placement)
                (ceiling (max 1 source-width) cell-width))
            (or (terminal-image-placement-rows placement)
                (ceiling (max 1 source-height) cell-height)))))

(defun terminal-image-render-list (store cell-width cell-height)
  "Return visible (PLACEMENT IMAGE X Y WIDTH HEIGHT) records in z order."
  (sort
   (loop for placement in (terminal-image-store-placements store)
         for image = (gethash (terminal-image-placement-image-id placement)
                              (terminal-image-store-images store))
         when (and image (not (terminal-image-placement-virtual-p placement)))
           collect (multiple-value-bind (columns rows)
                       (placement-cell-size placement image cell-width cell-height)
                     (list placement image
                           (+ (* (terminal-image-placement-column placement) cell-width)
                              (terminal-image-placement-x-offset placement))
                           (+ (* (terminal-image-placement-row placement) cell-height)
                              (terminal-image-placement-y-offset placement))
                           (* columns cell-width) (* rows cell-height))))
   #'< :key (lambda (record)
              (terminal-image-placement-z-index (first record)))))

(defun kitty-command-image-id (store command)
  (let ((id (kitty-graphics:graphics-command-integer command #\i 0))
        (number (kitty-graphics:graphics-command-integer command #\I 0)))
    (values (if (plusp id) id (gethash number (terminal-image-store-numbers store) 0))
            number)))

(defun kitty-response (command status &optional message id)
  (let ((quiet (kitty-graphics:graphics-command-integer command #\q 0)))
    (unless (or (and (string= status "OK") (= quiet 1))
                (and (not (string= status "OK")) (= quiet 2)))
      (let ((controls '()))
        (when (plusp (or id 0)) (push (cons #\i (princ-to-string id)) controls))
        (let ((number (kitty-graphics:graphics-command-integer command #\I 0))
              (placement (kitty-graphics:graphics-command-integer command #\p 0)))
          (when (plusp number) (push (cons #\I (princ-to-string number)) controls))
          (when (plusp placement) (push (cons #\p (princ-to-string placement)) controls)))
        (kitty-graphics:encode-graphics-response (nreverse controls) status message)))))

(defun resolve-kitty-image (store command)
  (multiple-value-bind (existing-id number) (kitty-command-image-id store command)
    (let ((requested-id (kitty-graphics:graphics-command-integer command #\i 0)))
      (cond
        ((plusp (length (kitty-graphics:graphics-command-data command)))
         (multiple-value-bind (rgba width height)
             (kitty-graphics:graphics-command-rgba command)
           (let ((id (image-store-id store requested-id number)))
             (values (store-image store id number width height rgba) id))))
        ((plusp existing-id)
         (let ((image (gethash existing-id (terminal-image-store-images store))))
           (values image existing-id)))
        (t (values nil (or requested-id existing-id)))))))

(defun delete-kitty-images (store command row column &optional (cell-width 1) (cell-height 1))
  (let ((selector (char (kitty-graphics:graphics-command-value command #\d "a") 0)))
    (labels ((intersects (placement target-row target-column)
               (let ((image (gethash (terminal-image-placement-image-id placement)
                                     (terminal-image-store-images store))))
                 (and image
                      (multiple-value-bind (columns rows)
                          (placement-cell-size placement image cell-width cell-height)
                        (and (<= (terminal-image-placement-row placement) target-row)
                             (< target-row (+ (terminal-image-placement-row placement) rows))
                             (<= (terminal-image-placement-column placement) target-column)
                             (< target-column
                                (+ (terminal-image-placement-column placement) columns)))))))
             (delete-placements (predicate free-p)
               (let ((ids '()))
                 (setf (terminal-image-store-placements store)
                       (delete-if (lambda (placement)
                                    (when (funcall predicate placement)
                                      (pushnew (terminal-image-placement-image-id placement) ids)
                                      t))
                                  (terminal-image-store-placements store)))
                 (when free-p
                   (dolist (id ids)
                     (unless (find id (terminal-image-store-placements store)
                                   :key #'terminal-image-placement-image-id)
                       (remove-image store id)))))))
      (case (char-downcase selector)
        (#\a
         (delete-placements (constantly t) nil)
         (when (upper-case-p selector)
           (let ((ids (loop for id being the hash-keys
                              of (terminal-image-store-images store) collect id)))
             (mapc (lambda (id) (remove-image store id)) ids))))
        (#\i
         (multiple-value-bind (id number) (kitty-command-image-id store command)
           (declare (ignore number))
           (let ((pid (kitty-graphics:graphics-command-integer command #\p 0)))
             (delete-placements
              (lambda (placement)
                (and (= id (terminal-image-placement-image-id placement))
                     (or (zerop pid)
                         (= pid (terminal-image-placement-placement-id placement)))))
              nil)
             (when (and (upper-case-p selector) (plusp id))
               (remove-image store id)))))
        (#\n
         (let* ((number (kitty-graphics:graphics-command-integer command #\I 0))
                (id (gethash number (terminal-image-store-numbers store) 0)))
           (delete-placements
            (lambda (placement) (= id (terminal-image-placement-image-id placement)))
            nil)
           (when (and (upper-case-p selector) (plusp id))
             (remove-image store id))))
        (#\c (delete-placements (lambda (p) (intersects p row column))
                                 (upper-case-p selector)))
        (#\p
         (let ((target-row (1- (kitty-graphics:graphics-command-integer command #\y 1)))
               (target-column (1- (kitty-graphics:graphics-command-integer command #\x 1))))
           (delete-placements (lambda (p) (intersects p target-row target-column))
                              (upper-case-p selector))))
        (#\q
         (let ((target-row (1- (kitty-graphics:graphics-command-integer command #\y 1)))
               (target-column (1- (kitty-graphics:graphics-command-integer command #\x 1)))
               (z (kitty-graphics:graphics-command-integer command #\z 0)))
           (delete-placements
            (lambda (p) (and (intersects p target-row target-column)
                             (= z (terminal-image-placement-z-index p))))
            (upper-case-p selector))))
        (#\r (delete-placements
              (lambda (p)
                (let ((image (gethash (terminal-image-placement-image-id p)
                                      (terminal-image-store-images store))))
                  (and image
                       (multiple-value-bind (columns rows)
                           (placement-cell-size p image cell-width cell-height)
                         (declare (ignore columns))
                         (<= (terminal-image-placement-row p) row
                             (1- (+ (terminal-image-placement-row p) rows)))))))
              (upper-case-p selector)))
        (#\x
         (let ((target-column (1- (kitty-graphics:graphics-command-integer command #\x 1))))
           (delete-placements
            (lambda (p)
              (let ((image (gethash (terminal-image-placement-image-id p)
                                    (terminal-image-store-images store))))
                (and image
                     (multiple-value-bind (columns rows)
                         (placement-cell-size p image cell-width cell-height)
                       (declare (ignore rows))
                       (<= (terminal-image-placement-column p) target-column
                           (1- (+ (terminal-image-placement-column p) columns)))))))
            (upper-case-p selector))))
        (#\y
         (let ((target-row (1- (kitty-graphics:graphics-command-integer command #\y 1))))
           (delete-placements
            (lambda (p)
              (let ((image (gethash (terminal-image-placement-image-id p)
                                    (terminal-image-store-images store))))
                (and image
                     (multiple-value-bind (columns rows)
                         (placement-cell-size p image cell-width cell-height)
                       (declare (ignore columns))
                       (<= (terminal-image-placement-row p) target-row
                           (1- (+ (terminal-image-placement-row p) rows)))))))
            (upper-case-p selector))))
        (#\z
         (let ((z (kitty-graphics:graphics-command-integer command #\z 0)))
           (delete-placements (lambda (p) (= z (terminal-image-placement-z-index p)))
                              (upper-case-p selector))))
        (t nil))
      (setf (terminal-image-store-dirty-p store) t))))

(defun handle-kitty-command (store payload engine)
  (handler-case
      (let ((command (kitty-graphics:graphics-decoder-feed
                      (terminal-image-store-kitty-decoder store) payload)))
        (when command
          (multiple-value-bind (row column) (engine-cursor-position engine)
            (case (kitty-graphics:graphics-command-action command)
              (#\d
               (delete-kitty-images store command row column)
               nil)
              ((#\t #\T)
               (multiple-value-bind (image id) (resolve-kitty-image store command)
                 (if image
                     (progn
                       (when (char= #\T (kitty-graphics:graphics-command-action command))
                         (place-image store id row column command))
                       (kitty-response command "OK" nil id))
                     (kitty-response command "ENOENT" "image not found" id))))
              (#\q
               ;; Queries test decodability but must not consume storage or
               ;; create a placement.
               (handler-case
                   (progn (kitty-graphics:graphics-command-rgba command)
                          (kitty-response command "OK" nil
                                          (kitty-graphics:graphics-command-integer
                                           command #\i 0)))
                 (error (condition)
                   (kitty-response command "EINVAL" (princ-to-string condition)))))
              (#\p
               (multiple-value-bind (id number) (kitty-command-image-id store command)
                 (declare (ignore number))
                 (if (gethash id (terminal-image-store-images store))
                     (progn (place-image store id row column command)
                            (kitty-response command "OK" nil id))
                     (kitty-response command "ENOENT" "image not found" id))))
              (t (kitty-response command "ENOTSUP" "action not implemented"))))))
    (error (condition)
      (kitty-graphics:encode-graphics-response '() "EINVAL" (princ-to-string condition)))))

(defun handle-sixel-command (store payload engine)
  (handler-case
      (let ((image (cl-libsixel:decode-sixel payload)))
        (multiple-value-bind (row column) (engine-cursor-position engine)
          (let* ((id (image-store-id store 0 0))
                 (stored (store-image store id 0
                                      (cl-libsixel:sixel-image-width image)
                                      (cl-libsixel:sixel-image-height image)
                                      (cl-libsixel:sixel-image-rgba image))))
            (declare (ignore stored))
            (push (make-terminal-image-placement :image-id id :row row :column column)
                  (terminal-image-store-placements store))
            (setf (terminal-image-store-dirty-p store) t))))
    (error (condition)
      (warn "Could not decode SIXEL image: ~a" condition))))

(defstruct (terminal-graphics-parser
            (:constructor make-terminal-graphics-parser
                (&key (max-control-bytes (* 70 1024 1024)))))
  (state :ground)
  kind
  (max-control-bytes (* 70 1024 1024) :type (integer 1024 *))
  (candidate (make-array 256 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0)))

(defun parser-reset-candidate (parser &rest bytes)
  (setf (fill-pointer (terminal-graphics-parser-candidate parser)) 0)
  (dolist (byte bytes)
    (vector-push-extend byte (terminal-graphics-parser-candidate parser))))

(defun finish-control-string (parser output feed-normal on-sixel on-kitty)
  (flet ((flush ()
           (when (plusp (fill-pointer output))
             (funcall feed-normal output 0 (fill-pointer output))
             (setf (fill-pointer output) 0))))
    (case (terminal-graphics-parser-kind parser)
      (:sixel (flush) (funcall on-sixel (copy-seq (terminal-graphics-parser-candidate parser))))
      (:kitty
       (flush)
       (let* ((bytes (terminal-graphics-parser-candidate parser))
              (c1p (= (aref bytes 0) #x9f))
              (start (if c1p 1 2))
              (end (- (length bytes) (if (= (aref bytes (1- (length bytes))) #x9c) 1 2))))
         (funcall on-kitty (subseq bytes start end))))
      (t
       (loop for byte across (terminal-graphics-parser-candidate parser)
             do (vector-push-extend byte output))))
    (setf (terminal-graphics-parser-state parser) :ground
          (terminal-graphics-parser-kind parser) nil
          (fill-pointer (terminal-graphics-parser-candidate parser)) 0)))

(defun terminal-graphics-feed (parser octets feed-normal on-sixel on-kitty
                               &key (start 0) (end (length octets)))
  "Demultiplex graphics control strings, calling FEED-NORMAL with other bytes."
  (let ((output (make-array (- end start) :element-type '(unsigned-byte 8)
                                          :adjustable t :fill-pointer 0)))
    (labels ((candidate (byte)
               (vector-push-extend byte (terminal-graphics-parser-candidate parser))
               (when (and (member (terminal-graphics-parser-kind parser) '(:sixel :kitty))
                          (> (fill-pointer (terminal-graphics-parser-candidate parser))
                             (terminal-graphics-parser-max-control-bytes parser)))
                 (setf (fill-pointer (terminal-graphics-parser-candidate parser)) 0
                       (terminal-graphics-parser-kind parser) nil
                       (terminal-graphics-parser-state parser) :discard-string)))
             (begin (state kind &rest bytes)
               (apply #'parser-reset-candidate parser bytes)
               (setf (terminal-graphics-parser-state parser) state
                     (terminal-graphics-parser-kind parser) kind))
             (finish ()
               (finish-control-string parser output feed-normal on-sixel on-kitty)))
      (loop for index from start below end
            for byte = (aref octets index)
            do (case (terminal-graphics-parser-state parser)
                 (:ground
                  (cond ((= byte 27) (setf (terminal-graphics-parser-state parser) :escape))
                        ((= byte #x90) (begin :dcs-header nil byte))
                        ((= byte #x9f) (begin :apc-start nil byte))
                        (t (vector-push-extend byte output))))
                 (:escape
                  (cond ((= byte (char-code #\P)) (begin :dcs-header nil 27 byte))
                        ((= byte (char-code #\_)) (begin :apc-start nil 27 byte))
                        ((= byte 27) (vector-push-extend 27 output))
                        (t (vector-push-extend 27 output)
                           (vector-push-extend byte output)
                           (setf (terminal-graphics-parser-state parser) :ground))))
                 (:dcs-header
                  (candidate byte)
                  (when (<= #x40 byte #x7e)
                    (setf (terminal-graphics-parser-kind parser)
                          (if (and *sixel-images* (= byte (char-code #\q))) :sixel :generic)
                          (terminal-graphics-parser-state parser) :string)))
                 (:apc-start
                  (candidate byte)
                  (setf (terminal-graphics-parser-kind parser)
                        (if (and *kitty-graphics-images* (= byte (char-code #\G)))
                            :kitty :generic)
                        (terminal-graphics-parser-state parser) :string))
                 (:string
                  (cond ((member byte '(#x18 #x1a))
                         (setf (terminal-graphics-parser-state parser) :ground
                               (fill-pointer (terminal-graphics-parser-candidate parser)) 0))
                        ((= byte 27) (candidate byte)
                         (setf (terminal-graphics-parser-state parser) :string-escape))
                        ((= byte #x9c) (candidate byte) (finish))
                        (t (candidate byte))))
                 (:string-escape
                  (candidate byte)
                  (if (= byte (char-code #\\))
                      (finish)
                      (setf (terminal-graphics-parser-state parser) :string)))
                 (:discard-string
                  (cond ((member byte '(#x18 #x1a #x9c))
                         (setf (terminal-graphics-parser-state parser) :ground))
                        ((= byte 27)
                         (setf (terminal-graphics-parser-state parser) :discard-escape))))
                 (:discard-escape
                  (setf (terminal-graphics-parser-state parser)
                        (if (= byte (char-code #\\)) :ground :discard-string)))))
      (when (plusp (fill-pointer output))
        (funcall feed-normal output 0 (fill-pointer output))))
    parser))
