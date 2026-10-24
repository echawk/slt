(defpackage #:cl-libsixel/tests
  (:use #:cl #:cl-libsixel)
  (:export #:run-tests))

(in-package #:cl-libsixel/tests)

(defun run-tests ()
  ;; Red, one pixel wide and one sixel band high. A missing system library is
  ;; not a Lisp binding failure, so allow packaging environments to skip it.
  (unless (libsixel-available-p)
    (format t "~&cl-libsixel: libsixel is unavailable; decoder test skipped.~%")
    (return-from run-tests t))
  (let ((image (decode-sixel "q#0;2;100;0;0#0~")))
    (assert (= 1 (sixel-image-width image)))
    (assert (= 6 (sixel-image-height image)))
    (assert (= 6 (length (sixel-image-pixels image))))
    (assert (= (* 4 (sixel-image-color-count image))
               (length (sixel-image-palette image))))
    (assert (equalp #(255 0 0 255) (subseq (sixel-image-palette image) 0 4)))
    (assert (= 24 (length (sixel-image-rgba image)))))
  t)
