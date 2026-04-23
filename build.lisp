(in-package #:slt)

(defun default-executable-name (&optional (operating-system (uiop:operating-system)))
  (case operating-system
    ((:windows :win32 :mswindows) "slt.exe")
    (otherwise "slt")))

(defun default-executable-output (&optional
                                    (root (uiop:ensure-directory-pathname
                                           (truename ".")))
                                    (operating-system (uiop:operating-system)))
  (merge-pathnames (format nil "build/~a"
                           (default-executable-name operating-system))
                   (uiop:ensure-directory-pathname root)))

(defun normalize-executable-output (output &key
                                             (root (uiop:ensure-directory-pathname
                                                    (truename ".")))
                                             (operating-system
                                               (uiop:operating-system)))
  (let ((pathname (cond
                    ((null output)
                     (default-executable-output root operating-system))
                    ((pathnamep output)
                     (merge-pathnames output root))
                    (t
                     (merge-pathnames (pathname output) root)))))
    (if (uiop:directory-pathname-p pathname)
        (merge-pathnames (default-executable-name operating-system)
                         (uiop:ensure-directory-pathname pathname))
        pathname)))

(defun dump-executable (&key output (compression 9))
  (let ((pathname (normalize-executable-output output)))
    (ensure-directories-exist pathname)
    (setf uiop:*image-entry-point* #'main)
    (uiop:dump-image (namestring pathname)
                     :executable t
                     :compression compression)))
