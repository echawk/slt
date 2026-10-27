(uiop:define-package #:slt/bench
  (:use #:cl #:slt)
  (:export #:run-throughput #:run-image-stream))

(in-package #:slt/bench)

;;;; Engine throughput: how fast child output becomes an up-to-date grid.
;;;; Workloads model common output: plain scrolling text (cat of a log),
;;;; colorful text (ls --color, compiler diagnostics), and full-screen
;;;; redraws with cursor addressing (editors, htop).

(defun esc (control &rest arguments)
  (apply #'format nil (concatenate 'string "~c" control) (code-char 27) arguments))

(defun workload-plain (lines)
  (with-output-to-string (out)
    (dotimes (i lines)
      (format out "~6d The quick brown fox jumps over the lazy dog; 0123456789 abcdefghij~c~c"
              i #\Return #\Linefeed))))

(defun workload-color (lines)
  (with-output-to-string (out)
    (dotimes (i lines)
      (dotimes (j 8)
        (write-string (esc "[38;5;~dm" (mod (+ i j) 256)) out)
        (format out "word~d " j))
      (write-string (esc "[0m") out)
      (format out "~c~c" #\Return #\Linefeed))))

(defun workload-fullscreen (frames rows columns)
  (with-output-to-string (out)
    (dotimes (frame frames)
      (dotimes (row rows)
        (write-string (esc "[~d;1H" (1+ row)) out)
        (write-string (esc "[~d;~dm" (+ 30 (mod (+ row frame) 8)) (+ 40 (mod row 8))) out)
        (dotimes (column columns)
          (write-char (code-char (+ 33 (mod (+ row column frame) 90))) out)))
      (write-string (esc "[0m") out))))

(defun time-engine (engine-name octets &key (rows 50) (columns 160) (chunk 65536))
  "Feed OCTETS in CHUNK-sized reads, updating a grid after each read as the
emulator does.  Return megabytes per second."
  (let ((engine (make-engine engine-name :rows rows :columns columns))
        (grid (make-grid rows columns)))
    (unwind-protect
         (let ((start (get-internal-real-time)))
           (loop for offset from 0 below (length octets) by chunk
                 do (engine-feed engine octets :start offset
                                               :end (min (length octets) (+ offset chunk)))
                    (engine-update-grid engine grid)
                    (grid-clear-dirty grid))
           (let ((seconds (max 1d-6 (/ (- (get-internal-real-time) start)
                                       internal-time-units-per-second))))
             (/ (length octets) seconds 1d6)))
      (engine-close engine))))

(defun run-throughput (&key (engines (available-engines)) (stream *standard-output*))
  (let ((workloads (list (cons "plain" (slt::string->octets (workload-plain 40000)))
                         (cons "color" (slt::string->octets (workload-color 20000)))
                         (cons "fullscreen" (slt::string->octets
                                             (workload-fullscreen 200 50 160))))))
    (format stream "~&~12a~{~12@a~}~%" "MB/s" (mapcar #'string-downcase engines))
    (dolist (workload workloads)
      (format stream "~12a" (car workload))
      (dolist (engine engines)
        (time-engine engine (subseq (cdr workload) 0 (min 65536 (length (cdr workload)))))
        (format stream "~12,2f" (time-engine engine (cdr workload))))
      (format stream "   (~,1f MB)~%" (/ (length (cdr workload)) 1d6)))))
