(in-package #:iterm2-images)

;;;; iTerm2 inline images: OSC 1337 ; File=ARGUMENTS : BASE64 ST, or the
;;;; same file in parts, MultipartFile=ARGUMENTS, then FilePart=BASE64 as
;;;; often as needed, then FileEnd.  The ARGUMENTS are KEY=VALUE separated
;;;; by semicolons.  Only files with inline=1 are images to show; the
;;;; others are downloads, which are left to the caller (and ignored here).
;;;; Sizes follow WezTerm's reading of iTerm2's (wezterm/wezterm e68a8ec,
;;;; term/src/terminalstate/iterm.rs), and doNotMoveCursor=1 is WezTerm's.

(defparameter +osc-prefix+ "1337;"
  "What the OSC strings carrying files start with.")

(defparameter +file-commands+
  '(("File" . :file) ("MultipartFile" . :multipart-file)
    ("FilePart" . :file-part) ("FileEnd" . :file-end))
  "The OSC 1337 commands that send files, as written before = (or alone).")

(defun octets-text (octets start end)
  (map 'string #'code-char (subseq octets start end)))

(defun file-command (octets start end)
  "The file command OCTETS hold from START to END (after 1337;), and where
its argument begins; or NIL."
  (let* ((equals (position (char-code #\=) octets :start start :end end))
         (command (cdr (assoc (octets-text octets start (or equals end)) +file-commands+
                              :test #'string=))))
    (and command (values command (if equals (1+ equals) end)))))

(defun file-command-start (octets start end)
  "When the OSC payload in OCTETS from START to END is a file command,
where the command begins (after 1337;); otherwise NIL."
  (let ((after (+ start (length +osc-prefix+))))
    (and (<= after end)
         (loop for char across +osc-prefix+
               for index from start
               always (= (char-code char) (aref octets index)))
         (file-command octets after end)
         after)))

(defun parse-arguments (text)
  "The KEY=VALUE;... TEXT as an alist of strings."
  (loop for field in (uiop:split-string text :separator ";")
        for equals = (position #\= field)
        when equals
          collect (cons (subseq field 0 equals) (subseq field (1+ equals)))))

;;; Decoded images

(defparameter +automatic-size+ "auto")
(defparameter +pixel-suffix+ "px")
(defparameter +percent-suffix+ "%")
(defconstant +percent+ 100)
(defparameter +true+ "1")
(defparameter +false+ "0")

(defstruct (image (:constructor make-image (arguments frames width height &optional loops-p)))
  "An image file sent inline, decoded: its ARGUMENTS (an alist of strings),
FRAMES (more than one for an animation), size in pixels, and whether an
animation loops."
  arguments frames width height loops-p)

(defun image-argument (image key &optional default)
  "The argument KEY (\"width\", \"inline\", ...) IMAGE was sent with."
  (let ((entry (assoc key (image-arguments image) :test #'string=)))
    (if entry (cdr entry) default)))

(defun image-moves-cursor-p (image)
  "Whether the cursor moves past IMAGE once shown (not with doNotMoveCursor=1)."
  (string/= +true+ (image-argument image "doNotMoveCursor" +false+)))

(defun size-pixels (text cell-pixels screen-pixels)
  "The pixels the size TEXT (N cells, Npx, N% of the screen or auto) asks
for, or NIL for auto."
  (flet ((number-before (suffix)
           (and (uiop:string-suffix-p text suffix)
                (parse-integer text :end (- (length text) (length suffix)) :junk-allowed t))))
    (let ((pixels (number-before +pixel-suffix+))
          (percent (number-before +percent-suffix+))
          (cells (and (plusp (length text)) (every #'digit-char-p text)
                      (parse-integer text))))
      (cond (pixels pixels)
            (percent (floor (* percent screen-pixels) +percent+))
            (cells (* cells cell-pixels))))))

(defun fit-size (width height box-width box-height)
  "WIDTH x HEIGHT scaled to fit BOX-WIDTH x BOX-HEIGHT, keeping proportions."
  (let ((scale (min (/ box-width width) (/ box-height height))))
    (values (max 1 (round (* width scale))) (max 1 (round (* height scale))))))

(defun image-display-size (image &key cell-width cell-height screen-width screen-height)
  "The size in pixels to show IMAGE at, as its arguments ask, on a screen
of SCREEN-WIDTH x SCREEN-HEIGHT pixels with cells of CELL-WIDTH x
CELL-HEIGHT: (values WIDTH HEIGHT STRETCH-P), STRETCH-P when the image
should fill that size whatever its proportions."
  (let* ((width (image-width image))
         (height (image-height image))
         (wanted-width (size-pixels (image-argument image "width" +automatic-size+)
                                    cell-width screen-width))
         (wanted-height (size-pixels (image-argument image "height" +automatic-size+)
                                     cell-height screen-height))
         (preserve-p (string/= +false+ (image-argument image "preserveAspectRatio" +true+))))
    (cond
      ((and wanted-width wanted-height)
       (if preserve-p
           (multiple-value-call #'values (fit-size width height wanted-width wanted-height) nil)
           (values wanted-width wanted-height t)))
      (wanted-width (values wanted-width (max 1 (round (* wanted-width height) width)) nil))
      (wanted-height (values (max 1 (round (* wanted-height width) height)) wanted-height nil))
      ;; Its own size, shrunk to fit the screen.
      ((or (> width screen-width) (> height screen-height))
       (multiple-value-call #'values (fit-size width height screen-width screen-height) nil))
      (t (values width height nil)))))

;;; Receiving files

(defstruct (decoder (:constructor make-decoder (&key (max-encoded-bytes (* 64 1024 1024)))))
  "Receives file commands, keeping a file sent in parts until its end."
  (max-encoded-bytes (* 64 1024 1024) :type (integer 1 *))
  ;; A MultipartFile's arguments, its base64 parts so far (newest first)
  ;; and their length; or NIL.
  (arguments nil)
  (parts '() :type list)
  (length 0 :type (integer 0 *))
  (receiving-p nil))

(defun decoder-reset (decoder)
  "Forget a file being received in parts."
  (setf (decoder-arguments decoder) nil
        (decoder-parts decoder) '()
        (decoder-length decoder) 0
        (decoder-receiving-p decoder) nil)
  decoder)

(defun decode-file (arguments parts)
  "The IMAGE in the base64 PARTS, or NIL when ARGUMENTS do not ask for it to
be shown."
  (when (string= +true+ (cdr (or (assoc "inline" arguments :test #'string=)
                                 (cons nil +false+))))
    (multiple-value-call #'make-image
      arguments
      (decode-image-file (handler-case (simd-base64:decode parts)
                           (simd-base64:base64-error ()
                             (image-error "Invalid base64 image file")))))))

(defun decoder-feed (decoder octets start end)
  "Act on the file command in OCTETS from START to END (where
FILE-COMMAND-START says it begins): return the IMAGE a whole file makes, or
NIL.  Signal IMAGE-ERROR for a file that cannot be decoded.  OCTETS may be
reused once this returns."
  (multiple-value-bind (command argument-start) (file-command octets start end)
    (case command
      (:file
       (let ((colon (position (char-code #\:) octets :start argument-start :end end)))
         (and colon
              (decode-file (parse-arguments (octets-text octets argument-start colon))
                           (list (subseq octets (1+ colon) end))))))
      (:multipart-file
       (decoder-reset decoder)
       (setf (decoder-arguments decoder) (parse-arguments (octets-text octets argument-start end))
             (decoder-receiving-p decoder) t)
       nil)
      (:file-part
       (when (decoder-receiving-p decoder)
         (let ((length (+ (decoder-length decoder) (- end argument-start))))
           (if (> length (decoder-max-encoded-bytes decoder))
               (progn (decoder-reset decoder)
                      (image-error "Image file exceeds ~d octets"
                                   (decoder-max-encoded-bytes decoder)))
               (setf (decoder-parts decoder) (cons (subseq octets argument-start end)
                                                   (decoder-parts decoder))
                     (decoder-length decoder) length))))
       nil)
      (:file-end
       (when (decoder-receiving-p decoder)
         (let ((arguments (decoder-arguments decoder))
               (parts (reverse (decoder-parts decoder))))
           (decoder-reset decoder)
           (decode-file arguments parts)))))))
