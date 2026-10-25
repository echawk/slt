(in-package #:terminal-osc)

;;;; OSC payloads.  An OSC string is ESC ] NUMBER ; PAYLOAD, terminated by
;;;; ST (ESC \) or BEL; replies use the same terminator as the request.

(defun parse-osc (octets &key (start 0) (end (length octets)))
  "Split an OSC string's payload (OCTETS from START to END, after ESC ])
into its number and the text after the semicolon, decoded as UTF-8 with
replacement.  The number is NIL when the string does not start with one."
  (let* ((separator (or (position 59 octets :start start :end end) end))
         (number (and (< start separator)
                      (loop for index from start below separator
                            always (<= 48 (aref octets index) 57))
                      (< (- separator start) 9)
                      (parse-integer (map 'string #'code-char
                                          (subseq octets start separator)))))
         (text (if (< separator end)
                   (babel:octets-to-string octets :start (1+ separator) :end end
                                                  :encoding :utf-8 :errorp nil)
                   "")))
    (values number text)))

(defun encode-osc (number payload &optional (terminator :st))
  "ESC ] NUMBER ; PAYLOAD and TERMINATOR (:ST or :BEL)."
  (format nil "~c]~d;~a~a" (code-char 27) number payload
          (if (eq terminator :bel)
              (string (code-char 7))
              (format nil "~c\\" (code-char 27)))))

(defun split (string char)
  (loop with start = 0
        for position = (position char string :start start)
        collect (subseq string start position)
        while position
        do (setf start (1+ position))))

;;; Colors

(defun parse-decimal (string)
  "STRING, digits with at most one point, as a rational."
  (let* ((point (position #\. string))
         (whole (subseq string 0 (or point (length string))))
         (fraction (if point (subseq string (1+ point)) "")))
    (+ (if (plusp (length whole)) (parse-integer whole) 0)
       (if (plusp (length fraction))
           (/ (parse-integer fraction) (expt 10 (length fraction)))
           0))))

(defun hex-channel (digits)
  "An XParseColor hex field of 1-4 digits, scaled to 0-255."
  (let ((length (length digits)))
    (when (and (<= 1 length 4) (every (lambda (c) (digit-char-p c 16)) digits))
      (let ((value (parse-integer digits :radix 16))
            (maximum (1- (expt 16 length))))
        (round (* 255 value) maximum)))))

(defun parse-color-spec (spec)
  "An X11 color specification as (VALUES R G B) in 0-255, or NIL.
Accepts rgb:R/G/B (1-4 hex digits each), #RGB #RRGGBB #RRRGGGBBB
#RRRRGGGGBBBB, and rgbi:R/G/B with intensities from 0 to 1."
  (let ((spec (string-trim " " spec)))
    (cond
      ((and (> (length spec) 4) (string-equal "rgb:" spec :end2 4))
       (let ((fields (split (subseq spec 4) #\/)))
         (when (= 3 (length fields))
           (let ((channels (mapcar #'hex-channel fields)))
             (when (every #'identity channels)
               (values-list channels))))))
      ((and (> (length spec) 5) (string-equal "rgbi:" spec :end2 5))
       (let ((fields (split (subseq spec 5) #\/)))
         (when (= 3 (length fields))
           (let ((channels (mapcar (lambda (field)
                                     (let ((value (and (plusp (length field))
                                                       (every (lambda (c) (or (digit-char-p c)
                                                                              (char= c #\.)))
                                                              field)
                                                       (<= (count #\. field) 1)
                                                       (parse-decimal field))))
                                       (and (realp value) (<= 0 value 1)
                                            (round (* 255 value)))))
                                   fields)))
             (when (every #'identity channels)
               (values-list channels))))))
      ((and (plusp (length spec)) (char= #\# (char spec 0))
            (member (1- (length spec)) '(3 6 9 12)))
       (let ((width (/ (1- (length spec)) 3)))
         (let ((channels (loop for i below 3
                               collect (let ((digits (subseq spec (+ 1 (* i width))
                                                             (+ 1 (* (1+ i) width)))))
                                         ;; #RGB means the high bits, as in X11.
                                         (and (every (lambda (c) (digit-char-p c 16)) digits)
                                              (ash (parse-integer digits :radix 16)
                                                   (- 8 (* 4 width))))))))
           (when (every #'identity channels)
             (values-list (mapcar (lambda (c) (min 255 c)) channels)))))))))

(defun format-color-spec (red green blue)
  "rgb:RRRR/GGGG/BBBB, as xterm reports colors."
  (string-downcase
   (format nil "rgb:~4,'0x/~4,'0x/~4,'0x" (* red 257) (* green 257) (* blue 257))))

(defun parse-palette-request (text)
  "OSC 4 ; INDEX ; SPEC ; INDEX ; SPEC ...: a list of (INDEX . SPEC), where
SPEC is \"?\" for a query."
  (loop for (index spec) on (split text #\;) by #'cddr
        for number = (ignore-errors (parse-integer index))
        when (and number spec (<= 0 number 255))
          collect (cons number spec)))

(defun parse-dynamic-color-request (number text)
  "OSC 10-19 ; SPEC ; SPEC ...: each SPEC applies to the next number, so
OSC 10 ; fg ; bg sets both.  A list of (NUMBER . SPEC)."
  (loop for spec in (split text #\;)
        for n from number to 19
        collect (cons n spec)))

(defun parse-palette-reset (text)
  "OSC 104 ; INDEX ; ...: the indices to reset, or :ALL."
  (let ((indices (loop for field in (split text #\;)
                       for number = (ignore-errors (parse-integer field))
                       when (and number (<= 0 number 255)) collect number)))
    (or indices :all)))

;;; Clipboard

(defun parse-clipboard-request (text)
  "OSC 52 ; SELECTIONS ; DATA: (VALUES SELECTIONS DATA) where SELECTIONS is
a string of c p q s 0-7 (\"s 0\" when empty, as in xterm) and DATA the
decoded text, :QUERY for ?, or :CLEAR when empty.  NIL if malformed."
  (let* ((separator (position #\; text))
         (selections (if separator (subseq text 0 separator) ""))
         (data (if separator (subseq text (1+ separator)) nil)))
    (when (and data (every (lambda (c) (find c "cpqs01234567")) selections))
      (values (if (string= selections "") "s0" selections)
              (cond ((string= data "?") :query)
                    ((string= data "") :clear)
                    (t (let ((octets (ignore-errors
                                      (cl-base64:base64-string-to-usb8-array
                                       (remove-if-not
                                        (lambda (c) (or (alphanumericp c) (find c "+/=")))
                                        data)))))
                         (and octets
                              (babel:octets-to-string octets :encoding :utf-8
                                                             :errorp nil)))))))))

(defun encode-clipboard-reply (selections text &optional (terminator :st))
  "The reply to an OSC 52 query: TEXT, base64 encoded."
  (encode-osc 52 (format nil "~a;~a" selections
                         (cl-base64:usb8-array-to-base64-string
                          (babel:string-to-octets text :encoding :utf-8)))
              terminator))

;;; Hyperlinks

(defun parse-hyperlink (text)
  "OSC 8 ; PARAMS ; URI: (VALUES URI ID) where URI is NIL to end the link
and ID the id parameter, if any.  PARAMS are colon-separated key=value
pairs."
  (let ((separator (position #\; text)))
    (when separator
      (let* ((params (subseq text 0 separator))
             (uri (subseq text (1+ separator)))
             (id (loop for pair in (split params #\:)
                       for equals = (position #\= pair)
                       when (and equals (string= "id" (subseq pair 0 equals)))
                         return (subseq pair (1+ equals)))))
        (values (if (string= uri "") nil uri) id)))))

;;; Working directory

(defun percent-decode (string)
  (let ((octets (make-array (length string) :element-type '(unsigned-byte 8)
                                            :fill-pointer 0)))
    (loop with i = 0
          while (< i (length string))
          do (let ((char (char string i)))
               (if (and (char= char #\%) (< (+ i 2) (length string))
                        (digit-char-p (char string (+ i 1)) 16)
                        (digit-char-p (char string (+ i 2)) 16))
                   (progn (vector-push (parse-integer string :start (1+ i) :end (+ i 3)
                                                             :radix 16)
                                       octets)
                          (incf i 3))
                   (progn (loop for byte across (babel:string-to-octets (string char)
                                                                        :encoding :utf-8)
                                do (vector-push-extend byte octets))
                          (incf i)))))
    (babel:octets-to-string (coerce octets '(simple-array (unsigned-byte 8) (*)))
                            :encoding :utf-8 :errorp nil)))

(defun parse-working-directory (text)
  "OSC 7 ; file://HOST/PATH (or kitty-shell-cwd://HOST/PATH): (VALUES PATH
HOST), PATH percent-decoded, or NIL."
  (let ((scheme-end (search "://" text)))
    (when (and scheme-end
               (member (subseq text 0 scheme-end) '("file" "kitty-shell-cwd")
                       :test #'string-equal))
      (let* ((rest (subseq text (+ scheme-end 3)))
             (slash (position #\/ rest)))
        (when slash
          (values (percent-decode (subseq rest slash)) (subseq rest 0 slash)))))))

;;; Shell integration

(defun parse-semantic-prompt (text)
  "OSC 133 ; MARK [; ARGUMENTS]: (VALUES KIND EXIT-STATUS OPTIONS) where KIND
is :PROMPT-START (A), :PROMPT-END (B, input starts), :COMMAND-START (C,
output starts), :COMMAND-END (D) or :PROMPT-CONTINUATION (P), EXIT-STATUS
the integer argument of D, and OPTIONS an alist of key=value arguments."
  (let* ((fields (split text #\;))
         (kind (cdr (assoc (first fields)
                           '(("A" . :prompt-start) ("B" . :prompt-end)
                             ("C" . :command-start) ("D" . :command-end)
                             ("P" . :prompt-continuation) ("N" . :prompt-start)
                             ("L" . :fresh-line))
                           :test #'string=))))
    (when kind
      (let ((status nil) (options '()))
        (dolist (field (rest fields))
          (let ((equals (position #\= field)))
            (if equals
                (push (cons (subseq field 0 equals) (subseq field (1+ equals))) options)
                (when (and (eq kind :command-end) (null status))
                  (setf status (ignore-errors (parse-integer field)))))))
        (values kind status (nreverse options))))))

;;; Notifications

(defun parse-notification (number text)
  "OSC 9 ; TEXT (iTerm2) or OSC 777 ; notify ; TITLE ; BODY (rxvt):
a plist (:TITLE title :BODY body), or for OSC 9 ; 4 ; STATE ; VALUE
(ConEmu progress) (:PROGRESS state :VALUE value).  NIL otherwise."
  (case number
    (9 (let ((fields (split text #\;)))
         (if (and (string= (first fields) "4") (rest fields))
             (list :progress (ignore-errors (parse-integer (second fields)))
                   :value (ignore-errors (parse-integer (or (third fields) ""))))
             (and (plusp (length text)) (list :title nil :body text)))))
    (777 (let ((fields (split text #\;)))
           (when (and (string= (first fields) "notify") (second fields))
             (list :title (second fields)
                   :body (format nil "~{~a~^;~}" (cddr fields))))))))

(defstruct (notification-assembler (:constructor make-notification-assembler ()))
  "Pending kitty (OSC 99) notifications, by identifier."
  (pending (make-hash-table :test #'equal) :type hash-table))

(defparameter *notification-limit* 8192
  "Most characters kept for a notification's title or body.")

(defun notification-assembler-feed (assembler text)
  "Feed the payload of an OSC 99 string (the text after 99;).  Return a
plist (:ID :TITLE :BODY :URGENCY :OCCASION) once a notification is
complete, (:QUERY id) for a support query, (:CLOSE id) to close one, or NIL."
  (let* ((first (position #\; text))
         (metadata (if first (subseq text 0 first) text))
         (payload (if first (subseq text (1+ first)) ""))
         (keys (loop for pair in (split metadata #\:)
                     for equals = (position #\= pair)
                     when (and equals (= equals 1))
                       collect (cons (char pair 0) (subseq pair 2))))
         (id (or (cdr (assoc #\i keys)) ""))
         (done (not (equal "0" (cdr (assoc #\d keys)))))
         (type (or (cdr (assoc #\p keys)) "title"))
         (text (if (equal "1" (cdr (assoc #\e keys)))
                   (or (ignore-errors
                        (babel:octets-to-string
                         (cl-base64:base64-string-to-usb8-array payload)
                         :encoding :utf-8 :errorp nil))
                       "")
                   payload))
         (pending (assemble-entry assembler id)))
    (cond
      ((string= type "?") (list :query id))
      ((string= type "close") (remhash id (notification-assembler-pending assembler))
       (list :close id))
      (t
       (flet ((append-to (key)
                (setf (getf pending key)
                      (let ((joined (concatenate 'string (or (getf pending key) "") text)))
                        (subseq joined 0 (min (length joined) *notification-limit*))))))
         (cond ((string= type "title") (append-to :title))
               ((string= type "body") (append-to :body))))
       (let ((urgency (cdr (assoc #\u keys)))
             (occasion (cdr (assoc #\o keys))))
         (when urgency (setf (getf pending :urgency) (ignore-errors (parse-integer urgency))))
         (when occasion (setf (getf pending :occasion)
                              (cond ((string= occasion "unfocused") :unfocused)
                                    ((string= occasion "invisible") :invisible)
                                    (t :always)))))
       (if done
           (progn
             (remhash id (notification-assembler-pending assembler))
             (let ((title (getf pending :title))
                   (body (getf pending :body)))
               (when (or (plusp (length title)) (plusp (length body)))
                 (list :id id
                       :title (if (plusp (length title)) title body)
                       :body (if (plusp (length title)) body nil)
                       :urgency (getf pending :urgency)
                       :occasion (or (getf pending :occasion) :always)))))
           (progn
             (setf (gethash id (notification-assembler-pending assembler)) pending)
             nil))))))

(defun assemble-entry (assembler id)
  (let ((table (notification-assembler-pending assembler)))
    ;; Bound how many unfinished notifications are kept.
    (when (and (not (gethash id table)) (> (hash-table-count table) 32))
      (clrhash table))
    (copy-list (gethash id table))))
