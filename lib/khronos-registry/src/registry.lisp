(in-package #:khronos-registry)

;;;; What the Khronos registries (Vulkan's vk.xml, EGL's egl.xml) share:
;;;; reading their XML, their C literals, versions and depends expressions,
;;;; declarations, and making Lisp names of C ones.  The schema is
;;;; described in KhronosGroup/Vulkan-Docs, registry.adoc.

(defparameter +word-separator+ #\_
  "What separates words in C names (VK_FORMAT_R8_UNORM).")


(defparameter +list-separator+ #\,
  "What separates the APIs of an api attribute, and alternatives in a
depends expression.")


(defparameter +conjunction+ #\+
  "What joins requirements that must all hold in a depends expression.")


;; The widths of C's unsigned and unsigned long long, for (~0U) and (~0ULL).
(defconstant +unsigned-bits+ 32)
(defconstant +unsigned-long-long-bits+ 64)
(defparameter +unsigned-long-long-suffix+ "ULL")


;;; Elements

(defun read-registry-document (pathname)
  "The registry XML at PATHNAME, parsed as XML (plump otherwise treats some
of its tags, <param> among them, as HTML's)."
  (let ((plump:*tag-dispatchers* plump:*xml-tags*))
    (plump:parse pathname)))

(defun attribute (element name)
  (plump:attribute element name))

(defun child-elements (node &optional tag)
  "NODE's child elements, those named TAG when it is given."
  (loop for child across (plump:children node)
        when (and (plump:element-p child)
                  (or (null tag) (string= tag (plump:tag-name child))))
          collect child))

(defun child-element (node tag)
  (first (child-elements node tag)))

(defun child-text (node tag)
  (let ((child (child-element node tag)))
    (and child (plump:text child))))

(defun split-list (string &optional (separator +list-separator+))
  (loop for start = 0 then (1+ end)
        for end = (position separator string :start start)
        collect (subseq string start end)
        while end))


(defun element-name (element)
  "The name an element defines: as an attribute, a <name> child, or the
<name> in its <proto> (commands and function pointers)."
  (or (attribute element "name")
      (child-text element "name")
      (let ((proto (child-element element "proto")))
        (and proto (child-text proto "name")))))


;;; Values

(defun parse-value (text)
  "The value of an enumerant's TEXT: an integer in C syntax, (~NU) or
(~NULL), a float (1000.0F), a quoted string, or a cast macro's value
(EGL_CAST(EGLint,-1))."
  (let ((text (string-trim " " text)))
    (cond
      ((char= #\" (char text 0)) (subseq text 1 (1- (length text))))
      ((and (alpha-char-p (char text 0)) (find #\( text) (find #\, text))
       ;; NAME(TYPE,VALUE): the value, as the type's.
       (parse-value (subseq text (1+ (position #\, text)) (position #\) text :from-end t))))
      ((and (char= #\( (char text 0)) (char= #\~ (char text 1)))
       ;; (~NU) is 32 bits, (~NULL) 64.
       (let* ((digits-end (position-if-not #'digit-char-p text :start 2))
              (bits (if (search +unsigned-long-long-suffix+ text :start2 digits-end)
                        +unsigned-long-long-bits+
                        +unsigned-bits+)))
         (ldb (byte bits 0) (lognot (parse-integer text :start 2 :end digits-end)))))
      ((find #\. text)
       (let ((*read-default-float-format* 'single-float))
         (coerce (read-from-string (string-right-trim "Ff" text)) 'single-float)))
      ((search "0x" text)
       (let ((magnitude (parse-integer text :start (+ 2 (search "0x" text)) :radix 16)))
         (if (char= #\- (char text 0)) (- magnitude) magnitude)))
      (t (parse-integer (string-right-trim "UL" text))))))

(defun version-parts (version)
  (mapcar #'parse-integer (split-list version #\.)))

(defun version<= (a b)
  (loop for x in (version-parts a)
        for y in (version-parts b)
        when (< x y) return t
        when (> x y) return nil
        finally (return t)))


(defun depends-satisfied-p (expression names)
  "Whether the depends EXPRESSION holds given the feature and extension
NAMES (a hash table): names joined by +CONJUNCTION+ (and) and
+LIST-SEPARATOR+ (or), left to right, grouped with parentheses."
  (let ((position 0))
    (labels ((peek () (and (< position (length expression)) (char expression position)))
             (term ()
               (if (eql (peek) #\()
                   (progn (incf position)
                          (prog1 (disjunction) (incf position)))
                   (let ((end (or (position-if (lambda (char)
                                                 (find char (list #\( #\) +conjunction+
                                                                  +list-separator+)))
                                               expression :start position)
                                  (length expression))))
                     (prog1 (gethash (subseq expression position end) names)
                       (setf position end)))))
             (disjunction ()
               (let ((value (term)))
                 (loop for operator = (peek)
                       while (member operator (list +conjunction+ +list-separator+))
                       do (incf position)
                          (let ((next (term)))
                            (setf value (if (eql operator +conjunction+)
                                            (and value next)
                                            (or value next)))))
                 value)))
      (and (disjunction) t))))


(defun lisp-words (string)
  "STRING's words in lower case, hyphenated: camel case is split where a
lower-case letter meets an upper-case one, or an upper-case letter or
digit meets a capitalized word; underscores separate words."
  (with-output-to-string (out)
    (loop for i from 0 below (length string)
          for char = (char string i)
          for previous = (and (plusp i) (char string (1- i)))
          for next = (and (< (1+ i) (length string)) (char string (1+ i)))
          do (cond ((char= char +word-separator+) (write-char #\- out))
                   (t (when (and previous (upper-case-p char)
                                 (or (lower-case-p previous)
                                     (and (or (upper-case-p previous) (digit-char-p previous))
                                          next (lower-case-p next))))
                        (write-char #\- out))
                      (write-char (char-downcase char) out))))))

(defun without-prefix (prefix string)
  (if (and (> (length string) (length prefix)) (string= prefix string :end2 (length prefix)))
      (subseq string (length prefix))
      string))


(defparameter +type-tags+ '("type" "ptype")
  "The tags a declaration names its type with: Vulkan's and EGL's.")

(defun parse-declaration (element)
  "The member or parameter ELEMENT's (values NAME TYPE POINTERS DIMENSIONS
BITS): its type's name, how many pointers to it, and the array
dimensions after its name (numbers or constants' names), and its bit
width when a bitfield."
  (let (name type (pointers 0) (suffix (make-string-output-stream)))
    (loop for child across (plump:children element)
          do (cond ((plump:text-node-p child)
                    (if name
                        (write-string (plump:text child) suffix)
                        (incf pointers (count #\* (plump:text child)))))
                   ((plump:element-p child)
                    (let ((tag (plump:tag-name child)))
                      (cond ((member tag +type-tags+ :test #'string=)
                             (setf type (plump:text child)))
                            ((string= tag "name") (setf name (plump:text child)))
                            ((and name (string= tag "enum"))
                             (write-string (plump:text child) suffix)))))))
    (let* ((suffix (get-output-stream-string suffix))
           (dimensions (loop for start = (position #\[ suffix) then (position #\[ suffix :start end)
                             for end = (and start (position #\] suffix :start start))
                             while start
                             collect (let ((text (string-trim " " (subseq suffix (1+ start) end))))
                                       (if (every #'digit-char-p text) (parse-integer text) text))))
           (colon (position #\: suffix)))
      (values name type pointers dimensions
              (and colon (parse-integer suffix :start (1+ colon) :junk-allowed t))))))

