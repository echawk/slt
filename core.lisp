(in-package #:slt)

(defconstant +shift-mask+ #x01)
(defconstant +control-mask+ #x04)
(defconstant +mod1-mask+ #x08)
(defconstant +mod4-mask+ #x40)
(defconstant +escape-char+ (code-char 27))
(defconstant +rubout-char+ (code-char 127))

(defstruct (cell-view
            (:constructor make-cell-view
                (&key char fg bg attributes cursor-p)))
  (char " " :type string)
  (fg "#E5E5E5" :type string)
  (bg "#000000" :type string)
  (attributes '() :type list)
  (cursor-p nil :type boolean))

(defparameter *keysym-map*
  '(("BackSpace" . :backspace)
    ("Delete" . :delete)
    ("Down" . :down)
    ("End" . :end)
    ("Escape" . :escape)
    ("Home" . :home)
    ("Insert" . :insert)
    ("KP_0" . :kp_0)
    ("KP_1" . :kp_1)
    ("KP_2" . :kp_2)
    ("KP_3" . :kp_3)
    ("KP_4" . :kp_4)
    ("KP_5" . :kp_5)
    ("KP_6" . :kp_6)
    ("KP_7" . :kp_7)
    ("KP_8" . :kp_8)
    ("KP_9" . :kp_9)
    ("KP_Delete" . :kp_delete)
    ("KP_Down" . :kp_down)
    ("KP_End" . :kp_end)
    ("KP_Enter" . :kp_enter)
    ("KP_Home" . :kp_home)
    ("KP_Insert" . :kp_insert)
    ("KP_Left" . :kp_left)
    ("KP_Next" . :kp_next)
    ("KP_Prior" . :kp_prior)
    ("KP_Right" . :kp_right)
    ("KP_Up" . :kp_up)
    ("Left" . :left)
    ("Next" . :next)
    ("Page_Down" . :next)
    ("Page_Up" . :prior)
    ("Prior" . :prior)
    ("Return" . :return)
    ("Right" . :right)
    ("Tab" . :tab)
    ("Up" . :up)))

(defun color->hex (color)
  (destructuring-bind (red green blue) (3bst:color-rgb color)
    (format nil "#~2,'0X~2,'0X~2,'0X"
            (round (* red 255))
            (round (* green 255))
            (round (* blue 255)))))

(defun cell-char-string (char attributes)
  (if (or (null char)
          (member :invisible attributes)
          (member :wdummy attributes)
          (char= char (code-char 0)))
      " "
      (string char)))

(defun terminal-cursor-row (term)
  (3bst::y (3bst::cursor term)))

(defun terminal-cursor-column (term)
  (3bst::x (3bst::cursor term)))

(defun terminal-cursor-hidden-p (term)
  (logtest 3bst::+mode-hide+ (3bst::mode term)))

(defun term-cell-view (term row column)
  (let* ((screen (3bst::screen term))
         (glyph (3bst:glyph-at screen row column))
         (glyph-attributes (3bst:glyph-attributes glyph))
         (display-glyph (if (and (member :wdummy glyph-attributes) (plusp column))
                            (3bst:glyph-at screen row (1- column))
                            glyph))
         (attributes (3bst:glyph-attributes display-glyph))
         (reversep (member :reverse attributes))
         (cursorp (and (= row (terminal-cursor-row term))
                       (= column (terminal-cursor-column term))
                       (not (terminal-cursor-hidden-p term))))
         (foreground (color->hex (if reversep
                                     (3bst:bg display-glyph)
                                     (3bst:fg display-glyph))))
         (background (color->hex (if reversep
                                     (3bst:fg display-glyph)
                                     (3bst:bg display-glyph))))
         (char (if (member :wdummy glyph-attributes)
                   " "
                   (cell-char-string (3bst:c display-glyph) attributes))))
    (when cursorp
      (rotatef foreground background))
    (make-cell-view :char char
                    :fg foreground
                    :bg background
                    :attributes attributes
                    :cursor-p cursorp)))

(defun dirty-rows (term)
  (loop for dirty-bit across (3bst:dirty term)
        for row from 0
        when (plusp dirty-bit)
          collect row))

(defun clear-dirty-rows (term &optional (rows (dirty-rows term)))
  (dolist (row rows term)
    (setf (aref (3bst:dirty term) row) 0)))

(defun mark-all-dirty (term)
  (fill (3bst:dirty term) 1)
  term)

(defun resize-term (term rows columns)
  (3bst::tresize columns rows :term term)
  (mark-all-dirty term)
  term)

(defun decode-modifier-state (state)
  (let (modifiers)
    (when (logtest +shift-mask+ state)
      (push :shift modifiers))
    (when (logtest +control-mask+ state)
      (push :control modifiers))
    (when (logtest +mod1-mask+ state)
      (push :mod1 modifiers))
    (when (logtest +mod4-mask+ state)
      (push :mod4 modifiers))
    (nreverse modifiers)))

(defun keysym->terminal-key (keysym)
  (cond
    ((null keysym) nil)
    ((assoc keysym *keysym-map* :test #'string=)
     (cdr (assoc keysym *keysym-map* :test #'string=)))
    ((and (> (length keysym) 1)
          (char= (aref keysym 0) #\F)
          (every #'digit-char-p (subseq keysym 1)))
     (intern keysym :keyword))
    (t nil)))

(defun control-string-for-char (char)
  (cond
    ((char= char #\Space)
     (string (code-char 0)))
    ((char= char #\?)
     (string +rubout-char+))
    (t
     (let ((code (char-code (char-upcase char))))
       (when (<= (char-code #\@) code (char-code #\_))
         (string (code-char (- code 64))))))))

(defun event-character (char keysym)
  (cond
    ((and char (plusp (length char)))
     (aref char 0))
    ((and keysym (= (length keysym) 1))
     (aref keysym 0))
    ((and keysym (string= keysym "space"))
     #\Space)
    (t nil)))

(defun decode-backslash-escapes (string)
  (with-output-to-string (out)
    (loop with length = (length string)
          for index from 0 below length
          for char = (aref string index)
          do (if (char/= char #\\)
                 (write-char char out)
                 (if (= index (1- length))
                     (write-char char out)
                     (let ((next (aref string (1+ index))))
                       (cond
                         ((char= next #\\)
                          (write-char #\\ out)
                          (incf index))
                         ((char= next #\r)
                          (write-char #\Return out)
                          (incf index))
                         ((char= next #\n)
                          (write-char #\Linefeed out)
                          (incf index))
                         ((char= next #\t)
                          (write-char #\Tab out)
                          (incf index))
                         ((char= next #\e)
                          (write-char +escape-char+ out)
                          (incf index))
                         ((digit-char-p next 8)
                          (let ((start (1+ index))
                                (end (1+ index)))
                            (loop while (and (< end length)
                                             (< (- end start) 3)
                                             (digit-char-p (aref string end) 8))
                                  do (incf end))
                            (write-char (code-char (parse-integer string
                                                                  :start start
                                                                  :end end
                                                                  :radix 8))
                                        out)
                            (setf index (1- end))))
                         (t
                          (write-char next out)
                          (incf index)))))))))

(defun translate-key-event (term char keysym state)
  (let* ((modifiers (decode-modifier-state state))
         (special-key (keysym->terminal-key keysym))
         (character (event-character char keysym)))
    (cond
      ((member special-key '(:return :kp_enter))
       (string #\Return))
      (special-key
       (or (let ((mapped (3bst::kmap special-key modifiers :term term)))
             (and mapped (decode-backslash-escapes mapped)))
           (case special-key
             (:backspace (string +rubout-char+))
             (:escape (string +escape-char+))
             (:tab (string #\Tab))
             (t nil))))
      (character
       (let ((string (string character)))
         (cond
           ((member :control modifiers)
            (control-string-for-char character))
           ((member :mod1 modifiers)
            (format nil "~C~A" +escape-char+ string))
           (t string))))
      (t nil))))
