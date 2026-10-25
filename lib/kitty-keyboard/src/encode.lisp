(in-package #:kitty-keyboard)

;;;; Encoding key events under a set of enhancement flags.

(defun csi (&rest parts)
  (apply #'concatenate 'string (string (code-char 27)) "[" parts))

(defun text-codepoints (text)
  "TEXT's code points joined with colons, controls left out."
  (format nil "~{~d~^:~}"
          (loop for char across text
                for code = (char-code char)
                unless (or (< code 32) (<= 127 code 159))
                  collect code)))

(defun modifier-field (bits event force)
  "The modifiers field: 1 + BITS, with the event type when not a press;
empty unless needed (or FORCE)."
  (let ((type (ecase event (:press 1) (:repeat 2) (:release 3))))
    (cond ((/= type 1) (format nil "~d:~d" (1+ bits) type))
          ((or (plusp bits) force) (format nil "~d" (1+ bits)))
          (t ""))))

(defun encode-csi-u (code bits event &key shifted base text)
  "CSI CODE[:SHIFTED[:BASE]] ; MODIFIERS[:EVENT] ; TEXT u, fields left out
when empty."
  (let* ((alternates (cond ((and shifted base) (format nil ":~d:~d" shifted base))
                           (shifted (format nil ":~d" shifted))
                           (base (format nil "::~d" base))
                           (t "")))
         (text-field (and text (plusp (length text)) (text-codepoints text)))
         (text-field (and text-field (plusp (length text-field)) text-field))
         (mods (modifier-field bits event nil)))
    (csi (format nil "~d~a" code alternates)
         (cond (text-field (format nil ";~a;~a" mods text-field))
               ((plusp (length mods)) (format nil ";~a" mods))
               (t ""))
         "u")))

(defun encode-functional (key bits event &key application-cursor-keys legacy)
  "A functional key in its CSI form (CSI 1;m X, CSI n;m ~ or CSI n;m u).
With LEGACY, keys without modifiers use their legacy forms (SS3 for the
cursor keys in application mode and F1-F4)."
  (destructuring-bind (code form extra) (gethash key *functional-keys*)
    (let ((mods (modifier-field bits event nil)))
      (ecase form
        (:letter
         (cond
           ((and legacy (string= mods "")
                 (or (member key '(:f1 :f2 :f4))
                     (and application-cursor-keys
                          (member key '(:up :down :left :right :home :end)))))
            (format nil "~cO~c" (code-char 27) extra))
           ((string= mods "") (csi (string extra)))
           (t (csi (format nil "1;~a~c" mods extra)))))
        (:tilde
         (csi (if (string= mods "")
                  (format nil "~d~~" extra)
                  (format nil "~d;~a~~" extra mods))))
        (:u (encode-csi-u code bits event))))))

(defun legacy-c0-key (key bits)
  "Enter, Escape, Backspace, Tab and Space in the legacy encoding, or NIL."
  (let* ((esc (string (code-char 27)))
         (alt (logtest bits 2))
         (ctrl (logtest bits 4))
         (shift (logtest bits 1))
         (others (logandc2 bits (logior 1 2 4 +lock-bits+))))
    (when (zerop others)
      (let ((base (case key
                    (:enter (string (code-char 13)))
                    (:escape esc)
                    (:backspace (string (code-char (if ctrl 8 127))))
                    (:tab (if shift (csi "Z") (string (code-char 9))))
                    (#\Space (string (code-char (if ctrl 0 32)))))))
        (when base
          (if (and alt (not (and (eq key :tab) shift ctrl)))
              (concatenate 'string esc base)
              (if (and (eq key :tab) shift ctrl alt) nil base)))))))

(defun legacy-text-key (key bits text)
  "A text key with Shift, Alt and Ctrl combinations in the legacy encoding,
or NIL when the combination has no legacy form."
  (let* ((shift (logtest bits 1))
         (alt (logtest bits 2))
         (ctrl (logtest bits 4))
         (others (logandc2 bits (logior 1 2 4 +lock-bits+))))
    (when (and (zerop others)
               (find (char-downcase key) *legacy-text-keys*)
               (not (and ctrl shift)))
      (let ((body (cond
                    (ctrl (let ((byte (ctrl-byte (char-downcase key))))
                            (string (if byte (code-char byte) (char-downcase key)))))
                    (shift (if (and text (plusp (length text))) text
                               (string (shifted-char (char-downcase key)))))
                    (t (string (char-downcase key))))))
        (if alt (concatenate 'string (string (code-char 27)) body) body)))))

(defun plain-text-p (text)
  (and text (plusp (length text))
       (notany (lambda (char) (let ((code (char-code char)))
                                (or (< code 32) (<= 127 code 159))))
               text)))

(defun encode-key (flags key &key modifiers (event :press) text shifted-key base-key
                               application-cursor-keys)
  "The bytes (a string) to send for a key event, or NIL for none.

FLAGS are the enhancement flags in effect (see KEYBOARD-FLAGS).  KEY is the
unshifted key, a character, or a functional key keyword (:ENTER, :F5,
:KP-0, :LEFT-SHIFT ...); KEY NIL with TEXT is pure text input (from an input
method).  MODIFIERS lists :SHIFT :ALT :CONTROL :SUPER :HYPER :META
:CAPS-LOCK :NUM-LOCK.  EVENT is :PRESS, :REPEAT or :RELEASE.  TEXT is what
the key types, if anything; SHIFTED-KEY and BASE-KEY (characters) feed the
alternate-keys enhancement.  APPLICATION-CURSOR-KEYS is DECCKM."
  (let* ((bits (modifier-bits modifiers))
         (all (logtest flags +report-all-keys+))
         (disambiguate (or all (logtest flags +disambiguate+)))
         (events (logtest flags +report-events+))
         (alternates (logtest flags +report-alternates+))
         (event (if (and (eq event :repeat) (not events)) :press event))
         (key (if (characterp key) (char-downcase key) key)))
    (when (and (eq event :release) (not events))
      (return-from encode-key nil))
    (flet ((csi-u (code &optional with-text)
             (encode-csi-u code (if all bits (logandc2 bits +lock-bits+)) event
                           :shifted (and alternates shifted-key (logtest bits 1)
                                         (char-code shifted-key))
                           :base (and alternates base-key (char-code base-key))
                           :text (and with-text (logtest flags +report-text+)
                                      (not (eq event :release)) text))))
      (cond
        ;; Text from an input method, with no key.
        ((null key)
         (cond ((not (plain-text-p text)) nil)
               ((and all (logtest flags +report-text+)) (csi-u 0 t))
               ((eq event :release) nil)
               (t text)))
        (all
         (cond ((characterp key) (csi-u (key-code key) t))
               (t (destructuring-bind (code form extra) (gethash key *functional-keys*)
                    (declare (ignore code extra))
                    (if (eq form :u)
                        (csi-u (key-code key) (member key '(:enter :tab)))
                        (encode-functional key bits event))))))
        ;; Pressing a modifier alone reports nothing below report-all-keys.
        ((member key *modifier-keys*) nil)
        ;; Keys typing text with at most Shift (keypad digits too): the
        ;; text itself.
        ((and (or (characterp key) (assoc key *keypad-legacy*)) (plain-text-p text)
              (zerop (logandc2 bits (logior 1 +lock-bits+))))
         (if (eq event :release) nil text))
        ((member key '(:enter :tab :backspace))
         (cond ((eq event :release) nil)
               ((and disambiguate (plusp (logandc2 bits +lock-bits+))
                     (not (and (eq key :tab) (= (logandc2 bits +lock-bits+) 1))))
                (csi-u (key-code key)))
               (t (or (legacy-c0-key key bits) (csi-u (key-code key))))))
        ((eq key :escape)
         (if disambiguate
             (csi-u 27)
             (and (not (eq event :release))
                  (or (legacy-c0-key key bits) (csi-u 27)))))
        (disambiguate
         (if (characterp key)
             (csi-u (key-code key))
             (encode-functional key (logandc2 bits +lock-bits+) event)))
        ;; Legacy encoding.
        ((eq event :release) nil)
        ((characterp key)
         (or (and (char= key #\Space) (legacy-c0-key key bits))
             (legacy-text-key key bits text)
             (and (zerop bits) (string key))
             (csi-u (key-code key))))
        ((assoc key *keypad-legacy*)
         (let ((twin (cdr (assoc key *keypad-legacy*))))
           (if (characterp twin)
               (if (zerop (logandc2 bits +lock-bits+))
                   (string twin)
                   (or (legacy-text-key twin bits nil) (csi-u (key-code key))))
               (encode-key flags twin :modifiers modifiers :event event
                                      :application-cursor-keys application-cursor-keys))))
        ((eq key :kp-begin)
         (encode-functional key (logandc2 bits +lock-bits+) event :legacy t))
        ((eq key :menu)
         (let ((mods (modifier-field (logandc2 bits +lock-bits+) event nil)))
           (csi (if (string= mods "") "29~" (format nil "29;~a~~" mods)))))
        (t
         (encode-functional key (logandc2 bits +lock-bits+) event
                            :application-cursor-keys application-cursor-keys
                            :legacy t))))))
