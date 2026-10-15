;;;; api.lisp -- the Lispy interface.

(in-package #:libvterm)

;;; ------------------------------------------------------------------
;;; Packed cell encoding (shared with src/shim.c; see README.md)

(defconstant +attr-bold+            #x00001)
(defconstant +attr-italic+          #x00002)
(defconstant +attr-underline-mask+  #x0000C) ; 2 bits: 0 off 1 single 2 double 3 curly
(defconstant +attr-blink+           #x00010)
(defconstant +attr-inverse+         #x00020)
(defconstant +attr-conceal+         #x00040)
(defconstant +attr-strike+          #x00080)
(defconstant +attr-combining+       #x00100)
(defconstant +attr-width-mask+      #x00600)
(defconstant +attr-width-shift+     9)

(defconstant +color-kind-default+ 0)
(defconstant +color-kind-palette+ 1)
(defconstant +color-kind-rgb+ 2)

(deftype u32-vector () '(simple-array (unsigned-byte 32) (*)))
(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(declaim (inline attrs-width attrs-combining-p))
(defun attrs-width (attrs)
  "Cell width from a packed attribute word: 1, 2, or 0 for the right half of
a wide character."
  (ldb (byte 2 9) attrs))

(defun attrs-combining-p (attrs)
  "True when the cell holds combining codepoints beyond the base character
(which VTERM-ROW-INTO's CODES array does not carry; use VTERM-CELL)."
  (logtest attrs +attr-combining+))

(defun decode-color (packed)
  "Packed colour word -> (:default) | (:palette n) | (:rgb r g b)."
  (case (ldb (byte 8 24) packed)
    (0 (list :default))
    (1 (list :palette (ldb (byte 8 0) packed)))
    (t (list :rgb (ldb (byte 8 16) packed) (ldb (byte 8 8) packed)
             (ldb (byte 8 0) packed)))))

(defun decode-attrs (packed)
  "Packed attribute word -> list of keywords from :bold :italic :underline
:double-underline :curly-underline :blink :inverse :conceal :strike."
  (let ((out '()))
    (when (logtest packed +attr-strike+) (push :strike out))
    (when (logtest packed +attr-conceal+) (push :conceal out))
    (when (logtest packed +attr-inverse+) (push :inverse out))
    (when (logtest packed +attr-blink+) (push :blink out))
    (case (ldb (byte 2 2) packed)
      (1 (push :underline out))
      (2 (push :double-underline out))
      (3 (push :curly-underline out)))
    (when (logtest packed +attr-italic+) (push :italic out))
    (when (logtest packed +attr-bold+) (push :bold out))
    out))

;;; ------------------------------------------------------------------
;;; Terminal objects and the id registry used by the C callbacks

(defstruct (vterm (:constructor %make-vterm) (:copier nil) (:predicate vterm-p))
  (id 0 :type fixnum)
  (pointer nil)
  (rows 0 :type fixnum)
  (columns 0 :type fixnum)
  (damage (make-array 0 :element-type 'bit) :type simple-bit-vector)
  (cursor-row 0 :type fixnum)
  (cursor-visible t)
  (title nil)
  (icon-name nil)
  (string-buffer nil)                   ; octets of a TITLE/ICONNAME in progress
  (properties '())                      ; plist of other termprops
  (capture nil)                         ; adjustable octet vector when encoding keys
  (pending-errors '())
  (on-output nil)
  (on-bell nil)
  (on-title nil)
  (on-scrollback nil))

(setf (documentation 'vterm-on-output 'function)
      "Function of (vterm octets) called with bytes libvterm wants written to
the pty (keyboard input, DA/DSR replies...).  SETFable."
      (documentation 'vterm-on-bell 'function) "Function of (vterm), or NIL."
      (documentation 'vterm-on-title 'function) "Function of (vterm title-string), or NIL."
      (documentation 'vterm-on-scrollback 'function)
      "Function of (vterm codes fg bg attrs) called with the packed contents of
a line scrolled off the top of the primary screen, or NIL.")

(defmethod print-object ((vt vterm) stream)
  (print-unreadable-object (vt stream :type t :identity t)
    (format stream "#~D ~Dx~D~:[ freed~;~]"
            (vterm-id vt) (vterm-rows vt) (vterm-columns vt) (vterm-pointer vt))))

(defvar *registry* (make-hash-table :test 'eql #+sbcl :synchronized #+sbcl t))
(defvar *next-id* 0)
(declaim (type fixnum *next-id*))

(defun %allocate-id ()
  (loop (setf *next-id* (logand (1+ *next-id*) #x3fffffff))
        (unless (or (zerop *next-id*) (gethash *next-id* *registry*))
          (return *next-id*))))

(declaim (inline %lookup))
(defun %lookup (id) (gethash id *registry*))

(defun vterm-live-p (vt) (and (vterm-pointer vt) t))

(defun %ptr (vt)
  (or (vterm-pointer vt) (error "~S has been freed." vt)))

(defun %mark-rows (vt start end)
  (let* ((bv (vterm-damage vt))
         (n (length bv))
         (s (max 0 (min start n)))
         (e (max s (min end n))))
    (fill bv 1 :start s :end e)))

(defmacro %guarded ((vt) &body body)
  "Run user code from inside a C callback: never unwind through libvterm;
errors are queued and re-signalled when control is back in Lisp."
  `(handler-case (progn ,@body)
     (error (e) (push e (vterm-pending-errors ,vt)))))

(defun %signal-pending (vt)
  (let ((errs (vterm-pending-errors vt)))
    (when errs
      (setf (vterm-pending-errors vt) '())
      (error (car (last errs))))))

;;; C callbacks.  All dispatch on the integer id passed as libvterm's user
;;; pointer, so any number of terminals can be live concurrently.

(cffi:defcallback cb-damage :int ((id :int) (sr :int) (er :int) (sc :int) (ec :int))
  (declare (ignore sc ec))
  (let ((vt (%lookup id))) (when vt (%mark-rows vt sr er)))
  1)

(cffi:defcallback cb-moverect :int ((id :int) (dsr :int) (der :int) (dsc :int) (dec :int)
                                    (ssr :int) (ser :int) (ssc :int) (sec :int))
  (declare (ignore dsc dec ssr ser ssc sec))
  (let ((vt (%lookup id))) (when vt (%mark-rows vt dsr der)))
  1)

(cffi:defcallback cb-movecursor :int ((id :int) (row :int) (col :int) (visible :int))
  (declare (ignore col visible))
  (let ((vt (%lookup id)))
    (when vt
      (let ((old (vterm-cursor-row vt)))
        (%mark-rows vt old (1+ old))
        (%mark-rows vt row (1+ row))
        (setf (vterm-cursor-row vt) row))))
  1)

(cffi:defcallback cb-prop-int :int ((id :int) (prop :int) (value :int))
  (let ((vt (%lookup id)))
    (when vt
      (cond ((= prop +prop-cursorvisible+)
             (setf (vterm-cursor-visible vt) (/= value 0))
             (let ((r (vterm-cursor-row vt))) (%mark-rows vt r (1+ r))))
            (t
             (let ((key (case prop
                          (#.+prop-cursorblink+ :cursor-blink)
                          (#.+prop-altscreen+ :altscreen)
                          (#.+prop-reverse+ :reverse)
                          (#.+prop-cursorshape+ :cursor-shape)
                          (#.+prop-mouse+ :mouse)
                          (#.+prop-focusreport+ :focus-report)
                          (t prop)))
                   (val (if (member prop (list +prop-cursorshape+ +prop-mouse+))
                            value
                            (/= value 0))))
               (setf (getf (vterm-properties vt) key) val)
               (when (member prop (list +prop-altscreen+ +prop-reverse+))
                 (vterm-mark-all-dirty vt)))))))
  1)

(cffi:defcallback cb-prop-string :int ((id :int) (prop :int) (str :pointer) (len :int)
                                       (initial :int) (final :int))
  (let ((vt (%lookup id)))
    (when (and vt (or (= prop +prop-title+) (= prop +prop-iconname+)))
      (let ((buf (if (or (/= initial 0) (null (vterm-string-buffer vt)))
                     (setf (vterm-string-buffer vt)
                           (make-array 64 :element-type '(unsigned-byte 8)
                                          :adjustable t :fill-pointer 0))
                     (vterm-string-buffer vt))))
        (dotimes (i len)
          (vector-push-extend (cffi:mem-aref str :uint8 i) buf))
        (when (/= final 0)
          (let ((s (babel:octets-to-string (coerce buf 'octets) :encoding :utf-8
                                                                :errorp nil)))
            (setf (vterm-string-buffer vt) nil)
            (if (= prop +prop-title+)
                (progn (setf (vterm-title vt) s)
                       (let ((f (vterm-on-title vt)))
                         (when f (%guarded (vt) (funcall f vt s)))))
                (setf (vterm-icon-name vt) s)))))))
  1)

(cffi:defcallback cb-bell :int ((id :int))
  (let ((vt (%lookup id)))
    (when vt
      (let ((f (vterm-on-bell vt)))
        (when f (%guarded (vt) (funcall f vt))))))
  1)

(cffi:defcallback cb-resize :int ((id :int) (rows :int) (cols :int))
  (let ((vt (%lookup id)))
    (when vt (%note-size vt rows cols)))
  1)

(cffi:defcallback cb-sb-pushline :int ((id :int) (cols :int) (cells :pointer))
  (let* ((vt (%lookup id))
         (f (and vt (vterm-on-scrollback vt))))
    (when f
      (let ((codes (make-row-buffer cols)) (fg (make-row-buffer cols))
            (bg (make-row-buffer cols)) (attrs (make-row-buffer cols)))
        (cffi:with-pointer-to-vector-data (pc codes)
          (cffi:with-pointer-to-vector-data (pf fg)
            (cffi:with-pointer-to-vector-data (pb bg)
              (cffi:with-pointer-to-vector-data (pa attrs)
                (%clvt-pack-cells cells cols pc pf pb pa)))))
        (%guarded (vt) (funcall f vt codes fg bg attrs)))))
  1)

(cffi:defcallback cb-output :void ((id :int) (s :pointer) (len :size))
  (let ((vt (%lookup id)))
    (when vt
      (let ((cap (vterm-capture vt)))
        (if cap
            (dotimes (i len) (vector-push-extend (cffi:mem-aref s :uint8 i) cap))
            (let ((f (vterm-on-output vt)))
              (when f
                (let ((octets (make-array len :element-type '(unsigned-byte 8))))
                  (dotimes (i len) (setf (aref octets i) (cffi:mem-aref s :uint8 i)))
                  (%guarded (vt) (funcall f vt octets))))))))))

(defun %install-hooks ()
  (cffi:with-foreign-object (h '(:struct clvt-hooks))
    (setf (cffi:foreign-slot-value h '(:struct clvt-hooks) 'damage) (cffi:callback cb-damage)
          (cffi:foreign-slot-value h '(:struct clvt-hooks) 'moverect) (cffi:callback cb-moverect)
          (cffi:foreign-slot-value h '(:struct clvt-hooks) 'movecursor) (cffi:callback cb-movecursor)
          (cffi:foreign-slot-value h '(:struct clvt-hooks) 'prop-int) (cffi:callback cb-prop-int)
          (cffi:foreign-slot-value h '(:struct clvt-hooks) 'prop-string) (cffi:callback cb-prop-string)
          (cffi:foreign-slot-value h '(:struct clvt-hooks) 'bell) (cffi:callback cb-bell)
          (cffi:foreign-slot-value h '(:struct clvt-hooks) 'resize) (cffi:callback cb-resize)
          (cffi:foreign-slot-value h '(:struct clvt-hooks) 'sb-pushline) (cffi:callback cb-sb-pushline)
          (cffi:foreign-slot-value h '(:struct clvt-hooks) 'output) (cffi:callback cb-output))
    (%clvt-set-hooks h)))

;;; ------------------------------------------------------------------
;;; Lifecycle

(defun %note-size (vt rows cols)
  (setf (vterm-rows vt) rows
        (vterm-columns vt) cols
        (vterm-damage vt) (make-array rows :element-type 'bit :initial-element 1)))

(defun make-vterm (&key (rows 24) (columns 80) on-output on-bell on-title on-scrollback)
  "Create a terminal of ROWS x COLUMNS.  UTF-8 input, the alternate screen
and full reset are enabled.  The ON-* closures may also be set later with
the SETFable accessors of the same name.  Free with VTERM-FREE (or use
WITH-VTERM)."
  (check-type rows (integer 1 #.(ash 1 20)))
  (check-type columns (integer 1 #.(ash 1 20)))
  (load-libvterm)
  (%install-hooks)
  (let* ((id (%allocate-id))
         (vt (%make-vterm :id id :on-output on-output :on-bell on-bell
                          :on-title on-title :on-scrollback on-scrollback)))
    (%note-size vt rows columns)
    ;; Register before clvt_new so callbacks fired by the reset find us.
    (setf (gethash id *registry*) vt)
    (let ((p (%clvt-new rows columns id)))
      (when (cffi:null-pointer-p p)
        (remhash id *registry*)
        (error "vterm_new(~D, ~D) failed" rows columns))
      (setf (vterm-pointer vt) p))
    (%signal-pending vt)
    vt))

(defun vterm-free (vt)
  "Release the C terminal.  Safe to call more than once."
  (let ((p (vterm-pointer vt)))
    (when p
      (setf (vterm-pointer vt) nil)
      (%vterm-free p)
      (remhash (vterm-id vt) *registry*)))
  nil)

(defmacro with-vterm ((var &rest args) &body body)
  "Bind VAR to (MAKE-VTERM ,@ARGS) around BODY, freeing it afterwards."
  `(let ((,var (make-vterm ,@args)))
     (unwind-protect (progn ,@body)
       (vterm-free ,var))))

;;; ------------------------------------------------------------------
;;; Input

(defun vterm-feed (vt data &key (start 0) end)
  "Feed bytes from the pty to the terminal.  DATA is an octet vector or a
string (encoded as UTF-8); START/END index into DATA.  Returns the number of
octets consumed."
  (let ((p (%ptr vt)) octets s e)
    (etypecase data
      (string (setf octets (babel:string-to-octets data :encoding :utf-8
                                                        :start start :end end)
                    s 0 e (length octets)))
      (octets (setf octets data s start e (or end (length data))))
      (vector (setf octets (coerce (subseq data start end) 'octets)
                    s 0 e (length octets))))
    (assert (<= 0 s e (length octets)))
    (prog1 (if (= s e)
               0
               (cffi:with-pointer-to-vector-data (ptr octets)
                 (%vterm-input-write p (cffi:inc-pointer ptr s) (- e s))))
      (%signal-pending vt))))

(defun vterm-resize (vt rows columns)
  "Resize the terminal; all rows become dirty."
  (check-type rows (integer 1))
  (check-type columns (integer 1))
  (%vterm-set-size (%ptr vt) rows columns)
  (%note-size vt rows columns)
  (%signal-pending vt)
  vt)

(defun %mods (mods)
  (etypecase mods
    (integer mods)
    (list (loop for m in mods
                sum (ecase m
                      (:shift +mod-shift+)
                      ((:alt :meta) +mod-alt+)
                      ((:ctrl :control) +mod-ctrl+))))))

(defun %key-code (key)
  (or (cdr (assoc key *key-codes*))
      (let ((name (symbol-name key)))
        (and (> (length name) 1) (char= (char name 0) #\F)
             (let ((n (parse-integer name :start 1 :junk-allowed t)))
               (and n (<= 0 n 255)
                    (= (length name) (1+ (length (princ-to-string n))))
                    (+ +key-function-0+ n)))))
      (error "Unknown key ~S" key)))

(defun vterm-key (vt key &key mods)
  "Send a special key (:up :down :left :right :home :end :page-up :page-down
:insert :delete :backspace :tab :enter :escape :f1..:f12 :kp-0...).  MODS is
a list of :shift :alt/:meta :ctrl/:control, or a VTermModifier integer.  The
encoded bytes are delivered to the ON-OUTPUT closure."
  (%vterm-keyboard-key (%ptr vt) (%key-code key) (%mods mods))
  (%signal-pending vt)
  nil)

(defun vterm-char (vt char &key mods)
  "Send a character (a CHARACTER or codepoint) with optional MODS; output
goes to ON-OUTPUT."
  (%vterm-keyboard-unichar (%ptr vt) (if (characterp char) (char-code char) char)
                           (%mods mods))
  (%signal-pending vt)
  nil)

(defun %capture (vt thunk)
  (let ((buf (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (old (vterm-capture vt)))
    (setf (vterm-capture vt) buf)
    (unwind-protect (funcall thunk)
      (setf (vterm-capture vt) old))
    (coerce buf 'octets)))

(defun vterm-encode-key (vt key &key mods)
  "Like VTERM-KEY but return the encoded octets instead of calling ON-OUTPUT.
The encoding depends on terminal modes (e.g. DECCKM)."
  (%capture vt (lambda () (vterm-key vt key :mods mods))))

(defun vterm-encode-char (vt char &key mods)
  "Like VTERM-CHAR but return the encoded octets instead of calling ON-OUTPUT."
  (%capture vt (lambda () (vterm-char vt char :mods mods))))

(defun vterm-paste-start (vt)
  "Emit the bracketed-paste start sequence if the application enabled it."
  (%vterm-keyboard-start-paste (%ptr vt)) nil)

(defun vterm-paste-end (vt)
  (%vterm-keyboard-end-paste (%ptr vt)) nil)

;;; ------------------------------------------------------------------
;;; State queries

(defun vterm-cursor (vt)
  "Return (values row col visible-p)."
  (cffi:with-foreign-objects ((r :int) (c :int))
    (%clvt-get-cursor (%ptr vt) r c)
    (values (cffi:mem-ref r :int) (cffi:mem-ref c :int) (vterm-cursor-visible vt))))

(defun vterm-altscreen-p (vt)
  (getf (vterm-properties vt) :altscreen))

(defun vterm-property (vt key)
  "Last value libvterm reported for a terminal property: :cursor-blink
:altscreen :reverse :focus-report (booleans), :cursor-shape (1 block,
2 underline, 3 bar), :mouse (0 none, 1 click, 2 drag, 3 move)."
  (getf (vterm-properties vt) key))

;;; ------------------------------------------------------------------
;;; Screen contents

(defun make-row-buffer (columns)
  "A (simple-array (unsigned-byte 32) (COLUMNS)) suitable for VTERM-ROW-INTO."
  (make-array columns :element-type '(unsigned-byte 32) :initial-element 0))

(defun %check-bounds (vt row col)
  (unless (and (< -1 row (vterm-rows vt)) (< -1 col (vterm-columns vt)))
    (error "Cell (~D, ~D) outside ~Dx~D terminal" row col (vterm-rows vt) (vterm-columns vt))))

(defun vterm-cell (vt row col)
  "Return a plist (:codepoints (cp ...) :width w :fg color :bg color
:attrs (kw ...)).  Colours are (:default) | (:palette n) | (:rgb r g b).
The right half of a wide character has :width 0 and no codepoints."
  (%check-bounds vt row col)
  (cffi:with-foreign-object (cell '(:struct clvt-cell))
    (when (zerop (%clvt-get-cell (%ptr vt) row col cell))
      (error "vterm_screen_get_cell failed at (~D, ~D)" row col))
    (let* ((chars (cffi:foreign-slot-pointer cell '(:struct clvt-cell) 'chars))
           (attrs (cffi:foreign-slot-value cell '(:struct clvt-cell) 'attrs)))
      (list :codepoints (loop for i below +max-chars-per-cell+
                              for c = (cffi:mem-aref chars :uint32 i)
                              until (zerop c) collect c)
            :width (attrs-width attrs)
            :fg (decode-color (cffi:foreign-slot-value cell '(:struct clvt-cell) 'fg))
            :bg (decode-color (cffi:foreign-slot-value cell '(:struct clvt-cell) 'bg))
            :attrs (decode-attrs attrs)))))

(defmacro %with-u32-pointers (bindings &body body)
  "Each binding (ptr array offset): pin ARRAY (or use NULL if it is NIL) and
bind PTR to the address of element OFFSET."
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind ((ptr array offset) &rest more) bindings
        (let ((arr (gensym)) (k (gensym)) (raw (gensym)))
          `(let ((,arr ,array))
             (flet ((,k (,ptr) (%with-u32-pointers ,more ,@body)))
               (declare (dynamic-extent #',k))
               (if ,arr
                   (cffi:with-pointer-to-vector-data (,raw ,arr)
                     (,k (cffi:inc-pointer ,raw (* 4 ,offset))))
                   (,k (cffi:null-pointer)))))))))

(defun vterm-row-into (vt row codes fg bg attrs &key (start 0) end)
  "Fast path: fill caller-provided (simple-array (unsigned-byte 32) (*))
arrays with columns [START, END) of ROW; element I describes column I, so
arrays need at least END elements.  Any array may be NIL to skip it.
  CODES: base codepoint (0 = empty cell or right half of a wide char).
  FG/BG: 0 default | #x01000000+index palette | #x02000000+#xRRGGBB rgb.
  ATTRS: bit 0 bold, 1 italic, 2-3 underline (1 single 2 double 3 curly),
         4 blink, 5 inverse, 6 conceal, 7 strike, 8 has combining chars,
         9-10 width (1, 2, or 0 for a wide char's right half).
Returns the number of cells written."
  (let ((end (or end (vterm-columns vt))))
    (assert (<= 0 start end (vterm-columns vt)))
    (assert (< -1 row (vterm-rows vt)))
    (dolist (a (list codes fg bg attrs))
      (when a
        (check-type a u32-vector)
        (assert (<= end (length a)) () "Row buffer of length ~D shorter than ~D" (length a) end)))
    (let ((p (%ptr vt)))
      (%with-u32-pointers ((pc codes start) (pf fg start) (pb bg start) (pa attrs start))
        (%clvt-get-row p row start end pc pf pb pa)))))

(defun %text (vt sr er sc ec)
  (let* ((len (* (- er sr) (1+ (* (- ec sc) +max-chars-per-cell+ 4))))
         (buf (make-array (max len 1) :element-type '(unsigned-byte 8))))
    (let ((n (cffi:with-pointer-to-vector-data (p buf)
               (%clvt-get-text (%ptr vt) p len sr er sc ec))))
      (babel:octets-to-string buf :end (min n len) :encoding :utf-8 :errorp nil))))

(defun vterm-row-text (vt row &key (start 0) end)
  "Text of ROW (columns START..END) with trailing blanks removed."
  (assert (< -1 row (vterm-rows vt)))
  (%text vt row (1+ row) start (or end (vterm-columns vt))))

(defun vterm-screen-text (vt)
  "Whole screen as one string, rows separated by newlines (trailing blanks
of each row removed)."
  (with-output-to-string (s)
    (dotimes (r (vterm-rows vt))
      (when (plusp r) (terpri s))
      (write-string (vterm-row-text vt r) s))))

;;; ------------------------------------------------------------------
;;; Damage

(defun vterm-take-damage (vt)
  "Return a fresh bit-vector (one bit per row, 1 = changed since the last
call) and clear the terminal's damage.  Second value: true if any row is
dirty.  Damage, scroll/moverect, cursor movement and visibility changes,
resizes and alt-screen switches all mark rows."
  (let ((p (vterm-pointer vt)))
    (when p (%vterm-screen-flush-damage (%vterm-obtain-screen p))))
  (let* ((bv (vterm-damage vt))
         (copy (copy-seq bv))
         (any (find 1 bv)))
    (fill bv 0)
    (values copy (and any t))))

(defun vterm-mark-all-dirty (vt)
  (fill (vterm-damage vt) 1)
  vt)
