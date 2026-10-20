;;;; ffi.lisp -- raw foreign declarations for libvterm and the shim.
;;;;
;;;; Only functions whose arguments are scalars or pointers are called
;;;; directly; anything involving by-value structs, bitfields or the
;;;; VTermColor union goes through src/shim.c.

(in-package #:libvterm)

;;; ------------------------------------------------------------------
;;; Constants (mirroring vterm.h / vterm_keycodes.h of 0.3.x)

(defconstant +max-chars-per-cell+ 6)

(defconstant +prop-cursorvisible+ 1)
(defconstant +prop-cursorblink+ 2)
(defconstant +prop-altscreen+ 3)
(defconstant +prop-title+ 4)
(defconstant +prop-iconname+ 5)
(defconstant +prop-reverse+ 6)
(defconstant +prop-cursorshape+ 7)
(defconstant +prop-mouse+ 8)
(defconstant +prop-focusreport+ 9)

(defconstant +mod-shift+ 1)
(defconstant +mod-alt+ 2)
(defconstant +mod-ctrl+ 4)

(defconstant +key-function-0+ 256)

(defparameter *key-codes*
  '((:enter . 1) (:return . 1) (:tab . 2) (:backspace . 3) (:escape . 4)
    (:up . 5) (:down . 6) (:left . 7) (:right . 8)
    (:insert . 9) (:delete . 10) (:home . 11) (:end . 12)
    (:page-up . 13) (:page-down . 14)
    (:kp-0 . 512) (:kp-1 . 513) (:kp-2 . 514) (:kp-3 . 515) (:kp-4 . 516)
    (:kp-5 . 517) (:kp-6 . 518) (:kp-7 . 519) (:kp-8 . 520) (:kp-9 . 521)
    (:kp-mult . 522) (:kp-plus . 523) (:kp-comma . 524) (:kp-minus . 525)
    (:kp-period . 526) (:kp-divide . 527) (:kp-enter . 528) (:kp-equal . 529))
  "Keyword -> VTermKey.  :f1..:f63 are computed as VTERM_KEY_FUNCTION(n).")

;;; ------------------------------------------------------------------
;;; Shim structs

(cffi:defcstruct (clvt-hooks :class clvt-hooks)
  (damage :pointer)
  (moverect :pointer)
  (movecursor :pointer)
  (prop-int :pointer)
  (prop-string :pointer)
  (bell :pointer)
  (resize :pointer)
  (sb-pushline :pointer)
  (output :pointer))

(cffi:defcstruct (clvt-cell :class clvt-cell)
  (chars :uint32 :count 6)
  (fg :uint32)
  (bg :uint32)
  (attrs :uint32))

;;; ------------------------------------------------------------------
;;; libvterm proper

(cffi:defcfun ("vterm_free" %vterm-free) :void (vt :pointer))
(cffi:defcfun ("vterm_set_size" %vterm-set-size) :void (vt :pointer) (rows :int) (cols :int))
(cffi:defcfun ("vterm_get_size" %vterm-get-size) :void
  (vt :pointer) (rowsp :pointer) (colsp :pointer))
(cffi:defcfun ("vterm_input_write" %vterm-input-write) :size
  (vt :pointer) (bytes :pointer) (len :size))
(cffi:defcfun ("vterm_keyboard_unichar" %vterm-keyboard-unichar) :void
  (vt :pointer) (c :uint32) (mod :int))
(cffi:defcfun ("vterm_keyboard_key" %vterm-keyboard-key) :void
  (vt :pointer) (key :int) (mod :int))
(cffi:defcfun ("vterm_mouse_move" %vterm-mouse-move) :void
  (vt :pointer) (row :int) (col :int) (mod :int))
(cffi:defcfun ("vterm_mouse_button" %vterm-mouse-button) :void
  (vt :pointer) (button :int) (pressed :bool) (mod :int))
(cffi:defcfun ("vterm_keyboard_start_paste" %vterm-keyboard-start-paste) :void (vt :pointer))
(cffi:defcfun ("vterm_keyboard_end_paste" %vterm-keyboard-end-paste) :void (vt :pointer))
(cffi:defcfun ("vterm_obtain_screen" %vterm-obtain-screen) :pointer (vt :pointer))
(cffi:defcfun ("vterm_screen_flush_damage" %vterm-screen-flush-damage) :void (screen :pointer))

;;; ------------------------------------------------------------------
;;; Shim

(cffi:defcfun ("clvt_abi_version" %clvt-abi-version) :int)
(cffi:defcfun ("clvt_header_version" %clvt-header-version) :int)
(cffi:defcfun ("clvt_sizeof" %clvt-sizeof) :int (which :int))
(cffi:defcfun ("clvt_set_hooks" %clvt-set-hooks) :void (hooks :pointer))
(cffi:defcfun ("clvt_new" %clvt-new) :pointer (rows :int) (cols :int) (id :int))
(cffi:defcfun ("clvt_get_cursor" %clvt-get-cursor) :void
  (vt :pointer) (row :pointer) (col :pointer))
(cffi:defcfun ("clvt_get_cell" %clvt-get-cell) :int
  (vt :pointer) (row :int) (col :int) (out :pointer))
(cffi:defcfun ("clvt_get_row" %clvt-get-row) :int
  (vt :pointer) (row :int) (start-col :int) (end-col :int)
  (codes :pointer) (fg :pointer) (bg :pointer) (attrs :pointer))
(cffi:defcfun ("clvt_pack_cells" %clvt-pack-cells) :void
  (cells :pointer) (n :int)
  (codes :pointer) (fg :pointer) (bg :pointer) (attrs :pointer))
(cffi:defcfun ("clvt_get_text" %clvt-get-text) :size
  (vt :pointer) (buf :pointer) (len :size)
  (start-row :int) (end-row :int) (start-col :int) (end-col :int))

;;; ------------------------------------------------------------------
;;; Load-time ABI sanity check: the shim reports the sizes it was compiled
;;; with; they must match what this file assumes.

(defun check-abi ()
  (flet ((want (what got expected)
           (unless (eql got expected)
             (error "libvterm ABI mismatch: ~A is ~A, binding expects ~A. ~
                     Rebuild with scripts/build-libvterm.sh --force."
                    what got expected))))
    (want "shim ABI version" (%clvt-abi-version) 1)
    (want "vterm.h version (major*100+minor)" (%clvt-header-version) 3)
    (want "sizeof(VTermScreenCell)" (%clvt-sizeof 0) 40)
    (want "sizeof(clvt_cell)" (%clvt-sizeof 1) (cffi:foreign-type-size '(:struct clvt-cell)))
    (want "sizeof(VTermPos)" (%clvt-sizeof 2) 8)
    (want "sizeof(VTermRect)" (%clvt-sizeof 3) 16)
    (want "sizeof(clvt_hooks)" (%clvt-sizeof 4) (cffi:foreign-type-size '(:struct clvt-hooks)))
    (want "VTERM_MAX_CHARS_PER_CELL" (%clvt-sizeof 5) +max-chars-per-cell+))
  t)

(check-abi)
