;;;; Input checks for the wayland frontend, loaded by t/window-snapshot.lisp.
;;;; Weston's headless backend has no keyboard or pointer, so the checks
;;;; hand the frontend the events a seat would send: keys through the
;;;; default keymap (xkbcommon's, "us"), and a click.

(in-package #:slt)

;;; Keys, as linux/input-event-codes.h numbers them.
(defconstant +test-key-a+ 30)
(defconstant +test-key-enter+ 28)
(defconstant +test-key-space+ 57)
(defconstant +test-key-f1+ 59)

(defparameter +test-serial+ 1 "The serial the made-up events carry.")
(defparameter +test-time+ 0 "The time the made-up events carry.")

(defun wayland-test-keys (frontend view keys)
  "The keypress callback's arguments for each of KEYS: (CODE STATE
MODIFIERS), MODIFIERS the depressed xkbcommon modifiers' names."
  (let ((events '())
        (keymap (wayland-keymap frontend)))
    (setf (wayland-view-keypress-callback view)
          (lambda (&rest arguments) (push arguments events)))
    (loop for (code state modifiers) in keys
          do (wayland-keyboard-event
              frontend :modifiers
              (list +test-serial+
                    (loop for name in modifiers
                          sum (ash 1 (xkb:xkb-keymap-mod-get-index keymap name)))
                    0 0 0))
             (wayland-keyboard-event frontend :key (list +test-serial+ +test-time+ code state)))
    (nreverse events)))

(defun check-wayland-keyboard (frontend view)
  (let ((callback (wayland-view-keypress-callback view))
        (context (xkb:xkb-context-new :no-flags))
        (pressed wl:+wl-keyboard-key-state-pressed+)
        (released wl:+wl-keyboard-key-state-released+))
    (unwind-protect
         (let ((keymap (xkb:xkb-keymap-new-from-names context (cffi:null-pointer) :no-flags)))
           (setf (wayland-keymap frontend) keymap
                 (wayland-xkb-state frontend) (xkb:xkb-state-new keymap)
                 (wayland-keyboard-view frontend) view)
           (let ((events (wayland-test-keys
                          frontend view
                          `((,+test-key-a+ ,pressed ()) (,+test-key-a+ ,released ())
                            (,+test-key-a+ ,pressed ("Shift"))
                            (,+test-key-a+ ,pressed ("Control"))
                            (,+test-key-enter+ ,pressed ()) (,+test-key-space+ ,pressed ())
                            (,+test-key-f1+ ,pressed ())))))
             (check (equal events
                           `(("a" "a" 0 :press) ("a" "a" 0 :release)
                             ("A" "A" ,+shift-mask+ :press) ("a" "a" ,+control-mask+ :press)
                             ("" "Return" 0 :press) (" " "space" 0 :press)
                             ("" "F1" 0 :press)))
                    "keys arrive as text, X11 keysym names and modifier bits")))
      (setf (wayland-view-keypress-callback view) callback
            (wayland-keyboard-view frontend) nil)
      (wayland-free-xkb-state frontend)
      (xkb:xkb-context-unref context))))

(defun check-wayland-pointer (frontend view)
  (let ((callback (wayland-view-mouse-callback view))
        (scale (wayland-view-scale view))
        (cw (wayland-view-cell-width view))
        (ch (wayland-view-cell-height view))
        (events '()))
    (setf (wayland-view-mouse-callback view)
          (lambda (&rest arguments) (push (subseq arguments 0 5) events)))
    (unwind-protect
         (progn
           ;; Into the middle of row 1, column 2, and click.
           (wayland-pointer-event frontend :enter
                                  (list +test-serial+ (wayland-view-surface view)
                                        (/ (* 5/2 cw) scale) (/ (* 3/2 ch) scale)))
           (wayland-pointer-event frontend :button
                                  (list +test-serial+ +test-time+ +evdev-button-left+
                                        wl:+wl-pointer-button-state-pressed+))
           (wayland-pointer-event frontend :button
                                  (list +test-serial+ +test-time+ +evdev-button-left+
                                        wl:+wl-pointer-button-state-released+))
           (check (equal (nreverse events) '((:press 1 2 1 1) (:release 1 2 1 1)))
                  "a click reaches the cell under the pointer"))
      (setf (wayland-view-mouse-callback view) callback
            (wayland-pointer-view frontend) nil))))

(defun check-wayland-input (frontend view)
  (check-wayland-keyboard frontend view)
  (check-wayland-pointer frontend view))
