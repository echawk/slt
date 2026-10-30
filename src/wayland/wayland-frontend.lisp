(in-package #:slt)

;;;; The wayland frontend: xdg-shell windows drawn with OpenGL (through EGL)
;;;; or Vulkan, as *GL-API* says, without GLFW.  libwayland-client is reached
;;;; through wayland-cffi; xkbcommon (cl-xkb) turns key codes into keysyms
;;;; and text with the compositor's keymap; the clipboard and dropped files
;;;; come through wl_data_device.  The sizes the emulator sees are buffer
;;;; pixels: a window's size in surface units times its scale.
;;;;
;;;; All Wayland requests come from the event loop's thread.  Other threads
;;;; wake it through a pipe it polls along with the compositor's socket.

(defvar *wayland-scroll-rows* 3
  "Rows scrolled per wheel step.")

(defvar *wayland-multi-click-seconds* 0.4
  "The longest gap between clicks of a double or triple click.")

(defparameter +wayland-app-id+ "slt"
  "The xdg-shell application id, which the desktop file is matched by.")

(defparameter +wayland-interface-versions+
  '(("wl_compositor" . 6) ("xdg_wm_base" . 6) ("wl_seat" . 7) ("wl_output" . 2)
    ("wl_data_device_manager" . 3) ("zxdg_decoration_manager_v1" . 1)
    ("wp_cursor_shape_manager_v1" . 1))
  "The globals the frontend binds, each at most at the version whose events
it handles: wl_compositor 6 sends the preferred buffer scale, xdg_wm_base
6 the suspended state, wl_output 2 the scale.")

(defparameter +wayland-text-mime-types+
  '("text/plain;charset=utf-8" "UTF8_STRING" "text/plain")
  "The types the clipboard's text is offered and asked for as, in order.")

(defparameter +wayland-uri-list-mime-type+ "text/uri-list"
  "The type dropped files arrive as.")

(defconstant +xkb-keycode-offset+ 8
  "xkbcommon's keycodes are the evdev codes Wayland sends plus 8.")

(defconstant +evdev-button-left+ #x110 "BTN_LEFT in linux/input-event-codes.h.")
(defconstant +evdev-button-right+ #x111)
(defconstant +evdev-button-middle+ #x112)

(defparameter +wayland-buttons+
  (list (cons +evdev-button-left+ 1) (cons +evdev-button-middle+ 2)
        (cons +evdev-button-right+ 3))
  "Each evdev button and the number the mouse callback takes for it.")

(defconstant +wayland-axis-step+ 10
  "The axis value of one wheel step, as compositors send it.")

(defconstant +keysym-function-start+ #xfe00
  "Keysyms from here to +KEYSYM-FUNCTION-END+ are keys with names rather
than characters: cursor and function keys, the keypad, modifiers and dead
keys.  The terminal sees them by their X11 names.")
(defconstant +keysym-function-end+ #xffff)

(defparameter +xkb-modifier-masks+
  (list (cons "Shift" +shift-mask+) (cons "Control" +control-mask+)
        (cons "Mod1" +mod1-mask+) (cons "Mod4" +mod4-mask+))
  "The xkbcommon modifiers (XKB_MOD_NAME_SHIFT, _CTRL, _ALT, _LOGO) and
the X11 state bits keypresses report them as.")

(defparameter +locale-variables+ '("LC_ALL" "LC_CTYPE" "LANG")
  "Where the locale whose compose sequences (dead keys) apply is found.")

(defparameter +default-locale+ "C")

(defconstant +pollin+ 1 "poll(2)'s POLLIN: there is data to read.")
(defconstant +poll-forever+ -1 "poll(2)'s timeout to wait with no limit.")

(defconstant +pipe-chunk-size+ 4096
  "The octets read from a pipe at a time.")

(cffi:defcstruct pollfd
  (fd :int)
  (events :short)
  (revents :short))

(defclass wayland-frontend (terminal-frontend)
  ((display :initform nil :accessor wayland-display)
   (registry :initform nil :accessor wayland-registry)
   ;; The globals the compositor announced: (NAME INTERFACE VERSION) each.
   (globals :initform '() :accessor wayland-globals)
   ;; The bound globals, by interface name.
   (bound :initform (make-hash-table :test 'equal) :reader wayland-bound)
   ;; Each output's scale, by the output's address.
   (output-scales :initform (make-hash-table) :reader wayland-output-scales)
   (keyboard :initform nil :accessor wayland-keyboard)
   (pointer :initform nil :accessor wayland-pointer)
   (cursor-shape-device :initform nil :accessor wayland-cursor-shape-device)
   (data-device :initform nil :accessor wayland-data-device)
   (xkb-context :initform nil :accessor wayland-xkb-context)
   (keymap :initform nil :accessor wayland-keymap)
   (xkb-state :initform nil :accessor wayland-xkb-state)
   (compose-table :initform nil :accessor wayland-compose-table)
   (compose-state :initform nil :accessor wayland-compose-state)
   ;; Keys repeat RATE times a second after DELAY milliseconds held.
   (repeat-rate :initform 0 :accessor wayland-repeat-rate)
   (repeat-delay :initform 0 :accessor wayland-repeat-delay)
   ;; The key repeating and its next repeat's timer.
   (repeat-code :initform nil :accessor wayland-repeat-code)
   (repeat-timer :initform nil :accessor wayland-repeat-timer)
   ;; The views with the keyboard focus and under the pointer.
   (keyboard-view :initform nil :accessor wayland-keyboard-view)
   (pointer-view :initform nil :accessor wayland-pointer-view)
   ;; The pointer's position in surface units.
   (pointer-x :initform 0 :accessor wayland-pointer-x)
   (pointer-y :initform 0 :accessor wayland-pointer-y)
   ;; The serial of the last input event, which setting the selection needs.
   (serial :initform 0 :accessor wayland-serial)
   ;; Each data offer's MIME types, by the offer's address.
   (offer-types :initform (make-hash-table) :reader wayland-offer-types)
   (selection-offer :initform nil :accessor wayland-selection-offer)
   (drag-offer :initform nil :accessor wayland-drag-offer)
   (drag-view :initform nil :accessor wayland-drag-view)
   ;; The clipboard's text while this client owns it, and its source.
   (data-source :initform nil :accessor wayland-data-source)
   (clipboard :initform nil :accessor wayland-clipboard)
   (views :initform '() :accessor wayland-views)
   (timers :initform '() :accessor wayland-timers)
   (wakeups :initform '() :accessor wayland-wakeups)
   (lock :initform (sb-thread:make-mutex :name "wayland frontend") :reader wayland-lock)
   ;; The pipe other threads wake the event loop through.
   (wake-read :initform nil :accessor wayland-wake-read)
   (wake-write :initform nil :accessor wayland-wake-write)
   (exit-p :initform nil :accessor wayland-exit-p)))

(defstruct (wayland-view (:constructor %make-wayland-view))
  frontend
  surface
  xdg-surface
  toplevel
  decoration
  egl-window
  renderer
  host
  font
  ;; (FAMILY SIZE CELL-WIDTH CELL-HEIGHT), to rasterize again at another scale.
  font-options
  atlas
  instances
  (cell-width 1)
  (cell-height 1)
  ;; The window's size in surface units, and its buffer's scale.
  (width 0)
  (height 0)
  (scale 1)
  ;; The size the compositor configured, applied when it is acknowledged.
  (pending-width nil)
  (pending-height nil)
  ;; The outputs the surface is on, for their scale, unless the compositor
  ;; tells the scale itself (PREFERRED-SCALE-P).
  (outputs '())
  (preferred-scale-p nil)
  (configured-p nil)
  ;; Hidden, so frames are not drawn (they would wait for a refresh that
  ;; does not come).
  (suspended-p nil)
  grid
  image-store
  keypress-callback
  resize-callback
  scroll-callback
  (scroll-remainder 0)
  mouse-callback
  paste-callback
  focus-callback
  drop-callback
  close-callback
  (button nil)
  (last-click nil)
  (click-count 1)
  (closed-p nil))

(defun wayland-libraries-available-p ()
  "True when libwayland-client and the glyph rasterizer's libraries load."
  (and (ignore-errors (wl:load-wayland))
       (glyph-libraries-available-p)))

;;; Connecting and the globals

(defun wayland-bound-global (frontend interface)
  (gethash interface (wayland-bound frontend)))

(defun wayland-bind (frontend name interface version)
  "Bind the global NAME, an INTERFACE at VERSION, at most the version the
frontend handles."
  (wl:wl-registry-bind (wayland-registry frontend) name interface
                       (min version (cdr (assoc interface +wayland-interface-versions+
                                                :test #'string=)))))

(defun wayland-roundtrip (frontend)
  (let ((display (wayland-display frontend)))
    (wl:check-display (wl:display-roundtrip display) display "wl_display_roundtrip")))

(defun wayland-ensure-display (frontend)
  "Connect to the compositor and bind its globals, unless that is done."
  (or (wayland-display frontend)
      (progn
        (wl:load-wayland)
        (let ((display (or (wl:display-connect)
                           (error "No Wayland compositor at ~a"
                                  (or (uiop:getenv "WAYLAND_DISPLAY") "wayland-0")))))
          (setf (wayland-display frontend) display
                (wayland-registry frontend) (wl:wl-display-get-registry display))
          (wl:set-listener (wayland-registry frontend)
                           (lambda (event &rest arguments)
                             (wayland-registry-event frontend event arguments)))
          (wayland-roundtrip frontend)
          (wayland-bind-globals frontend)
          ;; The seat's capabilities, the keymap and the outputs' scales.
          (wayland-roundtrip frontend)
          (wayland-make-wake-pipe frontend)
          display))))

(defun wayland-registry-event (frontend event arguments)
  (case event
    (:global
     (destructuring-bind (name interface version) arguments
       (push (list name interface version) (wayland-globals frontend))
       ;; Outputs come and go after the first roundtrip.
       (when (and (string= interface "wl_output") (wayland-bound-global frontend "wl_compositor"))
         (wayland-bind-output frontend name version))))
    (:global-remove
     (let ((name (first arguments)))
       (setf (wayland-globals frontend) (remove name (wayland-globals frontend) :key #'first))))))

(defun wayland-bind-globals (frontend)
  (loop for (name interface version) in (reverse (wayland-globals frontend))
        do (cond ((string= interface "wl_output") (wayland-bind-output frontend name version))
                 ((and (assoc interface +wayland-interface-versions+ :test #'string=)
                       (not (wayland-bound-global frontend interface)))
                  (setf (gethash interface (wayland-bound frontend))
                        (wayland-bind frontend name interface version)))))
  (unless (and (wayland-bound-global frontend "wl_compositor")
               (wayland-bound-global frontend "xdg_wm_base"))
    (error "The Wayland compositor has no wl_compositor or xdg_wm_base"))
  (let ((wm-base (wayland-bound-global frontend "xdg_wm_base")))
    (wl:set-listener wm-base (lambda (event &rest arguments)
                               (when (eq event :ping)
                                 (wl:xdg-wm-base-pong wm-base (first arguments))))))
  (let ((seat (wayland-bound-global frontend "wl_seat")))
    (when seat
      (wl:set-listener seat (lambda (event &rest arguments)
                              (when (eq event :capabilities)
                                (wayland-seat-capabilities frontend (first arguments)))))
      (let ((manager (wayland-bound-global frontend "wl_data_device_manager")))
        (when manager
          (let ((device (wl:wl-data-device-manager-get-data-device manager seat)))
            (setf (wayland-data-device frontend) device)
            (wl:set-listener device (lambda (event &rest arguments)
                                      (wayland-data-device-event frontend event arguments)))))))))

(defun wayland-bind-output (frontend name version)
  (let ((output (wayland-bind frontend name "wl_output" version)))
    (setf (gethash (cffi:pointer-address output) (wayland-output-scales frontend)) 1)
    (wl:set-listener output
                     (lambda (event &rest arguments)
                       (when (eq event :scale)
                         (setf (gethash (cffi:pointer-address output)
                                        (wayland-output-scales frontend))
                               (first arguments))
                         (dolist (view (wayland-views frontend))
                           (wayland-update-output-scale view)))))))

(defun wayland-seat-capabilities (frontend capabilities)
  (when (and (logtest capabilities wl:+wl-seat-capability-keyboard+)
             (not (wayland-keyboard frontend)))
    (let ((keyboard (wl:wl-seat-get-keyboard (wayland-bound-global frontend "wl_seat"))))
      (setf (wayland-keyboard frontend) keyboard)
      (wl:set-listener keyboard (lambda (event &rest arguments)
                                  (wayland-keyboard-event frontend event arguments)))))
  (when (and (logtest capabilities wl:+wl-seat-capability-pointer+)
             (not (wayland-pointer frontend)))
    (let ((pointer (wl:wl-seat-get-pointer (wayland-bound-global frontend "wl_seat")))
          (shapes (wayland-bound-global frontend "wp_cursor_shape_manager_v1")))
      (setf (wayland-pointer frontend) pointer)
      (when shapes
        (setf (wayland-cursor-shape-device frontend)
              (wl:wp-cursor-shape-manager-v1-get-pointer shapes pointer)))
      (wl:set-listener pointer (lambda (event &rest arguments)
                                 (wayland-pointer-event frontend event arguments))))))

(defun wayland-find-view (frontend surface)
  (and surface
       (find (cffi:pointer-address surface) (wayland-views frontend)
             :key (lambda (view) (cffi:pointer-address (wayland-view-surface view))))))

;;; Fonts: slt/glyphs, at the window's scale.

(defun wayland-view-set-font (view scale)
  "Open the view's font for SCALE and size its cells and atlas."
  (destructuring-bind (family size cell-width cell-height) (wayland-view-font-options view)
    (let ((font (open-glyph-font family (* size scale))))
      (when (wayland-view-font view)
        (close-glyph-font (wayland-view-font view)))
      (multiple-value-bind (font-width font-height) (glyph-font-cell-size font)
        (let ((cw (or (and cell-width (round (* cell-width scale))) font-width))
              (ch (or (and cell-height (round (* cell-height scale))) font-height)))
          (setf (wayland-view-font view) font
                (wayland-view-scale view) scale
                (wayland-view-cell-width view) cw
                (wayland-view-cell-height view) ch
                (wayland-view-atlas view) (make-glyph-atlas cw ch (glyph-font-rasterizer font)
                                                           :shaper (glyph-font-shaper font))
                (wayland-view-instances view) nil))))))

;;; Drawing

(defun wayland-view-pixel-size (view)
  "The window's size in buffer pixels."
  (values (max 1 (* (wayland-view-width view) (wayland-view-scale view)))
          (max 1 (* (wayland-view-height view) (wayland-view-scale view)))))

(defun wayland-frame (view palette)
  "VIEW's grid packed, as a frame for its renderer."
  (let ((grid (wayland-view-grid view)))
    (setf (wayland-view-instances view)
          (pack-grid grid (wayland-view-atlas view) palette (wayland-view-instances view)))
    (multiple-value-bind (width height) (wayland-view-pixel-size view)
      (list (wayland-view-instances view)
            (grid-columns grid)
            (* (grid-rows grid) (grid-columns grid))
            (wayland-view-cell-width view)
            (wayland-view-cell-height view)
            width height
            (wayland-view-atlas view)
            palette
            (and (wayland-view-image-store view)
                 (terminal-image-render-list (wayland-view-image-store view)
                                             (wayland-view-cell-width view)
                                             (wayland-view-cell-height view)))))))

(defun wayland-redraw (view)
  (when (and (wayland-view-renderer view) (wayland-view-grid view)
             (not (wayland-view-closed-p view)) (not (wayland-view-suspended-p view)))
    (with-view-palette (view)
      (renderer-present (wayland-view-renderer view) (wayland-view-host view)
                        (wayland-frame view *palette*)))))

(defun wayland-vulkan-surface (instance display surface)
  (vulkan-receive (vulkan-surface vk:surface-khr)
    (vk:with-struct (info vk:wayland-surface-create-info-khr :display display :surface surface)
      (vulkan-call (vk:create-wayland-surface-khr instance info (cffi:null-pointer)
                                                  vulkan-surface)))))

(defun wayland-make-renderer (view)
  "Make the window's buffer and the renderer drawing into it."
  (let ((display (wayland-display (wayland-view-frontend view)))
        (surface (wayland-view-surface view)))
    (multiple-value-bind (width height) (wayland-view-pixel-size view)
      (unless (eq *gl-api* :vulkan)
        (wl:load-wayland-egl)
        (setf (wayland-view-egl-window view) (wl:egl-window-create surface width height)))
      (multiple-value-bind (renderer host)
          (make-window-renderer
           :egl-platform egl:+platform-wayland-khr+
           :native-display display
           :native-window (wayland-view-egl-window view)
           :vulkan-extensions (list vk:+khr-wayland-surface-extension-name+)
           :vulkan-surface (lambda (instance)
                             (wayland-vulkan-surface instance display surface)))
        (setf (wayland-view-renderer view) renderer
              (wayland-view-host view) host)))))

(defun wayland-resize-buffer (view)
  "Size the window's buffer to its size and scale."
  (wl:wl-surface-set-buffer-scale (wayland-view-surface view) (wayland-view-scale view))
  (when (wayland-view-egl-window view)
    (multiple-value-bind (width height) (wayland-view-pixel-size view)
      (wl:egl-window-resize (wayland-view-egl-window view) width height 0 0))))

(defun wayland-view-changed-size (view)
  "The window's size or scale changed: fit the buffer and the grid."
  (wayland-resize-buffer view)
  (when (wayland-view-resize-callback view)
    (funcall (wayland-view-resize-callback view) t))
  (wayland-redraw view))

(defun wayland-set-scale (view scale)
  (unless (= scale (wayland-view-scale view))
    (wayland-view-set-font view scale)
    (when (wayland-view-renderer view)
      (wayland-view-changed-size view))))

(defun wayland-update-output-scale (view)
  "Draw at the largest scale of the outputs VIEW is on, unless the
compositor says which."
  (unless (wayland-view-preferred-scale-p view)
    (let ((scales (wayland-output-scales (wayland-view-frontend view))))
      (wayland-set-scale view (reduce #'max (wayland-view-outputs view)
                                      :key (lambda (output) (gethash output scales 1))
                                      :initial-value 1)))))

;;; The window's events

(defun octets-uint32s (octets)
  "The uint32s of a Wayland array, OCTETS."
  (cffi:with-pointer-to-vector-data (pointer octets)
    (loop for index below (floor (length octets) (cffi:foreign-type-size :uint32))
          collect (cffi:mem-aref pointer :uint32 index))))

(defun wayland-surface-event (view event arguments)
  (case event
    (:enter (pushnew (cffi:pointer-address (first arguments)) (wayland-view-outputs view))
     (wayland-update-output-scale view))
    (:leave (setf (wayland-view-outputs view)
                  (remove (cffi:pointer-address (first arguments)) (wayland-view-outputs view)))
     (wayland-update-output-scale view))
    (:preferred-buffer-scale
     (setf (wayland-view-preferred-scale-p view) t)
     (wayland-set-scale view (first arguments)))))

(defun wayland-toplevel-event (view event arguments)
  (case event
    (:configure
     (destructuring-bind (width height states) arguments
       ;; Zero leaves the size to the client.
       (setf (wayland-view-pending-width view) (and (plusp width) width)
             (wayland-view-pending-height view) (and (plusp height) height))
       (let ((suspended (member wl:+xdg-toplevel-state-suspended+ (octets-uint32s states))))
         (when (and (wayland-view-suspended-p view) (not suspended))
           (setf (wayland-view-suspended-p view) nil)
           (wayland-redraw view))
         (setf (wayland-view-suspended-p view) (and suspended t)))))
    (:close
     (when (wayland-view-close-callback view)
       (funcall (wayland-view-close-callback view))))))

(defun wayland-xdg-surface-event (view event arguments)
  (when (eq event :configure)
    (wl:xdg-surface-ack-configure (wayland-view-xdg-surface view) (first arguments))
    (let ((width (or (wayland-view-pending-width view) (wayland-view-width view)))
          (height (or (wayland-view-pending-height view) (wayland-view-height view))))
      (setf (wayland-view-configured-p view) t)
      (unless (and (= width (wayland-view-width view)) (= height (wayland-view-height view)))
        (setf (wayland-view-width view) width
              (wayland-view-height view) height)
        (when (wayland-view-renderer view)
          (wayland-view-changed-size view))))))

;;; Keyboard

(defun wayland-locale ()
  (or (find-if (lambda (value) (and value (plusp (length value))))
               (mapcar #'uiop:getenv +locale-variables+))
      +default-locale+))

(defun wayland-ensure-xkb (frontend)
  (unless (wayland-xkb-context frontend)
    (let ((context (xkb:xkb-context-new :no-flags)))
      (when (cffi:null-pointer-p context)
        (error "xkbcommon has no context"))
      (setf (wayland-xkb-context frontend) context)
      (let ((table (xkb:xkb-compose-table-new-from-locale context (wayland-locale) :no-flags)))
        ;; Without a table for the locale, there are no compose sequences.
        (unless (cffi:null-pointer-p table)
          (setf (wayland-compose-table frontend) table
                (wayland-compose-state frontend) (xkb:xkb-compose-state-new table :no-flags)))))))

(defun wayland-free-xkb-state (frontend)
  (let ((state (shiftf (wayland-xkb-state frontend) nil))
        (keymap (shiftf (wayland-keymap frontend) nil)))
    (when state (xkb:xkb-state-unref state))
    (when keymap (xkb:xkb-keymap-unref keymap))))

(defun wayland-read-keymap (fd size)
  "The keymap text the compositor shares in FD, SIZE octets with a NUL."
  (let ((map (sb-posix:mmap nil size sb-posix:prot-read sb-posix:map-private fd 0)))
    (unwind-protect
         (let ((text (cffi:foreign-string-to-lisp map :count size)))
           (subseq text 0 (or (position (code-char 0) text) (length text))))
      (sb-posix:munmap map size))))

(defun wayland-set-keymap (frontend format fd size)
  (unwind-protect
       (when (= format wl:+wl-keyboard-keymap-format-xkb-v1+)
         (wayland-ensure-xkb frontend)
         (let ((keymap (xkb:xkb-keymap-new-from-string (wayland-xkb-context frontend)
                                                       (wayland-read-keymap fd size)
                                                       :text-v1 :no-flags)))
           (if (cffi:null-pointer-p keymap)
               (format *error-output* "~&slt: the compositor's keymap does not compile~%")
               (progn
                 (wayland-free-xkb-state frontend)
                 (setf (wayland-keymap frontend) keymap
                       (wayland-xkb-state frontend) (xkb:xkb-state-new keymap))))))
    (sb-posix:close fd)))

(defun wayland-modifier-state (frontend)
  "The modifiers held, as X11 state bits."
  (let ((state (wayland-xkb-state frontend)))
    (if state
        (loop for (name . mask) in +xkb-modifier-masks+
              when (plusp (xkb:xkb-state-mod-name-is-active state name :mods-effective))
                sum mask)
        0)))

(defun wayland-keysym-key (keysym)
  "(values TEXT NAME) for KEYSYM, as the keypress callback takes them: a
character's text and name are the character; a named key's text is empty."
  (let ((char (and (not (<= +keysym-function-start+ keysym +keysym-function-end+))
                   (let ((code (xkb:xkb-keysym-to-utf32 keysym)))
                     (and (plusp code) (code-char code))))))
    (if (and char (graphic-char-p char) (char/= char #\Space))
        (values (string char) (string char))
        (values (if (eql char #\Space) " " "") (xkb:xkb-keysym-get-name keysym)))))

(defun wayland-compose (frontend keysym)
  "KEYSYM, or what the compose sequence it ends gives, or NIL while a
sequence goes on or when it was cancelled."
  (let ((compose (wayland-compose-state frontend)))
    (if (or (null compose) (eq (xkb:xkb-compose-state-feed compose keysym) :ignored))
        keysym
        (ecase (xkb:xkb-compose-state-get-status compose)
          (:nothing keysym)
          (:composing nil)
          (:composed (prog1 (xkb:xkb-compose-state-get-one-sym compose)
                       (xkb:xkb-compose-state-reset compose)))
          (:cancelled (xkb:xkb-compose-state-reset compose) nil)))))

(defun wayland-deliver-key (frontend code kind)
  "Pass the press, repeat or release (KIND) of the xkbcommon keycode CODE
to the focused view."
  (let ((view (wayland-keyboard-view frontend))
        (state (wayland-xkb-state frontend)))
    (when (and view state (wayland-view-keypress-callback view))
      (let* ((keysym (xkb:xkb-state-key-get-one-sym state code))
             (keysym (if (eq kind :press) (wayland-compose frontend keysym) keysym)))
        (when (and keysym (plusp keysym))
          (multiple-value-bind (text name) (wayland-keysym-key keysym)
            (when name
              (funcall (wayland-view-keypress-callback view) text name
                       (wayland-modifier-state frontend) kind))))))))

(defun wayland-stop-repeat (frontend)
  (let ((timer (shiftf (wayland-repeat-timer frontend) nil)))
    (setf (wayland-repeat-code frontend) nil)
    (when timer
      (wayland-cancel-timer frontend timer))))

(defun wayland-start-repeat (frontend code)
  (wayland-stop-repeat frontend)
  (let ((rate (wayland-repeat-rate frontend))
        (keymap (wayland-keymap frontend)))
    (when (and (plusp rate) keymap (plusp (xkb:xkb-keymap-key-repeats keymap code)))
      (setf (wayland-repeat-code frontend) code)
      (labels ((repeat ()
                 (when (eql code (wayland-repeat-code frontend))
                   (wayland-deliver-key frontend code :repeat)
                   (setf (wayland-repeat-timer frontend)
                         (wayland-add-timer frontend (/ 1000 rate) #'repeat)))))
        (setf (wayland-repeat-timer frontend)
              (wayland-add-timer frontend (wayland-repeat-delay frontend) #'repeat))))))

(defun wayland-keyboard-event (frontend event arguments)
  (case event
    (:keymap (apply #'wayland-set-keymap frontend arguments))
    (:enter
     (destructuring-bind (serial surface keys) arguments
       (declare (ignore keys))
       (setf (wayland-serial frontend) serial)
       (let ((view (wayland-find-view frontend surface)))
         (setf (wayland-keyboard-view frontend) view)
         (when (and view (wayland-view-focus-callback view))
           (funcall (wayland-view-focus-callback view) t)))))
    (:leave
     (wayland-stop-repeat frontend)
     (let ((view (shiftf (wayland-keyboard-view frontend) nil)))
       (when (and view (wayland-view-focus-callback view))
         (funcall (wayland-view-focus-callback view) nil))))
    (:key
     (destructuring-bind (serial time key state) arguments
       (declare (ignore time))
       (setf (wayland-serial frontend) serial)
       (let ((code (+ key +xkb-keycode-offset+)))
         (if (= state wl:+wl-keyboard-key-state-pressed+)
             (progn
               (wayland-deliver-key frontend code :press)
               (wayland-start-repeat frontend code))
             (progn
               (when (eql code (wayland-repeat-code frontend))
                 (wayland-stop-repeat frontend))
               (wayland-deliver-key frontend code :release))))))
    (:modifiers
     (destructuring-bind (serial depressed latched locked group) arguments
       (declare (ignore serial))
       (when (wayland-xkb-state frontend)
         (xkb:xkb-state-update-mask (wayland-xkb-state frontend)
                                    depressed latched locked 0 0 group))))
    (:repeat-info
     (destructuring-bind (rate delay) arguments
       (setf (wayland-repeat-rate frontend) rate
             (wayland-repeat-delay frontend) delay)))))

;;; Pointer

(defun wayland-pointer-pixels (view)
  "The pointer's position in buffer pixels, the grid's units."
  (let ((frontend (wayland-view-frontend view))
        (scale (wayland-view-scale view)))
    (values (* (wayland-pointer-x frontend) scale) (* (wayland-pointer-y frontend) scale))))

(defun wayland-pointer-cell (view)
  (multiple-value-bind (x y) (wayland-pointer-pixels view)
    (list (max 0 (floor y (wayland-view-cell-height view)))
          (max 0 (floor x (wayland-view-cell-width view))))))

(defun wayland-count-click (view cell)
  (let ((now (get-internal-real-time))
        (last (wayland-view-last-click view)))
    (setf (wayland-view-click-count view)
          (if (and last
                   (equal cell (cdr last))
                   (< (- now (car last))
                      (* *wayland-multi-click-seconds* internal-time-units-per-second)))
              (1+ (wayland-view-click-count view))
              1)
          (wayland-view-last-click view) (cons now cell))))

(defun wayland-deliver-mouse (view action button)
  (when (wayland-view-mouse-callback view)
    (let ((cell (wayland-pointer-cell view)))
      (multiple-value-bind (x y) (wayland-pointer-pixels view)
        (funcall (wayland-view-mouse-callback view)
                 action (first cell) (second cell) (wayland-view-click-count view)
                 button (wayland-modifier-state (wayland-view-frontend view)) x y)))))

(defun wayland-scroll (view value)
  "Scroll by the vertical axis VALUE, positive downwards."
  (when (wayland-view-scroll-callback view)
    (let* ((rows (+ (wayland-view-scroll-remainder view)
                    (* (/ value +wayland-axis-step+) *wayland-scroll-rows*)))
           (whole (truncate rows)))
      (setf (wayland-view-scroll-remainder view) (- rows whole))
      (unless (zerop whole)
        (funcall (wayland-view-scroll-callback view) whole)))))

(defun wayland-pointer-event (frontend event arguments)
  (case event
    (:enter
     (destructuring-bind (serial surface x y) arguments
       (setf (wayland-pointer-view frontend) (wayland-find-view frontend surface)
             (wayland-pointer-x frontend) x
             (wayland-pointer-y frontend) y)
       (when (wayland-cursor-shape-device frontend)
         (wl:wp-cursor-shape-device-v1-set-shape (wayland-cursor-shape-device frontend) serial
                                                 wl:+wp-cursor-shape-device-v1-shape-text+))))
    (:leave (setf (wayland-pointer-view frontend) nil))
    (:motion
     (destructuring-bind (time x y) arguments
       (declare (ignore time))
       (setf (wayland-pointer-x frontend) x
             (wayland-pointer-y frontend) y)
       (let ((view (wayland-pointer-view frontend)))
         (when view
           (let ((button (wayland-view-button view)))
             (wayland-deliver-mouse view (if button :drag :motion) button))))))
    (:button
     (destructuring-bind (serial time code state) arguments
       (declare (ignore time))
       (setf (wayland-serial frontend) serial)
       (let ((view (wayland-pointer-view frontend))
             (button (cdr (assoc code +wayland-buttons+))))
         (when (and view button)
           (if (= state wl:+wl-pointer-button-state-pressed+)
               (progn
                 (setf (wayland-view-button view) button)
                 (wayland-count-click view (wayland-pointer-cell view))
                 (wayland-deliver-mouse view :press button))
               (progn
                 (setf (wayland-view-button view) nil)
                 (wayland-deliver-mouse view :release button)))))))
    (:axis
     (destructuring-bind (time axis value) arguments
       (declare (ignore time))
       (let ((view (wayland-pointer-view frontend)))
         (when (and view (= axis wl:+wl-pointer-axis-vertical-scroll+))
           (wayland-scroll view value)))))))

;;; Data transfer: the clipboard and dropped files.  The other side writes
;;; into a pipe, which a thread reads, so that the event loop does not wait.

(defun wayland-read-fd (fd)
  "All the octets to be read from FD, which is then closed."
  (with-open-stream (stream (sb-sys:make-fd-stream fd :input t
                                                      :element-type '(unsigned-byte 8)))
    (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t
                                :fill-pointer 0))
          (buffer (make-array +pipe-chunk-size+ :element-type '(unsigned-byte 8))))
      (loop for end = (read-sequence buffer stream)
            while (plusp end)
            do (loop for index below end
                     do (vector-push-extend (aref buffer index) octets)))
      octets)))

(defun wayland-receive (frontend offer mime-type continuation)
  "Ask OFFER's source for its data as MIME-TYPE, and call CONTINUATION
with the octets on the event loop's thread."
  (multiple-value-bind (read write) (sb-posix:pipe)
    (wl:wl-data-offer-receive offer mime-type write)
    (wl:display-flush (wayland-display frontend))
    (sb-posix:close write)
    (bt:make-thread (lambda ()
                      (let ((octets (ignore-errors (wayland-read-fd read))))
                        (wayland-post frontend (lambda () (funcall continuation octets)))))
                    :name "slt wayland receive")))

(defun wayland-send (fd text)
  "Write TEXT into FD, for a client pasting it, and close FD."
  (bt:make-thread (lambda ()
                    (ignore-errors
                     (with-open-stream (stream (sb-sys:make-fd-stream
                                                fd :output t :element-type '(unsigned-byte 8)))
                       (write-sequence (sb-ext:string-to-octets text :external-format :utf-8)
                                       stream))))
                  :name "slt wayland send"))

(defun wayland-offer-type (frontend offer types)
  "The first of TYPES OFFER offers."
  (let ((offered (gethash (cffi:pointer-address offer) (wayland-offer-types frontend))))
    (find-if (lambda (type) (member type offered :test #'string=)) types)))

(defun wayland-forget-offer (frontend offer)
  (when offer
    (remhash (cffi:pointer-address offer) (wayland-offer-types frontend))
    (wl:wl-data-offer-destroy offer)))

(defun wayland-data-device-event (frontend event arguments)
  (case event
    (:data-offer
     (let* ((offer (first arguments))
            (address (cffi:pointer-address offer)))
       (setf (gethash address (wayland-offer-types frontend)) '())
       (wl:set-listener offer (lambda (event &rest arguments)
                                (when (eq event :offer)
                                  (push (first arguments)
                                        (gethash address (wayland-offer-types frontend))))))))
    (:selection
     (let ((previous (wayland-selection-offer frontend)))
       (setf (wayland-selection-offer frontend) (first arguments))
       (unless (eql previous (first arguments))
         (wayland-forget-offer frontend previous))))
    (:enter
     (destructuring-bind (serial surface x y offer) arguments
       (declare (ignore x y))
       (wayland-forget-offer frontend (wayland-drag-offer frontend))
       (setf (wayland-drag-offer frontend) offer
             (wayland-drag-view frontend) (wayland-find-view frontend surface))
       (when offer
         (let ((files-p (wayland-offer-type frontend offer
                                            (list +wayland-uri-list-mime-type+))))
           (wl:wl-data-offer-accept offer serial (and files-p +wayland-uri-list-mime-type+))
           (when files-p
             (wl:wl-data-offer-set-actions offer wl:+wl-data-device-manager-dnd-action-copy+
                                           wl:+wl-data-device-manager-dnd-action-copy+))))))
    (:leave
     (wayland-forget-offer frontend (shiftf (wayland-drag-offer frontend) nil)))
    (:drop
     (let ((offer (shiftf (wayland-drag-offer frontend) nil))
           (view (wayland-drag-view frontend)))
       (when offer
         (if (wayland-offer-type frontend offer (list +wayland-uri-list-mime-type+))
             (wayland-receive frontend offer +wayland-uri-list-mime-type+
                              (lambda (octets)
                                (wl:wl-data-offer-finish offer)
                                (wayland-forget-offer frontend offer)
                                (let ((paths (and octets
                                                  (uri-list-paths
                                                   (sb-ext:octets-to-string
                                                    (coerce octets '(vector (unsigned-byte 8)))
                                                    :external-format :utf-8)))))
                                  (when (and paths view (wayland-view-drop-callback view))
                                    (funcall (wayland-view-drop-callback view) paths)))))
             (wayland-forget-offer frontend offer)))))))

(defun wayland-paste (frontend view)
  "Ask for the selection's text, which arrives through VIEW's paste callback."
  (let* ((offer (wayland-selection-offer frontend))
         (type (and offer (wayland-offer-type frontend offer +wayland-text-mime-types+))))
    (when type
      (wayland-receive frontend offer type
                       (lambda (octets)
                         (when (and octets (plusp (length octets))
                                    (wayland-view-paste-callback view))
                           (funcall (wayland-view-paste-callback view)
                                    (sb-ext:octets-to-string
                                     (coerce octets '(vector (unsigned-byte 8)))
                                     :external-format :utf-8))))))))

(defun wayland-offer-clipboard (frontend text)
  "Own the selection, offering TEXT."
  (let ((manager (wayland-bound-global frontend "wl_data_device_manager"))
        (previous (shiftf (wayland-data-source frontend) nil)))
    (when previous
      (wl:wl-data-source-destroy previous))
    (setf (wayland-clipboard frontend) text)
    (when (and manager (wayland-data-device frontend))
      (let ((source (wl:wl-data-device-manager-create-data-source manager)))
        (dolist (type +wayland-text-mime-types+)
          (wl:wl-data-source-offer source type))
        (wl:set-listener source
                         (lambda (event &rest arguments)
                           (case event
                             (:send (wayland-send (second arguments)
                                                  (or (wayland-clipboard frontend) "")))
                             (:cancelled
                              (when (eql source (wayland-data-source frontend))
                                (setf (wayland-data-source frontend) nil
                                      (wayland-clipboard frontend) nil))
                              (wl:wl-data-source-destroy source)))))
        (setf (wayland-data-source frontend) source)
        (wl:wl-data-device-set-selection (wayland-data-device frontend) source
                                         (wayland-serial frontend))))))

;;; The event loop

(defun wayland-make-wake-pipe (frontend)
  (multiple-value-bind (read write) (sb-posix:pipe)
    (dolist (fd (list read write))
      (sb-posix:fcntl fd sb-posix:f-setfl
                      (logior (sb-posix:fcntl fd sb-posix:f-getfl) sb-posix:o-nonblock)))
    (setf (wayland-wake-read frontend) read
          (wayland-wake-write frontend) write)))

(defun wayland-post (frontend thunk)
  "Run THUNK on the event loop's thread soon.  Callable from any thread."
  (sb-thread:with-mutex ((wayland-lock frontend))
    (push thunk (wayland-wakeups frontend)))
  (let ((fd (wayland-wake-write frontend)))
    (when fd
      ;; A full pipe already wakes the loop.
      (cffi:with-foreign-object (octet :uint8)
        (ignore-errors (sb-posix:write fd octet 1))))))

(defun wayland-drain-wake-pipe (frontend)
  (cffi:with-foreign-object (buffer :uint8 +pipe-chunk-size+)
    (loop while (ignore-errors (= +pipe-chunk-size+
                                  (sb-posix:read (wayland-wake-read frontend) buffer
                                                 +pipe-chunk-size+))))))

(defun wayland-add-timer (frontend delay-ms thunk)
  (let ((timer (cons (+ (get-internal-real-time)
                        (round (* (max 0 delay-ms) internal-time-units-per-second) 1000))
                     thunk)))
    (sb-thread:with-mutex ((wayland-lock frontend))
      (push timer (wayland-timers frontend)))
    timer))

(defun wayland-cancel-timer (frontend timer)
  (sb-thread:with-mutex ((wayland-lock frontend))
    (setf (wayland-timers frontend) (remove timer (wayland-timers frontend)))))

(defun wayland-run-due (frontend)
  "Run the wakeups and the timers that are due; return the milliseconds
until the next timer, or NIL."
  (let ((now (get-internal-real-time))
        (due '())
        (wakeups '()))
    (sb-thread:with-mutex ((wayland-lock frontend))
      (setf wakeups (reverse (wayland-wakeups frontend))
            (wayland-wakeups frontend) '()
            due (remove-if (lambda (timer) (> (car timer) now)) (wayland-timers frontend))
            (wayland-timers frontend) (set-difference (wayland-timers frontend) due)))
    (mapc #'funcall wakeups)
    (dolist (timer (sort due #'< :key #'car))
      (funcall (cdr timer)))
    (let ((times (sb-thread:with-mutex ((wayland-lock frontend))
                   (mapcar #'car (wayland-timers frontend)))))
      (when times
        (max 0 (ceiling (* 1000 (- (reduce #'min times) (get-internal-real-time)))
                        internal-time-units-per-second))))))

(defun wayland-poll (fds timeout-ms)
  "Wait up to TIMEOUT-MS (or for ever, when NIL) for one of FDS to have
data; return a list of whether each has."
  (cffi:with-foreign-object (pollfds '(:struct pollfd) (length fds))
    (loop for fd in fds
          for index from 0
          for pollfd = (cffi:mem-aptr pollfds '(:struct pollfd) index)
          do (setf (cffi:foreign-slot-value pollfd '(:struct pollfd) 'fd) fd
                   (cffi:foreign-slot-value pollfd '(:struct pollfd) 'events) +pollin+
                   (cffi:foreign-slot-value pollfd '(:struct pollfd) 'revents) 0))
    ;; Interrupted (by another thread's GC), it returns early: no harm.
    (cffi:foreign-funcall "poll" :pointer pollfds :unsigned-long (length fds)
                                 :int (or timeout-ms +poll-forever+) :int)
    (loop for index below (length fds)
          collect (plusp (cffi:foreign-slot-value (cffi:mem-aptr pollfds '(:struct pollfd) index)
                                                  '(:struct pollfd) 'revents)))))

(defun wayland-wait (frontend timeout-ms)
  "Dispatch the compositor's events, waiting up to TIMEOUT-MS for them or
a wakeup."
  (let ((display (wayland-display frontend)))
    (loop until (zerop (wl:display-prepare-read display))
          do (wl:check-display (wl:display-dispatch-pending display) display
                               "wl_display_dispatch_pending"))
    ;; A full socket waits for the next round.
    (wl:display-flush display)
    (destructuring-bind (display-ready wake-ready)
        (wayland-poll (list (wl:display-fd display) (wayland-wake-read frontend)) timeout-ms)
      (if display-ready
          (wl:check-display (wl:display-read-events display) display "wl_display_read_events")
          (wl:display-cancel-read display))
      (when wake-ready
        (wayland-drain-wake-pipe frontend)))
    (wl:check-display (wl:display-dispatch-pending display) display
                      "wl_display_dispatch_pending")))

(defun wayland-destroy-view (view)
  "Free the view's renderer, buffer, window and font."
  (let ((renderer (shiftf (wayland-view-renderer view) nil)))
    (when renderer
      (ignore-errors (free-renderer renderer (wayland-view-host view)))))
  (let ((egl-window (shiftf (wayland-view-egl-window view) nil)))
    (when egl-window
      (wl:egl-window-destroy egl-window)))
  (let ((decoration (shiftf (wayland-view-decoration view) nil)))
    (when decoration
      (wl:zxdg-toplevel-decoration-v1-destroy decoration)))
  (wl:xdg-toplevel-destroy (wayland-view-toplevel view))
  (wl:xdg-surface-destroy (wayland-view-xdg-surface view))
  (wl:wl-surface-destroy (wayland-view-surface view))
  (let ((font (shiftf (wayland-view-font view) nil)))
    (when font
      (ignore-errors (close-glyph-font font)))))

(defun wayland-disconnect (frontend)
  (dolist (view (shiftf (wayland-views frontend) '()))
    (wayland-destroy-view view))
  (wayland-free-xkb-state frontend)
  (let ((compose (shiftf (wayland-compose-state frontend) nil))
        (table (shiftf (wayland-compose-table frontend) nil))
        (context (shiftf (wayland-xkb-context frontend) nil)))
    (when compose (xkb:xkb-compose-state-unref compose))
    (when table (xkb:xkb-compose-table-unref table))
    (when context (xkb:xkb-context-unref context)))
  (let ((display (shiftf (wayland-display frontend) nil)))
    (when display
      (wl:display-disconnect display)))
  (dolist (fd (list (shiftf (wayland-wake-read frontend) nil)
                    (shiftf (wayland-wake-write frontend) nil)))
    (when fd
      (sb-posix:close fd))))

(defmethod frontend-call-with-event-loop ((frontend wayland-frontend) thunk)
  ;; Mesa and the Vulkan drivers compute with floats that would trap.
  (float-features:with-float-traps-masked t
    (unwind-protect
         (progn
           (funcall thunk)
           (loop until (or (wayland-exit-p frontend) (null (wayland-display frontend)))
                 do (let ((timeout (wayland-run-due frontend)))
                      (unless (wayland-exit-p frontend)
                        (handler-case (wayland-wait frontend timeout)
                          (wl:wayland-error (condition)
                            (format *error-output* "~&slt: Wayland: ~a~%" condition)
                            (setf (wayland-exit-p frontend) t)))))))
      (wayland-disconnect frontend))))

;;; The frontend protocol

(defmethod frontend-available-font-families ((frontend wayland-frontend))
  (glyph-font-families))

(defmethod frontend-resolve-font-size ((frontend wayland-frontend) font-size)
  (abs (or font-size 15)))

(defmethod frontend-create-view ((frontend wayland-frontend)
                                &key rows columns font-family font-size
                                  cell-width cell-height title)
  (wayland-ensure-display frontend)
  (let* ((compositor (wayland-bound-global frontend "wl_compositor"))
         (surface (wl:wl-compositor-create-surface compositor))
         (xdg-surface (wl:xdg-wm-base-get-xdg-surface
                       (wayland-bound-global frontend "xdg_wm_base") surface))
         (toplevel (wl:xdg-surface-get-toplevel xdg-surface))
         (decorations (wayland-bound-global frontend "zxdg_decoration_manager_v1"))
         (view (%make-wayland-view
                :frontend frontend :surface surface :xdg-surface xdg-surface
                :toplevel toplevel
                :font-options (list font-family
                                    (frontend-resolve-font-size frontend font-size)
                                    cell-width cell-height))))
    (wl:set-listener surface (lambda (event &rest arguments)
                               (wayland-surface-event view event arguments)))
    (wl:set-listener xdg-surface (lambda (event &rest arguments)
                                   (wayland-xdg-surface-event view event arguments)))
    (wl:set-listener toplevel (lambda (event &rest arguments)
                                (wayland-toplevel-event view event arguments)))
    (wl:xdg-toplevel-set-title toplevel (or title "slt"))
    (wl:xdg-toplevel-set-app-id toplevel +wayland-app-id+)
    ;; Compositors that draw title bars do so when asked; others (GNOME)
    ;; leave windows without one.
    (when decorations
      (let ((decoration (wl:zxdg-decoration-manager-v1-get-toplevel-decoration decorations
                                                                                toplevel)))
        (setf (wayland-view-decoration view) decoration)
        (wl:zxdg-toplevel-decoration-v1-set-mode
         decoration wl:+zxdg-toplevel-decoration-v1-mode-server-side+)))
    (push view (wayland-views frontend))
    ;; The first configure, and the scale, come before the first buffer.
    (wl:wl-surface-commit surface)
    (loop until (wayland-view-configured-p view)
          do (wayland-roundtrip frontend))
    ;; A scale the compositor gave has opened the font already.
    (unless (wayland-view-font view)
      (wayland-view-set-font view (wayland-view-scale view)))
    (let ((scale (wayland-view-scale view)))
      (when (zerop (wayland-view-width view))
        (setf (wayland-view-width view)
              (ceiling (* columns (wayland-view-cell-width view)) scale)
              (wayland-view-height view)
              (ceiling (* rows (wayland-view-cell-height view)) scale))))
    (wl:wl-surface-set-buffer-scale surface (wayland-view-scale view))
    (wayland-make-renderer view)
    view))

(defmethod frontend-view-cell-width ((frontend wayland-frontend) (view wayland-view))
  (wayland-view-cell-width view))

(defmethod frontend-view-cell-height ((frontend wayland-frontend) (view wayland-view))
  (wayland-view-cell-height view))

(defmethod frontend-view-window-size ((frontend wayland-frontend) (view wayland-view))
  (wayland-view-pixel-size view))

(defmethod frontend-resize-view ((frontend wayland-frontend) (view wayland-view) rows columns)
  (declare (ignore rows columns))
  view)

(defmethod frontend-render-grid ((frontend wayland-frontend) (view wayland-view) grid)
  (setf (wayland-view-grid view) grid)
  (wayland-redraw view)
  view)

(defmethod frontend-render-images ((frontend wayland-frontend) (view wayland-view) store)
  (setf (wayland-view-image-store view) store)
  (wayland-redraw view)
  view)

(defmethod frontend-snapshot ((frontend wayland-frontend) (view wayland-view))
  (with-view-palette (view)
    (multiple-value-bind (width height) (wayland-view-pixel-size view)
      (values (renderer-snapshot (wayland-view-renderer view) (wayland-view-host view)
                                 (wayland-frame view *palette*))
              width height))))

(defmethod frontend-set-title ((frontend wayland-frontend) (view wayland-view) title)
  (unless (wayland-view-closed-p view)
    (wl:xdg-toplevel-set-title (wayland-view-toplevel view) title)))

(defmethod frontend-schedule ((frontend wayland-frontend) (view wayland-view) delay-ms thunk)
  (wayland-add-timer frontend delay-ms thunk))

(defmethod frontend-cancel-scheduled ((frontend wayland-frontend) (view wayland-view) token)
  (wayland-cancel-timer frontend token))

(defmethod frontend-wakeup ((frontend wayland-frontend) view thunk)
  (declare (ignore view))
  (wayland-post frontend thunk)
  t)

(defmethod frontend-bind-resize ((frontend wayland-frontend) (view wayland-view) callback)
  (setf (wayland-view-resize-callback view) callback))

(defmethod frontend-bind-keypress ((frontend wayland-frontend) (view wayland-view) callback)
  (setf (wayland-view-keypress-callback view) callback))

(defmethod frontend-bind-scroll ((frontend wayland-frontend) (view wayland-view) callback)
  (setf (wayland-view-scroll-callback view) callback))

(defmethod frontend-bind-mouse ((frontend wayland-frontend) (view wayland-view) callback)
  (setf (wayland-view-mouse-callback view) callback))

(defmethod frontend-bind-paste ((frontend wayland-frontend) (view wayland-view) callback)
  (setf (wayland-view-paste-callback view) callback))

(defmethod frontend-bind-focus ((frontend wayland-frontend) (view wayland-view) callback)
  (setf (wayland-view-focus-callback view) callback))

(defmethod frontend-bind-drop ((frontend wayland-frontend) (view wayland-view) callback)
  (setf (wayland-view-drop-callback view) callback))

(defmethod frontend-on-close ((frontend wayland-frontend) (view wayland-view) callback)
  (setf (wayland-view-close-callback view) callback))

(defmethod frontend-clipboard-text ((frontend wayland-frontend) (view wayland-view))
  ;; Our own selection is returned now; another client's arrives through
  ;; the paste callback once it is written.
  (or (and (wayland-data-source frontend) (wayland-clipboard frontend))
      (progn (wayland-paste frontend view) nil)))

(defmethod frontend-set-clipboard-text ((frontend wayland-frontend) (view wayland-view) text)
  (wayland-offer-clipboard frontend text))

(defmethod frontend-focus ((frontend wayland-frontend) (view wayland-view))
  ;; Wayland gives the focus only to windows the user activates.
  view)

(defmethod frontend-request-exit ((frontend wayland-frontend) (view wayland-view))
  (unless (wayland-view-closed-p view)
    (setf (wayland-view-closed-p view) t
          (wayland-views frontend) (remove view (wayland-views frontend)))
    (dolist (slot '(keyboard-view pointer-view drag-view))
      (when (eq view (slot-value frontend slot))
        (setf (slot-value frontend slot) nil)))
    (wayland-destroy-view view)
    (setf (wayland-exit-p frontend) t)))

(register-frontend :wayland (lambda () (make-instance 'wayland-frontend)))
