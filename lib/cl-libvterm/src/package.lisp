;;;; package.lisp

(defpackage #:libvterm
  (:use #:cl)
  (:export
   ;; library
   #:load-libvterm
   #:*libvterm-version*
   ;; terminal lifecycle
   #:vterm
   #:vterm-p
   #:make-vterm
   #:vterm-free
   #:with-vterm
   #:vterm-live-p
   ;; I/O
   #:vterm-feed
   #:vterm-resize
   #:vterm-rows
   #:vterm-columns
   #:vterm-on-output
   #:vterm-on-bell
   #:vterm-on-title
   #:vterm-on-scrollback
   ;; state
   #:vterm-cursor
   #:vterm-title
   #:vterm-icon-name
   #:vterm-altscreen-p
   #:vterm-property
   ;; screen contents
   #:vterm-cell
   #:vterm-row-into
   #:vterm-row-text
   #:vterm-screen-text
   #:decode-color
   #:decode-attrs
   #:attrs-width
   #:attrs-combining-p
   #:make-row-buffer
   ;; damage
   #:vterm-take-damage
   #:vterm-mark-all-dirty
   ;; keyboard
   #:vterm-key
   #:vterm-char
   #:vterm-encode-key
   #:vterm-encode-char
   #:vterm-encode-mouse
   #:vterm-paste-start
   #:vterm-paste-end
   ;; packed-attribute bit masks
   #:+attr-bold+ #:+attr-italic+ #:+attr-underline-mask+ #:+attr-blink+
   #:+attr-inverse+ #:+attr-conceal+ #:+attr-strike+ #:+attr-combining+
   #:+attr-width-mask+ #:+attr-width-shift+
   #:+color-kind-default+ #:+color-kind-palette+ #:+color-kind-rgb+))
