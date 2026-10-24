(defpackage #:lispvt
  (:use #:cl)
  (:export
   ;; Terminal
   #:terminal #:make-terminal #:terminal-p
   #:terminal-feed #:terminal-feed-string #:terminal-resize #:terminal-reset
   #:terminal-rows #:terminal-columns #:terminal-title
   #:terminal-output-function #:terminal-title-function #:terminal-bell-function
   #:terminal-scroll-function
   #:terminal-cursor-row #:terminal-cursor-column #:terminal-cursor-visible-p
   #:terminal-cursor-shape #:terminal-mode #:terminal-alternate-screen-p
   ;; Reading the screen
   #:terminal-line #:terminal-dirty #:terminal-row-dirty-p #:terminal-clear-dirty
   #:terminal-row-text #:terminal-text
   #:line #:line-codes #:line-fg #:line-bg #:line-attrs #:line-combining
   #:line-wrapped
   ;; Scrollback
   #:terminal-scroll-viewport #:terminal-viewport-offset #:terminal-history-count
   ;; Cells
   #:+color-default+ #:palette-color #:rgb-color
   #:+attr-bold+ #:+attr-faint+ #:+attr-italic+ #:+attr-underline+
   #:+attr-blink+ #:+attr-inverse+ #:+attr-invisible+ #:+attr-strike+
   #:+attr-wide+ #:+attr-wide-spacer+ #:+attr-double-underline+
   #:char-width
   #:scan-printable))
