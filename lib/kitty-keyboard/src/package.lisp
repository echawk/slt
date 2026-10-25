(defpackage #:kitty-keyboard
  (:use #:cl)
  (:documentation "The Kitty keyboard protocol
(https://sw.kovidgoyal.net/kitty/keyboard-protocol/).")
  (:export
   ;; Flags
   #:+disambiguate+
   #:+report-events+
   #:+report-alternates+
   #:+report-all-keys+
   #:+report-text+
   ;; Per-screen state
   #:keyboard-state
   #:make-keyboard-state
   #:keyboard-flags
   #:keyboard-set-flags
   #:keyboard-push
   #:keyboard-pop
   #:keyboard-reset
   #:keyboard-control-sequence
   ;; Keys
   #:functional-key-p
   #:unshifted-key
   #:key-code
   #:modifier-bits
   ;; Encoding
   #:encode-key))
