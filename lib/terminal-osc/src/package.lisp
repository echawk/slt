(defpackage #:terminal-osc
  (:use #:cl)
  (:documentation "OSC (Operating System Command) payloads: parsing requests
and encoding replies.  Nothing here acts on them.")
  (:export
   #:parse-osc
   #:encode-osc
   ;; Colors (OSC 4, 10-19, 104, 110-119)
   #:parse-color-spec
   #:format-color-spec
   #:parse-palette-request
   #:parse-dynamic-color-request
   #:parse-palette-reset
   ;; Clipboard (OSC 52)
   #:parse-clipboard-request
   #:encode-clipboard-reply
   ;; Hyperlinks (OSC 8)
   #:parse-hyperlink
   ;; Working directory (OSC 7)
   #:parse-working-directory
   ;; Shell integration (OSC 133)
   #:parse-semantic-prompt
   ;; Notifications (OSC 9, 777, 99)
   #:parse-notification
   #:notification-assembler
   #:make-notification-assembler
   #:notification-assembler-feed))
