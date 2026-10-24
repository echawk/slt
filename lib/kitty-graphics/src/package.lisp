(defpackage #:kitty-graphics
  (:use #:cl)
  (:export
   #:graphics-protocol-error
   #:graphics-protocol-error-message
   #:graphics-command
   #:graphics-command-p
   #:graphics-command-controls
   #:graphics-command-data
   #:graphics-command-value
   #:graphics-command-integer
   #:graphics-command-action
   #:graphics-command-more-p
   #:parse-graphics-command
   #:encode-graphics-command
   #:encode-graphics-response
   #:graphics-decoder
   #:make-graphics-decoder
   #:graphics-decoder-max-encoded-bytes
   #:graphics-decoder-reset
   #:graphics-decoder-feed
   #:graphics-command-pixel-data
   #:graphics-command-rgba))
