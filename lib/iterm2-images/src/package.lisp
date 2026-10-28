(defpackage #:iterm2-images
  (:use #:cl)
  (:export
   ;; Errors
   #:image-error
   #:image-error-message
   ;; Image files
   #:*max-image-octets*
   #:decode-image-file
   #:image-file-format
   #:frame
   #:frame-rgba
   #:frame-delay-ms
   ;; The protocol
   #:file-command-start
   #:decoder
   #:make-decoder
   #:decoder-feed
   #:decoder-reset
   #:image
   #:make-image
   #:image-arguments
   #:image-argument
   #:image-frames
   #:image-width
   #:image-height
   #:image-loops-p
   #:image-moves-cursor-p
   #:image-display-size))
