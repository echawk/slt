(defpackage #:cl-libsixel
  (:nicknames #:libsixel)
  (:use #:cl)
  (:export
   #:load-libsixel
   #:libsixel-available-p
   #:libsixel-error
   #:libsixel-error-status
   #:libsixel-error-message
   #:sixel-image
   #:sixel-image-p
   #:sixel-image-width
   #:sixel-image-height
   #:sixel-image-pixels
   #:sixel-image-palette
   #:sixel-image-color-count
   #:decode-sixel
   #:sixel-image-rgba))
