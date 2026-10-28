(defpackage #:simd-base64
  (:use #:cl)
  (:export
   #:base64-error
   #:decoder
   #:make-decoder
   #:decoder-output
   #:decoder-layout
   #:decoder-fill
   #:decoder-feed
   #:decoder-finish
   #:decode
   #:decoded-length))
