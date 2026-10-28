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
   #:decoded-length
   #:encode
   #:encoded-length
   ;; cl-base64's
   #:base64-stream-to-integer
   #:base64-string-to-integer
   #:base64-string-to-string
   #:base64-string-to-stream
   #:base64-string-to-usb8-array
   #:string-to-base64-string
   #:string-to-base64-stream
   #:usb8-array-to-base64-string
   #:usb8-array-to-base64-stream
   #:integer-to-base64-string
   #:integer-to-base64-stream
   #:*uri-encode-table*
   #:*uri-decode-table*
   #:make-decode-table))
