(defpackage #:ecma48
  (:use #:cl)
  (:documentation "An incremental scanner for ECMA-48 control functions in
UTF-8 terminal output.")
  (:export
   ;; Scanner
   #:scanner
   #:make-scanner
   #:scanner-p
   #:scan
   #:scanner-flush
   #:scanner-reset
   #:scanner-in-sequence-p
   ;; Events
   #:event
   #:event-kind
   #:event-final
   #:event-private
   #:event-intermediates
   #:event-intermediate
   #:event-terminator
   #:event-octets
   #:event-start
   #:event-end
   #:event-data-start
   #:event-data-end
   #:event-parameter
   #:event-parameter-count
   #:event-parameter-default-p
   #:event-subparameter-p
   #:event-parameters
   #:event-data
   #:event-raw
   #:event-valid-p
   #:copy-event))
