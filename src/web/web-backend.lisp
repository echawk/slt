(in-package #:slt)

;;;; Web frontend: a headless backend that serves a browser client over
;;;; HTTP and streams the grid to it over a WebSocket.
;;;;
;;;; The browser side is web/slt-terminal.js, an ES module (an SltTerminal
;;;; class and a <slt-terminal> custom element) that existing web apps can
;;;; import; this server also hosts it with CORS enabled, plus a demo page.
;;;;
;;;; Protocol (JSON text frames, version 1):
;;;;   server -> client
;;;;     {"t":"hello","v":1,"rows":R,"cols":C,"title":S}
;;;;     {"t":"frame","rows":[[ROW,[[TEXT,FG,BG,ATTRS],...]],...],
;;;;      "cursor":[ROW,COL,VISIBLE,SHAPE]}
;;;;       FG/BG are grid colors: 0 default, #x01000000+N palette, #x02000000+RGB.
;;;;       ATTRS are the grid attribute bits; a wide glyph is its own run.
;;;;     {"t":"title","title":S}  {"t":"bell"}  {"t":"exit"}
;;;;   client -> server
;;;;     {"t":"key","char":S,"keysym":S,"state":N}   (X11 keysyms/modifier bits)
;;;;     {"t":"paste","text":S}
;;;;     {"t":"scroll","rows":N}                     (negative scrolls back)
;;;;     {"t":"resize","rows":R,"cols":C}
;;;;
;;;; Anyone who can open the socket can run commands as you, so the server
;;;; listens on 127.0.0.1 by default and requires a random token (?token=)
;;;; on every page and socket request.  *WEB-ALLOWED-ORIGINS* can further
;;;; restrict which pages may open the socket.

(defvar *web-host* "127.0.0.1")
(defvar *web-port* 8080)
(defvar *web-token* nil
  "The access token; a random one is generated when NIL.")
(defvar *web-allowed-origins* nil
  "When non-NIL, a list of Origin header values allowed to open the socket.")

(defun web-asset-pathname (name)
  (asdf:system-relative-pathname :slt (concatenate 'string "web/" name)))

(defvar *web-assets* (make-hash-table :test #'equal)
  "Asset contents captured at load time, so a dumped executable is
self-contained.  Set *WEB-ASSETS-FROM-DISK* to edit the files live.")

(defvar *web-assets-from-disk* nil)

(defun web-asset (name)
  (if *web-assets-from-disk*
      (uiop:read-file-string (web-asset-pathname name))
      (or (gethash name *web-assets*)
          (error "Unknown web asset ~s." name))))

(dolist (name '("slt-terminal.js" "index.html"))
  (setf (gethash name *web-assets*)
        (uiop:read-file-string (web-asset-pathname name))))

(defun random-token ()
  (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
    (format nil "~(~{~2,'0x~}~)" (loop repeat 16 collect (read-byte in)))))

(defclass web-backend (headless-backend)
  ((host :initarg :host :initform *web-host* :reader web-backend-host)
   (port :initarg :port :initform *web-port* :reader web-backend-port)
   (token :initarg :token :initform (or *web-token* (random-token)) :reader web-backend-token)
   (acceptor :initform nil :accessor web-backend-acceptor)
   (resource :initform nil :accessor web-backend-resource)
   (dispatcher :initform nil :accessor web-backend-dispatcher)))

(defclass web-acceptor (hunchensocket:websocket-acceptor)
  ((backend :initarg :backend :reader web-acceptor-backend)))

(defclass web-resource (hunchensocket:websocket-resource)
  ((backend :initarg :backend :reader web-resource-backend))
  (:default-initargs :client-class 'hunchensocket:websocket-client))

(defun web-url (backend)
  (format nil "http://~a:~d/?token=~a"
          (web-backend-host backend) (web-backend-port backend) (web-backend-token backend)))

(defun token-valid-p (backend request)
  (let ((token (hunchentoot:get-parameter "token" request)))
    (and token (string= token (web-backend-token backend)))))

;;; HTTP

(defun serve-web-asset (name content-type &key cors)
  (setf (hunchentoot:content-type*) content-type)
  (when cors
    (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*"))
  (setf (hunchentoot:header-out "Cache-Control") "no-cache")
  (web-asset name))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor web-acceptor) request)
  (let ((backend (web-acceptor-backend acceptor))
        (path (hunchentoot:script-name request)))
    (cond
      ((string-equal "websocket" (or (hunchentoot:header-in :upgrade request) ""))
       (call-next-method))
      ((string= path "/slt-terminal.js")
       ;; Not secret: served to any origin so other apps can import it.
       (serve-web-asset "slt-terminal.js" "text/javascript; charset=utf-8" :cors t))
      ((not (token-valid-p backend request))
       (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
       "slt: missing or wrong ?token=")
      ((string= path "/")
       (serve-web-asset "index.html" "text/html; charset=utf-8"))
      (t
       (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
       "not found"))))

;;; WebSocket

(defun web-send (client object)
  (handler-case
      (hunchensocket:send-text-message client (com.inuoe.jzon:stringify object))
    (error () nil)))

(defun web-broadcast (backend object)
  (let ((resource (web-backend-resource backend)))
    (when resource
      (let ((message (com.inuoe.jzon:stringify object)))
        (dolist (client (hunchensocket:clients resource))
          (handler-case (hunchensocket:send-text-message client message)
            (error () nil)))))))

(defun json-object (&rest plist)
  "A hash table for jzon from alternating string keys and values."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on plist by #'cddr
          do (setf (gethash key table) value))
    table))

(defun grid-row-runs (grid row)
  "ROW as a vector of [TEXT FG BG ATTRS] runs of identically styled cells."
  (let ((runs (make-array 0 :adjustable t :fill-pointer t))
        (text (make-string-output-stream))
        (style nil))
    (flet ((flush ()
             (when style
               (vector-push-extend (vector (get-output-stream-string text)
                                           (first style) (second style) (third style))
                                   runs))
             (setf style nil)))
      (dotimes (column (grid-columns grid))
        (let* ((index (grid-index grid row column))
               (attrs (aref (grid-attrs grid) index)))
          (unless (logtest attrs +attr-wide-spacer+)
            (let ((cell-style (list (aref (grid-fg grid) index)
                                    (aref (grid-bg grid) index)
                                    attrs)))
              (when (or (logtest attrs +attr-wide+)
                        (and style (logtest (third style) +attr-wide+))
                        (not (equal style cell-style)))
                (flush)
                (setf style cell-style))
              (write-string (grid-cell-string grid index) text)))))
      (flush))
    runs))

(defun web-frame (grid &key all)
  (let ((rows (make-array 0 :adjustable t :fill-pointer t)))
    (dotimes (row (grid-rows grid))
      (when (or all (= 1 (sbit (grid-dirty grid) row)))
        (vector-push-extend (vector row (grid-row-runs grid row)) rows)))
    (json-object "t" "frame"
            "rows" rows
            "cursor" (vector (grid-cursor-row grid) (grid-cursor-column grid)
                             (grid-cursor-visible-p grid)
                             (string-downcase (grid-cursor-shape grid))))))

(defun web-hello (view)
  (json-object "t" "hello" "v" 1
          "rows" (headless-view-rows view)
          "cols" (headless-view-columns view)
          "title" (headless-view-title view)))

(defmethod hunchensocket:client-connected ((resource web-resource) client)
  (let ((backend (web-resource-backend resource)))
    (headless-call backend
                   (lambda ()
                     (let ((view (headless-backend-view backend)))
                       (when view
                         (web-send client (web-hello view))
                         (when (headless-view-grid view)
                           (web-send client (web-frame (headless-view-grid view) :all t)))))))))

(defun json-integer (table key &optional (default 0))
  (let ((value (gethash key table)))
    (if (integerp value) value default)))

(defun json-string (table key)
  (let ((value (gethash key table)))
    (if (stringp value) value "")))

(defmethod hunchensocket:text-message-received ((resource web-resource) client message)
  (declare (ignore client))
  (let* ((backend (web-resource-backend resource))
         (view (headless-backend-view backend))
         (data (ignore-errors (com.inuoe.jzon:parse message))))
    (when (and view (hash-table-p data))
      (let ((type (gethash "t" data)))
        (cond
          ((equal type "key")
           (headless-key view (json-string data "char") (json-string data "keysym")
                         (json-integer data "state")))
          ((equal type "paste")
           (headless-paste view (json-string data "text")))
          ((equal type "scroll")
           (let ((rows (json-integer data "rows" 0)))
             (when (< -10000 rows 10000)
               (headless-scroll view rows))))
          ((equal type "resize")
           (let ((rows (json-integer data "rows" 0))
                 (columns (json-integer data "cols" 0)))
             (when (and (< 0 rows 1000) (< 0 columns 1000))
               (headless-resize-cells view rows columns)))))))))

(defun web-origin-allowed-p (request)
  (or (null *web-allowed-origins*)
      (member (hunchentoot:header-in :origin request) *web-allowed-origins*
              :test #'equal)))

;;; Backend protocol

(defmethod headless-present ((backend web-backend) view grid)
  (declare (ignore view))
  (web-broadcast backend (web-frame grid)))

(defmethod backend-set-title :after ((backend web-backend) view title)
  (declare (ignore view))
  (web-broadcast backend (json-object "t" "title" "title" title)))

(defmethod backend-resize-view :after ((backend web-backend) view rows columns)
  (declare (ignore view))
  (web-broadcast backend (json-object "t" "resize" "rows" rows "cols" columns)))

(defmethod backend-request-exit :before ((backend web-backend) view)
  (declare (ignore view))
  (web-broadcast backend (json-object "t" "exit")))

(defun start-web-server (backend)
  (let* ((resource (make-instance 'web-resource :backend backend))
         (dispatcher (lambda (request)
                       (and (string= (hunchentoot:script-name request) "/ws")
                            (token-valid-p backend request)
                            (web-origin-allowed-p request)
                            resource)))
         (acceptor (make-instance 'web-acceptor
                                  :backend backend
                                  :address (web-backend-host backend)
                                  :port (web-backend-port backend)
                                  :access-log-destination nil)))
    (setf (web-backend-resource backend) resource
          (web-backend-dispatcher backend) dispatcher
          (web-backend-acceptor backend) acceptor)
    (push dispatcher hunchensocket:*websocket-dispatch-table*)
    (hunchentoot:start acceptor)
    (format t "~&slt: serving on ~a~%" (web-url backend))
    (finish-output)
    backend))

(defun stop-web-server (backend)
  (setf hunchensocket:*websocket-dispatch-table*
        (remove (web-backend-dispatcher backend) hunchensocket:*websocket-dispatch-table*))
  (when (web-backend-acceptor backend)
    (ignore-errors (hunchentoot:stop (web-backend-acceptor backend) :soft t))
    (setf (web-backend-acceptor backend) nil)))

(defmethod backend-call-with-event-loop ((backend web-backend) thunk)
  (start-web-server backend)
  (unwind-protect (call-next-method)
    (stop-web-server backend)))

(register-backend :web (lambda () (make-instance 'web-backend)))
