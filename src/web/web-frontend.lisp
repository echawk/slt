(in-package #:slt)

;;;; Web frontend: a headless frontend that serves a browser client over
;;;; HTTP and streams the grid to it over a WebSocket.
;;;;
;;;; The browser side is web/slt-terminal.js, an ES module (an SltTerminal
;;;; class and a <slt-terminal> custom element) that existing web apps can
;;;; import; this server also hosts it with CORS enabled, plus a demo page.
;;;;
;;;; Protocol (JSON text frames, version 1):
;;;;   server -> client
;;;;     {"t":"hello","v":1,"rows":R,"cols":C,"title":S,"mouse":B}
;;;;     {"t":"frame","rows":[[ROW,[[TEXT,FG,BG,ATTRS],...]],...],
;;;;      "cursor":[ROW,COL,VISIBLE,SHAPE]}
;;;;       FG/BG are grid colors: 0 default, #x01000000+N palette, #x02000000+RGB.
;;;;       ATTRS are the grid attribute bits; a wide glyph is its own run.
;;;;     {"t":"title","title":S}  {"t":"bell"}  {"t":"exit"}
;;;;     {"t":"mouse","tracking":B}   (whether the program wants mouse events)
;;;;     {"t":"images","items":[...]} (RGBA image placements)
;;;;     {"t":"colors","palette":{N:"#RRGGBB",...},"foreground":S,"background":S,
;;;;      "cursor":S,"selection":S}
;;;;       Colors the program changed (OSC 4, 10-19), each key only when it
;;;;       differs from the defaults; the client keeps its own theme for the rest.
;;;;   client -> server
;;;;     {"t":"key","char":S,"keysym":S,"state":N,"event":S}
;;;;         (X11 keysyms/modifier bits; event "press", "repeat" or "release")
;;;;     {"t":"paste","text":S}
;;;;     {"t":"scroll","rows":N}                     (negative scrolls back)
;;;;     {"t":"mouse","action":A,"row":R,"col":C,"button":B,"state":N}
;;;;       A is "press", "release", "drag" or "motion"; B is 1-3, or 0 for none
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

(defclass web-frontend (headless-frontend)
  ((host :initarg :host :initform *web-host* :reader web-frontend-host)
   (port :initarg :port :initform *web-port* :reader web-frontend-port)
   (token :initarg :token :initform (or *web-token* (random-token)) :reader web-frontend-token)
   (acceptor :initform nil :accessor web-frontend-acceptor)
   (resource :initform nil :accessor web-frontend-resource)
   (dispatcher :initform nil :accessor web-frontend-dispatcher)
   ;; The palette whose colors the clients were last sent.
   (sent-palette :initform nil :accessor web-frontend-sent-palette)))

(defclass web-acceptor (hunchensocket:websocket-acceptor)
  ((frontend :initarg :frontend :reader web-acceptor-frontend)))

(defclass web-resource (hunchensocket:websocket-resource)
  ((frontend :initarg :frontend :reader web-resource-frontend))
  (:default-initargs :client-class 'hunchensocket:websocket-client))

(defun web-url (frontend)
  (format nil "http://~a:~d/?token=~a"
          (web-frontend-host frontend) (web-frontend-port frontend) (web-frontend-token frontend)))

(defun token-valid-p (frontend request)
  (let ((token (hunchentoot:get-parameter "token" request)))
    (and token (string= token (web-frontend-token frontend)))))

;;; HTTP

(defun serve-web-asset (name content-type &key cors)
  (setf (hunchentoot:content-type*) content-type)
  (when cors
    (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*"))
  (setf (hunchentoot:header-out "Cache-Control") "no-cache")
  (web-asset name))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor web-acceptor) request)
  (let ((frontend (web-acceptor-frontend acceptor))
        (path (hunchentoot:script-name request)))
    (cond
      ((string-equal "websocket" (or (hunchentoot:header-in :upgrade request) ""))
       (call-next-method))
      ((string= path "/slt-terminal.js")
       ;; Not secret: served to any origin so other apps can import it.
       (serve-web-asset "slt-terminal.js" "text/javascript; charset=utf-8" :cors t))
      ((not (token-valid-p frontend request))
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

(defun web-broadcast (frontend object)
  (let ((resource (web-frontend-resource frontend)))
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
  "ROW as a vector of [TEXT FG BG ATTRS] runs of identically styled cells,
with a fifth element, the underline color, when it is not the default."
  (let ((runs (make-array 0 :adjustable t :fill-pointer t))
        (text (make-string-output-stream))
        (style nil))
    (flet ((flush ()
             (when style
               (vector-push-extend (if (zerop (fourth style))
                                       (vector (get-output-stream-string text)
                                               (first style) (second style) (third style))
                                       (vector (get-output-stream-string text)
                                               (first style) (second style) (third style)
                                               (fourth style)))
                                   runs))
             (setf style nil)))
      (dotimes (column (grid-columns grid))
        (let* ((index (grid-index grid row column))
               (attrs (aref (grid-attrs grid) index)))
          (unless (logtest attrs +attr-wide-spacer+)
            (let ((cell-style (list (aref (grid-fg grid) index)
                                    (aref (grid-bg grid) index)
                                    attrs
                                    (aref (grid-ul grid) index))))
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
          "title" (headless-view-title view)
          "mouse" (headless-view-mouse-tracking view)))

(defmethod hunchensocket:client-connected ((resource web-resource) client)
  (let ((frontend (web-resource-frontend resource)))
    (headless-call frontend
                   (lambda ()
                     (let ((view (headless-frontend-view frontend)))
                       (when view
                         (web-send client (web-hello view))
                         (web-send client (web-colors-message view))
                         (when (headless-view-grid view)
                           (web-send client (web-frame (headless-view-grid view) :all t)))
                         (when (headless-view-image-store view)
                           (web-send client
                                     (web-images-message
                                      (terminal-image-render-list
                                       (headless-view-image-store view)
                                       (headless-view-cell-width view)
                                       (headless-view-cell-height view)))))))))))

(defun json-integer (table key &optional (default 0))
  (let ((value (gethash key table)))
    (if (integerp value) value default)))

(defun json-string (table key)
  (let ((value (gethash key table)))
    (if (stringp value) value "")))

(defmethod hunchensocket:text-message-received ((resource web-resource) client message)
  (declare (ignore client))
  (let* ((frontend (web-resource-frontend resource))
         (view (headless-frontend-view frontend))
         (data (ignore-errors (com.inuoe.jzon:parse message))))
    (when (and view (hash-table-p data))
      (let ((type (gethash "t" data)))
        (cond
          ((equal type "key")
           (headless-key view (json-string data "char") (json-string data "keysym")
                         (json-integer data "state")
                         (cond ((equal (gethash "event" data) "repeat") :repeat)
                               ((equal (gethash "event" data) "release") :release)
                               (t :press))))
          ((equal type "paste")
           (headless-paste view (json-string data "text")))
          ((equal type "scroll")
           (let ((rows (json-integer data "rows" 0)))
             (when (< -10000 rows 10000)
               (headless-scroll view rows))))
          ((equal type "mouse")
           (let ((action (cdr (assoc (json-string data "action")
                                     '(("press" . :press) ("release" . :release)
                                       ("drag" . :drag) ("motion" . :motion))
                                     :test #'string=)))
                 (row (json-integer data "row" -1))
                 (column (json-integer data "col" -1))
                 (button (json-integer data "button")))
             (when (and action (< -1 row 1000) (< -1 column 1000) (<= 0 button 3))
               (headless-mouse view action row column 1
                               (if (zerop button) nil button)
                               (logand (json-integer data "state") #xff)))))
          ((equal type "resize")
           (let ((rows (json-integer data "rows" 0))
                 (columns (json-integer data "cols" 0)))
             (when (and (< 0 rows 1000) (< 0 columns 1000))
               (headless-resize-cells view rows columns)))))))))

(defun web-origin-allowed-p (request)
  (or (null *web-allowed-origins*)
      (member (hunchentoot:header-in :origin request) *web-allowed-origins*
              :test #'equal)))

;;; Frontend protocol

(defun web-colors-message (view)
  "The colors VIEW's palette changed from the default one."
  (let* ((palette (view-palette view))
         (default *palette*)
         (changed (make-hash-table :test #'equal))
         (message (json-object "t" "colors" "palette" changed)))
    (unless (eq palette default)
      (dotimes (index 256)
        (let ((rgb (aref (palette-colors palette) index)))
          (unless (= rgb (aref (palette-colors default) index))
            (setf (gethash (princ-to-string index) changed) (rgb->hex rgb)))))
      (loop for (key reader) in '(("foreground" palette-foreground)
                                  ("background" palette-background)
                                  ("cursor" palette-cursor)
                                  ("selection" palette-selection))
            for rgb = (funcall reader palette)
            unless (= rgb (funcall reader default))
              do (setf (gethash key message) (rgb->hex rgb))))
    message))

(defmethod headless-present ((frontend web-frontend) view grid)
  ;; Programs' color changes reach the clients before the cells using them.
  (let ((palette (view-palette view)))
    (unless (eq palette (web-frontend-sent-palette frontend))
      (setf (web-frontend-sent-palette frontend) palette)
      (web-broadcast frontend (web-colors-message view))))
  (web-broadcast frontend (web-frame grid)))

(defun web-image-record (record)
  (destructuring-bind (placement image x y width height) record
    (json-object
     "id" (terminal-image-id image)
     "x" x "y" y "w" width "h" height
     "z" (terminal-image-placement-z-index placement)
     "sx" (terminal-image-placement-source-x placement)
     "sy" (terminal-image-placement-source-y placement)
     "sw" (or (terminal-image-placement-source-width placement)
              (- (terminal-image-width image)
                 (terminal-image-placement-source-x placement)))
     "sh" (or (terminal-image-placement-source-height placement)
              (- (terminal-image-height image)
                 (terminal-image-placement-source-y placement)))
     "iw" (terminal-image-width image) "ih" (terminal-image-height image))))

(defun web-images-message (records)
  "An images message: each image's pixels once, under \"sources\", however
many placements and placement fragments draw it."
  (let ((seen '()))
    (json-object
     "t" "images"
     "sources" (coerce
                (loop for (nil image) in records
                      unless (member (terminal-image-id image) seen)
                        do (push (terminal-image-id image) seen)
                        and collect (json-object
                                     "id" (terminal-image-id image)
                                     "iw" (terminal-image-width image)
                                     "ih" (terminal-image-height image)
                                     "rgba" (cl-base64:usb8-array-to-base64-string
                                             (terminal-image-rgba image))))
                'vector)
     "items" (coerce (mapcar #'web-image-record records) 'vector))))

(defmethod frontend-render-images :after ((frontend web-frontend) (view headless-view) store)
  (web-broadcast frontend
                 (web-images-message
                  (terminal-image-render-list store (headless-view-cell-width view)
                                              (headless-view-cell-height view)))))

(defmethod frontend-set-title :after ((frontend web-frontend) view title)
  (declare (ignore view))
  (web-broadcast frontend (json-object "t" "title" "title" title)))

(defmethod frontend-resize-view :after ((frontend web-frontend) view rows columns)
  (declare (ignore view))
  (web-broadcast frontend (json-object "t" "resize" "rows" rows "cols" columns)))

(defmethod frontend-set-mouse-tracking :after ((frontend web-frontend) view tracking)
  (declare (ignore view))
  (web-broadcast frontend (json-object "t" "mouse" "tracking" tracking)))

(defmethod frontend-request-exit :before ((frontend web-frontend) view)
  (declare (ignore view))
  (web-broadcast frontend (json-object "t" "exit")))

(defun start-web-server (frontend)
  (let* ((resource (make-instance 'web-resource :frontend frontend))
         (dispatcher (lambda (request)
                       (and (string= (hunchentoot:script-name request) "/ws")
                            (token-valid-p frontend request)
                            (web-origin-allowed-p request)
                            resource)))
         (acceptor (make-instance 'web-acceptor
                                  :frontend frontend
                                  :address (web-frontend-host frontend)
                                  :port (web-frontend-port frontend)
                                  :access-log-destination nil)))
    (setf (web-frontend-resource frontend) resource
          (web-frontend-dispatcher frontend) dispatcher
          (web-frontend-acceptor frontend) acceptor)
    (push dispatcher hunchensocket:*websocket-dispatch-table*)
    (hunchentoot:start acceptor)
    (format t "~&slt: serving on ~a~%" (web-url frontend))
    (finish-output)
    frontend))

(defun stop-web-server (frontend)
  (setf hunchensocket:*websocket-dispatch-table*
        (remove (web-frontend-dispatcher frontend) hunchensocket:*websocket-dispatch-table*))
  (when (web-frontend-acceptor frontend)
    (ignore-errors (hunchentoot:stop (web-frontend-acceptor frontend) :soft t))
    (setf (web-frontend-acceptor frontend) nil)))

(defmethod frontend-call-with-event-loop ((frontend web-frontend) thunk)
  (start-web-server frontend)
  (unwind-protect (call-next-method)
    (stop-web-server frontend)))

(register-frontend :web (lambda () (make-instance 'web-frontend)))
