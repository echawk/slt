(defpackage #:wayland-cffi/tests
  (:use #:cl)
  (:local-nicknames (#:wl #:wayland-cffi) (#:gen #:wayland-cffi.generator))
  (:export #:run-tests))

(in-package #:wayland-cffi/tests)

(defvar *failures* 0)

(defmacro is (form)
  `(unless ,form
     (incf *failures*)
     (format t "~&FAIL: ~s~%" ',form)))

(defun message (interface kind name)
  "The (NAME SIGNATURE TYPES) of INTERFACE's request or event NAME."
  (let ((description (find interface gen::*interface-descriptions*
                           :key #'first :test #'string=)))
    (find name (ecase kind (:request (third description)) (:event (fourth description)))
          :key #'first :test #'string=)))

(defun test-bindings ()
  (is (= wl:+wl-seat-capability-keyboard+ 2))
  (is (= wl:+xdg-toplevel-state-activated+ 4))
  (is (= wl:+wl-data-device-manager-dnd-action-copy+ 1))
  (is (fboundp 'wl:wl-registry-bind))
  (is (fboundp 'wl:xdg-toplevel-set-title))
  (is (fboundp 'wl:zxdg-decoration-manager-v1-get-toplevel-decoration))
  ;; As wayland-scanner writes them.
  (is (equal (message "wl_registry" :request "bind") '("bind" "usun" (nil nil nil nil))))
  (is (equal (message "wl_surface" :request "attach")
             '("attach" "?oii" ("wl_buffer" nil nil))))
  (is (equal (second (message "wl_surface" :request "set_buffer_scale")) "3i"))
  (is (equal (message "wl_data_device" :event "data_offer")
             '("data_offer" "n" ("wl_data_offer")))))

(defun test-display ()
  "Where there is a compositor: bind wl_compositor from the registry."
  (if (and (uiop:getenv "WAYLAND_DISPLAY") (ignore-errors (wl:load-wayland)))
      (let ((display (wl:display-connect))
            (globals '()))
        (is display)
        (when display
          (let ((registry (wl:wl-display-get-registry display)))
            (wl:set-listener registry
                             (lambda (event &rest arguments)
                               (when (eq event :global)
                                 (push (second arguments) globals))))
            (wl:check-display (wl:display-roundtrip display) display "roundtrip")
            (format t "~&wayland-cffi: ~d globals~%" (length globals))
            (is (member "wl_compositor" globals :test #'string=))
            (wl:destroy-proxy registry)
            (wl:display-disconnect display))))
      (format t "~&wayland-cffi: no compositor; display test skipped~%")))

(defun run-tests ()
  (let ((*failures* 0))
    (test-bindings)
    (test-display)
    (format t "~&wayland-cffi: ~:[all tests passed~;~:*~d failure~:p~]~%"
            (and (plusp *failures*) *failures*))
    (zerop *failures*)))
