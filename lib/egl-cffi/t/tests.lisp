(defpackage #:egl-cffi/tests
  (:use #:cl)
  (:local-nicknames (#:egl #:egl-cffi) (#:gen #:egl-cffi.generator))
  (:export #:run-tests))

(in-package #:egl-cffi/tests)

(defvar *failures* 0)

(defmacro is (form)
  `(unless ,form
     (incf *failures*)
     (format t "~&FAIL: ~s~%" ',form)))

(defun test-bindings ()
  (is (= egl:+platform-wayland-khr+ #x31D8))
  (is (= egl:+platform-x11-khr+ #x31D5))
  (is (= egl:+none+ #x3038))
  (is (fboundp 'egl:get-platform-display))
  (is (fboundp 'egl:create-platform-window-surface))
  ;; EGL_CAST(EGLint,-1) is -1.
  (is (= egl:+dont-care+ -1)))

(defun test-display ()
  "Where there is EGL: initialize a display with no window system (Mesa's
surfaceless platform) when it has one."
  (if (ignore-errors (egl:load-egl))
      (let ((display (egl:get-display (cffi:null-pointer))))
        (format t "~&egl-cffi: default display ~a~%" display)
        (is (cffi:pointerp display)))
      (format t "~&egl-cffi: no EGL; display test skipped~%")))

(defun run-tests ()
  (let ((*failures* 0))
    (test-bindings)
    (test-display)
    (format t "~&egl-cffi: ~:[all tests passed~;~:*~d failure~:p~]~%"
            (and (plusp *failures*) *failures*))
    (zerop *failures*)))
