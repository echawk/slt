(defpackage #:vulkan-cffi/tests
  (:use #:cl)
  (:local-nicknames (#:vk #:vulkan-cffi) (#:gen #:vulkan-cffi.generator))
  (:export #:run-tests))

(in-package #:vulkan-cffi/tests)

(defvar *failures* 0)

(defmacro is (form)
  `(unless ,form
     (incf *failures*)
     (format t "~&FAIL: ~s~%" ',form)))

(defun test-names ()
  (is (string= "instance-create-info" (gen::lisp-words "InstanceCreateInfo")))
  (is (string= "p-next" (gen::lisp-words "pNext")))
  (is (string= "device-id" (gen::lisp-words "deviceID")))
  (is (string= "surface-khr" (gen::lisp-words "SurfaceKHR")))
  (is (string= "extent2d" (gen::lisp-words "Extent2D")))
  (is (string= "physical-device-vulkan11-features"
               (gen::lisp-words "PhysicalDeviceVulkan11Features")))
  (is (eq 'vk:create-instance (gen::command-symbol "vkCreateInstance")))
  (is (eq 'vk:+format-b8g8r8a8-unorm+ (gen::constant-symbol "VK_FORMAT_B8G8R8A8_UNORM")))
  ;; Vulkan's names are not COMMON-LISP's.
  (is (not (eq 'vk::format 'cl:format))))

(defun test-values ()
  (is (= 256 (gen::parse-value "256")))
  (is (= #xFFFFFFFF (gen::parse-value "(~0U)")))
  (is (= #xFFFFFFFE (gen::parse-value "(~1U)")))
  (is (= #xFFFFFFFFFFFFFFFF (gen::parse-value "(~0ULL)")))
  (is (= 1000.0 (gen::parse-value "1000.0F")))
  (is (= #x10 (gen::parse-value "0x00000010")))
  (is (string= "VK_KHR_surface" (gen::parse-value "\"VK_KHR_surface\"")))
  (is (= vk:+whole-size+ #xFFFFFFFFFFFFFFFF))
  (is (= vk:+error-out-of-date-khr+ -1000001004))
  (is (= (vk:structure-type-of 'vk:instance-create-info)
         vk:+structure-type-instance-create-info+))
  (is (= (vk:make-api-version 0 1 3 0) 4206592)))

(defun test-depends ()
  (let ((names (make-hash-table :test 'equal)))
    (setf (gethash "A" names) t (gethash "B" names) t)
    (is (gen::depends-satisfied-p "A" names))
    (is (not (gen::depends-satisfied-p "C" names)))
    (is (gen::depends-satisfied-p "A+B" names))
    (is (not (gen::depends-satisfied-p "A+C" names)))
    (is (gen::depends-satisfied-p "C,B" names))
    (is (gen::depends-satisfied-p "(C,A)+B" names))
    (is (not (gen::depends-satisfied-p "(C+A),(C+B)" names)))))

(defun device-names ()
  "The names of the physical devices a new instance enumerates, creating
it with portability enumeration when the loader offers it (MoltenVK)."
  (vk:load-vulkan)
  (cffi:with-foreign-objects ((count :uint32) (instance 'vk:instance))
    (vk:check-result (vk:enumerate-instance-extension-properties
                      (cffi:null-pointer) count (cffi:null-pointer)))
    (let* ((available (cffi:mem-ref count :uint32))
           (portability-p
             (cffi:with-foreign-object (properties '(:struct vk:extension-properties) available)
               (vk:enumerate-instance-extension-properties (cffi:null-pointer) count properties)
               (loop for i below available
                     thereis (string= vk:+khr-portability-enumeration-extension-name+
                                      (cffi:foreign-string-to-lisp
                                       (cffi:foreign-slot-pointer
                                        (cffi:mem-aptr properties '(:struct vk:extension-properties) i)
                                        '(:struct vk:extension-properties) 'vk:extension-name)))))))
      (cffi:with-foreign-string (extension vk:+khr-portability-enumeration-extension-name+)
        (cffi:with-foreign-object (extensions :pointer)
          (setf (cffi:mem-ref extensions :pointer) extension)
          (vk:with-structs ((application vk:application-info
                                         :api-version (vk:make-api-version 0 1 0 0))
                            (info vk:instance-create-info
                                  :p-application-info application
                                  :flags (if portability-p
                                             vk:+instance-create-enumerate-portability-bit-khr+
                                             0)
                                  :enabled-extension-count (if portability-p 1 0)
                                  :pp-enabled-extension-names extensions))
            (vk:check-result (vk:create-instance info (cffi:null-pointer) instance)))))
      (let ((instance (cffi:mem-ref instance 'vk:instance)))
        (unwind-protect
             (progn
               (vk:load-instance-commands instance)
               (vk:check-result (vk:enumerate-physical-devices instance count (cffi:null-pointer)))
               (let ((devices (cffi:mem-ref count :uint32)))
                 (cffi:with-foreign-objects ((handles 'vk:physical-device devices)
                                             (properties '(:struct vk:physical-device-properties)))
                   (vk:enumerate-physical-devices instance count handles)
                   (loop for i below devices
                         collect (progn
                                   (vk:get-physical-device-properties
                                    (cffi:mem-aref handles 'vk:physical-device i) properties)
                                   (cffi:foreign-string-to-lisp
                                    (cffi:foreign-slot-pointer
                                     properties '(:struct vk:physical-device-properties)
                                     'vk:device-name)))))))
          (vk:destroy-instance instance (cffi:null-pointer)))))))

(defun test-instance ()
  ;; Only where there is a Vulkan loader; MoltenVK computes with floats
  ;; that would trap in Lisp.
  (if (ignore-errors (cffi:load-foreign-library 'gen::vulkan-loader))
      (let ((names (float-features:with-float-traps-masked t (device-names))))
        (format t "~&vulkan-cffi: devices ~s~%" names)
        (is (every #'stringp names)))
      (format t "~&vulkan-cffi: no Vulkan loader; instance test skipped~%")))

(defun run-tests ()
  (let ((*failures* 0))
    (test-names)
    (test-values)
    (test-depends)
    (test-instance)
    (format t "~&vulkan-cffi: ~:[all tests passed~;~:*~d failure~:p~]~%"
            (and (plusp *failures*) *failures*))
    (zerop *failures*)))
