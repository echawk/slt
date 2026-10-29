(in-package #:slt)

;;;; Vulkan renderer: the OpenGL renderer's design in Vulkan 1.0.  The grid
;;;; is one instanced draw, six vertices per cell from the vertex index and
;;;; each instance a cell's four packed words from PACK-GRID; glyphs come
;;;; from an R8 atlas image, color ones from an RGBA one.  Terminal images
;;;; are textured quads.  One frame is in flight at a time: each frame
;;;; waits for the last, so buffers and staging memory are simply reused.
;;;;
;;;; The renderer draws into a swapchain on a surface its host makes (a
;;;; GLFW window, a CAMetalLayer, a Wayland or X11 window), or with no
;;;; surface only offscreen, for snapshots.  Calls go through vulkan-cffi,
;;;; generated from the Vulkan registry.

(defmacro define-shader (name file)
  "Define NAME as the SPIR-V in src/vulkan/shaders/FILE, read when this is
compiled.  The .spv files are compiled from their GLSL with glslc (make
shaders)."
  `(defparameter ,name
     ,(let ((path (merge-pathnames (concatenate 'string "shaders/" file)
                                   (or *compile-file-truename* *load-truename*))))
        (with-open-file (in path :element-type '(unsigned-byte 8))
          (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
            (read-sequence octets in)
            octets)))))

(define-shader *vulkan-cells-vertex-shader* "cells.vert.spv")
(define-shader *vulkan-cells-fragment-shader* "cells.frag.spv")
(define-shader *vulkan-image-vertex-shader* "image.vert.spv")
(define-shader *vulkan-image-fragment-shader* "image.frag.spv")

(defvar *vulkan-validation* nil
  "When true, enable the Khronos validation layer, if installed.")

(defparameter +vulkan-validation-layer+ "VK_LAYER_KHRONOS_validation")
(defparameter +vulkan-shader-entry+ "main")

(defconstant +vulkan-target-format+ vk:+format-b8g8r8a8-unorm+
  "The format drawn into, swapchain and snapshot alike: blending in UNORM
matches the other renderers, which do not convert to sRGB.")

(defconstant +vulkan-cell-octets+ (* 4 +gpu-words-per-cell+))
(defconstant +vulkan-cell-vertices+ 6 "Two triangles per cell.")
(defconstant +vulkan-image-vertex-floats+ 4 "x, y, u, v.")
(defconstant +vulkan-image-vertices+ 6)
(defconstant +vulkan-max-images+ 256
  "Terminal images drawn at once, each with a descriptor set of its own.")
(defconstant +vulkan-color-components+
  (logior vk:+color-component-r-bit+ vk:+color-component-g-bit+
          vk:+color-component-b-bit+ vk:+color-component-a-bit+))
(defconstant +vulkan-no-timeout+ (ldb (byte 64 0) -1))
(defconstant +vulkan-null-handle+ 0 "A non-dispatchable handle to nothing.")
(defconstant +vulkan-undefined-extent+ (ldb (byte 32 0) -1)
  "The width of a surface whose size the swapchain decides.")
(defconstant +rgb-channels+ 3)
(defconstant +rgba-channels+ 4)
(defconstant +channel-maximum+ 255.0)

;; The push constants of the cells' shaders, laid out as their Frame block.
(cffi:defcstruct vulkan-frame-constants
  (cursor-color :float :count 4)
  (cell-size :float :count 2)
  (viewport :float :count 2)
  (columns :uint32)
  (atlas-columns :uint32)
  (layer :uint32))

;;; Calling Vulkan

(defmacro vk-call (form)
  "Run the Vulkan call FORM, signalling VULKAN-ERROR for an error result."
  `(vk:check-result ,form ',(first form)))

(defmacro receive ((var type) &body body)
  "Run BODY with VAR pointing to room for a TYPE, and return what it holds."
  `(cffi:with-foreign-object (,var ',type)
     ,@body
     (cffi:mem-ref ,var ',type)))

(defun call-with-string-array (strings function)
  "Call FUNCTION with a foreign array of STRINGS as C strings."
  (let ((pointers (mapcar #'cffi:foreign-string-alloc strings)))
    (unwind-protect
         (cffi:with-foreign-object (array :pointer (max 1 (length strings)))
           (loop for pointer in pointers
                 for i from 0
                 do (setf (cffi:mem-aref array :pointer i) pointer))
           (funcall function array))
      (mapc #'cffi:foreign-string-free pointers))))

(defun foreign-name (pointer type slot)
  "The C string in the array SLOT of the TYPE structure at POINTER."
  (cffi:foreign-string-to-lisp (cffi:foreign-slot-pointer pointer type slot)))

(defmacro enumerate-into ((pointer type count) call &body body)
  "Run the Vulkan enumeration CALL twice, as its count and as its items: it
refers to COUNT (a pointer to the count) and POINTER (the items, null the
first time); then run BODY with POINTER to the COUNT items, COUNT now
their number."
  `(cffi:with-foreign-object (,count :uint32)
     (let ((,pointer (cffi:null-pointer)))
       (vk-call ,call))
     (let ((items (cffi:mem-ref ,count :uint32)))
       (cffi:with-foreign-object (,pointer ',type (max 1 items))
         (vk-call ,call)
         (let ((,count (cffi:mem-ref ,count :uint32)))
           (declare (ignorable ,count))
           ,@body)))))

(defun extension-names (enumerate)
  "The extension names ENUMERATE lists: called with the count's pointer and
the properties' pointer, as vkEnumerate*ExtensionProperties is."
  (enumerate-into (properties (:struct vk:extension-properties) count)
      (funcall enumerate count properties)
    (loop for i below count
          collect (foreign-name (cffi:mem-aptr properties '(:struct vk:extension-properties) i)
                                '(:struct vk:extension-properties) 'vk:extension-name))))

(defun layer-names ()
  (enumerate-into (properties (:struct vk:layer-properties) count)
      (vk:enumerate-instance-layer-properties count properties)
    (loop for i below count
          collect (foreign-name (cffi:mem-aptr properties '(:struct vk:layer-properties) i)
                                '(:struct vk:layer-properties) 'vk:layer-name))))

;;; The renderer's state

(defstruct vulkan-buffer
  (handle +vulkan-null-handle+) (memory +vulkan-null-handle+)
  (pointer (cffi:null-pointer)) (size 0))

(defstruct vulkan-texture
  (image +vulkan-null-handle+) (memory +vulkan-null-handle+) (view +vulkan-null-handle+)
  (format vk:+format-r8g8b8a8-unorm+) (width 0) (height 0)
  ;; Host memory the pixels are copied through.
  (staging nil)
  ;; Whether the image holds pixels yet, in the shader-read layout.
  (ready-p nil)
  ;; The atlas generation and height the texture was made for.
  (generation -1) (descriptor-set +vulkan-null-handle+))

(defstruct (vulkan-renderer (:constructor %make-vulkan-renderer))
  (instance (cffi:null-pointer))
  (physical-device (cffi:null-pointer))
  (device (cffi:null-pointer))
  (queue (cffi:null-pointer))
  (queue-family 0)
  (surface +vulkan-null-handle+)
  ;; The render passes: one leaving the image for presenting, one for
  ;; copying out.
  (present-pass +vulkan-null-handle+)
  (snapshot-pass +vulkan-null-handle+)
  (sampler +vulkan-null-handle+)
  (image-sampler +vulkan-null-handle+)
  (cells-set-layout +vulkan-null-handle+)
  (image-set-layout +vulkan-null-handle+)
  (cells-layout +vulkan-null-handle+)
  (image-layout +vulkan-null-handle+)
  ;; Cells drawn whole, cells in layers (blended), and images.
  (cells-pipeline +vulkan-null-handle+)
  (layer-pipeline +vulkan-null-handle+)
  (image-pipeline +vulkan-null-handle+)
  (descriptor-pool +vulkan-null-handle+)
  (cells-set +vulkan-null-handle+)
  (command-pool +vulkan-null-handle+)
  (command-buffer (cffi:null-pointer))
  (fence +vulkan-null-handle+)
  (acquired +vulkan-null-handle+)
  (atlas nil)
  (color-atlas nil)
  (instances nil)
  (image-vertices nil)
  ;; Image id to (RGBA TEXTURE): the texture holds RGBA.
  (image-textures (make-hash-table))
  ;; The swapchain, its images' views, framebuffers and semaphores, and
  ;; its size; rebuilt when the window's size changes.
  (swapchain +vulkan-null-handle+)
  (swapchain-views '())
  (framebuffers (vector))
  (finished (vector))
  (swapchain-width 0)
  (swapchain-height 0)
  (present-mode vk:+present-mode-fifo-khr+)
  ;; The offscreen target snapshots draw into, and its read-back buffer.
  (target nil)
  (target-framebuffer +vulkan-null-handle+)
  (readback nil))

;;; Instance and device

(defun create-vulkan-instance (extensions)
  "An instance with EXTENSIONS, enumerating portability drivers (MoltenVK)
where the loader offers them."
  (let* ((available (extension-names (lambda (count properties)
                                       (vk:enumerate-instance-extension-properties
                                        (cffi:null-pointer) count properties))))
         (portability-p (member vk:+khr-portability-enumeration-extension-name+ available
                                :test #'string=))
         (extensions (remove-duplicates
                      (append extensions
                              (and portability-p
                                   (list vk:+khr-portability-enumeration-extension-name+)))
                      :test #'string=))
         (layers (and *vulkan-validation*
                      (member +vulkan-validation-layer+ (layer-names) :test #'string=)
                      (list +vulkan-validation-layer+))))
    (call-with-string-array
     extensions
     (lambda (extension-array)
       (call-with-string-array
        layers
        (lambda (layer-array)
          (cffi:with-foreign-string (name "slt")
            (vk:with-structs ((application vk:application-info
                                           :p-application-name name
                                           :p-engine-name name
                                           :api-version (vk:make-api-version 0 1 0 0))
                              (info vk:instance-create-info
                                    :flags (if portability-p
                                               vk:+instance-create-enumerate-portability-bit-khr+
                                               0)
                                    :p-application-info application
                                    :enabled-layer-count (length layers)
                                    :pp-enabled-layer-names layer-array
                                    :enabled-extension-count (length extensions)
                                    :pp-enabled-extension-names extension-array))
              (receive (instance vk:instance)
                (vk-call (vk:create-instance info (cffi:null-pointer) instance)))))))))))

(defun physical-devices (instance)
  (enumerate-into (devices vk:physical-device count)
      (vk:enumerate-physical-devices instance count devices)
    (loop for i below count collect (cffi:mem-aref devices 'vk:physical-device i))))

(defun graphics-queue-family (physical-device surface)
  "A queue family of PHYSICAL-DEVICE that draws and, given a SURFACE,
presents to it; or NIL."
  (enumerate-into (families (:struct vk:queue-family-properties) count)
      (progn (vk:get-physical-device-queue-family-properties physical-device count families)
             vk:+success+)
    (loop for i below count
          when (and (logtest vk:+queue-graphics-bit+
                             (cffi:foreign-slot-value
                              (cffi:mem-aptr families '(:struct vk:queue-family-properties) i)
                              '(:struct vk:queue-family-properties) 'vk:queue-flags))
                    (or (eql surface +vulkan-null-handle+)
                        (= vk:+true+
                           (receive (supported vk:bool32)
                             (vk-call (vk:get-physical-device-surface-support-khr
                                       physical-device i surface supported))))))
            return i)))

(defun create-vulkan-device (renderer)
  "Pick the first device that can draw (and present), and create it with
one queue."
  (let ((instance (vulkan-renderer-instance renderer))
        (surface (vulkan-renderer-surface renderer)))
    (destructuring-bind (physical-device family)
        (or (loop for device in (physical-devices instance)
                  for family = (graphics-queue-family device surface)
                  when family return (list device family))
            (error "No Vulkan device can draw~:[~; to this window~]"
                   (/= surface +vulkan-null-handle+)))
      (let* ((available (extension-names (lambda (count properties)
                                           (vk:enumerate-device-extension-properties
                                            physical-device (cffi:null-pointer) count properties))))
             (extensions (append (and (/= surface +vulkan-null-handle+)
                                      (list vk:+khr-swapchain-extension-name+))
                                 ;; A portability driver must have it enabled.
                                 (and (member vk:+khr-portability-subset-extension-name+ available
                                              :test #'string=)
                                      (list vk:+khr-portability-subset-extension-name+)))))
        (cffi:with-foreign-object (priority :float)
          (setf (cffi:mem-ref priority :float) 1.0)
          (call-with-string-array
           extensions
           (lambda (extension-array)
             (vk:with-structs ((queue-info vk:device-queue-create-info
                                           :queue-family-index family
                                           :queue-count 1
                                           :p-queue-priorities priority)
                               (info vk:device-create-info
                                     :queue-create-info-count 1
                                     :p-queue-create-infos queue-info
                                     :enabled-extension-count (length extensions)
                                     :pp-enabled-extension-names extension-array))
               (let ((device (receive (device vk:device)
                               (vk-call (vk:create-device physical-device info
                                                          (cffi:null-pointer) device)))))
                 (vk:load-device-commands device)
                 (setf (vulkan-renderer-physical-device renderer) physical-device
                       (vulkan-renderer-queue-family renderer) family
                       (vulkan-renderer-device renderer) device
                       (vulkan-renderer-queue renderer)
                       (receive (queue vk:queue)
                         (vk:get-device-queue device family 0 queue))))))))))))

;;; Memory

(defun memory-type (renderer type-bits properties)
  "The index of a memory type among TYPE-BITS with all of PROPERTIES."
  (cffi:with-foreign-object (memory '(:struct vk:physical-device-memory-properties))
    (vk:get-physical-device-memory-properties (vulkan-renderer-physical-device renderer) memory)
    (loop with types = (cffi:foreign-slot-pointer
                        memory '(:struct vk:physical-device-memory-properties) 'vk:memory-types)
          for i below (cffi:foreign-slot-value
                       memory '(:struct vk:physical-device-memory-properties) 'vk:memory-type-count)
          for flags = (cffi:foreign-slot-value (cffi:mem-aptr types '(:struct vk:memory-type) i)
                                               '(:struct vk:memory-type) 'vk:property-flags)
          when (and (logbitp i type-bits) (= properties (logand flags properties)))
            return i
          finally (error "No Vulkan memory type has properties ~b" properties))))

(defun allocate-memory (renderer requirements properties)
  "Memory for REQUIREMENTS (a VkMemoryRequirements) with PROPERTIES."
  (vk:with-struct (info vk:memory-allocate-info
                        :allocation-size (cffi:foreign-slot-value
                                          requirements '(:struct vk:memory-requirements) 'vk:size)
                        :memory-type-index (memory-type
                                            renderer
                                            (cffi:foreign-slot-value
                                             requirements '(:struct vk:memory-requirements)
                                             'vk:memory-type-bits)
                                            properties))
    (receive (memory vk:device-memory)
      (vk-call (vk:allocate-memory (vulkan-renderer-device renderer) info
                                   (cffi:null-pointer) memory)))))

(defun create-host-buffer (renderer size usage)
  "A buffer of SIZE octets for USAGE in memory the host writes and reads,
mapped."
  (let ((device (vulkan-renderer-device renderer)))
    (vk:with-struct (info vk:buffer-create-info
                          :size size :usage usage :sharing-mode vk:+sharing-mode-exclusive+)
      (let ((buffer (receive (buffer vk:buffer)
                      (vk-call (vk:create-buffer device info (cffi:null-pointer) buffer)))))
        (cffi:with-foreign-object (requirements '(:struct vk:memory-requirements))
          (vk:get-buffer-memory-requirements device buffer requirements)
          (let ((memory (allocate-memory renderer requirements
                                         (logior vk:+memory-property-host-visible-bit+
                                                 vk:+memory-property-host-coherent-bit+))))
            (vk-call (vk:bind-buffer-memory device buffer memory 0))
            (make-vulkan-buffer
             :handle buffer :memory memory :size size
             :pointer (receive (pointer :pointer)
                        (vk-call (vk:map-memory device memory 0 vk:+whole-size+ 0 pointer))))))))))

(defun free-buffer (renderer buffer)
  (when buffer
    (let ((device (vulkan-renderer-device renderer)))
      (vk:destroy-buffer device (vulkan-buffer-handle buffer) (cffi:null-pointer))
      (vk:free-memory device (vulkan-buffer-memory buffer) (cffi:null-pointer)))))

(defun ensure-host-buffer (renderer buffer size usage)
  "BUFFER if it holds SIZE octets, else a new one, BUFFER freed."
  (if (and buffer (<= size (vulkan-buffer-size buffer)))
      buffer
      (progn (free-buffer renderer buffer)
             ;; Grow by half again, so a growing grid does not reallocate
             ;; every frame.
             (create-host-buffer renderer (max size (floor (* 3 size) 2)) usage))))

(defun copy-into-buffer (buffer octets count &key (offset 0))
  "Copy COUNT octets of the Lisp vector OCTETS (any element type, its data
contiguous) into BUFFER's memory at OFFSET."
  (sb-sys:with-pinned-objects (octets)
    (cffi:foreign-funcall "memcpy"
                          :pointer (cffi:inc-pointer (vulkan-buffer-pointer buffer) offset)
                          :pointer (sb-sys:vector-sap octets)
                          :size count
                          :pointer)))

;;; Images

(defun set-color-range (range)
  "Make the VkImageSubresourceRange at RANGE the whole of a color image."
  (cffi:with-foreign-slots ((vk:aspect-mask vk:base-mip-level vk:level-count
                             vk:base-array-layer vk:layer-count)
                            range (:struct vk:image-subresource-range))
    (setf vk:aspect-mask vk:+image-aspect-color-bit+
          vk:base-mip-level 0 vk:level-count 1
          vk:base-array-layer 0 vk:layer-count 1)))

(defun create-image (renderer width height format usage)
  "A device-local 2D image, its memory and a view of it."
  (let ((device (vulkan-renderer-device renderer)))
    (vk:with-struct (info vk:image-create-info
                          :image-type vk:+image-type-2d+ :format format
                          :mip-levels 1 :array-layers 1 :samples vk:+sample-count-1-bit+
                          :tiling vk:+image-tiling-optimal+ :usage usage
                          :sharing-mode vk:+sharing-mode-exclusive+
                          :initial-layout vk:+image-layout-undefined+)
      (cffi:with-foreign-slots ((vk:width vk:height vk:depth)
                                (cffi:foreign-slot-pointer info '(:struct vk:image-create-info)
                                                           'vk:extent)
                                (:struct vk:extent3d))
        (setf vk:width width vk:height height vk:depth 1))
      (let ((image (receive (image vk:image)
                     (vk-call (vk:create-image device info (cffi:null-pointer) image)))))
        (cffi:with-foreign-object (requirements '(:struct vk:memory-requirements))
          (vk:get-image-memory-requirements device image requirements)
          (let ((memory (allocate-memory renderer requirements
                                         vk:+memory-property-device-local-bit+)))
            (vk-call (vk:bind-image-memory device image memory 0))
            (values image memory (create-image-view renderer image format))))))))

(defun create-image-view (renderer image format)
  (vk:with-struct (info vk:image-view-create-info
                        :image image :view-type vk:+image-view-type-2d+ :format format)
    (set-color-range (cffi:foreign-slot-pointer info '(:struct vk:image-view-create-info)
                                                'vk:subresource-range))
    (receive (view vk:image-view)
      (vk-call (vk:create-image-view (vulkan-renderer-device renderer) info
                                     (cffi:null-pointer) view)))))

(defun image-barrier (command-buffer image old-layout new-layout
                      source-access target-access source-stage target-stage)
  "Record moving IMAGE from OLD-LAYOUT to NEW-LAYOUT after SOURCE-ACCESS in
SOURCE-STAGE, before TARGET-ACCESS in TARGET-STAGE."
  (vk:with-struct (barrier vk:image-memory-barrier
                           :src-access-mask source-access :dst-access-mask target-access
                           :old-layout old-layout :new-layout new-layout
                           :src-queue-family-index vk:+queue-family-ignored+
                           :dst-queue-family-index vk:+queue-family-ignored+
                           :image image)
    (set-color-range (cffi:foreign-slot-pointer barrier '(:struct vk:image-memory-barrier)
                                                'vk:subresource-range))
    (vk:cmd-pipeline-barrier command-buffer source-stage target-stage 0
                             0 (cffi:null-pointer) 0 (cffi:null-pointer) 1 barrier)))

(defun format-octets (format)
  (if (= format vk:+format-r8-unorm+) 1 +rgba-channels+))

(defun create-texture (renderer width height format)
  (multiple-value-bind (image memory view)
      (create-image renderer width height format
                    (logior vk:+image-usage-sampled-bit+ vk:+image-usage-transfer-dst-bit+))
    (make-vulkan-texture :image image :memory memory :view view :format format
                         :width width :height height)))

(defun free-texture (renderer texture)
  (when texture
    (let ((device (vulkan-renderer-device renderer)))
      (vk:destroy-image-view device (vulkan-texture-view texture) (cffi:null-pointer))
      (vk:destroy-image device (vulkan-texture-image texture) (cffi:null-pointer))
      (vk:free-memory device (vulkan-texture-memory texture) (cffi:null-pointer))
      (free-buffer renderer (vulkan-texture-staging texture))
      (unless (eql +vulkan-null-handle+ (vulkan-texture-descriptor-set texture))
        (cffi:with-foreign-object (set 'vk:descriptor-set)
          (setf (cffi:mem-ref set 'vk:descriptor-set) (vulkan-texture-descriptor-set texture))
          (vk:free-descriptor-sets device (vulkan-renderer-descriptor-pool renderer) 1 set))))))

(defun upload-texture-rows (renderer texture pixels start end)
  "Record copying rows START to END of PIXELS (the whole image's, row
after row) into TEXTURE, through its staging buffer."
  (let* ((command-buffer (vulkan-renderer-command-buffer renderer))
         (row-octets (* (vulkan-texture-width texture) (format-octets (vulkan-texture-format texture))))
         (count (* row-octets (- end start))))
    (setf (vulkan-texture-staging texture)
          (ensure-host-buffer renderer (vulkan-texture-staging texture)
                              (* row-octets (vulkan-texture-height texture))
                              vk:+buffer-usage-transfer-src-bit+))
    (sb-sys:with-pinned-objects (pixels)
      (cffi:foreign-funcall "memcpy"
                            :pointer (vulkan-buffer-pointer (vulkan-texture-staging texture))
                            :pointer (sb-sys:sap+ (sb-sys:vector-sap pixels) (* start row-octets))
                            :size count :pointer))
    (image-barrier command-buffer (vulkan-texture-image texture)
                   (if (vulkan-texture-ready-p texture)
                       vk:+image-layout-shader-read-only-optimal+
                       vk:+image-layout-undefined+)
                   vk:+image-layout-transfer-dst-optimal+
                   (if (vulkan-texture-ready-p texture) vk:+access-shader-read-bit+ 0)
                   vk:+access-transfer-write-bit+
                   (if (vulkan-texture-ready-p texture)
                       vk:+pipeline-stage-fragment-shader-bit+
                       vk:+pipeline-stage-top-of-pipe-bit+)
                   vk:+pipeline-stage-transfer-bit+)
    (vk:with-struct (region vk:buffer-image-copy)
      (cffi:with-foreign-slots ((vk:aspect-mask vk:mip-level vk:base-array-layer vk:layer-count)
                                (cffi:foreign-slot-pointer region '(:struct vk:buffer-image-copy)
                                                           'vk:image-subresource)
                                (:struct vk:image-subresource-layers))
        (setf vk:aspect-mask vk:+image-aspect-color-bit+ vk:mip-level 0
              vk:base-array-layer 0 vk:layer-count 1))
      (cffi:with-foreign-slots ((vk:x vk:y vk:z)
                                (cffi:foreign-slot-pointer region '(:struct vk:buffer-image-copy)
                                                           'vk:image-offset)
                                (:struct vk:offset3d))
        (setf vk:x 0 vk:y start vk:z 0))
      (cffi:with-foreign-slots ((vk:width vk:height vk:depth)
                                (cffi:foreign-slot-pointer region '(:struct vk:buffer-image-copy)
                                                           'vk:image-extent)
                                (:struct vk:extent3d))
        (setf vk:width (vulkan-texture-width texture) vk:height (- end start) vk:depth 1))
      (vk:cmd-copy-buffer-to-image command-buffer
                                   (vulkan-buffer-handle (vulkan-texture-staging texture))
                                   (vulkan-texture-image texture)
                                   vk:+image-layout-transfer-dst-optimal+ 1 region))
    (image-barrier command-buffer (vulkan-texture-image texture)
                   vk:+image-layout-transfer-dst-optimal+
                   vk:+image-layout-shader-read-only-optimal+
                   vk:+access-transfer-write-bit+ vk:+access-shader-read-bit+
                   vk:+pipeline-stage-transfer-bit+ vk:+pipeline-stage-fragment-shader-bit+)
    (setf (vulkan-texture-ready-p texture) t)))

;;; Descriptors

(defun write-texture-descriptor (renderer set binding texture sampler)
  (vk:with-struct (image-info vk:descriptor-image-info
                              :sampler sampler
                              :image-view (vulkan-texture-view texture)
                              :image-layout vk:+image-layout-shader-read-only-optimal+)
    (vk:with-struct (write vk:write-descriptor-set
                           :dst-set set :dst-binding binding :descriptor-count 1
                           :descriptor-type vk:+descriptor-type-combined-image-sampler+
                           :p-image-info image-info)
      (vk:update-descriptor-sets (vulkan-renderer-device renderer) 1 write 0 (cffi:null-pointer)))))

(defun create-set-layout (renderer bindings)
  "A descriptor set layout of BINDINGS sampled images for the fragment
shader."
  (cffi:with-foreign-object (layout-bindings '(:struct vk:descriptor-set-layout-binding) bindings)
    (dotimes (i bindings)
      (let ((binding (cffi:mem-aptr layout-bindings '(:struct vk:descriptor-set-layout-binding) i)))
        (vk:zero-struct binding '(:struct vk:descriptor-set-layout-binding))
        (cffi:with-foreign-slots ((vk:binding vk:descriptor-type vk:descriptor-count
                                   vk:stage-flags)
                                  binding (:struct vk:descriptor-set-layout-binding))
          (setf vk:binding i
                vk:descriptor-type vk:+descriptor-type-combined-image-sampler+
                vk:descriptor-count 1
                vk:stage-flags vk:+shader-stage-fragment-bit+))))
    (vk:with-struct (info vk:descriptor-set-layout-create-info
                          :binding-count bindings :p-bindings layout-bindings)
      (receive (layout vk:descriptor-set-layout)
        (vk-call (vk:create-descriptor-set-layout (vulkan-renderer-device renderer) info
                                                  (cffi:null-pointer) layout))))))

(defun allocate-descriptor-set (renderer layout)
  (cffi:with-foreign-object (layouts 'vk:descriptor-set-layout)
    (setf (cffi:mem-ref layouts 'vk:descriptor-set-layout) layout)
    (vk:with-struct (info vk:descriptor-set-allocate-info
                          :descriptor-pool (vulkan-renderer-descriptor-pool renderer)
                          :descriptor-set-count 1 :p-set-layouts layouts)
      (receive (set vk:descriptor-set)
        (vk-call (vk:allocate-descriptor-sets (vulkan-renderer-device renderer) info set))))))

(defun create-descriptor-pool (renderer)
  (vk:with-struct (size vk:descriptor-pool-size
                        :type vk:+descriptor-type-combined-image-sampler+
                        ;; The cells' two atlases, and an image each.
                        :descriptor-count (+ 2 +vulkan-max-images+))
    (vk:with-struct (info vk:descriptor-pool-create-info
                          :flags vk:+descriptor-pool-create-free-descriptor-set-bit+
                          :max-sets (1+ +vulkan-max-images+)
                          :pool-size-count 1 :p-pool-sizes size)
      (receive (pool vk:descriptor-pool)
        (vk-call (vk:create-descriptor-pool (vulkan-renderer-device renderer) info
                                            (cffi:null-pointer) pool))))))

(defun create-sampler (renderer filter)
  (vk:with-struct (info vk:sampler-create-info
                        :mag-filter filter :min-filter filter
                        :mipmap-mode vk:+sampler-mipmap-mode-nearest+
                        :address-mode-u vk:+sampler-address-mode-clamp-to-edge+
                        :address-mode-v vk:+sampler-address-mode-clamp-to-edge+
                        :address-mode-w vk:+sampler-address-mode-clamp-to-edge+)
    (receive (sampler vk:sampler)
      (vk-call (vk:create-sampler (vulkan-renderer-device renderer) info
                                  (cffi:null-pointer) sampler)))))

;;; Render passes and pipelines

(defun create-render-pass (renderer final-layout)
  "A render pass clearing one color attachment and leaving it in
FINAL-LAYOUT."
  (vk:with-structs ((attachment vk:attachment-description
                                :format +vulkan-target-format+
                                :samples vk:+sample-count-1-bit+
                                :load-op vk:+attachment-load-op-clear+
                                :store-op vk:+attachment-store-op-store+
                                :stencil-load-op vk:+attachment-load-op-dont-care+
                                :stencil-store-op vk:+attachment-store-op-dont-care+
                                :initial-layout vk:+image-layout-undefined+
                                :final-layout final-layout)
                    (reference vk:attachment-reference
                               :attachment 0
                               :layout vk:+image-layout-color-attachment-optimal+)
                    (subpass vk:subpass-description
                             :pipeline-bind-point vk:+pipeline-bind-point-graphics+
                             :color-attachment-count 1 :p-color-attachments reference)
                    ;; Writing waits for the swapchain image to be acquired.
                    (dependency vk:subpass-dependency
                                :src-subpass vk:+subpass-external+ :dst-subpass 0
                                :src-stage-mask vk:+pipeline-stage-color-attachment-output-bit+
                                :dst-stage-mask vk:+pipeline-stage-color-attachment-output-bit+
                                :src-access-mask 0
                                :dst-access-mask vk:+access-color-attachment-write-bit+)
                    (info vk:render-pass-create-info
                          :attachment-count 1 :p-attachments attachment
                          :subpass-count 1 :p-subpasses subpass
                          :dependency-count 1 :p-dependencies dependency))
    (receive (pass vk:render-pass)
      (vk-call (vk:create-render-pass (vulkan-renderer-device renderer) info
                                      (cffi:null-pointer) pass)))))

(defun create-shader-module (renderer code)
  (cffi:with-foreign-object (words :uint8 (length code))
    (dotimes (i (length code))
      (setf (cffi:mem-aref words :uint8 i) (aref code i)))
    (vk:with-struct (info vk:shader-module-create-info :code-size (length code) :p-code words)
      (receive (module vk:shader-module)
        (vk-call (vk:create-shader-module (vulkan-renderer-device renderer) info
                                          (cffi:null-pointer) module))))))

(defun create-pipeline-layout (renderer set-layout push-constant-size)
  (cffi:with-foreign-object (layouts 'vk:descriptor-set-layout)
    (setf (cffi:mem-ref layouts 'vk:descriptor-set-layout) set-layout)
    (vk:with-structs ((range vk:push-constant-range
                             :stage-flags (logior vk:+shader-stage-vertex-bit+
                                                  vk:+shader-stage-fragment-bit+)
                             :offset 0 :size push-constant-size)
                      (info vk:pipeline-layout-create-info
                            :set-layout-count 1 :p-set-layouts layouts
                            :push-constant-range-count (if (plusp push-constant-size) 1 0)
                            :p-push-constant-ranges range))
      (receive (layout vk:pipeline-layout)
        (vk-call (vk:create-pipeline-layout (vulkan-renderer-device renderer) info
                                            (cffi:null-pointer) layout))))))

(defun create-pipeline (renderer layout vertex-code fragment-code
                        &key blend-p stride input-rate format)
  "A pipeline drawing triangle lists with LAYOUT, one vertex attribute of
FORMAT per STRIDE octets at INPUT-RATE, alpha blended when BLEND-P."
  (let ((device (vulkan-renderer-device renderer))
        (vertex (create-shader-module renderer vertex-code))
        (fragment (create-shader-module renderer fragment-code)))
    (unwind-protect
         (cffi:with-foreign-string (entry +vulkan-shader-entry+)
           (cffi:with-foreign-objects ((stages '(:struct vk:pipeline-shader-stage-create-info) 2)
                                       (dynamic 'vk:dynamic-state 2))
             (loop for (stage module) in (list (list vk:+shader-stage-vertex-bit+ vertex)
                                               (list vk:+shader-stage-fragment-bit+ fragment))
                   for i from 0
                   for info = (cffi:mem-aptr stages '(:struct vk:pipeline-shader-stage-create-info) i)
                   do (vk:zero-struct info '(:struct vk:pipeline-shader-stage-create-info))
                      (cffi:with-foreign-slots ((vk:s-type vk:stage vk:module vk:p-name)
                                                info (:struct vk:pipeline-shader-stage-create-info))
                        (setf vk:s-type (vk:structure-type-of 'vk:pipeline-shader-stage-create-info)
                              vk:stage stage vk:module module vk:p-name entry)))
             (setf (cffi:mem-aref dynamic 'vk:dynamic-state 0) vk:+dynamic-state-viewport+
                   (cffi:mem-aref dynamic 'vk:dynamic-state 1) vk:+dynamic-state-scissor+)
             (vk:with-structs ((binding vk:vertex-input-binding-description
                                        :binding 0 :stride stride :input-rate input-rate)
                               (attribute vk:vertex-input-attribute-description
                                          :location 0 :binding 0 :format format :offset 0)
                               (vertex-input vk:pipeline-vertex-input-state-create-info
                                             :vertex-binding-description-count 1
                                             :p-vertex-binding-descriptions binding
                                             :vertex-attribute-description-count 1
                                             :p-vertex-attribute-descriptions attribute)
                               (assembly vk:pipeline-input-assembly-state-create-info
                                         :topology vk:+primitive-topology-triangle-list+)
                               (viewport vk:pipeline-viewport-state-create-info
                                         :viewport-count 1 :scissor-count 1)
                               (raster vk:pipeline-rasterization-state-create-info
                                       :polygon-mode vk:+polygon-mode-fill+
                                       :cull-mode vk:+cull-mode-none+
                                       :front-face vk:+front-face-counter-clockwise+
                                       :line-width 1.0)
                               (multisample vk:pipeline-multisample-state-create-info
                                            :rasterization-samples vk:+sample-count-1-bit+)
                               (blend-attachment vk:pipeline-color-blend-attachment-state
                                                 :blend-enable (if blend-p vk:+true+ vk:+false+)
                                                 :src-color-blend-factor vk:+blend-factor-src-alpha+
                                                 :dst-color-blend-factor
                                                 vk:+blend-factor-one-minus-src-alpha+
                                                 :color-blend-op vk:+blend-op-add+
                                                 :src-alpha-blend-factor vk:+blend-factor-src-alpha+
                                                 :dst-alpha-blend-factor
                                                 vk:+blend-factor-one-minus-src-alpha+
                                                 :alpha-blend-op vk:+blend-op-add+
                                                 :color-write-mask +vulkan-color-components+)
                               (blend vk:pipeline-color-blend-state-create-info
                                      :attachment-count 1 :p-attachments blend-attachment)
                               (dynamic-state vk:pipeline-dynamic-state-create-info
                                              :dynamic-state-count 2 :p-dynamic-states dynamic)
                               (info vk:graphics-pipeline-create-info
                                     :stage-count 2 :p-stages stages
                                     :p-vertex-input-state vertex-input
                                     :p-input-assembly-state assembly
                                     :p-viewport-state viewport
                                     :p-rasterization-state raster
                                     :p-multisample-state multisample
                                     :p-color-blend-state blend
                                     :p-dynamic-state dynamic-state
                                     :layout layout
                                     ;; Compatible with the snapshot pass too.
                                     :render-pass (vulkan-renderer-present-pass renderer)
                                     :subpass 0))
               (receive (pipeline vk:pipeline)
                 (vk-call (vk:create-graphics-pipelines device +vulkan-null-handle+ 1 info
                                                        (cffi:null-pointer) pipeline))))))
      (vk:destroy-shader-module device vertex (cffi:null-pointer))
      (vk:destroy-shader-module device fragment (cffi:null-pointer)))))

;;; Creating and freeing

(defun make-vulkan-renderer (&key instance-extensions surface-function)
  "Create a renderer.  INSTANCE-EXTENSIONS are those its host needs for
its surface; SURFACE-FUNCTION, called with the instance, makes the
surface to present to.  With neither, the renderer draws only offscreen."
  (vk:load-vulkan)
  (let ((renderer (%make-vulkan-renderer)))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (free-vulkan-renderer renderer))))
      (let ((instance (create-vulkan-instance
                       (append instance-extensions
                               (and surface-function (list vk:+khr-surface-extension-name+))))))
        (setf (vulkan-renderer-instance renderer) instance)
        (vk:load-instance-commands instance)
        (when surface-function
          (setf (vulkan-renderer-surface renderer) (funcall surface-function instance))))
      (create-vulkan-device renderer)
      (let ((device (vulkan-renderer-device renderer)))
        (setf (vulkan-renderer-present-pass renderer)
              (create-render-pass renderer (if (eql (vulkan-renderer-surface renderer)
                                                    +vulkan-null-handle+)
                                               vk:+image-layout-transfer-src-optimal+
                                               vk:+image-layout-present-src-khr+))
              (vulkan-renderer-snapshot-pass renderer)
              (create-render-pass renderer vk:+image-layout-transfer-src-optimal+)
              (vulkan-renderer-sampler renderer) (create-sampler renderer vk:+filter-nearest+)
              (vulkan-renderer-image-sampler renderer) (create-sampler renderer vk:+filter-linear+)
              (vulkan-renderer-cells-set-layout renderer) (create-set-layout renderer 2)
              (vulkan-renderer-image-set-layout renderer) (create-set-layout renderer 1)
              (vulkan-renderer-cells-layout renderer)
              (create-pipeline-layout renderer (vulkan-renderer-cells-set-layout renderer)
                                      (cffi:foreign-type-size
                                       '(:struct vulkan-frame-constants)))
              (vulkan-renderer-image-layout renderer)
              (create-pipeline-layout renderer (vulkan-renderer-image-set-layout renderer) 0))
        (flet ((cells-pipeline (blend-p)
                 (create-pipeline renderer (vulkan-renderer-cells-layout renderer)
                                  *vulkan-cells-vertex-shader* *vulkan-cells-fragment-shader*
                                  :blend-p blend-p :stride +vulkan-cell-octets+
                                  :input-rate vk:+vertex-input-rate-instance+
                                  :format vk:+format-r32g32b32a32-uint+)))
          (setf (vulkan-renderer-cells-pipeline renderer) (cells-pipeline nil)
                (vulkan-renderer-layer-pipeline renderer) (cells-pipeline t)
                (vulkan-renderer-image-pipeline renderer)
                (create-pipeline renderer (vulkan-renderer-image-layout renderer)
                                 *vulkan-image-vertex-shader* *vulkan-image-fragment-shader*
                                 :blend-p t
                                 :stride (* +vulkan-image-vertex-floats+
                                            (cffi:foreign-type-size :float))
                                 :input-rate vk:+vertex-input-rate-vertex+
                                 :format vk:+format-r32g32b32a32-sfloat+)))
        (setf (vulkan-renderer-descriptor-pool renderer) (create-descriptor-pool renderer)
              (vulkan-renderer-cells-set renderer)
              (allocate-descriptor-set renderer (vulkan-renderer-cells-set-layout renderer)))
        (vk:with-struct (info vk:command-pool-create-info
                              :flags vk:+command-pool-create-reset-command-buffer-bit+
                              :queue-family-index (vulkan-renderer-queue-family renderer))
          (setf (vulkan-renderer-command-pool renderer)
                (receive (pool vk:command-pool)
                  (vk-call (vk:create-command-pool device info (cffi:null-pointer) pool)))))
        (vk:with-struct (info vk:command-buffer-allocate-info
                              :command-pool (vulkan-renderer-command-pool renderer)
                              :level vk:+command-buffer-level-primary+
                              :command-buffer-count 1)
          (setf (vulkan-renderer-command-buffer renderer)
                (receive (buffer vk:command-buffer)
                  (vk-call (vk:allocate-command-buffers device info buffer)))))
        (vk:with-struct (info vk:fence-create-info :flags vk:+fence-create-signaled-bit+)
          (setf (vulkan-renderer-fence renderer)
                (receive (fence vk:fence)
                  (vk-call (vk:create-fence device info (cffi:null-pointer) fence)))))
        (setf (vulkan-renderer-acquired renderer) (create-semaphore renderer))
        ;; Without color glyphs the shader never reads it; give it a pixel.
        (setf (vulkan-renderer-color-atlas renderer)
              (create-texture renderer 1 1 vk:+format-r8g8b8a8-unorm+))))
    renderer))

(defun create-semaphore (renderer)
  (vk:with-struct (info vk:semaphore-create-info)
    (receive (semaphore vk:semaphore)
      (vk-call (vk:create-semaphore (vulkan-renderer-device renderer) info
                                    (cffi:null-pointer) semaphore)))))

(defun free-vulkan-renderer (renderer)
  (let ((device (vulkan-renderer-device renderer))
        (null (cffi:null-pointer)))
    (unless (cffi:null-pointer-p device)
      (vk:device-wait-idle device)
      (free-swapchain renderer)
      (free-snapshot-target renderer)
      (maphash (lambda (id entry) (declare (ignore id)) (free-texture renderer (second entry)))
               (vulkan-renderer-image-textures renderer))
      (clrhash (vulkan-renderer-image-textures renderer))
      (free-texture renderer (vulkan-renderer-atlas renderer))
      (free-texture renderer (vulkan-renderer-color-atlas renderer))
      (free-buffer renderer (vulkan-renderer-instances renderer))
      (free-buffer renderer (vulkan-renderer-image-vertices renderer))
      (vk:destroy-semaphore device (vulkan-renderer-acquired renderer) null)
      (vk:destroy-fence device (vulkan-renderer-fence renderer) null)
      (vk:destroy-command-pool device (vulkan-renderer-command-pool renderer) null)
      (vk:destroy-descriptor-pool device (vulkan-renderer-descriptor-pool renderer) null)
      (dolist (pipeline (list (vulkan-renderer-cells-pipeline renderer)
                              (vulkan-renderer-layer-pipeline renderer)
                              (vulkan-renderer-image-pipeline renderer)))
        (vk:destroy-pipeline device pipeline null))
      (vk:destroy-pipeline-layout device (vulkan-renderer-cells-layout renderer) null)
      (vk:destroy-pipeline-layout device (vulkan-renderer-image-layout renderer) null)
      (vk:destroy-descriptor-set-layout device (vulkan-renderer-cells-set-layout renderer) null)
      (vk:destroy-descriptor-set-layout device (vulkan-renderer-image-set-layout renderer) null)
      (vk:destroy-sampler device (vulkan-renderer-sampler renderer) null)
      (vk:destroy-sampler device (vulkan-renderer-image-sampler renderer) null)
      (vk:destroy-render-pass device (vulkan-renderer-present-pass renderer) null)
      (vk:destroy-render-pass device (vulkan-renderer-snapshot-pass renderer) null)
      (vk:destroy-device device null)
      (setf (vulkan-renderer-device renderer) null))
    (let ((instance (vulkan-renderer-instance renderer)))
      (unless (cffi:null-pointer-p instance)
        (unless (eql (vulkan-renderer-surface renderer) +vulkan-null-handle+)
          (vk:destroy-surface-khr instance (vulkan-renderer-surface renderer) null))
        (vk:destroy-instance instance null)
        (setf (vulkan-renderer-instance renderer) null)))))

;;; Targets: the swapchain, or an offscreen image

(defun create-framebuffer (renderer pass view width height)
  (cffi:with-foreign-object (views 'vk:image-view)
    (setf (cffi:mem-ref views 'vk:image-view) view)
    (vk:with-struct (info vk:framebuffer-create-info
                          :render-pass pass :attachment-count 1 :p-attachments views
                          :width width :height height :layers 1)
      (receive (framebuffer vk:framebuffer)
        (vk-call (vk:create-framebuffer (vulkan-renderer-device renderer) info
                                        (cffi:null-pointer) framebuffer))))))

(defun free-swapchain (renderer)
  (let ((device (vulkan-renderer-device renderer))
        (null (cffi:null-pointer)))
    (loop for framebuffer across (vulkan-renderer-framebuffers renderer)
          do (vk:destroy-framebuffer device framebuffer null))
    (loop for semaphore across (vulkan-renderer-finished renderer)
          do (vk:destroy-semaphore device semaphore null))
    (dolist (view (vulkan-renderer-swapchain-views renderer))
      (vk:destroy-image-view device view null))
    (unless (eql (vulkan-renderer-swapchain renderer) +vulkan-null-handle+)
      (vk:destroy-swapchain-khr device (vulkan-renderer-swapchain renderer) null))
    (setf (vulkan-renderer-framebuffers renderer) (vector)
          (vulkan-renderer-finished renderer) (vector)
          (vulkan-renderer-swapchain-views renderer) '()
          (vulkan-renderer-swapchain renderer) +vulkan-null-handle+)))

(defun surface-capabilities (renderer capabilities)
  (vk-call (vk:get-physical-device-surface-capabilities-khr
            (vulkan-renderer-physical-device renderer) (vulkan-renderer-surface renderer)
            capabilities)))

(defun create-swapchain (renderer width height)
  "Make the swapchain WIDTH x HEIGHT, or the surface's own size."
  (let ((device (vulkan-renderer-device renderer)))
    (vk:device-wait-idle device)
    (free-swapchain renderer)
    (cffi:with-foreign-object (capabilities '(:struct vk:surface-capabilities-khr))
      (surface-capabilities renderer capabilities)
      (cffi:with-foreign-slots ((vk:min-image-count vk:max-image-count vk:current-transform)
                                capabilities (:struct vk:surface-capabilities-khr))
        (let* ((current (cffi:foreign-slot-pointer
                         capabilities '(:struct vk:surface-capabilities-khr) 'vk:current-extent))
               (current-width (cffi:foreign-slot-value current '(:struct vk:extent2d) 'vk:width))
               ;; A surface without a size of its own takes ours.
               (sized-p (/= current-width +vulkan-undefined-extent+))
               (width (if sized-p current-width width))
               (height (if sized-p
                           (cffi:foreign-slot-value current '(:struct vk:extent2d) 'vk:height)
                           height))
               ;; One more than the least, so acquiring rarely waits.
               (images (if (plusp vk:max-image-count)
                           (min (1+ vk:min-image-count) vk:max-image-count)
                           (1+ vk:min-image-count))))
          (vk:with-struct (info vk:swapchain-create-info-khr
                                :surface (vulkan-renderer-surface renderer)
                                :min-image-count images
                                :image-format +vulkan-target-format+
                                :image-color-space vk:+color-space-srgb-nonlinear-khr+
                                :image-array-layers 1
                                :image-usage vk:+image-usage-color-attachment-bit+
                                :image-sharing-mode vk:+sharing-mode-exclusive+
                                :pre-transform vk:current-transform
                                :composite-alpha vk:+composite-alpha-opaque-bit-khr+
                                :present-mode (vulkan-renderer-present-mode renderer)
                                :clipped vk:+true+)
            (cffi:with-foreign-slots ((vk:width vk:height)
                                      (cffi:foreign-slot-pointer
                                       info '(:struct vk:swapchain-create-info-khr) 'vk:image-extent)
                                      (:struct vk:extent2d))
              (setf vk:width width vk:height height))
            (let ((swapchain (receive (swapchain vk:swapchain-khr)
                               (vk-call (vk:create-swapchain-khr device info (cffi:null-pointer)
                                                                 swapchain)))))
              (setf (vulkan-renderer-swapchain renderer) swapchain
                    (vulkan-renderer-swapchain-width renderer) width
                    (vulkan-renderer-swapchain-height renderer) height)
              (let ((images (enumerate-into (images vk:image count)
                                (vk:get-swapchain-images-khr device swapchain count images)
                              (loop for i below count collect (cffi:mem-aref images 'vk:image i)))))
                (setf (vulkan-renderer-swapchain-views renderer)
                      (mapcar (lambda (image) (create-image-view renderer image +vulkan-target-format+))
                              images)
                      (vulkan-renderer-framebuffers renderer)
                      (map 'vector (lambda (view)
                                     (create-framebuffer renderer (vulkan-renderer-present-pass renderer)
                                                         view width height))
                           (vulkan-renderer-swapchain-views renderer))
                      (vulkan-renderer-finished renderer)
                      (map 'vector (lambda (image) (declare (ignore image)) (create-semaphore renderer))
                           images))))))))))

(defun free-snapshot-target (renderer)
  (let ((target (vulkan-renderer-target renderer)))
    (when target
      (vk:destroy-framebuffer (vulkan-renderer-device renderer)
                              (vulkan-renderer-target-framebuffer renderer) (cffi:null-pointer))
      (free-texture renderer target)
      (free-buffer renderer (vulkan-renderer-readback renderer))
      (setf (vulkan-renderer-target renderer) nil (vulkan-renderer-readback renderer) nil))))

(defun ensure-snapshot-target (renderer width height)
  (let ((target (vulkan-renderer-target renderer)))
    (unless (and target (= width (vulkan-texture-width target))
                 (= height (vulkan-texture-height target)))
      (free-snapshot-target renderer)
      (multiple-value-bind (image memory view)
          (create-image renderer width height +vulkan-target-format+
                        (logior vk:+image-usage-color-attachment-bit+
                                vk:+image-usage-transfer-src-bit+))
        (setf (vulkan-renderer-target renderer)
              (make-vulkan-texture :image image :memory memory :view view
                                   :format +vulkan-target-format+ :width width :height height)
              (vulkan-renderer-target-framebuffer renderer)
              (create-framebuffer renderer (vulkan-renderer-snapshot-pass renderer)
                                  view width height)
              (vulkan-renderer-readback renderer)
              (create-host-buffer renderer (* +rgba-channels+ width height)
                                  vk:+buffer-usage-transfer-dst-bit+))))))

;;; Uploading

(defun upload-atlas (renderer atlas)
  "Bring the atlas images up to date with ATLAS, recording the copies."
  (let ((width (glyph-atlas-width atlas))
        (height (glyph-atlas-height atlas)))
    (flet ((ensure (texture format generation)
             ;; A cleared or grown atlas is a new image, uploaded whole.
             (if (and texture (= (vulkan-texture-generation texture) generation)
                      (= (vulkan-texture-height texture) height)
                      (= (vulkan-texture-width texture) width))
                 (values texture nil)
                 (progn (free-texture renderer texture)
                        (let ((texture (create-texture renderer width height format)))
                          (setf (vulkan-texture-generation texture) generation)
                          (values texture t))))))
      (multiple-value-bind (texture new-p)
          (ensure (vulkan-renderer-atlas renderer) vk:+format-r8-unorm+
                  (glyph-atlas-generation atlas))
        (setf (vulkan-renderer-atlas renderer) texture)
        (multiple-value-bind (start end) (glyph-atlas-take-dirty atlas)
          (cond (new-p (upload-texture-rows renderer texture (glyph-atlas-pixels atlas) 0 height))
                (start (upload-texture-rows renderer texture (glyph-atlas-pixels atlas) start end))))
        (when new-p
          (write-texture-descriptor renderer (vulkan-renderer-cells-set renderer) 0 texture
                                    (vulkan-renderer-sampler renderer))))
      (let ((pixels (glyph-atlas-color-pixels atlas))
            (color (vulkan-renderer-color-atlas renderer)))
        (if pixels
            (multiple-value-bind (texture new-p)
                (ensure (and (> (vulkan-texture-width color) 1) color)
                        vk:+format-r8g8b8a8-unorm+ (glyph-atlas-generation atlas))
              (when (and new-p (not (eq texture color)))
                (free-texture renderer color))
              (setf (vulkan-renderer-color-atlas renderer) texture)
              (multiple-value-bind (start end) (glyph-atlas-take-color-dirty atlas)
                (cond (new-p (upload-texture-rows renderer texture pixels 0 height))
                      (start (upload-texture-rows renderer texture pixels start end))))
              (when new-p
                (write-texture-descriptor renderer (vulkan-renderer-cells-set renderer) 1 texture
                                          (vulkan-renderer-sampler renderer))))
            (unless (vulkan-texture-ready-p color)
              ;; The placeholder pixel, made readable once.
              (upload-texture-rows renderer color
                                   (make-array +rgba-channels+ :element-type '(unsigned-byte 8)
                                                               :initial-element 0)
                                   0 1)
              (write-texture-descriptor renderer (vulkan-renderer-cells-set renderer) 1 color
                                        (vulkan-renderer-sampler renderer))))))))

(defun upload-instances (renderer instances)
  (declare (type u32-vector instances))
  (let ((octets (* 4 (length instances))))
    (setf (vulkan-renderer-instances renderer)
          (ensure-host-buffer renderer (vulkan-renderer-instances renderer)
                              (max +vulkan-cell-octets+ octets)
                              vk:+buffer-usage-vertex-buffer-bit+))
    (copy-into-buffer (vulkan-renderer-instances renderer) instances octets)))

(defun image-texture (renderer image)
  "The texture holding IMAGE's pixels, uploading them when new or changed."
  (let* ((id (terminal-image-id image))
         (entry (gethash id (vulkan-renderer-image-textures renderer)))
         (rgba (terminal-image-rgba image)))
    (if (and entry (eq rgba (first entry)))
        (second entry)
        (let ((texture (if (and entry
                                (= (vulkan-texture-width (second entry)) (terminal-image-width image))
                                (= (vulkan-texture-height (second entry)) (terminal-image-height image)))
                           ;; A new frame of the same size reuses the image.
                           (second entry)
                           (progn
                             (when entry (free-texture renderer (second entry)))
                             (let ((texture (create-texture renderer (terminal-image-width image)
                                                            (terminal-image-height image)
                                                            vk:+format-r8g8b8a8-unorm+)))
                               (setf (vulkan-texture-descriptor-set texture)
                                     (allocate-descriptor-set
                                      renderer (vulkan-renderer-image-set-layout renderer)))
                               (write-texture-descriptor renderer (vulkan-texture-descriptor-set texture)
                                                         0 texture
                                                         (vulkan-renderer-image-sampler renderer))
                               texture)))))
          (upload-texture-rows renderer texture rgba 0 (terminal-image-height image))
          (setf (gethash id (vulkan-renderer-image-textures renderer)) (list rgba texture))
          texture))))

(defun prune-image-textures (renderer records)
  "Free the textures of images RECORDS no longer draw."
  (let ((live (mapcar (lambda (record) (terminal-image-id (second record))) records)))
    (maphash (lambda (id entry)
               (unless (member id live)
                 (free-texture renderer (second entry))
                 (remhash id (vulkan-renderer-image-textures renderer))))
             (vulkan-renderer-image-textures renderer))))

(defun image-quads (records width height)
  "The vertices of RECORDS' quads, six of four floats each, in normalized
device coordinates (y down) and texture coordinates."
  (let ((vertices (make-array (* (length records) +vulkan-image-vertices+
                                 +vulkan-image-vertex-floats+)
                              :element-type 'single-float))
        (index 0))
    (dolist (record records vertices)
      (destructuring-bind (placement image x y w h) record
        (let* ((iw (terminal-image-width image)) (ih (terminal-image-height image))
               (sx (terminal-image-placement-source-x placement))
               (sy (terminal-image-placement-source-y placement))
               (sw (or (terminal-image-placement-source-width placement) (- iw sx)))
               (sh (or (terminal-image-placement-source-height placement) (- ih sy)))
               (x0 (- (* 2.0 (/ x width)) 1.0)) (x1 (- (* 2.0 (/ (+ x w) width)) 1.0))
               (y0 (- (* 2.0 (/ y height)) 1.0)) (y1 (- (* 2.0 (/ (+ y h) height)) 1.0))
               (u0 (/ sx iw)) (u1 (/ (+ sx sw) iw))
               (v0 (/ sy ih)) (v1 (/ (+ sy sh) ih)))
          (dolist (value (list x0 y0 u0 v0  x1 y0 u1 v0  x0 y1 u0 v1
                               x0 y1 u0 v1  x1 y0 u1 v0  x1 y1 u1 v1))
            (setf (aref vertices index) (coerce value 'single-float))
            (incf index)))))))

;;; Drawing

(defun record-frame (renderer framebuffer pass instances columns cell-count cell-width cell-height
                     width height atlas palette image-records)
  "Record drawing a frame into FRAMEBUFFER, through PASS."
  (let ((command-buffer (vulkan-renderer-command-buffer renderer))
        (background (palette-background palette))
        (cursor (palette-cursor palette)))
    (flet ((channel (color shift) (/ (ldb (byte 8 shift) color) +channel-maximum+)))
      (prune-image-textures renderer image-records)
      (when (plusp cell-count)
        (upload-atlas renderer atlas)
        (upload-instances renderer instances))
      (let* ((textures (mapcar (lambda (record) (image-texture renderer (second record)))
                               image-records))
             (quads (image-quads image-records width height))
             (quad-octets (* (length quads) (cffi:foreign-type-size :float))))
        (when image-records
          (setf (vulkan-renderer-image-vertices renderer)
                (ensure-host-buffer renderer (vulkan-renderer-image-vertices renderer) quad-octets
                                    vk:+buffer-usage-vertex-buffer-bit+))
          (copy-into-buffer (vulkan-renderer-image-vertices renderer) quads quad-octets))
        (cffi:with-foreign-object (clear '(:union vk:clear-value))
         (vk:zero-struct clear '(:union vk:clear-value))
         (vk:with-structs ((begin vk:render-pass-begin-info
                                 :render-pass pass :framebuffer framebuffer
                                 :clear-value-count 1 :p-clear-values clear)
                          (viewport vk:viewport :x 0.0 :y 0.0
                                                :width (float width 1.0) :height (float height 1.0)
                                                :min-depth 0.0 :max-depth 1.0)
                          (scissor vk:rect2d))
          (let ((color (cffi:foreign-slot-pointer
                        (cffi:foreign-slot-pointer clear '(:union vk:clear-value) 'vk:color)
                        '(:union vk:clear-color-value) 'vk:float32)))
            (loop for value in (list (channel background 16) (channel background 8)
                                     (channel background 0) 1.0)
                  for i from 0
                  do (setf (cffi:mem-aref color :float i) value)))
          (dolist (rect (list (cffi:foreign-slot-pointer begin '(:struct vk:render-pass-begin-info)
                                                         'vk:render-area)
                              scissor))
            (cffi:with-foreign-slots ((vk:width vk:height)
                                      (cffi:foreign-slot-pointer rect '(:struct vk:rect2d) 'vk:extent)
                                      (:struct vk:extent2d))
              (setf vk:width width vk:height height)))
          (vk:cmd-begin-render-pass command-buffer begin vk:+subpass-contents-inline+)
          (vk:cmd-set-viewport command-buffer 0 1 viewport)
          (vk:cmd-set-scissor command-buffer 0 1 scissor)
          (multiple-value-bind (below-backgrounds below-text above) (image-layers image-records)
            (labels ((images (records)
                       (dolist (record records)
                         (let ((index (position record image-records)))
                           (vk:cmd-bind-pipeline command-buffer vk:+pipeline-bind-point-graphics+
                                                 (vulkan-renderer-image-pipeline renderer))
                           (bind-vertex-buffer command-buffer (vulkan-renderer-image-vertices renderer))
                           (bind-set command-buffer (vulkan-renderer-image-layout renderer)
                                     (vulkan-texture-descriptor-set (nth index textures)))
                           (vk:cmd-draw command-buffer +vulkan-image-vertices+ 1
                                        (* index +vulkan-image-vertices+) 0))))
                     (cells (layer)
                       (when (plusp cell-count)
                         (vk:cmd-bind-pipeline command-buffer vk:+pipeline-bind-point-graphics+
                                               (if (= layer 2)
                                                   (vulkan-renderer-cells-pipeline renderer)
                                                   (vulkan-renderer-layer-pipeline renderer)))
                         (bind-vertex-buffer command-buffer (vulkan-renderer-instances renderer))
                         (bind-set command-buffer (vulkan-renderer-cells-layout renderer)
                                   (vulkan-renderer-cells-set renderer))
                         (cffi:with-foreign-object (constants '(:struct vulkan-frame-constants))
                           (setf (cffi:foreign-slot-value
                                  constants '(:struct vulkan-frame-constants) 'columns)
                                 columns
                                 (cffi:foreign-slot-value
                                  constants '(:struct vulkan-frame-constants) 'atlas-columns)
                                 (glyph-atlas-slots-per-row atlas)
                                 (cffi:foreign-slot-value
                                  constants '(:struct vulkan-frame-constants) 'layer)
                                 layer)
                           (flet ((floats (slot values)
                                    (let ((pointer (cffi:foreign-slot-pointer
                                                    constants '(:struct vulkan-frame-constants) slot)))
                                      (loop for value in values
                                            for i from 0
                                            do (setf (cffi:mem-aref pointer :float i)
                                                     (float value 1.0))))))
                             (floats 'cursor-color (list (channel cursor 16) (channel cursor 8)
                                                         (channel cursor 0) 1.0))
                             (floats 'cell-size (list cell-width cell-height))
                             (floats 'viewport (list (max 1 width) (max 1 height))))
                           (vk:cmd-push-constants command-buffer (vulkan-renderer-cells-layout renderer)
                                                  (logior vk:+shader-stage-vertex-bit+
                                                          vk:+shader-stage-fragment-bit+)
                                                  0 (cffi:foreign-type-size
                                                     '(:struct vulkan-frame-constants))
                                                  constants))
                         (vk:cmd-draw command-buffer +vulkan-cell-vertices+ cell-count 0 0))))
              (images below-backgrounds)
              (if (or below-backgrounds below-text)
                  (progn (cells 0) (images below-text) (cells 1))
                  (cells 2))
              (images above)))
          (vk:cmd-end-render-pass command-buffer)))))))

(defun bind-vertex-buffer (command-buffer buffer)
  (cffi:with-foreign-objects ((buffers 'vk:buffer) (offsets 'vk:device-size))
    (setf (cffi:mem-ref buffers 'vk:buffer) (vulkan-buffer-handle buffer)
          (cffi:mem-ref offsets 'vk:device-size) 0)
    (vk:cmd-bind-vertex-buffers command-buffer 0 1 buffers offsets)))

(defun bind-set (command-buffer layout set)
  (cffi:with-foreign-object (sets 'vk:descriptor-set)
    (setf (cffi:mem-ref sets 'vk:descriptor-set) set)
    (vk:cmd-bind-descriptor-sets command-buffer vk:+pipeline-bind-point-graphics+ layout
                                 0 1 sets 0 (cffi:null-pointer))))

(defun begin-frame (renderer)
  "Wait for the last frame, and start recording the next."
  (let ((device (vulkan-renderer-device renderer))
        (command-buffer (vulkan-renderer-command-buffer renderer)))
    (cffi:with-foreign-object (fences 'vk:fence)
      (setf (cffi:mem-ref fences 'vk:fence) (vulkan-renderer-fence renderer))
      (vk-call (vk:wait-for-fences device 1 fences vk:+true+ +vulkan-no-timeout+)))
    (vk-call (vk:reset-command-buffer command-buffer 0))
    (vk:with-struct (info vk:command-buffer-begin-info
                          :flags vk:+command-buffer-usage-one-time-submit-bit+)
      (vk-call (vk:begin-command-buffer command-buffer info)))))

(defun submit-frame (renderer &key wait signal)
  "Submit the recorded frame, after the semaphore WAIT and signalling
SIGNAL when given."
  (let ((device (vulkan-renderer-device renderer))
        (command-buffer (vulkan-renderer-command-buffer renderer)))
    (vk-call (vk:end-command-buffer command-buffer))
    (cffi:with-foreign-objects ((fences 'vk:fence) (buffers 'vk:command-buffer)
                                (waits 'vk:semaphore) (stages 'vk:pipeline-stage-flags)
                                (signals 'vk:semaphore))
      (setf (cffi:mem-ref fences 'vk:fence) (vulkan-renderer-fence renderer)
            (cffi:mem-ref buffers 'vk:command-buffer) command-buffer
            (cffi:mem-ref waits 'vk:semaphore) (or wait +vulkan-null-handle+)
            (cffi:mem-ref stages 'vk:pipeline-stage-flags)
            vk:+pipeline-stage-color-attachment-output-bit+
            (cffi:mem-ref signals 'vk:semaphore) (or signal +vulkan-null-handle+))
      (vk-call (vk:reset-fences device 1 fences))
      (vk:with-struct (info vk:submit-info
                            :wait-semaphore-count (if wait 1 0)
                            :p-wait-semaphores waits :p-wait-dst-stage-mask stages
                            :command-buffer-count 1 :p-command-buffers buffers
                            :signal-semaphore-count (if signal 1 0) :p-signal-semaphores signals)
        (vk-call (vk:queue-submit (vulkan-renderer-queue renderer) 1 info
                                  (vulkan-renderer-fence renderer)))))))

(defun vulkan-draw (renderer instances columns cell-count cell-width cell-height
                    width height atlas palette &optional image-records)
  "Draw CELL-COUNT packed cells into the window, WIDTH x HEIGHT pixels, and
present them."
  (when (or (eql (vulkan-renderer-swapchain renderer) +vulkan-null-handle+)
            (/= width (vulkan-renderer-swapchain-width renderer))
            (/= height (vulkan-renderer-swapchain-height renderer)))
    (create-swapchain renderer width height))
  (let ((device (vulkan-renderer-device renderer)))
    (begin-frame renderer)
    (let ((index nil))
      (cffi:with-foreign-object (image-index :uint32)
        (let ((result (vk:acquire-next-image-khr device (vulkan-renderer-swapchain renderer)
                                                 +vulkan-no-timeout+
                                                 (vulkan-renderer-acquired renderer)
                                                 +vulkan-null-handle+ image-index)))
          (if (= result vk:+error-out-of-date-khr+)
              ;; Resized under us: rebuild, and draw at the next request.
              (setf (vulkan-renderer-swapchain-width renderer) 0)
              (progn (vk:check-result result 'vk:acquire-next-image-khr)
                     (setf index (cffi:mem-ref image-index :uint32))))))
      (if (null index)
          (vk:end-command-buffer (vulkan-renderer-command-buffer renderer))
          (let ((finished (aref (vulkan-renderer-finished renderer) index)))
            (record-frame renderer (aref (vulkan-renderer-framebuffers renderer) index)
                          (vulkan-renderer-present-pass renderer)
                          instances columns cell-count cell-width cell-height
                          (vulkan-renderer-swapchain-width renderer)
                          (vulkan-renderer-swapchain-height renderer)
                          atlas palette image-records)
            (submit-frame renderer :wait (vulkan-renderer-acquired renderer) :signal finished)
            (cffi:with-foreign-objects ((waits 'vk:semaphore) (swapchains 'vk:swapchain-khr)
                                        (indices :uint32))
              (setf (cffi:mem-ref waits 'vk:semaphore) finished
                    (cffi:mem-ref swapchains 'vk:swapchain-khr) (vulkan-renderer-swapchain renderer)
                    (cffi:mem-ref indices :uint32) index)
              (vk:with-struct (info vk:present-info-khr
                                    :wait-semaphore-count 1 :p-wait-semaphores waits
                                    :swapchain-count 1 :p-swapchains swapchains
                                    :p-image-indices indices)
                (let ((result (vk:queue-present-khr (vulkan-renderer-queue renderer) info)))
                  (if (member result (list vk:+error-out-of-date-khr+ vk:+suboptimal-khr+))
                      (setf (vulkan-renderer-swapchain-width renderer) 0)
                      (vk:check-result result 'vk:queue-present-khr))))))))))

(defun vulkan-snapshot (renderer instances columns cell-count cell-width cell-height
                        width height atlas palette &optional image-records)
  "Render offscreen and return the frame as WIDTH x HEIGHT RGB octets,
top row first, like GL-SNAPSHOT."
  (ensure-snapshot-target renderer width height)
  (begin-frame renderer)
  (record-frame renderer (vulkan-renderer-target-framebuffer renderer)
                (vulkan-renderer-snapshot-pass renderer)
                instances columns cell-count cell-width cell-height width height
                atlas palette image-records)
  (vk:with-struct (region vk:buffer-image-copy)
    (cffi:with-foreign-slots ((vk:aspect-mask vk:layer-count)
                              (cffi:foreign-slot-pointer region '(:struct vk:buffer-image-copy)
                                                         'vk:image-subresource)
                              (:struct vk:image-subresource-layers))
      (setf vk:aspect-mask vk:+image-aspect-color-bit+ vk:layer-count 1))
    (cffi:with-foreign-slots ((vk:width vk:height vk:depth)
                              (cffi:foreign-slot-pointer region '(:struct vk:buffer-image-copy)
                                                         'vk:image-extent)
                              (:struct vk:extent3d))
      (setf vk:width width vk:height height vk:depth 1))
    (vk:cmd-copy-image-to-buffer (vulkan-renderer-command-buffer renderer)
                                 (vulkan-texture-image (vulkan-renderer-target renderer))
                                 vk:+image-layout-transfer-src-optimal+
                                 (vulkan-buffer-handle (vulkan-renderer-readback renderer))
                                 1 region))
  (submit-frame renderer)
  (vk-call (vk:queue-wait-idle (vulkan-renderer-queue renderer)))
  (let ((pixels (vulkan-buffer-pointer (vulkan-renderer-readback renderer)))
        (rgb (make-array (* +rgb-channels+ width height) :element-type '(unsigned-byte 8))))
    ;; BGRA, rows top to bottom.
    (dotimes (pixel (* width height) rgb)
      (let ((source (* +rgba-channels+ pixel))
            (target (* +rgb-channels+ pixel)))
        (setf (aref rgb target) (cffi:mem-aref pixels :uint8 (+ source 2))
              (aref rgb (+ target 1)) (cffi:mem-aref pixels :uint8 (+ source 1))
              (aref rgb (+ target 2)) (cffi:mem-aref pixels :uint8 source))))))
