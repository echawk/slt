(in-package #:slt)

;;;; Vulkan in the gl frontend's GLFW window, as *GL-API* :VULKAN: GLFW
;;;; makes the window without a context, names the instance extensions its
;;;; surfaces need (X11, Wayland or Metal) and makes the surface.

(cffi:defcfun ("glfwInitVulkanLoader" %glfw-init-vulkan-loader) :void
  (get-instance-proc-addr :pointer))

(cffi:defcfun ("glfwCreateWindowSurface" %glfw-create-window-surface) vk:result
  (instance vk:instance) (window :pointer) (allocator :pointer) (surface :pointer))

(defmethod prepare-glfw ((api (eql :vulkan)))
  ;; GLFW looks for the Vulkan loader itself, not where CFFI finds it
  ;; (Homebrew's on macOS); since GLFW 3.4 it can be handed ours.
  (vk:load-vulkan)
  (when (cffi:foreign-symbol-pointer "glfwInitVulkanLoader")
    (%glfw-init-vulkan-loader (cffi:foreign-symbol-pointer "vkGetInstanceProcAddr"))))

(defmethod api-window-hints ((api (eql :vulkan)))
  '(:client-api :no-api))

(defmethod make-api-renderer ((api (eql :vulkan)) view)
  (unless (glfw:vulkan-supported-p)
    (error "GLFW finds no Vulkan loader"))
  (let ((window (gl-view-pointer view)))
    (make-vulkan-renderer
     :instance-extensions (glfw:get-required-instance-extensions)
     :surface-function (lambda (instance)
                         (vulkan-receive (surface vk:surface-khr)
                           (vulkan-call (%glfw-create-window-surface
                                     instance window (cffi:null-pointer) surface))))
     :vsync *gl-vsync*)))

(defmethod renderer-present ((renderer vulkan-renderer) view frame)
  (declare (ignore view))
  (apply #'vulkan-draw renderer frame))

(defmethod renderer-snapshot ((renderer vulkan-renderer) view frame)
  (declare (ignore view))
  (apply #'vulkan-snapshot renderer frame))

(defmethod free-renderer ((renderer vulkan-renderer) view)
  (declare (ignore view))
  (free-vulkan-renderer renderer))
