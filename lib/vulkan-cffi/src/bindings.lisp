(in-package #:vulkan-cffi.generator)

;;;; The bindings: what a renderer needs of Vulkan 1.3, with presenting to
;;;; a window on macOS (through MoltenVK), X11 and Wayland, and the debug
;;;; messenger the validation layers report through.

(define-vulkan-bindings
  :version "1.3"
  :extensions ("VK_KHR_surface"
               "VK_KHR_swapchain"
               "VK_EXT_metal_surface"
               "VK_KHR_xlib_surface"
               "VK_KHR_xcb_surface"
               "VK_KHR_wayland_surface"
               "VK_KHR_portability_enumeration"
               "VK_KHR_portability_subset"
               "VK_EXT_debug_utils"))
