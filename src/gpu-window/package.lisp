(in-package #:slt)

;;; The window hosts refer to the bindings as EGL and VK.
(sb-ext:add-package-local-nickname '#:egl '#:egl-cffi '#:slt)
(sb-ext:add-package-local-nickname '#:vk '#:vulkan-cffi '#:slt)
