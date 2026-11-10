(in-package #:slt)

;;; The window hosts refer to the bindings as EGL and VK.
(trivial-package-local-nicknames:add-package-local-nickname '#:egl '#:egl-cffi '#:slt)
(trivial-package-local-nicknames:add-package-local-nickname '#:vk '#:vulkan-cffi '#:slt)
