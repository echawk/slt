(in-package #:slt)

;;;; Short names for the libraries the gl frontend builds on: Shirakumo's
;;;; GLFW bindings (which ship GLFW 3.4 binaries for Linux and macOS) and,
;;;; off macOS, Shinmera's font-discovery (fontconfig).

(sb-ext:add-package-local-nickname '#:glfw '#:org.shirakumo.fraf.glfw '#:slt)
(sb-ext:add-package-local-nickname '#:%glfw '#:org.shirakumo.fraf.glfw.cffi '#:slt)
#-darwin
(sb-ext:add-package-local-nickname '#:font-discovery '#:org.shirakumo.font-discovery '#:slt)
