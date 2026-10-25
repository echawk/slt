(in-package #:slt)

;;;; Short names for the libraries the gl frontend builds on.  cl-glfw3,
;;;; which binds the system's GLFW, names its packages GLFW and %GLFW
;;;; itself; off macOS, Shinmera's font-discovery (fontconfig).

#-darwin
(sb-ext:add-package-local-nickname '#:font-discovery '#:org.shirakumo.font-discovery '#:slt)
