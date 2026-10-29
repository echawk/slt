(pushnew :hunchentoot-no-ssl *features*) ; the web frontend serves plain HTTP on localhost

(asdf:defsystem #:slt/core
  :description "Engine-neutral terminal core: grid, engines, hooks, PTY transport."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.2.0"
  :depends-on (:lispvt :ecma48 :kitty-graphics :cl-libsixel :kitty-keyboard :terminal-osc
               :iterm2-images
               :babel :bordeaux-threads :cffi (:feature :sbcl :sb-posix))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "hooks")
               (:file "utf8")
               (:file "grid")
               (:file "core")
               (:file "frontend")
               (:file "engine")
               (:file "images")
               (:file "sequences")
               (:file "engine-lispvt")
               (:file "repl")
               (:file "transport")
               (:file "gpu")
               (:file "emulator")
               (:file "controls")
               (:file "smooth-scroll")
               (:file "headless-frontend")))

(asdf:defsystem #:slt
  :description "A Common Lisp terminal emulator with pluggable engines and GUI frontends."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.2.0"
  :depends-on (:slt/core :slt/cli :slt/web :slt/ltk
               ;; REPL servers, with the contribs their clients ask for.
               :swank/exts :slynk/arglists :slynk/fancy-inspector
               :slynk/package-fu :slynk/mrepl :slynk/trace-dialog
               :slynk/profiler :slynk/stickers :slynk/indentation
               (:feature :darwin :slt/cocoa))
  :pathname "src/"
  :serial t
  :components ((:file "repl-server"))
  :in-order-to ((test-op (test-op "slt/tests"))))

(asdf:defsystem #:slt/cli
  :description "The slt command line; options for the web frontend and REPL
servers appear when those are loaded."
  :depends-on (:slt/core :clingon)
  :pathname "src/"
  :components ((:file "cli")))

(asdf:defsystem #:slt/ltk
  :description "Tk frontend for slt, through LTK (needs wish at run time)."
  :depends-on (:slt/core :ltk)
  :pathname "src/"
  :components ((:file "ltk-frontend")))

(asdf:defsystem #:slt/gtk
  :description "GTK 3 frontend through cl-cffi-gtk: Cairo and Pango drawing."
  :depends-on (:slt/core :cl-cffi-gtk)
  :pathname "src/"
  :components ((:file "gtk-frontend")))

(asdf:defsystem #:slt/glyphs
  :description "Glyph rasterizers for the gl and clx frontends: FreeType with
families found through font-discovery (fontconfig), CoreText on macOS."
  :depends-on (:slt/core
               (:feature :darwin :slt/cocoa)
               (:feature (:not :darwin) :font-discovery))
  :pathname "src/glyphs/"
  :serial t
  :components ((:file "glyph-font")
               (:file "freetype" :if-feature (:not :darwin))))

(asdf:defsystem #:slt/clx
  :description "X11 frontend through CLX, the Lisp X protocol library: glyphs
from slt/glyphs drawn with the RENDER extension."
  :depends-on (:slt/core :slt/glyphs :clx)
  :pathname "src/"
  :components ((:file "clx-frontend")))

(asdf:defsystem #:slt/tests
  :depends-on (:slt :slt/conformance)
  :pathname "t/"
  :serial t
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call :slt/tests :run-tests)))

(asdf:defsystem #:slt/conformance
  :description "Engine conformance suite: byte streams with expected screens."
  :depends-on (:slt/core)
  :pathname "conformance/"
  :serial t
  :components ((:file "harness")
               (:file "cases")))

(asdf:defsystem #:slt/libvterm
  :description "libvterm engine for slt (requires lib/cl-libvterm to be built)."
  :depends-on (:slt/core :libvterm)
  :pathname "src/"
  :components ((:file "engine-libvterm")))

(asdf:defsystem #:slt/bench
  :description "Engine and image throughput benchmarks."
  :depends-on (:slt/core :simd-base64)
  :pathname "bench/"
  :components ((:file "throughput")
               (:file "images")))

(asdf:defsystem #:slt/ghostty
  :description "libghostty-vt engine for slt (run 'make ghostty' first)."
  :depends-on (:slt/core :ghostty-vt)
  :pathname "src/"
  :components ((:file "engine-ghostty")))

(asdf:defsystem #:slt/cocoa
  :description "Native macOS frontend: AppKit window, Metal renderer, CoreText glyphs."
  :depends-on (:slt/core :cocoas :float-features)
  :pathname "src/macos/"
  :serial t
  :components ((:file "objc")
               (:file "coretext")
               (:file "metal")
               (:file "cocoa-frontend")))

(asdf:defsystem #:slt/gl-renderer
  :description "OpenGL 3.3 / OpenGL ES 3.0 renderer, in whatever context its
host makes current (GLFW, EGL)."
  :depends-on (:slt/core :cl-opengl)
  :pathname "src/gl/"
  :components ((:file "gl-renderer")))

(asdf:defsystem #:slt/gl
  :description "GLFW frontend, drawn with OpenGL 3.3 / OpenGL ES 3.0 (or Vulkan, with slt/glfw-vulkan): X11 and Wayland on Linux and the BSDs, and macOS.  Needs the system's GLFW 3.3 or later."
  :depends-on (:slt/core :slt/gl-renderer :slt/glyphs :cl-glfw3 :float-features)
  :pathname "src/gl/"
  :components ((:file "gl-frontend")))

(asdf:defsystem #:slt/vulkan
  :description "Vulkan renderer, drawing into a window its host makes a
surface of, or offscreen.  Needs a Vulkan loader at run time (MoltenVK on
macOS)."
  :depends-on (:slt/core :vulkan-cffi)
  :pathname "src/vulkan/"
  :serial t
  :components ((:file "package")
               (:file "vulkan-renderer")
               (:static-file "shaders/cells.vert")
               (:static-file "shaders/cells.frag")
               (:static-file "shaders/image.vert")
               (:static-file "shaders/image.frag")))

(asdf:defsystem #:slt/glfw-vulkan
  :description "The gl frontend's GLFW window drawn with Vulkan: *gl-api* :vulkan."
  :depends-on (:slt/gl :slt/vulkan)
  :pathname "src/vulkan/"
  :components ((:file "glfw-host")))

(asdf:defsystem #:slt/web
  :description "Web frontend: serves web/slt-terminal.js and streams the grid over a WebSocket."
  :depends-on (:slt/core :simd-base64 :hunchentoot :hunchensocket :com.inuoe.jzon)
  :pathname "src/web/"
  :components ((:file "web-frontend")))

(asdf:defsystem #:slt/app
  :description "The distributed executable: bin/slt plus its native
libraries, built with Deploy by 'make' (scripts/build).  This is the
command line over slt/core; the build first loads slt (ltk and web
frontends, REPL servers) unless LEAN, plus the optional engines and
frontends."
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-console-op"
  :build-pathname "slt"
  :entry-point "slt:main"
  :depends-on (:slt/cli)
  :pathname "src/"
  :components ((:file "deploy")))
