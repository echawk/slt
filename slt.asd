(pushnew :hunchentoot-no-ssl *features*) ; the web backend serves plain HTTP on localhost

(asdf:defsystem #:slt/core
  :description "Engine-neutral terminal core: grid, engines, hooks, PTY transport."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.2.0"
  :depends-on (:lispvt :babel :bordeaux-threads :cffi (:feature :sbcl :sb-posix))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "hooks")
               (:file "utf8")
               (:file "grid")
               (:file "core")
               (:file "backend")
               (:file "engine")
               (:file "engine-lispvt")
               (:file "transport")
               (:file "gpu")
               (:file "frontend")
               (:file "headless-backend")))

(asdf:defsystem #:slt
  :description "A Common Lisp terminal emulator with pluggable engines and GUI backends."
  :author "Ethan Hawk <ethhawk@iu.edu>"
  :license "MIT"
  :version "0.2.0"
  :depends-on (:slt/core :slt/cli :slt/3bst :slt/web :slt/ltk
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
  :description "The slt command line; options for the web backend and REPL
servers appear when those are loaded."
  :depends-on (:slt/core :clingon)
  :pathname "src/"
  :components ((:file "cli")))

(asdf:defsystem #:slt/ltk
  :description "Tk backend for slt, through LTK (needs wish at run time)."
  :depends-on (:slt/core :ltk)
  :pathname "src/"
  :components ((:file "ltk-backend")))

(asdf:defsystem #:slt/gtk
  :description "GTK 3 backend through cl-cffi-gtk: Cairo and Pango drawing."
  :depends-on (:slt/core :cl-cffi-gtk)
  :pathname "src/"
  :components ((:file "gtk-backend")))

(asdf:defsystem #:slt/clx
  :description "X11 backend through CLX: core fonts, no foreign libraries."
  :depends-on (:slt/core :clx)
  :pathname "src/"
  :components ((:file "clx-backend")))

(asdf:defsystem #:slt/3bst
  :description "3bst engine for slt: pure Lisp, a fork of 3b's port of st."
  :depends-on (:slt/core :3bst)
  :pathname "src/"
  :components ((:file "engine-3bst")))

(asdf:defsystem #:slt/sdl2
  :description "SDL2 backend (needs SDL2, SDL2_ttf and SDL2_image installed)."
  :depends-on (:slt/core :sdl2 :sdl2-ttf :sdl2-image)
  :pathname "src/"
  :components ((:file "sdl2-backend")))

(asdf:defsystem #:slt/tests
  :depends-on (:slt :slt/sdl2 :slt/conformance)
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
  :description "Engine throughput benchmarks."
  :depends-on (:slt/core)
  :pathname "bench/"
  :components ((:file "throughput")))

(asdf:defsystem #:slt/ghostty
  :description "libghostty-vt engine for slt (run 'make ghostty' first)."
  :depends-on (:slt/core :ghostty-vt)
  :pathname "src/"
  :components ((:file "engine-ghostty")))

(asdf:defsystem #:slt/cocoa
  :description "Native macOS backend: AppKit window, Metal renderer, CoreText glyphs."
  :depends-on (:slt/core :cocoas :float-features)
  :pathname "src/macos/"
  :serial t
  :components ((:file "objc")
               (:file "coretext")
               (:file "metal")
               (:file "cocoa-backend")))

(asdf:defsystem #:slt/gl
  :description "GLFW + OpenGL 3.3 / OpenGL ES 3.0 backend: X11 and Wayland on Linux and the BSDs, and macOS."
  :depends-on (:slt/core :glfw :cl-opengl :float-features
               ;; Glyphs: CoreText on macOS, font-discovery and FreeType elsewhere.
               (:feature :darwin :slt/cocoa)
               (:feature (:not :darwin) :font-discovery))
  :pathname "src/gl/"
  :serial t
  :components ((:file "package")
               (:file "gl-renderer")
               (:file "freetype" :if-feature (:not :darwin))
               (:file "gl-backend")))

(asdf:defsystem #:slt/web
  :description "Web frontend: serves web/slt-terminal.js and streams the grid over a WebSocket."
  :depends-on (:slt/core :hunchentoot :hunchensocket :com.inuoe.jzon)
  :pathname "src/web/"
  :components ((:file "web-backend")))

(asdf:defsystem #:slt/app
  :description "The distributed executable: bin/slt plus its native
libraries, built with Deploy by 'make' (scripts/build).  This is the
command line over slt/core; the build first loads slt (ltk and web
backends, REPL servers, 3bst) unless LEAN, plus the optional engines and
backends."
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-console-op"
  :build-pathname "slt"
  :entry-point "slt:main"
  :depends-on (:slt/cli)
  :pathname "src/"
  :components ((:file "deploy")))
