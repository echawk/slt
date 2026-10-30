# slt -- build with `make`, run with `bin/slt`, install with `make install`.
#
#   make                          build bin/: the executable and its libraries
#   make run ARGS='--engine lispvt' build, then run bin/slt with ARGS
#   make check                    unit tests and engine conformance
#   make install                  install into $(DESTDIR)$(PREFIX)
#   make lean [install]           bin/ with only this system's GUI frontends
#                                 and the pure-Lisp lispvt engine (= LEAN=1)
#   make lean-FRONTEND [install]  the same with FRONTEND: cocoa, wayland, x11,
#                                 gl, gtk, clx, ltk or web (= LEAN=FRONTEND)
#   make tiny [install]           the same as make lean-ltk (= TINY=1)
#   make app                      macOS: build/slt.app around bin/
#   make install-app              macOS: install slt.app into $(APPDIR)
#
# Packagers: `make deps` fetches the Lisp dependencies (the only step that
# needs the network besides building native engines), then
#   make ENGINES='...' && make install DESTDIR=... PREFIX=/usr
# For a lean package, pass LEAN=1 (or LEAN=FRONTEND) to every step: `make
# deps LEAN=gtk` fetches only what that build loads (no Zig, Ghostty or
# other frontends), then
#   make LEAN=gtk && make install LEAN=gtk DESTDIR=... PREFIX=/usr
# bin/ is self-contained: the executable finds its libraries next to it, so
# it is installed into $(LIBDIR)/slt with a symlink in $(BINDIR).
#
# Needs GNU make (gmake on the BSDs), SBCL and ocicl.

PREFIX  ?= /usr/local
BINDIR  ?= $(PREFIX)/bin
LIBDIR  ?= $(PREFIX)/lib
DESTDIR ?=
# macOS application bundles go here; /Applications works too.
APPDIR  ?= $(HOME)/Applications

SBCL  ?= sbcl
OCICL ?= ocicl
export SBCL

# Native engines to build in; the pure-Lisp lispvt is always included.
# ghostty downloads Zig and Ghostty to build; libvterm needs only a C compiler.
ENGINES ?= ghostty libvterm

# Frontends outside the slt system: on Linux and the BSDs the wayland and
# x11 ones, drawing with OpenGL or, with *gl-api* :vulkan, Vulkan.  (macOS
# always has cocoa, plus ltk, web and headless everywhere.)  The GLFW one is
# FRONTENDS='gl glfw-vulkan'.
ifeq ($(shell uname -s),Darwin)
FRONTENDS ?=
else
FRONTENDS ?= wayland x11
endif

# The slt system: the ltk and web frontends and the REPL servers.  bin/slt
# is the command line over slt/core plus BASE, ENGINES and FRONTENDS.
BASE ?= slt

# LEAN=FRONTEND: only FRONTEND and the lispvt engine, which lives in
# slt/core.  LEAN=1 means the native GUI frontends (cocoa on macOS, wayland
# and x11 elsewhere), and TINY=1 means LEAN=ltk, which needs only Tk's wish
# at run time.  Each lean build's Lisp dependencies are fetched on their own
# into build/lean-FRONTENDS/ocicl (scripts/fetch-lean), and no native engine
# is built.
LEAN_FRONTENDS = cocoa wayland x11 gl gtk clx ltk web
ifeq ($(shell uname -s),Darwin)
NATIVE_FRONTEND = cocoa
else
NATIVE_FRONTEND = wayland x11
endif
empty :=
space := $(empty) $(empty)
LEAN_GOAL := $(firstword $(filter lean-%,$(MAKECMDGOALS)))
ifneq ($(LEAN_GOAL),)
LEAN_FRONTEND := $(LEAN_GOAL:lean-%=%)
else ifneq ($(filter tiny,$(MAKECMDGOALS)),)
LEAN_FRONTEND := ltk
else ifdef TINY
LEAN_FRONTEND := ltk
else ifneq ($(filter lean,$(MAKECMDGOALS))$(filter 1,$(LEAN)),)
# `make lean FRONTENDS=gtk` still picks the frontend.
LEAN_FRONTEND := $(if $(filter command,$(origin FRONTENDS)),$(firstword $(FRONTENDS)),$(NATIVE_FRONTEND))
else
LEAN_FRONTEND := $(LEAN)
endif
ifneq ($(LEAN_FRONTEND),)
ifneq ($(filter-out $(LEAN_FRONTENDS),$(LEAN_FRONTEND)),)
$(error LEAN=$(LEAN_FRONTEND): pick one of $(LEAN_FRONTENDS))
endif
LEAN_DIR = build/lean-$(subst $(space),-,$(strip $(LEAN_FRONTEND)))
override FRONTENDS = $(LEAN_FRONTEND)
endif
ifdef LEAN_DIR
BASE =
ENGINES =
export SLT_OCICL := $(CURDIR)/$(LEAN_DIR)/ocicl
DEPS_STAMP = $(LEAN_DIR)/.stamp
else
DEPS_STAMP = ocicl/.stamp
endif

LISP    = scripts/lisp
SOURCES = slt.asd ocicl.csv $(shell find src lib conformance -name '*.lisp' -o -name '*.c')
STAMPS  = $(ENGINES:%=build/stamp/%)
GHOSTTY_DIR = $(dir ocicl/$(shell awk -F', ' '$$1 == "ghostty-vt" {print $$3}' ocicl.csv))

# Rebuild when ENGINES or FRONTENDS change: build/stamp/config holds the
# configuration bin/ was built with, and a different one discards bin/.
CONFIG := $(BASE) / $(ENGINES) / $(FRONTENDS)
$(shell mkdir -p build/stamp && \
        (echo '$(CONFIG)' | cmp -s - build/stamp/config || \
         { echo '$(CONFIG)' > build/stamp/config; rm -rf bin; }))

.PHONY: all lean tiny $(LEAN_FRONTENDS:%=lean-%) deps run check test conformance web-test gl-test vulkan-test gtk-test clx-test window-test install uninstall app install-app uninstall-app icon shaders clean distclean

all: bin/slt

bin/slt: $(DEPS_STAMP) $(STAMPS) $(SOURCES)
	SLT_SYSTEMS="$(BASE) $(ENGINES:%=slt/%) $(FRONTENDS:%=slt/%)" scripts/build

lean tiny $(LEAN_FRONTENDS:%=lean-%): bin/slt

deps: $(DEPS_STAMP)

ocicl/.stamp: ocicl.csv
	$(OCICL) install
	touch $@

build/lean-%/.stamp: ocicl.csv slt.asd
	OCICL=$(OCICL) scripts/fetch-lean $(@D) slt/app $(FRONTENDS:%=slt/%)
	touch $@

# Native engine libraries; the scripts skip work that is already done.
build/stamp/libvterm:
	lib/cl-libvterm/scripts/build-libvterm.sh
	@mkdir -p $(@D) && touch $@

build/stamp/ghostty: ocicl/.stamp
	sh $(GHOSTTY_DIR)scripts/build-libghostty-vt.sh
	@mkdir -p $(@D) && touch $@

run: bin/slt
	bin/slt $(ARGS)

check: test conformance

test: ocicl/.stamp $(STAMPS)
	$(LISP) --eval '(asdf:test-system :slt)'

# Runs the same byte streams through every engine that loads.
conformance: ocicl/.stamp $(STAMPS)
	$(LISP) --eval '(asdf:load-system :slt/conformance)' \
		--eval '(ignore-errors (asdf:load-system :slt/libvterm))' \
		--eval '(ignore-errors (asdf:load-system :slt/ghostty))' \
		--eval '(uiop:quit (if (slt/conformance:conformance-main) 0 1))'

# Browser end to end; needs $$SLT_CHROME or scripts/fetch-chrome-headless.
web-test: ocicl/.stamp
	scripts/web-test

# gl frontend smoke test; needs a display (on Linux CI: xvfb-run make gl-test).
gl-test: ocicl/.stamp
	scripts/gl-test

# Vulkan renderer smoke test, offscreen; needs a Vulkan driver (on Linux CI,
# Mesa's lavapipe) but no display.
vulkan-test: ocicl/.stamp
	scripts/vulkan-test

# gtk frontend smoke test; needs a display and GTK 3 (on Linux CI: xvfb-run).
gtk-test: ocicl/.stamp
	scripts/gtk-test

# clx frontend smoke test; needs an X server with the RENDER extension (on
# Linux CI: xvfb-run -s '-screen 0 1024x768x24').
clx-test: ocicl/.stamp
	scripts/clx-test

# x11 frontend smoke test (SLT_FRONTEND, SLT_GL_API pick another frontend
# and API); runs Xvfb itself.
window-test: ocicl/.stamp
	scripts/window-test

install: bin/slt
	mkdir -p $(DESTDIR)$(LIBDIR)/slt $(DESTDIR)$(BINDIR)
	cp -RP bin/. $(DESTDIR)$(LIBDIR)/slt/
	ln -sf $(LIBDIR)/slt/slt $(DESTDIR)$(BINDIR)/slt

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/slt
	rm -rf $(DESTDIR)$(LIBDIR)/slt

# macOS application bundle.  `make lean app` and `make lean install-app`
# bundle the lean build (`make lean-gl app` another lean one).
app: build/slt.app

build/slt.app: bin/slt macos/Info.plist macos/slt.icns scripts/make-app
	scripts/make-app $@

install-app: build/slt.app
	mkdir -p $(DESTDIR)$(APPDIR)
	rm -rf $(DESTDIR)$(APPDIR)/slt.app
	cp -RP build/slt.app $(DESTDIR)$(APPDIR)/

uninstall-app:
	rm -rf $(DESTDIR)$(APPDIR)/slt.app

# Regenerate macos/slt.icns from macos/slt.svg (needs rsvg-convert).
icon:
	scripts/make-icon

# Recompile the Vulkan renderer's SPIR-V from its GLSL (needs glslc, from
# shaderc).  The .spv files are committed, so building does not need it.
VULKAN_SHADERS := $(wildcard src/vulkan/shaders/*.vert src/vulkan/shaders/*.frag)
GLSLC ?= glslc
GLSLC_TARGET ?= vulkan1.0

shaders: $(VULKAN_SHADERS:%=%.spv)

%.vert.spv: %.vert
	$(GLSLC) -O --target-env=$(GLSLC_TARGET) -o $@ $<

%.frag.spv: %.frag
	$(GLSLC) -O --target-env=$(GLSLC_TARGET) -o $@ $<

clean:
	rm -rf bin build

# Also the fetched dependencies, native engine builds and compiled Lisp.
distclean: clean
	rm -rf .cache .deps ocicl lib/cl-libvterm/build lib/cl-libvterm/.deps
