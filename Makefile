# slt -- build with `make`, run with `bin/slt`, install with `make install`.
#
#   make                          build bin/: the executable and its libraries
#   make run ARGS='--engine 3bst' build, then run bin/slt with ARGS
#   make check                    unit tests and engine conformance
#   make install                  install into $(DESTDIR)$(PREFIX)
#   make lean [install]           bin/ with only this system's GUI backend and
#                                 the pure-Lisp lispvt engine (= LEAN=1)
#   make tiny [install]           the same with the Tk (ltk) backend (= TINY=1)
#
# Packagers: `make deps` fetches the Lisp dependencies (the only step that
# needs the network besides building native engines), then
#   make ENGINES='...' && make install DESTDIR=... PREFIX=/usr
# For a lean package, pass LEAN=1 (or TINY=1) to every step: `make deps
# LEAN=1` fetches only what that build loads (no Zig, Ghostty or web stack),
# then
#   make LEAN=1 && make install LEAN=1 DESTDIR=... PREFIX=/usr
# bin/ is self-contained: the executable finds its libraries next to it, so
# it is installed into $(LIBDIR)/slt with a symlink in $(BINDIR).
#
# Needs GNU make (gmake on the BSDs), SBCL and ocicl.

PREFIX  ?= /usr/local
BINDIR  ?= $(PREFIX)/bin
LIBDIR  ?= $(PREFIX)/lib
DESTDIR ?=

SBCL  ?= sbcl
OCICL ?= ocicl
export SBCL

# Native engines to build in; the pure-Lisp lispvt is always included (3bst too, via BASE).
# ghostty downloads Zig and Ghostty to build; libvterm needs only a C compiler.
ENGINES ?= ghostty libvterm

# Backends outside the slt system: on Linux and the BSDs the GLFW/OpenGL one.
# (macOS always has cocoa, plus ltk, web and headless everywhere.)
ifeq ($(shell uname -s),Darwin)
BACKENDS ?=
else
BACKENDS ?= gl
endif

# The slt system: the ltk and web backends, the REPL servers and the 3bst
# engine.  bin/slt is the command line over slt/core plus BASE, ENGINES and
# BACKENDS.
BASE ?= slt

# LEAN=1: only the native GUI backend (cocoa on macOS, gl elsewhere) and the
# lispvt engine, which lives in slt/core.  Its Lisp dependencies are fetched
# on their own into build/lean/ocicl (scripts/fetch-lean), and no native
# engine is built.  TINY=1: the same with the ltk backend instead, which
# needs only Tk's wish at run time; its dependencies go to build/tiny/ocicl.
ifeq ($(shell uname -s),Darwin)
NATIVE_BACKEND = cocoa
else
NATIVE_BACKEND = gl
endif
ifneq ($(filter lean,$(MAKECMDGOALS)),)
LEAN = 1
endif
ifneq ($(filter tiny,$(MAKECMDGOALS)),)
TINY = 1
endif
ifdef TINY
LEAN_DIR = build/tiny
BACKENDS = ltk
else ifdef LEAN
LEAN_DIR = build/lean
BACKENDS = $(NATIVE_BACKEND)
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

# Rebuild when ENGINES or BACKENDS change: build/stamp/config holds the
# configuration bin/ was built with, and a different one discards bin/.
CONFIG := $(BASE) / $(ENGINES) / $(BACKENDS)
$(shell mkdir -p build/stamp && \
        (echo '$(CONFIG)' | cmp -s - build/stamp/config || \
         { echo '$(CONFIG)' > build/stamp/config; rm -rf bin; }))

.PHONY: all lean tiny deps run check test conformance web-test gl-test install uninstall clean distclean

all: bin/slt

bin/slt: $(DEPS_STAMP) $(STAMPS) $(SOURCES)
	SLT_SYSTEMS="$(BASE) $(ENGINES:%=slt/%) $(BACKENDS:%=slt/%)" scripts/build

lean tiny: bin/slt

deps: $(DEPS_STAMP)

ocicl/.stamp: ocicl.csv
	$(OCICL) install
	touch $@

build/lean/.stamp build/tiny/.stamp: ocicl.csv slt.asd
	OCICL=$(OCICL) scripts/fetch-lean $(@D) slt/app $(BACKENDS:%=slt/%)
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
		--eval '(asdf:load-system :slt/3bst)' \
		--eval '(ignore-errors (asdf:load-system :slt/libvterm))' \
		--eval '(ignore-errors (asdf:load-system :slt/ghostty))' \
		--eval '(uiop:quit (if (slt/conformance:conformance-main) 0 1))'

# Browser end to end; needs $$SLT_CHROME or scripts/fetch-chrome-headless.
web-test: ocicl/.stamp
	scripts/web-test

# gl backend smoke test; needs a display (on Linux CI: xvfb-run make gl-test).
gl-test: ocicl/.stamp
	scripts/gl-test

install: bin/slt
	mkdir -p $(DESTDIR)$(LIBDIR)/slt $(DESTDIR)$(BINDIR)
	cp -RP bin/. $(DESTDIR)$(LIBDIR)/slt/
	ln -sf $(LIBDIR)/slt/slt $(DESTDIR)$(BINDIR)/slt

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/slt
	rm -rf $(DESTDIR)$(LIBDIR)/slt

clean:
	rm -rf bin build

# Also the fetched dependencies, native engine builds and compiled Lisp.
distclean: clean
	rm -rf .cache .deps ocicl lib/cl-libvterm/build lib/cl-libvterm/.deps
