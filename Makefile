# slt -- build with `make`, run with `bin/slt`, install with `make install`.
#
#   make                          build bin/: the executable and its libraries
#   make run ARGS='--engine 3bst' build, then run bin/slt with ARGS
#   make check                    unit tests and engine conformance
#   make install                  install into $(DESTDIR)$(PREFIX)
#
# Packagers: `make deps` fetches the Lisp dependencies (the only step that
# needs the network besides building native engines), then
#   make ENGINES='...' && make install DESTDIR=... PREFIX=/usr
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

# Native engines to build in; the pure-Lisp 3bst is always included.
# ghostty downloads Zig and Ghostty to build; libvterm needs only a C compiler.
ENGINES ?= ghostty libvterm

# Backends outside the slt system: on Linux and the BSDs the GLFW/OpenGL one.
# (macOS always has cocoa, plus ltk, web and headless everywhere.)
ifeq ($(shell uname -s),Darwin)
BACKENDS ?=
else
BACKENDS ?= gl
endif

LISP    = scripts/lisp
SOURCES = slt.asd ocicl.csv $(shell find src lib conformance -name '*.lisp' -o -name '*.c')
STAMPS  = $(ENGINES:%=build/stamp/%)
GHOSTTY_DIR = $(dir ocicl/$(shell awk -F', ' '$$1 == "ghostty-vt" {print $$3}' ocicl.csv))

# Rebuild when ENGINES or BACKENDS change: build/stamp/config holds the
# configuration bin/ was built with, and a different one discards bin/.
CONFIG := $(ENGINES) / $(BACKENDS)
$(shell mkdir -p build/stamp && \
        (echo '$(CONFIG)' | cmp -s - build/stamp/config || \
         { echo '$(CONFIG)' > build/stamp/config; rm -rf bin; }))

.PHONY: all deps run check test conformance web-test gl-test install uninstall clean distclean

all: bin/slt

bin/slt: ocicl/.stamp $(STAMPS) $(SOURCES)
	SLT_SYSTEMS="$(ENGINES:%=slt/%) $(BACKENDS:%=slt/%)" scripts/build

deps: ocicl/.stamp

ocicl/.stamp: ocicl.csv
	$(OCICL) install
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
