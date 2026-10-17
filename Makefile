OCICL ?= ocicl
BUILD_OUTPUT ?= build/slt
LISP = scripts/lisp

.PHONY: deps test bundle web-test gl-test conformance libvterm ghostty run run-sdl2 dump clean

deps: ocicl/.stamp

ocicl/.stamp: ocicl.csv
	$(OCICL) install
	touch $@

test: deps
	./run-tests

# Optional native engines; conformance runs every engine that loads.
libvterm:
	lib/cl-libvterm/scripts/build-libvterm.sh

# Builds libghostty-vt inside the ocicl-installed ghostty-vt tree (rerun
# after `ocicl latest` moves it to a new directory).
ghostty: deps
	sh "$$(dirname "ocicl/$$(awk -F', ' '$$1 == "ghostty-vt" {print $$3}' ocicl.csv)")/scripts/build-libghostty-vt.sh"

# Web frontend end to end; browser checks need $$SLT_CHROME or
# scripts/fetch-chrome-headless.
web-test: deps
	scripts/web-test

# gl backend smoke test; needs a display (on Linux CI: xvfb-run make gl-test).
gl-test: deps
	scripts/gl-test

conformance: deps
	$(LISP) --eval '(asdf:load-system :slt/conformance)' \
		--eval '(ignore-errors (asdf:load-system :slt/libvterm))' \
		--eval '(ignore-errors (asdf:load-system :slt/ghostty))' \
		--eval '(uiop:quit (if (slt/conformance:conformance-main) 0 1))'

run: deps
	./run-slt $(ARGS)

run-sdl2: deps
	SLT_BACKEND=sdl2 ./run-slt $(ARGS)

$(BUILD_OUTPUT): deps
	mkdir -p "$(@D)"
	$(LISP) --eval '(asdf:load-system :slt)' \
		--eval '(slt:dump-executable :output "$(BUILD_OUTPUT)")'

dump: $(BUILD_OUTPUT)

# Self-contained distribution in bin/ (executable + native libraries).
bundle: deps libvterm ghostty
	scripts/bundle

clean:
	rm -rf build
