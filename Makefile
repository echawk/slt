OCICL ?= ocicl
BUILD_OUTPUT ?= build/slt
LISP = scripts/lisp

.PHONY: deps test conformance libvterm ghostty run run-sdl2 dump clean

deps: ocicl/.stamp

ocicl/.stamp: ocicl.csv
	$(OCICL) install
	touch $@

test: deps
	./run-tests

# Optional native engines; conformance runs every engine that loads.
libvterm:
	lib/cl-libvterm/scripts/build-libvterm.sh

ghostty:
	lib/cl-ghostty-vt/scripts/build-libghostty-vt.sh

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

clean:
	rm -rf build
