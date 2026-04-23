QLOT ?= qlot
SBCL ?= sbcl
CACHE_DIR ?= $(CURDIR)/.cache
BUILD_OUTPUT ?= build/slt

.PHONY: qlot-install test run run-sdl2 dump clean

qlot-install: qlfile qlfile.lock
	XDG_CACHE_HOME="$(CACHE_DIR)" $(QLOT) install

test: qlot-install
	XDG_CACHE_HOME="$(CACHE_DIR)" $(QLOT) exec $(SBCL) --noinform --no-sysinit --no-userinit --non-interactive \
		--eval '(setf uiop/lisp-build:*compile-file-warnings-behaviour* :warn uiop/lisp-build:*compile-file-failure-behaviour* :warn)' \
		--eval '(asdf:load-asd (merge-pathnames "slt.asd" (truename ".")))' \
		--eval '(asdf:test-system :slt)'

run: qlot-install
	./run-slt $(ARGS)

run-sdl2: qlot-install
	SLT_BACKEND=sdl2 ./run-slt $(ARGS)

$(BUILD_OUTPUT): qlot-install
	mkdir -p "$(@D)"
	XDG_CACHE_HOME="$(CACHE_DIR)" $(QLOT) exec $(SBCL) --noinform --no-sysinit --no-userinit --non-interactive \
		--eval '(setf uiop/lisp-build:*compile-file-warnings-behaviour* :warn uiop/lisp-build:*compile-file-failure-behaviour* :warn)' \
		--eval '(asdf:load-asd (merge-pathnames "slt.asd" (truename ".")))' \
		--eval '(asdf:load-system :slt)' \
		--eval '(slt:dump-executable :output "$(BUILD_OUTPUT)")'

dump: $(BUILD_OUTPUT)

clean:
	rm -rf build
