OCICL ?= ocicl
BUILD_OUTPUT ?= build/slt
LISP = scripts/lisp

.PHONY: deps test run run-sdl2 dump clean

deps: ocicl/.stamp

ocicl/.stamp: ocicl.csv
	$(OCICL) install
	touch $@

test: deps
	./run-tests

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
