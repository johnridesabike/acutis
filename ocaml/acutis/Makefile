default: build

.PHONY: build
build:
	dune build

.PHONY: watch
watch:
	dune build --watch

.PHONY: test
test:
	dune build @runtest

.PHONY: test-watch
test-watch:
	dune build @runtest --watch

.PHONY: clean
	dune clean

format:
	dune build --auto-promote @fmt
