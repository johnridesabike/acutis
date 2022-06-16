default: build

.PHONY: build
build:
	dune build

.PHONY: build-release
build-release:
	dune build --profile=release

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
