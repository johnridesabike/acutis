default: build-release

.PHONY: build-release
build-release:
	dune build --profile=release

.PHONY: build
build:
	dune build

.PHONY: build-watch
build-watch:
	dune build --watch

.PHONY: test
test:
	dune build @runtest

.PHONY: test-watch
test-watch:
	dune build @runtest --watch

.PHONY: clean
clean:
	dune clean

.PHONY: format
format:
	dune build --auto-promote @fmt

.PHONY: docs
docs:
	dune build @doc

.PHONY: docs-watch
docs-watch:
	dune build @doc --watch
