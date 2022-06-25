default: build-release

.PHONY: build-release
build-release:
	opam exec -- dune build --profile=release

.PHONY: build
build:
	opam exec -- dune build

.PHONY: build-watch
build-watch:
	opam exec -- dune build --watch

.PHONY: test
test:
	opam exec -- dune build @runtest

.PHONY: test-watch
test-watch:
	opam exec -- dune build @runtest --watch

.PHONY: clean
clean:
	opam exec -- dune clean

.PHONY: format
format:
	opam exec -- dune build --auto-promote @fmt

.PHONY: docs
docs:
	opam exec -- dune build @doc

.PHONY: docs-watch
docs-watch:
	opam exec -- dune build @doc --watch

.PHONY: dev-switch
dev-switch:
	opam switch create . --deps-only --with-test --with-doc

.PHONY: dev-install
dev-install:
	opam install . --deps-only --with-test --with-doc
