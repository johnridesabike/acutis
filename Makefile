default: build

.PHONY: build
build:
	opam exec -- dune build

.PHONY: release
release:
	opam exec -- dune build --profile=release

.PHONY: watch
watch:
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

.PHONY: dev
dev:
	opam switch create . --deps-only --with-test --with-doc

.PHONY: dev-install
dev-install:
	opam install . --deps-only --with-test --with-doc

.PHONY: format
format:
	opam exec -- dune build --auto-promote @fmt

.PHONY: docs
docs:
	opam exec -- dune build @doc

.PHONY: docs-watch
docs-watch:
	opam exec -- dune build @doc --watch

.PHONY: docs-site
docs-site:
	opam exec -- dune build @doc
	opam exec -- dune build --profile=release
	cd docs && npm run build
