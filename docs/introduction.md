---
title: Getting started with the Acutis language
description: How to install and start using the Acutis template language.
showTitle: true
layout: main.acutis
next:
  text: Read the language manual 👉
  url: /manual/
---

Acutis is written in [OCaml], and familiarity with the OCaml toolchain is a
prerequisite for using it. It also comes with a JavaScript API to use in web or
Node.js projects.

[ocaml]: https://ocaml.org/

Beware: both the OCaml and JavaScript API are unstable and experimental!

[The OCaml API is documented here](../api/).

You can also use the [CLI](<#acutis-command-line-interface-(cli)>).

## Why Acutis?

Acutis is an experiment with a new kind of template language.

I created Acutis because there were specific language and toolchain features I
wanted in my templates that I couldn't find in existing projects. I was inspired
by the declarative, component-based design in React as well as the
pattern-matching abilities in OCaml.

## What state is it in?

Acutis is young software. I'm currently the only person who uses it. Also, I
haven't developed any editor tooling for it yet. Use it if you don't mind
getting your getting your hands dirty.

## Installation: OPAM

The OCaml package manager, OPAM, can install Acutis by using the URL to its git
repository.

```shell
opam pin https://github.com/johnridesabike/acutis.git
```

## Installation: JavaScript

The JavaScript API is available through npm.

```shell
npm install acutis-lang
```

## Editor plugin

### VS Code

You can enable syntax highlighting with
[the VS Code extension](https://marketplace.visualstudio.com/items?itemName=jbpjackson.acutis-vscode).

## Examples

- This documentation uses Acutis with [Eleventy]. [Browse the source here][1].
- [The Acutis CLI][2].

[1]: https://github.com/johnridesabike/acutis/tree/master/docs
[2]: https://github.com/johnridesabike/acutis/blob/master/bin/cli.ml
[eleventy]: https://www.11ty.dev/

## Acutis command line interface (CLI)

If you install Acutis through OPAM, then it comes with a command line tool
called `acutis`. This accepts JSON data and a list of template filenames, and it
prints the rendered output.

```shell
acutis index.acutis src/**/*.acutis < data.json > dist/index.html
```

Full options:

```txt
Parse and execute Acutis language templates.

Usage:
  acutis [options] [template] [...templates]

Options:
  --data     The path to a JSON data file. Default: stdin.
  --output   The path to write the output. Default: stdout.
  --version  Show the version number and exit.
  -help      Display this list of options
  --help     Display this list of options
```
