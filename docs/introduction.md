---
title: Getting started with the Acutis language
description: How to install and start using the Acutis template language.
showTitle: true
layout: main.acutis.js
next:
  text: Read the language manual 👉
  url: /manual/
---

Acutis is written in [OCaml], and familiarity with the OCaml toolchain is a
prerequisite for using it. It also comes with a JavaScript API to use in web or
Node.js projects.

[ocaml]: https://ocaml.org/

Beware: both the OCaml and JavaScript API are unstable and experimental.

[The OCaml API is documented here](../api/).

You can also use the [CLI].

[cli]: #acutis-command-line-interface-(cli)

## Why Acutis?

Acutis is an experiment with a new kind of template language.

I created Acutis because there were specific language features I wanted in my
templates that I couldn't find in existing projects. I was inspired by the
declarative, component-based design in React as well as the pattern-matching
abilities in OCaml.

## What state is it in?

Acutis is young software. I'm currently the only person who uses it. Also, I
haven't developed much editor tooling for it yet. Use it if you don't mind
getting your hands dirty.

## Installation: OCaml

The OCaml package manager, OPAM, can install Acutis through the git repository.

```shell
opam pin https://github.com/johnridesabike/acutis.git
```

This command will install three packages:

<dl>
<dt class="font-monospace">
  acutis
<dd>
  The core Acutis library.

<dt class="font-monospace">
  acutis_json
<dd>
  A JSON-compatible implementation. This also includes the CLI.

<dt class="font-monospace">
  acutis_js
<dd>
  A JavaScript-compatible implementation for compiling with js_of_ocaml.
</dl>

## Installation: JavaScript

The Acutis JavaScript package is available through npm.

```shell
npm install acutis-lang
```

[You can view the JavaScript API here][3]. (It's in OCaml syntax. I'm sorry.)

### Using with Eleventy

[Eleventy] is a static site generator in JavaScript. Acutis includes an Eleventy
template language plugin.

After installing the npm package, you can import the compiler and the plugin
into your JavaScript project.

```javascript
const acutis = require("acutis-lang"); // The compiler and runtime.
const acutisEleventy = require("acutis-lang/eleventy"); // The plugin.
```

Inside your Eleventy configuration, you can enable the plugin with Eleventy's
`addPlugin` function.

```javascript
eleventyConfig.addPlugin(acutisEleventy);
```

Beware: loading Acutis in Node.js has the side-effect of modifying how all
uncaught exceptions are handled. This should not affect well-behaved code, but
may make debugging errors more complicated.

## Running directly versus JavaScript compilation

Acutis supports two main targets: running the compiled template directly with
its built-in runtime, or emitting a JavaScript file that can be run on its own.

Both are available either through the CLI or the JavaScript interface. Depending
on what your needs are, one may be more useful than the other. JavaScript
compilation is slightly more experimental.

## Editor plugins

These offer basic syntax highlighting and indentation features.

- [Vim plugin](https://github.com/johnridesabike/vim-acutis)
- [VS Code plugin](https://marketplace.visualstudio.com/items?itemName=jbpjackson.acutis-vscode)

## Examples

- This documentation uses Acutis with [Eleventy]. [Browse the source here][1].
- [The Acutis CLI][2].
- [The JavaScript interface][3].
- [The Eleventy plugin source][4].

[1]: https://github.com/johnridesabike/acutis/tree/master/docs
[2]: https://github.com/johnridesabike/acutis/blob/master/bin/cli.ml
[3]: https://github.com/johnridesabike/acutis/blob/master/js/acutis_js.ml
[4]: https://github.com/johnridesabike/acutis/blob/master/eleventy.js
[eleventy]: https://www.11ty.dev/

## Acutis command line interface (CLI)

If you install Acutis through OPAM, then the `acutis_json` package comes with a
command-line tool called `acutis`. This accepts JSON data and a list of template
filenames, and it prints the rendered output.

```shell
acutis index.acutis src/**/*.acutis < data.json > dist/index.html
```

Full options:

```txt
Usage:
  acutis [OPTIONS...] [TEMPLATE] [COMPONENTS...]

Compile and render Acutis language templates.

Options:
  --mode {render|js|cjs}
                Either render the template, compile it to a JavaScript module, or compile it to a CommonJS module. Default: render.
  --output      The path to write the output. Default: stdout.
  --data        The path to a JSON file to be used with --mode=render. Default: stdin.
  --fun         Add an external JavaScript function as a component. This takes three arguments: file path, function name, and type interface.
  --version     Print the version number and exit.
  --printast    Print the template's untyped AST form and exit.
  --printtypes  Print the template's type interface and exit.
  --printopt    Print the template's optimized form and exit.
  -help         Display this list of options
  --help        Display this list of options
```
