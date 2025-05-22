---
title: Getting started with the Acutis language
description: How to install and start using the Acutis template language.
showTitle: true
layout: main.acutis.js
next:
  text: Read the language manual 👉
  url: /manual/
---

Acutis is written in [OCaml], so familiarity with the OCaml toolchain is a
prerequisite for using it. It also comes with a JavaScript API to use in web or
Node.js projects.

[ocaml]: https://ocaml.org/

Beware: both the OCaml and JavaScript API are unstable and experimental.

[The OCaml API is documented here](/api/acutis/Acutis/).

You can also use the [CLI].

[cli]: #acutis-command-line-interface-(cli)

## Why Acutis?

Acutis is an experiment with a new kind of template language.

I created Acutis because there were specific language features I wanted in my
templates that I couldn't find in existing projects. I was inspired by the
declarative, component-based design in React as well as the pattern-matching
abilities in OCaml.

## Installation: OCaml

The OCaml package manager, OPAM, can install Acutis through the git repository.

```shell
opam pin acutis git+https://git.sr.ht/~johnridesabike/acutis
```

## Installation: JavaScript

The Acutis JavaScript package is available through npm.

```shell
npm install acutis-lang
```

[You can view the JavaScript API here][3]. (It's in OCaml syntax, sorry. I'll
document it if it ever becomes stable.)

### Using with Eleventy

[Eleventy] is a static site generator in JavaScript. Acutis includes an Eleventy
template language plugin.

After installing the npm package, you can import the compiler and the plugin
into your JavaScript project.

```javascript
// The compiler and runtime for creating components:
import acutis from "acutis-lang";
// The Eleventy plugin:
import * as acutisEleventy from "acutis-lang/eleventy";
```

Inside your Eleventy configuration, you can enable the plugin with Eleventy's
`addPlugin` function.

```javascript
export default (eleventyConfig) => {
  // ...
  // Render the templates in memory:
  eleventyConfig.addPlugin(acutisEleventy.plugin);
  // Or print templates into ECMAScript modules for Eleventy to render:
  eleventyConfig.addPlugin(acutisEleventy.printESM);
  // ...
};
```

Beware: loading Acutis in Node.js has the side-effect of modifying how all
uncaught exceptions are handled. This should not affect well-behaved code, but
may make debugging errors more complicated.

## Running templates directly versus JavaScript compilation

Acutis supports two main targets: running the compiled template directly with
its built-in runtime, or emitting a JavaScript file that can be run on its own.

Both are available either through the [CLI] or the JavaScript interface.
Depending on what your needs are, one may be more useful than the other.
JavaScript compilation is slightly more experimental.

## Editor plugins

This offers basic syntax highlighting and indentation features.

- [Vim plugin](https://git.sr.ht/~johnridesabike/vim-acutis)

## Examples

- [The documentation source][1], built with Acutis and [Eleventy].
- [The Acutis CLI][2].
- [The JavaScript interface][3].
- [The Eleventy plugin source][4].

[1]: https://git.sr.ht/~johnridesabike/acutis/tree/master/item/docs
[2]: https://git.sr.ht/~johnridesabike/acutis/tree/master/item/acutis_cli.ml
[3]: https://git.sr.ht/~johnridesabike/acutis/tree/master/item/acutis_js.ml
[4]: https://git.sr.ht/~johnridesabike/acutis/tree/master/item/eleventy.js
[eleventy]: https://www.11ty.dev/

## Acutis command line interface (CLI)

If you install Acutis through OPAM, then it comes with a command-line tool
called `acutis`. This accepts JSON data and a list of template filenames, and it
prints the rendered output.

Example:

```shell
acutis index.acutis components/**/*.acutis < data.json > index.html
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
