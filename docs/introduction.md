---
title: Getting started with the Acutis language
description: How to install and start using the Acutis template language.
showTitle: true
layout: layout.acutis
next: 
  text: Read the language manual 👉
  url: /manual/
---

Acutis allows you to integrate it with your existing build process through
its [API]. Currently, there aren't any configuration files or starter projects
to clone.

To start quickly experimenting with it without a build system, you can use
the [CLI].

## Why Acutis?

The template engines that inspired Acutis are all great software, and you
will be served very well by them. So why use this?

I created Acutis because there were specific language features I wanted. I
was inspired by the functional, component-based, composable design in React
as well as the pattern-matching abilities in ReScript (and OCaml, Reason, and
all of the other languages in that family). You'll get the most out of using
Acutis if you want to experiment with something different.

## What state is it in?

Acutis is young software. In the "make it work, make it right, make it fast"
order, it's just at the end of the "make it work" stage. So far, I'm the only
person who uses it who I know of, and it's far from battle tested.

Also, I haven't developed any editor tooling for it yet.

## Installation

Acutis is available through npm. You can install it locally for a project:

```shell
npm install acutis-lang
```

or globally:

```shell
npm install -g acutis-lang
```

If you're using it in a ReScript project, then add it to the dependencies in
your `bsconfig.json` file.

```json
{
  "bs-dependencies": ["acutis-lang"]
}
```

⚠️ Acutis requires ReScript compiler version 9.0 or higher.

## Editor plugin

### VS Code

You can enable syntax highlighting with [the VS Code extension.][vscode]

## Examples

- This documentation uses Acutis with [Eleventy]. [Browse the source here][1].
- [The Acutis CLI][2].

[1]: https://github.com/johnridesabike/acutis/tree/master/docs
[2]: https://github.com/johnridesabike/acutis/blob/master/cli
[API]: ../api/
[CLI]: ../api/#acutis-command-line-interface-(cli)
[Eleventy]: https://www.11ty.dev/
[license]: ../license/
[vscode]: https://marketplace.visualstudio.com/items?itemName=jbpjackson.acutis-vscode