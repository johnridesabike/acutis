---
title: Getting started with the Acutis language
description: How to install and start using the Acutis template language.
showTitle: true
layout: layout.acutis
next: 
  text: Read the language manual 👉
  url: /manual/
---

Acutis is designed for you to integrate with your existing build process
through its [API], so currently there aren't any configuration files or
starter projects to clone.

To quickly start experimenting with it, you can install it and use the [CLI]
without needing a complicated build system.

## Why Acutis?

The template engines that inspired Acutis are all great software, and you
will be served very well by them. So why use this?

I created Acutis because there were specific language features I wanted. I
was inspired by the functional, component-based, composable design in React
as well as the pattern-matching abilities in ReScript (and OCaml, Reason, and
all of the other languages in that family). You'll get the most out of using
Acutis if you want to experiment with something different. Be prepared to get
your hands dirty to fit it into your existing build system.

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

If you're using it in a ReScript project, then add it to your `bsconfig.json`
file.

```json
{
  "bs-dependencies": ["acutis-lang"]
}
```

## Conventions

I use the `.acutis` file extension for my templates, but there's nothing in
the language that enforces that. You can use something else (for example,
`.html`) or you can configure your editor to treat `.acutis` files as HTML
(or whatever format you want).

[license]: /license/
[API]: /api/
[CLI]: /api/#acutis-command-line-interface-(cli)