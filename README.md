<p align="center"><img src="./docs/icon.svg" height="64" width="64" alt="Acutis icon." /></p>

## Acutis language

Acutis is a template language that features static typing, pattern matching, and
asynchronous template components. It's a simple, yet powerful alternative to
engines like Mustache, Handlebars, Liquid, and Nunjucks.

Acutis is an experimental, personal project and is not stable.

```acutis
{% interface
  siteTitle = string
  blogPosts =
    [
       {
          title: string,
          author: {name: ?string},
          date: string,
          image: ?{alt: string, src: string},
          content: string
       }
    ]
/ ~%}
<h1> Blog posts for {{ siteTitle }} </h1>
{%~ map blogPosts with {image, title, content, date, author: {name}} %}
  <article class="h-entry">
    <header>
      {%~ match image with null ~%}
        {* no image *}
      {%~ with !{src, alt} %}
        <img src="{{ src }}" alt="{{ alt }}">
      {% /match ~%}
      <h2 class="p-name"> {{ title }} </h2>
      <span class="p-author"> By {{ name ? "Anonymous" }} </span>
      <span class="dt-published"> Posted on {{ date }} </span>
    </header>
    <div class="e-content"> {{{ content }}} </div>
  </article>
{% /map %}
```

[Read the documentation here](https://johnridesa.bike/acutis/).

## Usage in OCaml

The Acutis source is written in OCaml. You can install it with the OCaml package
manager, OPAM.

```shell
opam pin https://github.com/johnridesabike/acutis.git
```

## JavaScript & Eleventy plugin

You can use npm to install a JavaScript build of Acutis. This includes an
[Eleventy] plugin.

[eleventy]: https://www.11ty.dev/

Install the JavaScript package in your project:

```shell
npm install acutis-lang
```

Import the package in your Eleventy-powered site:

```javascript
const acutis = require("acutis-lang");
const acutisEleventy = require("acutis-lang/eleventy");
```

## Development

Clone this repository with git. Then create a development OPAM switch:

```shell
opam switch create . --deps-only --with-test --with-doc
```

Compile the code:

```shell
dune build
```

Run tests:

```shell
dune runtest
```

## License

    Copyright (c) 2022 John Jackson.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at http://mozilla.org/MPL/2.0/.
