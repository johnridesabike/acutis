<p align="center"><img src="./docs/icon.svg" height="64" width="64" alt="Acutis icon." /></p>

## Acutis language

Acutis is a template language that features static typing, pattern matching, and
asynchronous template components. It's a simple, yet powerful alternative to
engines like Mustache, Handlebars, Liquid, and Nunjucks.

Acutis is an experimental, personal project and is not stable.

```acutis
<h1> Blog posts for {{ siteTitle }} </h1>
{% map blogPosts with {image, title, date, excerpt, author: {name}} %}
  <article>
    <header>
      {% match image
         with null %}
        {* no image *}
      {% with !{src, alt} %}
        <img src="{{ src }}" alt="{{ alt }}" />
      {% /match %}
      <h2> {{ title }} </h2>
      <span class="byline"> By {{ name ? "Anonymous" }} </span>
      {% DateTime date format="MMMM Do, YYYY" / %}
    </header>
    <p> {{ excerpt }} </p>
  </article>
{% /map %}
```

## Getting started

Acutis is written in OCaml. You will need to use the OCaml package manager,
OPAM.

```shell
opam pin https://github.com/johnridesabike/acutis.git
```

[Read the documentation here](https://johnridesa.bike/acutis/).

## Development

Once you clone this repository, create a development OPAM switch:

```shell
make dev
```

Run the compiler in watch mode:

```shell
make watch
```

Run tests:

```shell
make test
```

## License

    Copyright (c) 2022 John Jackson.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at http://mozilla.org/MPL/2.0/.
