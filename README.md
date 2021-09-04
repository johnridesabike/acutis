<p align="center"><img src="./docs/icon.svg" height="64" width="64" alt="Acutis icon." /></p>

## Acutis language

Acutis is a template language for building documents. It features strong
typing, pattern matching, template components, and asynchronous templates. It
can run in a browser or in Node.

Acutis is a small, personal project and is not stable yet. Use Acutis if you
want to experiment with a different kind of template language.


```acutis
<h1> Blog posts for {{ siteTitle }} </h1>
{% map blogPosts with {image, title, date, excerpt, author: {name}} %}
  <article>
    <header>
      {% match image
         with null %}
        {* no image *}
      {% with {src, alt} %}
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

```sh
npm install acutis-lang
```

You can use the Acutis API to add it to your build system.

[Read the docs](https://johnridesa.bike/acutis/).

[Changelog](https://github.com/johnridesabike/acutis/blob/master/CHANGELOG.md).

## Development

[Read the development docs](https://johnridesa.bike/acutis/development/).

Compile the source in watch mode:

```shell
yarn start
```

Run the tests:

```shell
yarn test
```

## License
    Copyright (c) 2021 John Jackson. 

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at http://mozilla.org/MPL/2.0/.