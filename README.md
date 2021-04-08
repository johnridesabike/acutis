<p align="center"><img src="./docs/favicon.svg" height="64" width="64" alt="Acutis icon." /></p>

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
    Copyright 2021 John Jackson

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at
 
        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.