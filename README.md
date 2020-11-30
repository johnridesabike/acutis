## Acutis language

Acutis is a template language for web pages and more. It features strong
typing, pattern matching, template components, and asychronous templates. It
can run in a browser or in Node.

⚠️ Acutis is young and not battle tested. ⚠️

```html
<h1> Blog posts for {{ siteTitle }} </h1>
{% map blogPosts with {image, title, date, excerpt, author: {name}} %}
  <article>
    <header>
      {% match image
         with null %}
        {* no image *}
      {% with {src, alt} %}
        <img src="{{ src }}" alt="{{ alt }}" />
      {% /match}
      <h2> {{ title }} </h2>
      <span class="byline"> By {{ name }} </span>
      {% DateTime date format="MMMM Do, YYYY" / %}
    </header>
    <p> {{ excerpt }} </p>
  </article>
{% /map %}
```

## Getting started

[Read the docs here][1].

## Development

[Clone the source here][2]. Acutis is built with [ReScript] and compiled to
JavaScript.

Compile the source in watch mode:

```shell
yarn start
```

Run the tests:

```shell
yarn test
```

Bundle the compiled JavaScript:

```shell
yarn bundle
```

[1]: https://johnridesa.bike/acutis/
[2]: https://github.com/johnridesabike/acutis/
[ReScript]: https://rescript-lang.org/