## Acutis language

Acutis is a template language for web pages and more. It features strong
typing, pattern matching, template components, and asynchronous templates. It
can run in a browser or in Node.

⚠️ Acutis is young and not battle tested. ⚠️

```jinja2
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

[Read the docs here][1].

## Development

[Read the development docs][2].

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
[2]: https://johnridesa.bike/acutis/development/