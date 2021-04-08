---
title: Acutis
description: Acutis is a template language for the web & beyond.
showTitle: false
layout: layout.acutis
next:
  text: Read how to get started 👉
  url: /introduction/
---

Acutis is a template engine that uses pattern matching, strong typing,
asynchronous templates, and template components. It's built in [ReScript]
and designed for JavaScript. It's comparable to engines like Mustache,
Handlebars, Liquid, and Nunjucks.

[Source code][1] | [Changelog][2]

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

[ReScript]: https://rescript-lang.org/
[1]: https://github.com/johnridesabike/acutis 
[2]: https://github.com/johnridesabike/acutis/blob/master/CHANGELOG.md