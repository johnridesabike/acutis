---
title: Acutis
description: Acutis is a template language for the web & beyond.
showTitle: false
layout: main.acutis
next:
  text: Read how to get started 👉
  url: /introduction/
---

Acutis is a template language that features static typing, pattern matching, and
asynchronous template components. It's a simple, yet powerful alternative to
engines like Mustache, Handlebars, Liquid, and Nunjucks.

[Source code] | [Changelog]

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

[source code]: https://github.com/johnridesabike/acutis
[changelog]: https://github.com/johnridesabike/acutis/blob/master/CHANGELOG.md
