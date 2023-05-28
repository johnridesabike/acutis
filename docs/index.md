---
title: Acutis
description: Acutis is a template language for the web & beyond.
showTitle: false
layout: main.acutis.js
next:
  text: Read how to get started 👉
  url: /introduction/
---

Acutis is a template language that features static typing, pattern matching, and
asynchronous template components. You can render the templates like a script or
compile them to JavaScript files. It's a simple alternative to engines like
Mustache, Handlebars, Liquid, and Nunjucks.

[Source code] | [Changelog]

```acutis
{%~ interface
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
      {% match image with null ~%}
        {* no image *}
      {%~ with !{src, alt} ~%}
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

[See how this example compiles in the playground][example].

[source code]: https://github.com/johnridesabike/acutis
[changelog]: https://github.com/johnridesabike/acutis/blob/master/CHANGELOG.md
[example]:
  http://localhost:8080/acutis/playground/?props=ewogICJzaXRlVGl0bGUiOiAiTXkgQmxvZyIsCiAgImJsb2dQb3N0cyI6IFsKICAgIHsKICAgICAgInRpdGxlIjogIkhlbGxvLCB3b3JsZCEiLAogICAgICAiYXV0aG9yIjogeyAibmFtZSI6ICJKb2huIiB9LAogICAgICAiZGF0ZSI6ICIyMDIwLTExLTMwIiwKICAgICAgImltYWdlIjogbnVsbCwKICAgICAgImNvbnRlbnQiOiAiPHA%2BVGhpcyBpcyBteSBmaXJzdCBwb3N0PC9wPiIKICAgIH0KICBdCn0%3D&source=eyUgaW50ZXJmYWNlCiAgc2l0ZVRpdGxlID0gc3RyaW5nCiAgYmxvZ1Bvc3RzID0KICAgIFsKICAgICAgIHsKICAgICAgICAgIHRpdGxlOiBzdHJpbmcsCiAgICAgICAgICBhdXRob3I6IHtuYW1lOiA%2Fc3RyaW5nfSwKICAgICAgICAgIGRhdGU6IHN0cmluZywKICAgICAgICAgIGltYWdlOiA%2Fe2FsdDogc3RyaW5nLCBzcmM6IHN0cmluZ30sCiAgICAgICAgICBjb250ZW50OiBzdHJpbmcKICAgICAgIH0KICAgIF0KLyB%2BJX0KPGgxPiBCbG9nIHBvc3RzIGZvciB7eyBzaXRlVGl0bGUgfX0gPC9oMT4KeyV%2BIG1hcCBibG9nUG9zdHMgd2l0aCB7aW1hZ2UsIHRpdGxlLCBjb250ZW50LCBkYXRlLCBhdXRob3I6IHtuYW1lfX0gJX0KICA8YXJ0aWNsZSBjbGFzcz0iaC1lbnRyeSI%2BCiAgICA8aGVhZGVyPgogICAgICB7JX4gbWF0Y2ggaW1hZ2Ugd2l0aCBudWxsIH4lfQogICAgICAgIHsqIG5vIGltYWdlICp9CiAgICAgIHslfiB3aXRoICF7c3JjLCBhbHR9ICV9CiAgICAgICAgPGltZyBzcmM9Int7IHNyYyB9fSIgYWx0PSJ7eyBhbHQgfX0iPgogICAgICB7JSAvbWF0Y2ggfiV9CiAgICAgIDxoMiBjbGFzcz0icC1uYW1lIj4ge3sgdGl0bGUgfX0gPC9oMj4KICAgICAgPHNwYW4gY2xhc3M9InAtYXV0aG9yIj4gQnkge3sgbmFtZSA%2FICJBbm9ueW1vdXMiIH19IDwvc3Bhbj4KICAgICAgPHNwYW4gY2xhc3M9ImR0LXB1Ymxpc2hlZCI%2BIFBvc3RlZCBvbiB7eyBkYXRlIH19IDwvc3Bhbj4KICAgIDwvaGVhZGVyPgogICAgPGRpdiBjbGFzcz0iZS1jb250ZW50Ij4ge3t7IGNvbnRlbnQgfX19IDwvZGl2PgogIDwvYXJ0aWNsZT4KeyUgL21hcCAlfQ%3D%3D
