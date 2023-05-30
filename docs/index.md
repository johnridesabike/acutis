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
asynchronous template components. You can execute your templates like scripts or
compile them to self-contained JavaScript files.

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
{%~ /map %}
```

[See how this example compiles in the playground][example].

[source code]: https://github.com/johnridesabike/acutis
[changelog]: https://github.com/johnridesabike/acutis/blob/master/CHANGELOG.md
[example]:
  ./playground/?props=ewogICJzaXRlVGl0bGUiOiAiTXkgQmxvZyIsCiAgImJsb2dQb3N0cyI6IFsKICAgIHsKICAgICAgInRpdGxlIjogIk15IHNlY29uZCBwb3N0IiwKICAgICAgImF1dGhvciI6IHsgIm5hbWUiOiAiSm9obiIgfSwKICAgICAgImRhdGUiOiAiMjAyMC0xMi0wMSIsCiAgICAgICJpbWFnZSI6IG51bGwsCiAgICAgICJjb250ZW50IjogIjxwPlRoaXMgaXMgbXkgc2Vjb25kIHBvc3Q8L3A%2BIgogICAgfSwKICAgIHsKICAgICAgInRpdGxlIjogIkhlbGxvLCB3b3JsZCEiLAogICAgICAiYXV0aG9yIjogeyAibmFtZSI6ICJKb2huIiB9LAogICAgICAiZGF0ZSI6ICIyMDIwLTExLTMwIiwKICAgICAgImltYWdlIjogbnVsbCwKICAgICAgImNvbnRlbnQiOiAiPHA%2BVGhpcyBpcyBteSBmaXJzdCBwb3N0PC9wPiIKICAgIH0KICBdCn0%3D&source=eyV%2BIGludGVyZmFjZQogIHNpdGVUaXRsZSA9IHN0cmluZwogIGJsb2dQb3N0cyA9CiAgICBbCiAgICAgICB7CiAgICAgICAgICB0aXRsZTogc3RyaW5nLAogICAgICAgICAgYXV0aG9yOiB7bmFtZTogP3N0cmluZ30sCiAgICAgICAgICBkYXRlOiBzdHJpbmcsCiAgICAgICAgICBpbWFnZTogP3thbHQ6IHN0cmluZywgc3JjOiBzdHJpbmd9LAogICAgICAgICAgY29udGVudDogc3RyaW5nCiAgICAgICB9CiAgICBdCi8gfiV9CjxoMT4gQmxvZyBwb3N0cyBmb3Ige3sgc2l0ZVRpdGxlIH19IDwvaDE%2BCnslfiBtYXAgYmxvZ1Bvc3RzIHdpdGgge2ltYWdlLCB0aXRsZSwgY29udGVudCwgZGF0ZSwgYXV0aG9yOiB7bmFtZX19ICV9CiAgPGFydGljbGUgY2xhc3M9ImgtZW50cnkiPgogICAgPGhlYWRlcj4KICAgICAgeyUgbWF0Y2ggaW1hZ2Ugd2l0aCBudWxsIH4lfQogICAgICAgIHsqIG5vIGltYWdlICp9CiAgICAgIHslfiB3aXRoICF7c3JjLCBhbHR9IH4lfQogICAgICAgIDxpbWcgc3JjPSJ7eyBzcmMgfX0iIGFsdD0ie3sgYWx0IH19Ij4KICAgICAgeyUgL21hdGNoIH4lfQogICAgICA8aDIgY2xhc3M9InAtbmFtZSI%2BIHt7IHRpdGxlIH19IDwvaDI%2BCiAgICAgIDxzcGFuIGNsYXNzPSJwLWF1dGhvciI%2BIEJ5IHt7IG5hbWUgPyAiQW5vbnltb3VzIiB9fSA8L3NwYW4%2BCiAgICAgIDxzcGFuIGNsYXNzPSJkdC1wdWJsaXNoZWQiPiBQb3N0ZWQgb24ge3sgZGF0ZSB9fSA8L3NwYW4%2BCiAgICA8L2hlYWRlcj4KICAgIDxkaXYgY2xhc3M9ImUtY29udGVudCI%2BIHt7eyBjb250ZW50IH19fSA8L2Rpdj4KICA8L2FydGljbGU%2BCnslfiAvbWFwICV9
