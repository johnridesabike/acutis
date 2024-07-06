---
title: Acutis
description: Acutis is a simple and type-safe template language.
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
~%}
<h1> Blog posts for {% siteTitle %} </h1>
{%~ map blogPosts with {image, title, content, date, author: {name}} %}
  <article class="h-entry">
    <header>
      {% match image with null ~%}
        {* no image *}
      {%~ with !{src, alt} ~%}
        <img src="{% src %}" alt="{% alt %}">
      {% /match ~%}
      <h2 class="p-name"> {% title %} </h2>
      <span class="p-author"> By {% name ? "Anonymous" %} </span>
      <span class="dt-published"> Posted on {% date %} </span>
    </header>
    <div class="e-content"> {{% content %}} </div>
  </article>
{%~ /map %}
```

[See how this example compiles in the playground][example].

[source code]: https://github.com/johnridesabike/acutis
[changelog]: https://github.com/johnridesabike/acutis/blob/master/CHANGELOG.md
[example]:
  ./playground/?props=ewogICJzaXRlVGl0bGUiOiAiTXkgQmxvZyIsCiAgImJsb2dQb3N0cyI6IFsKICAgIHsKICAgICAgInRpdGxlIjogIk15IHNlY29uZCBwb3N0IiwKICAgICAgImF1dGhvciI6IHsgIm5hbWUiOiAiSm9obiIgfSwKICAgICAgImRhdGUiOiAiMjAyMC0xMi0wMSIsCiAgICAgICJpbWFnZSI6IG51bGwsCiAgICAgICJjb250ZW50IjogIjxwPlRoaXMgaXMgbXkgc2Vjb25kIHBvc3Q8L3A%2BIgogICAgfSwKICAgIHsKICAgICAgInRpdGxlIjogIkhlbGxvLCB3b3JsZCEiLAogICAgICAiYXV0aG9yIjogeyAibmFtZSI6ICJKb2huIiB9LAogICAgICAiZGF0ZSI6ICIyMDIwLTExLTMwIiwKICAgICAgImltYWdlIjogbnVsbCwKICAgICAgImNvbnRlbnQiOiAiPHA%2BVGhpcyBpcyBteSBmaXJzdCBwb3N0PC9wPiIKICAgIH0KICBdCn0%3D&source=eyUgaW50ZXJmYWNlCiAgc2l0ZVRpdGxlID0gc3RyaW5nCiAgYmxvZ1Bvc3RzID0KICAgIFsKICAgICAgIHsKICAgICAgICAgIHRpdGxlOiBzdHJpbmcsCiAgICAgICAgICBhdXRob3I6IHtuYW1lOiA%2Fc3RyaW5nfSwKICAgICAgICAgIGRhdGU6IHN0cmluZywKICAgICAgICAgIGltYWdlOiA%2Fe2FsdDogc3RyaW5nLCBzcmM6IHN0cmluZ30sCiAgICAgICAgICBjb250ZW50OiBzdHJpbmcKICAgICAgIH0KICAgIF0KfiV9CjxoMT4gQmxvZyBwb3N0cyBmb3IgeyUgc2l0ZVRpdGxlICV9IDwvaDE%2BCnslfiBtYXAgYmxvZ1Bvc3RzIHdpdGgge2ltYWdlLCB0aXRsZSwgY29udGVudCwgZGF0ZSwgYXV0aG9yOiB7bmFtZX19ICV9CiAgPGFydGljbGUgY2xhc3M9ImgtZW50cnkiPgogICAgPGhlYWRlcj4KICAgICAgeyUgbWF0Y2ggaW1hZ2Ugd2l0aCBudWxsIH4lfQogICAgICAgIHsqIG5vIGltYWdlICp9CiAgICAgIHslfiB3aXRoICF7c3JjLCBhbHR9IH4lfQogICAgICAgIDxpbWcgc3JjPSJ7JSBzcmMgJX0iIGFsdD0ieyUgYWx0ICV9Ij4KICAgICAgeyUgL21hdGNoIH4lfQogICAgICA8aDIgY2xhc3M9InAtbmFtZSI%2BIHslIHRpdGxlICV9IDwvaDI%2BCiAgICAgIDxzcGFuIGNsYXNzPSJwLWF1dGhvciI%2BIEJ5IHslIG5hbWUgPyAiQW5vbnltb3VzIiAlfSA8L3NwYW4%2BCiAgICAgIDxzcGFuIGNsYXNzPSJkdC1wdWJsaXNoZWQiPiBQb3N0ZWQgb24geyUgZGF0ZSAlfSA8L3NwYW4%2BCiAgICA8L2hlYWRlcj4KICAgIDxkaXYgY2xhc3M9ImUtY29udGVudCI%2BIHt7JSBjb250ZW50ICV9fSA8L2Rpdj4KICA8L2FydGljbGU%2BCnslfiAvbWFwICV9
