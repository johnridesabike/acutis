<p align="center"><img src="./docs/icon.svg" height="64" width="64" alt="Acutis icon." /></p>

## Acutis language

Acutis is a template language that features static typing, pattern matching, and
asynchronous template components. You can execute your templates like scripts or
compile them to self-contained JavaScript files.

Acutis is an experimental, personal project and is not stable.

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

[example]:
  https://johnridesa.bike/acutis/playground/?props=ewogICJzaXRlVGl0bGUiOiAiTXkgQmxvZyIsCiAgImJsb2dQb3N0cyI6IFsKICAgIHsKICAgICAgInRpdGxlIjogIk15IHNlY29uZCBwb3N0IiwKICAgICAgImF1dGhvciI6IHsgIm5hbWUiOiAiSm9obiIgfSwKICAgICAgImRhdGUiOiAiMjAyMC0xMi0wMSIsCiAgICAgICJpbWFnZSI6IG51bGwsCiAgICAgICJjb250ZW50IjogIjxwPlRoaXMgaXMgbXkgc2Vjb25kIHBvc3Q8L3A%2BIgogICAgfSwKICAgIHsKICAgICAgInRpdGxlIjogIkhlbGxvLCB3b3JsZCEiLAogICAgICAiYXV0aG9yIjogeyAibmFtZSI6ICJKb2huIiB9LAogICAgICAiZGF0ZSI6ICIyMDIwLTExLTMwIiwKICAgICAgImltYWdlIjogbnVsbCwKICAgICAgImNvbnRlbnQiOiAiPHA%2BVGhpcyBpcyBteSBmaXJzdCBwb3N0PC9wPiIKICAgIH0KICBdCn0%3D&source=eyUgaW50ZXJmYWNlCiAgc2l0ZVRpdGxlID0gc3RyaW5nCiAgYmxvZ1Bvc3RzID0KICAgIFsKICAgICAgIHsKICAgICAgICAgIHRpdGxlOiBzdHJpbmcsCiAgICAgICAgICBhdXRob3I6IHtuYW1lOiA%2Fc3RyaW5nfSwKICAgICAgICAgIGRhdGU6IHN0cmluZywKICAgICAgICAgIGltYWdlOiA%2Fe2FsdDogc3RyaW5nLCBzcmM6IHN0cmluZ30sCiAgICAgICAgICBjb250ZW50OiBzdHJpbmcKICAgICAgIH0KICAgIF0KfiV9CjxoMT4gQmxvZyBwb3N0cyBmb3Ige3sgc2l0ZVRpdGxlIH19IDwvaDE%2BCnslfiBtYXAgYmxvZ1Bvc3RzIHdpdGgge2ltYWdlLCB0aXRsZSwgY29udGVudCwgZGF0ZSwgYXV0aG9yOiB7bmFtZX19ICV9CiAgPGFydGljbGUgY2xhc3M9ImgtZW50cnkiPgogICAgPGhlYWRlcj4KICAgICAgeyUgbWF0Y2ggaW1hZ2Ugd2l0aCBudWxsIH4lfQogICAgICAgIHsqIG5vIGltYWdlICp9CiAgICAgIHslfiB3aXRoICF7c3JjLCBhbHR9IH4lfQogICAgICAgIDxpbWcgc3JjPSJ7eyBzcmMgfX0iIGFsdD0ie3sgYWx0IH19Ij4KICAgICAgeyUgL21hdGNoIH4lfQogICAgICA8aDIgY2xhc3M9InAtbmFtZSI%2BIHt7IHRpdGxlIH19IDwvaDI%2BCiAgICAgIDxzcGFuIGNsYXNzPSJwLWF1dGhvciI%2BIEJ5IHt7IG5hbWUgPyAiQW5vbnltb3VzIiB9fSA8L3NwYW4%2BCiAgICAgIDxzcGFuIGNsYXNzPSJkdC1wdWJsaXNoZWQiPiBQb3N0ZWQgb24ge3sgZGF0ZSB9fSA8L3NwYW4%2BCiAgICA8L2hlYWRlcj4KICAgIDxkaXYgY2xhc3M9ImUtY29udGVudCI%2BIHt7eyBjb250ZW50IH19fSA8L2Rpdj4KICA8L2FydGljbGU%2BCnslfiAvbWFwICV9

[Read the documentation here](https://johnridesa.bike/acutis/).

## Usage in OCaml

The Acutis source is written in OCaml. You can install it with the OCaml package
manager, OPAM.

```shell
opam pin https://github.com/johnridesabike/acutis.git
```

## JavaScript & Eleventy plugin

You can use npm to install a JavaScript build of Acutis. This includes an
[Eleventy] plugin.

[eleventy]: https://www.11ty.dev/

Install the JavaScript package in your project:

```shell
npm install acutis-lang
```

Import the package in your Eleventy-powered site:

```javascript
const acutis = require("acutis-lang");
const acutisEleventy = require("acutis-lang/eleventy");
```

## Development

Clone this repository with git. Then create a development OPAM switch:

```shell
opam switch create . --deps-only --with-test --with-doc
```

Compile the code:

```shell
dune build
```

Run tests:

```shell
dune runtest
```

## License

    Copyright (c) 2022 John Jackson.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at http://mozilla.org/MPL/2.0/.
