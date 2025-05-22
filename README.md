![The Acutis icon.](./docs/icon.svg)

[Project home](https://sr.ht/~johnridesabike/acutis/) |
[Source code](https://git.sr.ht/~johnridesabike/acutis) |
[Mailing list](https://lists.sr.ht/~johnridesabike/public-inbox) |
[Documentation](https://acutis.johnridesa.bike/)

# The Acutis template language

Acutis is a template language that features static typing, pattern matching, and
asynchronous template components. You can execute your templates like scripts or
compile them to self-contained JavaScript files.

Acutis is an experimental, personal project and is not stable.

```acutis
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

[example]:
  https://acutis.johnridesa.bike/playground/?props=ewogICJzaXRlVGl0bGUiOiAiTXkgQmxvZyIsCiAgImJsb2dQb3N0cyI6IFsKICAgIHsKICAgICAgInRpdGxlIjogIk15IHNlY29uZCBwb3N0IiwKICAgICAgImF1dGhvciI6IHsgIm5hbWUiOiAiSm9obiIgfSwKICAgICAgImRhdGUiOiAiMjAyMC0xMi0wMSIsCiAgICAgICJpbWFnZSI6IG51bGwsCiAgICAgICJjb250ZW50IjogIjxwPlRoaXMgaXMgbXkgc2Vjb25kIHBvc3Q8L3A%2BIgogICAgfSwKICAgIHsKICAgICAgInRpdGxlIjogIkhlbGxvLCB3b3JsZCEiLAogICAgICAiYXV0aG9yIjogeyAibmFtZSI6ICJKb2huIiB9LAogICAgICAiZGF0ZSI6ICIyMDIwLTExLTMwIiwKICAgICAgImltYWdlIjogbnVsbCwKICAgICAgImNvbnRlbnQiOiAiPHA%2BVGhpcyBpcyBteSBmaXJzdCBwb3N0PC9wPiIKICAgIH0KICBdCn0%3D&source=PGgxPiBCbG9nIHBvc3RzIGZvciB7JSBzaXRlVGl0bGUgJX0gPC9oMT4KeyV%2BIG1hcCBibG9nUG9zdHMgd2l0aCB7aW1hZ2UsIHRpdGxlLCBjb250ZW50LCBkYXRlLCBhdXRob3I6IHtuYW1lfX0gJX0KICA8YXJ0aWNsZSBjbGFzcz0iaC1lbnRyeSI%2BCiAgICA8aGVhZGVyPgogICAgICB7JSBtYXRjaCBpbWFnZSB3aXRoIG51bGwgfiV9CiAgICAgICAgeyogbm8gaW1hZ2UgKn0KICAgICAgeyV%2BIHdpdGggIXtzcmMsIGFsdH0gfiV9CiAgICAgICAgPGltZyBzcmM9InslIHNyYyAlfSIgYWx0PSJ7JSBhbHQgJX0iPgogICAgICB7JSAvbWF0Y2ggfiV9CiAgICAgIDxoMiBjbGFzcz0icC1uYW1lIj4geyUgdGl0bGUgJX0gPC9oMj4KICAgICAgPHNwYW4gY2xhc3M9InAtYXV0aG9yIj4gQnkgeyUgbmFtZSA%2FICJBbm9ueW1vdXMiICV9IDwvc3Bhbj4KICAgICAgPHNwYW4gY2xhc3M9ImR0LXB1Ymxpc2hlZCI%2BIFBvc3RlZCBvbiB7JSBkYXRlICV9IDwvc3Bhbj4KICAgIDwvaGVhZGVyPgogICAgPGRpdiBjbGFzcz0iZS1jb250ZW50Ij4ge3slIGNvbnRlbnQgJX19IDwvZGl2PgogIDwvYXJ0aWNsZT4KeyV%2BIC9tYXAgJX0%3D

## Usage in OCaml

The Acutis source is written in OCaml. You can install it with the OCaml package
manager, OPAM.

```shell
opam pin acutis git+https://git.sr.ht/~johnridesabike/acutis
```

## JavaScript & Eleventy plugin

You can use npm to install a JavaScript build of Acutis. This includes an
[Eleventy] plugin.

[eleventy]: https://www.11ty.dev/

Install the JavaScript package in your project:

```shell
npm install acutis-lang
```

[Read more about its usage in the documentation](https://acutis.johnridesa.bike/introduction/).

## Development

To get started developing this project, clone its source code repository with
Git. Then create a development OPAM switch:

```shell
opam switch create . --deps-only --with-test --with-doc
```

Use Dune to compile the code:

```shell
dune build
```

Or use Dune to compile the code and also run the tests:

```shell
dune build @ci
```

Send patches to <~johnridesabike/public-inbox@lists.sr.ht>.

## License

Acutis was created and is maintained by
[John Jackson](https://johnridesa.bike/).

    Copyright (c) 2022 John Jackson.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at http://mozilla.org/MPL/2.0/.
