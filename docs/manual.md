---
title: Acutis language manual
description: The manual for how to use the Acutis template language.
showTitle: true
layout: main.acutis
next: null
---

An Acutis template is parsed as three basic building blocks:

- **Text**, which is rendered as-is.
- **Echoes**, which are values wrapped in `{{` "mustaches" `}}`.
- **Expressions**, which are wrapped in `{%` and `%}`.

Most of the language features occur within `{%` expressions `%}`.

[[toc]]

## Props and bindings

Every template accepts a map of properties (props) that binds values to names.

In this document, we will use JSON data to represent our input type, but the
Acutis rendering engine is flexible enough to allow other types of input.

As an example, the JSON object `{"color": "blue"}` binds the string `"blue"` to
the name `color`.

Bindings in Acutis are what some languages call "variables." The term _variable_
is not completely accurate in Acutis because bindings are immutable and
therefore cannot _vary_. Nonetheless, you may still see the two terms used
interchangeably.

## Echoing values

If you apply the data from the last section to this template:

```acutis
My favorite color is {{ color }}.
```

It renders:

```acutis
My favorite color is blue.
```

### Nullish coalescing

The `?` (question mark) echoes the value on its right-hand side if the value on
its left-hand side is `null`.

```acutis
My favorite color is {{ color ? fallbackColor }}.

You can chain ?s: {{ a ? b ? "If this prints, a and b are both null." }}
```

### Escaping

Acutis escapes echoes by default. It transforms the following characters into
HTML entities:

```acutis
& " ' > < / ` =
```

If the `&` (ampersand) character appears before a value, then it will not escape
the value.

```acutis
My favorite color is {{ &color }}.
```

## Comments

```acutis
{* TODO: add more colors. *}
```

Anything wrapped `{*` and `*}` is always ignored in the output. It is possible
to nest comments, similar to ML-style languages.

## Whitespace control

The `~` (tilde) symbol trims whitespace before or after an expression or an echo
statement.

```acutis
<p>
  {{~ color ~}}
</p>
```

Renders:

```acutis
<p>Blue</p>
```

## Types and interfaces

Acutis is statically typed, so types are checked at compile time and erased at
runtime. The type scheme is mostly based on JSON with some additional features.

Acutis does not need annotations. The compiler can infer the types 100%
correctly based on how you use the data in your templates. This is a
double-edged sword, though. It means your code is more concise, but, if you
write a template incorrectly, the compiler will infer the types in ways you may
not expect. You may optionally add an interface to each template which
explicitly declares its types.

### Interfaces

An interface contains a sequence of prop names and their respective types,
following the format `prop = type`. Each interface begins with `{% interface`
and ends with `/ %}`.

Examples:

```acutis
{% interface
  page = {title: string, url: string}
  visible = false | true
/ %}
```

Interfaces are optional, and they exist mainly for the benefit of us humans who
may have difficulty inferring types as well as our computers do. They can make
large templates easier to read and understand.

Once you add an interface to a template, then that interface must include _all_
of the template's props and their types. If the compiler finds a prop in your
template that is not listed in the interface, then it will raise an error.

Interfaces may exist anywhere inside a template. Their location does not affect
how the compiler parses them. You may even divide an interface across multiple
`{% interface / %}` blocks.

### Constants: int, float, and string

Examples:

```acutis
{% interface
  a = int
  b = float
  c = string
/ %}
```

```txt
1
1.5
"abc"
```

The three "constant" types are `int`, `float`, and `string`, which work as you'd
expect from other languages.

### Nullable

Examples:

```acutis
{% interface
  a = ?string
/ %}
```

```txt
null
!"This isn't null."
```

Any type may be "wrapped" in a nullable. Nullable types are indicated by a `?`
preceding the wrapped type, e.g. `?int`. They can either be `null` or not-null,
which is written in patterns with a `!`.

Because of the way Acutis "wraps" nullable values, they're closer to what other
languages call "option" types.

### List

Examples:

```acutis
{% interface
  a = [string]
/ %}
```

```txt
[]
["a", "b"]
[1, 2, 3, ...rest]
```

A list is an ordered sequence of values. Lists are homogeneous, so each item
must be of the same type. They are indicated with brackets, e.g. `[int]`.

You can append or destructure items from the "head," or the front, of a list
with the `...` syntax: `[head, ...tail]`.

### Tuple

Examples:

```acutis
{% interface
  a = (int, string, ?float)
/ %}
```

```txt
(12, "abc", null)
```

A tuple is an ordered sequence of items. They're heterogeneous, so each item may
be a different type than its neighbor. They are indicated with parentheses, e.g.
`(int, string, ?float)`.

Unlike lists, which are dynamically sized, each tuple has a fixed size defined
by its type.

### Record

Examples:

```acutis
{% interface
  a = {a: int, b: string}
/ %}
```

```txt
{a: 12, b: "xyz"}
```

A record is a series of key-value pairs. Records are indicated by braces, e.g.
`{a: int, b: string}`.

Records are extensible. The compiler assumes that every record may contain
additional fields which have not been specified yet.

### Dictionary

Examples:

```acutis
{% interface
  a = <int>
/ %}
```

```txt
<a: 12, b: 101>
```

Dictionaries, or "dicts," are to records what lists are to tuples. They
represent key-value pairs like records, but they are dynamically sized and
homogeneously typed. They are specified with angled brackets, e.g. `<int>`.

The order of keys in a dict is not defined. The compiler currently sorts them
alphabetically.

### Enumeration

Examples:

```acutis
{% interface
  a = @"abc" | @"def"
  b = @12 | @34 | ...
/ %}
```

```txt
@"abc"
@12
```

An enumeration, or an "enum," is a set of integers or strings. Each enum value
is prefixed with an `@`, e.g. `@"a" | @"b" | @"c"`.

Enums can be "open" or "closed." A closed enum only allows its specified values.
An open enum allows the possibility of adding new values. "Openness" is
indicated with ellipses, e.g. `@"a" | @"b" | ...`.

### Boolean

Examples:

```acutis
{% interface
  a = false | true
/ %}
```

```txt
false
true
```

Boolean values are really just a special kind of enum. The `false | true` type
works exactly like any closed binary enum would, such as `@0 | @1`.

It is possible to declare a type which is _only_ `false` or _only_ `true`, but
this is not usually useful.

### Tagged union

Examples:

```acutis
{% interface
  a =
      {@shape: "circle", radius: int}
    | {@shape: "rectagle", height: int, width: int}
/ %}
```

```txt
{@shape: "circle", radius: 12}
{@shape: "rectangle", height: 11, width: 7}
```

A record may have a "tag" field, indicated by an `@`, which allows it to unify
with other records. This is like a combination of a record and enum type, since
the tag field works like an enum.

Unions may be "open" or "closed," similar to enums.

Tag fields may only contain literal integer, string, or boolean values.

### Echoable

```acutis
{% interface
  a = echoable
/ %}
```

Acutis can echo any int, string, float, or enum value. If it can't determine
which of these types a particular echoed value is, then it uses a catch-all
`echoable` type.

### Unknown

```acutis
{% interface
  a = _
/ %}
```

Finally, if Acutis can't determine anything at all about a value's type, then it
uses "unknown," which is indicated by an underscore: `_`.

## Typing philosophy

Acutis is _strongly_ and _statically_ typed. The compiler guarantees that values
are coherent. For example, a `string` will never appear where an `int` is
expected.

Acutis is also _structurally_ typed. This allows the compiler to do more than
just check type equality. It can check for subtypes, which sees if a particular
structure is compatible as a subset of another structure.

The goal is to enable real-world data to easily fit into a system that's 100%
type-safe. This means you must sometimes make decisions about which types best
fit your data. For example, records and tuples are heterogeneous and
fixed-sized, while dicts and lists are homogeneous and dynamically-sized. This
trade-off is necessary because a structure that is heterogeneous and dynamic
would be unsafe.

## Types and JSON

The Acutis rendering engine is generic enough to allow different input types.
However, it currently only includes decoders for JSON and JavaScript. Both are
very similar, so we'll use JSON as our primary example.

Acutis accepts incoming data and decodes it according to the template's type
scheme. Here are examples of how Acutis types correspond to JSON types and
values:

<!-- Adding bars inside inside tables messes with markdown tooling. -->

| Acutis type                                         | JSON type or value                               |
| --------------------------------------------------- | ------------------------------------------------ |
| `int`                                               | `number`                                         |
| `float`                                             | `number`                                         |
| `string`                                            | `string`                                         |
| <code>false &verbar; true</code>                    | `boolean`                                        |
| `?string`                                           | `null` or `string`                               |
| `[string]`                                          | `array` with `string` values                     |
| `(int, string, float)`                              | `array` with shape `[number, string, number]`    |
| `{a: int, b: string}`                               | `object` with shape `{"a": number, "b": string}` |
| `<string>`                                          | `object` with `string` values                    |
| <code>@&quot;a&quot; &verbar; @&quot;b&quot;</code> | Values `"a"` or `"b"`                            |
| <code>@1 &verbar; @2</code>                         | Values `1` or `2`                                |
| `{@tag: "a", b: int}`                               | `object` with shape `{"tag": "a", "b": number}`  |
| `echoable`                                          | `number` or `string` or `boolean`                |
| `_` (unknown)                                       | Any                                              |

You may need to preprocess your input data to fit Acutis' type constraints. For
example, if you have a value can be one of several incompatible types, then you
will have to transform it into a legal type such as a tagged union.

## Introduction to pattern matching

The `match` and `map` statements are the core of Acutis' powers. They use
pattern matching to conditionally output sections of a template.

Pattern matching in Acutis combines object destructuring with equality checking.
If you're used to destructuring objects and using the `switch` statement in
JavaScript, then this may seem like a natural progression from that.

Consider this pattern:

```js
{published: true, title, dates: {posted, updated}}
```

This matches any object where `published` equals `true`, which contains a
`title` field, and which contains a `dates` object with `posted` and `updated`
fields. Additionally, it _binds_ the values of `title`, `posted`, and `updated`
to those names.

Therefore, we can use these with the `match` statement and the `with` clause:

```acutis
{% match article
   with {published: true, title, dates: {posted, updated}} %}
  {{ title }} was posted on {{ posted }} and updated on {{ updated }}.
{% with {published: false} %}
  {* Don't render unpublished articles. *}
{% /match %}
```

The `match`...`with` block is analogous to a `switch`...`case` block in some
languages. Here, the structure and values of the `article` binding is checked
against the patterns after each `with` clause. If one of the patterns _matches_
the contents of `article`, then the following template section is rendered.

## Multiple patterns for a block

Acutis allows multiple `with` patterns to render single block expression.

```acutis
{% match greeting
   with "Hello"
   with "Hola"
   with "Konnichiwa" %}
  I can speak this language.
{% with unknown %}
  I don't know what "{{ unknown }}" means.
{% /match %}
```

## Shadowing bindings

Bindings are immutable. Binding a value to an existing name does not override
the original, but _shadows_ it. Bindings are also scoped to their blocks.

Consider this JSON object:

```json
{ "color": "blue", "other": { "color": "green" } }
```

And this template:

```acutis
My favorite is {{ color }}.

{% match other with {color} ~%}
  Another is {{ color }}.
{%~ /match %}

But my favorite is still {{ color }}.
```

Which renders:

```acutis
My favorite is blue.

Another is green.

But my favorite is still blue.
```

The top-level `color` is not affected by the nested `color` binding.

## Ignoring bindings

The compiler will raise an error if you declare a binding but never use it. You
can suppress this warning either by prefixing the name with an `_` (underscore)
or by replacing the entire name with `_`.

```acutis
{% match list with [head, ...tail] %}
  The list's head is {{ head }}.
  This will fail to compile. We forgot to use 'tail'.
{% with [] %} The list is empty.
{% /match %}
```

```acutis
{% match list with [head, ..._tail] %}
  The list's head is {{ head }}.
  This will compile without an error.
{% with [] %} The list is empty.
{% /match %}
```

## Exhaustive and partial patterns

The Acutis compiler analyzes patterns to determine whether they are exhaustive
or partial. An exhaustive set of patterns covers every possible shape of input
data.

It's easy for a programmer to accidentally write a partial set of patterns,
especially when matching a complex data structure. Fortunately, the compiler can
handle nested patterns easily. Here's an example:

```acutis
{% match author with {name, books: [{title}]} %}
  {{ name }}'s latest books is {{ title }}.
{% with {name, books: []} %}
  {{ name }} hasn't published any books yet.
{% /match %}
```

This fails with the error message:

```txt
Matching error.
This pattern-matching is not exhaustive. Here's an example of a pattern which
is not matched:
{books: [{title: _}, ..._], name: _}
```

The example pattern it generated should tell you what you missed. In this case,
the original set of patterns only matched a `books` list with one or zero items,
but not more than one (thus the `..._]` in the error's example).

## Mapping

The `map` statement is similar to `match` except that it is used on lists to
render each value of the list.

These props:

```json
{
  "articles": [
    { "title": "Acutis templates for beginners", "author": "John" },
    { "title": "Level up your Acutis skills", "author": "Carlo" }
  ]
}
```

And this template:

```acutis
{% map articles with {title, author} ~%}
  The article "{{ title }}" was written by {{ author }}.
{% /map %}
```

Will render:

```txt
The article "Acutis templates for beginners" was written by John.
The article "Level up your Acutis skills" was written by Carlo.
```

Just like `match`, `map` can take multiple patterns to conditionally render
different template sections based on an value's content.

```acutis
{% map articles
   with {title, author: null} %}
  The article "{{ title }}" was written anonymously.
{% with {title, author} %}
  The article "{{ title }}" was written by {{ author }}.
{% /map %}
```

### Mapping static patterns

You can map static list patterns.

```acutis
{% map ["Carlo", "John"] with name ~%}
  Hello, {{ name }}.
{% /map %}
```

You can also concatenate a static list pattern with an list binding by using the
`...` (spread) syntax.

```acutis
{% map ["Carlo", "John", ...others] with name ~%}
  Hello, {{ name }}.
{% /map %}
```

### Matching the item index

You can optionally include an item's index in the pattern. _For lists, indices
always begin at zero._

```acutis
{% map articles with {title, author}, index %}
  {{ index }}. {{ title }} was written by {{ author }}.
{% /map %}
```

Because the `index` binding is just another pattern, you can also use it to
conditionally render sections by matching the index with specific numbers.

```acutis
{% map articles
   with {title}, 0 %}
  Our first article is {{ title }}.
{% with {title} %}
  {{ title }}
{% /map %}
```

### Mapping dictionaries

You can map dictionaries by using `map_dict`.

Each of the dictionary's values will be matched with the pattern after the
`with` clause. The index will be the string key associated with the value.

```acutis
{% map_dict
    < author: {name: "John"},
      editor: {name: "Carlo"} >
    with {name}, role ~%}
  {{ name }} is the {{ role }}.
{% /map_dict %}
```

## More about pattern matching

Pattern matching combines many concepts into one terse syntax. In other
languages, you may use variable assignment, `if`...`else` statements, and
imperative loops to control how your content is rendered. To help you maximize
your use of patterns, this section explains the system's nuances.

Patterns work by comparing literal values. The following code may not work as
you might expect: `{% match a with b %}`. It does not compare the value of `a`
with value of `b`, but rather binds the value of `a` to a new `b` binding.

### Reference table

| Pattern                      | Matches                                                                                                            |
| ---------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| `_`                          | Any value.                                                                                                         |
| `x`                          | Any value, and the value is bound to `x`.                                                                          |
| `true`                       | Exactly `true`.                                                                                                    |
| `false`                      | Exactly `false`.                                                                                                   |
| `"abc"`                      | The exact string `"abc"`.                                                                                          |
| `1.5`                        | The exact float `1.5`.                                                                                             |
| `7`                          | The exact integer `7`.                                                                                             |
| `null`                       | A nullable value which is `null`.                                                                                  |
| `!x`, `!1.5`, `!"abc"`, etc. | A nullable value which is not `null`.                                                                              |
| `@7`                         | The exact integer `7`, when `7` is part of an enum.                                                                |
| `@"a"`                       | The exact string `"a"`, when `"a"` is part of an enum.                                                             |
| `[]`                         | A list with _exactly zero_ items.                                                                                  |
| `["a", "b"]`                 | A list with _exactly two_ items, `"a"` and `"b"` receptively.                                                      |
| `["a", "b", ...rest]`        | A list with _at least two_ items, `"a"` and `"b"`. The remainder of the list is bound to `rest`.                   |
| `{a: 1}`                     | A record with field `"a"` containing integer `1`.                                                                  |
| `{a}`                        | A record with field `"a"` whose value is then bound to name `a`.                                                   |
| `{a: x}`                     | A record with field `"a"` whose value is then bound to name `x`.                                                   |
| `{"null": a}`                | A record with field `"null"` whose value is then bound to name `a`. (`null` without quotes is a reserved keyword.) |
| `(7, "a")`                   | A 2-tuple whose first value is `7` and whose second value is `"a"`                                                 |
| `{@tag: "a", b: 7}`          | A tagged record with a tag field `"tag"` whose value is `"a"` and with field `"b"` whose value is 7.               |
| `<a: 2>`                     | A dictionary with key `"a"` containing integer `2`.                                                                |

### Ignoring values with `_` (underscore)

Bindings will match any value, so you can use them as "catch-all" patterns.
However, Acutis will not let you reuse a binding in a pattern, so a pattern like
`{a: x, b: x}` is illegal.

Acutis treats the `_` (underscore) name as a special. Values bound to it are
immediately discarded. Therefore `{a: _, b: _}` will always match any object
with fields `a` and `b`, but it will ignore their contents.

Because bindings match _anything_, you can use `_` as a "default" case:

```acutis
{% match greeting
   with "Hello" %}
  This is an English greeting.
{% with "Hola" %}
  This is a Spanish greeting.
{% with _ %}
  This is some other kind of greeting.
{% /match %}
```

### Matching multiple values at once

You can match many values at once by separating them with a comma (`,`). This
can be useful for reasoning about two-dimensional matrices of data.

```acutis
{% match object, color
   with "sky", "blue"
   with "grass", "green" %}
  That's a common color.
{% with _, _ %}
  That's an unusual color.
{% /match %}
```

## Template components

Template components in Acutis are analogous to "partials" or "includes" in other
template languages. They are how we reuse common pieces of templates.

We can also use functions as templates to add custom runtime logic, which makes
them comparable to "filters" or "shortcodes" in other languages.

Components always begin with a capital letter. They accept XML-style props which
are turned into bindings within the component. They also end with an XML-style
`/` (backslash).

A couple of basic components:

File: `Byline.acutis`

```acutis
Written by {{ name }}.
```

File: `Articles.acutis`

```acutis
{% map articles with {title, author} %}
  {{ title }} {% Byline name=author / %}
{% /map %}
```

### Patterns in props

Patterns can be used in props. They compile into the values they would match in
pattern matching.

```acutis
{% Article
   published=true
   class="news"
   authors=[author, editor]
   / %}
```

### Prop punning

Props can be "punned." You can take code such as `{% DateTime date=date / %}`
and abbreviate it to `{% DateTime date / %}`.

### Template children props

Props can be template sections, which are considered the template's "children."
These are denoted with hashes, beginning with `#%}` and ending with `{%/#`.
Children names must begin with a capital letter, just like components.

```acutis
{% Layout
   Header=#%} <h1> {{ title }} </h1> {%/#
   Sidebar=#%}
    <h2> Menu </h2>
    {% map menu with {title, slug} %}
      <a href="{{ slug }}">{{ title }}</a>
    {% /map %}
   {%/#
   / %}
```

Inside the template, you can echo these children the same way you echo other
bindings, such as `{{ Header }}` or `{{ Sidebar }}`. Their contents are rendered
just like any template content, so they will not be escaped.

Template children may look like regular bindings, but they live on a separate
layer of the language. We can't use them with `map` or `match`, and we can't put
them inside patterns. We can pass them to other components' children props.

### Default children prop

A template section inside a component is automatically bound to a children prop
named `Children`.

An implicit children prop:

```acutis
{% Layout %} content {% /Layout %}
```

An explicit children prop:

```acutis
{% Layout Children=#%} content {%/# / %}
```

Both of those examples are rendered identically.

### Children in interfaces

If a template has an interface, then its children must be declared in it. Unlike
regular props, children have only two types: nullable and not nullable.
Non-nullable children only need their names listed. Nullable children are
labeled as such with `= ?`.

Examples:

```acutis
{% interface
  Child
  NullableChild = ?
/ %}
```
