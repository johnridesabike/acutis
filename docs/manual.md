---
title: Acutis language manual
description: The manual for how to use the Acutis template language.
showTitle: true
layout: layout.acutis
next:
  text: Read the API 👉
  url: /api/
---

An Acutis template is parsed as three basic building blocks:

- Text, which is rendered as-is.
- Echoes, which are values wrapped in `{{` "mustaches" `}}`.
- Expressions, which are wrapped in `{%` and `%}`.

Most of the language features occur within `{%` expressions `%}`.

[[toc]]

## Bindings

Every template accepts a properties object (props) that binds values to
names. The object `{"color": "blue"}` binds `"blue"` to the name `color`.

If a template tries to access a binding that doesn't exist, then the value
defaults to `null`. (See [Pattern matching][1] and [Type][2] for more
information about this.)

[1]: #patterns%2C-matching%2C-and-mapping
[2]: #type

## Echoing values

If you apply the props from the last section to this template:

```jinja2
My favorite color is {{ color }}.
```

It renders:

```jinja2
My favorite color is blue.
```

### Nullish coalescing

The `?` (question mark) echoes the value on its right-hand side if the value
on its left-hand side is `null`.

```jinja2
My favorite color is {{ color ? fallbackColor }}.

You can chain ?s: {{ a ? b ? "If this prints, a and b are both null." }}
```

### Escaping

Acutis escapes echoes by default. It transforms the following characters into
HTML entities:

```jinja2
& " ' > < / ` =
```

If the `&` (ampersand) character appears before a value, then the value will
not be escaped.

```jinja2
My favorite color is {{ &color }}.
```

## Comments

```jinja2
{* TODO: add more colors. *}
```

Anything wrapped `{*` and `*}` is always ignored in the output. It is
possible to nest comments, similar to in ML-style languages.

```jinja2
{* None of {* this is *} rendered. *}
```

## Whitespace control

The `~` (tilde) symbol trims whitespace before or after an expression or an
echo statement.

```jinja2
<p>
  {{~ color ~}}
</p>
```

Renders:

```jinja2
<p>Blue</p>
```

## Pattern matching

The `match` and `map` statements are the core of Acutis' superpowers. They
use pattern matching to conditionally output sections of a template.

Pattern matching in Acutis combines object destructuring with equality
checking. If you're used to destructuring objects and using the `switch`
statement in JavaScript, then this may seem like a natural progression from
that.

### Example

Consider this pattern:

```reason
{published: true, title, dates: {posted, updated}}
```

This matches any object where `published` equals `true`, which contains a
`title` field, and which contains a `dates` object with `posted` and
`updated` fields. Additionally, it *binds* the values of `title`, `posted`,
and `updated` to those names.

Therefore, we can use these with the `match` statement and the `with` clause:

```jinja2
{% match article
   with {published: true, title, dates: {posted, updated}} %}
  {{ title }} was posted on {{ posted }} and updated on {{ updated }}.
{% with {published: false} %}
  {* Don't render unpublished articles. *}
{% /match %}
```

The `match`...`with` block is analogous to a `switch`...`case` block in some
languages. Here, the structure and values of the `article` binding is checked
against the patterns after each `with` clause. If one of the patterns
*matches* the contents of `article`, then the following template section is
rendered.

## Multiple patterns for a block

Acutis allows multiple `with` patterns to render single block expression.

```jinja2
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
the original, but *shadows* it. Bindings are also scoped to their blocks.

Consider this object about my favorite colors:

```json
{"color": "blue", "other": {"color": "green"}}
```

And this template:

```jinja2
My favorite is {{ color }}.

{% match other with {color} ~%}
  Another is {{ color }}.
{%~ /match %}

But my favorite is still {{ color }}.
```

Which renders:

```jinja2
My favorite is blue.

Another is green.

But my favorite is still blue.
```

The top-level `color` is not affected by the nested `color` binding.

## Mapping

The `map` statement is similar to `match` except that it is used on arrays
and objects to render each value of the array or object.

The value types must be homogenous (all the same type), or else a type error
is likely.

### Example

These props:

```json
{
  "articles": [
    {"title": "Acutis templates for beginners", "author": "John"}, 
    {"title": "Level up your Acutis skills", "author": "Carlo"}
  ]
}
```

And this template:

```jinja2
{% map articles with {title, author} ~%}
  The article "{{ title }}" was written by {{ author }}.
{% /map %}
```

Renders:

```html
The article "Acutis templates for beginners" was written by John.
The article "Level up your Acutis skills" was written by Carlo.
```

Just like `match`, `map` can take multiple patterns to conditionally render
different template sections based on an value's content.

```jinja2
{% map articles
   with {title, author: null} %}
  The article "{{ title }}" was written anonymously.
{% with {title, author} %}
  The article "{{ title }}" was written by {{ author }}.
{% /map %}
```

### Mapping static patterns

You can map static array patterns.

```jinja2
{% map ["Carlo", "John"] with name ~%}
  Hello, {{ name }}.
{% /map %}
```

You can also concatenate a static array pattern with an array binding by
using the `...` (spread) syntax.

```jinja2
{% map ["Carlo", "John", ...others] with name ~%}
  Hello, {{ name }}.
{% /map %}
```

### Matching the item index

You can optionally include an item's index in the pattern.
*For arrays, indices always begin at zero.*

```jinja2
{% map articles with {title, author}, index %}
  {{ index }}. {{ title }} was written by {{ author }}.
{% /map %}
```

Because the `index` binding is just another pattern, you can also use it to
conditionally render sections by matching the index with specific numbers.

```jinja2
{% map articles
   with {title}, 0 %}
  Our first article is {{ title }}.
{% with {title} %}
  {{ title }}
{% /map %}
```

### Mapping objects

You can `map` an object binding or pattern. Each of the object's values will
be matched with the pattern after the `with` clause. The index will be the
key associated with the value.

```jinja2
{% map {
    author: {name: "John"},
    editor: {name: "Carlo"}
  } with {name}, role ~%}
  {{ name }} is the {{ role }}.
{% /map %}
```

## Pattern matching in-depth

Pattern matching combines many concepts into one terse syntax. In other
languages, you may use variable assignment, `if`...`else` statements, and
imperative loops to control how your content is rendered. To help you
maximize your use of patterns, this section explains the system's nuances.

### Reference table

| Pattern               | Matches                                              |
|-----------------------|------------------------------------------------------|
| `x`                   | A binding. It matches any value, and the value is bound to `x`. |
| `true`                | Exactly `true`.                                      |
| `false`               | Exactly `false`.                                     |
| `null`                | Exactly `null`.                                      |
| `"abc"`               | The exact string `"abc"`.                            |
| `1.5`                 | The exact number `1.5`.                              |
| `1.5e1`               | The exact number `15`.                               |
| `[]`                  | An array with *exactly zero* items.                  |
| `["a", "b"]`          | An array that contains strings `"a"` and `"b"` as its first two items, receptively. *There may be more items.* |
| `["a", "b", ...rest]` | An array that contains strings `"a"` and `"b"` as its first two items, receptively. The remainder of the array is bound to `rest`. |
| `{}`                  | An object with *exactly zero* fields.                |
| `{a: 1.5}`            | An object with a field `"a"` containing number `1.5`. *There may be more fields.* |
| `{a}`                 | An object with a field `"a"` whose value is then bound to name `a`. *There may be more fields.* |
| `{a: x}`              | An object with a field `"a"` whose value is then bound to name `x`. *There may be more fields.* |
| `{"null": a}`         | An object with a field `"null"` whose value is then bound to name `a`. (`null` itself is a reserved keyword). *There may be more fields.* |

Notably, Acutis does not check the size of objects and arrays except for
empty objects (`{}`) and empty arrays (`[]`).

### Ignoring values with `_` (underscore)

Bindings will match any value, so they can be used as "catch-all" patterns.
However, Acutis will not let you reuse a binding in a pattern, so a pattern
like `{a: x, b: x}` is illegal.

Acutis treats the `_` (underscore) name as a special case. Values bound to it
are immediately discarded. Therefore `{a: _, b: _}` will always match any
object with fields `a` and `b`, but it will ignore their contents.

Because bindings match *anything*, you can use `_` as a "default" case:

```jinja2
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

```jinja2
{% match object, color
   with "sky", "blue"
   with "grass", "green" %}
  That's a common color.
{% with _, _ %}
  That's an unusual color.
{% /match %}
```

The number of patterns must match the number of values. This is illegal:

```jinja2
{% match object, color with "sky" %}
  I forgot to match the color!
{% /match %}
```

### The rules of pattern matching

- When matching a value, at least one of the patterns *must* match. If no match
  is found, then Acutis raises an error. (You can avoid this with a catch-all
  pattern.)
- Patterns are checked in the order they're written. It's usually best to put
  more specific patterns before less specific patterns, with catch-all
  patterns at the end.
- When binding values, a binding name cannot be used more than once per
  pattern. The only exception is `_`.
- Each `with` clause must have the same number of patterns as the number of
  values being matched.
- Patterns can only contain concrete values. The following code doesn't work
  as you may expect: `{% match a with b %}...`. It doesn't compare the value
  of `a` with value of `b`, but rather binds the value of `a` to a new `b`
  binding.

## Type

Acutis uses strong typing, where every value can only be of one type. Acutis
also uses dynamic typing, where types are checked at runtime, not compile
time.

Type definitions types are borrowed from [JSON]:

[JSON]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON 

- Object
- Array
- String
- Number
- Boolean
- Null

When a value is in a pattern, the matched value must be the same type. The
only exception to this rule that Acutis assumes all values are *nullable*, so
`null` can be compared with any value without a type error.

Only `string` and `number` values can be echoed. `null` cannot be echoed.

Acutis is suited for environments where data comes from a statically typed
source. Union types, such as `boolean | number`, are not supported. If you
must use data like that, we recommend you normalize the data before passing
it to templates. However, because type checking happens at runtime, it is
sometimes possible write patterns that cleverly guard against potential
errors. This usually isn't best practice, though.

## Template components

Template components in Acutis are analogous to "partials" or "includes" in
other template languages. They are how we reuse common pieces of templates.
We can also write them in JavaScript to add custom runtime logic, which makes
them also comparable to "filters" or "shortcodes" in other languages.

Components always begin with a capital letter. They accept XML-style props
which are turned into bindings within the component. They also end with
XML-style `/` (backslash). If you've used React JSX before, these concepts
will look similar.

Components are global. Once they are loaded into the render context, any
template in the render tree can access them. (See [`renderContext`] in the API
manual for more information.)

[`renderContext`]: ../api/#rendercontext

A couple of basic components:

File: `Byline.acutis`

```jinja2
Written by {{ name }}.
```

File: `Articles.acutis`

```jinja2
{% map articles with {title, author} %}
  {{ title }} {% Byline name=author / %}
{% /map %}
```

### Patterns in props

Patterns can be used in props. They compile into the values they would match
in pattern matching.

```jinja2
{% Article
   published=true
   class="news"
   authors=[author, editor]
   / %}
```

⚠️ Unbound names in these patterns will not default to `null`. They will raise
an error.

### Prop punning

Props can be "punned." You can take code such as `{% DateTime date=date / %}`
and abbreviate it to `{% DateTime date / %}`.

### Template children props

Props can be template sections, which are considered the template's
"children." These are denoted with octothorpes, beginning with `#` and ending
with `/#`. Children's names must begin with a capital letter, just like
components.

```jinja2
{% Layout
   Header=#%}
    <h1> {{ title }} </h1>
   {%/#
   Sidebar=#%}
    <h2> Menu </h2>
    {% map menu with {title, slug} %}
      <a href="{{ slug }}">{{ title }}</a>
    {% /map %}
   {%/#
   / %}
```

Inside the template, you can echo these children the same way you echo other
bindings, such as `{{ Header }}` or `{{ Sidebar }}`. Their contents are
rendered just like any template content, so they will not be escaped.

Template children may look like regular bindings, but they live on a separate
layer of the language. We can't use them with `map` or `match`, and we can't
put them inside patterns. We can pass them to other components' children
props.

### Default children prop

A template section inside a component is automatically bound to a children
prop named `Children`.

An implicit children prop:

```jinja2
{% Layout %}
  content
{% /Layout %}
```

An explicit children prop:

```jinja2
{% Layout 
   Children=#%}
    content
   {%/#
   / %}
```

Both of those examples are rendered identically. If you've used React, this
should look familiar.