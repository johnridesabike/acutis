---
title: Acutis API for JavaScript & ReScript
description: |
  The manual for the application programming interface for the Acutis template
  language.
showTitle: true
layout: main.acutis
next: null
---

An Acutis template is rendered using _template source(s)_, a _JSON object_, and
an _environment module_. The Acutis runtime includes a synchronous and a
JavaScript-Promise-based asynchronous environment, although other environments
could potentially be added.

⚠️ Warning: the API is experimental and likely to change. ⚠️

All of this page's examples are written in ReScript.

[[toc]]

## Example code

```reason
open AcutisLang
let components = Compile.Components.make([
  Source.src(~name="Footer", "Copyright 2022.")
])
let props = Js.Dict.fromArray([("name", "Carlo")])
Compile.make(~name="Main", "Hello {{ name }}.", Result.getExn(components))
  ->Result.flatMap(template => Render.sync(template, props))
  ->Result.getOrElse(errors => {
    failwith("I couldn't render this template.")
    Js.Console.error(errors)
  })
  ->Js.log // Logs: "Hello Carlo."
```

## Compiling

Acutis uses two compile functions. One is for the top-level "root" template, and
one is for the template components.

### `Compile.make`

The function `Compile.make` compiles a root source into a [result] of a
template.

```reason
open AcutisLang
let templateResult =
  Compile.make(~name="Main", "Hello {{ name }}", Compile.Components.empty())
```

The function `Compile.makeExn` is identical except that it raises an exception
if the compilation fails, instead of returning a `result`.

### `Compile.Components.make`

The function `Compile.Components.make` accepts an array of [sources] and returns
a [result] of their compiled output.

```reason
open AcutisLang
let header = Source.src(~name="Header", "<h1>Hello</h1>");
let footer = Source.src(~name="Footer", "Copyright 2022 Carlo.");
let componentsResult = Compile.Components.make([header, footer])
```

The function `Compile.Components.makeExn` is identical except that it raises an
exception if the compilation fails instead of returning a `result`.

### `Compile.Components.empty`

If you aren't using components, or you need a placeholder, then the function
`Compile.Components.empty` will return an empty components value.

## Type schemes

The compiler infers type schemes for normal Acutis templates. But when you
define a function to be used as a template, then you need to define its type
scheme.

The functions for defining a type scheme are found in the `Typescheme` module.
Type `ty` represents an individual type (e.g. `int`), and `t` represents the
entire type scheme.

```reason
type ty
type t = Belt.Map.String.t<ty>
let unknown: unit => ty
let int: unit => ty
let int_: unit => ty // int is a JS keyword
let float: unit => ty
let float_: unit => ty // float is a JS keyword
let string: unit => ty
let echo: unit => ty
let nullable: ty => ty
let list: ty => ty
let tuple: array<ty> => ty
let record: array<(string, ty)> => ty
let dict: ty => ty
let enum_int: array<int> => ty
let enum_string: array<string> => ty
let bool: unit => ty
let union_int: (string, array<(int, array<(string, ty)>)>) => ty
let union_string: (string, array<(string, array<(string, ty)>)>) => ty
let union_boolean: (string, ~f: array<(string, ty)>, ~t: array<(string, ty)>) => ty
let make: array<(string, ty)> => t
```

You need to also define the type scheme for a template's children, which are
distinct from the props. These functions are found in the `Typescheme.Child`
module. The only meaningful types for children is whether they can be nullable
or not.

```reason
module Child: {
  type ty
  type t = Belt.Map.String.t<ty>
  let make: array<(string, ty)> => t
  let child: string => (string, ty)
  let nullable: string => (string, ty)
}
```

## Sources

The `Source` module's functions classify different types of sources to prepare
them for the compiler.

### `Source.src`

The `Source.src` function accepts the name of a template and the raw source code
in a string.

```reason
open AcutisLang
let src = Source.src(~name="Hello", "Hello {{ name }}")
```

### `Source.fn`

The `Source.fn` function accepts the name of the template, a type scheme, and a
function.

```reason
open AcutisLang
let src = Source.fn(
  ~name="Hello",
  Typescheme.make([
    ("name", Typescheme.string())
  ]),
  Typescheme.Child.make([]),
  (Env, props, children) =>
    Env.return(. "Hello " ++ Js.Dict.unsafeGet(props, "name") ++ ".")
)
```

## Render

The `Render` module handles all of the runtime code. `Render.make` is a
general-purpose function for any return type, but you will probably use one of
the specialized functions.

### `Render.sync` and `Render.async`

The `sync` and `async` functions are both predefined functions that render to
specialized types. `sync` returns a [result] of a string, and `async` returns a
JavaScript Promise of a result of a string.

```reason
open AcutisLang
// Assume template and props are already defined.
let result = Render.sync(template, props)
let promise_result = Render.async(template, props)
```

### A note on synchronous and asynchronous templates

If you have a template function that uses JavaScript `async` functions or
promises, then you need to render your whole template tree with an asynchronous
environment.

Templates components inherit their environment from their parent templates, so
synchronous templates can be used interchangeably between environment types.
This does not work the other way around, though. Asynchronous templates cannot
be made synchronous.

## Results

Acutis handles errors with a "result" data type. This is represented in
JavaScript as an object and in ReScript as a [polymorphic variant]. It will
either contain a function's successful output, or it will contain an array of
errors.

You can either access these values manually, or use the included utility
functions.

### JavaScript: `{ NAME, VAL }`

Acutis returns data inside an object with `NAME` and `VAL` properties. If any
errors occurred, then `NAME` will be `"errors"` and `VAL` will be an array of
the errors. If no errors occurred, then `NAME` will be `"ok"` and `VAL` will be
the rendered output.

#### Example successful output

```json
{
  "NAME": "ok",
  "VAL": "The content of your output."
}
```

#### Example unsuccessful output

```json
{
  "NAME": "errors",
  "VAL": [
    {
      "message": "Error details goes here."
    }
  ]
}
```

### ReScript: `[#ok('a) | #errors(array<Debug.error>)]`

In ReScript, the output uses a polymorphic variant defined as
`[#ok('a) | #errors(array<Debug.error>)]`.

This is conceptually the same as the standard [ReScript result type]. We're
using a custom polymorphic variant because of its nicer JavaScript
representation.

### `Result.map`

The `Result.map` function accepts a result and a function that accepts its
contents and returns a new value. The given function only executes if the result
does not have errors.

```reason
// Assume props are already defined.
let result = Render.sync(template, props)
let resultALLCAPS = Result.map(result, Js.String.toUpperCase)
```

### `Result.flatMap`

The `Result.flatMap` function works almost identically to `Result.map`, except
that the given function must _return another `result`_.

```reason
// Assume src, components, props is defined already
let templateResult = Compile.make(~name="Main", src, components)
let outputResult = Result.flatMap(
  templateResult,
  (template) => Render.sync(template, props)
)
```

### `Result.getExn`

The `Result.getExn` function returns the value of the result if there are no
errors, and raises an exception if there are errors.

The exception raised is a ReScript exception, which doesn't always play nicely
with JavaScript tooling. Often, you'll want to use the more flexible
`Result.getOrElse` function instead.

```reason
// Raises an exception if the result has errors.
let output = Result.getExn(result)
```

### `Result.getOrElse`

The `Result.getOrElse` function returns the value of the result if there are no
errors, and executes a given function if there are errors.

This can be useful for either getting a "default" value, or doing something with
the errors (such as logging them to the console).

#### ReScript example

```reason
let output = Result.getOrElse(
  result,
  (errors) => {
    Js.Console.error(errors)
    "Some default value."
  }
)
```

## Error messages

Here's an example error message in JSON:

```json
{
  "kind": "Type",
  "location": {
    "name": "Main",
    "character": 11
  },
  "message": "This pattern is type int but expected type string.",
  "path": []
}
```

Any of these fields may not be present if there isn't any data for them. If an
exception was thrown while rendering, then it will be caught and added to an
`exn` field.

## Template component functions

Suppose we want a `Footer` template that will always display the current year.
We can write a template to similar to the following.

```reason
open AcutisLang
let footer =
  Source.fn(
    ~name="Footer",
    Typescheme.make([]),
    Typescheme.Child.make([]),
    (type a, module(Env): Source.env<a>, props, children) => {
      let year = Js.Date.make()->Js.Date.getFullYear->Belt.Int.toString
      Env.return(. "Copyright " ++ year ++ ".")
    }
  )
```

## Environment

An environment is a ReScript module (or a JavaScript object) that contains
functions for returning rendered template contents.

### `return`

The `return` function returns a string as-is, wrapped inside the environment's
type.

In JavaScript, you can access it as `return_`.

```reason
open AcutisLang
let sayHi =
  Source.fn(
    ~name="SayHi",
    Typescheme.make([]),
    Typescheme.Child.make([]),
    (type a, module(Env): Source.env<a>, props, children) =>
      Env.return(. "Hello")
  )
```

### `error`

The `error` function returns an error with the given message. This will cause
the template to fail to render.

#### ReScript example

```reason
open AcutisLang
let fail =
  Source.fn(
    ~name="Fail",
    Typescheme.make([]),
    Typescheme.Child.make([]),
    (type a, module(Env): Source.env<a>, props, children) =>
      Env.error(. "This always fails.")
  )
```

### `map`

The `map` function takes a child prop and a function to transform the child's
contents. It returns a new child and does not mutate the original.

```reason
open AcutisLang
let shout =
  Source.fn(
    ~name="Shout",
    Typescheme.make([]),
    Typescheme.Child.make([
      Typescheme.Child.nullable("Children")
    ]),
    (type a, module(Env): Source.env<a>, props, children) =>
      switch Js.Dict.get(children, "Children") {
      | Some(children) => Env.map(. children, Js.String.toUpperCase)
      | None => env.return(. "")
      }
  )
```

### `flatMap`

The `flatMap` function works similarly to `map` except that it does not
automatically wrap the returned data. When using `flatMap`, you're responsible
for wrapping the output with `return` in your callback. This means that you can
choose to use `error` instead to represent failures.

## Node helpers API

Acutis includes functions for rendering templates in a Node.js environment, such
as for a static site generator. They can be imported from module
`acutis-lang/node-utils`.

### `loadTemplate`

Calling `loadTemplate(fileName)` will return a [Promise] of a template function.

If the filename ends with a `.js` or `.mjs` extension, then it will load it with
dynamic `import`. It uses the "default" export as the template source.

For all other file extension, it loads the file's text content as the template.

### `filenameToComponent`

Calling `filenameToComponent(fileName)` strips the extension and capitalizes the
first letter. Example: `filenameToComponent("footer.js")` returns `"Footer"`.
This is just a convenience to help components adhere to template naming rules.

## Acutis command line interface (CLI)

You can use Acutis from the command line. It accepts JSON data and an array of
template filenames, and it prints the rendered output.

```shell
acutis Index.acutis src/**/*.(js|mjs|acutis) < data.json > dist/index.html
```

Full options:

```txt
Usage:
  acutis [options] [template] [templates...]

Options:
  -a, --async                   Enable asynchronous templates. Default: false.
  -d, --data                    The path to the JSON data file. Default: stdin.
  -o, --output                  The path to write the output. Default: stdout.
  -t, --table                   Print errors in a table. Default: false.
  -h, --help                    Show this message and exit.
  -v, --version                 Show the version and exit.

Templates:
  The first template listed is the root template. Templates with .js, .mjs,
  or .cjs extensions are rendered using their default export. For all other
  file types, their text content is used as the template.

Examples:
  acutis --data=data.json --output=index.html Index.acutis Header.js Footer.acutis
  acutis Index.acutis _includes/**/*.(js|mjs|acutis) < data.json > index.html
  curl https://example.com/api | acutis Index.acutis *.(js|acutis) -o index.html
```

[polymorphic variant]:
  https://rescript-lang.org/docs/manual/latest/polymorphic-variant
[promise]:
  https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise
[rescript result type]:
  https://rescript-lang.org/docs/manual/latest/api/belt/result
[result]: #results
[sources]: #sources
