---
title: Acutis API for JavaScript & ReScript
description: |
  The manual for the application programming interface for the Acutis template
  language.
showTitle: true
layout: layout.acutis
next: null
---

An Acutis template tree is rendered using a *source string*, a *props
object*, a *render context function*, and an *object with template
components*.

Template sources are compiled into functions. These functions are then executed
with their props and a render context function, which produces the final output.

[[toc]]

## Example code

### JavaScript

```js
const { compile, renderContext, result } = require("acutis-lang");
const template = compile("Hello {{ name }}.", module.filename);
const footer = compile("Copyright 2020.", "Footer")
const render = renderContext({Footer: footer});
const props = {name: "Carlo"}
const { data, errors } = result(template(render, props, {}));
if (data) {
  console.log(data); // Logs: "Hello Carlo."
} else {
  console.error(errors);
}
```

### ReScript 

```reason
open Acutis
let template = Compile.make("Hello {{ name }}.", __FILE__)
let footer = Compile.make("Copyright 2020.", "Footer")
let render = Render.makeContext(Js.Dict.fromArray([("Footer", footer)]))
let props = Js.Dict.fromArray([("name", "Carlo")])
switch template(. render, props, Js.Dict.empty()) {
| Ok(data) => Js.log(data) // Logs: "Hello Carlo."
| Error(errors) => Js.Console.error(errors)
}
```

## Function reference

### `compile`

The `compile` function (`Compile.make` in ReScript) compiles a string into a
template function. It can optionally accept a string name for debugging. See
[template functions][1] to read about how these work.

#### JavaScript example

```js
const { compile } = require("acutis-lang");
const template = compile("Hello {{ name }}.", module.filename);
```

#### ReScript example

```reason
open Acutis
let template = Compile.make("Hello {{ name }}.", ~name=__FILE__)
```

⚠️ In ReScript, the function returned by `Compile.make` is [uncurried].

### `renderContext`

The `renderContext` function (`Render.makeContext` in ReScript) accepts an
object with all of the template components. This makes our template
components global across all templates in the render tree. It returns the
function used to actually render the compiled templates.

#### JavaScript example

```js
const { compile, renderContext } = require("acutis-lang");
const components = {
  "Footer": compile("Copyright 2020.", "Footer")
};
const render = renderContext(components);
```

#### ReScript example

```reason
open Acutis
let components = Js.Dict.fromArray([
  ("Footer", Compile.make("Copyright 2020.", ~name="Footer"))
])
let render = Render.makeContext(components)
```

### `renderContextAsync`

The `renderContextAsync` function (`Render.makeContextAsync` in ReScript)
works identically to `renderContext` except that it returns a [Promise] of
the result. Use this if you have [template functions][1] that require
asynchronous logic.

## How to handle the output

The data returned by a template can either contain the rendered output or an
array of error messages. How you use this data will be different depending on
whether you're using JavaScript or ReScript.

If you're using asynchronous templates, then the output will always be
wrapped inside a resolved promise. Acutis makes sure that promises never
reject since it captures any errors inside the output.

### JavaScript: the `result` function

Acutis provides a `result` function which converts its internal data
structure into something nicer for JavaScript.

```js
const { result } = require("acutis-lang");
// suppose template, render, and props are already defined.
const x = result(template(render, props, {}));
// or with an async renderer
const xAsync = template(renderAsync, props, {}).then(result);
```

In this example, `x` becomes an object that looks like this:

```json
{
  "data": "The content of your output.",
  "errors": []
}
```

If there are any errors present, then `data` will be `null`. 

```js
if (x.data !== null) {
  console.log(x.data);
} else {
  console.error(x.errors);
}
```

### ReScript: the result type

In ReScript, the data uses the [result] data type which you can directly
access. This is the actual type definition:

```reason
type renderResult = result<string, array<Errors.t>>
```

```reason
switch x {
| Ok(data) => Js.log(data)
| Error(errors) => Js.Console.error(errors)
}
```

### Handling errors

In either language, errors are represented the same. Here's an example error
message:

```json
{
  "kind": "Syntax",
  "location": {
    "character": 11,
  },
  "message": "Invalid character: \"*\".",
  "template": "TemplateName"
}
```

Any of these fields may not be present if there isn't any data for them. If
an exception was thrown while rendering, then it will be caught and added to
an `exn` field.

When Acutis encounters an error, it will try to keep processing the template
and report as many errors as it can find. But as long as there is at least
one error, then it won't return any rendered data.

## Template functions overview

Templates returned by `compile` are functions that accept three arguments:

1. The render context function, created by `renderContext`.
2. A props object.
3. A children object.

The children argument is similar to the props, except it contains rendered
template sections. You'll typically pass an empty object (`{}`) to this from
your top-level template. See [using the `children` argument][3] for more
information about this.

```js
const result = template(renderContext, props, children);
```

## Template component functions

Because templates are just functions, we can write them manually with custom
JavaScript. Any functions we write will need to adhere to the same signature
as the functions created by `compile`.

The render context function (taken from the first argument) accepts three
arguments: a low-level abstract syntax tree (AST), a props object , and the
optional children object.

⚠️ In ReScript, these functions must be [uncurried].

### `makeAst`

The `makeAst` function (`Compile.makeAst` in ReScript) is part of the
low-level API that produces the raw AST for a template.

Suppose we want a `Footer` template that will always return the current year.
We can write a template to similar to the following.

#### JavaScript example

```js
const { makeAst } = require("acutis-lang");
const ast = makeAst("Copyright {{ year }}", module.filename);
function Footer(render, props, children) {
  const props = {year: new Date().getFullYear()};
  return render(ast, props, children)
}
```

#### ReScript example

```reason
let ast = Acutis.Compile.makeAst("Copyright {{ year }}", __FILE__)
let footer = (. render, props, children) => {
  let props = Js.Dict.fromArray([
    ("year", Js.Date.make()->Js.Date.getFullYear)
  ])
  render(ast, props, children)
}
```

### Synchronous and asynchronous templates

If you have a template function that uses `async` functions or promises, then
you need to render your whole template tree with an asynchronous render
context.

Templates components inherit their context from their parent templates, so
synchronous templates can be used interchangeably between context types. This
does not work the other way around, though. Asynchronous templates cannot be
made synchronous.

### Using the children argument

You will usually not need to directly manipulate the children argument, or
often you can just pass an empty object (`{}`) as children.

[Template components can accept template sections as children props][4], but
these are not interchangeable with pattern props. Therefore, Acutis keeps
rendered template children in a separate object.

If you are manually rendering a template section within a function, then make
sure you pass it to the children argument.

## Node helpers API

Acutis includes functions for rendering templates in a Node.js environment,
such as for a static site generator. They can be imported from module
`acutis-lang/node-utils`.

### `loadTemplate`

Calling `loadTemplate(fileName)` will return a [Promise] of a template
function.

- If the filename ends with a `.js` extension, then it will load it with
  `require`.
- If the filename ends with a `.mjs` extension, then it will load it
  with dynamic `import`.

For both of those cases, it uses the "default" export as the template
function.

For all other file extension, it loads the file's text content as the
template.

### `filenameToComponent`

Calling `filenameToComponent(fileName)` strips the extension and capitalizes
the first letter. Example: `filenameToComponent("footer.js")` returns
`"Footer"`. This is just a convenience to help components adhere to template
naming rules.

## Acutis command line interface (CLI)

You can use Acutis from the command line. It accepts JSON data and an array
of template filenames, and it prints the rendered output.

```shell
acutis Index.acutis src/**/*.(js|mjs|acutis) < data.json > dist/index.html
```

Full options:

```html
Usage:
  acutis [options] [template] [templates...]

Options:
  -a, --async                   Enable asynchronous templates. Default: false.
  -d, --data                    The path to the JSON data file. Default: stdin.
  -o, --output                  The path to write the output. Default: stdout.
  -h, --help                    Show this message and exit.
  -v, --version                 Show the version and exit.

Templates:
  The first template listed is the root template. Templates with .js or .mjs
  extensions are rendered using their default export function. For all other
  file types, their text content is used as the template.

Examples:
  acutis --data=data.json --output=index.html Index.acutis Header.js Footer.acutis
  acutis Index.acutis _includes/**/*.(js|mjs|acutis) < data.json > index.html
  curl https://example.com/api | acutis Index.acutis *.(js|acutis) -o index.html 
```

[1]: #template-functions-overview
[2]: https://rescript-lang.org/docs/manual/latest/exception 
[3]: #using-the-children-argument
[4]: ../manual/#template-children-props
[uncurried]: https://rescript-lang.org/docs/manual/latest/function#uncurried-function
[Promise]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise 
[result]: https://rescript-lang.org/docs/manual/latest/api/belt/result