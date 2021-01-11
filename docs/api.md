---
title: Acutis API for JavaScript & ReScript
description: |
  The manual for the application programming interface for the Acutis template
  language.
showTitle: true
layout: layout.acutis
next: null
---

An Acutis template tree is rendered using a *template source*, a *props
object*, an *environment object*, and an *object with template components*.

Template sources are compiled into functions. These functions are then
executed with their props and their environment's renderer, which produces
the final output.

(Note: This document refers to "objects" due to their representation in
JavaScript. In ReScript, these are records or dictionaries.)

[[toc]]

## Example code

### JavaScript

```js
const { Compile, Environment } = require("acutis-lang");
const template = Compile.make("Hello {{ name }}.", module.filename);
const footer = Compile.make("Copyright 2021.", "Footer")
const env = Environment.make({Footer: footer});
const result = template(env, {name: "Carlo"}, {});
if (result.NAME === "errors") {
  console.error(result.VAL);
} else {
  console.log(result.VAL); // Logs: "Hello Carlo."
}
```

### ReScript 

```reason
open Acutis
let template = Compile.make("Hello {{ name }}.", __FILE__)
let footer = Compile.make("Copyright 2021.", "Footer")
let env = Environment.make(Js.Dict.fromArray([("Footer", footer)]))
let props = Js.Dict.fromArray([("name", "Carlo")])
switch template(. env, props, Js.Dict.empty()) {
| #data(data) => Js.log(data) // Logs: "Hello Carlo."
| #errors(errors) => Js.Console.error(errors)
}
```

## Function reference

### `Compile.make`

The `Compile.make` function compiles a string into a template function. It
can optionally accept a string name for debugging. See [template
functions] to read about how these work.

#### JavaScript example

```js
const { Compile } = require("acutis-lang");
const template = Compile.make("Hello {{ name }}.", module.filename);
```

#### ReScript example

```reason
open Acutis
let template = Compile.make("Hello {{ name }}.", ~name=__FILE__)
```

⚠️ In ReScript, the function returned by `Compile.make` is [uncurried].

### `Environment.make`

The `Environment.make` function accepts an object with all of the template
components. This makes our template components global across all templates in
the render tree. It returns an object with functions used to render compiled
templates.

#### JavaScript example

```js
const { Compile, Environment } = require("acutis-lang");
const components = {
  Footer: Compile.make("Copyright 2021.", "Footer")
};
const env = Environment.make(components);
```

#### ReScript example

```reason
open Acutis
let components = Js.Dict.fromArray([
  ("Footer", Compile.make("Copyright 2021.", ~name="Footer"))
])
let env = Environment.make(components)
```

### `Environment.Async.make`

The functions returned by `Environment.Async.make` work identically to
`Environment.make` except that they each return a [Promise]. Use this if you
have [template functions] that require asynchronous logic.

## How to handle the output

The data returned by a template can either contain the rendered output or an
array of error messages. How you use this data will be different depending on
whether you're using JavaScript or ReScript.

If you're using asynchronous templates, then the output will always be
wrapped inside a resolved promise. Acutis makes sure that promises never
reject since it captures any errors inside the output.

### JavaScript: `{ NAME, VAL }`

Acutis returns data inside an object with `NAME` and `VAL` properties. If any
errors occurred, then `NAME` will be `"errors"` and `VAL` will be an array of
the errors. If no errors occurred, then `NAME` will be `"data"` and `VAL` will
be the rendered output.

#### Example successful output

```json
{
  "NAME": "data",
  "VAL": "The content of your output."
}
```

#### Example unsuccessful output

```json
{
  "NAME": "errors",
  "VAL": [
    {
      "message": "An object with error details goes here."
    }
  ]
}
```

#### Example usage

```js
if (result.NAME === "errors") {
  console.error(result.VAL);
} else {
  console.log(result.VAL);
}
```

### ReScript: `[#data('a) | #errors('e)]`

In ReScript, the output uses a polymorphic variant defined as `[#data('a) |
#errors('e)]`. You can use `switch` with it like any variant.

```reason
switch result {
| #data(data) => Js.log(data)
| #errors(errors) => Js.Console.error(errors)
}
```

### Handling errors

In either language, errors are represented the same. Here's an example error
message:

```json
{
  "kind": "Render",
  "location": {
    "character": 11,
  },
  "message": "\"a\" is type null. I can only echo strings and numbers.",
  "path": [
    "ChildTemplate.acutis",
    "match",
    "MainTemplate.acutis"
  ]
}
```

Any of these fields may not be present if there isn't any data for them. If
an exception was thrown while rendering, then it will be caught and added to
an `exn` field.

When Acutis encounters an error, it will try to keep processing the template
and report as many errors as it can find. But as long as there is at least
one error, then it won't return any rendered data.

## Template functions overview

Templates returned by `Compile.make` are functions that accept three
arguments:

1. An environment object created by `Environment.make`.
2. A props object.
3. A children object. 

In ReScript, the environment is a [record], and props and children are both
[JS dictionary objects][dict].

The children argument is similar to the props, except it contains rendered
template sections. You'll typically pass an empty object (`{}`) to this from
your top-level template. See [using the `children` argument][1] for more
information about this.

```js
const result = template(env, props, children);
```

## Template component functions

Because templates are just functions, we can write them manually with custom
JavaScript. Any functions we write will need to adhere to the same signature
as the functions created by `Compile.make`.

⚠️ In ReScript, template functions and their environment functions are
[uncurried].

### `Compile.makeAst`

The `Compile.makeAst` function is part of the low-level API that produces the
raw AST for a template. It accepts a string of the template contents, and its
output should be passed to [`environment.render`][environment.render].

Suppose we want a `Footer` template that will always display the current
year. We can write a template to similar to the following.

#### JavaScript example

```js
const { Compile } = require("acutis-lang");
const ast = Compile.makeAst("Copyright {{ year }}", module.filename);
function Footer(env, props, children) {
  const year = new Date().getFullYear();
  return env.render(ast, { year }, children)
}
```

#### ReScript example

```reason
let ast = Acutis.Compile.makeAst("Copyright {{ year }}", __FILE__)
let footer = (. env, props, children) => {
  Js.Dict.set(props, "year", Js.Date.make()->Js.Date.getFullYear)
  env.render(. ast, props, children)
}
```

⚠️ In ReScript, you may need to help the type inference by annotating your
functions like this:

```reason
open Acutis
let footer: Acutis_Types.template<'a> = (. env, props, children) =>
  env.render(. ast, props, children)
```

## The environment functions

The environment object (taken from the first argument) contains functions for
returning rendered template contents.

### `environment.render`

The `render` function processes compiled template ASTs.

#### JavaScript example

```js
// Assume the AST is already defined
function template(env, props, children) {
  return env.render(ast, props, children);
}
```

#### ReScript example

```reason
// Assume the AST is already defined
let template = (. env, props, children) =>
  env.render(. ast, props, children)
```

### `environment.return`

The `return` function returns a string as-is, wrapped inside the result type.
This is useful if you don't need the Acutis language features for a
component.

#### JavaScript example

```js
function sayHi(env, props, children) {
  return env.return("Hello");
}
```

#### ReScript example

```reason
let sayHi = (. env, props, children) =>
  env.return(. "Hello")
```

### `environment.error`

The `error` function returns an error with the given message. This will cause
the template to fail to render.

#### JavaScript example

```js
function fail(env, props, children) {
  return env.error("Fail.");
}
```

#### ReScript example

```reason
let fail = (. env, props, children) =>
  env.error(. "Fail.")
```

### `environment.mapChild`

The `mapChild` function takes a child prop and a callback to transform the
child's contents. It returns a new child and does not mutate the original. If
the child has an error, then `mapChild` has no effect.

Template children are valid return types (each one is a fully rendered
template). So they can either be returned as your template or passed to the
children argument of `environment.render`.

#### JavaScript example

```js
function shout(env, props, { Children }) {
  if (Children) {
    return env.mapChild(Children, (x) => x.toUpperCase());
  } else {
    return env.return("");
  }
}
```

#### ReScript example

```reason
let shout = (. env, props, children) =>
  switch Js.Dict.get(children, "Children") {
  | Some(children) =>
    env.mapChild(. children, (. x) => Js.String.toUpperCase(x))
  | None => env.return(. "")
  }
```

### `environment.flatMapChild`

The `flatMapChild` function works similarly to `mapChild` except that it does
not automatically wrap the returned data. When using `flatMapChild`, you're
responsible for wrapping the output with `environment.return` in your
callback. This means that you can choose to use `environment.error` instead
to represent failures.

#### JavaScript example

```js
// assume we already defined a function called somethingThatThrows
function template(env, props, { Children }) {
  return env.flatMapChild(Children, (contents) => {
    try {
      return env.return(somethingThatThrows(contents));
    } catch (e) {
      return env.error(e.message);
    }
  });
}
```

#### ReScript example

```reason
// assume we already defined a function called somethingThatThrows
let shout = (. env, props, children) =>
  env.flatMapChild(.
    Js.Dict.unsafeGet(children, "Children"),
    (. contents) =>
      switch somethingThatThrows(contents) {
      | result => env.return(. result)
      | exception Failure(message) => env.error(. message)
      }
  )
```

## Template function tips

### Synchronous and asynchronous templates

If you have a template function that uses `async` functions or promises, then
you need to render your whole template tree with an asynchronous environment.

Templates components inherit their environment from their parent templates,
so synchronous templates can be used interchangeably between environment
types. This does not work the other way around, though. Asynchronous
templates cannot be made synchronous.

### Using the children argument

[Template components can accept template sections as children props][2], but
these are not interchangeable with pattern props. Therefore, Acutis keeps
rendered template children in a separate object.

You will usually not need to directly manipulate the children argument, or
often you can just pass an empty object (`{}`) as children.

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
  -t, --table                   Print errors in a table. Default: false.
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

[1]: #using-the-children-argument
[2]: ../manual/#template-children-props
[template functions]: #template-functions-overview
[environment.render]: #environment.render
[record]: https://rescript-lang.org/docs/manual/latest/record
[dict]: https://rescript-lang.org/docs/manual/latest/api/js/dict
[uncurried]: https://rescript-lang.org/docs/manual/latest/function#uncurried-function
[Promise]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise 