---
title: Acutis API for JavaScript & ReScript
description: |
  The manual for the application programming interface for the Acutis template
  language.
showTitle: true
layout: layout.acutis
next: null
---

An Acutis template tree is rendered using *template source(s)*, a *props
object*, and an *environment object*,

Template sources are compiled into functions. These functions are then
executed with their props and their environment's renderer, which produces
the final output.

(Note: This document refers to "objects" due to their representation in
JavaScript. In ReScript, these are either records or dictionaries.)

[[toc]]

## Example code

### JavaScript

```js
const { Compile, Environment, Result, Source } = require("acutis-lang");
const components = Compile.Components.make([
  Source.string("Footer", "Copyright 2021.")
]);
const env = Environment.sync
const src = Source.string("Main", "Hello {{ name }}");
const template = Compile.make(src, Result.getExn(components));
const result = Result.flatMap(
  template,
  template => template(env, {name: "Carlo"}, {})
 );
const output = Result.getOrElse(
  result,
  (errors) => {
    console.error(errors);
    throw new Error("I couldn't render this template.");
  }
);
console.log(output); // Logs: "Hello Carlo."
```

### ReScript 

```reason
open AcutisLang
let components = Compile.Components.make([
  Source.string(~name="Footer", "Copyright 2021.")
])
let env = Environment.sync
let props = Js.Dict.fromArray([("name", "Carlo")])
Source.string(~name="Main", "Hello {{ name }}")
  ->Compile.make(Result.getExn(components))
  ->Result.flatMap(template => template(env, props, Js.Dict.empty()))
  ->Result.getOrElse(errors => {
    Js.Console.error(errors)
    failwith("I couldn't render this template.")
  })
  ->Js.log // Logs: "Hello Carlo."
```

## Sources

The `Source` module's functions classify different types of sources to
prepare them for the compiler. They return an internal data type that the
compiler uses.

⚠️ Note: The first `name` argument for each source will be the name used if
the you include the template as a [component]. The compiler currently allows
you to name your templates anything you want, but they must adhere to the
component naming syntax to be usable as components.

### `Source.string`

The `Source.string` function accepts the name of a template and the raw
source code in a string.

#### JavaScript example

```js
const { Source } = require("acutis-lang");
const src = Source.string("Main", "Hello {{ name }}");
```

#### ReScript example

```reason
open AcutisLang
let src = Source.string(~name="Main", "Hello {{ name }}")
```

### `Source.func`

The `Source.func` function accepts the name of the template and a function.
See [template functions] for more information on how these work.

#### JavaScript example

```js
const { Source } = require("acutis-lang");
const src = Source.func(
  "Main",
  (env, props, children) => env.return("Hello")
);
```

#### ReScript example

```reason
open AcutisLang
let src = Source.func(
  ~name="Main",
  (env, props, children) => env.return(. "Hello")
)
```

### `Source.funcWithString`

The `Source.funcWithString` function accepts the name of the template, the
raw template source as a string, and a function which accepts the compiled
template and returns a template function. See [template functions] for more
on how these work.

This is useful if you have a regular string template, but you want to wrap it
in some custom runtime logic.

#### JavaScript example

```js
const { Source } = require("acutis-lang");
const src = Source.funcWithString(
  "Main",
  "Hello {{ name }}",
  (ast) => (env, props, children) => env.render(ast, props, children)
);
```

#### ReScript example

```reason
open AcutisLang
let src = Source.funcWithString(
  ~name="Main",
  "Hello {{ name }}",
  (ast, env, props, children) => env.render(. ast, props, children)
)
```

## Compiling

Once you classify a template source through one of the `Source` functions,
the next step is compiling it.

### `Compile.make`

The `Compile.make` function compiles a source and its components into a
[result] of a template function. See [results] and [template functions] to
read about how these work.

#### JavaScript example

```js
const { Compile, Source } = require("acutis-lang");
const components = Compile.Components.empty();
const src = Source.string("Main", "Hello {{ name }}");
const templateResult = Compile.make(src, components);
```

#### ReScript example

```reason
open AcutisLang
let components = Compile.Components.empty()
let src = Source.string(~name="Main", "Hello {{ name }}")
let templateResult = Compile.make(src, components)
```

### `Compile.Components.make`

The `Compile.Components.make` function accepts an array of [sources] and
returns a [result] of their compiled output.

#### JavaScript example

```js
const { Compile, Source } = require("acutis-lang");
const header = Source.string("Header", "<h1>Hello</h1>");
const footer = Source.string("Footer", "Copyright 2021 Carlo.");
const componentsResult = Compile.Components.make([header, footer])
```

#### ReScript example

```reason
open AcutisLang
let header = Source.string(~name="Header", "<h1>Hello</h1>");
let footer = Source.string(~name="Footer", "Copyright 2021 Carlo.");
let componentsResult = Compile.Components.make([header, footer])
```

### `Compile.Components.empty`

If you aren't using components, or you need a placeholder, then the
`Compile.Components.empty` function will return an empty components value.

Running `Compile.Components.empty()` is equivalent to
`Compile.Components.make([])`. It's just slightly more efficient.

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
the errors. If no errors occurred, then `NAME` will be `"ok"` and `VAL` will
be the rendered output.

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

#### Example usage

```js
if (result.NAME === "errors") {
  console.error(result.VAL);
} else {
  console.log(result.VAL);
}
```

### ReScript: `[#ok('a) | #errors(array<Acutis_Types.Errors.t>)]`

In ReScript, the output uses a polymorphic variant defined as `[#ok('a) |
#errors(array<Acutis_Types.Errors.t>)]`. You can use `switch` with it like
any variant.

This is conceptually the same as the standard [ReScript result type]. We're
using a custom polymorphic variant because of its nicer JavaScript
representation.

```reason
switch result {
| #ok(data) => Js.log(data)
| #errors(errors) => Js.Console.error(errors)
}
```

### `Result.map`

The `Result.map` function accepts a result and a function that accepts its
contents and returns a new value. The given function only executes if the
result does not have errors.

#### JavaScript Example

```js
// Assume props and children are already defined.
const result = template(Environment.sync, props, children);
const resultALLCAPS = Result.map(result, x => x.toUpperCase());
```

#### ReScript Example

```reason
// Assume props and children are already defined.
let result = template(Environment.sync, props, children)
let resultALLCAPS = Result.map(result, Js.String.toUpperCase)
```

### `Result.flatMap`

The `Result.flatMap` function works almost identically to `Result.map`,
except that the given function must _return another `result`_.

#### Example

We can write the JavaScript and ReScript example identically.

```reason
// Assume src, components, props, and children is defined already
let templateResult = Compile.make(src, components)
let outputResult = Result.flatMap(
  templateResult,
  (template) => template(Environment.sync, props, children)
)
```

### `Result.getExn`

The `Result.getExn` function returns the value of the result if there are no
errors, and raises an exception if there are errors.

The exception raised is a ReScript exception, which doesn't always play
nicely with JavaScript tooling. Often, you'll want to use the more flexible
`Result.getOrElse` function instead.

#### Example

We can write the JavaScript and ReScript example identically.

```reason
// Raises an exception if the result has errors.
let output = Result.getExn(result)
```

### `Result.getOrElse`

The `Result.getOrElse` function returns the value of the result if there are no
errors, and executes a given function if there are errors.

This can be useful for either getting a "default" value, or doing something
with the errors (such as logging them to the console).

#### JavaScript example

```js
const output = Result.getOrElse(
  result,
  (errors) => {
    console.error(errors);
    throw new Error("I can't continue due to those errors.");
  }
);
```

#### ReScript example

```reason
let output = Result.getOrElse(
  result,
  (errors) => {
    Js.Console.error(errors)
    failwith("I can't continue due to those errors.")
  }
)
```

## Error messages

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
    "ChildTemplate",
    "match",
    "MainTemplate"
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

Suppose we want a `Footer` template that will always display the current
year. We can write a template to similar to the following.

#### JavaScript example

```js
const { Source } = require("acutis-lang");
const footer = Source.funcWithString(
  "Footer",
  "Copyright {{ year }}"
  (ast) => (env, props, children) => {
    const year = new Date().getFullYear();
    return env.render(ast, { year }, children)
  }
);
```

#### ReScript example

```reason
let footer = 
  AcutisLang.Source.funcWithString(
    ~name="Footer",
    "Copyright {{ year }}"
    (ast, env, props, children) => {
      Js.Dict.set(props, "year", Js.Date.make()->Js.Date.getFullYear)
      env.render(. ast, props, children)
    }
  )
```

## Environment

An environment is an object that contains logic that is global across all
levels of a template tree.

### `Environment.sync`

If you use `Environment.sync`, then your templates will render as [results]
of strings.

### `Environment.async`

This works identically to `Environment.sync`, except except that its render
functions each return a [Promise] of a [result] of a string. Use this if you
have [template functions] that require asynchronous logic.

If you don't need asynchronous template component functions, then
`Environment.sync` is a bit faster, since it doesn't need to allocate a bunch
of Promises.

## Environment functions

Each environment object contains functions for returning rendered template
contents.

⚠️ In ReScript, environment functions are [uncurried].

### `render`

The `render` function processes compiled templates.

#### JavaScript example

```js
Source.funcWithString(
  "Footer",
  "Copyright {{ year }}"
  (ast) => (env, props, children) => {
    const year = new Date().getFullYear();
    return env.render(ast, { year }, children)
  }
);
```

#### ReScript example

```reason
AcutisLang.Source.funcWithString(
  ~name="Footer",
  "Copyright {{ year }}"
  (ast, env, props, children) => {
    Js.Dict.set(props, "year", Js.Date.make()->Js.Date.getFullYear)
    env.render(. ast, props, children)
  }
)
```

### `return`

The `return` function returns a string as-is, wrapped inside the result type.
This is useful if you don't need the Acutis language features for a
component.

#### JavaScript example

```js
const { Source } = require("acutis-lang");
const sayHi = Source.func(
  "SayHi",
  (env, props, children) => env.return("Hello.")
);
```

#### ReScript example

```reason
let sayHi =
  AcutisLang.Source.func(
    ~name="SayHi",
    (env, props, children) => env.return(. "Hello")
  )
```

### `error`

The `error` function returns an error with the given message. This will cause
the template to fail to render.

#### JavaScript example

```js
const { Source } = require("acutis-lang");
const fail = Source.func(
  "Fail",
  (env, props, children) => env.error("This always fails.")
);
```

#### ReScript example

```reason
let fail =
  AcutisLang.Source.func(
    ~name="Fail",
    (env, props, children) => env.error(. "This always fails.")
  )
```

### `mapChild`

The `mapChild` function takes a child prop and a callback to transform the
child's contents. It returns a new child and does not mutate the original. If
the child has an error, then `mapChild` has no effect.

Template children are valid return types (each one is a fully rendered
template). So they can either be returned as your template or passed to the
children argument of `environment.render`.

#### JavaScript example

```js
const { Source } = require("acutis-lang");
const shout = Source.func(
  "Shout",
  (env, props, { Children }) => {
    if (Children) {
      return env.mapChild(Children, (x) => x.toUpperCase());
    } else {
      return env.return("");
    }
  }
);
```

#### ReScript example

```reason
let shout =
  AcutisLang.Source.func(
    ~name="Shout",
    (env, props, children) =>
      switch Js.Dict.get(children, "Children") {
      | Some(children) =>
        env.mapChild(. children, Js.String.toUpperCase)
      | None => env.return(. "")
      }
  )
```

### `flatMapChild`

The `flatMapChild` function works similarly to `mapChild` except that it does
not automatically wrap the returned data. When using `flatMapChild`, you're
responsible for wrapping the output with `environment.return` in your
callback. This means that you can choose to use `environment.error` instead
to represent failures.

#### JavaScript example

```js
const { Source } = require("acutis-lang");
// assume we already defined a function called somethingThatThrows
const src = Source.func(
  "Template",
  (env, props, { Children }) => {
    return env.flatMapChild(Children, (contents) => {
      try {
        return env.return(somethingThatThrows(contents));
      } catch (e) {
        return env.error(e.message);
      }
    })
  }
);
```

#### ReScript example

```reason
// assume we already defined a function called somethingThatThrows
let src =
  AcutisLang.Source.func(
    ~name="Template",
    (env, props, children) =>
      env.flatMapChild(.
        Js.Dict.unsafeGet(children, "Children"),
        contents =>
          switch somethingThatThrows(contents) {
          | result => env.return(. result)
          | exception Failure(message) => env.error(. message)
          }
      )
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

If the filename ends with a `.js` or `.mjs` extension, then it will load it
with dynamic `import`. It uses the "default" export as the template source.

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
  The first template listed is the root template. Templates with .js, .mjs,
  or .cjs extensions are rendered using their default export. For all other
  file types, their text content is used as the template.

Examples:
  acutis --data=data.json --output=index.html Index.acutis Header.js Footer.acutis
  acutis Index.acutis _includes/**/*.(js|mjs|acutis) < data.json > index.html
  curl https://example.com/api | acutis Index.acutis *.(js|acutis) -o index.html
```

[1]: #using-the-children-argument
[2]: ../manual/#template-children-props
[component]: ../manual/#template-components
[dict]: https://rescript-lang.org/docs/manual/latest/api/js/dict
[polymorphic variant]: https://rescript-lang.org/docs/manual/latest/polymorphic-variant
[Promise]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise 
[record]: https://rescript-lang.org/docs/manual/latest/record
[ReScript result type]: https://rescript-lang.org/docs/manual/latest/api/belt/result 
[result]: #results
[results]: #results
[string map]: https://rescript-lang.org/docs/manual/latest/api/belt/map-string
[sources]: #sources
[template functions]: #template-functions-overview
[uncurried]: https://rescript-lang.org/docs/manual/latest/function#uncurried-function