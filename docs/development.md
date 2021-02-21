---
title: Acutis development information
description: Technical details on how Acutis works under the hood.
showTitle: true
layout: layout.acutis
next: null
---

The Acutis compiler is built in [ReScript] and compiled to JavaScript. The
Acutis compiler doesn't use any dependencies, aside from the ReScript
standard library, so almost everything is hand-written.

[ReScript]: https://rescript-lang.org/

[Get the source code](https://github.com/johnridesabike/acutis).

The compiler follows these basic steps:

```
source → lexical analysis → abstract syntax tree → render
```

Both the abstract syntax tree (AST) and render steps are broken into two
submodules: 1. pattern matching and 2. everything else. We can think of the
language of patterns almost as a sub-language within Acutis. (In fact, I
developed the pattern matching logic before any of the other template logic.)

[[toc]]

## Technical overview

### Types

All of the types are defined in `Acutis_Types.res` along with some of their
utility functions. This includes tokens, AST, pattern AST, and the types of
some of the public API functions.

`Types` is a common module name in ReScript projects. `Acutis_Types` has that
extra layer of namespace to avoid shadowing when a user `open`s the `Acutis`
module.

### Lexical analysis / tokenization

Lexical analysis, also called tokenization, is performed in `Lexer.res`. It
scans the raw source code, one character at a time, and creates low-level
tokens.

The lexer has two basic modes:

1. **String mode**. This is the main body of every template. It can parse
   echoes and comments, and everything else is read as strings which are
   output as-is. (For example, your HTML code.)
2. **Expression mode**. This reads statements, components, patterns, and all
   of the other parts of the language.

Every template starts in string mode. As soon as the lexer encounters a `{%`,
it begins expression mode.

All of the tokens are stored in a temporary [`Belt.MutableQueue.t`][1] data
type.

[1]: https://rescript-lang.org/docs/manual/latest/api/belt/mutable-queue

### Compiling the abstract syntax tree

The tokens compile into the AST with `Compile.res`. The functions in this
module inspect each token in the queue and create nodes for the AST.

This compiler has a submodule for compiling patterns. This is only exposed
for testing purposes.

The AST is represented as an array of nodes. Some nodes, such as `map` or
`match` expressions, contain arrays that represent branching paths of the
tree.

### Rendering

The `Environment.res` module generates functions for producing specific
output formats. These functions call the `Render.res` module, which takes the
AST and the input "props" to produce the final output.

Like `Compile.res`, it exposes a submodule for rendering patterns.

We may normally expect the renderer to produce a string data type. However,
the output could theoretically be anything defined in the environment. I
designed this flexibility specifically for [promises].

[promises]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise

When the renderer processes a node, it adds the result to a queue. Once it
renders the entire AST, it concatenates the queue into the final output.

If we're processing templates synchronously, then the queue is filled with
strings and the output is a string.

If we're processing templates asynchronously, then the queue is filled with
promises of strings. They are resolved with `Promise.all` and then
concatenated.

## Rendering concepts

Acutis relies on recursive function types to render.

1. The render environment with a render function:
   `env.render(ast, props, children) → result`
2. The template functions with this signature:
   `template(env, props, children) → result`

The `env` argument in 2 is the same `env` record as in 1. This record
contains the `render` function with the logic and data that must be global
across the entire template tree.

The specific logic is how to use the output type, synchronously or
asynchronously. This is why there are two functions to create render
environments:

1. `Environment.make(components) →` \
   `env.render(ast, props, children) →` \
   `result`
2. `Environment.Async.make(components) →` \
   `env.render(ast, props, children) →` \
   `promise<result>`

In both versions, `components` is a dictionary object containing all of the
template components.

Suppose you have a root template, `template`, which you render like this:
`template(env, props, children)`. What happens is:

1. The `template` function takes the `render` function from `env` and
   executes it with `props`, `children`, and the AST.
2. The `render` function recursively iterates through the AST and renders
   each node with the `props` and `children`. Each result is enqueued.
3. If the `render` function encounters a template component, which is just
   another template function, `x`, it executes `x(env, props, children)`,
   beginning a new loop at step 1. Here, `x` is the component, `render` is
   the same (recursive) render environment, and `props` and `children` are
   both taken from the same AST node as `x`. The result of this is enqueued
   in the original queue.
4. Steps 2 and 3 repeat until the renderer has traversed the entire AST.
5. The queue of results is concatenated and returned as the final result.

## Error handling

Acutis shouldn't raise exceptions or reject promises. The AST and the
rendered output is always returned inside of a variant that can return either
successfully rendered data or an array of errors. If there are any errors,
then they're formatted according to the `Acuits_Types.Errors.t` type.

The `Debug.res` file handles creating the specific messages.

The lexer and compiler both stop as soon as they encounter an error. The
renderer will always transverse as much of the AST as possible and report all
of the errors it finds.

Acutis will not return partially-rendered content alongside errors.

## Building the source

The source files are in `.res`, ReScript, format. You can compile them with
the command `yarn build`, or compile in watch mode with `yarn start`. This
will generate `.js` files alongside the source.

You can test your changes with `yarn test` or `yarn test:watch` for watch
mode. This tests the generated JavaScript files.

## Syntax design

I designed Acutis' syntax to build on the work of existing conventions in
similar languages as much as possible.

I borrowed the `{{` "mustache" `}}` syntax from the titular Mustache
language.

I borrowed the `{%` expression `%}` syntax from Liquid and Jinja.

I borrowed the pattern syntax from ReScript, which is really borrowed from
both JavaScript and OCaml.

I borrowed the `{% Component x=y %} z {% /Component %}` syntax from React
JSX. Like JSX, this is just syntax for a function call.

I designed the language so different parts will look as unambiguous as
possible. An identifier like `x` will always be a binding. An expression that
begins like `{% Abc` will always be a component. `{{ Xyz }}` will always be a
template child. This makes parsing easier, for both humans and computers.

I chose the `#` "template section" `/#` syntax, as in
`{% Abc Xyz=#%} hi {%/# /%}`, because I thought of the section contents as
"blocks," and `#` kind of looks like a block. It is also semantically neutral
compared to most alternatives. I ruled out `{` and `}` because
`{% Abc Xyz={%} hi {%} /%}` just looks confusing. The curly braces are
already doing too much work.

## Current limitations and ideas for the future

I made Acutis strongly typed because this catches errors early and makes
fixing bugs easier. Acutis is dynamically typed (where types are checked at
runtime) only because that was easier to build. I think of Acutis templates
as theoretically statically typed (where types are checked at compile time),
but that would require adding another step in the compiler.

I borrowed the types themselves from JSON. This was convenient and "good
enough," even though JSON is imperfect. A more flexible and robust set of
types would be nice to have.

Component dependencies are not analyzed. It might be nice to check those at
compile time so that circular dependencies aren't allowed.

I designed the language to be as simple as possible, with the philosophy of
"there should be one obvious way to do something." For this reason, I've
excluded some utility features such as `if`...`else` statements and direct
access of object and array items. These can be achieved through pattern
matching instead, although that may be slightly more verbose. I'm not opposed
to adding these features in the future if there is a need which justifies
their maintenance.

Props, template children, and template components each live in separate
"layers" of the language due to their internal type differences. This feels
like a leaky implementation detail though, and theoretically it would be nice
if they could all coexist in one single `props` object for each template.

I have only done minimal performance measurements. In the "make it work, make
it right, make it fast," order, performance comes last.

Editor tooling, build system plugins, etc. are all on the wishlist.