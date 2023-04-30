# Changelog

Most changes are breaking changes.

## Unreleased

- Remove format specifier options. `%i` `%f` and `%b` do not take any flags now.
- Fix false positive "unused binding" error.
- Fix bug in decoding unknown tags in open tagged unions.
- Optimization: filter empty text nodes from final tree.

## 0.21.0

- Add `.` record access syntax: `{{ book.title }}`.
- Simplify the lexer.
- Support the `unknown` data type for function components.
- Calling a component with an extra prop is now a type error.

## 0.20.0

- Change escape syntax to `{{{` `}}}`.
- Add format specifiers for echoing integers, floats, and boolean values.
- Remove the polymorphic "echoable" type.

## 0.19.0

- Make template blocks a first-class data type. Remove the child prop type that
  was previously used for them.
- Change template block syntax to begin and end with `#` (`#%} block {%#`).
- Add detection for unused bound variables.
- Make parser require a text node (`%}...{%`) separates each expression.

## 0.18.0

- Add Eleventy plugin.
- Distinguish filenames from component names when compiling components.

## 0.17.0

- Change interface syntax for consistency with other blocks.
- Fix bug with escaped quote characters in strings.

## 0.16.0

- Added interfaces. These allow you to optionally declare each template's types.
- Improved pattern-matching compilation. This fixed bugs and made the algorithm
  more robust.

## 0.15.0

- Acutis is rewritten in OCaml.
- The JavaScript output is now compiled with js_of_ocaml.
- The JavaScript API has completely changed.
- There is a new OCaml API.
- The language itself is the same, except for a few edge cases where the new
  parser behaves slightly differently.

## 0.14.0

- Acutis is now statically typed. This breaks all older templates.
- Added types:
  - Tuple.
  - Dictionary.
  - Enum.
  - Tagged union.
  - Unknown.
- Changed types:
  - Numbers are now either integers or floating points.
  - Boolean is a type of enum.
  - Records and lists (formerly called arrays) behave differently than before.
  - Nullable behaves more like an "option" type.
- Added syntax for new types.
- A new function `map_dict` now compliments the list `map`.
- Changed how external template functions work.
- Pattern matching compiles into decision trees.
- JavaScript API has almost completely changed.
- Changed license to MPL.
- There are many more changes than I can list here. See the updated docs for
  more information.

## 0.13.0

- Breaking change: invalid string escape sequences are now compile errors.
  - The only valid escape sequences currently are `\\` and `\"`.
- Fixed related escape sequence bugs.

## 0.12.0

- Added objects as a mappable type.

## 0.11.0

- Compile API changes:
  - Internal ASTs are now statically linked into directed acyclic graphs at
    compile time.
  - Circular template component dependencies are no longer allowed.
  - Components are compiled to an internal `Compile.Components.t` type.
  - `Compile.fromArray` is replaced with `Compile.Components.make`.
  - `Compile.emptyMap` is replaced with `Compile.Components.empty`.
  - `Compile.make` requires a second argument of `Compile.Components.t` type.
- Environments are no longer dynamically constructed from functions.
  - `Environment.make` function replaced with static `Environment.sync`.
  - `Environment.Async.make` function replaced with static `Environment.async`.

## 0.10.0

- New `Source` module with functions for classifying raw template sources.
- Template names are no longer optional.
- Results now use `[#ok(_) | #errors(_)]` type.
- New `Result` module with utility functions for using result types.
- Components are now stored in `Belt.Map.String.t` instead of `Js.Dict.t`.
- `Compile.make` now takes a `Source.t.` type.
- New `Compile.fromArray` creates a string map for components.
- Removed documentation for `Compile.makeAst`.
- ReScript: Added a `public` configuration to only export selected modules.
- Some public API functions changed from uncurried to curried[^1]:
  - `Source.func` function argument.
  - `Source.funcWithString` function argument.
  - `env.mapChild` function argument.
  - `env.flatMapChild` function argument.
- Upgraded ReScript to 9.0.1.

[^1]: My measurements showed negligible performance differences.

## 0.9.0

- Upgraded ReScript compiler to 9.0.
- Added ReScript standard library as a dependency.
- Removed JavaScript bundles. We now publish the compiled ReScript directly.
- Dropped Node 10 from testing and `package.json` engines. However, nothing else
  has changed yet that should stop Node 10 from working.

## 0.8.0

- Added ability to `map` static array patterns.
- Internal AST changes.

## 0.7.0

- Changed non-escape echo syntax: `{% raw x %}` is now `{{ &x }}`.
- `&` can be selectively used in `?` chains: `{{ a ? &b }}`.
- Fixed bugs allowing binding to reserved words; made parser stricter overall.
- Internal AST changes.

## 0.6.0

- Added nullish coalescing to `{{ echo }}` and `raw` statements.
- ReScript: Changed namespace to `AcutisLang`.
- Added `exports` field to package.json
- Removed prebundled IIFE files.
- Minified all bundled files.

## 0.5.0

- Added `acutis-lang` binary as an alias for `acutis`.
- Removed TypeScript definitions. (They may be added back in the future.)
- Errors now have a `path` field for showing stack traces.
- Replaced the render function with a record containing functions:
  - `render` renders an AST.
  - `return` returns a string wrapped in the result type.
  - `error` returns an error.
  - `mapChild` uses a callback to transform a template child.
  - `flatMapChild` uses a callback to transform a template child without
    automatically wrapping it in a result type.
- Renamed `Render.makeContext` to `Environment.make`.
- Renamed `Render.makeContextAsync` to `Environment.Async.make`.
- Renamed several type names to better reflect their semantics.
- Function names are now consistent between ReScript and JavaScript.
  - JavaScript: renamed `compile` to `Compile.make`.
  - JavaScript: renamed `makeAst` to `Compile.makeAst`.
  - JavaScript: renamed `renderContext` to `Environment.make`.
  - JavaScript: renamed `renderContextAsync` to `Environment.Async.make`.

## 0.4.1

- Added description to package.json.

## 0.4.0

- Changed output from ReScript stdlib result type to a polymorphic variant.
- Removed JavaScript `result` function in favor of directly accessing the data
  structure.
- Made the `exn` field in the error record more type-safe by hiding its
  contents.

## 0.3.0

- Rewrote error handling.
  - Exceptions are no longer thrown.
  - Errors are more readable.
  - Rendered template output is returned in a ReScript result data type, which
    either contains the compiled output or an array of errors.
  - A JavaScript `result` function can convert the output into a format friendly
    for JavaScript environments.
- Improved detecting and reporting bad AST input.
- CLI: Added option to print errors with `console.table`.

## 0.2.1

- Fixed whitespace control to work on empty strings.
- Added license to bundled source files.

## 0.1.0

- Initial release
