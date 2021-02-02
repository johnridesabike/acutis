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
- Rescript: Changed namespace to `AcutisLang`.
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
- Removed JavaScript `result` function in favor of directly accessing the
  data structure.
- Made the `exn` field in the error record more type-safe by hiding its
  contents.

## 0.3.0

- Rewrote error handling.
  - Exceptions are no longer thrown.
  - Errors are more readable.
  - Rendered template output is returned in a ReScript result data type,
    which either contains the compiled output or an array of errors.
  - A JavaScript `result` function can convert the output into a format
    friendly for JavaScript environments.
- Improved detecting and reporting bad AST input.
- CLI: Added option to print errors with `console.table`.

## 0.2.1

- Fixed whitespace control to work on empty strings.
- Added license to bundled source files.

## 0.1.0

- Initial release