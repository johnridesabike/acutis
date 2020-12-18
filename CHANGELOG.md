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