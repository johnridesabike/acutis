Test that the CLI options work as intended.

Process data through stdin.
  $ cat data.json | acutis layout.acutis a.acutis
  Carlo

Process data through an argument.
  $ acutis --data=data.json layout.acutis a.acutis
  Carlo

Process data through stdin as an argument.
  $ cat data.json | acutis --data=- layout.acutis a.acutis
  Carlo

Output to a file.
  $ acutis --data=data.json --output="result.txt" layout.acutis a.acutis
  $ cat result.txt
  Carlo

Output to stdout as an argument.
  $ acutis --data=data.json --output=- layout.acutis a.acutis
  Carlo

Display the help.
  $ acutis --help
  Usage:
    acutis [OPTIONS...] [TEMPLATE] [COMPONENTS...]
  
  Compile and render Acutis language templates.
  
  Options:
    --mode {render|js|cjs}
                  Either render the template, compile it to a JavaScript module, or compile it to a CommonJS module. Default: render.
    --output      The path to write the output. Default: stdout.
    --data        The path to a JSON file to be used with --mode=render. Default: stdin.
    --fun         Add an external JavaScript function as a component. This takes three arguments: file path, function name, and type interface.
    --version     Print the version number and exit.
    --printast    Print the template's untyped AST form and exit.
    --printtypes  Print the template's type interface and exit.
    --printopt    Print the template's optimized form and exit.
    --printinst   Print the template's runtime instructions and exit.
    -help         Display this list of options
    --help        Display this list of options

Errors

  $ acutis --data=data.json
  You need to provide a template.
  
  Usage:
    acutis [OPTIONS...] [TEMPLATE] [COMPONENTS...]
  
  Compile and render Acutis language templates.
  
  Options:
    --mode {render|js|cjs}
                  Either render the template, compile it to a JavaScript module, or compile it to a CommonJS module. Default: render.
    --output      The path to write the output. Default: stdout.
    --data        The path to a JSON file to be used with --mode=render. Default: stdin.
    --fun         Add an external JavaScript function as a component. This takes three arguments: file path, function name, and type interface.
    --version     Print the version number and exit.
    --printast    Print the template's untyped AST form and exit.
    --printtypes  Print the template's type interface and exit.
    --printopt    Print the template's optimized form and exit.
    --printinst   Print the template's runtime instructions and exit.
    -help         Display this list of options
    --help        Display this list of options
  [1]

  $ acutis --data=data.json notfound.acutis
  System error:
  notfound.acutis: No such file or directory
  [1]

Multiple warnings and errors are printed correctly.
  $ echo "{% bad syntax" > syntax1.acutis
  $ echo "{% match a with {a} %}{% /match %}" > unused1.acutis
  $ echo "{% match b with {b} %}{% /match %}" > unused2.acutis
  $ echo "{% match c with {c} %}{% /match %}{% Unused1 a /%} {% Unused2 b /%}" \
  > > error.acutis
  $ cat data.json | \
  > acutis error.acutis syntax1.acutis unused1.acutis unused2.acutis
  File "syntax1.acutis", 1:8-1:14
  Parse error.
  Sequential echoes must be separated by a '?'.
  
  File "unused1.acutis", 1:18-1:19
  Type warning.
  This variable is bound but never used:
    a
  
  File "unused2.acutis", 1:18-1:19
  Type warning.
  This variable is bound but never used:
    b
  
  File "error.acutis", 1:18-1:19
  Type warning.
  This variable is bound but never used:
    c
  
  File "error.acutis"
  Render error.
  The data supplied does not match this template's interface.
  Path:
  <input>
  Expected type:
  a = {a: _}
  b = {b: _}
  c = {c: _}
  Input is missing keys:
  a, b, c
  [1]
