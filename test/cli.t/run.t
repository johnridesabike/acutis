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

Escapes are customizable.
  $ echo "{% \"&<\" %}" > escapes.acutis
  $ acutis --data data.json --escape _ _ escapes.acutis
  &<
  $ acutis --data data.json --escape "&" "&amp;" escapes.acutis
  &amp;<
  $ acutis --data data.json --escape "&" "&custom1;" --escape "<" "&custom2;" \
  >   escapes.acutis
  &custom1;&custom2;

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
    --escape      Add a character to escape. This takes two arguments: a character and its replacement. Default: HTML character escapes.
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
    --escape      Add a character to escape. This takes two arguments: a character and its replacement. Default: HTML character escapes.
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
  Error in file "syntax1.acutis", 1:8-1:14.
  Sequential echoes must be separated by a '?'.
  
  Warning in file "unused1.acutis", 1:18-1:19.
  This variable is bound but never used:
    a
  
  Warning in file "unused2.acutis", 1:18-1:19.
  This variable is bound but never used:
    b
  
  Warning in file "error.acutis", 1:18-1:19.
  This variable is bound but never used:
    c
  
  Error while rendering "error.acutis".
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
