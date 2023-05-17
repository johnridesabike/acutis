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
  Parse and execute Acutis language templates.
  
  Usage:
    acutis [options] [template] [...templates]
  
  Options:
    --data         The path to a JSON data file. Default: stdin.
    --output       The path to write the output. Default: stdout.
    --js TODO      
    --jsmodule TODO
    --version      Show the version number and exit.
    --printast     Print the template's untyped AST form and exit.
    --printtypes   Print the template's type interface and exit.
    --printopt     Print the template's optimized form and exit.
    -help          Display this list of options
    --help         Display this list of options

Errors

  $ echo '{"badjson": error}' | acutis layout.acutis
  Error decoding JSON input.
  Line 1, bytes 12-19:
  Invalid token 'error}
  '
  [1]

  $ acutis --data=data.json
  Compile error.
  You need to provide a template.
  [1]

  $ acutis --data=data.json notfound.acutis
  System error:
  notfound.acutis: No such file or directory
  [1]
