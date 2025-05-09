  $ acutis --mode js template.acutis > compiled.mjs

  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({
  >     bool: true,
  >     int_enum: 1,
  >     string_enum: "a",
  >     tuple: ["a", "b"],
  >     nested: {level_1: ["a", "b"]},
  >     long_type: {tag: false},
  >     list: [{a: "a"}, {a: "b"}],
  >   })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  File "template.acutis"
  Render error.
  The data supplied does not match this template's interface.
  Path:
  <input> -> bool
  Expected type:
  false
  Received value:
  true

  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({
  >     bool: false,
  >     int_enum: 3,
  >     string_enum: "a",
  >     tuple: ["a", "b"],
  >     nested: {level_1: ["a", "b"]},
  >     long_type: {tag: false},
  >     list: [{a: "a"}, {a: "b"}],
  >   })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  File "template.acutis"
  Render error.
  The data supplied does not match this template's interface.
  Path:
  <input> -> int_enum
  Expected type:
  @1 | @2
  Received value:
  3

  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({
  >     bool: false,
  >     int_enum: 1,
  >     string_enum: "c",
  >     tuple: ["a", "b"],
  >     nested: {level_1: ["a", "b"]},
  >     long_type: {tag: false},
  >     list: [{a: "a"}, {a: "b"}],
  >   })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  File "template.acutis"
  Render error.
  The data supplied does not match this template's interface.
  Path:
  <input> -> string_enum
  Expected type:
  @"a" | @"b"
  Received value:
  c

  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({
  >     bool: false,
  >     int_enum: 1,
  >     string_enum: "a",
  >     tuple: ["a"],
  >     nested: {level_1: ["a", "b"]},
  >     long_type: {tag: false},
  >     list: [{a: "a"}, {a: "b"}],
  >     })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  File "template.acutis"
  Render error.
  The data supplied does not match this template's interface.
  Path:
  <input> -> tuple
  Expected type:
  (string, string)
  Received value:
  a

  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({
  >     bool: false,
  >     int_enum: 1,
  >     string_enum: "a",
  >     tuple: ["a", "b"],
  >     nested: {level_1: ["a", 1]},
  >     long_type: {tag: false},
  >     list: [{a: "a"}, {a: "b"}],
  >     })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  File "template.acutis"
  Render error.
  The data supplied does not match this template's interface.
  Path:
  <input> -> nested -> level_1 -> 1
  Expected type:
  string
  Received value:
  1

  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({
  >     bool: false,
  >     int_enum: 1,
  >     string_enum: "a",
  >     tuple: ["a", "b"],
  >     nested: {level_1: ["a", "b"]},
  >     long_type: {tag: true, bad: "fail"},
  >     list: [{a: "a"}, {a: "b"}],
  >     })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  File "template.acutis"
  Render error.
  The data supplied does not match this template's interface.
  Path:
  <input> -> long_type
  Expected type:
  {@tag: false} |
  {
    @tag: true,
    another_loooong_field: string,
    looong_field: string,
    yet_another_field: string
  }
  Input is missing keys:
  another_loooong_field, looong_field, yet_another_field


  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({
  >     bool: false,
  >     int_enum: 1,
  >     string_enum: "a",
  >     tuple: ["a", "b"],
  >     nested: {level_1: ["a", "b"]},
  >     long_type: "fail",
  >     list: [{a: "a"}, {a: "b"}],
  >     })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  File "template.acutis"
  Render error.
  The data supplied does not match this template's interface.
  Path:
  <input> -> long_type
  Expected type:
  {@tag: false} |
  {
    @tag: true,
    another_loooong_field: string,
    looong_field: string,
    yet_another_field: string
  }
  Received value:
  fail

  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({
  >     bool: false,
  >     int_enum: 1,
  >     string_enum: "a",
  >     tuple: ["a"],
  >     nested: {level_1: ["a", 1]},
  >     long_type: {tag: true, bad: "fail"},
  >     list: "fail",
  >     })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  File "template.acutis"
  Render error.
  The data supplied does not match this template's interface.
  Path:
  <input> -> list
  Expected type:
  [{a: string}]
  Received value:
  fail
  
  File "template.acutis"
  Render error.
  The data supplied does not match this template's interface.
  Path:
  <input> -> long_type
  Expected type:
  {@tag: false} |
  {
    @tag: true,
    another_loooong_field: string,
    looong_field: string,
    yet_another_field: string
  }
  Input is missing keys:
  another_loooong_field, looong_field, yet_another_field
  
  File "template.acutis"
  Render error.
  The data supplied does not match this template's interface.
  Path:
  <input> -> nested -> level_1 -> 1
  Expected type:
  string
  Received value:
  1
  
  File "template.acutis"
  Render error.
  The data supplied does not match this template's interface.
  Path:
  <input> -> tuple
  Expected type:
  (string, string)
  Received value:
  a


  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({})
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  File "template.acutis"
  Render error.
  The data supplied does not match this template's interface.
  Path:
  <input>
  Expected type:
  bool = false
  int_enum = @1 | @2
  list = [{a: string}]
  long_type =
    {@tag: false} |
    {
      @tag: true,
      another_loooong_field: string,
      looong_field: string,
      yet_another_field: string
    }
  nested = {level_1: [string]}
  string_enum = @"a" | @"b"
  tuple = (string, string)
  Input is missing keys:
  bool, int_enum, list, long_type, nested, string_enum, tuple
