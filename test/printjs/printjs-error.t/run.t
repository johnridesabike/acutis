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
  bool <- <input>
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
  int_enum <- <input>
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
  string_enum <- <input>
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
  tuple <- <input>
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
  >     tuple: ["a"],
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
  1 <- level_1 <- nested <- <input>
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
  >     tuple: ["a"],
  >     nested: {level_1: ["a", 1]},
  >     long_type: {tag: true, bad: "fail"},
  >     list: [{a: "a"}, {a: "b"}],
  >     })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  File: template.acutis
  Render error.
  The data supplied does not match this template's interface.
  Path:
  long_type <- <input>
  Expected type:
  {@tag: false} |
  {
    @tag: true,
    another_loooong_field: string,
    looong_field: string,
    yet_another_field: string
  }
  Input is missing keys:
  yet_another_field, looong_field, another_loooong_field


  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({
  >     bool: false,
  >     int_enum: 1,
  >     string_enum: "a",
  >     tuple: ["a"],
  >     nested: {level_1: ["a", 1]},
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
  long_type <- <input>
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
  list <- <input>
  Expected type:
  [{a: string}]
  Received value:
  fail


  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({})
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  File: template.acutis
  Render error.
  The data supplied does not match this template's interface.
  Path:
  <input>
  Expected type:
  {
    bool: false,
    int_enum: @1 | @2,
    list: [{a: string}],
    long_type:
      {@tag: false} |
      {
        @tag: true,
        another_loooong_field: string,
        looong_field: string,
        yet_another_field: string
      },
    nested: {level_1: [string]},
    string_enum: @"a" | @"b",
    tuple: (string, string)
  }
  Input is missing keys:
  tuple, string_enum, nested, long_type, list, int_enum, bool
