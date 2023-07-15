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
  Decode error in field: <input> -> bool
  Expected type:
  false
  Recieved value:
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
  Decode error in field: <input> -> int_enum
  Expected type:
  @1 | @2
  Recieved value:
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
  Decode error in field: <input> -> string_enum
  Expected type:
  @"a" | @"b"
  Recieved value:
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
  Decode error in field: <input> -> tuple
  Expected type:
  (string, string)
  Recieved value:
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
  Decode error in field: <input> -> nested -> level_1 -> 1
  Expected type:
  string
  Recieved value:
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
  Decode error.
  An object is missing the field: another_loooong_field
  In field: <input> -> long_type


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
  Decode error in field: <input> -> long_type
  Expected type:
  {@tag: false} |
  {
    @tag: true,
    another_loooong_field: string,
    looong_field: string,
    yet_another_field: string
  }
  Recieved value:
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
  Decode error in field: <input> -> list
  Expected type:
  [{a: string}]
  Recieved value:
  fail


  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({})
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  Decode error.
  An object is missing the field: bool
  In field: <input>
