  $ acutis --mode js template.acutis > compiled.mjs

  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({
  >     bool: true,
  >     int_enum: 1,
  >     string_enum: "a",
  >     tuple: ["a", "b"],
  >     nested: {level_1: ["a", "b"]},
  >   })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  Decode error.
  Expected type:
    false
  Recieved value:
    true
  In field: bool

  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({
  >     bool: false,
  >     int_enum: 3,
  >     string_enum: "a",
  >     tuple: ["a", "b"],
  >     nested: {level_1: ["a", "b"]},
  >   })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  Decode error.
  Expected type:
    @1 | @2
  Recieved value:
    3
  In field: int_enum

  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({
  >     bool: false,
  >     int_enum: 1,
  >     string_enum: "c",
  >     tuple: ["a", "b"],
  >     nested: {level_1: ["a", "b"]},
  >   })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  Decode error.
  Expected type:
    @"a" | @"b"
  Recieved value:
    c
  In field: string_enum

  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({
  >     bool: false,
  >     int_enum: 1,
  >     string_enum: "a",
  >     tuple: ["a"],
  >     nested: {level_1: ["a", "b"]},
  >     })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  Decode error.
  Expected type:
    (string, string)
  Recieved value:
    a
  In field: tuple

  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({
  >     bool: false,
  >     int_enum: 1,
  >     string_enum: "a",
  >     tuple: ["a"],
  >     nested: {level_1: ["a", 1]},
  >     })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  Decode error.
  Expected type:
    string
  Recieved value:
    1
  In field: nested -> level_1 -> 1
