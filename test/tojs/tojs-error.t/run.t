  $ acutis --mode js template.acutis > compiled.mjs

  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({ bool: true, int_enum: 1, string_enum: "a", tuple: ["a", "b"] })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  This field must be a boolean.

  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({
  >     bool: false,
  >     int_enum: 3,
  >     string_enum: "a",
  >     tuple: ["a", "b"],
  >   })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  This field must be an int enum.

  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({
  >     bool: false,
  >     int_enum: 1,
  >     string_enum: "c",
  >     tuple: ["a", "b"],
  >   })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  This field must be a string enum.

  $ cat > run.mjs << EOF
  >   import main from "./compiled.mjs";
  >   main({ bool: false, int_enum: 1, string_enum: "a", tuple: ["a"] })
  >   .then(console.log)
  >   .catch((e) => console.error(e.message));
  > EOF
  $ node run.mjs
  This field must be an array.
