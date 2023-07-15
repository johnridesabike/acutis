const fs = require("fs/promises");
const { Compile, Component, Render, Typescheme } = require("../../");
const data = require("./data.json");

const filepath = process.argv[2];
const Ty = Typescheme;

const components = Compile.components([
  Component.funSync(
    "Comp",
    Ty.make([
      [
        "dict",
        Ty.record([
          ["a", Ty.int()],
          ["b", Ty.string()],
        ]),
      ],
      ["arr", Ty.list(Ty.string())],
      ["arrEmpty", Ty.list(Ty.string())],
      ["some", Ty.nullable(Ty.string())],
      ["none", Ty.nullable(Ty.string())],
      ["t", Ty.boolean()],
      ["f", Ty.boolean()],
    ]),
    (props) => JSON.stringify(props, null, 2)
  ),
  Component.funSync(
    "UnknownComp",
    Ty.make([
      ["dict", Ty.unknown()],
      ["arr", Ty.unknown()],
      ["arrEmpty", Ty.unknown()],
      ["some", Ty.unknown()],
      ["none", Ty.unknown()],
      ["t", Ty.unknown()],
      ["f", Ty.unknown()],
    ]),
    (props) => JSON.stringify(props, null, 2)
  ),
]);

fs.readFile(filepath)
  .then((src) => {
    const template = Compile.uint8Array(filepath, components, src);
    const result = Render.sync(template, data);
    process.stdout.write(result);
  })
  .catch(console.error);
