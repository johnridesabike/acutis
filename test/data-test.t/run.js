import fs from "node:fs/promises";
import acutis from "#main";
let { Compile, Component, Render, Typescheme, Utils } = acutis;

let filepath = process.argv[2];
let Ty = Typescheme;

let components = Compile.components([
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
      ["unknown", Ty.unknown()],
    ]),
    (props) => JSON.stringify(props, null, 2),
  ),
  Component.funSync(
    "UnknownComp",
    Ty.make([
      // The marshaled representation of these values is not deterministic.
      // Uncomment to test them but don't commit the output.
      /*
      ["dict", Ty.unknown()],
      ["arr", Ty.unknown()],
      ["arrEmpty", Ty.unknown()],
      ["some", Ty.unknown()],
      ["none", Ty.unknown()],
      ["t", Ty.unknown()],
      ["f", Ty.unknown()],
      */
      ["unknown", Ty.unknown()],
    ]),
    (props) => JSON.stringify(props, null, 2),
  ),
]);

Promise.all([fs.readFile("data.json"), fs.readFile(filepath)])
  .then(([dataSrc, src]) => {
    let data = JSON.parse(dataSrc);
    let template = Compile.uint8Array(filepath, components, src);
    let result = Render.sync(template, data);
    process.stdout.write(result);
  })
  .catch((e) => {
    if (Utils.isError(e)) {
      console.error(Utils.getError(e));
    } else {
      console.error(e);
    }
  });
