import fs from "node:fs/promises";
import acutis from "#main";
let { Compile, Component, Utils } = acutis;
let filepath = process.argv[2];
try {
  let [dataSrc, src] = await Promise.all([
    fs.readFile("data.json"),
    fs.readFile(filepath),
  ]);
  let components = Compile.components([
    Component.func(
      "Comp",
      {
        dict: { a: "int", b: "string" },
        arr: ["list", "string"],
        arrEmpty: ["list", "string"],
        some: ["nullable", "string"],
        none: ["nullable", "string"],
        t: ["enum", false, true],
        f: ["enum", false, true],
        unknown: "_",
      },
      (props) => JSON.stringify(props, null, 2),
    ),
    Component.func(
      "UnknownComp",
      {
        // The marshaled representation of these values is not deterministic.
        // Uncomment to test them but don't commit the output.
        /*
        dict: "_",
        arr: "_",
        arrEmpty: "_",
        some: "_",
        none: "_",
        t: "_",
        f: "_",
        */
        unknown: "_",
      },
      (props) => JSON.stringify(props, null, 2),
    ),
  ]);
  let data = JSON.parse(dataSrc);
  let template = Compile.uint8Array(filepath, components, src);
  let result = await Compile.render(template, data);
  process.stdout.write(result);
} catch (e) {
  if (Utils.isError(e)) {
    console.error(Utils.getError(e));
  } else {
    console.error(e);
  }
}
