import fs from "node:fs/promises";
import acutis from "#main";
let filepath = process.argv[2];
let [dataSrc, src] = await Promise.all([
  fs.readFile("data.json"),
  fs.readFile(filepath),
]);
let result = acutis
  .Acutis({})
  .createRender()
  .addFunc(
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
  )
  .addFunc(
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
  )
  .compileUint8Array(filepath, src)
  .apply(JSON.parse(dataSrc));
process.stdout.write(await result);
