import fs from "node:fs/promises";
import acutis from "#main";
let { Compile, Component, Utils } = acutis;
let filepath = process.argv[2];
try {
  let components = Compile.components([
    Component.func(
      "Slow",
      { children: "string" },
      ({ children }) =>
        new Promise((resolve) => {
          setTimeout(() => resolve(children), 50);
        }),
    ),
    Component.func(
      "Slower",
      { children: "string" },
      ({ children }) =>
        new Promise((resolve) => {
          setTimeout(() => resolve(children), 500);
        }),
    ),
    Component.func(
      "Sync",
      {},
      () => "JavaScript components may return promises or strings",
    ),
  ]);
  let src = await fs.readFile(filepath);
  let template = Compile.uint8Array(filepath, components, src);
  let result = await Compile.render(template, {});
  process.stdout.write(result);
} catch (e) {
  if (Utils.isError(e)) {
    console.error(Utils.getError(e));
  } else {
    console.error(e);
  }
}
