import fs from "node:fs/promises";
import acutis from "#main";
let { Compile, Component } = acutis;
let filepath = process.argv[2];

let components = Compile.components([
  Component.func(
    "Slow",
    { children: "string" },
    ({ children }) =>
      new Promise((resolve) => {
        setTimeout(() => resolve(children), 50);
      }),
  ).result,
  Component.func(
    "Slower",
    { children: "string" },
    ({ children }) =>
      new Promise((resolve) => {
        setTimeout(() => resolve(children), 500);
      }),
  ).result,
  Component.func(
    "Sync",
    {},
    () => "JavaScript components may return promises or strings",
  ).result,
]);
let src = await fs.readFile(filepath);
let template = Compile.uint8Array(filepath, components.result, src);
let result = await Compile.render(template.result, {});
process.stdout.write(result.result);
