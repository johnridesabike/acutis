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

fs.readFile(filepath)
  .then((src) => {
    let template = Compile.uint8Array(filepath, components, src);
    return Compile.render(template, {});
  })
  .then((result) => process.stdout.write(result))
  .catch(console.error);
