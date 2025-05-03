import fs from "node:fs/promises";
import acutis from "#main";
let { Compile, Component, Typescheme } = acutis;

let filepath = process.argv[2];
let Ty = Typescheme;

let components = Compile.components([
  Component.func(
    "Slow",
    Ty.make([["children", Ty.string()]]),
    ({ children }) =>
      new Promise((resolve) => {
        setTimeout(() => resolve(children), 50);
      }),
  ),
  Component.func(
    "Slower",
    Ty.make([["children", Ty.string()]]),
    ({ children }) =>
      new Promise((resolve) => {
        setTimeout(() => resolve(children), 500);
      }),
  ),
  Component.func(
    "Sync",
    Ty.make([]),
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
