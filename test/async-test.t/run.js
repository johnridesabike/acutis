import fs from "node:fs/promises";
import acutis from "#main";
let { Compile, Component, Render, Typescheme } = acutis;

let filepath = process.argv[2];
let Ty = Typescheme;

let components = Compile.components([
  Component.funAsync(
    "Slow",
    Ty.make([["children", Ty.string()]]),
    ({ children }) =>
      new Promise((resolve) => {
        setTimeout(() => resolve(children), 50);
      }),
  ),
  Component.funAsync(
    "Slower",
    Ty.make([["children", Ty.string()]]),
    ({ children }) =>
      new Promise((resolve) => {
        setTimeout(() => resolve(children), 500);
      }),
  ),
]);

fs.readFile(filepath)
  .then((src) => {
    let template = Compile.uint8Array(filepath, components, src);
    return Render.async(template, {});
  })
  .then((result) => process.stdout.write(result))
  .catch(console.error);
