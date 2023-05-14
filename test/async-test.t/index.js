const fs = require("fs/promises");
const { Compile, Component, Render, Typescheme } = require("../../");
const data = {};

const filepath = process.argv[2];
const Ty = Typescheme;

const components = Compile.components([
  Component.funAsync(
    "Slow",
    Ty.make([["children", Ty.string()]]),
    ({ children }) =>
      new Promise((resolve) => {
        setTimeout(() => resolve(children), 50);
      })
  ),
  Component.funAsync(
    "Slower",
    Ty.make([["children", Ty.string()]]),
    ({ children }) =>
      new Promise((resolve) => {
        setTimeout(() => resolve(children), 500);
      })
  ),
]);

fs.readFile(filepath)
  .then((src) => {
    const template = Compile.uint8Array(filepath, components, src);
    return Render.async(template, data);
  })
  .then((result) => process.stdout.write(result))
  .catch(console.error);
