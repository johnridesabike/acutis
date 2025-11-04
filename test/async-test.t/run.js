import fs from "node:fs/promises";
import acutis from "#main";
let filepath = process.argv[2];
let src = await fs.readFile(filepath);
let result = acutis
  .Acutis({})
  .createRender()
  .addFunc(
    "Slow",
    { children: "string" },
    ({ children }) =>
      new Promise((resolve) => {
        setTimeout(() => resolve(children), 50);
      }),
  )
  .addFunc(
    "Slower",
    { children: "string" },
    ({ children }) =>
      new Promise((resolve) => {
        setTimeout(() => resolve(children), 500);
      }),
  )
  .addFunc(
    "Sync",
    {},
    () => "JavaScript components may return promises or strings",
  )
  .compileUint8Array(filepath, src)
  .apply({});
process.stdout.write(await result);
