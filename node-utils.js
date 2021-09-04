/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

const util = require("util");
const fs = require("fs");
const path = require("path");
const Acutis = require("./lib/js/src/AcutisJs");

const readFile = util.promisify(fs.readFile);

function filenameToComponent(file) {
  const basename = path.basename(file, path.extname(file));
  const firstChar = basename.charAt(0).toUpperCase();
  const rest = basename.slice(1);
  return firstChar + rest;
}

async function loadTemplate(fileName) {
  const filePath = path.resolve(process.cwd(), fileName);
  switch (path.extname(fileName)) {
    case ".js":
    case ".mjs":
    case ".cjs":
      const jsmodule = await import(filePath);
      return jsmodule.default;
    default:
      const name = filenameToComponent(filePath);
      const src = await readFile(filePath, "utf-8");
      return Acutis.Source.string(name, src);
  }
}

module.exports = { loadTemplate, filenameToComponent };
