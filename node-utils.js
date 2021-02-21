/**
 *    Copyright 2021 John Jackson
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
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
