/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

const path = require("path");
const fs = require("fs");
const util = require("util");
const fastGlob = require("fast-glob");
const {
  Compile,
  Render,
  Utils,
} = require("../../ocaml/acutis/_build/default/bin/main.bc.js");
const { filenameToComponent } = require("../../node-utils");

const readFile = util.promisify(fs.readFile);

module.exports = function (eleventyConfig, config) {
  let components = Compile.components([]);
  eleventyConfig.addTemplateFormats("acutis");
  eleventyConfig.addExtension("acutis", {
    read: true,
    data: true,
    init: async function () {
      const glob = path.join(
        this.config.inputDir,
        this.config.dir.includes,
        "**/*.acutis"
      );
      const files = await fastGlob(glob);
      const queue = await Promise.all(
        files.map(async (fileName) => {
          const str = await readFile(fileName, "utf-8");
          const name = filenameToComponent(fileName);
          return Compile.src(name, str);
        })
      );
      try {
        components = Compile.components([...queue, ...config.components]);
      } catch (e) {
        if (Utils.isError(e)) {
          Utils.logError(e);
        } else {
          throw e;
        }
      }
    },
    compile: function (str, inputPath) {
      try {
        const template = Compile.make(inputPath, components, str);
        return async function (data) {
          return Render.async(template, data);
        };
      } catch (e) {
        if (Utils.isError(e)) {
          Utils.logError(e);
          return function (data) {
            return "error";
          };
        } else {
          console.log(e);
          throw e;
        }
      }
    },
  });
};
