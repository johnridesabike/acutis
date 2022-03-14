/**
  Copyright (c) 2021 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

const path = require("path");
const fs = require("fs");
const util = require("util");
const fastGlob = require("fast-glob");
const { Compile, Render, Result, Source } = require("../../");
const { filenameToComponent } = require("../../node-utils");

const readFile = util.promisify(fs.readFile);

function onComponentsError(e) {
  console.error(e);
  throw new Error(
    "I couldn't compile Acutis components due to the previous errors."
  );
}

module.exports = function (eleventyConfig, config) {
  let components = Compile.Components.empty();
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
          return Source.src(name, str);
        })
      );
      const componentsResult = Compile.Components.make([
        ...queue,
        ...config.components,
      ]);
      components = Result.getOrElse(componentsResult, onComponentsError);
    },
    compile: function (str, inputPath) {
      function onError(e) {
        console.error(e);
        throw new Error(
          `I couldn't render ${inputPath} due to the previous errors.`
        );
      }
      const template = Result.getOrElse(
        Compile.make(inputPath, str, components),
        onError
      );
      return async function (data) {
        const result = await Render.async(template, data);
        return Result.getOrElse(result, onError);
      };
    },
  });
};
