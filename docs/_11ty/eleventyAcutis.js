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

const path = require("path");
const fs = require("fs");
const util = require("util");
const fastGlob = require("fast-glob");
const { Compile, Environment, Result, Source } = require("../../");
const { filenameToComponent } = require("../../node-utils");

const readFile = util.promisify(fs.readFile);

function onComponentsError(e) {
  console.error(e);
  throw new Error(
    "I couldn't compile Acutis components due to the previous errors."
  );
}

module.exports = function (eleventyConfig, config) {
  const env = Environment.async;
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
          return Source.string(name, str);
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
      return async function (data) {
        const src = Source.string(inputPath, str);
        const template = Result.getOrElse(
          Compile.make(src, components),
          onError
        );
        const result = await template(env, data, {});
        return Result.getOrElse(result, onError);
      };
    },
  });
};
