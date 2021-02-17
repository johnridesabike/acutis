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

module.exports = (eleventyConfig, config) => {
  let env = Environment.Async.make(Compile.emptyMap);
  eleventyConfig.addTemplateFormats("acutis");
  eleventyConfig.addExtension("acutis", {
    read: true,
    data: true,
    init: function () {
      const filesGlob = path.join(
        this.config.inputDir,
        this.config.dir.includes,
        "**/*.acutis"
      );
      return fastGlob(filesGlob)
        .then((files) =>
          Promise.all(
            files.map((fileName) =>
              readFile(fileName, "utf-8").then((src) => {
                const name = filenameToComponent(fileName);
                return Source.string(name, src);
              })
            )
          )
        )
        .then((queue) => {
          const comps = Result.getOr(
            Compile.fromArray([...queue, ...config.components]),
            (e) => {
              console.error(e);
              throw new Error("Couldn't compile includes.");
            }
          );
          env = Environment.Async.make(comps);
        });
    },
    compile: (src, inputPath) => (props) => {
      const getOrThrow = (result) =>
        Result.getOr(result, (e) => {
          console.error(e);
          throw new Error(`Error with ${props.permalink}`);
        });
      const Template = getOrThrow(Compile.make(Source.string(inputPath, src)));
      return Template(env, props, {}).then(getOrThrow);
    },
  });
};
