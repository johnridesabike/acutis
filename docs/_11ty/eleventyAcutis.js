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
const { Compile, Environment } = require("../../lib/js/src/AcutisJs");
const { filenameToComponent } = require("../../node-utils");

const readFile = util.promisify(fs.readFile);

const cache = new Map();

module.exports = (eleventyConfig, config) => {
  let env = Environment.Async.make({});
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
      const components = {};
      return fastGlob(filesGlob)
        .then((files) =>
          Promise.all(
            files.map((fileName) =>
              readFile(fileName, "utf-8")
                .then((src) => {
                  if (!cache.has(src)) {
                    cache.set(src, Compile.make(src, fileName));
                  }
                  components[filenameToComponent(fileName)] = cache.get(src);
                })
                .catch((e) => console.error(e.message))
            )
          )
        )
        .then(() => {
          env = Environment.Async.make({ ...components, ...config.components });
        });
    },
    compile: (src, inputPath) => (props) => {
      if (!cache.has(src)) {
        cache.set(src, Compile.make(src, inputPath));
      }
      return cache
        .get(src)(env, props, {})
        .then(({ NAME, VAL }) => {
          if (NAME === "errors") {
            console.error(VAL);
            throw new Error(`Error with ${props.permalink}`);
          } else {
            return VAL;
          }
        });
    },
  });
};
