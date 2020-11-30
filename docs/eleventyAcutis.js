/**
 *    Copyright 2020 John Jackson
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

const fastGlob = require("fast-glob");
const { compile, renderContext, errorMessage } = require("../");
const { loadTemplate, filenameToComponent } = require("../node-utils");

module.exports = (eleventyConfig) => {
  // Remove stale cache.
  eleventyConfig.on("beforeWatch", (files) =>
    files.forEach((file) => {
      if (file.endsWith(".js")) {
        delete require.cache[require.resolve(file)];
      }
    })
  );
  const templates = {};
  eleventyConfig.addTemplateFormats("acutis");
  eleventyConfig.addExtension("acutis", {
    read: true,
    data: true,
    init: async () =>
      Promise.all(
        (await fastGlob("./_includes/**/*.(js|mjs|acutis)")).map(
          async (file) =>
            (templates[filenameToComponent(file)] = await loadTemplate(file))
        )
      ),
    compile: (str, inputPath) => (data) => {
      let result = "";
      try {
        const template = compile(str, inputPath);
        const render = renderContext(templates);
        result = template(render, data, {});
      } catch (e) {
        console.error(errorMessage(e));
      }
      return result;
    },
  });
};
