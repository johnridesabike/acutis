/**
 * Copyright (c) 2022 John Jackson.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

const path = require("path");
const fs = require("fs/promises");
const { Compile, Component, Render, Utils } = require(".");

const default_ignores = new Set([".git"]);
const global_ignores = new Set(["node_modules"]);

function forEachFiles(ignores, filePath, f) {
  if (ignores.has(filePath)) {
    return Promise.resolve();
  } else {
    return fs.stat(filePath).then((stats) => {
      if (stats.isDirectory()) {
        return fs
          .readdir(filePath)
          .then((files) =>
            Promise.all(
              files
                .filter((file) => !global_ignores.has(file))
                .map((file) =>
                  forEachFiles(ignores, path.join(filePath, file), f)
                )
            )
          );
      } else if (filePath.toLowerCase().endsWith(".acutis")) {
        return fs.readFile(filePath).then((src) => f(filePath, src));
      } else {
        return Promise.resolve();
      }
    });
  }
}

function acutisErrorToJsError(e) {
  if (Utils.isError(e)) {
    console.error(Utils.getError(e) + "\n");
    throw Error(
      "Error compiling an Acutis file. See the full error message above."
    );
  } else {
    throw e;
  }
}

function getFuncs(obj) {
  const result = [];
  for (const key in obj) {
    const f = obj[key];
    if (typeof f === "function" && "interface" in f) {
      result.push({ key, f });
    }
  }
  return result;
}

module.exports = function (eleventyConfig, config) {
  eleventyConfig.versionCheck(">= 1.0");
  let components = Compile.components([]);
  // Caching templates makes projects that heavily use "layout" templates
  // measurably faster.
  let cache = new Map();
  eleventyConfig.addTemplateFormats("acutis");
  eleventyConfig.addExtension("acutis", {
    // Because we pre-compile our components, 11ty's built-in file reading won't
    // reload all changes in watch mode.
    read: false,
    init: function () {
      cache.clear();
      const result =
        config && "components" in config
          ? getFuncs(config.components).map(({ key, f }) =>
              Component.funAsync(key, f.interface, f)
            )
          : [];
      const ignores = default_ignores;
      const dir = this.config.dir;
      function f(filePath, src) {
        result.push(Component.uint8Array(filePath, src));
      }
      return Promise.all([
        forEachFiles(ignores, path.join(dir.input, dir.includes), f),
        // Contrary to 11ty convention, this makes layouts available as regular
        // includes. But this doesn't seem harmful, and it can be useful.
        dir.layouts
          ? forEachFiles(ignores, path.join(dir.input, dir.layouts), f)
          : Promise.resolve(),
      ])
        .then(() => {
          components = Compile.components(result);
        })
        .catch(acutisErrorToJsError);
    },
    compile: function (str, inputPath) {
      // since `read: false` is set, 11ty doesn't read Acutis template files.
      // If str has a value, it's either a permalink or markdown content.
      // Permalinks can either be a string or a function.
      if (str) {
        if (typeof str === "function") {
          return Promise.resolve(str);
        } else {
          return Promise.resolve(str)
            .then((src) => {
              const template = Compile.string(inputPath, components, src);
              return function (data) {
                return Render.async(template, data).catch(acutisErrorToJsError);
              };
            })
            .catch(acutisErrorToJsError);
        }
      } else {
        let template = cache.get(inputPath);
        if (!template) {
          template = fs
            .readFile(inputPath)
            .then((src) => {
              const template = Compile.uint8Array(inputPath, components, src);
              return function (data) {
                return Render.async(template, data).catch(acutisErrorToJsError);
              };
            })
            .catch(acutisErrorToJsError);
          cache.set(inputPath, template);
        }
        return template;
      }
    },
  });
};

module.exports.toJs = function (eleventyConfig, config) {
  eleventyConfig.versionCheck(">= 1.0");
  eleventyConfig.on("eleventy.before", ({ dir }) => {
    const components_funs =
      config && "components" in config
        ? getFuncs(require(path.join(process.cwd(), config.components)))
        : [];
    const components_module =
      config && "components" in config ? config.components : "";
    const components = [];
    const input_ignores = new Set(default_ignores);
    input_ignores.add(dir.includes);
    input_ignores.add(dir.data);
    input_ignores.add(dir.output);
    const includes = path.join(dir.input, dir.includes);
    function f(filePath, src) {
      components.push(Component.uint8Array(filePath, src));
    }
    return forEachFiles(default_ignores, includes, f)
      .then(() => {
        return forEachFiles(input_ignores, dir.input, (filePath, src) => {
          const funsPath =
            "." +
            path.sep +
            path.relative(path.dirname(filePath), components_module);
          const components_compiled = Compile.components(
            components_funs
              .map(({ key, f }) =>
                Component.funToJs(funsPath, key, f.interface)
              )
              .concat(components)
          );
          const template = Compile.uint8Array(
            filePath,
            components_compiled,
            src
          );
          const js = Compile.toStringCjs(template);
          return fs.writeFile(filePath + ".js", js);
        });
      })
      .catch((e) => console.log(acutisErrorToJsError(e)));
  });
  eleventyConfig.addExtension("acutis.js", { key: "11ty.js" });
};
