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

function getComponents(result, filePath) {
  return fs.stat(filePath).then((stats) => {
    if (stats.isDirectory()) {
      return fs
        .readdir(filePath)
        .then((files) =>
          Promise.all(
            files.map((file) =>
              getComponents(result, path.join(filePath, file))
            )
          )
        );
    } else if (filePath.toLowerCase().endsWith(".acutis")) {
      return fs.readFile(filePath).then((src) => {
        const name = Utils.filenameToComponent(filePath);
        result.push(Component.uint8Array(name, src));
      });
    } else {
      return;
    }
  });
}

function acutisErrorToJsError(e) {
  if (Utils.isError(e)) {
    throw Error(Utils.getError(e));
  } else {
    throw e;
  }
}

module.exports = function (eleventyConfig, config) {
  eleventyConfig.versionCheck(">= 1.0");
  let components = Compile.components([]);
  // Caching templates makes projects that heavily use "layout" templates
  // measurably faster.
  let cache = new Map();
  eleventyConfig.addTemplateFormats("acutis");
  eleventyConfig.addExtension("acutis", {
    read: false,
    init: function () {
      cache.clear();
      const result =
        config && "components" in config ? config.components.slice() : [];
      const inc = path.join(this.config.dir.input, this.config.dir.includes);
      return getComponents(result, inc)
        .then(() => {
          components = Compile.components(result);
        })
        .catch(acutisErrorToJsError);
    },
    compile: function (str, inputPath) {
      // since `read: false` is set, 11ty doesn't read file contents
      // so if str has a value, it's a permalink
      // (which can be a string or a function)
      if (str) {
        return function (data) {
          return typeof str === "function" ? str(data) : str;
        };
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
