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
const { Compile, Render, Utils } = require("../../");

const readFile = util.promisify(fs.readFile);

/*
 Due to the way JSOO modifies Node.JS's default error handling, in addition to
 the incompatibilities between OCaml/JSOO exceptions and JS exceptions, we
 avoid throwing errors that come from the OCaml side.

 We continue to raise any exceptions that come from Eleventy, since Eleventy
 will handle those itself.
*/

module.exports = function (eleventyConfig, config) {
  let components = Compile.components([]);
  eleventyConfig.addTemplateFormats("acutis");
  eleventyConfig.addExtension("acutis", {
    read: true,
    data: true,
    init: function () {
      const glob = path.join(
        this.config.inputDir,
        this.config.dir.includes,
        "**/*.acutis"
      );
      return fastGlob(glob)
        .then((files) => {
          const filesp = files.map((fileName) =>
            readFile(fileName, "utf-8").then((src) => {
              const name = Utils.filenameToComponent(fileName);
              return Compile.fromString(name, src);
            })
          );
          return Promise.all(filesp);
        })
        .then((arr) => {
          components = Compile.components([...arr, ...config.components]);
        })
        .catch((e) => {
          if (Utils.isError(e)) {
            console.error(Utils.getError(e));
          } else {
            throw e;
          }
        });
    },
    compile: function (str, inputPath) {
      try {
        const template = Compile.make(inputPath, components, str);
        return function (data) {
          return Render.async(template, data).catch((e) => {
            if (Utils.isError(e)) {
              console.error(Utils.getError(e));
              return "error";
            } else {
              throw e;
            }
          });
        };
      } catch (e) {
        if (Utils.isError(e)) {
          console.error(Utils.getError(e));
          return function (_data) {
            return "error";
          };
        } else {
          throw e;
        }
      }
    },
  });
};
