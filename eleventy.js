/**
 * Copyright (c) 2022 John Jackson.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

let path = require("path");
let fs = require("fs/promises");
let { Compile, Component, Render, Utils } = require(".");

function FileWalker(root) {
  this.ignores = new Set();
  this.ignores.add(".git");
  this.ignoresGlobal = new Set();
  this.ignoresGlobal.add("node_modules");
  this.root = root;
}

FileWalker.prototype.ignore = function (filePath) {
  this.ignores.add(filePath);
  return this;
};

FileWalker.prototype._walk = function (filePath, f) {
  if (this.ignores.has(filePath)) {
    return Promise.resolve();
  } else {
    return fs.stat(filePath).then((stats) => {
      if (stats.isDirectory()) {
        return fs
          .readdir(filePath)
          .then((files) =>
            Promise.all(
              files.map((file) =>
                this.ignoresGlobal.has(file)
                  ? Promise.resolve()
                  : this._walk(path.join(filePath, file), f)
              )
            )
          );
      } else if (filePath.toLowerCase().endsWith(".acutis")) {
        return fs.readFile(filePath).then((src) => f(stats, filePath, src));
      } else {
        return Promise.resolve();
      }
    });
  }
};

FileWalker.prototype.walk = function (f) {
  return this._walk(this.root, f);
};

let defaultIgnores = new Set([".git"]);
let globalIgnores = new Set(["node_modules"]);

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
              files.map((file) =>
                globalIgnores.has(file)
                  ? Promise.resolve()
                  : forEachFiles(ignores, path.join(filePath, file), f)
              )
            )
          );
      } else if (filePath.toLowerCase().endsWith(".acutis")) {
        return fs.readFile(filePath).then((src) => f(stats, filePath, src));
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
  let result = [];
  for (let key in obj) {
    let f = obj[key];
    if (typeof f === "function" && "interface" in f) {
      result.push({ key, f });
    }
  }
  return result;
}

/**
 * 1. The interpreted implementation.
 */

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
      let result =
        config && "components" in config
          ? getFuncs(config.components).map(({ key, f }) =>
              Component.funAsync(key, f.interface, f)
            )
          : [];
      let dir = this.config.dir;
      return new FileWalker(path.join(dir.input, dir.includes))
        .walk((_stats, filePath, src) =>
          result.push(Component.uint8Array(filePath, src))
        )
        .then(() => (components = Compile.components(result)))
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
              let template = Compile.string(inputPath, components, src);
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
              let template = Compile.uint8Array(inputPath, components, src);
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

/**
 * 2. The compile-to-JS implementation.
 */

// When building to JS files, we have to bypass Eleventy's own build system and
// so we need to use our own. This is a very rudimentary script but it should
// get the job done.
//
// Instead of creating a regular custom language extension, we output JS files
// that use Eleventy's own JS template engine. If we aren't careful, this can
// cause an infinite recompiliation loop in watch mode, which is why it's
// important to track which files have changed.

// We must store stats from each previous build so we know which templates we
// need to rebuild.

function RebuildOracle() {
  this.shouldBuild = true;
  this.prevConfig = null;
  this.prevComponents = new Map();
  this.prevTemplates = new Map();
  this.currConfig = null;
  this.currComponents = new Map();
  this.currTemplates = new Map();
}

RebuildOracle.prototype.reset = function () {
  this.shouldBuild = false;
  this.prevConfig = this.currConfig;
  this.prevComponents = this.currComponents;
  this.prevTemplates = this.currTemplates;
  this.currConfig = null;
  this.currComponents = new Map();
  this.currTemplates = new Map();
};

RebuildOracle.prototype.addConfig = function (config) {
  this.currConfig = config;
  if (!this.shouldBuild && config && this.prevConfig) {
    this.shouldBuild =
      config.componentsPath !== this.prevConfig.componentsPath ||
      config.components !== this.prevConfig.components;
  }
};

RebuildOracle.prototype.addComponent = function (filePath, mtime) {
  this.currComponents.set(filePath, mtime);
  if (!this.shouldBuild) {
    let prevMtime = this.prevComponents.get(filePath);
    this.shouldBuild = prevMtime ? mtime > prevMtime : true;
  }
};

RebuildOracle.prototype.addTemplate = function (filePath, mtime) {
  this.currTemplates.set(filePath, mtime);
  if (this.shouldBuild) {
    return true;
  } else {
    let prevMtime = this.prevTemplates.get(filePath);
    return prevMtime ? mtime > prevMtime : true;
  }
};

let oracle = new RebuildOracle();

// Before each Eleventy build, we need to rebuild any .acutis files which
// changed.

module.exports.printJs = function (eleventyConfig, config) {
  eleventyConfig.versionCheck(">= 2.0");
  eleventyConfig.on("eleventy.before", ({ dir }) => {
    oracle.addConfig(config);
    let compPath =
      config && "componentsPath" in config ? config.componentsPath : "";
    let compFuns =
      config && "components" in config ? getFuncs(config.components) : {};
    let compSrc = [];
    let dirIncludes = path.join(dir.input, dir.includes);
    return new FileWalker(dirIncludes)
      .walk((stats, filePath, src) => {
        oracle.addComponent(filePath, stats.mtimeMs);
        compSrc.push(Component.uint8Array(filePath, src));
      })
      .then(() =>
        new FileWalker(dir.input)
          .ignore(dirIncludes)
          .ignore(path.join(dir.input, dir.data))
          .ignore(dir.output) // Not relative to input
          .walk((stats, filePath, src) => {
            let shouldBuild = oracle.addTemplate(filePath, stats.mtimeMs);
            if (shouldBuild) {
              let relativeCompPath =
                "." +
                path.sep +
                path.relative(path.dirname(filePath), compPath);
              let components = Compile.components(
                compFuns
                  .map(({ key, f }) =>
                    Component.funPath(relativeCompPath, key, f.interface)
                  )
                  .concat(compSrc)
              );
              let template = Compile.uint8Array(filePath, components, src);
              let js = Compile.toCJSString(template);
              return fs.writeFile(filePath + ".js", js);
            } else {
              return Promise.resolve();
            }
          })
      )
      .then(() => oracle.reset())
      .catch(acutisErrorToJsError);
  });
  eleventyConfig.addExtension("acutis.js", { key: "11ty.js" });
  eleventyConfig.addWatchTarget("**/*.acutis");
};
