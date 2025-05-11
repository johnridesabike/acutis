/**
 * Copyright (c) 2022 John Jackson.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

/**
 * This exports three implementations of the plugin: one that runs templates
 * purely in memory, one that prints templates into CommonJS modules, and one
 * that prints templates into ECMAScript modules. After Eleventy finishes
 * building, all versions should produce the same output with the same input.
 * However, they each interact with Eleventy's build system differently.
 *
 * This API is unstable and not rigorously tested.
 */

import path from "node:path";
import fs from "node:fs/promises";
import acutis from "#main";
let { Compile, Component, Utils } = acutis;

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
                  : this._walk(path.join(filePath, file), f),
              ),
            ),
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

function acutisErrorToJsError(e) {
  if (Utils.isError(e)) {
    console.error(Utils.getError(e) + "\n");
    throw Error(
      "Error compiling an Acutis file. See the full error message above.",
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
 * Build pages using the pure render implementation. This evaluates source files
 * in memory.
 */
export function plugin(eleventyConfig, config) {
  eleventyConfig.versionCheck(">= 3.0");
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
      let result = [];
      let dir = this.config.dir;
      cache.clear();
      return new Promise((resolve) => {
        if (config && "components" in config) {
          getFuncs(config.components).forEach(({ key, f }) =>
            result.push(Component.func(key, f.interface, f)),
          );
        }
        resolve();
      })
        .then(() =>
          new FileWalker(path.join(dir.input, dir.includes)).walk(
            (_stats, filePath, src) =>
              result.push(Component.uint8Array(filePath, src)),
          ),
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
          // Compile exceptions are raised synchronously and runtime exceptions
          // are raised asynchronously. To catch them all, we wrap compilation
          // in a promise.
          return new Promise((resolve) => {
            let template = Compile.string(inputPath, components, str);
            resolve((data) =>
              Compile.render(template, data).catch(acutisErrorToJsError),
            );
          }).catch(acutisErrorToJsError);
        }
      } else {
        let template = cache.get(inputPath);
        if (!template) {
          template = fs
            .readFile(inputPath)
            .then((src) => {
              let template = Compile.uint8Array(inputPath, components, src);
              return (data) =>
                Compile.render(template, data).catch(acutisErrorToJsError);
            })
            .catch(acutisErrorToJsError);
          cache.set(inputPath, template);
        }
        return template;
      }
    },
  });
}

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

function printJs(printer, eleventyConfig, config) {
  eleventyConfig.versionCheck(">= 3.0");
  let extension = config && "extension" in config ? config.extension : ".js";
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
                    Component.funcPath(relativeCompPath, key, f.interface),
                  )
                  .concat(compSrc),
              );
              let template = Compile.uint8Array(filePath, components, src);
              let js = printer(template);
              return fs.writeFile(filePath + extension, js);
            } else {
              return Promise.resolve();
            }
          }),
      )
      .then(() => oracle.reset())
      .catch(acutisErrorToJsError);
  });
  eleventyConfig.addExtension("acutis" + extension, { key: "11ty.js" });
  eleventyConfig.addTemplateFormats("acutis" + extension);
  eleventyConfig.addWatchTarget("**/*.acutis");
}

/**
 * Build pages with the compile-to-JavaScript implementation. This prints source
 * files into JavaScript files using ECMAScript module format. Eleventy then
 * evaluates them to render the final pages. The compiled filenames end in
 * `.acutis.js` but Eleventy treats them like `.11ty.js` templates.
 */
export function printESM(eleventyConfig, config) {
  return printJs(Compile.toESMString, eleventyConfig, config);
}

/**
 * This is the same as `printESM` except it prints using CommonJS module format.
 */
export function printCJS(eleventyConfig, config) {
  return printJs(Compile.toCJSString, eleventyConfig, config);
}
