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
let compiler = acutis.Acutis({});

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

FileWalker.prototype._walk = async function (filePath, f) {
  if (this.ignores.has(filePath)) {
    return;
  } else {
    let stats = await fs.stat(filePath);
    if (stats.isDirectory()) {
      let files = await fs.readdir(filePath);
      return Promise.all(
        files.map((file) =>
          this.ignoresGlobal.has(file)
            ? Promise.resolve()
            : this._walk(path.join(filePath, file), f),
        ),
      );
    } else if (filePath.toLowerCase().endsWith(".acutis")) {
      let src = await fs.readFile(filePath);
      return f(stats, filePath, src);
    } else {
      return;
    }
  }
};

FileWalker.prototype.walk = function (f) {
  return this._walk(this.root, f);
};

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
  eleventyConfig.addTemplateFormats("acutis");
  let render = compiler.createRender();
  // Caching templates makes projects that heavily use "layout" templates
  // measurably faster.
  let cache = new Map();
  // Recompile components before each build.
  eleventyConfig.on("eleventy.before", async ({ directories }) => {
    let result = [];
    cache.clear();
    if (config && "components" in config) {
      getFuncs(config.components).forEach(({ key, f }) =>
        render.addFunc(key, f.interface, f),
      );
    }
    await new FileWalker(
      path.join(directories.input, directories.includes),
    ).walk((_stats, filePath, src) => render.addUint8Array(filePath, src));
  });
  eleventyConfig.addExtension("acutis", {
    // Because we pre-compile our components, 11ty's built-in file reading won't
    // reload all changes in watch mode.
    read: false,
    compile: async (str, inputPath) => {
      // since `read: false` is set, 11ty doesn't read Acutis template files.
      // If str has a value, it's either a permalink or markdown content.
      // Permalinks can either be a string or a function.
      if (str) {
        if (typeof str === "function") {
          return str;
        } else {
          let template = render.compileString(inputPath, str);
          return (data) => template.apply(data);
        }
      } else {
        let templatePromise = cache.get(inputPath);
        if (!templatePromise) {
          templatePromise = fs.readFile(inputPath).then((src) => {
            let template = render.compileUint8Array(inputPath, src);
            return (data) => template.apply(data);
          });
          cache.set(inputPath, templatePromise);
        }
        return templatePromise;
      }
    },
  });
}

// When building to JS files, we have to bypass Eleventy's own build system and
// so we need to use our own. This is a very rudimentary script but it should
// get the job done.

// Instead of creating a regular custom language extension, we output JS files
// that use Eleventy's own JS template engine. If we aren't careful, this can
// cause an infinite recompilation loop in watch mode, which is why it's
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
  eleventyConfig.on("eleventy.before", async ({ directories }) => {
    oracle.addConfig(config);
    let compPath =
      config && "componentsPath" in config ? config.componentsPath : "";
    let compFuns =
      config && "components" in config ? getFuncs(config.components) : [];
    let compSrc = [];
    let dirIncludes = path.join(directories.input, directories.includes);
    await new FileWalker(dirIncludes).walk((stats, filePath, src) => {
      oracle.addComponent(filePath, stats.mtimeMs);
      compSrc.push({ filePath, src });
    });
    await new FileWalker(directories.input)
      .ignore(dirIncludes)
      .ignore(path.join(directories.input, directories.data))
      .ignore(directories.output) // Not relative to input
      .walk((stats, filePath, src) => {
        let shouldBuild = oracle.addTemplate(filePath, stats.mtimeMs);
        if (shouldBuild) {
          let relativeCompPath =
            "." + path.sep + path.relative(path.dirname(filePath), compPath);
          let printjs = compiler.createPrintJS();
          compSrc.forEach(({ filePath, src }) =>
            printjs.addUint8Array(filePath, src),
          );
          compFuns.forEach(({ key, f }) =>
            printjs.addFunc(key, f.interface, relativeCompPath),
          );
          let result = printjs.compileUint8Array(filePath, src)[printer]();
          return fs.writeFile(filePath + extension, result);
        } else {
          return;
        }
      });
    oracle.reset();
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
  return printJs("toESMString", eleventyConfig, config);
}

/**
 * This is the same as `printESM` except it prints using CommonJS module format.
 */
export function printCJS(eleventyConfig, config) {
  return printJs("toCJSString", eleventyConfig, config);
}
