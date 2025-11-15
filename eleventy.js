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
  this.ignoredFileNames = new Set(); // File names ignored in all paths.
  this.ignoredFileNames.add(".git");
  this.ignoredFileNames.add("node_modules");
  this.ignoredPaths = new Set(); // Complete file paths ignored.
  this.root = root; // A directory to start walking.
}

FileWalker.prototype.ignorePath = function (filePath) {
  this.ignoredPaths.add(filePath);
  return this;
};

FileWalker.prototype.walk = async function* (filePath) {
  let stats = await fs.stat(filePath);
  if (stats.isDirectory()) {
    for (let fileName of await fs.readdir(filePath)) {
      if (!this.ignoredFileNames.has(fileName)) {
        let newFilePath = path.join(filePath, fileName);
        let isIgnored = false;
        for (let ignoredPath of this.ignoredPaths) {
          if (path.relative(ignoredPath, newFilePath) === "") {
            isIgnored = true;
            break;
          }
        }
        if (!isIgnored) {
          yield* this.walk(newFilePath);
        }
      }
    }
  } else if (path.extname(filePath).toLowerCase() === ".acutis") {
    let src = await fs.readFile(filePath);
    yield { filePath, src, stats };
  }
};

FileWalker.prototype[Symbol.asyncIterator] = function () {
  return this.walk(this.root);
};

function* getFuncs(obj) {
  for (let key in obj) {
    let f = obj[key];
    if (typeof f === "function" && "interface" in f) {
      yield { key, f };
    }
  }
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
      for (let { key, f } of getFuncs(config.components)) {
        render.addFunc(key, f.interface, f);
      }
    }
    let fw = new FileWalker(path.join(directories.input, directories.includes));
    for await (let { filePath, src } of fw) {
      render.addUint8Array(filePath, src);
    }
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
    let compFuns = config && "components" in config ? config.components : {};
    let compSrc = [];
    let dirIncludes = path.join(directories.input, directories.includes);
    for await (let { filePath, src, stats } of new FileWalker(dirIncludes)) {
      oracle.addComponent(filePath, stats.mtimeMs);
      compSrc.push({ filePath, src });
    }
    let fwInput = new FileWalker(directories.input)
      .ignorePath(directories.includes) // Already relative to input.
      .ignorePath(directories.data) // Already relative to input.
      .ignorePath(path.relative(directories.input, directories.output));
    for await (let { filePath, src, stats } of fwInput) {
      let shouldBuild = oracle.addTemplate(filePath, stats.mtimeMs);
      if (shouldBuild) {
        let relativeCompPath =
          "." + path.sep + path.relative(path.dirname(filePath), compPath);
        let printjs = compiler.createPrintJS();
        for (let { filePath, src } of compSrc) {
          printjs.addUint8Array(filePath, src);
        }
        for (let { key, f } of getFuncs(compFuns)) {
          printjs.addFunc(key, f.interface, relativeCompPath);
        }
        let result = printjs.compileUint8Array(filePath, src)[printer]();
        await fs.writeFile(filePath + extension, result);
      }
    }
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
