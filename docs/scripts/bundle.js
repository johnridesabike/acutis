#!/usr/bin/env node

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
const cwd = process.cwd();

require("esbuild")
  .build({
    entryPoints: [path.join(cwd, "_assets", "playground.mjs")],
    bundle: true,
    minify: true,
    format: "esm",
    target: ["chrome58", "firefox57", "safari11", "edge16"],
    sourcemap: true,
    outfile: path.join(cwd, "_site", "playground.js"),
  })
  .catch((e) => {
    console.error(e);
    process.exitCode = 1;
  });
