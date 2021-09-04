#!/usr/bin/env node

/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
