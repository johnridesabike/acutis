/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

import fs from "node:fs/promises";
import postcss from "postcss";
import postCssImport from "postcss-import";
import postCssCustomProperties from "postcss-custom-properties";

let postcssWithOptions = postcss([postCssImport, postCssCustomProperties()]);

let cssPath = "style.css";
let cssFile = fs.readFile(new URL(cssPath, import.meta.url));

export default function Template() {}

Template.prototype.data = async function data() {
  let css = await cssFile;
  return {
    permalink: cssPath,
    cssPath,
    css: css,
  };
};

Template.prototype.render = async function render({ css, cssPath }) {
  let result = await postcssWithOptions.process(css, { from: cssPath });
  return result.css;
};
