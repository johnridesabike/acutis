/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

const fs = require("fs");
const path = require("path");
const util = require("util");
const postcss = require("postcss");

const readFile = util.promisify(fs.readFile);

const cssPath = "style.css";

module.exports = class {
  async data() {
    const css = await readFile(path.join(__dirname, cssPath));
    return {
      permalink: cssPath,
      cssPath,
      css: css,
    };
  }

  render({ css, cssPath }) {
    return postcss([
      require("postcss-import"),
      require("postcss-custom-properties")(),
      require("cssnano"),
    ])
      .process(css, { from: cssPath })
      .then((result) => result.css);
  }
};