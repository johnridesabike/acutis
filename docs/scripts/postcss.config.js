/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

const path = require("path");

module.exports = () => ({
  plugins: [
    require("postcss-import"),
    require("postcss-custom-properties")({
      importFrom: path.join("_assets", "style.css"),
    }),
    require("cssnano"),
  ],
});
