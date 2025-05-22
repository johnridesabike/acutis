/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

import module from "node:module";
let require = module.createRequire(import.meta.url);
let acutisPkg = require("acutis-lang/package.json");

export default {
  title: "Acutis",
  subtitle: acutisPkg.description,
  pathPrefix: "/",
  url:
    process.env.NODE_ENV === "production"
      ? "https://acutis.johnridesa.bike"
      : "http://localhost:8080",
  menu: [
    { path: "/", name: "Home" },
    { path: "/introduction/", name: "Getting started" },
    { path: "/manual/", name: "Language manual" },
    { path: "/api/acutis/Acutis/index.html", name: "OCaml API" },
    { path: "/playground/", name: "Playground" },
    { path: "/license/", name: "License" },
  ],
};
