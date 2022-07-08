/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

let acutisPkg = require("../../package.json");

module.exports = {
  title: "Acutis",
  subtitle: acutisPkg.description,
  pathPrefix: "/acutis/",
  url:
    process.env.NODE_ENV === "production"
      ? "https://johnridesa.bike/acutis"
      : "http://localhost:8080/acutis",
  menu: [
    { path: "/", name: "Home" },
    { path: "/introduction/", name: "Getting started" },
    { path: "/manual/", name: "Language manual" },
    { path: "/api/", name: "OCaml API" },
    { path: "/playground/", name: "Playground" },
    { path: "/credits/", name: "Credits" },
  ],
};
