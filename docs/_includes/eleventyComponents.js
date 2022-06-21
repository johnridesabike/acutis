/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

const {
  Compile,
  Typescheme,
  TypeschemeChildren,
} = require("../../ocaml/acutis/_build/default/bin/main.bc.js");
const site = require("../_data/site");

module.exports = [
  Compile.fnAsync(
    "Log",
    Typescheme.make([["val", Typescheme.unknown(0)]]),
    TypeschemeChildren.make([]),
    (props, _children) => {
      console.log(props);
      return Promise.resolve("");
    }
  ),
  Compile.fnAsync(
    "Debugger",
    Typescheme.make([["val", Typescheme.unknown(0)]]),
    TypeschemeChildren.make([]),
    (_props, _children) => {
      debugger;
      return Promise.resolve("");
    }
  ),
  Compile.fnAsync(
    "Footer",
    Typescheme.make([
      ["year", Typescheme.nullable(Typescheme.string(0))],
      ["link", Typescheme.nullable(Typescheme.string(0))],
      ["name", Typescheme.string(0)],
      ["siteUrl", Typescheme.string(0)],
    ]),
    TypeschemeChildren.make([]),
    (props, _children) => {
      if (!props.year) {
        props.year = new Date().getFullYear();
      }
      var link;
      if (props.link == null) {
        link = props.name;
      } else {
        link = `<a href="${props.link}">${props.name}</a>`;
      }
      return Promise.resolve(`
        <footer class="footer">
          <p>
            Published in ${props.year} by ${link}.
          </p>
          <p class="footer__license">
            <a href="${props.siteUrl}/license/">View the license</a>.
          </p>
        </footer>`);
    }
  ),
  Compile.fnAsync(
    "Link",
    Typescheme.make([
      ["path", Typescheme.string(0)],
      ["page", Typescheme.record([["url", Typescheme.string(0)]])],
    ]),
    TypeschemeChildren.make([TypeschemeChildren.child("Children")]),
    ({ path, page }, { Children }) => {
      const current = path === page.url ? "true" : "false";
      const href = site.url + path;
      return Children.then(
        (Children) =>
          `<a href="${href}" aria-current="${current}">${Children}</a>`
      );
    }
  ),
];
