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
} = require("../../_build/default/js/acutis_js.bc.js");
const site = require("../_data/site");

module.exports = [
  Compile.fromFunAsync(
    "Log",
    Typescheme.make([["val", Typescheme.unknown()]]),
    TypeschemeChildren.make([]),
    (props, _children) => {
      console.log(props);
      return Promise.resolve("");
    }
  ),
  Compile.fromFunAsync(
    "Debugger",
    Typescheme.make([["val", Typescheme.unknown()]]),
    TypeschemeChildren.make([]),
    (_props, _children) => {
      debugger;
      return Promise.resolve("");
    }
  ),
  Compile.fromFunAsync(
    "Footer",
    Typescheme.make([
      ["year", Typescheme.nullable(Typescheme.string())],
      ["link", Typescheme.nullable(Typescheme.string())],
      ["name", Typescheme.string()],
      ["siteUrl", Typescheme.string()],
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
  Compile.fromFunAsync(
    "Link",
    Typescheme.make([
      ["path", Typescheme.string()],
      ["page", Typescheme.record([["url", Typescheme.string()]])],
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
