/**
  Copyright (c) 2021 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

const { Source, Typescheme } = require("../../");
const site = require("../_data/site");

module.exports = [
  Source.fn(
    "Log",
    Typescheme.props([["val", Typescheme.unknown()]]),
    Typescheme.Child.props([]),
    (Env, props, _children) => {
      console.log(props);
      return Env.return_("");
    }
  ),
  Source.fn(
    "Debugger",
    Typescheme.props([["val", Typescheme.unknown()]]),
    Typescheme.Child.props([]),
    (Env, props, children) => {
      debugger;
      return Env.return_("");
    }
  ),
  Source.fn(
    "Footer",
    Typescheme.props([
      ["year", Typescheme.nullable(Typescheme.string())],
      ["link", Typescheme.nullable(Typescheme.string())],
      ["name", Typescheme.string()],
      ["siteUrl", Typescheme.string()],
    ]),
    Typescheme.Child.props([]),
    (Env, props, _children) => {
      if (!props.year) {
        props.year = new Date().getFullYear();
      }
      var link;
      if (props.link == null) {
        link = props.name;
      } else {
        link = `<a href="${props.link}">${props.name}</a>`;
      }
      return Env.return_(`
        <footer class="footer">
          <p>
            Published in ${props.year} by ${link}
            .
          </p>
          <p class="footer__license">
            <a href="${props.siteUrl}/license/">View the license</a>.
          </p>
        </footer>`);
    }
  ),
  Source.fn(
    "Link",
    Typescheme.props([
      ["path", Typescheme.string()],
      ["page", Typescheme.record([["url", Typescheme.string()]])],
    ]),
    Typescheme.Child.props([Typescheme.Child.child("Children")]),
    (Env, { path, page }, { Children }) => {
      const current = path === page.url ? "true" : "false";
      const href = site.url + path;
      return Env.map(
        Children,
        (Children) =>
          `<a href="${href}" aria-current="${current}">${Children}</a>`
      );
    }
  ),
];
