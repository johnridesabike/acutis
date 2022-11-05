/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

const { Component, Typescheme } = require("../..");
const site = require("../_data/site");

module.exports = [
  Component.funAsync(
    "Log",
    Typescheme.make([["val", Typescheme.unknown()]]),
    (props) => {
      console.log(props);
      return Promise.resolve("");
    }
  ),
  Component.funAsync(
    "Debugger",
    Typescheme.make([["val", Typescheme.unknown()]]),
    (_props) => {
      debugger;
      return Promise.resolve("");
    }
  ),
  Component.funAsync(
    "Footer",
    Typescheme.make([
      ["year", Typescheme.nullable(Typescheme.string())],
      ["link", Typescheme.nullable(Typescheme.string())],
      ["name", Typescheme.string()],
      ["siteUrl", Typescheme.string()],
    ]),
    (props) => {
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
  Component.funAsync(
    "Link",
    Typescheme.make([
      ["path", Typescheme.string()],
      ["page", Typescheme.record([["url", Typescheme.string()]])],
      ["children", Typescheme.string()],
    ]),
    ({ path, page, children }) => {
      const current = path === page.url ? "true" : "false";
      const href = site.url + path;
      return Promise.resolve(
        `<a href="${href}" aria-current="${current}">${children}</a>`
      );
    }
  ),
];
