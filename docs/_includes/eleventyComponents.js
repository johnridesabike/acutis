/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

const { Typescheme } = require("acutis-lang");
const site = require("../_data/site");

module.exports.Footer = function Footer(props) {
  const year = props.year ? props.year : new Date().getFullYear();
  const name = props.link
    ? `<a href="${props.link}">${props.name}</a>`
    : props.name;
  return Promise.resolve(`
    <footer class="footer">
      <p>
        Published in ${year} by ${name}.
      </p>
      <p class="footer__license">
        <a href="${props.siteUrl}/license/">View the license</a>.
      </p>
    </footer>`);
};
module.exports.Footer.interface = Typescheme.make([
  ["year", Typescheme.nullable(Typescheme.string())],
  ["link", Typescheme.nullable(Typescheme.string())],
  ["name", Typescheme.string()],
  ["siteUrl", Typescheme.string()],
]);

module.exports.Link = function Link({ path, page, children }) {
  const current = path === page.url ? "true" : "false";
  const href = site.url + path;
  return Promise.resolve(
    `<a href="${href}" aria-current="${current}">${children}</a>`
  );
};
module.exports.Link.interface = Typescheme.make([
  ["path", Typescheme.string()],
  ["page", Typescheme.record([["url", Typescheme.string()]])],
  ["children", Typescheme.string()],
]);
