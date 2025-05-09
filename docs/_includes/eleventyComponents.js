/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

import site from "../_data/site.js";

export function Footer(props) {
  let year = props.year ? props.year : new Date().getFullYear();
  let name = props.link
    ? `<a href="${props.link}">${props.name}</a>`
    : props.name;
  return `
    <footer class="footer">
      <p>
        Published in ${year} by ${name}.
      </p>
      <p class="footer__license">
        <a href="${props.siteUrl}/license/">View the license</a>.
      </p>
    </footer>`;
}
Footer.interface = {
  year: ["nullable", "string"],
  link: ["nullable", "string"],
  name: "string",
  siteUrl: "string",
};

export function Link({ path, page, children }) {
  let current = path === page.url ? "true" : "false";
  let href = site.url + path;
  return `<a href="${href}" aria-current="${current}">${children}</a>`;
}
Link.interface = {
  path: "string",
  page: { url: "string" },
  children: "string",
};
