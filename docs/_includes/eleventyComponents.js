/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

const { Source } = require("../../");
const site = require("../_data/site");

module.exports = [
  Source.func("Log", (env, props, _children) => {
    console.log(props);
    return env.return("");
  }),
  Source.func("Debugger", (env, props, children) => {
    debugger;
    return env.return("");
  }),
  Source.funcWithString(
    "Footer",
    `<footer class="footer">
  <p>
    Published in {{ year }} by
    {%~ match link with null ~%} {{ name }}
    {%~ with link %} <a href="{{ link }}">{{ name }}</a>
    {%~ /match ~%}
    .
  </p>
  <p class="footer__license">
    <a href="{{ siteUrl }}/license/">View the license</a>.
  </p>
</footer>
`,
    (ast) => (env, props, children) => {
      if (!props.year) {
        props.year = new Date().getFullYear();
      }
      return env.render(ast, props, children);
    }
  ),
  Source.funcWithString(
    "Link",
    `<a href="{{ href }}" aria-current="{{ current }}">{{ Children }}</a>`,
    (ast) =>
      (env, { path, page }, children) => {
        const current = path === page.url ? "true" : "false";
        const href = site.url + path;
        return env.render(ast, { href, current }, children);
      }
  ),
];
