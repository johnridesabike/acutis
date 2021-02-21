/**
 *    Copyright 2021 John Jackson
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
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
    (ast) => (env, { path, page }, children) => {
      const current = path === page.url ? "true" : "false";
      const href = site.url + path;
      return env.render(ast, { href, current }, children);
    }
  ),
];
