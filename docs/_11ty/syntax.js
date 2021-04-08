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
module.exports = function (Prism) {
  // Make sure markup-templating is loaded.
  require("prismjs/components/prism-markup-templating");
  Prism.languages.acutis = {
    comment: /^{\*[\s\S]*?\*}$/,
    tag: {
      pattern: /(^{%\s*)([a-z_])([a-zA-Z0-9_]+)/,
      lookbehind: true,
      alias: "keyword",
    },
    delimiter: {
      pattern: /^{[{%]|[}%]}$/,
      alias: "punctuation",
    },
    string: {
      pattern: /(")(?:\\.|(?!\1)[^\\\r\n])*\1/,
      greedy: true,
    },
    component: {
      pattern: /\b([A-Z])([a-zA-Z0-9_]+)?/,
      alias: "function",
    },
    keyword: /\b(?:match|map|with)\b/,
    operator: /&|\?|#|~/,
    number: /(-|\+)?\s*[0-9]+(\.[0-9]+)?(e|E)?/,
    boolean: /\b(false|true|null)\b/,
    variable: /([a-z_])([a-zA-Z0-9_]+)?/,
    punctuation: /[{}[\],.:/=]/,
  };

  var pattern = /{{[\s\S]*?}}|{%[\s\S]*?%}|{\*[\s\S]*?\*}/g;
  var markupTemplating = Prism.languages["markup-templating"];
  //console.dir(Prism.languages);

  Prism.hooks.add("before-tokenize", (env) =>
    markupTemplating.buildPlaceholders(env, "acutis", pattern)
  );
  Prism.hooks.add("after-tokenize", (env) =>
    markupTemplating.tokenizePlaceholders(env, "acutis")
  );
};
