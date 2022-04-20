/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
