/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

const path = require("path");
const syntaxHighlight = require("@11ty/eleventy-plugin-syntaxhighlight");
const markdownItAnchor = require("markdown-it-anchor");
const markdownItToc = require("markdown-it-table-of-contents");
const acutisEleventy = require("acutis-lang/eleventy");
const { pathPrefix } = require("./_data/site");

function acutisSyntax(Prism) {
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
    tag: {
      pattern: /(@)([a-z_])([a-zA-Z0-9_]+)?/,
      alias: "operator",
    },
    formatter: {
      pattern: /%(,)?(\.[0-9]+)?[a-z]/,
      alias: "function",
    },
    keyword: /(\/{0,1})\b(?:match|map|map_dict|with|interface)\b/,
    operator: /\?|#|~|!|@/,
    number: /(-|\+)?\s*[0-9]+(\.[0-9]+)?(e|E)?/,
    boolean: /\b(false|true|null)\b/,
    variable: /([a-z_])([a-zA-Z0-9_]+)?/,
    punctuation: /[{}[\],.:/=(\)<>\|]/,
  };

  var pattern = /{{({?)[\s\S]*?(}?)}}|{%[\s\S]*?%}|{\*[\s\S]*?\*}/g;
  var markupTemplating = Prism.languages["markup-templating"];

  Prism.hooks.add("before-tokenize", (env) =>
    markupTemplating.buildPlaceholders(env, "acutis", pattern)
  );
  Prism.hooks.add("after-tokenize", (env) =>
    markupTemplating.tokenizePlaceholders(env, "acutis")
  );
}

const acutisPath = require.resolve("acutis-lang");
const acutisDirPath = path.dirname(require.resolve("acutis-lang/package.json"));

module.exports = (eleventyConfig) => {
  eleventyConfig.addPlugin(syntaxHighlight, {
    init: ({ Prism }) => acutisSyntax(Prism),
  });
  // eleventyConfig.addPlugin(acutisEleventy, {
  //   components: require("./_includes/eleventyComponents"),
  // });
  eleventyConfig.addPlugin(acutisEleventy.printJs, {
    components: require("./_includes/eleventyComponents"),
    componentsPath: "./_includes/eleventyComponents",
  });
  // We gitignore generated files, but we don't want 11ty to ignore them.
  eleventyConfig.setUseGitIgnore(false);
  eleventyConfig.ignores.add("node_modules");
  eleventyConfig.addPassthroughCopy("playground.js");
  eleventyConfig.addPassthroughCopy({
    [path.join(acutisDirPath, "_doc", "_html")]: "api",
  });
  eleventyConfig.addPassthroughCopy({ [acutisPath]: "acutis.js" });
  eleventyConfig.addPassthroughCopy("icon.svg");
  eleventyConfig.addPassthroughCopy(".nojekyll");
  eleventyConfig.amendLibrary("md", (mdLib) =>
    mdLib
      .set({
        html: true,
        typographer: true,
      })
      .use(markdownItAnchor)
      .use(markdownItToc, {
        containerHeaderHtml: `<details open><summary class="toc-container-header">Contents</summary>`,
        containerFooterHtml: `</details>`,
        listType: "ol",
        includeLevel: [1, 2, 3],
      })
  );
  eleventyConfig.addWatchTarget("style.css");
  return {
    markdownTemplateEngine: false,
    pathPrefix,
    dir: {
      layouts: "_layouts",
    },
  };
};
