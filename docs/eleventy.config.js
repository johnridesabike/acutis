/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

import path from "node:path";
import module from "node:module";
import syntaxHighlight from "@11ty/eleventy-plugin-syntaxhighlight";
import markdownItAnchor from "markdown-it-anchor";
import markdownItToc from "markdown-it-table-of-contents";
import * as acutisEleventy from "acutis-lang/eleventy";
import siteData from "./_data/site.js";
import * as eleventyComponents from "./_includes/eleventyComponents.js";

async function acutisSyntax({ Prism }) {
  // Make sure markup-templating is loaded.
  await import("prismjs/components/prism-markup-templating.js");

  Prism.languages.acutis = {
    comment: /^{\*[\s\S]*?\*}$/,
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
    punctuation: /[{}[\],.:/=()<>|]/,
  };

  let pattern = /{({?)%[\s\S]*?%(}?)}|{\*[\s\S]*?\*}/g;
  let markupTemplating = Prism.languages["markup-templating"];

  Prism.hooks.add("before-tokenize", (env) =>
    markupTemplating.buildPlaceholders(env, "acutis", pattern),
  );
  Prism.hooks.add("after-tokenize", (env) =>
    markupTemplating.tokenizePlaceholders(env, "acutis"),
  );
}

let require = module.createRequire(import.meta.url);
let acutisPath = require.resolve("acutis-lang");
let acutisDirPath = path.dirname(acutisPath);

export default (eleventyConfig) => {
  eleventyConfig.addPlugin(syntaxHighlight, { init: acutisSyntax });
  eleventyConfig.addPlugin(acutisEleventy.printESM, {
    components: eleventyComponents,
    componentsPath: "./_includes/eleventyComponents.js",
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
      }),
  );
  eleventyConfig.addWatchTarget("style.css");
  return {
    markdownTemplateEngine: false,
    pathPrefix: siteData.pathPrefix,
    dir: {
      layouts: "_layouts",
    },
  };
};
