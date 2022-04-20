/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

const syntaxHighlight = require("@11ty/eleventy-plugin-syntaxhighlight");
const MarkdownIt = require("markdown-it");
const markdownItAnchor = require("markdown-it-anchor");
const markdownItToc = require("markdown-it-table-of-contents");

const acutis = require("./_11ty/eleventyAcutis");
const acutisSyntax = require("./_11ty/syntax");
const acutisComponents = require("./_includes/eleventyComponents");
const { pathPrefix } = require("./_data/site");

module.exports = (eleventyConfig) => {
  eleventyConfig.addPlugin(syntaxHighlight, {
    init: ({ Prism }) => acutisSyntax(Prism),
  });
  eleventyConfig.addPlugin(acutis, { components: acutisComponents });
  eleventyConfig.addPassthroughCopy({
    "_assets/playground.js": "playground.js",
  });
  eleventyConfig.addPassthroughCopy("icon.svg");
  eleventyConfig.addPassthroughCopy("favicon.ico");
  eleventyConfig.setLibrary(
    "md",
    MarkdownIt({
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
  return {
    markdownTemplateEngine: false,
    pathPrefix,
  };
};
