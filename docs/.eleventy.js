/**
 *    Copyright 2020 John Jackson
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

const fastGlob = require("fast-glob");
const syntaxHighlight = require("@11ty/eleventy-plugin-syntaxhighlight");
const acutis = require("./eleventyAcutis");
const MarkdownIt = require("markdown-it");
const markdownItAnchor = require("markdown-it-anchor");
const markdownItToc = require("markdown-it-table-of-contents");

module.exports = (eleventyConfig) => {
  eleventyConfig.addPlugin(syntaxHighlight);
  eleventyConfig.addPlugin(acutis);
  eleventyConfig.addPassthroughCopy("playground.js");
  eleventyConfig.addPassthroughCopy({
    "../dist/acutis.iife.min.js": "acutis.iife.min.js",
  });
  eleventyConfig.addPassthroughCopy({
    "../dist/acutis.iife.min.js.map": "acutis.iife.min.js.map",
  });
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
        slugify: (s) =>
          encodeURIComponent(
            String(s)
              .trim()
              .toLowerCase()
              .replace(/\s+/g, "-")
              // a hacky workaround. md-it-anchor parses the rendered text but
              // md-it-toc parses the raw markdown.
              .replace(/`/g, "")
          ),
      })
  );
  return {
    markdownTemplateEngine: false,
  };
};
