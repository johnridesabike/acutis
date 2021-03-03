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

import {
  Compile,
  Environment,
  Source,
  Result,
} from "./../../lib/es6/src/AcutisJs.mjs";

window.onload = function playground(_event) {
  var propsText = document.getElementById("props");
  var sourceText = document.getElementById("source");
  var sourceIsDirty = document.getElementById("source-is-dirty");
  var propsIsDirty = document.getElementById("props-is-dirty");

  function setDirty(el) {
    el.textContent = "*";
  }

  function setClean(el) {
    el.textContent = "";
  }

  propsText.oninput = function (_event) {
    setDirty(propsIsDirty);
  };

  sourceText.oninput = function (_event) {
    setDirty(sourceIsDirty);
  };

  propsText.value = JSON.stringify(
    {
      name: null,
      objects: [
        { name: "sky", color: "blue" },
        { name: "grass", color: "purple" },
        { name: "air" },
      ],
    },
    null,
    2
  );
  sourceText.value = `Hello {{ name ? "dear user" }},

{% map objects with {name, color} ~%}
  Have you noticed the {{ name }} is {{ color }} today?
{% match name, color
   with "sky", "blue"
   with "grass", "green" ~%}
  That's an ordinary color for it.
{% with _, _ ~%}
  That seems odd.
{% /match %}
{% with {name} ~%}
  The {{ name }}, I suppose, has no color.
{%~ /map %}
`;

  var resultText = document.getElementById("result");

  function render(_event) {
    try {
      var props = JSON.parse(propsText.value);
      var src = Source.string("Playground", sourceText.value);
      var template = Compile.make(src, Compile.Components.empty());
      var result = Result.flatMap(template, (template) =>
        template(Environment.sync, props, {})
      );
      resultText.value = Result.getOrElse(
        result,
        (errors) => "Errors:\n" + JSON.stringify(errors, null, 2)
      );
    } catch (e) {
      resultText.value = e.message;
    }
    setClean(propsIsDirty);
    setClean(sourceIsDirty);
  }

  document.getElementById("render").onclick = render;

  render();
};
