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

import * as Acutis from "./../../lib/es6/src/AcutisJs.mjs";

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

  var env = Acutis.Environment.make({});

  var resultText = document.getElementById("result");

  function render(_event) {
    try {
      var props = JSON.parse(propsText.value);
      var template = Acutis.Compile.make(sourceText.value, "playground");
      var result = template(env, props, {});
      if (result.NAME === "errors") {
        resultText.value = "Errors:\n" + JSON.stringify(result.VAL, null, 2);
      } else {
        resultText.value = result.VAL;
      }
    } catch (e) {
      resultText.value = e.message;
    }
    setClean(propsIsDirty);
    setClean(sourceIsDirty);
  }

  document.getElementById("render").onclick = render;

  render();
};
