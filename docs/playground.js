/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

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

{% map objects with {name, color: !color} ~%}
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
  var components = globalThis.Compile.components([]);

  function render(_event) {
    let result;
    try {
      var props = JSON.parse(propsText.value);
      var template = globalThis.Compile.string(
        "<playground>",
        components,
        sourceText.value
      );
      result = globalThis.Render.sync(template, props);
    } catch (e) {
      if (globalThis.Utils.isError(e)) {
        result = globalThis.Utils.getError(e);
      } else {
        result = e.message;
      }
    }
    resultText.value = result;
    setClean(propsIsDirty);
    setClean(sourceIsDirty);
  }

  document.getElementById("render").onclick = render;

  render();
};
