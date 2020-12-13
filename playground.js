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
  sourceText.value =
    "Hello\n" +
    "{%~ match name \n" +
    "    with null %} dear user,\n" +
    "{%~ with name %} {{ name }},\n" +
    "{%~ /match %}\n" +
    "\n" +
    "{% map objects with {name, color} ~%}\n" +
    "  Have you noticed the {{ name }} is {{ color }} today?\n" +
    "{% match name, color\n" +
    '   with "sky", "blue"\n' +
    '   with "grass", "green" ~%}\n' +
    "  That's an ordinary color for it.\n" +
    "{% with _, _ ~%}\n" +
    "  That seems odd.\n" +
    "{% /match %}\n" +
    "{% with {name} ~%}\n" +
    "  The {{ name }}, I suppose, has no color.\n" +
    "{%~ /map %}\n";

  var renderContext = Acutis.renderContext({});

  var resultText = document.getElementById("result");

  function render(_event) {
    try {
      var props = JSON.parse(propsText.value);
      var template = Acutis.compile(sourceText.value, "playground");
      var result = Acutis.result(template(renderContext, props, {}));
      if (result.data) {
        resultText.value = result.data;
      } else {
        resultText.value = "Errors:\n" + JSON.stringify(result.errors, null, 2);
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
