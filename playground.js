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
  var urlParams = new URLSearchParams(window.location.search);

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

  var urlPropsParam = urlParams.get("props");
  var urlPropsParamDecoded = null;
  if (urlPropsParam) {
    try {
      urlPropsParamDecoded = atob(urlPropsParam);
    } catch (e) {
      console.error(e);
    }
  }
  propsText.value = urlPropsParamDecoded
    ? urlPropsParamDecoded
    : JSON.stringify(
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

  var urlSourceParam = urlParams.get("source");
  var urlSourceParamDecoded = null;
  if (urlSourceParam) {
    try {
      urlSourceParamDecoded = atob(urlSourceParam);
    } catch (e) {
      console.error(e);
    }
  }
  sourceText.value = urlSourceParamDecoded
    ? urlSourceParamDecoded
    : 'Hello {{ name ? "dear user" }},\n\
\n\
{% map objects with {name, color: !color} ~%}\n\
  Have you noticed the {{ name }} is {{ color }} today?\n\
{% match name, color\n\
   with "sky", "blue"\n\
   with "grass", "green" ~%}\n\
  That\'s an ordinary color for it.\n\
{% with _, _ ~%}\n\
  That seems odd.\n\
{% /match %}\n\
{% with {name} ~%}\n\
  The {{ name }}, I suppose, has no color.\n\
{%~ /map %}\n\
';

  var getLinkElt = document.getElementById("getlink");
  getLinkElt.onclick = function getLink(_event) {
    var url = new URL(window.location);
    url.searchParams.set("props", btoa(propsText.value));
    url.searchParams.set("source", btoa(sourceText.value));
    if ("clipboard" in navigator) {
      navigator.clipboard.writeText(url.toString()).then(function () {
        var original = getLinkElt.textContent;
        getLinkElt.textContent = "Copied!";
        setTimeout(function () {
          getLinkElt.textContent = original;
        }, 2000);
      });
    }
    history
      ? history.pushState({}, "", url)
      : (window.location.search = url.searchParams.toString());
  };

  var resultText = document.getElementById("result");
  var jsresultText = document.getElementById("jsresult");
  var components = globalThis.Compile.components([]);

  function render(_event) {
    var result;
    var jsresult;
    try {
      var props = JSON.parse(propsText.value);
      var template = globalThis.Compile.string(
        "<playground>",
        components,
        sourceText.value
      );
      jsresult = globalThis.Compile.toJSString(template);
      result = globalThis.Render.sync(template, props);
    } catch (e) {
      jsresult = "";
      if (globalThis.Utils.isError(e)) {
        result = globalThis.Utils.getError(e);
      } else {
        result = e.message;
      }
    }
    jsresultText.value = jsresult;
    resultText.value = result;
    setClean(propsIsDirty);
    setClean(sourceIsDirty);
  }

  document.getElementById("render").onclick = render;

  render();
};
