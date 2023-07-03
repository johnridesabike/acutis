/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

var defaultSource =
  'Hello {{ name ? "dear user" }},\n\
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

var defaultProps = {
  name: null,
  objects: [
    { name: "sky", color: "blue" },
    { name: "grass", color: "purple" },
    { name: "air" },
  ],
};

function setDirty(elt) {
  elt.textContent = "*";
}

function setClean(elt) {
  elt.textContent = "";
}

window.onload = function playground(_event) {
  var sourceTextElt = document.getElementById("source");
  var sourceResultElt = document.getElementById("jsresult");
  var sourceDirtyElt = document.getElementById("source-is-dirty");
  var propsTextElt = document.getElementById("props");
  var renderResultElt = document.getElementById("result");
  var propsDirtyElt = document.getElementById("props-is-dirty");
  var renderButtonElt = document.getElementById("render");
  var urlParams = new URLSearchParams(window.location.search);

  propsTextElt.oninput = function (_event) {
    setDirty(propsDirtyElt);
  };

  sourceTextElt.oninput = function (_event) {
    setDirty(sourceDirtyElt);
  };

  var urlSourceParam = urlParams.get("source");
  var urlSourceParamDecoded = null;
  if (urlSourceParam) {
    try {
      urlSourceParamDecoded = atob(urlSourceParam);
    } catch (e) {
      console.error(e);
    }
  }
  sourceTextElt.value = urlSourceParamDecoded
    ? urlSourceParamDecoded
    : defaultSource;

  var urlPropsParam = urlParams.get("props");
  var urlPropsParamDecoded = null;
  if (urlPropsParam) {
    try {
      urlPropsParamDecoded = atob(urlPropsParam);
    } catch (e) {
      console.error(e);
    }
  }
  propsTextElt.value = urlPropsParamDecoded
    ? urlPropsParamDecoded
    : JSON.stringify(defaultProps, null, 2);

  var getLinkElt = document.getElementById("getlink");
  getLinkElt.onclick = function getLink(_event) {
    var url = new URL(window.location);
    url.searchParams.set("props", btoa(propsTextElt.value));
    url.searchParams.set("source", btoa(sourceTextElt.value));
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

  var componentsEmpty = Compile.components([]);

  function render(_event) {
    var compiled = null;
    var compileJSResult = "";
    var renderResult = "";
    try {
      compiled = Compile.string(
        "<playground>",
        componentsEmpty,
        sourceTextElt.value
      );
      compileJSResult = Compile.toJSString(compiled);
    } catch (e) {
      if (Utils.isError(e)) {
        compileJSResult = Utils.getError(e);
      } else {
        compileJSResult = e.message;
      }
    }
    if (compiled) {
      try {
        var json = JSON.parse(propsTextElt.value);
        renderResult = Render.sync(compiled, json);
      } catch (e) {
        if (Utils.isError(e)) {
          renderResult = Utils.getError(e);
        } else {
          renderResult = e.message;
        }
      }
    }
    sourceResultElt.value = compileJSResult;
    renderResultElt.value = renderResult;
    setClean(source);
    setClean(propsDirtyElt);
  }
  renderButtonElt.onclick = render;

  render();
};
