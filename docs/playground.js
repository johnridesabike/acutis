/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

let defaultSource =
  'Hello {% name ? "dear user" %},\n\
\n\
{% map objects with {name, color: !color} ~%}\n\
  Have you noticed the {% name %} is {% color %} today?\n\
{% match name, color\n\
   with "sky", "blue"\n\
   with "grass", "green" ~%}\n\
  That\'s an ordinary color for it.\n\
{% with _, _ ~%}\n\
  That seems odd.\n\
{% /match %}\n\
{% with {name} ~%}\n\
  The {% name %}, I suppose, has no color.\n\
{%~ /map %}\n\
';

let defaultProps = {
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
  let sourceTextElt = document.getElementById("source");
  let sourceResultElt = document.getElementById("jsresult");
  let sourceDirtyElt = document.getElementById("source-is-dirty");
  let propsTextElt = document.getElementById("props");
  let renderResultElt = document.getElementById("result");
  let propsDirtyElt = document.getElementById("props-is-dirty");
  let renderButtonElt = document.getElementById("render");
  let urlParams = new URLSearchParams(window.location.search);

  propsTextElt.oninput = function (_event) {
    setDirty(propsDirtyElt);
  };

  sourceTextElt.oninput = function (_event) {
    setDirty(sourceDirtyElt);
  };

  let urlSourceParam = urlParams.get("source");
  let urlSourceParamDecoded = null;
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

  let urlPropsParam = urlParams.get("props");
  let urlPropsParamDecoded = null;
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

  let getLinkElt = document.getElementById("getlink");
  getLinkElt.onclick = function getLink(_event) {
    let url = new URL(window.location);
    url.searchParams.set("props", btoa(propsTextElt.value));
    url.searchParams.set("source", btoa(sourceTextElt.value));
    if ("clipboard" in navigator) {
      navigator.clipboard.writeText(url.toString()).then(function () {
        let original = getLinkElt.textContent;
        getLinkElt.textContent = "Copied!";
        setTimeout(function () {
          getLinkElt.textContent = original;
        }, 2000);
      });
    }
    if (history) {
      history.pushState({}, "", url);
    } else {
      window.location.search = url.searchParams.toString();
    }
  };

  let componentsEmpty = Compile.components([]).result;

  async function render(_event) {
    sourceResultElt.value = "";
    renderResultElt.value = "";
    setClean(sourceDirtyElt);
    setClean(propsDirtyElt);
    let compiled = Compile.string(
      "<playground>",
      componentsEmpty,
      sourceTextElt.value,
    );
    if (compiled.result === null) {
      sourceResultElt.value = compiled.errors.join("\n\n");
    } else {
      sourceResultElt.value = Compile.toESMString(compiled.result);
      let result = [];
      try {
        let json = JSON.parse(propsTextElt.value);
        let rendered = await Compile.render(compiled.result, json);
        if (rendered.result === null) {
          result = rendered.errors;
        } else {
          result = [rendered.result];
        }
      } catch (e) {
        result = [e.message];
      }
      renderResultElt.value = compiled.errors.concat(result).join("\n\n");
    }
  }
  renderButtonElt.onclick = render;

  render();
};
