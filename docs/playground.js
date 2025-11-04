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
  let sourceDirtyElt = document.getElementById("source-is-dirty");
  let propsTextElt = document.getElementById("props");
  let renderResultElt = document.getElementById("result");
  let propsDirtyElt = document.getElementById("props-is-dirty");
  let runButtonElt = document.getElementById("run");
  let compileFormElt = document.getElementById("compiler");
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
  let linkEltOriginalText = getLinkElt.textContent;
  let timeoutId = null;
  getLinkElt.onclick = function getLink(ev) {
    ev.preventDefault();
    let url = new URL(window.location);
    url.searchParams.set("props", btoa(propsTextElt.value));
    url.searchParams.set("source", btoa(sourceTextElt.value));
    if ("clipboard" in navigator) {
      navigator.clipboard.writeText(url.toString()).then(function () {
        getLinkElt.textContent = "Copied!";
        if (timeoutId !== null) {
          clearTimeout(timeoutId);
        }
        timeoutId = setTimeout(function () {
          getLinkElt.textContent = linkEltOriginalText;
        }, 2000);
      });
    }
    if (history) {
      history.pushState({}, "", url);
    } else {
      window.location.search = url.searchParams.toString();
    }
  };

  function switchCompiler(ev) {
    if (ev) {
      ev.preventDefault();
    }
    switch (compileFormElt.elements["select-compiler"].value) {
      case "select-render":
        propsTextElt.disabled = false;
        break;
      default:
        propsTextElt.disabled = true;
    }
  }
  compileFormElt.onchange = switchCompiler;

  async function render(ev) {
    if (ev) {
      ev.preventDefault();
    }
    renderResultElt.value = "";
    setClean(sourceDirtyElt);
    setClean(propsDirtyElt);
    let result = [];
    let compiler = Acutis({
      onMessage: (msg) => result.push(msg),
    });
    try {
      switch (compileFormElt.elements["select-compiler"].value) {
        case "select-render": {
          let json = JSON.parse(propsTextElt.value);
          let rendered = compiler
            .createRender()
            .compileString("[playground]", sourceTextElt.value)
            .apply(json);
          result.push(await rendered);
          break;
        }
        default: {
          let rendered = compiler
            .createPrintJS()
            .compileString("[playground]", sourceTextElt.value)
            .toESMString();
          result.push(rendered);
        }
      }
    } catch (e) {
      result.push(e.message);
    }
    renderResultElt.value = result.join("\n\n");
  }
  runButtonElt.onclick = render;

  switchCompiler();
  render();
};
