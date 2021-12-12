/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

const { Compile, Result, Render, Source, Typescheme } = require("../");

let emptyComponents = Compile.Components.empty();

describe("The JS interface works as expected", () => {
  test("Bad input is reported correctly", () => {
    expect(Compile.make("X", 1, emptyComponents)).toEqual({
      NAME: "errors",
      VAL: [
        {
          kind: "Compile",
          message:
            "An exception was thrown while compiling this template. This is probably due to malformed input.",
          exn: {
            RE_EXN_ID: "Caml_js_exceptions.Error/1",
            _1: new TypeError("source.str.charAt is not a function"),
          },
          path: ["X"],
        },
      ],
    });
  });
});

describe("Async templates", () => {
  test("Async env", async () => {
    const X = Compile.make("X", "{{ name }}", emptyComponents);
    const result = Result.map(X, (X) => Render.async(X, { name: "Carlo" }, {}));
    const x = await Result.getExn(result);
    expect(x).toEqual({ NAME: "ok", VAL: "Carlo" });
  });

  test("Async template components", async () => {
    const Y = Source.fn(
      "Y",
      Typescheme.props([["name", Typescheme.string()]]),
      Typescheme.Child.props([]),
      async (Env, props, _children) => Env.$$return(props.name)
    );
    const comps = Result.getExn(Compile.Components.make([Y]));
    const X = Result.getExn(Compile.make("X", "{% Y name /%}", comps));
    const x = await Render.async(X, { name: "Carlo" }, {});
    expect(x).toEqual({ NAME: "ok", VAL: "Carlo" });
  });

  test("Async template component errors", async () => {
    const D = Source.fn(
      "D",
      Typescheme.props([]),
      Typescheme.Child.props([]),
      async (_env, _props, _children) => {
        throw new Error("fail.");
      }
    );
    const comps = Result.getExn(Compile.Components.make([D]));
    const E = Result.getExn(Compile.make("E", "{% D /%}", comps));
    expect(await Render.async(E, {}, {})).toEqual({
      NAME: "errors",
      VAL: [
        {
          message: `An exception was thrown while rendering a template component.`,
          kind: "Render",
          path: ["E"],
          exn: new Error("fail."),
        },
      ],
    });
  });
});

describe("Async helper functions", () => {
  test("Env.return", async () => {
    const X = Source.fn(
      "X",
      Typescheme.props([]),
      Typescheme.Child.props([]),
      (Env, _props, _children) => Env.$$return("a")
    );
    const comps = Result.getExn(Compile.Components.make([X]));
    const template = Result.getExn(Compile.make("", "{% X / %}", comps));
    expect(await Render.async(template, {}, {})).toEqual({
      NAME: "ok",
      VAL: "a",
    });
  });

  test("Env.error", async () => {
    const X = Source.fn(
      "X",
      Typescheme.props([]),
      Typescheme.Child.props([]),
      (Env, _props, _children) => Env.error("e")
    );
    const comps = Result.getExn(Compile.Components.make([X]));
    const template = Result.getExn(
      Compile.make("Template", "{% X / %}", comps)
    );
    expect(await Render.async(template, {}, {})).toEqual({
      NAME: "errors",
      VAL: [{ message: "e", kind: "Render", path: [] }],
    });
  });

  test("Env.map", async () => {
    const X = Source.fn(
      "X",
      Typescheme.props([]),
      Typescheme.Child.props([["Children", Typescheme.Child.child()]]),
      (Env, _props, { Children }) => Env.map(Children, (x) => x.toUpperCase())
    );
    const comps = Result.getExn(Compile.Components.make([X]));
    const template = Result.getExn(
      Compile.make("", "{% X ~%} a {%~ /X %}", comps)
    );
    expect(await Render.async(template, {}, {})).toEqual({
      NAME: "ok",
      VAL: "A",
    });
  });

  test("Env.flatmap", async () => {
    const X = Source.fn(
      "X",
      Typescheme.props([]),
      Typescheme.Child.props([["Children", Typescheme.Child.child()]]),
      (Env, _props, { Children }) =>
        Env.flatmap(Children, (x) => Env.$$return(x.toUpperCase()))
    );
    const comps = Result.getExn(Compile.Components.make([X]));
    const template = Result.getExn(
      Compile.make("", "{% X %} a {% /X %}", comps)
    );
    expect(await Render.async(template, {}, {})).toEqual({
      NAME: "ok",
      VAL: " A ",
    });
  });
});
