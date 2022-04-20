/**
  Copyright (c) 2022 John Jackson.

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
          message: `An exception was thrown while compiling template "X." This is probably due to malformed input.`,
          exn: {
            RE_EXN_ID: "Caml_js_exceptions.Error/1",
            _1: new TypeError("src.str.charAt is not a function"),
          },
          stack: [],
        },
      ],
    });
  });
});

describe("Async templates", () => {
  test("Async env", async () => {
    const X = Compile.make("X", "{{ name }}", emptyComponents);
    const result = Result.map(X, (X) => Render.async(X, { name: "Carlo" }));
    const x = await Result.getExn(result);
    expect(x).toEqual({ NAME: "ok", VAL: "Carlo" });
  });

  test("Async template components", async () => {
    const Y = Source.fn(
      "Y",
      Typescheme.make([["name", Typescheme.string()]]),
      Typescheme.Child.make([]),
      async (Env, props, _children) => Env.return_(props.name)
    );
    const comps = Result.getExn(Compile.Components.make([Y]));
    const X = Result.getExn(Compile.make("X", "{% Y name /%}", comps));
    const x = await Render.async(X, { name: "Carlo" });
    expect(x).toEqual({ NAME: "ok", VAL: "Carlo" });
  });

  test("Async template component errors", async () => {
    const D = Source.fn(
      "D",
      Typescheme.make([]),
      Typescheme.Child.make([]),
      async (_env, _props, _children) => {
        throw new Error("fail.");
      }
    );
    const comps = Result.getExn(Compile.Components.make([D]));
    const E = Result.getExn(Compile.make("E", "{% D /%}", comps));
    expect(await Render.async(E, {})).toEqual({
      NAME: "errors",
      VAL: [
        {
          message: `Template component "D" threw an exception.`,
          kind: "Render",
          stack: ["E"],
          exn: new Error("fail."),
          location: { name: "E", char: 3 },
        },
      ],
    });
  });
});

describe("Async helper functions", () => {
  test("Env.return", async () => {
    const X = Source.fn(
      "X",
      Typescheme.make([]),
      Typescheme.Child.make([]),
      (Env, _props, _children) => Env.return_("a")
    );
    const comps = Result.getExn(Compile.Components.make([X]));
    const template = Result.getExn(Compile.make("", "{% X / %}", comps));
    expect(await Render.async(template, {})).toEqual({
      NAME: "ok",
      VAL: "a",
    });
  });

  test("Env.error", async () => {
    const X = Source.fn(
      "X",
      Typescheme.make([]),
      Typescheme.Child.make([]),
      (Env, _props, _children) => Env.error("e")
    );
    const comps = Result.getExn(Compile.Components.make([X]));
    const template = Result.getExn(
      Compile.make("Template", "{% X / %}", comps)
    );
    expect(await Render.async(template, {})).toEqual({
      NAME: "errors",
      VAL: [{ message: "A template function raised this error:\ne", kind: "Render", stack: [] }],
    });
  });

  test("Env.map", async () => {
    const X = Source.fn(
      "X",
      Typescheme.make([]),
      Typescheme.Child.make([Typescheme.Child.child("Children")]),
      (Env, _props, { Children }) => Env.map(Children, (x) => x.toUpperCase())
    );
    const comps = Result.getExn(Compile.Components.make([X]));
    const template = Result.getExn(
      Compile.make("", "{% X ~%} a {%~ /X %}", comps)
    );
    expect(await Render.async(template, {})).toEqual({
      NAME: "ok",
      VAL: "A",
    });
  });

  test("Env.flatmap", async () => {
    const X = Source.fn(
      "X",
      Typescheme.make([]),
      Typescheme.Child.make([Typescheme.Child.child("Children")]),
      (Env, _props, { Children }) =>
        Env.flatmap(Children, (x) => Env.return_(x.toUpperCase()))
    );
    const comps = Result.getExn(Compile.Components.make([X]));
    const template = Result.getExn(
      Compile.make("", "{% X %} a {% /X %}", comps)
    );
    expect(await Render.async(template, {})).toEqual({
      NAME: "ok",
      VAL: " A ",
    });
  });
});
