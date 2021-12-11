/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

const {
  Deprecated_Compile,
  Compile,
  Deprecated_Environment,
  Result,
  Render,
  Deprecated_Source,
  Source,
} = require("../");

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
    const Y = Deprecated_Source.funcWithString(
      "Y",
      "{{ name }}",
      (ast) => async (env, props, children) => env.render(ast, props, children)
    );
    const comps = Result.getExn(
      Deprecated_Compile.Deprecated_Components.make([Y])
    );
    const env = Deprecated_Environment.async;
    const X = Result.getExn(
      Deprecated_Compile.make(
        Deprecated_Source.string("X", "{% Y name /%}"),
        comps
      )
    );
    const x = await X(env, { name: "Carlo" }, {});
    expect(x).toEqual({ NAME: "ok", VAL: "Carlo" });
  });

  test("Async template component errors", async () => {
    const A = Deprecated_Source.funcWithString(
      "A",
      "{{ name }}",
      (ast) => async (env, props, children) => env.render(ast, props, children)
    );
    const comps2 = Result.getExn(
      Deprecated_Compile.Deprecated_Components.make([A])
    );
    const env2 = Deprecated_Environment.async;
    const B = Result.getExn(
      Deprecated_Compile.make(Deprecated_Source.string("B", "{% A /%}"), comps2)
    );
    expect(await B(env2, {}, {})).toEqual({
      NAME: "errors",
      VAL: [
        {
          message: '"name" is type null. I can only echo strings and numbers.',
          path: ["A", "B"],
          location: { character: 4 },
          kind: "Render",
        },
      ],
    });
    const D = Deprecated_Source.func("D", async (_env, _props, _children) => {
      throw new Error("fail.");
    });
    const comps = Result.getExn(
      Deprecated_Compile.Deprecated_Components.make([D])
    );
    const env4 = Deprecated_Environment.async;
    const E = Result.getExn(
      Deprecated_Compile.make(Deprecated_Source.string("E", "{% D /%}"), comps)
    );
    expect(await E(env4, {}, {})).toEqual({
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
  test("env.return", async () => {
    const X = Deprecated_Source.func("X", (env, _props, _children) =>
      env.return("a")
    );
    const comps = Result.getExn(
      Deprecated_Compile.Deprecated_Components.make([X])
    );
    const env = Deprecated_Environment.async;
    const Template = Result.getExn(
      Deprecated_Compile.make(Deprecated_Source.string("", "{% X / %}"), comps)
    );
    expect(await Template(env, {}, {})).toEqual({ NAME: "ok", VAL: "a" });
  });

  test("env.error", async () => {
    const X = Deprecated_Source.func("X", (env, _props, _children) =>
      env.error("e")
    );
    const comps = Result.getExn(
      Deprecated_Compile.Deprecated_Components.make([X])
    );
    const env = Deprecated_Environment.async;
    const Template = Result.getExn(
      Deprecated_Compile.make(
        Deprecated_Source.string("Template", "{% X / %}"),
        comps
      )
    );
    expect(await Template(env, {}, {})).toEqual({
      NAME: "errors",
      VAL: [
        {
          message: "e",
          kind: "Render",
          path: ["Template"],
        },
      ],
    });
  });

  test("env.mapChild", async () => {
    const X = Deprecated_Source.func("X", (env, _props, { Children }) =>
      env.mapChild(Children, (x) => x.toUpperCase())
    );
    const comps = Result.getExn(
      Deprecated_Compile.Deprecated_Components.make([X])
    );
    const env = Deprecated_Environment.async;
    const Template = Result.getExn(
      Deprecated_Compile.make(
        Deprecated_Source.string("", "{% X ~%} a {%~ /X %}"),
        comps
      )
    );
    expect(await Template(env, {}, {})).toEqual({
      NAME: "ok",
      VAL: "A",
    });
    const BadTemplate = Result.getExn(
      Deprecated_Compile.make(
        Deprecated_Source.string("BadTemplate", "{% X %} {{ e }} {% /X %}"),
        comps
      )
    );
    expect(await BadTemplate(env, {}, {})).toEqual({
      NAME: "errors",
      VAL: [
        {
          message: '"e" is type null. I can only echo strings and numbers.',
          location: { character: 12 },
          kind: "Render",
          path: ["section: X#Children", "BadTemplate"],
        },
      ],
    });
  });

  test("env.flatMapChild", async () => {
    const X = Deprecated_Source.func("X", (env, _props, { Children }) =>
      env.flatMapChild(Children, (x) => env.return(x.toUpperCase()))
    );
    const comps = Result.getExn(
      Deprecated_Compile.Deprecated_Components.make([X])
    );
    const env = Deprecated_Environment.async;
    const Template = Result.getExn(
      Deprecated_Compile.make(
        Deprecated_Source.string("", "{% X %} a {% /X %}"),
        comps
      )
    );
    expect(await Template(env, {}, {})).toEqual({
      NAME: "ok",
      VAL: " A ",
    });
    const BadTemplate = Result.getExn(
      Deprecated_Compile.make(
        Deprecated_Source.string("BadTemplate", "{% X %} {{ e }} {% /X %}"),
        comps
      )
    );
    expect(await BadTemplate(env, {}, {})).toEqual({
      NAME: "errors",
      VAL: [
        {
          message: '"e" is type null. I can only echo strings and numbers.',
          location: { character: 12 },
          kind: "Render",
          path: ["section: X#Children", "BadTemplate"],
        },
      ],
    });
  });
});
