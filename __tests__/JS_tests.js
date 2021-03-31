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

const { Compile, Environment, Result, Source } = require("../");

let emptyComponents = Compile.Components.empty();

describe("The JS interface works as expected", () => {
  test("Bad input is reported correctly", () => {
    const X = Source.funcWithString("X", 1, (ast) => (env, props, children) =>
      env.render(ast, props, children)
    );
    expect(Compile.make(X, emptyComponents)).toEqual({
      NAME: "errors",
      VAL: [
        {
          kind: "Compile",
          message:
            "An exception was thrown while compiling this template. This is probably due to malformed input.",
          exn: {
            RE_EXN_ID: "Caml_js_exceptions.Error/2",
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
    const X = Compile.make(Source.string("X", "{{ name }}"), emptyComponents);
    const env = Environment.async;
    const result = Result.map(X, (X) => X(env, { name: "Carlo" }, {}));
    const x = await Result.getExn(result);
    expect(x).toEqual({ NAME: "ok", VAL: "Carlo" });
  });

  test("Async template components", async () => {
    const Y = Source.funcWithString(
      "Y",
      "{{ name }}",
      (ast) => async (env, props, children) => env.render(ast, props, children)
    );
    const comps = Result.getExn(Compile.Components.make([Y]));
    const env = Environment.async;
    const X = Result.getExn(
      Compile.make(Source.string("X", "{% Y name /%}"), comps)
    );
    const x = await X(env, { name: "Carlo" }, {});
    expect(x).toEqual({ NAME: "ok", VAL: "Carlo" });
  });

  test("Async template component errors", async () => {
    const A = Source.funcWithString(
      "A",
      "{{ name }}",
      (ast) => async (env, props, children) => env.render(ast, props, children)
    );
    const comps2 = Result.getExn(Compile.Components.make([A]));
    const env2 = Environment.async;
    const B = Result.getExn(
      Compile.make(Source.string("B", "{% A /%}"), comps2)
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
    const D = Source.func("D", async (_env, _props, _children) => {
      throw new Error("fail.");
    });
    const comps = Result.getExn(Compile.Components.make([D]));
    const env4 = Environment.async;
    const E = Result.getExn(
      Compile.make(Source.string("E", "{% D /%}"), comps)
    );
    expect(await E(env4, {}, {})).toEqual({
      NAME: "errors",
      VAL: [
        {
          message: `An exception was thrown while rendering a template component.`,
          kind: "Render",
          path: ["E"],
          exn: {
            RE_EXN_ID: "Promise.JsError/3",
            _1: new Error("fail."),
          },
        },
      ],
    });
  });
});

describe("Async helper functions", () => {
  test("env.return", async () => {
    const X = Source.func("X", (env, _props, _children) => env.return("a"));
    const comps = Result.getExn(Compile.Components.make([X]));
    const env = Environment.async;
    const Template = Result.getExn(
      Compile.make(Source.string("", "{% X / %}"), comps)
    );
    expect(await Template(env, {}, {})).toEqual({ NAME: "ok", VAL: "a" });
  });

  test("env.error", async () => {
    const X = Source.func("X", (env, _props, _children) => env.error("e"));
    const comps = Result.getExn(Compile.Components.make([X]));
    const env = Environment.async;
    const Template = Result.getExn(
      Compile.make(Source.string("Template", "{% X / %}"), comps)
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
    const X = Source.func("X", (env, _props, { Children }) =>
      env.mapChild(Children, (x) => x.toUpperCase())
    );
    const comps = Result.getExn(Compile.Components.make([X]));
    const env = Environment.async;
    const Template = Result.getExn(
      Compile.make(Source.string("", "{% X ~%} a {%~ /X %}"), comps)
    );
    expect(await Template(env, {}, {})).toEqual({
      NAME: "ok",
      VAL: "A",
    });
    const BadTemplate = Result.getExn(
      Compile.make(
        Source.string("BadTemplate", "{% X %} {{ e }} {% /X %}"),
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
    const X = Source.func("X", (env, _props, { Children }) =>
      env.flatMapChild(Children, (x) => env.return(x.toUpperCase()))
    );
    const comps = Result.getExn(Compile.Components.make([X]));
    const env = Environment.async;
    const Template = Result.getExn(
      Compile.make(Source.string("", "{% X %} a {% /X %}"), comps)
    );
    expect(await Template(env, {}, {})).toEqual({
      NAME: "ok",
      VAL: " A ",
    });
    const BadTemplate = Result.getExn(
      Compile.make(
        Source.string("BadTemplate", "{% X %} {{ e }} {% /X %}"),
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
