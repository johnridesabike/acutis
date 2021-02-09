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

const { Compile, Environment } = require("../");

describe("The JS interface works as expected", () => {
  test("Bad input is reported correctly", () => {
    function X(env, props, children) {
      return env.render(Compile.makeAst(1, "X"), props, children);
    }
    expect(X(Environment.make({}), { name: "Carlo" })).toEqual({
      NAME: "errors",
      VAL: [
        {
          kind: "Compile",
          message:
            "An exception was thrown while rendering this template. This is probably due to malformed input.",
          exn: {
            RE_EXN_ID: "Caml_js_exceptions.Error/2",
            _1: new TypeError("source.str.charAt is not a function"),
          },
          path: ["X"],
        },
      ],
    });
    function Y(env, props, children) {
      return env.render("{{ name }}", props, children);
    }
    expect(Y(Environment.make({}), { name: "Carlo" }, {})).toEqual({
      NAME: "errors",
      VAL: [
        {
          kind: "Render",
          message:
            "A template AST was not valid. Did you forget to compile one?",
          path: [],
        },
      ],
    });
  });
});

describe("Async templates", () => {
  test("Async env", async () => {
    const X = Compile.make("{{ name }}", "X");
    const env = Environment.Async.make({});
    const x = await X(env, { name: "Carlo" }, {});
    expect(x).toEqual({ NAME: "data", VAL: "Carlo" });
  });

  test("Async template components", async () => {
    const yAst = Compile.makeAst("{{ name }}");
    async function Y(env, props, children) {
      return env.render(yAst, props, children);
    }
    const X = Compile.make("{% Y name /%}", "X");
    const env = Environment.Async.make({ Y: Y });
    const x = await X(env, { name: "Carlo" }, {});
    expect(x).toEqual({ NAME: "data", VAL: "Carlo" });
  });

  test("Async template component errors", async () => {
    async function Y(env, props, children) {
      return env.render(123, props, children);
    }
    const X = Compile.make("{% Y name /%}", "X");
    const env = Environment.Async.make({ Y: Y });
    expect(await X(env, { name: "Carlo" }, {})).toEqual({
      NAME: "errors",
      VAL: [
        {
          kind: "Render",
          message:
            "A template AST was not valid. Did you forget to compile one?",
          path: ["X"],
        },
      ],
    });
    async function A(env, props, children) {
      return env.render(Compile.makeAst("{{ name }}", "A"), props, children);
    }
    const B = Compile.make("{% A /%}", "B");
    const env2 = Environment.Async.make({ A: A });
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
    const C = Compile.make("{{ a", "C");
    const env3 = Environment.Async.make({});
    expect(await C(env3, {}, {})).toEqual({
      NAME: "errors",
      VAL: [
        {
          message: "Unexpected end of file.",
          location: { character: 5 },
          kind: "Syntax",
          path: ["C"],
        },
      ],
    });
    async function D(_env, _props, _children) {
      throw new Error("fail.");
    }
    const E = Compile.make("{% D /%}", "E");
    const env4 = Environment.Async.make({ D: D });
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
    const X = (env, _props, _children) => {
      return env.return("a");
    };
    const Template = Compile.make("{% X / %}");
    const env = Environment.Async.make({ X: X });
    expect(await Template(env, {}, {})).toEqual({ NAME: "data", VAL: "a" });
  });

  test("env.error", async () => {
    const X = (env, _props, _children) => {
      return env.error("e");
    };
    const Template = Compile.make("{% X / %}");
    const env = Environment.Async.make({ X: X });
    expect(await Template(env, {}, {})).toEqual({
      NAME: "errors",
      VAL: [
        {
          message: "e",
          kind: "Render",
          path: [null],
        },
      ],
    });
  });

  test("env.mapChild", async () => {
    const X = (env, _props, { Children }) => {
      return env.mapChild(Children, (x) => x.toUpperCase());
    };
    const env = Environment.Async.make({ X: X });
    const Template = Compile.make("{% X ~%} a {%~ /X %}");
    expect(await Template(env, {}, {})).toEqual({
      NAME: "data",
      VAL: "A",
    });
    const BadTemplate = Compile.make("{% X %} {{ e }} {% /X %}", "BadTemplate");
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
    const X = (env, _props, { Children }) => {
      return env.flatMapChild(Children, (x) => env.return(x.toUpperCase()));
    };
    const env = Environment.Async.make({ X: X });
    const Template = Compile.make("{% X %} a {% /X %}");
    expect(await Template(env, {}, {})).toEqual({
      NAME: "data",
      VAL: " A ",
    });
    const BadTemplate = Compile.make("{% X %} {{ e }} {% /X %}", "BadTemplate");
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
