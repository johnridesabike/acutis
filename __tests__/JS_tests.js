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

const {
  compile,
  makeAst,
  renderContext,
  renderContextAsync,
  result,
} = require("../src/AcutisJs.bs");

describe("The JS interface works as expected", () => {
  test("Bad input is reported correctly", () => {
    function X(render, props, children) {
      return render(makeAst(1), props, children);
    }
    expect(result(X(renderContext({}), { name: "Carlo" }))).toEqual({
      data: null,
      errors: [
        {
          kind: "Compile",
          message:
            "An exception was thrown while rendering this template. This is probably due to malformed input.",
          exn: {
            RE_EXN_ID: "Caml_js_exceptions.Error/2",
            _1: new TypeError("source.str.charAt is not a function"),
          },
        },
      ],
    });
    function Y(render, props, children) {
      return render("{{ name }}", props, children);
    }
    expect(result(Y(renderContext({}), { name: "Carlo" }, {}))).toEqual({
      data: null,
      errors: [
        {
          message: `An exception was thrown while rendering this template. This is probably due to malformed input.`,
          kind: "Render",
          exn: {
            RE_EXN_ID: "Caml_js_exceptions.Error/2",
            _1: new TypeError("Cannot read property 'ast' of undefined"),
          },
        },
      ],
    });
  });
});

describe("Async templates", () => {
  test("Async context", async () => {
    const X = compile("{{ name }}", "X");
    const renderContext = renderContextAsync({});
    const x = await X(renderContext, { name: "Carlo" }, {});
    expect(result(x)).toEqual({ data: "Carlo", errors: [] });
  });

  test("Async template components", async () => {
    const yAst = makeAst("{{ name }}");
    async function Y(render, props, children) {
      return render(yAst, props, children);
    }
    const X = compile("{% Y name /%}", "X");
    const renderContext = renderContextAsync({ Y: Y });
    const x = await X(renderContext, { name: "Carlo" }, {});
    expect(result(x)).toEqual({ data: "Carlo", errors: [] });
  });

  test("Async template component errors", async () => {
    async function Y(render, props, children) {
      return render(123, props, children);
    }
    const X = compile("{% Y name /%}", "X");
    const renderContext = renderContextAsync({ Y: Y });
    expect(result(await X(renderContext, { name: "Carlo" }, {}))).toEqual({
      data: null,
      errors: [
        {
          message: `An exception was thrown while rendering this template. This is probably due to malformed input.`,
          kind: "Render",
          exn: {
            RE_EXN_ID: "Caml_js_exceptions.Error/2",
            _1: new TypeError("Cannot read property 'ast' of undefined"),
          },
        },
      ],
    });
    async function A(render, props, children) {
      return render(makeAst("{{ name }}", "A"), props, children);
    }
    const B = compile("{% A /%}", "B");
    const renderContext2 = renderContextAsync({ A: A });
    expect(result(await B(renderContext2, {}, {}))).toEqual({
      data: null,
      errors: [
        {
          message: '"name" is type null. I can only echo strings and numbers.',
          template: "A",
          location: { character: 4 },
          kind: "Render",
        },
      ],
    });
    const C = compile("{{ a", "C");
    const renderContext3 = renderContextAsync({});
    expect(result(await C(renderContext3, {}, {}))).toEqual({
      data: null,
      errors: [
        {
          message: 'Unexpected character: "". Expected: "}}".',
          template: "C",
          location: { character: 7 },
          kind: "Syntax",
        },
      ],
    });
    async function D(render, props, children) {
      throw new Error("fail.");
    }
    const E = compile("{% D /%}", "E");
    const renderContext4 = renderContextAsync({ D: D });
    expect(result(await E(renderContext4, {}, {}))).toEqual({
      data: null,
      errors: [
        {
          message: `An exception was thrown while rendering this template. This is probably due to malformed input.`,
          kind: "Render",
          template: "D",
          exn: new Error("fail."),
        },
      ],
    });
  });
});
