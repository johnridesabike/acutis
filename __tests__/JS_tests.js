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
  errorMessage,
  renderContextAsync,
} = require("../src/AcutisJs.bs");

function e(f) {
  try {
    return f();
  } catch (x) {
    return errorMessage(x, undefined);
  }
}

describe("The JS interface works as expected", () => {
  test("Bad input is reported correctly", () => {
    expect(e(() => compile(1))).toEqual(
      "Input template is type number but must be type string."
    );
    function X(render, props, children) {
      return render(makeAst(1), props, children);
    }
    expect(e(() => X(renderContext({}), { name: "Carlo" }))).toEqual(
      "Input template is type number but must be type string."
    );
    function Y(render, props, children) {
      return render("{{ name }}", props, children);
    }
    expect(e(() => Y(renderContext({}), { name: "Carlo" }))).toEqual(
      "The render function didn't receive an AST. Did you forget to compile the source first?"
    );
    expect(e(() => Y("?", { name: "Carlo" }))).toEqual(
      `An unexpected error occured in a template. This is probably due to malformed input.

TypeError: render is not a function`
    );
  });
});

describe("Async templates", () => {
  test("Async context", async () => {
    const X = compile("{{ name }}", "X");
    const renderContext = renderContextAsync({});
    const result = await X(renderContext, { name: "Carlo" }, {});
    expect(result).toEqual("Carlo");
  });

  test("Async template components", async () => {
    const yAst = makeAst("{{ name }}");
    async function Y(render, props, children) {
      return render(yAst, props, children);
    }
    const X = compile("{% Y name /%}", "X");
    const renderContext = renderContextAsync({ Y: Y });
    const result = await X(renderContext, { name: "Carlo" }, {});
    expect(result).toEqual("Carlo");
  });

  test("Async template component errors", async () => {
    async function Y(render, props, children) {
      return render(123, props, children);
    }
    const X = compile("{% Y name /%}", "X");
    const renderContext = renderContextAsync({ Y: Y });
    let result;
    try {
      await X(renderContext, { name: "Carlo" }, {});
    } catch (e) {
      result = errorMessage(e);
    }
    expect(result).toEqual(
      "The render function didn't receive an AST. Did you forget to compile the source first?"
    );
  });
});
