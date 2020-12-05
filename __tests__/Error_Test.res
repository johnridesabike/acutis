/**
   Copyright 2020 John Jackson

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
open TestFramework

let parse = src =>
  try {
    ignore(Compile.make(src))
    "Nothing raised"
  } catch {
  | x => Debug.errorMessage(x)
  }

let render = (src, json, components) =>
  try {
    Compile.make(src)(. Render.makeContext(components), json, Js.Dict.empty())
  } catch {
  | x => Debug.errorMessage(x)
  }

let components = Js.Dict.empty()
let dict = Js.Dict.fromArray

let props = dict([
  ("null_", Js.Json.null),
  ("true_", Js.Json.boolean(true)),
  ("false_", Js.Json.boolean(false)),
  ("number", Js.Json.number(1.0)),
  ("string", Js.Json.string("a")),
  ("array", Js.Json.stringArray(["a"])),
  ("object", Js.Json.object_(Js.Dict.empty())),
])

describe("Lexer", ({test, _}) => {
  test("Illegal characters", ({expect, _}) => {
    expect.string(parse(`a {% match*`)).toMatchSnapshot()
    expect.string(parse(`a {% . %}`)).toMatchSnapshot()
    expect.string(parse(`a {% raw b %%}`)).toMatchSnapshot()
    expect.string(parse(`a {{ b %}`)).toMatchSnapshot()
    expect.string(parse(`a {{ a b }}`)).toMatchSnapshot()
    expect.string(parse(j`a {{ ? }}`)).toMatchSnapshot()
  })
  test("Illegal binding names", ({expect, _}) => {
    expect.string(parse(`a {{ null }}`)).toMatchSnapshot()
    expect.string(parse(`a {{ false }}`)).toMatchSnapshot()
  })
  test("Other stuff", ({expect, _}) => {
    expect.string(parse(`a {{ "a`)).toMatchSnapshot()
    expect.string(parse(`a {* b`)).toMatchSnapshot()
    expect.string(parse(`a {* b {* c *}`)).toMatchSnapshot()
    expect.string(parse(`a {% b`)).toMatchSnapshot()
    expect.string(parse(`a {{ --1 }}`)).toMatchSnapshot()
  })
})

describe("Parser", ({test, _}) => {
  test("Unexpected tokens", ({expect, _}) => {
    expect.string(parse(`a {% { %}`)).toMatchSnapshot()
    expect.string(parse(`a {% [ %}`)).toMatchSnapshot()
    expect.string(parse(`a {% } %}`)).toMatchSnapshot()
    expect.string(parse(`a {% ] %}`)).toMatchSnapshot()
    expect.string(parse(`a {% , %}`)).toMatchSnapshot()
    expect.string(parse(`a {% map / %}`)).toMatchSnapshot()
    expect.string(parse(`a {% : %}`)).toMatchSnapshot()
    expect.string(parse(`a {% "a" %}`)).toMatchSnapshot()
    expect.string(parse(`a {% 1 %}`)).toMatchSnapshot()
    expect.string(parse(`{% match [ %}`)).toMatchSnapshot()
    expect.string(parse(`{% match 1 %}`)).toMatchSnapshot()
    expect.string(parse(`{% match null %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a, [ %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a, 1 %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a, null %}`)).toMatchSnapshot()
    expect.string(parse(`a {% match x with true %} b`)).toMatchSnapshot()
    expect.string(parse(`a {% map x with {y} %} {{ y }}`)).toMatchSnapshot()
    expect.string(parse(`a {% / %}`)).toMatchSnapshot()
    expect.string(parse(`{% A b=#%} c {%# / %}`)).toMatchSnapshot()
    expect.string(parse(`{% A B=#%} c {% / %} d`)).toMatchSnapshot()
    expect.string(parse("{% Z A=1 / %}")).toMatchSnapshot()
    expect.string(parse("{% ... %}")).toMatchSnapshot()
    expect.string(parse("{% = %}")).toMatchSnapshot()
    expect.string(parse("{% match ~ %}")).toMatchSnapshot()
    expect.string(parse("{% ~ ~ match %}")).toMatchSnapshot()
  })

  test("Invalid statements", ({expect, _}) => {
    expect.string(parse(`a {% c %}`)).toMatchSnapshot()
  })
})

describe("Patterns", ({test, _}) => {
  test("Unexpected tokens", ({expect, _}) => {
    expect.string(parse(`{% match a with } %} {%/ match %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a with {, %} {%/ match %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a with {1 %} {%/ match %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a with {"a" [] %} {%/ match %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a with [1 "a"] %} {%/ match %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a with [1, ,] %} {%/ match %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a with [1, ..."a"] %} {%/ match %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a with [1, ...{] %} {%/ match %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a with [1, ...a [] %} {%/ match %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a with [1, ...a b] %} {%/ match %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a with {a b} %} {%/ match %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a with {a ] %} {%/ match %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a } %} {%/ match %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a match %} %} {%/ match %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a with 1 a %} %} {%/ match %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a with 1 %}  {% /map %}`)).toMatchSnapshot()
    expect.string(parse(`{% match a with 1 %}  {% /1 %}`)).toMatchSnapshot()
    expect.string(parse(`{% A %}  {% /B %}`)).toMatchSnapshot()
    expect.string(parse(`{% A %}  {% /1 %}`)).toMatchSnapshot()
    expect.string(parse(`{% A 1 /A %}`)).toMatchSnapshot()
  })

  let json = Js.Json.parseExn
  test("Count mismatch failure", ({expect, _}) => {
    expect.string(
      render(
        `{% match a, b, c with 1, 2 %} d {% /match %}`,
        dict([("a", json("1")), ("b", json("2")), ("c", json("3"))]),
        components,
      ),
    ).toMatchSnapshot()
    expect.string(
      render(
        `{% map a with b, c, d %} {{ b }} {% /map %}`,
        dict([("a", json("[1, 2]"))]),
        components,
      ),
    ).toMatchSnapshot()
  })
  test("Match failure", ({expect, _}) => {
    expect.string(
      render(
        `
{% match a
   with {firstName: name, favoriteColor}
   with {lastName: name} %}
  {{name}}'s favorite color is {{favoriteColor}}
{% /match %}`,
        dict([("a", json(`{"name": "John", "favoriteColor": "green"}`))]),
        components,
      ),
    ).toMatchSnapshot()
    expect.string(
      render(
        `
{% map a
   with {firstName: name, favoriteColor}
   with {lastName: name} %}
  {{name}}'s favorite color is {{favoriteColor}}
{% /map %}`,
        dict([("a", json(`[{"name": "John", "favoriteColor": "green"}]`))]),
        components,
      ),
    ).toMatchSnapshot()
  })

  test("You can't bind a value more than once.", ({expect, _}) => {
    expect.string(
      render(
        "{% match a with [x, x] %} a {% /match %}",
        dict([("a", Js.Json.stringArray(["a", "b"]))]),
        components,
      ),
    ).toMatchSnapshot()
  })

  test("Illegal bindings", ({expect, _}) => {
    let props = dict([
      ("numberKey", json(`{"1": 1}`)),
      ("nullKey", json(`{"null": 1}`)),
      ("trueKey", json(`{"true": 1}`)),
      ("falseKey", json(`{"false": 1}`)),
    ])
    expect.string(
      render(`{% match numberKey with Abc %} {% /match %}`, props, components),
    ).toMatchSnapshot()
    expect.string(
      render(`{% match nullKey with {null} %} {% /match %}`, props, components),
    ).toMatchSnapshot()
    expect.string(
      render(`{% match trueKey with {true} %} {% /match %}`, props, components),
    ).toMatchSnapshot()
    expect.string(
      render(`{% match falseKey with {false} %} {% /match %}`, props, components),
    ).toMatchSnapshot()
    expect.string(
      render(`{% match numberKey with {"1"} %} {% /match %}`, props, components),
    ).toMatchSnapshot()
    expect.string(
      render(`{% map null with {"1"} %} {% /match %}`, props, components),
    ).toMatchSnapshot()
    expect.string(render(`{% raw null %}`, props, components)).toMatchSnapshot()
    expect.string(render(`{% raw 1 %}`, props, components)).toMatchSnapshot()
  })

  test("Missing bindings", ({expect, _}) => {
    let props = dict([("a", Js.Json.number(1.0)), ("c", Js.Json.null)])
    let components = dict([("A", (. _render, _props, _templates) => "")])
    expect.string(render(`{% A b={b} / %}`, props, components)).toMatchSnapshot()
    expect.string(render(`{% A b=[a, ...b] / %}`, props, components)).toMatchSnapshot()
    expect.string(render(`{% A b=[a, ...c] / %}`, props, components)).toMatchSnapshot()
  })

  describe("Type errors", ({test, _}) => {
    test("number pattern", ({expect, _}) => {
      expect.string(
        render(`{% match string with 1 %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match true_ with 1 %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match false_ with 1 %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match array with 1 %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match object with 1 %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
    })
    test("string pattern", ({expect, _}) => {
      expect.string(
        render(`{% match number with "a" %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match true_ with "a" %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match false_ with "a" %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match array with "a" %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match object with "" %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
    })
    test("Boolean pattern", ({expect, _}) => {
      expect.string(
        render(`{% match string with true %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match string with false %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match number with true %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match number with false %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match array with true %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match array with false %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match object with true %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
    })
    test("array pattern", ({expect, _}) => {
      expect.string(
        render(`{% match string with [] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match string with [1, ...a] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match true_ with [] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match true_ with [1] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match true_ with [1, ...a] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match false_ with [] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match number with [] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match object with [] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match object with [1] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
    })
    test("Object pattern", ({expect, _}) => {
      expect.string(
        render(`{% match string with {} %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match true_ with {} %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match true_ with {a: 1} %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match false_ with {} %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match array with {} %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match number with {} %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.string(
        render(`{% match number with {a: 1} %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
    })
  })
})

describe("Rendering", ({test, _}) => {
  test("Type mismatches", ({expect, _}) => {
    let data = dict([
      ("a", Js.Json.string("a")),
      ("b", Js.Json.boolean(true)),
      ("c", Js.Json.stringArray([])),
    ])
    expect.string(
      render("{% map a with {a} %}{{ a }}{% /map %}", data, components),
    ).toMatchSnapshot()
    expect.string(
      render("{% map b with {a} %}{{ a }}{% /map %}", data, components),
    ).toMatchSnapshot()
    expect.string(
      render("{% map a with {a} %}{{ a }}{% /map %}", data, components),
    ).toMatchSnapshot()
    expect.string(render("{{ b }}", data, components)).toMatchSnapshot()
    expect.string(render("{{ c }}", data, components)).toMatchSnapshot()
    expect.string(render("{% raw b %}", data, components)).toMatchSnapshot()
  })

  test("Missing bindings", ({expect, _}) => {
    expect.string(render("{{ z }}", Js.Dict.empty(), components)).toMatchSnapshot()
    expect.string(render("{{ Z }}", Js.Dict.empty(), components)).toMatchSnapshot()
    expect.string(render("{% Z / %}", Js.Dict.empty(), components)).toMatchSnapshot()
    let a = (. render, props, templates) => {
      render(. Compile.makeAst("{{ B }}"), props, templates)
    }
    let result = render(`{% A B=C / %}`, Js.Dict.empty(), dict([("A", a)]))
    expect.value(result).toMatchSnapshot()
  })

  test("Error messages display component name correctly", ({expect, _}) => {
    let a = (. render, props, templates) => {
      render(. Compile.makeAst("{{ a }}", ~name="A"), props, templates)
    }
    let components = dict([("A", a)])
    let data = Js.Dict.empty()
    let result = render(`{% A / %}`, data, components)
    expect.value(result).toMatchSnapshot()
  })
})

describe("Inputs", ({test, _}) => {
  let parse = src =>
    try {
      ignore(Compile.makeJs(. src, None))
      "Nothing raised"
    } catch {
    | x => Debug.errorMessage(x)
    }
  test("Bad source input", ({expect, _}) => {
    expect.string(parse(Obj.magic(1))).toMatchSnapshot()
  })
})
