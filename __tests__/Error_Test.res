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

let getError = x =>
  switch x {
  | Ok(_) => failwith("must error!")
  | Error(x) => x
  }

let render = (src, json, components) =>
  getError(Compile.make(src)(. Render.makeContext(components), json, Js.Dict.empty()))

let compile = x => getError(Compile.makeAst(x))

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
    expect.value(compile(`a {% match*`)).toMatchSnapshot()
    expect.value(compile(`a {% . %}`)).toMatchSnapshot()
    expect.value(compile(`a {% raw b %%}`)).toMatchSnapshot()
    expect.value(compile(`a {{ b %}`)).toMatchSnapshot()
    expect.value(compile(`a {{ a b }}`)).toMatchSnapshot()
    expect.value(compile(j`a {{ ? }}`)).toMatchSnapshot()
  })

  test("Illegal binding names", ({expect, _}) => {
    expect.value(compile(`a {{ null }}`)).toMatchSnapshot()
    expect.value(compile(`a {{ false }}`)).toMatchSnapshot()
  })

  test("Other stuff", ({expect, _}) => {
    expect.value(compile(`a {{ "a`)).toMatchSnapshot()
    expect.value(compile(`a {* b`)).toMatchSnapshot()
    expect.value(compile(`a {* b {* c *}`)).toMatchSnapshot()
    expect.value(compile(`a {% b`)).toMatchSnapshot()
    expect.value(compile(`a {{ --1 }}`)).toMatchSnapshot()
  })
})

describe("Parser", ({test, _}) => {
  test("Unexpected tokens", ({expect, _}) => {
    expect.value(compile(`a {% { %}`)).toMatchSnapshot()
    expect.value(compile(`a {% [ %}`)).toMatchSnapshot()
    expect.value(compile(`a {% } %}`)).toMatchSnapshot()
    expect.value(compile(`a {% ] %}`)).toMatchSnapshot()
    expect.value(compile(`a {% , %}`)).toMatchSnapshot()
    expect.value(compile(`a {% map / %}`)).toMatchSnapshot()
    expect.value(compile(`a {% : %}`)).toMatchSnapshot()
    expect.value(compile(`a {% "a" %}`)).toMatchSnapshot()
    expect.value(compile(`a {% 1 %}`)).toMatchSnapshot()
    expect.value(compile(`{% match [ %}`)).toMatchSnapshot()
    expect.value(compile(`{% match 1 %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a, [ %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a, 1 %}`)).toMatchSnapshot()
    expect.value(compile(`a {% match x with true %} b`)).toMatchSnapshot()
    expect.value(compile(`a {% map x with {y} %} {{ y }}`)).toMatchSnapshot()
    expect.value(compile(`a {% / %}`)).toMatchSnapshot()
    expect.value(compile(`{% A b=#%} c {%# / %}`)).toMatchSnapshot()
    expect.value(compile(`{% A B=#%} c {% / %} d`)).toMatchSnapshot()
    expect.value(compile("{% Z A=1 / %}")).toMatchSnapshot()
    expect.value(compile("{% ... %}")).toMatchSnapshot()
    expect.value(compile("{% = %}")).toMatchSnapshot()
    expect.value(compile("{% match ~ %}")).toMatchSnapshot()
    expect.value(compile("{% ~ ~ match %}")).toMatchSnapshot()
  })

  test("Illegal binding name", ({expect, _}) => {
    expect.value(compile(`{% match null %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a, null %}`)).toMatchSnapshot()
  })

  test("Invalid statements", ({expect, _}) => {
    expect.value(compile(`a {% c %}`)).toMatchSnapshot()
  })
})

describe("Patterns", ({test, _}) => {
  test("Unexpected tokens", ({expect, _}) => {
    expect.value(compile(`{% match a with } %} {%/ match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a with {, %} {%/ match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a with {1 %} {%/ match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a with {"a" [] %} {%/ match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a with [1 "a"] %} {%/ match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a with [1, ,] %} {%/ match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a with [1, ..."a"] %} {%/ match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a with [1, ...{] %} {%/ match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a with [1, ...a [] %} {%/ match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a with [1, ...a b] %} {%/ match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a with {a b} %} {%/ match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a with {a ] %} {%/ match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a } %} {%/ match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a match %} %} {%/ match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a with 1 a %} %} {%/ match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a with 1 %}  {% /map %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a with 1 %}  {% /1 %}`)).toMatchSnapshot()
    expect.value(compile(`{% A %}  {% /B %}`)).toMatchSnapshot()
    expect.value(compile(`{% A %}  {% /1 %}`)).toMatchSnapshot()
    expect.value(compile(`{% A 1 /A %}`)).toMatchSnapshot()
  })

  let json = Js.Json.parseExn
  test("Count mismatch failure", ({expect, _}) => {
    expect.value(
      render(
        `{% match a, b, c with 1, 2 %} d {% /match %}`,
        dict([("a", json("1")), ("b", json("2")), ("c", json("3"))]),
        components,
      ),
    ).toMatchSnapshot()
    expect.value(
      render(
        `{% map a with b, c, d %} {{ b }} {% /map %}`,
        dict([("a", json("[1, 2]"))]),
        components,
      ),
    ).toMatchSnapshot()
  })

  test("Match failure", ({expect, _}) => {
    expect.value(
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
    expect.value(
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
    expect.value(
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
    expect.value(
      render(`{% match numberKey with Abc %} {% /match %}`, props, components),
    ).toMatchSnapshot()
    expect.value(
      render(`{% match nullKey with {null} %} {% /match %}`, props, components),
    ).toMatchSnapshot()
    expect.value(
      render(`{% match trueKey with {true} %} {% /match %}`, props, components),
    ).toMatchSnapshot()
    expect.value(
      render(`{% match falseKey with {false} %} {% /match %}`, props, components),
    ).toMatchSnapshot()
    expect.value(
      render(`{% match numberKey with {"1"} %} {% /match %}`, props, components),
    ).toMatchSnapshot()
    expect.value(
      render(`{% map null with {"1"} %} {% /match %}`, props, components),
    ).toMatchSnapshot()
    expect.value(render(`{% raw null %}`, props, components)).toMatchSnapshot()
    expect.value(render(`{% raw 1 %}`, props, components)).toMatchSnapshot()
  })

  test("Missing bindings", ({expect, _}) => {
    let props = dict([("a", Js.Json.number(1.0)), ("c", Js.Json.null)])
    let components = dict([
      ("A", (. render, props, children) => render(. Compile.makeAst(""), props, children)),
    ])
    expect.value(render(`{% A b={b} / %}`, props, components)).toMatchSnapshot()
    expect.value(render(`{% A b=[a, ...b] / %}`, props, components)).toMatchSnapshot()
    expect.value(render(`{% A b=[a, ...c] / %}`, props, components)).toMatchSnapshot()
  })

  describe("Type errors", ({test, _}) => {
    test("number pattern", ({expect, _}) => {
      expect.value(
        render(`{% match string with 1 %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match true_ with 1 %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match false_ with 1 %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match array with 1 %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match object with 1 %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
    })

    test("string pattern", ({expect, _}) => {
      expect.value(
        render(`{% match number with "a" %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match true_ with "a" %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match false_ with "a" %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match array with "a" %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match object with "" %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
    })

    test("Boolean pattern", ({expect, _}) => {
      expect.value(
        render(`{% match string with true %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match string with false %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match number with true %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match number with false %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match array with true %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match array with false %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match object with true %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
    })

    test("array pattern", ({expect, _}) => {
      expect.value(
        render(`{% match string with [] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match string with [1, ...a] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match true_ with [] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match true_ with [1] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match true_ with [1, ...a] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match false_ with [] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match number with [] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match object with [] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match object with [1] %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
    })

    test("Object pattern", ({expect, _}) => {
      expect.value(
        render(`{% match string with {} %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match true_ with {} %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match true_ with {a: 1} %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match false_ with {} %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match array with {} %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match number with {} %} a {% /match %}`, props, components),
      ).toMatchSnapshot()
      expect.value(
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
    expect.value(
      render("{% map a with {a} %}{{ a }}{% /map %}", data, components),
    ).toMatchSnapshot()
    expect.value(
      render("{% map b with {a} %}{{ a }}{% /map %}", data, components),
    ).toMatchSnapshot()
    expect.value(
      render("{% map a with {a} %}{{ a }}{% /map %}", data, components),
    ).toMatchSnapshot()
    expect.value(render("{{ b }}", data, components)).toMatchSnapshot()
    expect.value(render("{{ c }}", data, components)).toMatchSnapshot()
    expect.value(render("{% raw b %}", data, components)).toMatchSnapshot()
  })

  test("Missing bindings", ({expect, _}) => {
    expect.value(render("{{ z }}", Js.Dict.empty(), components)).toMatchSnapshot()
    expect.value(render("{{ Z }}", Js.Dict.empty(), components)).toMatchSnapshot()
    expect.value(render("{% Z / %}", Js.Dict.empty(), components)).toMatchSnapshot()
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

  test("Multiple render errors are all reported", ({expect, _}) => {
    expect.value(
      render(
        "{{ a }} {{ b }} {{ c }}",
        dict([("a", Js.Json.null), ("b", Js.Json.numberArray([1.0, 2.0]))]),
        Js.Dict.empty(),
      ),
    ).toMatchSnapshot()
  })

  test("Exceptions thrown in components are caught correctly", ({expect, _}) => {
    let a = (. _render, _props, _templates) => {
      raise(Failure("fail."))
    }
    let components = dict([("A", a)])
    let data = Js.Dict.empty()
    let result = render(`{% A / %}`, data, components)
    expect.value(result).toMatchSnapshot()
  })
})

describe("Inputs", ({test, _}) => {
  test("Bad source input", ({expect, _}) => {
    expect.value(
      getError(
        Compile.makeJs(. Obj.magic(1), None)(.
          Render.makeContext(Js.Dict.empty()),
          Js.Dict.empty(),
          Js.Dict.empty(),
        ),
      ),
    ).toMatchSnapshot()
  })
})
