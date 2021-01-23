/**
   Copyright 2021 John Jackson

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
  | #data(_) => raise(Not_found)
  | #errors(x) => x
  }

let render = (~name=?, src, json, components) =>
  getError(Compile.make(src, ~name?)(. Environment.make(components), json, Js.Dict.empty()))

let compile = (~name=?, x) =>
  x->Compile.makeAst(~name?)->Acutis_Types.Valid.validate->Belt.Option.getExn->getError

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
    expect.value(compile(~name="BadCharacter", `a {% match*`)).toMatchSnapshot()
    expect.value(compile(~name="BadCharacter", `a {% . %}`)).toMatchSnapshot()
    expect.value(compile(`a {% match a %%}`)).toMatchSnapshot()
    expect.value(compile(`a {{ b %}`)).toMatchSnapshot()
    expect.value(compile(`a {{ a b }}`)).toMatchSnapshot()
    expect.value(compile(~name="BadCharacter", `a {{ ? }}`)).toMatchSnapshot()
    expect.value(compile(`a {{ a }%}`)).toMatchSnapshot()
  })

  test("Illegal binding names", ({expect, _}) => {
    expect.value(compile(`a {{ null }}`)).toMatchSnapshot()
    expect.value(compile(`a {{ false }}`)).toMatchSnapshot()
  })

  test("Other stuff", ({expect, _}) => {
    expect.value(compile(~name="BadString", `a {{ "a`)).toMatchSnapshot()
    expect.value(compile(`a {{ &"a`)).toMatchSnapshot()
    expect.value(compile(`a {* b`)).toMatchSnapshot()
    expect.value(compile(~name="BadComment", `a {* b {* c *}`)).toMatchSnapshot()
    expect.value(compile(`a {% b`)).toMatchSnapshot()
    expect.value(compile(~name="BadIdentifier", `a {{ --1 }}`)).toMatchSnapshot()
  })
})

describe("Parser", ({test, _}) => {
  test("Unexpected tokens", ({expect, _}) => {
    expect.value(compile(~name="BadToken", `a {% { %}`)).toMatchSnapshot()
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
    expect.value(compile("{{ a & b }}")).toMatchSnapshot()
  })

  test("Illegal binding name", ({expect, _}) => {
    expect.value(compile(~name="BadBinding", `{% match null %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a, null %}`)).toMatchSnapshot()
    expect.value(compile(`{% match a with [x, ...null] %}`)).toMatchSnapshot()
    expect.value(compile(`{% A null=1 /%}`)).toMatchSnapshot()
  })

  test("Invalid statements", ({expect, _}) => {
    expect.value(compile(~name="BadStatement", `a {% c %}`)).toMatchSnapshot()
  })

  test("You can't unescape child templates", ({expect, _}) => {
    expect.value(compile("{{ &A }}")).toMatchSnapshot()
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
    expect.value(render(`{{ &null }}`, props, components)).toMatchSnapshot()
  })

  test("Missing bindings", ({expect, _}) => {
    let props = dict([("a", Js.Json.number(1.0)), ("c", Js.Json.null)])
    let components = dict([
      (
        "A",
        (. env: Acutis_Types.environment<'a>, props, children) =>
          env.render(. Compile.makeAst(""), props, children),
      ),
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
    expect.value(render("{{ &b }}", data, components)).toMatchSnapshot()
  })

  test("Missing bindings", ({expect, _}) => {
    expect.value(render("{{ z }}", Js.Dict.empty(), components)).toMatchSnapshot()
    expect.value(render("{{ Z }}", Js.Dict.empty(), components)).toMatchSnapshot()
    expect.value(render("{% Z / %}", Js.Dict.empty(), components)).toMatchSnapshot()
    let a: Acutis_Types.template<_> = (. env, props, templates) => {
      env.render(. Compile.makeAst("{{ B }}"), props, templates)
    }
    let result = render(`{% A B=C / %}`, Js.Dict.empty(), dict([("A", a)]))
    expect.value(result).toMatchSnapshot()
  })

  test("Error messages display component name correctly", ({expect, _}) => {
    let a: Acutis_Types.template<_> = (. env, props, templates) => {
      env.render(. Compile.makeAst("{{ a }}", ~name="A"), props, templates)
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
    let a = (. _env, _props, _children) => {
      raise(Failure("fail."))
    }
    let components = dict([("A", a)])
    let data = Js.Dict.empty()
    let result = render(~name="ExceptionsTest", `{% A / %}`, data, components)
    expect.value(result).toMatchSnapshot()
  })
})

describe("Inputs", ({test, _}) => {
  test("Bad source input", ({expect, _}) => {
    expect.value(
      getError(
        Compile.make(Obj.magic(1))(.
          Environment.make(Js.Dict.empty()),
          Js.Dict.empty(),
          Js.Dict.empty(),
        ),
      ),
    ).toMatchSnapshot()
  })
})

describe("Stack trace is rendered correctly", ({test, _}) => {
  test("match", ({expect, _}) => {
    expect.value(
      render(
        ~name="MatchTest",
        `{% match a with 1.0 %} {% match a with 1.0 %} {{ b }} {% /match %} {% /match %}`,
        dict([("a", Js.Json.number(1.0))]),
        Js.Dict.empty(),
      ),
    ).toMatchSnapshot()
  })

  test("map", ({expect, _}) => {
    expect.value(
      render(
        ~name="MapTest",
        `{% map a with b %} {{ c }} {% /map %}`,
        dict([("a", Js.Json.numberArray([1.0, 2.0]))]),
        Js.Dict.empty(),
      ),
    ).toMatchSnapshot()
  })

  test("components", ({expect, _}) => {
    let c: Acutis_Types.template<_> = (. env, props, templates) => {
      env.render(. Compile.makeAst("{{ c }}", ~name="C"), props, templates)
    }
    let b: Acutis_Types.template<_> = (. env, props, templates) => {
      env.render(.
        Compile.makeAst(`{% match x with "x" %} {% C /%} {% /match %}`, ~name="B"),
        props,
        templates,
      )
    }
    let components = dict([("C", c), ("B", b)])
    let data = dict([("x", Js.Json.string("x"))])
    expect.value(render(~name="A", `{% B x / %}`, data, components)).toMatchSnapshot()
  })

  test("child templates", ({expect, _}) => {
    let b: Acutis_Types.template<_> = (. env, props, templates) => {
      env.render(. Compile.makeAst(`{{ Children }}`, ~name="B"), props, templates)
    }
    let components = dict([("B", b)])
    let data = Js.Dict.empty()
    expect.value(render(~name="A", `{% B %} {{ y }} {% /B %}`, data, components)).toMatchSnapshot()
  })
})
