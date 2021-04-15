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
  | #ok(_) => []
  | #errors(x) => x
  }

let json = x =>
  try {
    Js.Json.parseExn(x)
  } catch {
  | _ => Js.Json.string("THIS IS EASIER THAN DEALING WITH EXCEPTIONS.")
  }

let render = (~name="", src, json, components) =>
  Source.string(~name, src)
  ->Compile.make(components)
  ->Result.flatMap(f => f(Environment.sync, json, Js.Dict.empty()))
  ->getError

let compile = (~name="", x) =>
  switch Compile.makeAstInternalExn(~name, x) {
  | _ => []
  | exception Debug.Exit(e) => [e]
  }

let dict = Js.Dict.fromArray

let props = dict([
  ("null_", Js.Json.null),
  ("true_", Js.Json.boolean(true)),
  ("false_", Js.Json.boolean(false)),
  ("number", Js.Json.number(1.0)),
  ("string", Js.Json.string("a")),
  ("array", Js.Json.stringArray(["a"])),
  ("emptyObject", Js.Json.object_(Js.Dict.empty())),
  ("object", Js.Json.object_(Js.Dict.fromArray([("a", Js.Json.string("b"))]))),
])

describe("Lexer", ({test, _}) => {
  test("Illegal characters", ({expect, _}) => {
    expect.value(compile(~name="BadCharacter", `a {% match*`)).toMatchSnapshot()
    expect.value(compile(~name="BadCharacter", `a {% .a %}`)).toMatchSnapshot()
    expect.value(compile(~name="BadCharacter", `a {% ..b %}`)).toMatchSnapshot()
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
    expect.value(compile(~name="InvalidEscapeToken", "{{ \"\\a\" }}")).toMatchSnapshot()
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

  test("Components that didn't compile are reported correctly", ({expect, _}) => {
    let a = Source.string(~name="A", "{{")
    let b = Source.string(~name="B", "{%")
    let c = Source.string(~name="c", "ok")
    let components = Compile.Components.make([a, b, c])
    expect.value(components).toMatchSnapshot()
  })

  test("Duplicate components are reported correctly", ({expect, _}) => {
    let a1 = Source.func(~name="A", (env, _, _) => env.return(. ""))
    let a2 = Source.func(~name="A", (env, _, _) => env.return(. ""))
    let components = Compile.Components.make([a1, a2])
    expect.value(components).toMatchSnapshot()
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

  test("Count mismatch failure", ({expect, _}) => {
    expect.value(
      render(
        `{% match a, b, c with 1, 2 %} d {% /match %}`,
        dict([("a", Js.Json.number(1.0)), ("b", Js.Json.number(2.)), ("c", Js.Json.number(3.))]),
        Compile.Components.empty(),
      ),
    ).toMatchSnapshot()
    expect.value(
      render(
        `{% map a with b, c, d %} {{ b }} {% /map %}`,
        dict([("a", Js.Json.numberArray([1., 2.]))]),
        Compile.Components.empty(),
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
        Compile.Components.empty(),
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
        Compile.Components.empty(),
      ),
    ).toMatchSnapshot()
  })

  test("You can't bind a value more than once.", ({expect, _}) => {
    expect.value(
      render(
        "{% match a with [x, x] %} a {% /match %}",
        dict([("a", Js.Json.stringArray(["a", "b"]))]),
        Compile.Components.empty(),
      ),
    ).toMatchSnapshot()
  })

  @raises(Js.Exn.Error)
  test("Illegal bindings", ({expect, _}) => {
    let props = dict([
      ("numberKey", json(`{"1": 1}`)),
      ("nullKey", json(`{"null": 1}`)),
      ("trueKey", json(`{"true": 1}`)),
      ("falseKey", json(`{"false": 1}`)),
    ])
    expect.value(
      render(`{% match numberKey with Abc %} {% /match %}`, props, Compile.Components.empty()),
    ).toMatchSnapshot()
    expect.value(
      render(`{% match nullKey with {null} %} {% /match %}`, props, Compile.Components.empty()),
    ).toMatchSnapshot()
    expect.value(
      render(`{% match trueKey with {true} %} {% /match %}`, props, Compile.Components.empty()),
    ).toMatchSnapshot()
    expect.value(
      render(`{% match falseKey with {false} %} {% /match %}`, props, Compile.Components.empty()),
    ).toMatchSnapshot()
    expect.value(render(`{{ &null }}`, props, Compile.Components.empty())).toMatchSnapshot()
  })

  test("Missing bindings", ({expect, _}) => {
    let props = dict([("a", Js.Json.number(1.0)), ("c", Js.Json.null)])
    let a = Source.func(~name="A", (env, _p, _c) => env.return(. ""))
    let components = Compile.Components.make([a])->Result.getExn
    expect.value(render(`{% A b={b} / %}`, props, components)).toMatchSnapshot()
    expect.value(render(`{% A b=[a, ...b] / %}`, props, components)).toMatchSnapshot()
    expect.value(render(`{% A b=[a, ...c] / %}`, props, components)).toMatchSnapshot()
  })

  describe("Type errors", ({test, _}) => {
    test("number pattern", ({expect, _}) => {
      expect.value(
        render(`{% match string with 1 %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match true_ with 1 %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match false_ with 1 %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match array with 1 %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match emptyObject with 1 %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
    })

    test("string pattern", ({expect, _}) => {
      expect.value(
        render(`{% match number with "a" %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match true_ with "a" %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match false_ with "a" %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match array with "a" %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match emptyObject with "" %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
    })

    test("Boolean pattern", ({expect, _}) => {
      expect.value(
        render(`{% match string with true %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match string with false %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match number with true %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match number with false %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match array with true %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match array with false %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(
          `{% match emptyObject with true %} a {% /match %}`,
          props,
          Compile.Components.empty(),
        ),
      ).toMatchSnapshot()
    })

    test("array pattern", ({expect, _}) => {
      expect.value(
        render(`{% match string with [] %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(
          `{% match string with [1, ...a] %} a {% /match %}`,
          props,
          Compile.Components.empty(),
        ),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match true_ with [] %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match true_ with [1] %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(
          `{% match true_ with [1, ...a] %} a {% /match %}`,
          props,
          Compile.Components.empty(),
        ),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match false_ with [] %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match number with [] %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match emptyObject with [] %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(
          `{% match emptyObject with [1] %} a {% /match %}`,
          props,
          Compile.Components.empty(),
        ),
      ).toMatchSnapshot()
      expect.value(
        render(
          `{% match array with [a, b, ...c] %} a {% /match %}`,
          props,
          Compile.Components.empty(),
        ),
      ).toMatchSnapshot()
      expect.value(
        render(
          `{% match array with [1, ...a] %} a {% /match %}`,
          props,
          Compile.Components.empty(),
        ),
      ).toMatchSnapshot()
    })

    test("Object pattern", ({expect, _}) => {
      expect.value(
        render(`{% match string with {} %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match true_ with {} %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match true_ with {a: 1} %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match false_ with {} %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match array with {} %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match number with {} %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match number with {a: 1} %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
      expect.value(
        render(`{% match object with {a: 1} %} a {% /match %}`, props, Compile.Components.empty()),
      ).toMatchSnapshot()
    })
  })
})

@raises(Failure)
describe("Rendering", ({test, _}) => {
  test("Type mismatches", ({expect, _}) => {
    let data = dict([
      ("a", Js.Json.string("a")),
      ("b", Js.Json.boolean(true)),
      ("c", Js.Json.stringArray([])),
    ])
    expect.value(render("{{ b }}", data, Compile.Components.empty())).toMatchSnapshot()
    expect.value(render("{{ c }}", data, Compile.Components.empty())).toMatchSnapshot()
    expect.value(render("{{ &b }}", data, Compile.Components.empty())).toMatchSnapshot()
  })

  test("Map type mismatches", ({expect, _}) => {
    let data = dict([
      ("a", Js.Json.string("a")),
      ("b", Js.Json.boolean(true)),
      ("c", Js.Json.stringArray([])),
    ])
    expect.value(
      render("{% map a with {a} %}{{ a }}{% /map %}", data, Compile.Components.empty()),
    ).toMatchSnapshot()
    expect.value(
      render("{% map b with {a} %}{{ a }}{% /map %}", data, Compile.Components.empty()),
    ).toMatchSnapshot()
    expect.value(
      render("{% map null with {a} %}{{ a }}{% /map %}", data, Compile.Components.empty()),
    ).toMatchSnapshot()
    expect.value(
      render("{% map [1, 2, ...a] with {a} %}{{ a }}{% /map %}", data, Compile.Components.empty()),
    ).toMatchSnapshot()
    expect.value(
      render("{% map [1, 2, ...z] with {a} %}{{ a }}{% /map %}", data, Compile.Components.empty()),
    ).toMatchSnapshot()
  })

  test("Missing bindings", ({expect, _}) => {
    expect.value(render("{{ z }}", Js.Dict.empty(), Compile.Components.empty())).toMatchSnapshot()
    expect.value(render("{{ Z }}", Js.Dict.empty(), Compile.Components.empty())).toMatchSnapshot()
    expect.value(
      Compile.make(Source.string(~name="", "{% Z / %}"), Compile.Components.empty())->getError,
    ).toMatchSnapshot()
    let a = Source.funcWithString(~name="A", "{{ B }}", (ast, env, props, children) => {
      env.render(. ast, props, children)
    })
    let components = Compile.Components.make([a])->Result.getExn
    let result = render(`{% A B=C / %}`, Js.Dict.empty(), components)
    expect.value(result).toMatchSnapshot()
    // Get some coverage on the map-object error path.
    expect.value(
      render(
        "{% map {a: b} with b %} {{ b }} {% /map %}",
        Js.Dict.empty(),
        Compile.Components.empty(),
      ),
    ).toMatchSnapshot()
  })

  test("Error messages display component name correctly", ({expect, _}) => {
    let a = Source.funcWithString(~name="A", "{{ a }}", (ast, env, props, children) => {
      env.render(. ast, props, children)
    })
    let components = Compile.Components.make([a])->Result.getExn
    let data = Js.Dict.empty()
    let result = render(`{% A / %}`, data, components)
    expect.value(result).toMatchSnapshot()
  })

  test("Multiple render errors are all reported", ({expect, _}) => {
    expect.value(
      render(
        "{{ a }} {{ b }} {{ c }}",
        dict([("a", Js.Json.null), ("b", Js.Json.numberArray([1.0, 2.0]))]),
        Compile.Components.empty(),
      ),
    ).toMatchSnapshot()
  })

  test("Exceptions thrown in components are caught correctly", ({expect, _}) => {
    @raises(Failure)
    let a = Source.func(~name="A", (_env, _props, _children) => {
      raise(Failure("fail."))
    })
    let components = Compile.Components.make([a])->Result.getExn
    let data = Js.Dict.empty()
    let result = render(~name="ExceptionsTest", `{% A / %}`, data, components)
    expect.value(result).toMatchSnapshot()
  })
})

describe("Inputs", ({test, _}) => {
  test("Bad source input", ({expect, _}) => {
    expect.value(
      getError(
        Compile.make(
          Source.string(~name="Template", Obj.magic(1)),
          Compile.Components.empty(),
        )->Result.flatMap(f => f(Environment.sync, Js.Dict.empty(), Js.Dict.empty())),
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
        Compile.Components.empty(),
      ),
    ).toMatchSnapshot()
  })

  test("map", ({expect, _}) => {
    expect.value(
      render(
        ~name="MapTest",
        `{% map a with b %} {{ c }} {% /map %}`,
        dict([("a", Js.Json.numberArray([1.0, 2.0]))]),
        Compile.Components.empty(),
      ),
    ).toMatchSnapshot()
  })

  test("components", ({expect, _}) => {
    let c = Source.funcWithString(~name="C", "{{ c }}", (ast, env, props, children) => {
      env.render(. ast, props, children)
    })
    let b = Source.funcWithString(~name="B", `{% match x with "x" %} {% C /%} {% /match %}`, (
      ast,
      env,
      props,
      templates,
    ) => {
      env.render(. ast, props, templates)
    })
    let components = Compile.Components.make([c, b])->Result.getExn
    let data = dict([("x", Js.Json.string("x"))])
    expect.value(render(~name="A", `{% B x / %}`, data, components)).toMatchSnapshot()
  })

  test("child templates", ({expect, _}) => {
    let b = Source.funcWithString(~name="B", "{{ Children }}", (ast, env, props, children) => {
      env.render(. ast, props, children)
    })
    let components = Compile.Components.make([b])->Result.getExn
    let data = Js.Dict.empty()
    expect.value(render(~name="A", `{% B %} {{ y }} {% /B %}`, data, components)).toMatchSnapshot()
  })
})

@raises(Failure)
describe("Graphs are parsed correctly", ({test, _}) => {
  let emptyComponents = Compile.Components.make([])->Result.getExn
  test("Cyclic dependencies are reported", ({expect, _}) => {
    let a = Source.string(~name="A", "{% B /%}")
    let b = Source.string(~name="B", "{% C /%}")
    let c = Source.string(~name="C", "{% B /%}")
    let result = Compile.Components.make([a, b, c])
    expect.value(result).toEqual(
      #errors([
        {
          exn: None,
          kind: #Compile,
          location: Some({character: 4}),
          message: "Cyclic dependency detected. I can't compile any components in this path.",
          path: [
            Js.Json.string("B"),
            Js.Json.string("C"),
            Js.Json.string("B"),
            Js.Json.string("A"),
          ],
        },
      ]),
    )
  })
  test("Cyclic dependencies are reported (more complex)", ({expect, _}) => {
    let a = Source.string(~name="A", "{% B /%}")
    let b = Source.string(~name="B", "{% C /%}")
    let c = Source.string(~name="C", "{% B /%}")
    let d = Source.string(~name="D", "{% A /%}")
    let result = Compile.Components.make([a, b, c, d])
    expect.value(result).toEqual(
      #errors([
        {
          exn: None,
          kind: #Compile,
          location: Some({character: 4}),
          message: "Cyclic dependency detected. I can't compile any components in this path.",
          path: [
            Js.Json.string("B"),
            Js.Json.string("C"),
            Js.Json.string("B"),
            Js.Json.string("A"),
          ],
        },
        {
          exn: None,
          kind: #Compile,
          location: Some({character: 4}),
          message: `Component "A" either does not exist or couldn't be compiled.`,
          path: [Js.Json.string("D")],
        },
      ]),
    )
  })
  test("Missing dependencies are reported", ({expect, _}) => {
    let a = Source.string(~name="A", "{% B /%}")
    let b = Source.string(~name="B", "{% C /%}")
    let result = Compile.Components.make([a, b])
    expect.value(result).toEqual(
      #errors([
        {
          exn: None,
          kind: #Compile,
          location: Some({character: 4}),
          message: `Component "C" either does not exist or couldn't be compiled.`,
          path: [Js.Json.string("B"), Js.Json.string("A")],
        },
      ]),
    )
  })
  test("Runtime AST errors are reported", ({expect, _}) => {
    let a = Source.string(~name="A", "{% B /%}")

    @raises(Failure)
    let b = Source.funcWithString(~name="B", "", _ => failwith("lol"))
    let result = Compile.Components.make([a, b])
    let e = try {failwith("lol")} catch {
    | e => e
    }
    expect.value(result).toEqual(
      #errors([
        {
          exn: Some(AnyExn(e)),
          kind: #Compile,
          location: None,
          message: "An exception was thrown while compiling this template. This is probably due to malformed input.",
          path: [Js.Json.string("A")],
        },
      ]),
    )
    expect.value(Compile.make(b, emptyComponents)).toEqual(
      #errors([
        {
          exn: Some(AnyExn(e)),
          kind: #Compile,
          location: None,
          message: "An exception was thrown while compiling this template. This is probably due to malformed input.",
          path: [Js.Json.string("B")],
        },
      ]),
    )
  })
})
