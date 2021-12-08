/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
open TestFramework

let getError = x =>
  switch x {
  | #ok(_) => []
  | #errors(x) => x
  }

let dict = Js.Dict.fromArray

let compile = (~name="", src) =>
  switch Compile2.make(~name, src, Compile2.Components.empty()) {
  | #ok(_) => []
  | #errors(e) => e
  }

let render = (~name="", ~children=Js.Dict.empty(), src, props, components) => {
  let r =
    Compile2.Components.make(components)
    ->Result.flatMap(Compile2.make(~name, src))
    ->Result.flatMap(t => Render2.sync(t, props, children))
  switch r {
  | #ok(_) =>
    Js.log2("lol", r)
    assert false
  | #errors(e) => e
  }
}

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
    expect.value(compile(`{% match a, b, c with 1, 2 %} d {% /match %}`)).toMatchSnapshot()
    expect.value(compile(`{% map a with b, c, d %} {{ b }} {% /map %}`)).toMatchSnapshot()
  })

  test("Non-exhuastive matching", ({expect, _}) => {
    let r = compile(`
      {% match a with {firstName: name, favoriteColor: "green"} %}
        {{ name }}'s favorite color is green.
      {% /match %}`)
    expect.value(r).toMatchSnapshot()
  })

  test("You can't bind a value more than once.", ({expect, _}) => {
    expect.value(
      compile("{% match a with [x, x] %} a {% with _ %} b {% /match %}"),
    ).toMatchSnapshot()
  })

  test("Illegal bindings", ({expect, _}) => {
    expect.value(compile(`{% match numberKey with Abc %} {% /match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match nullKey with {null} %} {% /match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match trueKey with {true} %} {% /match %}`)).toMatchSnapshot()
    expect.value(compile(`{% match falseKey with {false} %} {% /match %}`)).toMatchSnapshot()
    expect.value(compile(`{{ &null }}`)).toMatchSnapshot()
  })

  test("Missing bindings", ({expect, _}) => {
    let props = dict([("a", Js.Json.number(1.0)), ("c", Js.Json.null)])
    let a = Source2.function(
      ~name="A",
      Typescheme.props([("b", Typescheme.record([("b", Typescheme.string())]))]),
      Typescheme.Child.props([]),
      (type a, module(Env): Source2.env<a>, _p, _c) => Env.return(. ""),
    )
    let z = Source2.function(
      ~name="Z",
      Typescheme.props([("b", Typescheme.list(Typescheme.int()))]),
      Typescheme.Child.props([]),
      (type a, module(Env): Source2.env<a>, _p, _c) => Env.return(. ""),
    )
    expect.value(render(`{% A b={b} / %}`, props, [a])).toMatchSnapshot()
    expect.value(render(`{% Z b=[a, ...b] / %}`, props, [z])).toMatchSnapshot()
    expect.value(render(`{% Z b=[a, ...c] / %}`, props, [z])).toMatchSnapshot()
  })

  describe("Type errors", ({test, _}) => {
    test("number pattern", ({expect, _}) => {
      expect.value(
        compile(`{% match a with "a" %} a {% with 1 %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with true %} a {% with 1 %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with false %} a {% with 1 %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with [] %} a {% with 1 %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with {a} %} {{ a }} {% with 1 %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
    })

    test("string pattern", ({expect, _}) => {
      expect.value(
        compile(`{% match a with 1 %} a {% with "a" %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with true %} a {% with "a" %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with false %} a {% with "a" %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with [1] %} a {% with "a" %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with {a} %} {{ a }} {% with "a" %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
    })

    test("Boolean pattern", ({expect, _}) => {
      expect.value(
        compile(`{% match a with "a" %} a {% with true %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with "a" %} a {% with false %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with 1 %} a {% with true %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with 1 %} a {% with false %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with [1] %} a {% with true %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with [1] %} a {% with false %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with {a} %} {{ a }} {% with true %} b {% with _ %} c {% /match %}`),
      ).toMatchSnapshot()
    })

    test("List pattern", ({expect, _}) => {
      expect.value(
        compile(`{% match a with "a" %} {% with [1] %} {% with _ %} {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with "a" %} {% with [1, ...a] %} {% with _ %} {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with true %} {% with [1] %} {% with _ %} {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with false %} {% with [] %} {% with _ %} {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with 1 %} {% with [] %} {% with _ %} {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with {a} %} {{ a }} {% with [] %} {% with _ %} {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with ["a", ...a] %} {% with [1, ...a] %} {% with _ %} {% /match %}`),
      ).toMatchSnapshot()
    })

    test("Record pattern", ({expect, _}) => {
      expect.value(
        compile(`{% match a with "a" %}  {% with {a} %} {{ a }} {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with true %}  {% with {a} %} {{ a }} {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with false %}  {% with {a} %} {{ a }} {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with [] %}  {% with {a} %} {{ a }} {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with 1 %}  {% with {a} %} {{ a }} {% /match %}`),
      ).toMatchSnapshot()
      expect.value(
        compile(`{% match a with {a: "a"} %} {% with {a: 1} %} {{ a }} {% /match %}`),
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
    expect.value(render("{{ b }}", data, [])).toMatchSnapshot()
    expect.value(render("{{ c }}", data, [])).toMatchSnapshot()
    expect.value(render("{{ &b }}", data, [])).toMatchSnapshot()
  })

  test("Map type mismatches", ({expect, _}) => {
    let data = dict([
      ("a", Js.Json.string("a")),
      ("b", Js.Json.boolean(true)),
      ("c", Js.Json.stringArray([])),
    ])
    expect.value(render("{% map a with {a} %}{{ a }}{% /map %}", data, [])).toMatchSnapshot()
    expect.value(render("{% map b with {a} %}{{ a }}{% /map %}", data, [])).toMatchSnapshot()
    expect.value(
      render("{% map [1, 2, ...a] with a %}{{ a }}{% /map %}", data, []),
    ).toMatchSnapshot()
    expect.value(
      render("{% map [1, 2, ...z] with a %}{{ a }}{% /map %}", data, []),
    ).toMatchSnapshot()
  })

  test("Missing bindings", ({expect, _}) => {
    expect.value(render("{{ z }}", Js.Dict.empty(), [])).toMatchSnapshot()
    expect.value(render("{{ Z }}", Js.Dict.empty(), [])).toMatchSnapshot()
    expect.value(
      Compile2.make(~name="", "{% Z / %}", Compile2.Components.empty())->getError,
    ).toMatchSnapshot()
    let a = Source2.src(~name="A", "{{ B }}")
    let result = render(`{% A B=C / %}`, Js.Dict.empty(), [a])
    expect.value(result).toMatchSnapshot()
    // Get some coverage on the map-object error path.
    expect.value(
      render("{% map_dict <a: b> with b %} {{ b }} {% /map_dict %}", Js.Dict.empty(), []),
    ).toMatchSnapshot()
  })

  test("Error messages display component name correctly", ({expect, _}) => {
    // This test is not relevant for the same reasons it was originally. It
    // needs to be renamed.
    let a = Source2.src(~name="A", "{{ a }}")
    let data = Js.Dict.empty()
    let result = render(`{% A / %}`, data, [a])
    expect.value(result).toMatchSnapshot()
  })

  // test("Multiple render errors are all reported", ({expect, _}) => {
  //   expect.value(
  //     render(
  //       "{{ a }} {{ b }} {{ c }}",
  //       dict([("a", Js.Json.null), ("b", Js.Json.numberArray([1.0, 2.0]))]),
  //       [],
  //     ),
  //   ).toMatchSnapshot()
  // })

  test("Exceptions thrown in components are caught correctly", ({expect, _}) => {
    @raises(Failure)
    let a = Source2.function(~name="A", Typescheme.props([]), Typescheme.Child.props([]), (
      _,
      _,
      _,
    ) => raise(Failure("fail.")))
    let data = Js.Dict.empty()
    let result = render(~name="ExceptionsTest", `{% A / %}`, data, [a])
    expect.value(result).toMatchSnapshot()
  })
})

describe("Inputs", ({test, _}) => {
  test("Bad source input", ({expect, _}) => {
    expect.value(
      getError(Compile2.make(~name="Template", Obj.magic(1), Compile2.Components.empty())),
    ).toMatchSnapshot()
  })
})

describe("Stack trace is rendered correctly", ({test, _}) => {
  test("components", ({expect, _}) => {
    let c = Source2.src(~name="C", "{{ c }}")
    let b = Source2.src(~name="B", `{% match x with "x" %} {% C /%} {% /match %}`)
    let components = [c, b]
    let data = dict([("x", Js.Json.string("x"))])
    expect.value(render(~name="A", `{% B x / %}`, data, components)).toMatchSnapshot()
  })
})

@raises(Failure)
describe("Graphs are parsed correctly", ({test, _}) => {
  test("Cyclic dependencies are reported", ({expect, _}) => {
    let a = Source2.src(~name="A", "{% B /%}")
    let b = Source2.src(~name="B", "{% C /%}")
    let c = Source2.src(~name="C", "{% B /%}")
    let result = Compile2.Components.make([a, b, c])
    expect.value(result).toEqual(
      #errors([
        {
          exn: None,
          kind: #Compile,
          location: Some({character: 4}),
          message: "Cyclic dependency detected. I can't compile any components in this path.",
          path: [Js.Json.string("C"), Js.Json.string("B"), Js.Json.string("C")],
        },
      ]),
    )
  })
  test("Cyclic dependencies are reported (more complex)", ({expect, _}) => {
    let a = Source2.src(~name="A", "{% B /%}")
    let b = Source2.src(~name="B", "{% C /%}")
    let c = Source2.src(~name="C", "{% D /%}")
    let d = Source2.src(~name="D", "{% B /%}")
    let result = Compile2.Components.make([a, b, c, d])
    expect.value(result).toEqual(
      #errors([
        {
          exn: None,
          kind: #Compile,
          location: Some({character: 4}),
          message: "Cyclic dependency detected. I can't compile any components in this path.",
          path: [
            Js.Json.string("C"),
            Js.Json.string("B"),
            Js.Json.string("D"),
            Js.Json.string("C"),
          ],
        },
      ]),
    )
  })
  test("Missing dependencies are reported", ({expect, _}) => {
    let a = Source2.src(~name="A", "{% B /%}")
    let b = Source2.src(~name="B", "{% C /%}")
    let result = Compile2.Components.make([a, b])
    expect.value(result).toEqual(
      #errors([
        {
          exn: None,
          kind: #Compile,
          location: Some({character: 4}),
          message: `Template component "C" is missing, which is required by "B."`,
          path: [Js.Json.string("B")],
        },
      ]),
    )
  })
  // test("Runtime AST errors are reported", ({expect, _}) => {
  //   let a = Source2.src(~name="A", "{% B /%}")
  //   @raises(Failure)
  //   let b = Source2.function(
  //     ~name="B",
  //     Typescheme.props([]),
  //     Typescheme.Child.props([]),
  //     _ => failwith("lol"),
  //   )
  //   let result = Compile2.Components.make([a, b])
  //   let e = try {failwith("lol")} catch {
  //   | e => e
  //   }
  //   expect.value(result).toEqual(
  //     #errors([
  //       {
  //         exn: Some(AnyExn(e)),
  //         kind: #Compile,
  //         location: None,
  //         message: "An exception was thrown while compiling this template. This is probably due to malformed input.",
  //         path: [Js.Json.string("A")],
  //       },
  //     ]),
  //   )
  // expect.value(Compile2.make(b, emptyComponents)).toEqual(
  //   #errors([
  //     {
  //       exn: Some(AnyExn(e)),
  //       kind: #Compile,
  //       location: None,
  //       message: "An exception was thrown while compiling this template. This is probably due to malformed input.",
  //       path: [Js.Json.string("B")],
  //     },
  //   ]),
  // )
  // })
})
