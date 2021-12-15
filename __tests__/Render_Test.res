/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
open TestFramework

let json = x =>
  try {
    Js.Json.parseExn(x)
  } catch {
  | _ => Js.Json.string("THIS IS EASIER THAN DEALING WITH EXCEPTIONS.")
  }

let dict = Js.Dict.fromArray

let render = (~name="", src, props, components) =>
  Compile.make(~name, src, Compile.Components.makeExn(components))->Result.flatMap(t =>
    Render.sync(t, props)
  )

@raises(Not_found)
describe("Render essentials", ({test, _}) => {
  test("Basic", ({expect, _}) => {
    let props = dict([
      ("a", Js.Json.string("Hello")),
      ("b", Js.Json.string("World")),
      ("c", Js.Json.string("&\"'></`=")),
    ])
    let result = render(
      `{{ a }} {{ b }}! {{ c }} {{ &c }} {{ &"<" }} {{ "d" }} {{ 1.5 }}`,
      props,
      [],
    )
    expect.value(result).toEqual(
      #ok("Hello World! &amp;&quot;&apos;&gt;&lt;&#x2F;&#x60;&#x3D; &\"'></`= < d 1.5"),
    )
  })

  test("Unbound identifiers default to null", ({expect, _}) => {
    let result = render(
      "{% match a with !a %} {{ a }} {% with null %} a doesn't exist. {% /match %}",
      Js.Dict.empty(),
      [],
    )
    expect.value(result).toEqual(#ok(" a doesn't exist. "))
  })

  test("Unwrapping not-null (!) works.", ({expect, _}) => {
    let result = render(
      "{% match a with !a %} {{ a }} {% with null %} a doesn't exist. {% /match %}",
      dict([("a", Js.Json.string("works"))]),
      [],
    )
    expect.value(result).toEqual(#ok(" works "))
  })

  test("Nested comments", ({expect, _}) => {
    let result = render("a {* b {* c *} d *} e", Js.Dict.empty(), [])
    expect.value(result).toEqual(#ok("a  e"))
  })

  test("Match", ({expect, _}) => {
    let data = dict([("a", json(`{"a": "a"}`))])
    let result = render(`{% match a with {a} %} {{ a }} {% /match %}`, data, [])
    expect.value(result).toEqual(#ok(` a `))
    let data = dict([("a", json(`{"a": "a", "b": true}`))])
    let result = render(
      `{% match a with {b: false, a} %} b is false. {{ a }} {% with {b: true, a} %} b is true. {{ a }} {% /match %}`,
      data,
      [],
    )
    expect.value(result).toEqual(#ok(` b is true. a `))
    let data = dict([("a", json(`{"a": true, "b": {"c": "hi"}}`))])
    let result = render(
      `{% match a with {a: false} %} a is false. {% with {a: true, b} %} {% match b with {c} %} {{ c }} {% /match %} {% /match %}`,
      data,
      [],
    )
    expect.value(result).toEqual(#ok(`  hi  `))
    let data = dict([("a", json(`{"a": {"b": "hi"}}`))])
    let result = render(`{% match a with {a: {b}} %} {{ b }} {% /match %}`, data, [])
    expect.value(result).toEqual(#ok(` hi `))
  })

  test("Whitespace control", ({expect, _}) => {
    let data = dict([("a", json(`{"a": {"b": "hi"}}`))])
    let result = render(
      `{% match a with {a: {b}} ~%}  
   \t  _ {{ b }} _
    \t \r  {%~ /match %}`,
      data,
      [],
    )
    expect.value(result).toEqual(#ok(`_ hi _`))
    let data = dict([("a", json(`{"a": {"b": "hi"}}`))])
    let result = render(`{% match a with {a: {b}} %} _ {{~ b ~}} _ {% /match %}`, data, [])
    expect.value(result).toEqual(#ok(` _hi_ `))
    let ohHai = Source.src(~name="OhHai", "{{ Children }} Oh hai {{ name }}.")
    let components = [ohHai]
    let result = render(`{% OhHai name="Mark" ~%} I did not. {%~ /OhHai %}`, dict([]), components)
    expect.value(result).toEqual(#ok(`I did not. Oh hai Mark.`))
    let result = render(
      `{% OhHai name="Mark" Children=#~%} I did not. {%~/# /%}`,
      Js.Dict.empty(),
      components,
    )
    expect.value(result).toEqual(#ok(`I did not. Oh hai Mark.`))
  })

  test("Map", ({expect, _}) => {
    let data = dict([("a", json(`[{"name": "John"}, {"name": "Megan"}]`))])
    let result = render(`{% map a with {name} %} {{ name }} {% /map %}`, data, [])
    expect.value(result).toEqual(#ok(` John  Megan `))
    let data = dict([("a", json(`[{"name": "John"}, {"name": "Megan"}]`)), ("b", json(`"hi"`))])
    let result = render(`{% map a with {name} %} {{ b }} {{ name }}. {% /map %}`, data, [])
    expect.value(result).toEqual(#ok(` hi John.  hi Megan. `))
    let result = render(
      `{% map a with {name}, index %} {{ index }} {{ name }}. {% /map %}`,
      data,
      [],
    )
    expect.value(result).toEqual(#ok(` 0 John.  1 Megan. `))
    let result = render(
      `{% map [{name: "Carlo"}, {name: "John"}] with {name} %} {{ name }}. {% /map %}`,
      data,
      [],
    )
    expect.value(result).toEqual(#ok(` Carlo.  John. `))
    let result = render(
      `{% map [{name: "Carlo"}, ...a] with {name} %} {{ name }}. {% /map %}`,
      data,
      [],
    )
    expect.value(result).toEqual(#ok(` Carlo.  John.  Megan. `))
  })

  test("Map: dictionaries", ({expect, _}) => {
    let data = dict([
      ("people", json(`{"lisa": {"job": "computers"}, "tommy": {"job": "banking"}}`)),
    ])
    let result = render(
      `{% map_dict people with {job}, key ~%} {{ key }}: {{ job }}. {% /map_dict %}`,
      data,
      [],
    )
    expect.value(result).toEqual(#ok(`lisa: computers. tommy: banking. `))
    let result = render(
      `{% map_dict <
          lisa: {job: "computers"},
          tommy: {job: "banking"}
        > with {job}, key ~%} {{ key }}: {{ job }}. {% /map_dict %}`,
      Js.Dict.empty(),
      [],
    )
    expect.value(result).toEqual(#ok(`lisa: computers. tommy: banking. `))
  })

  test("Component", ({expect, _}) => {
    let ohHai = Source.src(~name="OhHai", "{{ Children }} Oh hai {{ name }}.")
    let components = [ohHai]
    let result = render(
      `{% OhHai name="Mark" %} I did not. {% /OhHai %}`,
      Js.Dict.empty(),
      components,
    )
    expect.value(result).toEqual(#ok(` I did not.  Oh hai Mark.`))
    let addOne = Source.fn(
      ~name="AddOne",
      Typescheme.props([("index", Typescheme.int())]),
      Typescheme.Child.props([]),
      (type a, module(Env): Source.env<a>, props, _children) => {
        let index = props->Js.Dict.get("index")->Belt.Option.flatMap(Js.Json.decodeNumber)
        switch index {
        | None => Env.error(. "oops")
        | Some(i) => Env.return(. Belt.Float.toString(i +. 1.0))
        }
      },
    )
    let data = dict([("a", json(`[{"name": "John"}, {"name": "Megan"}]`))])
    let result = render(
      `{% map a with {name}, index %} {% AddOne index / %} {{ name }}. {% /map %}`,
      data,
      [addOne],
    )
    expect.value(result).toEqual(#ok(` 1 John.  2 Megan. `))
    let result = render(
      `{% map a with {name}, i %} {% AddOne index=i / %} {{ name }}. {% /map %}`,
      data,
      [addOne],
    )
    expect.value(result).toEqual(#ok(` 1 John.  2 Megan. `))
  })
})

describe("Nullish coalescing", ({test, _}) => {
  test("Nullish coalescing", ({expect, _}) => {
    let props = Js.Dict.fromArray([("b", Js.Json.string("b")), ("c", Js.Json.string("c"))])
    let result = render(`{{ a ? b ? c }}`, props, [])
    expect.value(result).toEqual(#ok("b"))
    let result = render(`{{ a ? d ? c }}`, props, [])
    expect.value(result).toEqual(#ok("c"))
    let result = render(
      `{% Comp a=null Z=#%}z{%/# /%}`,
      Js.Dict.empty(),
      [Source.src(~name="Comp", `{{ a ? B ? Z }}`)],
    )
    expect.value(result).toEqual(#ok("z"))
    let result = render(
      `{% Comp a=null /%}`,
      Js.Dict.empty(),
      [Source.src(~name="Comp", `{{ a ? B ? X ? "y" }}`)],
    )
    expect.value(result).toEqual(#ok("y"))
    let result = render(
      `{% Comp a=null /%}`,
      Js.Dict.empty(),
      [Source.src(~name="Comp", ` {{~ &a ? B ? X ? 1 ~}} `)],
    )
    expect.value(result).toEqual(#ok("1"))
    let result = render(
      `{% Comp x=null Z=#%}z{%/# /%}`,
      Js.Dict.empty(),
      [Source.src(~name="Comp", `{{ &x ? Y ? Z ? "x" }}`)],
    )
    expect.value(result).toEqual(#ok("z"))
  })
})

describe("Template sections", ({test, _}) => {
  test("Default `Children` child", ({expect, _}) => {
    let a = Source.src(~name="A", "{{ Children }}")
    let result = render(`{% A %} b {%/ A %}`, Js.Dict.empty(), [a])
    expect.value(result).toEqual(#ok(" b "))
    let result = render(`{% A Children=#%} b {%/# / %}`, Js.Dict.empty(), [a])
    expect.value(result).toEqual(#ok(" b "))
  })

  test("Child props are passed correctly", ({expect, _}) => {
    let x = Source.src(~name="X", "{{ PassthroughChild }}")
    let y = Source.src(~name="Y", "{% X PassthroughChild=A /%}")
    let result = render(`{% Y A=#%} a {%/# / %}`, Js.Dict.empty(), [x, y])
    expect.value(result).toEqual(#ok(" a "))
  })

  test("Child props are passed correctly with punning", ({expect, _}) => {
    let x = Source.src(~name="X", "{{ PassthroughChild }}")
    let y = Source.src(~name="Y", "{% X PassthroughChild /%}")
    let result = render(`{% Y PassthroughChild=#%} a {%/# / %}`, Js.Dict.empty(), [x, y])
    expect.value(result).toEqual(#ok(" a "))
  })
})

describe("API helper functions", ({test, _}) => {
  test("env.return", ({expect, _}) => {
    let x = Source.fn(~name="X", Typescheme.props([]), Typescheme.Child.props([]), (
      type a,
      module(Env): Source.env<a>,
      _props,
      _children,
    ) => {
      Env.return(. "a")
    })
    let result = render(`{% X / %}`, Js.Dict.empty(), [x])
    expect.value(result).toEqual(#ok("a"))
  })

  test("env.error", ({expect, _}) => {
    let x = Source.fn(~name="X", Typescheme.props([]), Typescheme.Child.props([]), (
      type a,
      module(Env): Source.env<a>,
      _props,
      _children,
    ) => {
      Env.error(. "e")
    })
    let result = render(`{% X / %}`, Js.Dict.empty(), [x])
    expect.value(result).toEqual(
      #errors([
        {
          message: "e",
          location: None,
          kind: #Render,
          path: [],
          exn: None,
        },
      ]),
    )
  })

  test("env.mapChild", ({expect, _}) => {
    let x = Source.fn(
      ~name="X",
      Typescheme.props([]),
      Typescheme.Child.props([("Children", Typescheme.Child.child())]),
      (type a, module(Env): Source.env<a>, _props, children) => {
        Env.map(.Js.Dict.unsafeGet(children, "Children"), child => Js.String.toUpperCase(child))
      },
    )
    let result = render(`{% X ~%} a {%~ /X %}`, Js.Dict.empty(), [x])
    expect.value(result).toEqual(#ok("A"))
    let errors = render(~name="A", `{% X %} {{ e }} {% /X %}`, Js.Dict.empty(), [x])
    expect.value(errors).toEqual(
      #errors([
        {
          message: "Input is missing JSON object key \"e\" which is required.",
          location: None,
          path: [],
          kind: #Decode,
          exn: None,
        },
      ]),
    )
  })

  test("env.flatMapChild", ({expect, _}) => {
    let x = Source.fn(
      ~name="X",
      Typescheme.props([]),
      Typescheme.Child.props([("Children", Typescheme.Child.child())]),
      (type a, module(Env): Source.env<a>, _props, children) =>
        Env.flatmap(.Js.Dict.unsafeGet(children, "Children"), child =>
          Env.return(. Js.String.toUpperCase(child))
        ),
    )
    let result = render(`{% X %} a {% /X %}`, Js.Dict.empty(), [x])
    expect.value(result).toEqual(#ok(" A "))
    let errors = render(~name="A", `{% X %} {{ e }} {% /X %}`, Js.Dict.empty(), [x])
    expect.value(errors).toEqual(
      #errors([
        {
          message: "Input is missing JSON object key \"e\" which is required.",
          location: None,
          path: [],
          kind: #Decode,
          exn: None,
        },
      ]),
    )
  })
})
