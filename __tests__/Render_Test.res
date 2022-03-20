/**
  Copyright (c) 2021 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
open TestFramework
module Json = Js.Json

let json = x =>
  try {
    Json.parseExn(x)
  } catch {
  | _ => Json.string("THIS IS EASIER THAN DEALING WITH EXCEPTIONS.")
  }

let dict = Js.Dict.fromArray

let render = (~name="", src, props, components) =>
  Compile.make(~name, src, Compile.Components.makeExn(components))->Result.flatMap(t =>
    Render.sync(t, props)
  )

describe("Render essentials", ({test, _}) => {
  test("Basic", ({expect, _}) => {
    let props = dict([
      ("a", Json.string("Hello")),
      ("b", Json.string("World")),
      ("c", Json.string("&\"'></`=")),
    ])
    let result = render(`{{ a }} {{ b }}! {{ c }} {{ &c }} {{ &"<" }} {{ "d" }}`, props, [])
    expect.value(result).toEqual(
      #ok("Hello World! &amp;&quot;&apos;&gt;&lt;&#x2F;&#x60;&#x3D; &\"'></`= < d"),
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
      dict([("a", Json.string("works"))]),
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

  test("List literal spread", ({expect, _}) => {
    let src = `
    {%~ map ["a", "b", ...["c", "d"]] with x ~%} {{ x }} {% /map %}`
    let result = render(src, Js.Dict.empty(), [])
    expect.value(result).toEqual(#ok("a b c d "))
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
      Typescheme.make([("index", Typescheme.int())]),
      Typescheme.Child.make([]),
      (type a, module(Env): Source.env<a>, props, _children) => {
        let index = props->Js.Dict.get("index")->Belt.Option.flatMap(Json.decodeNumber)
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
    let props = dict([("b", Json.string("b")), ("c", Json.string("c"))])
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
      `{% Comp x=null Z=#%}z{%/# /%}`,
      Js.Dict.empty(),
      [Source.src(~name="Comp", `{{ &x ? Y ? Z ? "x" }}`)],
    )
    expect.value(result).toEqual(#ok("z"))
  })
})

describe("Advanced types", ({test, _}) => {
  let props = dict([
    ("a", Json.object_(dict([("tag", Json.number(0.0)), ("a", Json.string("a"))]))),
    ("b", Json.object_(dict([("tag", Json.number(1.0)), ("a", Json.number(1.0))]))),
    ("c", Json.object_(dict([("tag", Json.string("a")), ("a", Json.string("a"))]))),
    ("d", Json.object_(dict([("tag", Json.string("b")), ("a", Json.number(1.0))]))),
    ("e", Json.object_(dict([("tag", Json.boolean(true)), ("a", Json.string("a"))]))),
    ("f", Json.object_(dict([("tag", Json.boolean(false)), ("a", Json.number(1.0))]))),
  ])
  test("Tagged unions", ({expect, _}) => {
    let src = `
    {%~ map [a, b]
        with {@tag: 0, a: "a"}
        with {@tag: 1, a: 1} ~%} success {%
        with _ %} fail {%
    /map ~%}
    {%~ map [c, d]
        with {@tag: "a", a: "a"}
        with {@tag: "b", a: 1} ~%} success {%
        with _ %} fail {%
    /map ~%}
    {%~ map [e, f]
        with {@tag: true, a: "a"}
        with {@tag: false, a: 1} ~%} success {%
        with _ %} fail {%
    /map ~%}
    `
    let result = render(src, props, [])
    expect.value(result).toEqual(#ok("success success success success success success "))
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

  test("Child.make are passed correctly", ({expect, _}) => {
    let x = Source.src(~name="X", "{{ PassthroughChild }}")
    let y = Source.src(~name="Y", "{% X PassthroughChild=A /%}")
    let result = render(`{% Y A=#%} a {%/# / %}`, Js.Dict.empty(), [x, y])
    expect.value(result).toEqual(#ok(" a "))
  })

  test("Child.make are passed correctly with punning", ({expect, _}) => {
    let x = Source.src(~name="X", "{{ PassthroughChild }}")
    let y = Source.src(~name="Y", "{% X PassthroughChild /%}")
    let result = render(`{% Y PassthroughChild=#%} a {%/# / %}`, Js.Dict.empty(), [x, y])
    expect.value(result).toEqual(#ok(" a "))
  })
})

describe("Typescheme API", ({test, _}) => {
  module Ty = Typescheme
  let empty_child_props = Typescheme.Child.make([])

  let f = props =>
    Source.fn(~name="F", props, empty_child_props, (type a, module(Env): Source.env<a>, props, _) =>
      Env.return(. Json.stringifyWithSpace(Json.object_(props), 2))
    )

  let parse_json = s =>
    try {
      s->Json.parseExn->Json.decodeObject->Belt.Option.getExn
    } catch {
    | _ => Js.Dict.empty()
    }

  test("Unknown, int, float, string, echo", ({expect, _}) => {
    let props = Ty.make([
      ("u", Ty.unknown()),
      ("i", Ty.int()),
      ("f", Ty.float()),
      ("s", Ty.string()),
      ("e", Ty.echo()),
    ])
    let rawjson = `{
  "e": true,
  "f": 1.5,
  "i": 1,
  "s": "s",
  "u": null
}`
    let data = parse_json(rawjson)
    let result = render(`{% F u i f s e / %}`, data, [f(props)])
    expect.value(result).toEqual(#ok(rawjson))
  })

  test("Nullable, list", ({expect, _}) => {
    let props = Ty.make([
      ("n1", Ty.nullable(Ty.string())),
      ("n2", Ty.nullable(Ty.string())),
      ("l", Ty.list(Ty.int())),
    ])
    let rawjson = `{
  "l": [
    0,
    1,
    2
  ],
  "n1": null,
  "n2": "n"
}`
    let data = parse_json(rawjson)
    let result = render(`{% F n1 n2 l / %}`, data, [f(props)])
    expect.value(result).toEqual(#ok(rawjson))
  })

  test("Tuple, record, dict", ({expect, _}) => {
    let props = Ty.make([
      ("t", Ty.tuple([Ty.int(), Ty.string()])),
      ("r", Ty.record([("a", Ty.int()), ("b", Ty.string())])),
      ("d", Ty.dict(Ty.nullable(Ty.int()))),
    ])
    let rawjson = `{
  "d": {
    "c": null,
    "d": 3
  },
  "r": {
    "a": 2,
    "b": "r"
  },
  "t": [
    1,
    "t"
  ]
}`
    let data = parse_json(rawjson)
    let result = render(`{% F t r d / %}`, data, [f(props)])
    expect.value(result).toEqual(#ok(rawjson))
  })

  test("Enums: int, string, boolean", ({expect, _}) => {
    let props = Ty.make([
      ("i", Ty.enum_int([0, 1])),
      ("s", Ty.enum_string(["a", "b"])),
      ("b", Ty.bool()),
    ])
    let rawjson = `{
  "b": true,
  "i": 1,
  "s": "a"
}`
    let data = parse_json(rawjson)
    let result = render(`{% F i s b / %}`, data, [f(props)])
    expect.value(result).toEqual(#ok(rawjson))
  })

  test("Unions: int, string, boolean", ({expect, _}) => {
    let i = Ty.union_int(
      "tag",
      [(0, [("a", Ty.string())]), (1, [("a", Ty.int()), ("b", Ty.string())])],
    )
    let s = Ty.union_string(
      "tag",
      [("a", [("a", Ty.string())]), ("b", [("a", Ty.int()), ("b", Ty.string())])],
    )
    let b = Ty.union_boolean(
      "tag",
      ~f=[("a", Ty.string())],
      ~t=[("a", Ty.int()), ("b", Ty.string())],
    )
    let props = Ty.make([("i1", i), ("i2", i), ("s1", s), ("s2", s), ("b1", b), ("b2", b)])
    let rawjson = `{
  "b1": {
    "a": "s",
    "tag": false
  },
  "b2": {
    "a": 0,
    "b": "s",
    "tag": true
  },
  "i1": {
    "a": "s",
    "tag": 0
  },
  "i2": {
    "a": 0,
    "b": "s",
    "tag": 1
  },
  "s1": {
    "a": "s",
    "tag": "a"
  },
  "s2": {
    "a": 0,
    "b": "s",
    "tag": "b"
  }
}`
    let data = parse_json(rawjson)
    let result = render(`{% F i1 i2 s1 s2 b1 b2 / %}`, data, [f(props)])
    expect.value(result).toEqual(#ok(rawjson))
  })
})

describe("Constructing values", ({test, _}) => {
  test("Constructing values", ({expect, _}) => {
    let src = `{%~
      match [(1, 2), (3, 4)],
            [@"y", @"z"],
            [@99, @100],
            [!{@tag: "a", a: 1.5}, null],
            {@tag: 0, a: !"z"},
            <a: "a">
      with  [(1, 2), (3, 4)],
            [@"y", @"z"],
            [@99, @100],
            [!{@tag: "a", a: 1.5}, null],
            {@tag: 0, a: !"z"},
            <a: "a">
    ~%} success {%~
      with _, _, _, _, _, _ %} fail {%
      /match ~%}`
    let result = render(src, Js.Dict.empty(), [])
    expect.value(result).toEqual(#ok("success"))
  })
})

describe("API helper functions", ({test, _}) => {
  test("env.return", ({expect, _}) => {
    let x = Source.fn(~name="X", Typescheme.make([]), Typescheme.Child.make([]), (
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
    let x = Source.fn(~name="X", Typescheme.make([]), Typescheme.Child.make([]), (
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
          message: "A template function raised this error:\ne",
          location: None,
          kind: #Render,
          stack: [],
          exn: None,
        },
      ]),
    )
  })

  test("env.mapChild", ({expect, _}) => {
    let x = Source.fn(
      ~name="X",
      Typescheme.make([]),
      Typescheme.Child.make([Typescheme.Child.child("Children")]),
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
          stack: [],
          kind: #Render,
          exn: None,
        },
      ]),
    )
  })

  test("env.flatMapChild", ({expect, _}) => {
    let x = Source.fn(
      ~name="X",
      Typescheme.make([]),
      Typescheme.Child.make([Typescheme.Child.child("Children")]),
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
          stack: [],
          kind: #Render,
          exn: None,
        },
      ]),
    )
  })
})
