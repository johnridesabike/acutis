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

let render2 = (~name="", ~children=Js.Dict.empty(), src, props, components) =>
  Optimize.make(~name, src, components)->Result.flatMap(f => f(Environment2.sync, props, children))

@raises(Not_found)
describe("Render essentials", ({test, _}) => {
  test("Basic", ({expect, _}) => {
    let props = dict([
      ("a", Js.Json.string("Hello")),
      ("b", Js.Json.string("World")),
      ("c", Js.Json.string("&\"'></`=")),
    ])
    let children = dict([("Z", render2(`Z`, props, []))])
    let result = render2(
      `{{ a }} {{ b }}! {{ c }} {{ &c }} {{ &"<" }} {{ "d" }} {{ 1.5 }} {{ Z }}`,
      props,
      ~children,
      [],
    )
    expect.value(result).toEqual(
      #ok("Hello World! &amp;&quot;&apos;&gt;&lt;&#x2F;&#x60;&#x3D; &\"'></`= < d 1.5 Z"),
    )
  })

  test("Unbound identifiers default to null", ({expect, _}) => {
    let result = render2(
      "{% match a with !a %} {{ a }} {% with null %} a doesn't exist. {% /match %}",
      Js.Dict.empty(),
      [],
    )
    expect.value(result).toEqual(#ok(" a doesn't exist. "))
  })

  test("Unwrapping not-null (!) works.", ({expect, _}) => {
    let result = render2(
      "{% match a with !a %} {{ a }} {% with null %} a doesn't exist. {% /match %}",
      dict([("a", Js.Json.string("works"))]),
      [],
    )
    expect.value(result).toEqual(#ok(" works "))
  })

  test("Nested comments", ({expect, _}) => {
    let result = render2("a {* b {* c *} d *} e", Js.Dict.empty(), [])
    expect.value(result).toEqual(#ok("a  e"))
  })

  test("Match", ({expect, _}) => {
    let data = dict([("a", json(`{"a": "a"}`))])
    let result = render2(`{% match a with {a} %} {{ a }} {% /match %}`, data, [])
    expect.value(result).toEqual(#ok(` a `))
    let data = dict([("a", json(`{"a": "a", "b": true}`))])
    let result = render2(
      `{% match a with {b: false, a} %} b is false. {{ a }} {% with {b: true, a} %} b is true. {{ a }} {% /match %}`,
      data,
      [],
    )
    expect.value(result).toEqual(#ok(` b is true. a `))
    let data = dict([("a", json(`{"a": true, "b": {"c": "hi"}}`))])
    let result = render2(
      `{% match a with {a: false} %} a is false. {% with {a: true, b} %} {% match b with {c} %} {{ c }} {% /match %} {% /match %}`,
      data,
      [],
    )
    expect.value(result).toEqual(#ok(`  hi  `))
    let data = dict([("a", json(`{"a": {"b": "hi"}}`))])
    let result = render2(`{% match a with {a: {b}} %} {{ b }} {% /match %}`, data, [])
    expect.value(result).toEqual(#ok(` hi `))
  })

  test("Whitespace control", ({expect, _}) => {
    let data = dict([("a", json(`{"a": {"b": "hi"}}`))])
    let result = render2(
      `{% match a with {a: {b}} ~%}  
   \t  _ {{ b }} _
    \t \r  {%~ /match %}`,
      data,
      [],
    )
    expect.value(result).toEqual(#ok(`_ hi _`))
    let data = dict([("a", json(`{"a": {"b": "hi"}}`))])
    let result = render2(`{% match a with {a: {b}} %} _ {{~ b ~}} _ {% /match %}`, data, [])
    expect.value(result).toEqual(#ok(` _hi_ `))
    let ohHai = ("OhHai", "{{ Children }} Oh hai {{ name }}.")
    let components = [ohHai]
    let result = render2(`{% OhHai name="Mark" ~%} I did not. {%~ /OhHai %}`, dict([]), components)
    expect.value(result).toEqual(#ok(`I did not. Oh hai Mark.`))
    let result = render2(
      `{% OhHai name="Mark" Children=#~%} I did not. {%~/# /%}`,
      Js.Dict.empty(),
      components,
    )
    expect.value(result).toEqual(#ok(`I did not. Oh hai Mark.`))
  })

  test("Map", ({expect, _}) => {
    let data = dict([("a", json(`[{"name": "John"}, {"name": "Megan"}]`))])
    let result = render2(`{% map a with {name} %} {{ name }} {% /map %}`, data, [])
    expect.value(result).toEqual(#ok(` John  Megan `))
    let data = dict([("a", json(`[{"name": "John"}, {"name": "Megan"}]`)), ("b", json(`"hi"`))])
    let result = render2(`{% map a with {name} %} {{ b }} {{ name }}. {% /map %}`, data, [])
    expect.value(result).toEqual(#ok(` hi John.  hi Megan. `))
    let result = render2(
      `{% map a with {name}, index %} {{ index }} {{ name }}. {% /map %}`,
      data,
      [],
    )
    expect.value(result).toEqual(#ok(` 0 John.  1 Megan. `))
    let result = render2(
      `{% map [{name: "Carlo"}, {name: "John"}] with {name} %} {{ name }}. {% /map %}`,
      data,
      [],
    )
    expect.value(result).toEqual(#ok(` Carlo.  John. `))
    let result = render2(
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
    let result = render2(
      `{% map_dict people with {job}, key ~%} {{ key }}: {{ job }}. {% /map_dict %}`,
      data,
      [],
    )
    expect.value(result).toEqual(#ok(`lisa: computers. tommy: banking. `))
    let result = render2(
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
    let ohHai = ("OhHai", "{{ Children }} Oh hai {{ name }}.")
    let components = [ohHai]
    let result = render2(
      `{% OhHai name="Mark" %} I did not. {% /OhHai %}`,
      Js.Dict.empty(),
      components,
    )
    expect.value(result).toEqual(#ok(` I did not.  Oh hai Mark.`))

    //   @raises(Not_found)
    //   let addOne = Source.func(~name="AddOne", (env, props, _children) => {
    //     let index =
    //       props->Js.Dict.get("index")->Belt.Option.flatMap(Js.Json.decodeNumber)->Belt.Option.getExn
    //     env.return(. Belt.Float.toString(index +. 1.0))
    //   })

    //   @raises(Not_found)
    //   let components = Compile.Components.make([addOne])
    //   let data = dict([("a", json(`[{"name": "John"}, {"name": "Megan"}]`))])
    //   let result = render(
    //     `{% map a with {name}, index %} {% AddOne index / %} {{ name }}. {% /map %}`,
    //     data,
    //     components->Result.getExn,
    //   )
    //   expect.value(result).toEqual(#ok(` 1 John.  2 Megan. `))
    //   let result = render(
    //     `{% map a with {name}, i %} {% AddOne index=i / %} {{ name }}. {% /map %}`,
    //     data,
    //     components->Result.getExn,
    //   )
    //   expect.value(result).toEqual(#ok(` 1 John.  2 Megan. `))
  })
})

describe("Nullish coalescing", ({test, _}) => {
  test("Nullish coalescing", ({expect, _}) => {
    let children = Js.Dict.fromArray([
      ("Z", render2("z", Js.Dict.empty(), [], ~children=Js.Dict.empty())),
    ])
    let props = Js.Dict.fromArray([("b", Js.Json.string("b")), ("c", Js.Json.string("c"))])
    let result = render2(`{{ a ? b ? c }}`, props, [], ~children=Js.Dict.empty())
    expect.value(result).toEqual(#ok("b"))
    let result = render2(`{{ a ? d ? c }}`, props, [], ~children=Js.Dict.empty())
    expect.value(result).toEqual(#ok("c"))
    let result = render2(`{{ a ? B ? Z }}`, Js.Dict.empty(), [], ~children)
    expect.value(result).toEqual(#ok("z"))
    let result = render2(`{{ a ? B ? X ? "y" }}`, Js.Dict.empty(), [], ~children)
    expect.value(result).toEqual(#ok("y"))
    let result = render2(` {{~ &a ? B ? X ? 1 ~}} `, Js.Dict.empty(), [], ~children)
    expect.value(result).toEqual(#ok("1"))
    let result = render2(`{{ &x ? Y ? Z ? X }}`, Js.Dict.empty(), [], ~children)
    expect.value(result).toEqual(#ok("z"))
  })
})

describe("Template sections", ({test, _}) => {
  test("Default `Children` child", ({expect, _}) => {
    let a = ("A", "{{ Children }}")
    let components = [a]
    let result = render2(`{% A %} b {%/ A %}`, Js.Dict.empty(), components)
    expect.value(result).toEqual(#ok(" b "))
    let result = render2(`{% A Children=#%} b {%/# / %}`, Js.Dict.empty(), components)
    expect.value(result).toEqual(#ok(" b "))
  })

  test("Child props are passed correctly", ({expect, _}) => {
    let x = ("X", "{{ PassthroughChild }}")
    let y = ("Y", "{% X PassthroughChild=A /%}")
    let components = [x, y]
    let result = render2(`{% Y A=#%} a {%/# / %}`, Js.Dict.empty(), components)
    expect.value(result).toEqual(#ok(" a "))
  })

  test("Child props are passed correctly with punning", ({expect, _}) => {
    let x = ("X", "{{ PassthroughChild }}")
    let y = ("Y", "{% X PassthroughChild /%}")
    let components = [x, y]
    let result = render2(`{% Y PassthroughChild=#%} a {%/# / %}`, Js.Dict.empty(), components)
    expect.value(result).toEqual(#ok(" a "))
  })
})

// describe("API helper functions", ({test, _}) => {
//   test("env.return", ({expect, _}) => {
//     let x = Source.func(~name="X", (env, _props, _children) => {
//       env.return(. "a")
//     })
//     let components = Compile.Components.make([x])
//     let result = render(`{% X / %}`, Js.Dict.empty(), components->Result.getExn)
//     expect.value(result).toEqual(#ok("a"))
//   })

//   test("env.error", ({expect, _}) => {
//     let x = Source.func(~name="X", (env, _props, _children) => {
//       env.error(. "e")
//     })
//     let components = Compile.Components.make([x])
//     let result = render(`{% X / %}`, Js.Dict.empty(), components->Result.getExn)
//     expect.value(result).toEqual(
//       #errors([
//         {
//           message: "e",
//           location: None,
//           kind: #Render,
//           path: [Js.Json.string("")],
//           exn: None,
//         },
//       ]),
//     )
//   })

//   test("env.mapChild", ({expect, _}) => {
//     let x = Source.func(~name="X", (env, _props, children) => {
//       env.mapChild(.Js.Dict.unsafeGet(children, "Children"), child => Js.String.toUpperCase(child))
//     })
//     let components = Compile.Components.make([x])
//     let result = render(`{% X ~%} a {%~ /X %}`, Js.Dict.empty(), components->Result.getExn)
//     expect.value(result).toEqual(#ok("A"))
//     let errors = render(
//       ~name="A",
//       `{% X %} {{ e }} {% /X %}`,
//       Js.Dict.empty(),
//       components->Result.getExn,
//     )
//     expect.value(errors).toEqual(
//       #errors([
//         {
//           message: "\"e\" is type null. I can only echo strings and numbers.",
//           location: Some({character: 12}),
//           path: [Js.Json.string("section: X#Children"), Js.Json.string("A")],
//           kind: #Render,
//           exn: None,
//         },
//       ]),
//     )
//   })

//   test("env.flatMapChild", ({expect, _}) => {
//     let x = Source.func(~name="X", (env, _props, children) =>
//       env.flatMapChild(.Js.Dict.unsafeGet(children, "Children"), child =>
//         env.return(. Js.String.toUpperCase(child))
//       )
//     )
//     let components = Compile.Components.make([x])
//     let result = render(`{% X %} a {% /X %}`, Js.Dict.empty(), components->Result.getExn)
//     expect.value(result).toEqual(#ok(" A "))
//     let errors = render(
//       ~name="A",
//       `{% X %} {{ e }} {% /X %}`,
//       Js.Dict.empty(),
//       components->Result.getExn,
//     )
//     expect.value(errors).toEqual(
//       #errors([
//         {
//           message: "\"e\" is type null. I can only echo strings and numbers.",
//           location: Some({character: 12}),
//           path: [Js.Json.string("section: X#Children"), Js.Json.string("A")],
//           kind: #Render,
//           exn: None,
//         },
//       ]),
//     )
//   })
// })
