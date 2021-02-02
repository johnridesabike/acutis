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

let json = Js.Json.parseExn
let emptyComponents = Js.Dict.empty()
let dict = Js.Dict.fromArray

let render = (~name=?, ~children=Js.Dict.empty(), src, props, components) =>
  Compile.make(src, ~name?)(. Environment.make(components), props, children)

describe("All together", ({test, _}) => {
  test("Basic", ({expect, _}) => {
    let props = dict([
      ("a", Js.Json.string("Hello")),
      ("b", Js.Json.string("World")),
      ("c", Js.Json.string("&\"'></`=")),
    ])
    let children = dict([("Z", render(`Z`, props, emptyComponents))])
    let result = render(
      `{{ a }} {{ b }}! {{ c }} {{ &c }} {{ &"<" }} {{ "d" }} {{ 1.5 }} {{ Z }}`,
      props,
      ~children,
      emptyComponents,
    )
    expect.value(result).toEqual(
      #data("Hello World! &amp;&quot;&apos;&gt;&lt;&#x2F;&#x60;&#x3D; &\"'></`= < d 1.5 Z"),
    )
  })

  test("Unbound identifiers default to null", ({expect, _}) => {
    let result = render(
      "{% match a with null %} a doesn't exist. {% with a %} {{ a }} {% /match %}",
      Js.Dict.empty(),
      emptyComponents,
    )
    expect.value(result).toEqual(#data(" a doesn't exist. "))
  })

  test("Nested comments", ({expect, _}) => {
    let result = render("a {* b {* c *} d *} e", Js.Dict.empty(), emptyComponents)
    expect.value(result).toEqual(#data("a  e"))
  })

  test("Match", ({expect, _}) => {
    let data = dict([("a", json(`{"a": "a"}`))])
    let result = render(`{% match a with {a} %} {{ a }} {% /match %}`, data, emptyComponents)
    expect.value(result).toEqual(#data(` a `))
    let data = dict([("a", json(`{"a": "a", "b": true}`))])
    let result = render(
      `{% match a with {b: false, a} %} b is false. {{ a }} {% with {b: true, a} %} b is true. {{ a }} {% /match %}`,
      data,
      emptyComponents,
    )
    expect.value(result).toEqual(#data(` b is true. a `))
    let data = dict([("a", json(`{"a": true, "b": {"c": "hi"}}`))])
    let result = render(
      `{% match a with {a: false} %} a is false. {% with {a: true, b} %} {% match b with {c} %} {{ c }} {% /match %} {% /match %}`,
      data,
      emptyComponents,
    )
    expect.value(result).toEqual(#data(`  hi  `))
    let data = dict([("a", json(`{"a": {"b": "hi"}}`))])
    let result = render(`{% match a with {a: {b}} %} {{ b }} {% /match %}`, data, emptyComponents)
    expect.value(result).toEqual(#data(` hi `))
  })

  test("Whitespace control", ({expect, _}) => {
    let data = dict([("a", json(`{"a": {"b": "hi"}}`))])
    let result = render(
      `{% match a with {a: {b}} ~%}  
   \t  _ {{ b }} _
    \t \r  {%~ /match %}`,
      data,
      emptyComponents,
    )
    expect.value(result).toEqual(#data(`_ hi _`))
    let data = dict([("a", json(`{"a": {"b": "hi"}}`))])
    let result = render(
      `{% match a with {a: {b}} %} _ {{~ b ~}} _ {% /match %}`,
      data,
      emptyComponents,
    )
    expect.value(result).toEqual(#data(` _hi_ `))
    let ohHai: Acutis_Types.template<_> = (. env, props, templates) =>
      env.render(. Compile.makeAst("{{ Children }} Oh hai {{ name }}."), props, templates)
    let components = dict([("OhHai", ohHai)])
    let result = render(
      `{% OhHai name="Mark" ~%} I did not. {%~ /OhHai %}`,
      Js.Dict.empty(),
      components,
    )
    expect.value(result).toEqual(#data(`I did not. Oh hai Mark.`))
    let result = render(
      `{% OhHai name="Mark" Children=#~%} I did not. {%~/# /%}`,
      Js.Dict.empty(),
      components,
    )
    expect.value(result).toEqual(#data(`I did not. Oh hai Mark.`))
  })

  test("Map", ({expect, _}) => {
    let data = dict([("a", json(`[{"name": "John"}, {"name": "Megan"}]`))])
    let result = render(`{% map a with {name} %} {{ name }} {% /map %}`, data, emptyComponents)
    expect.value(result).toEqual(#data(` John  Megan `))
    let data = dict([("a", json(`[{"name": "John"}, {"name": "Megan"}]`)), ("b", json(`"hi"`))])
    let result = render(
      `{% map a with {name} %} {{ b }} {{ name }}. {% /map %}`,
      data,
      emptyComponents,
    )
    expect.value(result).toEqual(#data(` hi John.  hi Megan. `))
    let result = render(
      `{% map a with {name}, index %} {{ index }} {{ name }}. {% /map %}`,
      data,
      emptyComponents,
    )
    expect.value(result).toEqual(#data(` 0 John.  1 Megan. `))
    let result = render(
      `{% map [{name: "Carlo"}, {name: "John"}] with {name} %} {{ name }}. {% /map %}`,
      data,
      emptyComponents,
    )
    expect.value(result).toEqual(#data(` Carlo.  John. `))
    let result = render(
      `{% map [{name: "Carlo"}, ...a] with {name} %} {{ name }}. {% /map %}`,
      data,
      emptyComponents,
    )
    expect.value(result).toEqual(#data(` Carlo.  John.  Megan. `))
  })

  test("Component", ({expect, _}) => {
    let ohHai: Acutis_Types.template<_> = (. env, props, templates) =>
      env.render(. Compile.makeAst("{{ Children }} Oh hai {{ name }}."), props, templates)
    let components = dict([("OhHai", ohHai)])
    let result = render(
      `{% OhHai name="Mark" %} I did not. {% /OhHai %}`,
      Js.Dict.empty(),
      components,
    )
    expect.value(result).toEqual(#data(` I did not.  Oh hai Mark.`))
    let addOne: Acutis_Types.template<_> = (. env, props, children) => {
      let index =
        props->Js.Dict.get("index")->Belt.Option.flatMap(Js.Json.decodeNumber)->Belt.Option.getExn
      env.render(. Compile.makeAst(Belt.Float.toString(index +. 1.0)), props, children)
    }
    let components = dict([("AddOne", addOne)])
    let data = dict([("a", json(`[{"name": "John"}, {"name": "Megan"}]`))])
    let result = render(
      `{% map a with {name}, index %} {% AddOne index / %} {{ name }}. {% /map %}`,
      data,
      components,
    )
    expect.value(result).toEqual(#data(` 1 John.  2 Megan. `))
    let result = render(
      `{% map a with {name}, i %} {% AddOne index=i / %} {{ name }}. {% /map %}`,
      data,
      components,
    )
    expect.value(result).toEqual(#data(` 1 John.  2 Megan. `))
  })
})

describe("Nullish coalescing", ({test, _}) => {
  test("Nullish coalescing", ({expect, _}) => {
    let children = Js.Dict.fromArray([("Z", render("z", Js.Dict.empty(), Js.Dict.empty()))])
    let props = Js.Dict.fromArray([("b", Js.Json.string("b")), ("c", Js.Json.string("c"))])
    let result = render(`{{ a ? b ? c }}`, props, Js.Dict.empty())
    expect.value(result).toEqual(#data("b"))
    let result = render(`{{ a ? d ? c }}`, props, Js.Dict.empty())
    expect.value(result).toEqual(#data("c"))
    let result = render(`{{ a ? B ? Z }}`, Js.Dict.empty(), Js.Dict.empty(), ~children)
    expect.value(result).toEqual(#data("z"))
    let result = render(`{{ a ? B ? X ? "y" }}`, Js.Dict.empty(), Js.Dict.empty(), ~children)
    expect.value(result).toEqual(#data("y"))
    let result = render(` {{~ &a ? B ? X ? 1 ~}} `, Js.Dict.empty(), Js.Dict.empty(), ~children)
    expect.value(result).toEqual(#data("1"))
    let result = render(`{{ &x ? Y ? Z ? X }}`, Js.Dict.empty(), Js.Dict.empty(), ~children)
    expect.value(result).toEqual(#data("z"))
  })
})

describe("Template sections", ({test, _}) => {
  test("Default `Children` child", ({expect, _}) => {
    let a: Acutis_Types.template<_> = (. env, props, children) => {
      env.render(. Compile.makeAst("{{ Children }}"), props, children)
    }
    let components = dict([("A", a)])
    let result = render(`{% A %} b {%/ A %}`, Js.Dict.empty(), components)
    expect.value(result).toEqual(#data(" b "))
    let result = render(`{% A Children=#%} b {%/# / %}`, Js.Dict.empty(), components)
    expect.value(result).toEqual(#data(" b "))
  })

  test("Child props are passed correctly", ({expect, _}) => {
    let x: Acutis_Types.template<_> = (. env, props, children) => {
      env.render(. Compile.makeAst("{{ PassthroughChild }}"), props, children)
    }
    let y: Acutis_Types.template<_> = (. env, props, children) => {
      env.render(. Compile.makeAst("{% X PassthroughChild=A /%}"), props, children)
    }
    let components = dict([("X", x), ("Y", y)])
    let result = render(`{% Y A=#%} a {%/# / %}`, Js.Dict.empty(), components)
    expect.value(result).toEqual(#data(" a "))
  })

  test("Child props are passed correctly with punning", ({expect, _}) => {
    let x: Acutis_Types.template<_> = (. env, props, children) => {
      env.render(. Compile.makeAst("{{ PassthroughChild }}"), props, children)
    }
    let y: Acutis_Types.template<_> = (. env, props, children) => {
      env.render(. Compile.makeAst("{% X PassthroughChild /%}"), props, children)
    }
    let components = dict([("X", x), ("Y", y)])
    let result = render(`{% Y PassthroughChild=#%} a {%/# / %}`, Js.Dict.empty(), components)
    expect.value(result).toEqual(#data(" a "))
  })
})

describe("API helper functions", ({test, _}) => {
  test("env.return", ({expect, _}) => {
    let x: Acutis_Types.template<_> = (. env, _props, _children) => {
      env.return(. "a")
    }
    let components = dict([("X", x)])
    let result = render(`{% X / %}`, Js.Dict.empty(), components)
    expect.value(result).toEqual(#data("a"))
  })

  test("env.error", ({expect, _}) => {
    let x: Acutis_Types.template<_> = (. env, _props, _children) => {
      env.error(. "e")
    }
    let components = dict([("X", x)])
    let result = render(`{% X / %}`, Js.Dict.empty(), components)
    expect.value(result).toEqual(
      #errors([
        {
          message: "e",
          location: None,
          kind: #Render,
          path: [Js.Json.null],
          exn: None,
        },
      ]),
    )
  })

  test("env.mapChild", ({expect, _}) => {
    let x: Acutis_Types.template<_> = (. env, _props, children) => {
      env.mapChild(.Js.Dict.unsafeGet(children, "Children"), (. child) =>
        Js.String.toUpperCase(child)
      )
    }
    let components = dict([("X", x)])
    let result = render(`{% X ~%} a {%~ /X %}`, Js.Dict.empty(), components)
    expect.value(result).toEqual(#data("A"))
    let errors = render(~name="A", `{% X %} {{ e }} {% /X %}`, Js.Dict.empty(), components)
    expect.value(errors).toEqual(
      #errors([
        {
          message: "\"e\" is type null. I can only echo strings and numbers.",
          location: Some({character: 12}),
          path: [Js.Json.string("section: X#Children"), Js.Json.string("A")],
          kind: #Render,
          exn: None,
        },
      ]),
    )
  })

  test("env.flatMapChild", ({expect, _}) => {
    let x: Acutis_Types.template<_> = (. env, _props, children) => {
      env.flatMapChild(.Js.Dict.unsafeGet(children, "Children"), (. child) =>
        env.return(. Js.String.toUpperCase(child))
      )
    }
    let components = dict([("X", x)])
    let result = render(`{% X %} a {% /X %}`, Js.Dict.empty(), components)
    expect.value(result).toEqual(#data(" A "))
    let errors = render(~name="A", `{% X %} {{ e }} {% /X %}`, Js.Dict.empty(), components)
    expect.value(errors).toEqual(
      #errors([
        {
          message: "\"e\" is type null. I can only echo strings and numbers.",
          location: Some({character: 12}),
          path: [Js.Json.string("section: X#Children"), Js.Json.string("A")],
          kind: #Render,
          exn: None,
        },
      ]),
    )
  })
})
