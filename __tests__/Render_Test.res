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

let json = Js.Json.parseExn
let emptyComponents = Js.Dict.empty()
let dict = Js.Dict.fromArray

let render = (~name=?, ~children=Js.Dict.empty(), src, props, components) => {
  Compile.make(src, ~name?)(. Render.makeContext(components), props, children)
}

describe("All together", ({test, _}) => {
  test("Basic", ({expect, _}) => {
    let props = dict([
      ("a", Js.Json.string("Hello")),
      ("b", Js.Json.string("World")),
      ("c", Js.Json.string("&\"'></`=")),
    ])
    let children = dict([("Z", render(`Z`, props, emptyComponents))])
    let result = render(
      `{{ a }} {{ b }}! {{ c }} {% raw c %} {{ "d" }} {{ 1.5 }} {{ Z }}`,
      props,
      ~children,
      emptyComponents,
    )
    expect.value(result).toEqual(
      #data("Hello World! &amp;&quot;&apos;&gt;&lt;&#x2F;&#x60;&#x3D; &\"'></`= d 1.5 Z"),
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
    let ohHai = (. render, props, templates) =>
      render(. Compile.makeAst("{{ Children }} Oh hai {{ name }}."), props, templates)
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
  })
  test("Component", ({expect, _}) => {
    let ohHai = (. render, props, templates) =>
      render(. Compile.makeAst("{{ Children }} Oh hai {{ name }}."), props, templates)
    let components = dict([("OhHai", ohHai)])
    let result = render(
      `{% OhHai name="Mark" %} I did not. {% /OhHai %}`,
      Js.Dict.empty(),
      components,
    )
    expect.value(result).toEqual(#data(` I did not.  Oh hai Mark.`))
    let addOne = (. render, props, children) => {
      let index =
        props->Js.Dict.get("index")->Belt.Option.flatMap(Js.Json.decodeNumber)->Belt.Option.getExn
      render(. Compile.makeAst(Belt.Float.toString(index +. 1.0)), props, children)
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

describe("Template sections", ({test, _}) => {
  test("Default `Children` child", ({expect, _}) => {
    let a = (. render, props, children) => {
      render(. Compile.makeAst("{{ Children }}"), props, children)
    }
    let components = dict([("A", a)])
    let result = render(`{% A %} b {%/ A %}`, Js.Dict.empty(), components)
    expect.value(result).toEqual(#data(" b "))
    let result = render(`{% A Children=#%} b {%/# / %}`, Js.Dict.empty(), components)
    expect.value(result).toEqual(#data(" b "))
  })

  test("Child props are passed correctly", ({expect, _}) => {
    let x = (. render, props, children) => {
      render(. Compile.makeAst("{{ PassthroughChild }}"), props, children)
    }
    let y = (. render, props, children) => {
      render(. Compile.makeAst("{% X PassthroughChild=A /%}"), props, children)
    }
    let components = dict([("X", x), ("Y", y)])
    let result = render(`{% Y A=#%} a {%/# / %}`, Js.Dict.empty(), components)
    expect.value(result).toEqual(#data(" a "))
  })

  test("Child props are passed correctly with punning", ({expect, _}) => {
    let x = (. render, props, children) => {
      render(. Compile.makeAst("{{ PassthroughChild }}"), props, children)
    }
    let y = (. render, props, children) => {
      render(. Compile.makeAst("{% X PassthroughChild /%}"), props, children)
    }
    let components = dict([("X", x), ("Y", y)])
    let result = render(`{% Y PassthroughChild=#%} a {%/# / %}`, Js.Dict.empty(), components)
    expect.value(result).toEqual(#data(" a "))
  })
})
