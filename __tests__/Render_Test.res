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

let json = x =>
  try {
    Js.Json.parseExn(x)
  } catch {
  | _ => Js.Json.string("THIS IS EASIER THAN DEALING WITH EXCEPTIONS.")
  }

let dict = Js.Dict.fromArray

let render = (~name="", ~children=Js.Dict.empty(), src, props, components) =>
  Source.string(~name, src)
  ->Compile.make(components)
  ->Result.flatMap(f => f(Environment.sync, props, children))

@raises(Not_found)
describe("Render essentials", ({test, _}) => {
  test("Basic", ({expect, _}) => {
    let props = dict([
      ("a", Js.Json.string("Hello")),
      ("b", Js.Json.string("World")),
      ("c", Js.Json.string("&\"'></`=")),
    ])
    let children = dict([("Z", render(`Z`, props, Compile.Components.empty()))])
    let result = render(
      `{{ a }} {{ b }}! {{ c }} {{ &c }} {{ &"<" }} {{ "d" }} {{ 1.5 }} {{ Z }}`,
      props,
      ~children,
      Compile.Components.empty(),
    )
    expect.value(result).toEqual(
      #ok("Hello World! &amp;&quot;&apos;&gt;&lt;&#x2F;&#x60;&#x3D; &\"'></`= < d 1.5 Z"),
    )
  })

  test("Unbound identifiers default to null", ({expect, _}) => {
    let result = render(
      "{% match a with null %} a doesn't exist. {% with a %} {{ a }} {% /match %}",
      Js.Dict.empty(),
      Compile.Components.empty(),
    )
    expect.value(result).toEqual(#ok(" a doesn't exist. "))
  })

  test("Nested comments", ({expect, _}) => {
    let result = render("a {* b {* c *} d *} e", Js.Dict.empty(), Compile.Components.empty())
    expect.value(result).toEqual(#ok("a  e"))
  })

  test("Match", ({expect, _}) => {
    let data = dict([("a", json(`{"a": "a"}`))])
    let result = render(
      `{% match a with {a} %} {{ a }} {% /match %}`,
      data,
      Compile.Components.empty(),
    )
    expect.value(result).toEqual(#ok(` a `))
    let data = dict([("a", json(`{"a": "a", "b": true}`))])
    let result = render(
      `{% match a with {b: false, a} %} b is false. {{ a }} {% with {b: true, a} %} b is true. {{ a }} {% /match %}`,
      data,
      Compile.Components.empty(),
    )
    expect.value(result).toEqual(#ok(` b is true. a `))
    let data = dict([("a", json(`{"a": true, "b": {"c": "hi"}}`))])
    let result = render(
      `{% match a with {a: false} %} a is false. {% with {a: true, b} %} {% match b with {c} %} {{ c }} {% /match %} {% /match %}`,
      data,
      Compile.Components.empty(),
    )
    expect.value(result).toEqual(#ok(`  hi  `))
    let data = dict([("a", json(`{"a": {"b": "hi"}}`))])
    let result = render(
      `{% match a with {a: {b}} %} {{ b }} {% /match %}`,
      data,
      Compile.Components.empty(),
    )
    expect.value(result).toEqual(#ok(` hi `))
  })

  test("Whitespace control", ({expect, _}) => {
    let data = dict([("a", json(`{"a": {"b": "hi"}}`))])
    let result = render(
      `{% match a with {a: {b}} ~%}  
   \t  _ {{ b }} _
    \t \r  {%~ /match %}`,
      data,
      Compile.Components.empty(),
    )
    expect.value(result).toEqual(#ok(`_ hi _`))
    let data = dict([("a", json(`{"a": {"b": "hi"}}`))])
    let result = render(
      `{% match a with {a: {b}} %} _ {{~ b ~}} _ {% /match %}`,
      data,
      Compile.Components.empty(),
    )
    expect.value(result).toEqual(#ok(` _hi_ `))
    let ohHai = Source.funcWithString(~name="OhHai", "{{ Children }} Oh hai {{ name }}.", (
      ast,
      env,
      props,
      templates,
    ) => env.render(. ast, props, templates))
    let components = Compile.Components.make([ohHai])
    let result = render(
      `{% OhHai name="Mark" ~%} I did not. {%~ /OhHai %}`,
      Js.Dict.empty(),
      components->Result.getExn,
    )
    expect.value(result).toEqual(#ok(`I did not. Oh hai Mark.`))
    let result = render(
      `{% OhHai name="Mark" Children=#~%} I did not. {%~/# /%}`,
      Js.Dict.empty(),
      components->Result.getExn,
    )
    expect.value(result).toEqual(#ok(`I did not. Oh hai Mark.`))
  })

  test("Map", ({expect, _}) => {
    let data = dict([("a", json(`[{"name": "John"}, {"name": "Megan"}]`))])
    let result = render(
      `{% map a with {name} %} {{ name }} {% /map %}`,
      data,
      Compile.Components.empty(),
    )
    expect.value(result).toEqual(#ok(` John  Megan `))
    let data = dict([("a", json(`[{"name": "John"}, {"name": "Megan"}]`)), ("b", json(`"hi"`))])
    let result = render(
      `{% map a with {name} %} {{ b }} {{ name }}. {% /map %}`,
      data,
      Compile.Components.empty(),
    )
    expect.value(result).toEqual(#ok(` hi John.  hi Megan. `))
    let result = render(
      `{% map a with {name}, index %} {{ index }} {{ name }}. {% /map %}`,
      data,
      Compile.Components.empty(),
    )
    expect.value(result).toEqual(#ok(` 0 John.  1 Megan. `))
    let result = render(
      `{% map [{name: "Carlo"}, {name: "John"}] with {name} %} {{ name }}. {% /map %}`,
      data,
      Compile.Components.empty(),
    )
    expect.value(result).toEqual(#ok(` Carlo.  John. `))
    let result = render(
      `{% map [{name: "Carlo"}, ...a] with {name} %} {{ name }}. {% /map %}`,
      data,
      Compile.Components.empty(),
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
      Compile.Components.empty(),
    )
    expect.value(result).toEqual(#ok(`lisa: computers. tommy: banking. `))
    let result = render(
      `{% map_dict <
          lisa: {job: "computers"},
          tommy: {job: "banking"}
        > with {job}, key ~%} {{ key }}: {{ job }}. {% /map_dict %}`,
      Js.Dict.empty(),
      Compile.Components.empty(),
    )
    expect.value(result).toEqual(#ok(`lisa: computers. tommy: banking. `))
  })

  test("Component", ({expect, _}) => {
    let ohHai = Source.funcWithString(~name="OhHai", "{{ Children }} Oh hai {{ name }}.", (
      ast,
      env,
      props,
      templates,
    ) => env.render(. ast, props, templates))
    let components = Compile.Components.make([ohHai])
    let result = render(
      `{% OhHai name="Mark" %} I did not. {% /OhHai %}`,
      Js.Dict.empty(),
      components->Result.getExn,
    )
    expect.value(result).toEqual(#ok(` I did not.  Oh hai Mark.`))

    @raises(Not_found)
    let addOne = Source.func(~name="AddOne", (env, props, _children) => {
      let index =
        props->Js.Dict.get("index")->Belt.Option.flatMap(Js.Json.decodeNumber)->Belt.Option.getExn
      env.return(. Belt.Float.toString(index +. 1.0))
    })

    @raises(Not_found)
    let components = Compile.Components.make([addOne])
    let data = dict([("a", json(`[{"name": "John"}, {"name": "Megan"}]`))])
    let result = render(
      `{% map a with {name}, index %} {% AddOne index / %} {{ name }}. {% /map %}`,
      data,
      components->Result.getExn,
    )
    expect.value(result).toEqual(#ok(` 1 John.  2 Megan. `))
    let result = render(
      `{% map a with {name}, i %} {% AddOne index=i / %} {{ name }}. {% /map %}`,
      data,
      components->Result.getExn,
    )
    expect.value(result).toEqual(#ok(` 1 John.  2 Megan. `))
  })
})

describe("Nullish coalescing", ({test, _}) => {
  test("Nullish coalescing", ({expect, _}) => {
    let children = Js.Dict.fromArray([
      ("Z", render("z", Js.Dict.empty(), Compile.Components.empty())),
    ])
    let props = Js.Dict.fromArray([("b", Js.Json.string("b")), ("c", Js.Json.string("c"))])
    let result = render(`{{ a ? b ? c }}`, props, Compile.Components.empty())
    expect.value(result).toEqual(#ok("b"))
    let result = render(`{{ a ? d ? c }}`, props, Compile.Components.empty())
    expect.value(result).toEqual(#ok("c"))
    let result = render(`{{ a ? B ? Z }}`, Js.Dict.empty(), Compile.Components.empty(), ~children)
    expect.value(result).toEqual(#ok("z"))
    let result = render(
      `{{ a ? B ? X ? "y" }}`,
      Js.Dict.empty(),
      Compile.Components.empty(),
      ~children,
    )
    expect.value(result).toEqual(#ok("y"))
    let result = render(
      ` {{~ &a ? B ? X ? 1 ~}} `,
      Js.Dict.empty(),
      Compile.Components.empty(),
      ~children,
    )
    expect.value(result).toEqual(#ok("1"))
    let result = render(
      `{{ &x ? Y ? Z ? X }}`,
      Js.Dict.empty(),
      Compile.Components.empty(),
      ~children,
    )
    expect.value(result).toEqual(#ok("z"))
  })
})

describe("Template sections", ({test, _}) => {
  test("Default `Children` child", ({expect, _}) => {
    let a = Source.funcWithString(~name="A", "{{ Children }}", (ast, env, props, children) => {
      env.render(. ast, props, children)
    })
    let components = Compile.Components.make([a])
    let result = render(`{% A %} b {%/ A %}`, Js.Dict.empty(), components->Result.getExn)
    expect.value(result).toEqual(#ok(" b "))
    let result = render(`{% A Children=#%} b {%/# / %}`, Js.Dict.empty(), components->Result.getExn)
    expect.value(result).toEqual(#ok(" b "))
  })

  test("Child props are passed correctly", ({expect, _}) => {
    let x = Source.funcWithString(~name="X", "{{ PassthroughChild }}", (
      ast,
      env,
      props,
      children,
    ) => {
      env.render(. ast, props, children)
    })
    let y = Source.funcWithString(~name="Y", "{% X PassthroughChild=A /%}", (
      ast,
      env,
      props,
      children,
    ) => {
      env.render(. ast, props, children)
    })
    let components = Compile.Components.make([x, y])
    let result = render(`{% Y A=#%} a {%/# / %}`, Js.Dict.empty(), components->Result.getExn)
    expect.value(result).toEqual(#ok(" a "))
  })

  test("Child props are passed correctly with punning", ({expect, _}) => {
    let x = Source.funcWithString(~name="X", "{{ PassthroughChild }}", (
      ast,
      env,
      props,
      children,
    ) => {
      env.render(. ast, props, children)
    })
    let y = Source.funcWithString(~name="Y", "{% X PassthroughChild /%}", (
      ast,
      env,
      props,
      children,
    ) => {
      env.render(. ast, props, children)
    })
    let components = Compile.Components.make([x, y])
    let result = render(
      `{% Y PassthroughChild=#%} a {%/# / %}`,
      Js.Dict.empty(),
      components->Result.getExn,
    )
    expect.value(result).toEqual(#ok(" a "))
  })
})

describe("API helper functions", ({test, _}) => {
  test("env.return", ({expect, _}) => {
    let x = Source.func(~name="X", (env, _props, _children) => {
      env.return(. "a")
    })
    let components = Compile.Components.make([x])
    let result = render(`{% X / %}`, Js.Dict.empty(), components->Result.getExn)
    expect.value(result).toEqual(#ok("a"))
  })

  test("env.error", ({expect, _}) => {
    let x = Source.func(~name="X", (env, _props, _children) => {
      env.error(. "e")
    })
    let components = Compile.Components.make([x])
    let result = render(`{% X / %}`, Js.Dict.empty(), components->Result.getExn)
    expect.value(result).toEqual(
      #errors([
        {
          message: "e",
          location: None,
          kind: #Render,
          path: [Js.Json.string("")],
          exn: None,
        },
      ]),
    )
  })

  test("env.mapChild", ({expect, _}) => {
    let x = Source.func(~name="X", (env, _props, children) => {
      env.mapChild(.Js.Dict.unsafeGet(children, "Children"), child => Js.String.toUpperCase(child))
    })
    let components = Compile.Components.make([x])
    let result = render(`{% X ~%} a {%~ /X %}`, Js.Dict.empty(), components->Result.getExn)
    expect.value(result).toEqual(#ok("A"))
    let errors = render(
      ~name="A",
      `{% X %} {{ e }} {% /X %}`,
      Js.Dict.empty(),
      components->Result.getExn,
    )
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
    let x = Source.func(~name="X", (env, _props, children) =>
      env.flatMapChild(.Js.Dict.unsafeGet(children, "Children"), child =>
        env.return(. Js.String.toUpperCase(child))
      )
    )
    let components = Compile.Components.make([x])
    let result = render(`{% X %} a {% /X %}`, Js.Dict.empty(), components->Result.getExn)
    expect.value(result).toEqual(#ok(" A "))
    let errors = render(
      ~name="A",
      `{% X %} {{ e }} {% /X %}`,
      Js.Dict.empty(),
      components->Result.getExn,
    )
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
