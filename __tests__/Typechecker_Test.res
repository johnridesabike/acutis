/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

open TestFramework
module MapString = Belt.Map.String

describe("basic", ({test, _}) => {
  test("pattern", ({expect, _}) => {
    let src = `
    {% match a
       with {a: true, b: !"lol", c: null, d: [true, false]}
       with {a: false, b: null, c: !1.0, z: 1} %}
    {% with _ %}
    {% /match %}`
    let {prop_types, _} = Compile.make(~name="test", src, Compile.Components.empty())->Result.getExn
    let result = prop_types->MapString.map(Typescheme.toString)->MapString.toArray
    expect.value(result).toEqual([
      ("a", `{"a": boolean, "b": ?string, "c": ?float, "d": [boolean], "z": int}`),
    ])
  })
})

describe("match", ({test, _}) => {
  test("basic 1", ({expect, _}) => {
    let src = `
    {% match a with !1 %} {% with null %} {% with !_ %} {% /match %}
    `
    let {prop_types, _} = Compile.make(~name="test", src, Compile.Components.empty())->Result.getExn
    let bindings = prop_types->MapString.map(Typescheme.toString)->MapString.toArray
    expect.value(bindings).toEqual([("a", `?int`)])
  })
  test("Typechecker nested", ({expect, _}) => {
    let src = `
    {% match a with {b} %}
      {% match b with !1 %} {% with null %} {% with !_ %} {% /match %}
    {% /match %}
    `
    let {prop_types, _} = Compile.make(~name="test", src, Compile.Components.empty())->Result.getExn
    let bindingsGlobal = prop_types->MapString.map(Typescheme.toString)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([("a", `{"b": ?int}`)])
    let src = `
    {% match a with {b: {c}, d } %}
      {% match c with !1 %} {{ d }} {% with null %} {% with !_ %} {% /match %}
    {% /match %}
    `
    let {prop_types, _} = Compile.make(~name="test", src, Compile.Components.empty())->Result.getExn
    let bindingsGlobal = prop_types->MapString.map(Typescheme.toString)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([("a", `{"b": {"c": ?int}, "d": echoable}`)])
  })

  test("Typechecker multiple patterns", ({expect, _}) => {
    let src = `
    {% match a, b with {c}, 1 %}
      {% match c with !{d: 1} %} {% with null %} {% with !_ %} {% /match %}
    {% with _, _ %}
    {% /match %}
    `
    let {prop_types, _} = Compile.make(~name="test", src, Compile.Components.empty())->Result.getExn
    let bindingsGlobal = prop_types->MapString.map(Typescheme.toString)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([("a", `{"c": ?{"d": int}}`), ("b", `int`)])
  })

  test("echoes", ({expect, _}) => {
    let src = `
      {{ a ? b }}
      {% match c with {d, e} %}
        {{ d }} {{ e }}
      {% /match %}
      {% match c with {e: 1} %} {% with _ %}
      {% /match %}
      {% match c with {f} %}
        {% match f with {g: !""} %}
          z
        {% with {g: null} %}
          z
        {% with {g} %}
          {{ g ? "g" }}
        {% /match %}
      {% /match %}
    `
    let {prop_types, _} = Compile.make(
      ~name="test",
      src,
      Compile.Components.empty(),
    )->Result.getOrElse(e => {
      Js.log(e)
      assert false
    })
    let bindingsGlobal = prop_types->MapString.map(Typescheme.toString)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([
      ("a", `?echoable`),
      ("b", `echoable`),
      ("c", `{"d": echoable, "e": int, "f": {"g": ?string}}`),
    ])
  })

  test("Type narrowing works when constructing values", ({expect, _}) => {
    let src = `
      {% match x with {a, b} %} {{ a }} {{ b }} {% /match %}
      {% match y with {c} %} {{ c }} {% /match %}
      {% map [{a: "a", b: 1}, {a: "a", c: true}, x, y] with {a} %}
        {{ a }}
      {% /map %}
      `
    let {prop_types, _} = Compile.make(~name="", src, Compile.Components.empty())->Result.getExn
    let result = prop_types->MapString.map(Typescheme.toString)->MapString.toArray
    expect.value(result).toEqual([
      ("x", `{"a": string, "b": echoable}`),
      ("y", `{"a": string, "c": echoable}`),
    ])
  })

  test("Inferrence works for nested types", ({expect, _}) => {
    let src = `{% match a with [{a: 1}, c, {b: "b"}] %} {% with _ %} {% /match %}`
    let {prop_types, _} = Compile.make(~name="test", src, Compile.Components.empty())->Result.getExn
    let bindingsGlobal = prop_types->MapString.map(Typescheme.toString)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([("a", `[{"a": int, "b": string}]`)])
  })
})

describe("component", ({test, _}) => {
  test("basic component", ({expect, _}) => {
    let a = Source.src(
      ~name="A",
      `{% map a with x %} {{ x }} {% /map %}
       {% map b with x %} {{ x }} {% /map %}`,
    )
    let src = `
    {% A a=[1, a] b=["b", b] /%}
    `
    let {prop_types, _} =
      Compile.make(~name="test", src, Compile.Components.make([a])->Result.getExn)->Result.getExn
    let bindings = prop_types->MapString.map(Typescheme.toString)->MapString.toArray
    expect.value(bindings).toEqual([("a", `int`), ("b", `string`)])
  })
})
