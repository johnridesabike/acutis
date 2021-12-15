/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

open TestFramework
open Typechecker
module MapString = Belt.Map.String
module P = Untyped.Pattern

let debug = Typescheme.debug

describe("basic", ({test, _}) => {
  test("pattern", ({expect, _}) => {
    let pat1 = P.URecord(
      Loc(1),
      [
        ("a", P.UTrue(Loc(1))),
        ("b", P.USome(Loc(1), P.UString(Loc(1), "lol"))),
        ("c", P.UNull(Loc(1))),
        ("d", P.UList(Loc(1), [P.UTrue(Loc(1)), P.UFalse(Loc(1))])),
      ],
    )
    let pat2 = P.URecord(
      Loc(1),
      [
        ("a", P.UFalse(Loc(1))),
        ("b", P.UNull(Loc(1))),
        ("c", P.USome(Loc(1), P.UFloat(Loc(1), 1.0))),
        ("z", P.UInt(Loc(1), 1)),
      ],
    )
    let t1 = Local.fromPattern(pat1, Belt.MutableQueue.make(), ~name="")
    let t2 = Local.fromPattern(pat2, Belt.MutableQueue.make(), ~name="")
    unify(t1, t2, Expand, ~loc=Loc(1), ~name="")
    expect.value(debug(t1)).toEqual(
      #Record([
        ("a", #Boolean),
        ("b", #Nullable(#String)),
        ("c", #Nullable(#Float)),
        ("d", #List(#Boolean)),
        ("z", #Int),
      ]),
    )
  })
})

describe("match", ({test, _}) => {
  test("basic 1", ({expect, _}) => {
    let src = `
    {% match a with !1 %} {% with null %} {% with !_ %} {% /match %}
    `
    let {prop_types, _} = Compile.make(~name="test", src, Compile.Components.empty())->Result.getExn
    let bindings = prop_types->MapString.map(debug)->MapString.toArray
    expect.value(bindings).toEqual([("a", #Nullable(#Int))])
  })
  test("Typechecker nested", ({expect, _}) => {
    let src = `
    {% match a with {b} %}
      {% match b with !1 %} {% with null %} {% with !_ %} {% /match %}
    {% /match %}
    `
    let {prop_types, _} = Compile.make(~name="test", src, Compile.Components.empty())->Result.getExn
    let bindingsGlobal = prop_types->MapString.map(debug)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([("a", #Record([("b", #Nullable(#Int))]))])
    let src = `
    {% match a with {b: {c}, d } %}
      {% match c with !1 %} {{ d }} {% with null %} {% with !_ %} {% /match %}
    {% /match %}
    `
    let {prop_types, _} = Compile.make(~name="test", src, Compile.Components.empty())->Result.getExn
    let bindingsGlobal = prop_types->MapString.map(debug)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([
      ("a", #Record([("b", #Record([("c", #Nullable(#Int))])), ("d", #Echo)])),
    ])
  })

  test("Typechecker multiple patterns", ({expect, _}) => {
    let src = `
    {% match a, b with {c}, 1 %}
      {% match c with !{d: 1} %} {% with null %} {% with !_ %} {% /match %}
    {% with _, _ %}
    {% /match %}
    `
    let {prop_types, _} = Compile.make(~name="test", src, Compile.Components.empty())->Result.getExn
    let bindingsGlobal = prop_types->MapString.map(debug)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([
      ("a", #Record([("c", #Nullable(#Record([("d", #Int)])))])),
      ("b", #Int),
    ])
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
    let bindingsGlobal = prop_types->MapString.map(debug)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([
      ("a", #Nullable(#Echo)),
      ("b", #Echo),
      ("c", #Record([("d", #Echo), ("e", #Int), ("f", #Record([("g", #Nullable(#String))]))])),
    ])
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
    let bindings = prop_types->MapString.map(debug)->MapString.toArray
    expect.value(bindings).toEqual([("a", #Int), ("b", #String)])
  })
})
