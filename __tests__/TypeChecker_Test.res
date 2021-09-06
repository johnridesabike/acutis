/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

open TestFramework
open TypeChecker
module T = Acutis_Types
module MapString = Belt.Map.String

type rec debug = [
  | #Polymorphic
  | #Boolean
  | #Int
  | #Float
  | #String
  | #Echo
  | #Nullable(debug)
  | #List(debug)
  | #Tuple(array<debug>)
  | #Dict(debug)
  | #Record(array<(string, debug)>)
]
let rec debug = (x): debug =>
  switch x.contents {
  | Polymorphic => #Polymorphic
  | Boolean => #Boolean
  | Int => #Int
  | Float => #Float
  | String => #String
  | Echo => #Echo
  | Nullable(x) => #Nullable(debug(x))
  | List(x) => #List(debug(x))
  | Tuple(x) => #Tuple(Array.map(x.contents, debug))
  | Dict(x, _) => #Dict(debug(x))
  | Record(x) => #Record(MapString.map(x.contents, debug)->MapString.toArray)
  }

describe("basic", ({test, _}) => {
  test("pattern", ({expect, _}) => {
    let pat1: T.Ast_Pattern.t = #Object(
      Loc(1),
      [
        ("a", #True(Loc(1))),
        ("b", #Some(Loc(1), #String(Loc(1), "lol"))),
        ("c", #Null(Loc(1))),
        ("d", #Array(Loc(1), [#True(Loc(1)), #False(Loc(1))])),
      ],
    )
    let pat2: T.Ast_Pattern.t = #Object(
      Loc(1),
      [
        ("a", #False(Loc(1))),
        ("b", #Null(Loc(1))),
        ("c", #Some(Loc(1), #Float(Loc(1), 1.0))),
        ("z", #Int(Loc(1), 1)),
      ],
    )
    let (t1, _) = Local.fromPattern(pat1, Context.make())
    let (t2, _) = Local.fromPattern(pat2, Context.make())
    unify(t1, t2, ~loc=Loc(1))
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
    {% match a with !1 %} {% with null %} {% /match %}
    `
    let nodes = Compile.makeAstInternalExn(~name="test", src)
    let bindings = make(nodes)->MapString.map(debug)->MapString.toArray
    expect.value(bindings).toEqual([("a", #Nullable(#Int))])
  })
  test("Typechecker nested", ({expect, _}) => {
    let src = `
    {% match a with {b} %}
      {% match b with !1 %} {% with null %} {% /match %}
    {% /match %}
    `
    let nodes = Compile.makeAstInternalExn(~name="test", src)
    let bindingsGlobal = make(nodes)->MapString.map(debug)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([("a", #Record([("b", #Nullable(#Int))]))])
    let src = `
    {% match a with {b: {c}, d } %}
      {% match c with !1 %} {{ d }} {% with null %} {% /match %}
    {% /match %}
    `
    let nodes = Compile.makeAstInternalExn(~name="test", src)
    let bindingsGlobal = make(nodes)->MapString.map(debug)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([
      ("a", #Record([("b", #Record([("c", #Nullable(#Int))])), ("d", #Echo)])),
    ])
  })

  test("Typechecker multiple patterns", ({expect, _}) => {
    let src = `
    {% match a, b with {c}, 1 %}
      {% match c with !{d: 1} %} {% with null %} {% /match %}
    {% /match %}
    `
    let nodes = Compile.makeAstInternalExn(~name="test", src)
    let bindingsGlobal = make(nodes)->MapString.map(debug)->MapString.toArray
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
      {% match c with {e: 1} %}
      {% /match %}
      {% match c with {f} %}
        {% match f with {g} %}
          {{ g ? "g" }}
        {% with {g: !""} %}
          z
        {% with {g: null} %}
          z
        {% /match %}
      {% /match %}
    `
    let nodes = Compile.makeAstInternalExn(~name="test", src)
    let bindingsGlobal = make(nodes)->MapString.map(debug)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([
      ("a", #Nullable(#Echo)),
      ("b", #Echo),
      ("c", #Record([("d", #Echo), ("e", #Int), ("f", #Record([("g", #Nullable(#String))]))])),
    ])
  })
})

describe("component", ({test, _}) => {
  test("basic component", ({expect, _}) => {
    let src = `
    {% A a=[1, a] b=["b", b] /%}
    `
    let nodes = Compile.makeAstInternalExn(~name="test", src)
    let bindings = make(nodes)->MapString.map(debug)->MapString.toArray
    expect.value(bindings).toEqual([("a", #Int), ("b", #String)])
  })
})

/*
describe("complete vs incomplete", ({test, _}) => {
  test("nullable - global scope", ({expect, _}) => {
    let src = `
    {% match a with !1 %} {% with null %} {% /match %}
    {% match b with 1 %} {% /match %}
    {% match b with null %} {% /match %}
    `
    let bindings = catch(() => {
      let nodes = Compile.makeAstInternalExn(~name="test", src)
      make(nodes)->MapString.map(debug)->MapString.toArray
    })
    expect.value(bindings).toEqual(
      #errors([
        {
          exn: None,
          kind: #Type,
          location: Some({character: 108}),
          message: "This is type int but expected type nullable(polymorphic).",
          path: [],
        },
      ]),
    )
  })
  test("nullable - local scope", ({expect, _}) => {
    let src = `
    {% match a with !"a" with null %} {{ a ? "default" }} {% /match %}
    `
    let nodes = Compile.makeAstInternalExn(~name="test", src)
    let bindings = make(nodes)->MapString.map(debug)->MapString.toArray
    expect.value(bindings).toEqual([("a", #Nullable(#String))])
  })
})
*/
