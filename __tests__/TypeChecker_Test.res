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
open TypeChecker
module T = Acutis_Types
module Queue = Belt.MutableQueue
module MutMapString = Belt.MutableMap.String
module MapString = Belt.Map.String

describe("basic", ({test, _}) => {
  test("pattern", ({expect, _}) => {
    let pat1: T.Ast_Pattern.t = #Object(
      Loc(1),
      [
        ("a", #True(Loc(1))),
        ("b", #String(Loc(1), "lol")),
        ("c", #Null(Loc(1))),
        ("d", #Array(Loc(1), [#True(Loc(1)), #False(Loc(1))])),
      ],
    )
    let pat2: T.Ast_Pattern.t = #Object(
      Loc(1),
      [
        ("a", #False(Loc(1))),
        ("b", #Null(Loc(1))),
        ("c", #Float(Loc(1), 1.0)),
        ("z", #Int(Loc(1), 1)),
      ],
    )
    let (t1, _) = Local.fromPattern(pat1, Context.make())
    let (t2, _) = Local.fromPattern(pat2, Context.make())
    unify(t1, t2)
    expect.value(debug(t1)).toEqual(
      #Record([
        ("a", #Boolean),
        ("b", #Nullable(#String)),
        ("c", #Nullable(#Float)),
        ("d", #Array(#Boolean)),
        ("z", #Int),
      ]),
    )
  })
})

describe("match", ({test, _}) => {
  test("basic 1", ({expect, _}) => {
    let src = `
    {% match a with 1 %} {% with null %} {% /match %}
    `
    let nodes = Compile.makeAstInternalExn(~name="test", src)
    let bindings = make(nodes)->MapString.map(debug)->MapString.toArray
    expect.value(bindings).toEqual([("a", #Nullable(#Int))])
  })
  test("Typechecker nested", ({expect, _}) => {
    let src = `
    {% match a with {b} %}
      {% match b with 1 %} {% with null %} {% /match %}
    {% /match %}
    `
    let nodes = Compile.makeAstInternalExn(~name="test", src)
    let bindingsGlobal = make(nodes)->MapString.map(debug)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([("a", #Record([("b", #Nullable(#Int))]))])
    let src = `
    {% match a with {b: {c}, d } %}
      {% match c with 1 %} {{ d }} {% with null %} {% /match %}
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
      {% match c with {d: 1} %} {% with null %} {% /match %}
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
        {% with {g: ""} %}
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
