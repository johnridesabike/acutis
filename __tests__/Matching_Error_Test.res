/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
open TestFramework
// module Array = Belt.Array
module T = Acutis_Types
module NE = NonEmpty
module TC = Typechecker
let ne = NE.fromArrayExn

let g = Utils.Dagmap.make(Belt.HashMap.String.make(~hintSize=0), ~f=(. _, _) => assert false)

let makeCases = c => {
  let (_, cases) = TC.makeCases(ne(c), TC.Context.make(), ~loc=Loc(0), ~name="", g)
  cases
}

describe("Unused patterns", ({test, _}) => {
  test("Basic dec tree 2", ({expect, _}) => {
    let l = l => Debug.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let case1 = {
      T.Ast.patterns: [
        [#Int(l(0), 10), #Int(l(1), 11), #Int(l(2), 12)]->ne,
        [#Binding(l(3), "x"), #Int(l(4), 21), #Int(l(5), 22)]->ne,
        [#Int(l(6), 10), #Int(l(7), 11), #Int(l(8), 12)]->ne, // unused
      ]->ne,
      nodes: nodes1,
    }
    let result =
      [case1]->makeCases->Matching.make(~loc=Loc(0), ~name="")->Belt.Result.map(x => x.tree)
    expect.value(result).toEqual(
      Error({
        message: `This match case is unused:
10, 11, 12`,
        kind: #Matching,
        location: None,
        exn: None,
        path: [],
      }),
    )
    let case1 = {
      T.Ast.patterns: [
        [#Int(l(0), 10), #Int(l(1), 11), #Int(l(2), 12)]->ne,
        [#Binding(l(3), "x"), #Int(l(4), 21), #Int(l(5), 22)]->ne,
        [#Int(l(9), 30), #Int(l(10), 31), #Int(l(11), 32)]->ne,
        [#Int(l(12), 30), #Binding(l(13), "y"), #Int(l(14), 42)]->ne,
        [#Int(l(15), 30), #Int(l(16), 31), #Int(l(17), 42)]->ne, // unused
      ]->ne,
      nodes: nodes1,
    }
    let result = [case1]->makeCases->Matching.make(~loc=Loc(0), ~name="")
    expect.value(result).toEqual(
      Error({
        message: `This match case is unused:
30, 31, 42`,
        kind: #Matching,
        location: None,
        exn: None,
        path: [],
      }),
    )
  })
  test("Nests merge into wildcards correctly 2", ({expect, _}) => {
    let l = l => Debug.Loc(l)
    let n1 = [T.Ast.Text("", NoTrim)]
    let n2 = [T.Ast.Text("", NoTrim)]
    let c1 = {
      T.Ast.patterns: [[#Binding(l(3), "x"), #Binding(l(4), "y")]->ne]->ne,
      nodes: n1,
    }
    let c2 = {
      T.Ast.patterns: [
        [#Tuple(l(0), [#Binding(l(0), "_"), #Binding(l(1), "_")]), #Int(l(2), 40)]->ne,
      ]->ne,
      nodes: n2,
    }
    let result = [c1, c2]->makeCases->Matching.make(~loc=Loc(0), ~name="")
    expect.value(result).toEqual(
      Error({
        message: `This match case is unused:
(_, _), 40`,
        kind: #Matching,
        location: None,
        exn: None,
        path: [],
      }),
    )
  })
  test("Unused nest patterns are reported correctly.", ({expect, _}) => {
    let l = l => Debug.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let nodes2 = [T.Ast.Text("", NoTrim)]
    let nodes3 = [T.Ast.Text("", NoTrim)]
    let case1 = {
      T.Ast.patterns: [[#Binding(l(0), "x"), #Int(l(1), 1)]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      T.Ast.patterns: [
        [#Tuple(l(2), [#String(l(3), "a"), #String(l(4), "b")]), #Int(l(5), 10)]->ne,
      ]->ne,
      nodes: nodes2,
    }
    let case3 = {
      T.Ast.patterns: [
        [#Tuple(l(6), [#String(l(7), "a"), #String(l(8), "b")]), #Int(l(9), 1)]->ne,
      ]->ne,
      nodes: nodes3,
    }
    let result = [case1, case2, case3]->makeCases->Matching.make(~loc=Loc(0), ~name="")
    expect.value(result).toEqual(
      Error({
        message: `This match case is unused:
("a", "b"), 1`,
        kind: #Matching,
        location: None,
        exn: None,
        path: [],
      }),
    )
  })
})

describe("Partial matching", ({test, _}) => {
  let getError = x =>
    switch x {
    | Error(e) => Some(e.Debug.message)
    | Ok(_) => None
    }
  test("Partial match test 1", ({expect, _}) => {
    let l = l => Debug.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let nodes2 = [T.Ast.Text("", NoTrim)]
    let case1 = {
      T.Ast.patterns: [
        [#Int(l(0), 0)]->ne,
        [#Int(l(3), 10)]->ne,
        [#Int(l(3), 20)]->ne,
        [#Int(l(3), 30)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let case2 = {
      T.Ast.patterns: [[#Int(l(0), 15)]->ne]->ne,
      nodes: nodes2,
    }
    let result =
      [case1, case2]
      ->makeCases
      ->Matching.make(~loc=Loc(0), ~name="")
      ->Belt.Result.flatMap(x => Matching.ParMatch.check(x.tree, ~loc=Loc(0)))
      ->getError
    expect.value(result).toEqual(
      Some(`This pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
1`),
    )
    let case1 = {
      T.Ast.patterns: [[#Array(l(0), [])]->ne, [#Array(l(0), [#Binding(l(1), "_")])]->ne]->ne,
      nodes: nodes1,
    }
    let result =
      [case1]
      ->makeCases
      ->Matching.make(~loc=Loc(0), ~name="")
      ->Belt.Result.flatMap(x => Matching.ParMatch.check(x.tree, ~loc=Loc(0)))
      ->getError
    expect.value(result).toEqual(
      Some(`This pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[_, ..._]`),
    )
    let case1 = {
      T.Ast.patterns: [[#Array(l(0), [#Binding(l(1), "_")])]->ne]->ne,
      nodes: nodes1,
    }
    let result =
      [case1]
      ->makeCases
      ->Matching.make(~loc=Loc(0), ~name="")
      ->Belt.Result.flatMap(x => Matching.ParMatch.check(x.tree, ~loc=Loc(0)))
      ->getError
    expect.value(result).toEqual(
      Some(`This pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[]`),
    )
    let case1 = {
      T.Ast.patterns: [[#Object(l(0), [("b", #Int(l(1), 10))])]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      T.Ast.patterns: [[#Object(l(2), [("a", #Int(l(3), 20))])]->ne]->ne,
      nodes: nodes2,
    }
    let result =
      [case1, case2]
      ->makeCases
      ->Matching.make(~loc=Loc(0), ~name="")
      ->Belt.Result.flatMap(x => Matching.ParMatch.check(x.tree, ~loc=Loc(0)))
      ->getError
    expect.value(result).toEqual(
      Some(`This pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{a, b: 0}`),
    )
  })
})
