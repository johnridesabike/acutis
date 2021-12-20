/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
open TestFramework
// module Array = Belt.Array
module NE = NonEmpty
module TC = Typechecker
module P = Parser.Pattern
let ne = NE.fromArrayExn

let g = Utils.Dagmap.make(Belt.HashMap.String.make(~hintSize=0), ~f=(. _, _) => assert false)

let makeCases = c => {
  let (_, cases) = TC.makeCases(ne(c), TC.Context.make(#Component), ~loc=Loc(0), ~name="", g)
  cases
}

describe("Unused patterns", ({test, _}) => {
  test("Basic dec tree 2", ({expect, _}) => {
    let nodes1 = [Parser.UText("", NoTrim)]
    let case1 = {
      Parser.patterns: [
        [P.UInt(Loc(0), 10), UInt(Loc(1), 11), UInt(Loc(2), 12)]->ne,
        [P.UBinding(Loc(3), "x"), UInt(Loc(4), 21), UInt(Loc(5), 22)]->ne,
        [P.UInt(Loc(6), 10), UInt(Loc(7), 11), UInt(Loc(8), 12)]->ne, // unused
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
      Parser.patterns: [
        [P.UInt(Loc(0), 10), UInt(Loc(1), 11), UInt(Loc(2), 12)]->ne,
        [P.UBinding(Loc(3), "x"), UInt(Loc(4), 21), UInt(Loc(5), 22)]->ne,
        [P.UInt(Loc(9), 30), UInt(Loc(10), 31), UInt(Loc(11), 32)]->ne,
        [P.UInt(Loc(12), 30), UBinding(Loc(13), "y"), UInt(Loc(14), 42)]->ne,
        [P.UInt(Loc(15), 30), UInt(Loc(16), 31), UInt(Loc(17), 42)]->ne, // unused
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
    let n1 = [Parser.UText("", NoTrim)]
    let n2 = [Parser.UText("", NoTrim)]
    let c1 = {
      Parser.patterns: [[P.UBinding(Loc(3), "x"), UBinding(Loc(4), "y")]->ne]->ne,
      nodes: n1,
    }
    let c2 = {
      Parser.patterns: [
        [P.UTuple(Loc(0), [UBinding(Loc(0), "_"), UBinding(Loc(1), "_")]), UInt(Loc(2), 40)]->ne,
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
    let nodes1 = [Parser.UText("", NoTrim)]
    let nodes2 = [Parser.UText("", NoTrim)]
    let nodes3 = [Parser.UText("", NoTrim)]
    let case1 = {
      Parser.patterns: [[P.UBinding(Loc(0), "x"), UInt(Loc(1), 1)]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      Parser.patterns: [
        [P.UTuple(Loc(2), [UString(Loc(3), "a"), UString(Loc(4), "b")]), UInt(Loc(5), 10)]->ne,
      ]->ne,
      nodes: nodes2,
    }
    let case3 = {
      Parser.patterns: [
        [P.UTuple(Loc(6), [UString(Loc(7), "a"), UString(Loc(8), "b")]), UInt(Loc(9), 1)]->ne,
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
  let getError = (x, f) =>
    switch x {
    | Ok(x) =>
      try {
        f(x)
        None
      } catch {
      | Debug.Exit(e) => Some(e.message)
      }
    | Error(_) => None
    }
  test("Partial match test 1", ({expect, _}) => {
    let nodes1 = [Parser.UText("", NoTrim)]
    let nodes2 = [Parser.UText("", NoTrim)]
    let case1 = {
      Parser.patterns: [
        [P.UInt(Loc(0), 0)]->ne,
        [P.UInt(Loc(3), 10)]->ne,
        [P.UInt(Loc(3), 20)]->ne,
        [P.UInt(Loc(3), 30)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let case2 = {
      Parser.patterns: [[P.UInt(Loc(0), 15)]->ne]->ne,
      nodes: nodes2,
    }
    let result =
      [case1, case2]
      ->makeCases
      ->Matching.make(~loc=Loc(0), ~name="")
      ->getError(x => Matching.ParMatch.check(x.tree, ~loc=Loc(0)))
    expect.value(result).toEqual(
      Some(`This pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
1`),
    )
    let case1 = {
      Parser.patterns: [
        [P.UList(Loc(0), [])]->ne,
        [P.UList(Loc(0), [UBinding(Loc(1), "_")])]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let result =
      [case1]
      ->makeCases
      ->Matching.make(~loc=Loc(0), ~name="")
      ->getError(x => Matching.ParMatch.check(x.tree, ~loc=Loc(0)))
    expect.value(result).toEqual(
      Some(`This pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[_, ..._]`),
    )
    let case1 = {
      Parser.patterns: [[P.UList(Loc(0), [UBinding(Loc(1), "_")])]->ne]->ne,
      nodes: nodes1,
    }
    let result =
      [case1]
      ->makeCases
      ->Matching.make(~loc=Loc(0), ~name="")
      ->getError(x => Matching.ParMatch.check(x.tree, ~loc=Loc(0)))
    expect.value(result).toEqual(
      Some(`This pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[]`),
    )
    let case1 = {
      Parser.patterns: [[P.URecord(Loc(0), [("b", UInt(Loc(1), 10))])]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      Parser.patterns: [[P.URecord(Loc(2), [("a", UInt(Loc(3), 20))])]->ne]->ne,
      nodes: nodes2,
    }
    let result =
      [case1, case2]
      ->makeCases
      ->Matching.make(~loc=Loc(0), ~name="")
      ->getError(x => Matching.ParMatch.check(x.tree, ~loc=Loc(0)))
    expect.value(result).toEqual(
      Some(`This pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{a, b: 0}`),
    )
  })
})
