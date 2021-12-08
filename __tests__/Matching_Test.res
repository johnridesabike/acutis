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
module SI = Belt.Set.Int
module MS = Belt.Map.String
module TC = Typechecker
let ne = NE.fromArrayExn

let makeCases = c => {
  let (_, cases) = TC.makeCases(ne(c), TC.Context.make(), ~loc=Loc(0), ~name="", Utils.Dag.make([]))
  cases
}

describe("Basic tree", ({test, _}) => {
  test("blah", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let nodes2 = [T.Ast.Text("", NoTrim)]
    let case1 = {
      T.Ast.patterns: [[#Some(l(0), #Binding(l(1), "a"))]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      T.Ast.patterns: [[#Null(l(3))]->ne]->ne,
      nodes: nodes2,
    }
    let result =
      [case1, case2]->makeCases->Matching.make(~loc=Loc(0), ~name="")->Belt.Result.map(x => x.tree)
    expect.value(result).toEqual(
      Ok(
        Construct({
          idx: 0,
          key: "",
          ids: SI.empty,
          kind: TPat_Nullable,
          nil: Some(End({names: MS.empty, exit: 1})),
          cons: Some(
            Nest({
              idx: 0,
              key: "",
              ids: SI.empty,
              kind: Tuple,
              child: Wildcard({
                idx: 0,
                key: "",
                ids: SI.fromArray([1]),
                child: End(End({names: MS.fromArray([("a", 1)]), exit: 0})),
              }),
              wildcard: None,
            }),
          ),
        }),
      ),
    )
  })
  test("cases are sorted correctly", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let nodes2 = [T.Ast.Text("", NoTrim)]
    let nodes3 = [T.Ast.Text("", NoTrim)]
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
    let case3 = {
      T.Ast.patterns: [[#Binding(l(0), "_")]->ne]->ne,
      nodes: nodes3,
    }
    let result =
      [case1, case2, case3]
      ->makeCases
      ->Matching.make(~loc=Loc(0), ~name="")
      ->Belt.Result.map(x => x.tree)
    expect.value(result).toEqual(
      Ok(
        Switch({
          idx: 0,
          key: "",
          ids: SI.empty,
          cases: {
            val: TPat_Int(0),
            ifMatch: End({names: MS.empty, exit: 0}),
            nextCase: Some({
              val: TPat_Int(10),
              ifMatch: End({names: MS.empty, exit: 0}),
              nextCase: Some({
                val: TPat_Int(15),
                ifMatch: End({names: MS.empty, exit: 1}),
                nextCase: Some({
                  val: TPat_Int(20),
                  ifMatch: End({names: MS.empty, exit: 0}),
                  nextCase: Some({
                    val: TPat_Int(30),
                    ifMatch: End({names: MS.empty, exit: 0}),
                    nextCase: None,
                  }),
                }),
              }),
            }),
          },
          wildcard: Some(End({names: MS.empty, exit: 2})),
        }),
      ),
    )
  })

  test("Basic dec tree 1", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let case1 = {
      T.Ast.patterns: [
        [#Int(l(0), 1), #Int(l(1), 2), #Int(l(2), 3)]->ne,
        [#Int(l(3), 1), #Int(l(4), 4), #Int(l(5), 5)]->ne,
        [#Int(l(6), 10), #Int(l(7), 20), #Int(l(8), 30)]->ne,
        [#Int(l(9), 10), #Int(l(10), 20), #Int(l(11), 40)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let nodes2 = [T.Ast.Text("", NoTrim)]
    let case2 = {
      T.Ast.patterns: [
        [#Int(l(12), 100), #Int(l(13), 102), #Int(l(14), 103)]->ne,
        [#Int(l(15), 100), #Int(l(16), 104), #Int(l(17), 105)]->ne,
      ]->ne,
      nodes: nodes2,
    }
    let nodes3 = [T.Ast.Text("", NoTrim)]
    let case3 = {
      T.Ast.patterns: [
        [#Int(l(18), 10), #Int(l(19), 20), #Int(l(20), 50)]->ne,
        [#Int(l(21), 1), #Int(l(22), 2), #Int(l(23), 100)]->ne,
        [#Int(l(24), 1), #Int(l(25), 2), #Int(l(26), 101)]->ne,
        [#Int(l(27), 100), #Int(l(28), 102), #Int(l(29), 106)]->ne,
      ]->ne,
      nodes: nodes3,
    }
    let nodes4 = [T.Ast.Text("", NoTrim)]
    let case4 = {
      T.Ast.patterns: [[#Binding(l(30), "_"), #Binding(l(31), "_"), #Binding(l(32), "_")]->ne]->ne,
      nodes: nodes4,
    }
    let result =
      [case1, case2, case3, case4]
      ->makeCases
      ->Matching.make(~loc=Loc(0), ~name="")
      ->Belt.Result.map(x => x.tree)
    expect.value(result).toEqual(
      Ok(
        Switch({
          idx: 0,
          key: "",
          ids: SI.empty,
          cases: {
            val: TPat_Int(1),
            ifMatch: Switch({
              idx: 1,
              key: "",
              ids: SI.empty,
              cases: {
                val: TPat_Int(2),
                ifMatch: Switch({
                  idx: 2,
                  key: "",
                  ids: SI.empty,
                  cases: {
                    val: TPat_Int(3),
                    ifMatch: End({names: MS.empty, exit: 0}),
                    nextCase: Some({
                      val: TPat_Int(100),
                      ifMatch: End({names: MS.empty, exit: 2}),
                      nextCase: Some({
                        val: TPat_Int(101),
                        ifMatch: End({names: MS.empty, exit: 2}),
                        nextCase: None,
                      }),
                    }),
                  },
                  wildcard: Some(End({names: MS.empty, exit: 3})),
                }),
                nextCase: Some({
                  val: TPat_Int(4),
                  ifMatch: Switch({
                    idx: 2,
                    key: "",
                    ids: SI.empty,
                    cases: {
                      val: TPat_Int(5),
                      ifMatch: End({names: MS.empty, exit: 0}),
                      nextCase: None,
                    },
                    wildcard: Some(End({names: MS.empty, exit: 3})),
                  }),
                  nextCase: None,
                }),
              },
              wildcard: Some(
                Wildcard({
                  idx: 2,
                  key: "",
                  ids: SI.empty,
                  child: End({names: MS.empty, exit: 3}),
                }),
              ),
            }),
            nextCase: Some({
              val: TPat_Int(10),
              ifMatch: Switch({
                idx: 1,
                key: "",
                ids: SI.empty,
                cases: {
                  val: TPat_Int(20),
                  ifMatch: Switch({
                    idx: 2,
                    key: "",
                    ids: SI.empty,
                    cases: {
                      val: TPat_Int(30),
                      ifMatch: End({names: MS.empty, exit: 0}),
                      nextCase: Some({
                        val: TPat_Int(40),
                        ifMatch: End({names: MS.empty, exit: 0}),
                        nextCase: Some({
                          val: TPat_Int(50),
                          ifMatch: End({names: MS.empty, exit: 2}),
                          nextCase: None,
                        }),
                      }),
                    },
                    wildcard: Some(End({names: MS.empty, exit: 3})),
                  }),
                  nextCase: None,
                },
                wildcard: Some(
                  Wildcard({
                    idx: 2,
                    key: "",
                    ids: SI.empty,
                    child: End({names: MS.empty, exit: 3}),
                  }),
                ),
              }),
              nextCase: Some({
                val: TPat_Int(100),
                ifMatch: Switch({
                  idx: 1,
                  key: "",
                  ids: SI.empty,
                  cases: {
                    val: TPat_Int(102),
                    ifMatch: Switch({
                      idx: 2,
                      key: "",
                      ids: SI.empty,
                      cases: {
                        val: TPat_Int(103),
                        ifMatch: End({names: MS.empty, exit: 1}),
                        nextCase: Some({
                          val: TPat_Int(106),
                          ifMatch: End({names: MS.empty, exit: 2}),
                          nextCase: None,
                        }),
                      },
                      wildcard: Some(End({names: MS.empty, exit: 3})),
                    }),
                    nextCase: Some({
                      val: TPat_Int(104),
                      ifMatch: Switch({
                        idx: 2,
                        key: "",
                        ids: SI.empty,
                        cases: {
                          val: TPat_Int(105),
                          ifMatch: End({names: MS.empty, exit: 1}),
                          nextCase: None,
                        },
                        wildcard: Some(End({names: MS.empty, exit: 3})),
                      }),
                      nextCase: None,
                    }),
                  },
                  wildcard: Some(
                    Wildcard({
                      idx: 2,
                      key: "",
                      ids: SI.empty,
                      child: End({names: MS.empty, exit: 3}),
                    }),
                  ),
                }),
                nextCase: None,
              }),
            }),
          },
          wildcard: Some(
            Wildcard({
              idx: 1,
              key: "",
              ids: SI.empty,
              child: Wildcard({
                idx: 2,
                key: "",
                ids: SI.empty,
                child: End({names: MS.empty, exit: 3}),
              }),
            }),
          ),
        }),
      ),
    )
  })

  test("Basic dec tree 2", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let case1 = {
      T.Ast.patterns: [
        [#Int(l(0), 10), #Int(l(1), 11), #Int(l(2), 12)]->ne,
        [#Binding(l(3), "x"), #Int(l(4), 21), #Int(l(5), 22)]->ne,
        [#Int(l(9), 30), #Int(l(10), 31), #Int(l(11), 32)]->ne,
        [#Int(l(12), 30), #Binding(l(13), "y"), #Int(l(14), 42)]->ne,
        [#Binding(l(18), "a"), #Binding(l(19), "b"), #Binding(l(20), "c")]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let result =
      [case1]->makeCases->Matching.make(~loc=Loc(0), ~name="")->Belt.Result.map(x => x.tree)
    expect.value(result).toEqual(
      Ok(
        Switch({
          idx: 0,
          key: "",
          ids: SI.empty->SI.add(3)->SI.add(18),
          cases: {
            val: TPat_Int(10),
            ifMatch: Switch({
              idx: 1,
              key: "",
              ids: SI.fromArray([19]),
              cases: {
                val: TPat_Int(11),
                ifMatch: Switch({
                  idx: 2,
                  key: "",
                  ids: SI.fromArray([20]),
                  cases: {
                    val: TPat_Int(12),
                    ifMatch: End({names: MS.empty, exit: 0}),
                    nextCase: None,
                  },
                  wildcard: Some(
                    End({
                      exit: 0,
                      names: MS.empty->MS.set("a", 18)->MS.set("b", 19)->MS.set("c", 20),
                    }),
                  ),
                }),
                nextCase: Some({
                  val: TPat_Int(21),
                  ifMatch: Switch({
                    idx: 2,
                    key: "",
                    ids: SI.fromArray([20]),
                    cases: {
                      val: TPat_Int(22),
                      ifMatch: End({names: MS.fromArray([("x", 3)]), exit: 0}),
                      nextCase: None,
                    },
                    wildcard: Some(
                      End({
                        exit: 0,
                        names: MS.empty->MS.set("a", 18)->MS.set("b", 19)->MS.set("c", 20),
                      }),
                    ),
                  }),
                  nextCase: None,
                }),
              },
              wildcard: Some(
                Wildcard({
                  idx: 2,
                  key: "",
                  ids: SI.fromArray([20]),
                  child: End({
                    exit: 0,
                    names: MS.empty->MS.set("a", 18)->MS.set("b", 19)->MS.set("c", 20),
                  }),
                }),
              ),
            }),
            nextCase: Some({
              val: TPat_Int(30),
              ifMatch: Switch({
                idx: 1,
                key: "",
                ids: SI.empty->SI.add(13)->SI.add(19),
                cases: {
                  val: TPat_Int(21),
                  ifMatch: Switch({
                    idx: 2,
                    key: "",
                    ids: SI.fromArray([20]),
                    cases: {
                      val: TPat_Int(22),
                      ifMatch: End({names: MS.fromArray([("x", 3)]), exit: 0}),
                      nextCase: Some({
                        val: TPat_Int(42),
                        ifMatch: End({names: MS.fromArray([("y", 13)]), exit: 0}),
                        nextCase: None,
                      }),
                    },
                    wildcard: Some(
                      End({
                        exit: 0,
                        names: MS.empty->MS.set("a", 18)->MS.set("b", 19)->MS.set("c", 20),
                      }),
                    ),
                  }),
                  nextCase: Some({
                    val: TPat_Int(31),
                    ifMatch: Switch({
                      idx: 2,
                      key: "",
                      ids: SI.fromArray([20]),
                      cases: {
                        val: TPat_Int(32),
                        ifMatch: End({names: MS.empty, exit: 0}),
                        nextCase: Some({
                          val: TPat_Int(42),
                          ifMatch: End({names: MS.fromArray([("y", 13)]), exit: 0}),
                          nextCase: None,
                        }),
                      },
                      wildcard: Some(
                        End({
                          exit: 0,
                          names: MS.empty->MS.set("a", 18)->MS.set("b", 19)->MS.set("c", 20),
                        }),
                      ),
                    }),
                    nextCase: None,
                  }),
                },
                wildcard: Some(
                  Switch({
                    idx: 2,
                    key: "",
                    ids: SI.fromArray([20]),
                    cases: {
                      val: TPat_Int(42),
                      ifMatch: End({names: MS.fromArray([("y", 13)]), exit: 0}),
                      nextCase: None,
                    },
                    wildcard: Some(
                      End({
                        exit: 0,
                        names: MS.empty->MS.set("a", 18)->MS.set("b", 19)->MS.set("c", 20),
                      }),
                    ),
                  }),
                ),
              }),
              nextCase: None,
            }),
          },
          wildcard: Some(
            Switch({
              idx: 1,
              key: "",
              ids: SI.fromArray([19]),
              cases: {
                val: TPat_Int(21),
                ifMatch: Switch({
                  idx: 2,
                  key: "",
                  ids: SI.fromArray([20]),
                  cases: {
                    val: TPat_Int(22),
                    ifMatch: End({names: MS.fromArray([("x", 3)]), exit: 0}),
                    nextCase: None,
                  },
                  wildcard: Some(
                    End({
                      exit: 0,
                      names: MS.empty->MS.set("a", 18)->MS.set("b", 19)->MS.set("c", 20),
                    }),
                  ),
                }),
                nextCase: None,
              },
              wildcard: Some(
                Wildcard({
                  idx: 2,
                  key: "",
                  ids: SI.fromArray([20]),
                  child: End({
                    exit: 0,
                    names: MS.empty->MS.set("a", 18)->MS.set("b", 19)->MS.set("c", 20),
                  }),
                }),
              ),
            }),
          ),
        }),
      ),
    )
  })
})

describe("Nests", ({test, _}) => {
  test("dec tree tuple", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let case1 = {
      T.Ast.patterns: [
        [#Tuple(l(0), [#Int(l(1), 10), #Int(l(2), 12)]), #Int(l(3), 13)]->ne,
        [#Tuple(l(4), [#Int(l(5), 10), #Int(l(6), 22)]), #Int(l(7), 23)]->ne,
        [#Binding(l(8), "_"), #Int(l(9), 33)]->ne,
        [#Binding(l(10), "_"), #Binding(l(11), "_")]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let result =
      [case1]->makeCases->Matching.make(~loc=Loc(0), ~name="")->Belt.Result.map(x => x.tree)
    expect.value(result).toEqual(
      Ok(
        Nest({
          idx: 0,
          key: "",
          ids: SI.empty,
          kind: Tuple,
          child: Switch({
            idx: 0,
            key: "",
            ids: SI.empty,
            cases: {
              val: TPat_Int(10),
              ifMatch: Switch({
                idx: 1,
                key: "",
                ids: SI.empty,
                cases: {
                  val: TPat_Int(12),
                  ifMatch: End(
                    Switch({
                      idx: 1,
                      key: "",
                      ids: SI.empty,
                      cases: {
                        val: TPat_Int(13),
                        ifMatch: End({names: MS.empty, exit: 0}),
                        nextCase: Some({
                          val: TPat_Int(33),
                          ifMatch: End({names: MS.empty, exit: 0}),
                          nextCase: None,
                        }),
                      },
                      wildcard: Some(End({names: MS.empty, exit: 0})),
                    }),
                  ),
                  nextCase: Some({
                    val: TPat_Int(22),
                    ifMatch: End(
                      Switch({
                        idx: 1,
                        key: "",
                        ids: SI.empty,
                        cases: {
                          val: TPat_Int(23),
                          ifMatch: End({names: MS.empty, exit: 0}),
                          nextCase: Some({
                            val: TPat_Int(33),
                            ifMatch: End({names: MS.empty, exit: 0}),
                            nextCase: None,
                          }),
                        },
                        wildcard: Some(End({names: MS.empty, exit: 0})),
                      }),
                    ),
                    nextCase: None,
                  }),
                },
                wildcard: None,
              }),
              nextCase: None,
            },
            wildcard: None,
          }),
          wildcard: Some(
            Switch({
              idx: 1,
              key: "",
              ids: SI.empty,
              cases: {
                val: TPat_Int(33),
                ifMatch: End({names: MS.empty, exit: 0}),
                nextCase: None,
              },
              wildcard: Some(End({names: MS.empty, exit: 0})),
            }),
          ),
        }),
      ),
    )
  })

  test("Nests merge into wildcards correctly 1.", ({expect, _}) => {
    let l = l => T.Loc(l)
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
        [#Tuple(l(10), [#Binding(l(11), "_"), #Binding(l(12), "y")]), #Binding(l(13), "z")]->ne,
      ]->ne,
      nodes: nodes3,
    }
    let result =
      [case1, case2, case3]
      ->makeCases
      ->Matching.make(~loc=Loc(0), ~name="")
      ->Belt.Result.map(x => x.tree)
    expect.value(result).toEqual(
      Ok(
        Nest({
          idx: 0,
          key: "",
          ids: SI.fromArray([0]),
          kind: Tuple,
          child: Switch({
            idx: 0,
            key: "",
            ids: SI.empty,
            cases: {
              val: TPat_String("a"),
              ifMatch: Switch({
                idx: 1,
                key: "",
                ids: SI.fromArray([12]),
                cases: {
                  val: TPat_String("b"),
                  ifMatch: End(
                    Switch({
                      idx: 1,
                      key: "",
                      ids: SI.fromArray([13]),
                      cases: {
                        val: TPat_Int(1),
                        ifMatch: End({names: MS.fromArray([("x", 0)]), exit: 0}),
                        nextCase: Some({
                          val: TPat_Int(10),
                          ifMatch: End({names: MS.empty, exit: 1}),
                          nextCase: None,
                        }),
                      },
                      wildcard: Some(
                        End({
                          names: MS.empty->MS.set("y", 12)->MS.set("z", 13),
                          exit: 2,
                        }),
                      ),
                    }),
                  ),
                  nextCase: None,
                },
                wildcard: Some(
                  End(
                    Switch({
                      idx: 1,
                      key: "",
                      ids: SI.fromArray([13]),
                      cases: {
                        val: TPat_Int(1),
                        ifMatch: End({names: MS.fromArray([("x", 0)]), exit: 0}),
                        nextCase: None,
                      },
                      wildcard: Some(
                        End({
                          names: MS.empty->MS.set("y", 12)->MS.set("z", 13),
                          exit: 2,
                        }),
                      ),
                    }),
                  ),
                ),
              }),
              nextCase: None,
            },
            wildcard: Some(
              Wildcard({
                idx: 1,
                key: "",
                ids: SI.fromArray([12]),
                child: End(
                  Switch({
                    idx: 1,
                    key: "",
                    ids: SI.fromArray([13]),
                    cases: {
                      val: TPat_Int(1),
                      ifMatch: End({names: MS.fromArray([("x", 0)]), exit: 0}),
                      nextCase: None,
                    },
                    wildcard: Some(
                      End({
                        names: MS.empty->MS.set("y", 12)->MS.set("z", 13),
                        exit: 2,
                      }),
                    ),
                  }),
                ),
              }),
            ),
          }),
          wildcard: Some(
            Switch({
              idx: 1,
              key: "",
              ids: SI.empty,
              cases: {
                val: TPat_Int(1),
                ifMatch: End({names: MS.fromArray([("x", 0)]), exit: 0}),
                nextCase: None,
              },
              wildcard: None,
            }),
          ),
        }),
      ),
    )
  })

  test("Nests merge correctly.", ({expect, _}) => {
    let l = l => T.Loc(l)
    let n1 = [T.Ast.Text("", NoTrim)]
    let c1 = {
      T.Ast.patterns: [[#Binding(l(0), "_"), #Binding(l(1), "_"), #Int((l(2), 12))]->ne]->ne,
      nodes: n1,
    }
    let c2 = {
      T.Ast.patterns: [
        [#Binding(l(3), "_"), #Tuple(l(4), [#Int(l(5), 20), #Int(l(6), 21)]), #Int(l(7), 22)]->ne,
      ]->ne,
      nodes: n1,
    }
    let c3 = {
      T.Ast.patterns: [
        [
          #Binding(l(8), "_"),
          #Tuple(l(9), [#Int(l(10), 20), #Int(l(11), 21)]),
          #Int(l(12), 32),
        ]->ne,
      ]->ne,
      nodes: n1,
    }
    let c4 = {
      T.Ast.patterns: [
        [
          #Binding(l(13), "_"),
          #Tuple(l(14), [#Binding(l(15), "_"), #Binding(l(16), "_")]),
          #Binding(l(17), "_"),
        ]->ne,
      ]->ne,
      nodes: n1,
    }
    let result =
      [c1, c2, c3, c4]
      ->makeCases
      ->Matching.make(~loc=Loc(0), ~name="")
      ->Belt.Result.map(x => x.tree)
    expect.value(result).toEqual(
      Ok(
        Wildcard({
          idx: 0,
          key: "",
          ids: SI.empty,
          child: Nest({
            idx: 1,
            key: "",
            ids: SI.empty,
            kind: Tuple,
            child: Switch({
              idx: 0,
              key: "",
              ids: SI.empty,
              cases: {
                val: TPat_Int(20),
                ifMatch: Switch({
                  idx: 1,
                  key: "",
                  ids: SI.empty,
                  cases: {
                    val: TPat_Int(21),
                    ifMatch: End(
                      Switch({
                        idx: 2,
                        key: "",
                        ids: SI.empty,
                        cases: {
                          val: TPat_Int(12),
                          ifMatch: End({names: MS.empty, exit: 0}),
                          nextCase: Some({
                            val: TPat_Int(22),
                            ifMatch: End({names: MS.empty, exit: 1}),
                            nextCase: Some({
                              val: TPat_Int(32),
                              ifMatch: End({names: MS.empty, exit: 2}),
                              nextCase: None,
                            }),
                          }),
                        },
                        wildcard: Some(End({names: MS.empty, exit: 3})),
                      }),
                    ),
                    nextCase: None,
                  },
                  wildcard: Some(
                    End(
                      Switch({
                        idx: 2,
                        key: "",
                        ids: SI.empty,
                        cases: {
                          val: TPat_Int(12),
                          ifMatch: End({names: MS.empty, exit: 0}),
                          nextCase: None,
                        },
                        wildcard: Some(End({names: MS.empty, exit: 3})),
                      }),
                    ),
                  ),
                }),
                nextCase: None,
              },
              wildcard: Some(
                Wildcard({
                  idx: 1,
                  key: "",
                  ids: SI.empty,
                  child: End(
                    Switch({
                      idx: 2,
                      key: "",
                      ids: SI.empty,
                      cases: {
                        val: TPat_Int(12),
                        ifMatch: End({names: MS.empty, exit: 0}),
                        nextCase: None,
                      },
                      wildcard: Some(End({names: MS.empty, exit: 3})),
                    }),
                  ),
                }),
              ),
            }),
            wildcard: Some(
              Switch({
                idx: 2,
                key: "",
                ids: SI.empty,
                cases: {
                  val: TPat_Int(12),
                  ifMatch: End({names: MS.empty, exit: 0}),
                  nextCase: None,
                },
                wildcard: None,
              }),
            ),
          }),
        }),
      ),
    )
  })

  test("Wildcards merge after nests correctly.", ({expect, _}) => {
    let l = l => T.Loc(l)
    let n1 = [T.Ast.Text("", NoTrim)]
    let n2 = [T.Ast.Text("", NoTrim)]
    let n3 = [T.Ast.Text("", NoTrim)]
    let c1 = {
      T.Ast.patterns: [[#Binding(l(3), "x"), #Int(l(4), 41)]->ne]->ne,
      nodes: n1,
    }
    let c2 = {
      T.Ast.patterns: [
        [
          #Tuple(l(0), [#Tuple(l(0), [#Int(l(1), 10), #Int(l(1), 20)]), #Int(l(1), 30)]),
          #Int(l(2), 40),
        ]->ne,
      ]->ne,
      nodes: n2,
    }
    let c3 = {
      T.Ast.patterns: [[#Binding(l(5), "y"), #Binding(l(6), "z")]->ne]->ne,
      nodes: n3,
    }
    let result =
      [c1, c2, c3]->makeCases->Matching.make(~loc=Loc(0), ~name="")->Belt.Result.map(x => x.tree)
    expect.value(result).toEqual(
      Ok(
        Nest({
          idx: 0,
          key: "",
          ids: SI.empty->SI.add(3)->SI.add(5),
          kind: Tuple,
          child: Nest({
            idx: 0,
            key: "",
            ids: SI.empty,
            kind: Tuple,
            child: Switch({
              idx: 0,
              key: "",
              ids: SI.empty,
              cases: {
                val: TPat_Int(10),
                ifMatch: Switch({
                  idx: 1,
                  key: "",
                  ids: SI.empty,
                  cases: {
                    val: TPat_Int(20),
                    ifMatch: End(
                      Switch({
                        idx: 1,
                        key: "",
                        ids: SI.empty,
                        cases: {
                          val: TPat_Int(30),
                          ifMatch: End(
                            Switch({
                              idx: 1,
                              key: "",
                              ids: SI.fromArray([6]),
                              cases: {
                                val: TPat_Int(40),
                                ifMatch: End({names: MS.empty, exit: 1}),
                                nextCase: Some({
                                  val: TPat_Int(41),
                                  ifMatch: End({names: MS.fromArray([("x", 3)]), exit: 0}),
                                  nextCase: None,
                                }),
                              },
                              wildcard: Some(
                                End({
                                  names: MS.empty->MS.set("y", 5)->MS.set("z", 6),
                                  exit: 2,
                                }),
                              ),
                            }),
                          ),
                          nextCase: None,
                        },
                        wildcard: None,
                      }),
                    ),
                    nextCase: None,
                  },
                  wildcard: None,
                }),
                nextCase: None,
              },
              wildcard: None,
            }),
            wildcard: None,
          }),
          wildcard: Some(
            Switch({
              idx: 1,
              key: "",
              ids: SI.fromArray([6]),
              cases: {
                val: TPat_Int(41),
                ifMatch: End({names: MS.fromArray([("x", 3)]), exit: 0}),
                nextCase: None,
              },
              wildcard: Some(
                End({
                  names: MS.empty->MS.set("y", 5)->MS.set("z", 6),
                  exit: 2,
                }),
              ),
            }),
          ),
        }),
      ),
    )
  })

  test("Different-sized lists merge correctly.", ({expect, _}) => {
    let l = l => T.Loc(l)
    let n1 = [T.Ast.Text("", NoTrim)]
    let n2 = [T.Ast.Text("", NoTrim)]
    let n3 = [T.Ast.Text("", NoTrim)]
    let c1 = {
      T.Ast.patterns: [[#Array(l(0), [])]->ne]->ne,
      nodes: n1,
    }
    let c2 = {
      T.Ast.patterns: [[#Array(l(0), [#Binding(l(2), "x")])]->ne]->ne,
      nodes: n2,
    }
    let c3 = {
      T.Ast.patterns: [
        [#ArrayWithTailBinding(l(0), [#Binding(l(2), "x")], #Binding(l(5), "y"))]->ne,
      ]->ne,
      nodes: n3,
    }
    let result =
      [c1, c2, c3]->makeCases->Matching.make(~loc=Loc(0), ~name="")->Belt.Result.map(x => x.tree)
    expect.value(result).toEqual(
      Ok(
        Construct({
          idx: 0,
          key: "",
          ids: SI.empty,
          kind: TPat_List,
          nil: Some(End({names: MS.empty, exit: 0})),
          cons: Some(
            Nest({
              idx: 0,
              key: "",
              ids: SI.empty,
              kind: Tuple,
              child: Wildcard({
                idx: 0,
                key: "",
                ids: SI.fromArray([2]),
                child: Construct({
                  idx: 1,
                  key: "",
                  ids: SI.fromArray([5]),
                  kind: TPat_List,
                  nil: Some(End(End({names: MS.fromArray([("x", 2)]), exit: 1}))),
                  cons: Some(
                    Wildcard({
                      idx: 1,
                      key: "",
                      ids: SI.fromArray([5]),
                      child: End(
                        End({
                          names: MS.empty->MS.set("x", 2)->MS.set("y", 5),
                          exit: 2,
                        }),
                      ),
                    }),
                  ),
                }),
              }),
              wildcard: None,
            }),
          ),
        }),
      ),
    )
  })

  test("dec tree list", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let nodes2 = [T.Ast.Text("", NoTrim)]
    let nodes3 = [T.Ast.Text("", NoTrim)]
    let nodes4 = [T.Ast.Text("", NoTrim)]
    let nodes5 = [T.Ast.Text("", NoTrim)]
    let case1 = {
      T.Ast.patterns: [[#Array(l(0), [#Int(l(2), 10), #Int(l(3), 11)]), #Int(l(4), 12)]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      T.Ast.patterns: [
        [
          #ArrayWithTailBinding(l(5), [#Int(l(6), 10), #Int(l(7), 11)], #Binding(l(8), "x")),
          #Int(l(9), 22),
        ]->ne,
      ]->ne,
      nodes: nodes2,
    }
    let case3 = {
      T.Ast.patterns: [[#Array(l(10), [#Int(l(11), 30)]), #Int(l(12), 32)]->ne]->ne,
      nodes: nodes3,
    }
    let case4 = {
      T.Ast.patterns: [[#Binding(l(13), "y"), #Int(l(14), 42)]->ne]->ne,
      nodes: nodes4,
    }
    let case5 = {
      T.Ast.patterns: [[#Binding(l(15), "_"), #Binding(l(16), "_")]->ne]->ne,
      nodes: nodes5,
    }
    let result =
      [case1, case2, case3, case4, case5]
      ->makeCases
      ->Matching.make(~loc=Loc(0), ~name="")
      ->Belt.Result.map(x => x.tree)
    expect.value(result).toEqual(
      Ok(
        Construct({
          idx: 0,
          key: "",
          ids: SI.fromArray([13]),
          kind: TPat_List,
          cons: Some(
            Nest({
              idx: 0,
              key: "",
              ids: SI.fromArray([13]),
              kind: Tuple,
              child: Switch({
                idx: 0,
                key: "",
                ids: SI.empty,
                cases: {
                  val: TPat_Int(10),
                  ifMatch: Construct({
                    idx: 1,
                    key: "",
                    ids: SI.empty,
                    kind: TPat_List,
                    cons: Some(
                      Nest({
                        idx: 1,
                        key: "",
                        ids: SI.empty,
                        kind: Tuple,
                        child: Switch({
                          idx: 0,
                          key: "",
                          ids: SI.empty,
                          cases: {
                            val: TPat_Int(11),
                            ifMatch: Construct({
                              idx: 1,
                              key: "",
                              ids: SI.fromArray([8]),
                              kind: TPat_List,
                              nil: Some(
                                End(
                                  End(
                                    Switch({
                                      idx: 1,
                                      key: "",
                                      ids: SI.empty,
                                      cases: {
                                        val: TPat_Int(12),
                                        ifMatch: End({names: MS.empty, exit: 0}),
                                        nextCase: Some({
                                          val: TPat_Int(22),
                                          ifMatch: End({
                                            names: MS.fromArray([("x", 8)]),
                                            exit: 1,
                                          }),
                                          nextCase: Some({
                                            val: TPat_Int(42),
                                            ifMatch: End({
                                              names: MS.fromArray([("y", 13)]),
                                              exit: 3,
                                            }),
                                            nextCase: None,
                                          }),
                                        }),
                                      },
                                      wildcard: Some(End({names: MS.empty, exit: 4})),
                                    }),
                                  ),
                                ),
                              ),
                              cons: Some(
                                Wildcard({
                                  idx: 1,
                                  key: "",
                                  ids: SI.fromArray([8]),
                                  child: End(
                                    End(
                                      Switch({
                                        idx: 1,
                                        key: "",
                                        ids: SI.empty,
                                        cases: {
                                          val: TPat_Int(22),
                                          ifMatch: End({
                                            names: MS.fromArray([("x", 8)]),
                                            exit: 1,
                                          }),
                                          nextCase: Some({
                                            val: TPat_Int(42),
                                            ifMatch: End({
                                              names: MS.fromArray([("y", 13)]),
                                              exit: 3,
                                            }),
                                            nextCase: None,
                                          }),
                                        },
                                        wildcard: Some(End({names: MS.empty, exit: 4})),
                                      }),
                                    ),
                                  ),
                                }),
                              ),
                            }),
                            nextCase: None,
                          },
                          wildcard: None,
                        }),
                        wildcard: None,
                      }),
                    ),
                    nil: None,
                  }),
                  nextCase: Some({
                    val: TPat_Int(30),
                    ifMatch: Construct({
                      idx: 1,
                      key: "",
                      ids: SI.empty,
                      kind: TPat_List,
                      nil: Some(
                        End(
                          Switch({
                            idx: 1,
                            key: "",
                            ids: SI.empty,
                            cases: {
                              val: TPat_Int(32),
                              ifMatch: End({names: MS.empty, exit: 2}),
                              nextCase: Some({
                                val: TPat_Int(42),
                                ifMatch: End({names: MS.fromArray([("y", 13)]), exit: 3}),
                                nextCase: None,
                              }),
                            },
                            wildcard: Some(End({names: MS.empty, exit: 4})),
                          }),
                        ),
                      ),
                      cons: None,
                    }),
                    nextCase: None,
                  }),
                },
                wildcard: None,
              }),
              wildcard: Some(
                Switch({
                  idx: 1,
                  key: "",
                  ids: SI.empty,
                  cases: {
                    val: TPat_Int(42),
                    ifMatch: End({names: MS.fromArray([("y", 13)]), exit: 3}),
                    nextCase: None,
                  },
                  wildcard: Some(End({names: MS.empty, exit: 4})),
                }),
              ),
            }),
          ),
          nil: Some(
            Switch({
              idx: 1,
              key: "",
              ids: SI.empty,
              cases: {
                val: TPat_Int(42),
                ifMatch: End({names: MS.fromArray([("y", 13)]), exit: 3}),
                nextCase: None,
              },
              wildcard: Some(End({names: MS.empty, exit: 4})),
            }),
          ),
        }),
      ),
    )
  })

  test("Records sort fields correctly", ({expect, _}) => {
    let l = T.Loc(0)
    let nodes1 = [T.Ast.Text("a", NoTrim)]
    let case1 = {
      T.Ast.patterns: [
        [#Object(l, [("a", #Int(l, 10)), ("b", #Int(l, 11))]), #Int(l, 12)]->ne,
        [#Object(l, [("b", #Int(l, 21)), ("a", #Int(l, 20))]), #Int(l, 22)]->ne,
        [#Binding(l, "_"), #Binding(l, "_")]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let result =
      [case1]->makeCases->Matching.make(~loc=Loc(0), ~name="")->Belt.Result.map(x => x.tree)
    expect.value(result).toEqual(
      Ok(
        Nest({
          idx: 0,
          key: "",
          ids: SI.empty,
          kind: Record,
          child: Switch({
            idx: 0,
            key: "a",
            ids: SI.empty,
            cases: {
              val: TPat_Int(10),
              ifMatch: Switch({
                idx: 1,
                key: "b",
                ids: SI.empty,
                cases: {
                  val: TPat_Int(11),
                  ifMatch: End(
                    Switch({
                      idx: 1,
                      key: "",
                      ids: SI.empty,
                      cases: {
                        val: TPat_Int(12),
                        ifMatch: End({names: MS.empty, exit: 0}),
                        nextCase: None,
                      },
                      wildcard: Some(End({names: MS.empty, exit: 0})),
                    }),
                  ),
                  nextCase: None,
                },
                wildcard: None,
              }),
              nextCase: Some({
                val: TPat_Int(20),
                ifMatch: Switch({
                  idx: 1,
                  key: "b",
                  ids: SI.empty,
                  cases: {
                    val: TPat_Int(21),
                    ifMatch: End(
                      Switch({
                        idx: 1,
                        key: "",
                        ids: SI.empty,
                        cases: {
                          val: TPat_Int(22),
                          ifMatch: End({names: MS.empty, exit: 0}),
                          nextCase: None,
                        },
                        wildcard: Some(End({names: MS.empty, exit: 0})),
                      }),
                    ),
                    nextCase: None,
                  },
                  wildcard: None,
                }),
                nextCase: None,
              }),
            },
            wildcard: None,
          }),
          wildcard: Some(
            Wildcard({
              idx: 1,
              key: "",
              ids: SI.empty,
              child: End({names: MS.empty, exit: 0}),
            }),
          ),
        }),
      ),
    )
  })

  test("Records: missing fields are automatically wildcards", ({expect, _}) => {
    let l = T.Loc(0)
    let nodes1 = [T.Ast.Text("a", NoTrim)]
    let case1 = {
      T.Ast.patterns: [
        [#Object(l, [("a", #Int(l, 10)), ("b", #Int(l, 11))]), #Int(l, 12)]->ne,
        [#Object(l, [("b", #Int(l, 21))]), #Int(l, 22)]->ne,
        [#Binding(l, "_"), #Binding(l, "_")]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let result =
      [case1]->makeCases->Matching.make(~loc=Loc(0), ~name="")->Belt.Result.map(x => x.tree)
    expect.value(result).toEqual(
      Ok(
        Nest({
          idx: 0,
          key: "",
          ids: SI.empty,
          kind: Record,
          child: Switch({
            idx: 0,
            key: "a",
            ids: SI.empty,
            cases: {
              val: TPat_Int(10),
              ifMatch: Switch({
                idx: 1,
                key: "b",
                ids: SI.empty,
                cases: {
                  val: TPat_Int(11),
                  ifMatch: End(
                    Switch({
                      idx: 1,
                      key: "",
                      ids: SI.empty,
                      cases: {
                        val: TPat_Int(12),
                        ifMatch: End({names: MS.empty, exit: 0}),
                        nextCase: None,
                      },
                      wildcard: Some(End({names: MS.empty, exit: 0})),
                    }),
                  ),
                  nextCase: Some({
                    val: TPat_Int(21),
                    ifMatch: End(
                      Switch({
                        idx: 1,
                        key: "",
                        ids: SI.empty,
                        cases: {
                          val: TPat_Int(22),
                          ifMatch: End({names: MS.empty, exit: 0}),
                          nextCase: None,
                        },
                        wildcard: Some(End({names: MS.empty, exit: 0})),
                      }),
                    ),
                    nextCase: None,
                  }),
                },
                wildcard: None,
              }),
              nextCase: None,
            },
            wildcard: Some(
              Switch({
                idx: 1,
                key: "b",
                ids: SI.empty,
                cases: {
                  val: TPat_Int(21),
                  ifMatch: End(
                    Switch({
                      idx: 1,
                      key: "",
                      ids: SI.empty,
                      cases: {
                        val: TPat_Int(22),
                        ifMatch: End({names: MS.empty, exit: 0}),
                        nextCase: None,
                      },
                      wildcard: Some(End({names: MS.empty, exit: 0})),
                    }),
                  ),
                  nextCase: None,
                },
                wildcard: None,
              }),
            ),
          }),
          wildcard: Some(
            Wildcard({
              idx: 1,
              key: "",
              ids: SI.empty,
              child: End({names: MS.empty, exit: 0}),
            }),
          ),
        }),
      ),
    )
  })

  test("Records: new fields expand existing rows", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1: T.Ast.nodes<_> = [Text("x", NoTrim)]
    let nodes2: T.Ast.nodes<_> = [Text("y", NoTrim)]
    let nodes3: T.Ast.nodes<_> = [Text("z", NoTrim)]
    let nodes4: T.Ast.nodes<_> = [Text("zz", NoTrim)]
    let case1 = {
      T.Ast.patterns: [[#Object(l(0), [("b", #Int(l(1), 10))])]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      T.Ast.patterns: [[#Object(l(2), [("a", #Int(l(3), 20))])]->ne]->ne,
      nodes: nodes2,
    }
    let case3 = {
      T.Ast.patterns: [[#Object(l(4), [("c", #Int(l(5), 30))])]->ne]->ne,
      nodes: nodes3,
    }
    let case4 = {
      T.Ast.patterns: [[#Binding(l(6), "x")]->ne]->ne,
      nodes: nodes4,
    }
    let result =
      [case1, case2, case3, case4]
      ->makeCases
      ->Matching.make(~loc=Loc(0), ~name="")
      ->Belt.Result.map(x => x.tree)
    expect.value(result).toEqual(
      Ok(
        Nest({
          idx: 0,
          key: "",
          ids: SI.fromArray([6]),
          kind: Record,
          child: Switch({
            idx: 0,
            key: "a",
            ids: SI.empty,
            cases: {
              val: TPat_Int(20),
              ifMatch: Switch({
                idx: 1,
                key: "b",
                ids: SI.empty,
                cases: {
                  val: TPat_Int(10),
                  ifMatch: Wildcard({
                    idx: 2,
                    key: "c",
                    ids: SI.empty,
                    child: End(End({names: MS.empty, exit: 0})),
                  }),
                  nextCase: None,
                },
                wildcard: Some(
                  Wildcard({
                    idx: 2,
                    key: "c",
                    ids: SI.empty,
                    child: End(End({names: MS.empty, exit: 1})),
                  }),
                ),
              }),
              nextCase: None,
            },
            wildcard: Some(
              Switch({
                idx: 1,
                key: "b",
                ids: SI.empty,
                cases: {
                  val: TPat_Int(10),
                  ifMatch: Wildcard({
                    idx: 2,
                    key: "c",
                    ids: SI.empty,
                    child: End(End({names: MS.empty, exit: 0})),
                  }),
                  nextCase: None,
                },
                wildcard: Some(
                  Switch({
                    idx: 2,
                    key: "c",
                    ids: SI.empty,
                    cases: {
                      val: TPat_Int(30),
                      ifMatch: End(End({names: MS.empty, exit: 2})),
                      nextCase: None,
                    },
                    wildcard: None,
                  }),
                ),
              }),
            ),
          }),
          wildcard: Some(End({names: MS.fromArray([("x", 6)]), exit: 3})),
        }),
      ),
    )
  })
})
