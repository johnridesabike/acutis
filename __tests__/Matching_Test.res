/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
open TestFramework
// module Array = Belt.Array
module NE = NonEmpty
module SI = Belt.Set.Int
module MS = Belt.Map.String
module TC = Typechecker
module P = Untyped.Pattern
let ne = NE.fromArrayExn

let g = Utils.Dagmap.make(Belt.HashMap.String.make(~hintSize=0), ~f=(. _, _) => assert false)

let makeCases = c => {
  let (_, cases) = TC.makeCases(ne(c), TC.Context.make(), ~loc=Loc(0), ~name="", g, Component)
  cases
}

describe("Basic tree", ({test, _}) => {
  test("blah", ({expect, _}) => {
    let nodes1 = [Untyped.UText("", NoTrim)]
    let nodes2 = [Untyped.UText("", NoTrim)]
    let case1 = {
      Untyped.patterns: [[P.USome(Loc(0), UBinding(Loc(1), "a"))]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      Untyped.patterns: [[P.UNull(Loc(3))]->ne]->ne,
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
          kind: TNullable,
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
    let nodes1 = [Untyped.UText("", NoTrim)]
    let nodes2 = [Untyped.UText("", NoTrim)]
    let nodes3 = [Untyped.UText("", NoTrim)]
    let case1 = {
      Untyped.patterns: [
        [P.UInt(Loc(0), 0)]->ne,
        [P.UInt(Loc(3), 10)]->ne,
        [P.UInt(Loc(3), 20)]->ne,
        [P.UInt(Loc(3), 30)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let case2 = {
      Untyped.patterns: [[P.UInt(Loc(0), 15)]->ne]->ne,
      nodes: nodes2,
    }
    let case3 = {
      Untyped.patterns: [[P.UBinding(Loc(0), "_")]->ne]->ne,
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
            val: TInt(0),
            ifMatch: End({names: MS.empty, exit: 0}),
            nextCase: Some({
              val: TInt(10),
              ifMatch: End({names: MS.empty, exit: 0}),
              nextCase: Some({
                val: TInt(15),
                ifMatch: End({names: MS.empty, exit: 1}),
                nextCase: Some({
                  val: TInt(20),
                  ifMatch: End({names: MS.empty, exit: 0}),
                  nextCase: Some({
                    val: TInt(30),
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
    let nodes1 = [Untyped.UText("", NoTrim)]
    let case1 = {
      Untyped.patterns: [
        [P.UInt(Loc(0), 1), UInt(Loc(1), 2), UInt(Loc(2), 3)]->ne,
        [P.UInt(Loc(3), 1), UInt(Loc(4), 4), UInt(Loc(5), 5)]->ne,
        [P.UInt(Loc(6), 10), UInt(Loc(7), 20), UInt(Loc(8), 30)]->ne,
        [P.UInt(Loc(9), 10), UInt(Loc(10), 20), UInt(Loc(11), 40)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let nodes2 = [Untyped.UText("", NoTrim)]
    let case2 = {
      Untyped.patterns: [
        [P.UInt(Loc(12), 100), UInt(Loc(13), 102), UInt(Loc(14), 103)]->ne,
        [P.UInt(Loc(15), 100), UInt(Loc(16), 104), UInt(Loc(17), 105)]->ne,
      ]->ne,
      nodes: nodes2,
    }
    let nodes3 = [Untyped.UText("", NoTrim)]
    let case3 = {
      Untyped.patterns: [
        [P.UInt(Loc(18), 10), UInt(Loc(19), 20), UInt(Loc(20), 50)]->ne,
        [P.UInt(Loc(21), 1), UInt(Loc(22), 2), UInt(Loc(23), 100)]->ne,
        [P.UInt(Loc(24), 1), UInt(Loc(25), 2), UInt(Loc(26), 101)]->ne,
        [P.UInt(Loc(27), 100), UInt(Loc(28), 102), UInt(Loc(29), 106)]->ne,
      ]->ne,
      nodes: nodes3,
    }
    let nodes4 = [Untyped.UText("", NoTrim)]
    let case4 = {
      Untyped.patterns: [
        [P.UBinding(Loc(30), "_"), UBinding(Loc(31), "_"), UBinding(Loc(32), "_")]->ne,
      ]->ne,
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
            val: TInt(1),
            ifMatch: Switch({
              idx: 1,
              key: "",
              ids: SI.empty,
              cases: {
                val: TInt(2),
                ifMatch: Switch({
                  idx: 2,
                  key: "",
                  ids: SI.empty,
                  cases: {
                    val: TInt(3),
                    ifMatch: End({names: MS.empty, exit: 0}),
                    nextCase: Some({
                      val: TInt(100),
                      ifMatch: End({names: MS.empty, exit: 2}),
                      nextCase: Some({
                        val: TInt(101),
                        ifMatch: End({names: MS.empty, exit: 2}),
                        nextCase: None,
                      }),
                    }),
                  },
                  wildcard: Some(End({names: MS.empty, exit: 3})),
                }),
                nextCase: Some({
                  val: TInt(4),
                  ifMatch: Switch({
                    idx: 2,
                    key: "",
                    ids: SI.empty,
                    cases: {
                      val: TInt(5),
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
              val: TInt(10),
              ifMatch: Switch({
                idx: 1,
                key: "",
                ids: SI.empty,
                cases: {
                  val: TInt(20),
                  ifMatch: Switch({
                    idx: 2,
                    key: "",
                    ids: SI.empty,
                    cases: {
                      val: TInt(30),
                      ifMatch: End({names: MS.empty, exit: 0}),
                      nextCase: Some({
                        val: TInt(40),
                        ifMatch: End({names: MS.empty, exit: 0}),
                        nextCase: Some({
                          val: TInt(50),
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
                val: TInt(100),
                ifMatch: Switch({
                  idx: 1,
                  key: "",
                  ids: SI.empty,
                  cases: {
                    val: TInt(102),
                    ifMatch: Switch({
                      idx: 2,
                      key: "",
                      ids: SI.empty,
                      cases: {
                        val: TInt(103),
                        ifMatch: End({names: MS.empty, exit: 1}),
                        nextCase: Some({
                          val: TInt(106),
                          ifMatch: End({names: MS.empty, exit: 2}),
                          nextCase: None,
                        }),
                      },
                      wildcard: Some(End({names: MS.empty, exit: 3})),
                    }),
                    nextCase: Some({
                      val: TInt(104),
                      ifMatch: Switch({
                        idx: 2,
                        key: "",
                        ids: SI.empty,
                        cases: {
                          val: TInt(105),
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
    let nodes1 = [Untyped.UText("", NoTrim)]
    let case1 = {
      Untyped.patterns: [
        [P.UInt(Loc(0), 10), UInt(Loc(1), 11), UInt(Loc(2), 12)]->ne,
        [P.UBinding(Loc(3), "x"), UInt(Loc(4), 21), UInt(Loc(5), 22)]->ne,
        [P.UInt(Loc(9), 30), UInt(Loc(10), 31), UInt(Loc(11), 32)]->ne,
        [P.UInt(Loc(12), 30), UBinding(Loc(13), "y"), UInt(Loc(14), 42)]->ne,
        [P.UBinding(Loc(18), "a"), UBinding(Loc(19), "b"), UBinding(Loc(20), "c")]->ne,
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
            val: TInt(10),
            ifMatch: Switch({
              idx: 1,
              key: "",
              ids: SI.fromArray([19]),
              cases: {
                val: TInt(11),
                ifMatch: Switch({
                  idx: 2,
                  key: "",
                  ids: SI.fromArray([20]),
                  cases: {
                    val: TInt(12),
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
                  val: TInt(21),
                  ifMatch: Switch({
                    idx: 2,
                    key: "",
                    ids: SI.fromArray([20]),
                    cases: {
                      val: TInt(22),
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
              val: TInt(30),
              ifMatch: Switch({
                idx: 1,
                key: "",
                ids: SI.empty->SI.add(13)->SI.add(19),
                cases: {
                  val: TInt(21),
                  ifMatch: Switch({
                    idx: 2,
                    key: "",
                    ids: SI.fromArray([20]),
                    cases: {
                      val: TInt(22),
                      ifMatch: End({names: MS.fromArray([("x", 3)]), exit: 0}),
                      nextCase: Some({
                        val: TInt(42),
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
                    val: TInt(31),
                    ifMatch: Switch({
                      idx: 2,
                      key: "",
                      ids: SI.fromArray([20]),
                      cases: {
                        val: TInt(32),
                        ifMatch: End({names: MS.empty, exit: 0}),
                        nextCase: Some({
                          val: TInt(42),
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
                      val: TInt(42),
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
                val: TInt(21),
                ifMatch: Switch({
                  idx: 2,
                  key: "",
                  ids: SI.fromArray([20]),
                  cases: {
                    val: TInt(22),
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
    let nodes1 = [Untyped.UText("", NoTrim)]
    let case1 = {
      Untyped.patterns: [
        [P.UTuple(Loc(0), [UInt(Loc(1), 10), UInt(Loc(2), 12)]), UInt(Loc(3), 13)]->ne,
        [P.UTuple(Loc(4), [UInt(Loc(5), 10), UInt(Loc(6), 22)]), UInt(Loc(7), 23)]->ne,
        [P.UBinding(Loc(8), "_"), UInt(Loc(9), 33)]->ne,
        [P.UBinding(Loc(10), "_"), UBinding(Loc(11), "_")]->ne,
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
              val: TInt(10),
              ifMatch: Switch({
                idx: 1,
                key: "",
                ids: SI.empty,
                cases: {
                  val: TInt(12),
                  ifMatch: End(
                    Switch({
                      idx: 1,
                      key: "",
                      ids: SI.empty,
                      cases: {
                        val: TInt(13),
                        ifMatch: End({names: MS.empty, exit: 0}),
                        nextCase: Some({
                          val: TInt(33),
                          ifMatch: End({names: MS.empty, exit: 0}),
                          nextCase: None,
                        }),
                      },
                      wildcard: Some(End({names: MS.empty, exit: 0})),
                    }),
                  ),
                  nextCase: Some({
                    val: TInt(22),
                    ifMatch: End(
                      Switch({
                        idx: 1,
                        key: "",
                        ids: SI.empty,
                        cases: {
                          val: TInt(23),
                          ifMatch: End({names: MS.empty, exit: 0}),
                          nextCase: Some({
                            val: TInt(33),
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
                val: TInt(33),
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
    let nodes1 = [Untyped.UText("", NoTrim)]
    let nodes2 = [Untyped.UText("", NoTrim)]
    let nodes3 = [Untyped.UText("", NoTrim)]
    let case1 = {
      Untyped.patterns: [[P.UBinding(Loc(0), "x"), UInt(Loc(1), 1)]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      Untyped.patterns: [
        [P.UTuple(Loc(2), [UString(Loc(3), "a"), UString(Loc(4), "b")]), UInt(Loc(5), 10)]->ne,
      ]->ne,
      nodes: nodes2,
    }
    let case3 = {
      Untyped.patterns: [
        [
          P.UTuple(Loc(10), [UBinding(Loc(11), "_"), UBinding(Loc(12), "y")]),
          UBinding(Loc(13), "z"),
        ]->ne,
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
              val: TString("a"),
              ifMatch: Switch({
                idx: 1,
                key: "",
                ids: SI.fromArray([12]),
                cases: {
                  val: TString("b"),
                  ifMatch: End(
                    Switch({
                      idx: 1,
                      key: "",
                      ids: SI.fromArray([13]),
                      cases: {
                        val: TInt(1),
                        ifMatch: End({names: MS.fromArray([("x", 0)]), exit: 0}),
                        nextCase: Some({
                          val: TInt(10),
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
                        val: TInt(1),
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
                      val: TInt(1),
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
                val: TInt(1),
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
    let n1 = [Untyped.UText("", NoTrim)]
    let c1 = {
      Untyped.patterns: [
        [P.UBinding(Loc(0), "_"), UBinding(Loc(1), "_"), UInt((Loc(2), 12))]->ne,
      ]->ne,
      nodes: n1,
    }
    let c2 = {
      Untyped.patterns: [
        [
          P.UBinding(Loc(3), "_"),
          UTuple(Loc(4), [UInt(Loc(5), 20), UInt(Loc(6), 21)]),
          UInt(Loc(7), 22),
        ]->ne,
      ]->ne,
      nodes: n1,
    }
    let c3 = {
      Untyped.patterns: [
        [
          P.UBinding(Loc(8), "_"),
          UTuple(Loc(9), [UInt(Loc(10), 20), UInt(Loc(11), 21)]),
          UInt(Loc(12), 32),
        ]->ne,
      ]->ne,
      nodes: n1,
    }
    let c4 = {
      Untyped.patterns: [
        [
          P.UBinding(Loc(13), "_"),
          UTuple(Loc(14), [UBinding(Loc(15), "_"), UBinding(Loc(16), "_")]),
          UBinding(Loc(17), "_"),
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
                val: TInt(20),
                ifMatch: Switch({
                  idx: 1,
                  key: "",
                  ids: SI.empty,
                  cases: {
                    val: TInt(21),
                    ifMatch: End(
                      Switch({
                        idx: 2,
                        key: "",
                        ids: SI.empty,
                        cases: {
                          val: TInt(12),
                          ifMatch: End({names: MS.empty, exit: 0}),
                          nextCase: Some({
                            val: TInt(22),
                            ifMatch: End({names: MS.empty, exit: 1}),
                            nextCase: Some({
                              val: TInt(32),
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
                          val: TInt(12),
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
                        val: TInt(12),
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
                  val: TInt(12),
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
    let n1 = [Untyped.UText("", NoTrim)]
    let n2 = [Untyped.UText("", NoTrim)]
    let n3 = [Untyped.UText("", NoTrim)]
    let c1 = {
      Untyped.patterns: [[P.UBinding(Loc(3), "x"), UInt(Loc(4), 41)]->ne]->ne,
      nodes: n1,
    }
    let c2 = {
      Untyped.patterns: [
        [
          P.UTuple(
            Loc(0),
            [UTuple(Loc(0), [UInt(Loc(1), 10), UInt(Loc(1), 20)]), UInt(Loc(1), 30)],
          ),
          UInt(Loc(2), 40),
        ]->ne,
      ]->ne,
      nodes: n2,
    }
    let c3 = {
      Untyped.patterns: [[P.UBinding(Loc(5), "y"), UBinding(Loc(6), "z")]->ne]->ne,
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
                val: TInt(10),
                ifMatch: Switch({
                  idx: 1,
                  key: "",
                  ids: SI.empty,
                  cases: {
                    val: TInt(20),
                    ifMatch: End(
                      Switch({
                        idx: 1,
                        key: "",
                        ids: SI.empty,
                        cases: {
                          val: TInt(30),
                          ifMatch: End(
                            Switch({
                              idx: 1,
                              key: "",
                              ids: SI.fromArray([6]),
                              cases: {
                                val: TInt(40),
                                ifMatch: End({names: MS.empty, exit: 1}),
                                nextCase: Some({
                                  val: TInt(41),
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
                val: TInt(41),
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
    let n1 = [Untyped.UText("", NoTrim)]
    let n2 = [Untyped.UText("", NoTrim)]
    let n3 = [Untyped.UText("", NoTrim)]
    let c1 = {
      Untyped.patterns: [[P.UList(Loc(0), [])]->ne]->ne,
      nodes: n1,
    }
    let c2 = {
      Untyped.patterns: [[P.UList(Loc(0), [UBinding(Loc(2), "x")])]->ne]->ne,
      nodes: n2,
    }
    let c3 = {
      Untyped.patterns: [
        [P.UListWithTailBinding(Loc(0), [UBinding(Loc(2), "x")], UBinding(Loc(5), "y"))]->ne,
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
          kind: TList,
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
                  kind: TList,
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
    let nodes1 = [Untyped.UText("", NoTrim)]
    let nodes2 = [Untyped.UText("", NoTrim)]
    let nodes3 = [Untyped.UText("", NoTrim)]
    let nodes4 = [Untyped.UText("", NoTrim)]
    let nodes5 = [Untyped.UText("", NoTrim)]
    let case1 = {
      Untyped.patterns: [
        [P.UList(Loc(0), [UInt(Loc(2), 10), UInt(Loc(3), 11)]), UInt(Loc(4), 12)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let case2 = {
      Untyped.patterns: [
        [
          P.UListWithTailBinding(
            Loc(5),
            [UInt(Loc(6), 10), UInt(Loc(7), 11)],
            UBinding(Loc(8), "x"),
          ),
          UInt(Loc(9), 22),
        ]->ne,
      ]->ne,
      nodes: nodes2,
    }
    let case3 = {
      Untyped.patterns: [[P.UList(Loc(10), [UInt(Loc(11), 30)]), UInt(Loc(12), 32)]->ne]->ne,
      nodes: nodes3,
    }
    let case4 = {
      Untyped.patterns: [[P.UBinding(Loc(13), "y"), UInt(Loc(14), 42)]->ne]->ne,
      nodes: nodes4,
    }
    let case5 = {
      Untyped.patterns: [[P.UBinding(Loc(15), "_"), UBinding(Loc(16), "_")]->ne]->ne,
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
          kind: TList,
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
                  val: TInt(10),
                  ifMatch: Construct({
                    idx: 1,
                    key: "",
                    ids: SI.empty,
                    kind: TList,
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
                            val: TInt(11),
                            ifMatch: Construct({
                              idx: 1,
                              key: "",
                              ids: SI.fromArray([8]),
                              kind: TList,
                              nil: Some(
                                End(
                                  End(
                                    Switch({
                                      idx: 1,
                                      key: "",
                                      ids: SI.empty,
                                      cases: {
                                        val: TInt(12),
                                        ifMatch: End({names: MS.empty, exit: 0}),
                                        nextCase: Some({
                                          val: TInt(22),
                                          ifMatch: End({
                                            names: MS.fromArray([("x", 8)]),
                                            exit: 1,
                                          }),
                                          nextCase: Some({
                                            val: TInt(42),
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
                                          val: TInt(22),
                                          ifMatch: End({
                                            names: MS.fromArray([("x", 8)]),
                                            exit: 1,
                                          }),
                                          nextCase: Some({
                                            val: TInt(42),
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
                    val: TInt(30),
                    ifMatch: Construct({
                      idx: 1,
                      key: "",
                      ids: SI.empty,
                      kind: TList,
                      nil: Some(
                        End(
                          Switch({
                            idx: 1,
                            key: "",
                            ids: SI.empty,
                            cases: {
                              val: TInt(32),
                              ifMatch: End({names: MS.empty, exit: 2}),
                              nextCase: Some({
                                val: TInt(42),
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
                    val: TInt(42),
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
                val: TInt(42),
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
    let l = Debug.Loc(0)
    let nodes1 = [Untyped.UText("a", NoTrim)]
    let case1 = {
      Untyped.patterns: [
        [P.URecord(l, [("a", UInt(l, 10)), ("b", UInt(l, 11))]), UInt(l, 12)]->ne,
        [P.URecord(l, [("b", UInt(l, 21)), ("a", UInt(l, 20))]), UInt(l, 22)]->ne,
        [P.UBinding(l, "_"), UBinding(l, "_")]->ne,
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
              val: TInt(10),
              ifMatch: Switch({
                idx: 1,
                key: "b",
                ids: SI.empty,
                cases: {
                  val: TInt(11),
                  ifMatch: End(
                    Switch({
                      idx: 1,
                      key: "",
                      ids: SI.empty,
                      cases: {
                        val: TInt(12),
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
                val: TInt(20),
                ifMatch: Switch({
                  idx: 1,
                  key: "b",
                  ids: SI.empty,
                  cases: {
                    val: TInt(21),
                    ifMatch: End(
                      Switch({
                        idx: 1,
                        key: "",
                        ids: SI.empty,
                        cases: {
                          val: TInt(22),
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
    let l = Debug.Loc(0)
    let nodes1 = [Untyped.UText("a", NoTrim)]
    let case1 = {
      Untyped.patterns: [
        [P.URecord(l, [("a", UInt(l, 10)), ("b", UInt(l, 11))]), UInt(l, 12)]->ne,
        [P.URecord(l, [("b", UInt(l, 21))]), UInt(l, 22)]->ne,
        [P.UBinding(l, "_"), UBinding(l, "_")]->ne,
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
              val: TInt(10),
              ifMatch: Switch({
                idx: 1,
                key: "b",
                ids: SI.empty,
                cases: {
                  val: TInt(11),
                  ifMatch: End(
                    Switch({
                      idx: 1,
                      key: "",
                      ids: SI.empty,
                      cases: {
                        val: TInt(12),
                        ifMatch: End({names: MS.empty, exit: 0}),
                        nextCase: None,
                      },
                      wildcard: Some(End({names: MS.empty, exit: 0})),
                    }),
                  ),
                  nextCase: Some({
                    val: TInt(21),
                    ifMatch: End(
                      Switch({
                        idx: 1,
                        key: "",
                        ids: SI.empty,
                        cases: {
                          val: TInt(22),
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
                  val: TInt(21),
                  ifMatch: End(
                    Switch({
                      idx: 1,
                      key: "",
                      ids: SI.empty,
                      cases: {
                        val: TInt(22),
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
    let nodes1 = [Untyped.UText("x", NoTrim)]
    let nodes2 = [Untyped.UText("y", NoTrim)]
    let nodes3 = [Untyped.UText("z", NoTrim)]
    let nodes4 = [Untyped.UText("zz", NoTrim)]
    let case1 = {
      Untyped.patterns: [[P.URecord(Loc(0), [("b", UInt(Loc(1), 10))])]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      Untyped.patterns: [[P.URecord(Loc(2), [("a", UInt(Loc(3), 20))])]->ne]->ne,
      nodes: nodes2,
    }
    let case3 = {
      Untyped.patterns: [[P.URecord(Loc(4), [("c", UInt(Loc(5), 30))])]->ne]->ne,
      nodes: nodes3,
    }
    let case4 = {
      Untyped.patterns: [[P.UBinding(Loc(6), "x")]->ne]->ne,
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
              val: TInt(20),
              ifMatch: Switch({
                idx: 1,
                key: "b",
                ids: SI.empty,
                cases: {
                  val: TInt(10),
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
                  val: TInt(10),
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
                      val: TInt(30),
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
