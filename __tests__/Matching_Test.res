/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
open TestFramework
module NE = NonEmpty
module SI = Belt.Set.Int
module MS = Belt.Map.String
module TC = Typechecker
module P = Parser.Pattern
let ne = NE.fromArrayExn
let e = Matching.Exit.unsafe_key

let g = Utils.Dagmap.prelinked(Belt.HashMap.String.make(~hintSize=0))

let makeCases = c => {
  let (_, cases) = TC.makeCases(ne(c), TC.Context.make(#Component), ~loc=Loc(0), ~name="", g)
  cases
}

describe("Basic tree", ({test, _}) => {
  test("blah", ({expect, _}) => {
    let nodes1 = [Parser.UText("", NoTrim)]
    let nodes2 = [Parser.UText("", NoTrim)]
    let case1 = {
      Parser.patterns: [[P.USome(Loc(0), UBinding(Loc(1), "a"))]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      Parser.patterns: [[P.UNull(Loc(3))]->ne]->ne,
      nodes: nodes2,
    }
    let result = [case1, case2]->makeCases->Matching.make(~name="")
    expect.value(result.tree).toEqual(
      Construct({
        key: 0,
        ids: SI.empty,
        extra: TNullable,
        nil: Some(End({names: MS.empty, exit: e(1)})),
        cons: Some(
          Nest({
            key: 0,
            ids: SI.empty,
            extra: Tuple,
            child: IntKeys(
              Wildcard({
                key: 0,
                ids: SI.fromArray([1]),
                child: End(End({names: MS.fromArray([("a", 1)]), exit: e(0)})),
              }),
            ),
            wildcard: None,
          }),
        ),
      }),
    )
  })
  test("cases are sorted correctly", ({expect, _}) => {
    let nodes1 = [Parser.UText("", NoTrim)]
    let nodes2 = [Parser.UText("", NoTrim)]
    let nodes3 = [Parser.UText("", NoTrim)]
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
    let case3 = {
      Parser.patterns: [[P.UBinding(Loc(0), "_")]->ne]->ne,
      nodes: nodes3,
    }
    let result = [case1, case2, case3]->makeCases->Matching.make(~name="")
    expect.value(result.tree).toEqual(
      Switch({
        key: 0,
        ids: SI.empty,
        cases: {
          val: PInt(0),
          ifMatch: End({names: MS.empty, exit: e(0)}),
          nextCase: Some({
            val: PInt(10),
            ifMatch: End({names: MS.empty, exit: e(0)}),
            nextCase: Some({
              val: PInt(15),
              ifMatch: End({names: MS.empty, exit: e(1)}),
              nextCase: Some({
                val: PInt(20),
                ifMatch: End({names: MS.empty, exit: e(0)}),
                nextCase: Some({
                  val: PInt(30),
                  ifMatch: End({names: MS.empty, exit: e(0)}),
                  nextCase: None,
                }),
              }),
            }),
          }),
        },
        wildcard: Some(End({names: MS.empty, exit: e(2)})),
      }),
    )
  })

  test("Basic dec tree 1", ({expect, _}) => {
    let nodes1 = [Parser.UText("", NoTrim)]
    let case1 = {
      Parser.patterns: [
        [P.UInt(Loc(0), 1), UInt(Loc(1), 2), UInt(Loc(2), 3)]->ne,
        [P.UInt(Loc(3), 1), UInt(Loc(4), 4), UInt(Loc(5), 5)]->ne,
        [P.UInt(Loc(6), 10), UInt(Loc(7), 20), UInt(Loc(8), 30)]->ne,
        [P.UInt(Loc(9), 10), UInt(Loc(10), 20), UInt(Loc(11), 40)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let nodes2 = [Parser.UText("", NoTrim)]
    let case2 = {
      Parser.patterns: [
        [P.UInt(Loc(12), 100), UInt(Loc(13), 102), UInt(Loc(14), 103)]->ne,
        [P.UInt(Loc(15), 100), UInt(Loc(16), 104), UInt(Loc(17), 105)]->ne,
      ]->ne,
      nodes: nodes2,
    }
    let nodes3 = [Parser.UText("", NoTrim)]
    let case3 = {
      Parser.patterns: [
        [P.UInt(Loc(18), 10), UInt(Loc(19), 20), UInt(Loc(20), 50)]->ne,
        [P.UInt(Loc(21), 1), UInt(Loc(22), 2), UInt(Loc(23), 100)]->ne,
        [P.UInt(Loc(24), 1), UInt(Loc(25), 2), UInt(Loc(26), 101)]->ne,
        [P.UInt(Loc(27), 100), UInt(Loc(28), 102), UInt(Loc(29), 106)]->ne,
      ]->ne,
      nodes: nodes3,
    }
    let nodes4 = [Parser.UText("", NoTrim)]
    let case4 = {
      Parser.patterns: [
        [P.UBinding(Loc(30), "_"), UBinding(Loc(31), "_"), UBinding(Loc(32), "_")]->ne,
      ]->ne,
      nodes: nodes4,
    }
    let result = [case1, case2, case3, case4]->makeCases->Matching.make(~name="")
    expect.value(result.tree).toEqual(
      Switch({
        key: 0,
        ids: SI.empty,
        cases: {
          val: PInt(1),
          ifMatch: Switch({
            key: 1,
            ids: SI.empty,
            cases: {
              val: PInt(2),
              ifMatch: Switch({
                key: 2,
                ids: SI.empty,
                cases: {
                  val: PInt(3),
                  ifMatch: End({names: MS.empty, exit: e(0)}),
                  nextCase: Some({
                    val: PInt(100),
                    ifMatch: End({names: MS.empty, exit: e(2)}),
                    nextCase: Some({
                      val: PInt(101),
                      ifMatch: End({names: MS.empty, exit: e(2)}),
                      nextCase: None,
                    }),
                  }),
                },
                wildcard: Some(End({names: MS.empty, exit: e(3)})),
              }),
              nextCase: Some({
                val: PInt(4),
                ifMatch: Switch({
                  key: 2,
                  ids: SI.empty,
                  cases: {
                    val: PInt(5),
                    ifMatch: End({names: MS.empty, exit: e(0)}),
                    nextCase: None,
                  },
                  wildcard: Some(End({names: MS.empty, exit: e(3)})),
                }),
                nextCase: None,
              }),
            },
            wildcard: Some(
              Wildcard({
                key: 2,
                ids: SI.empty,
                child: End({names: MS.empty, exit: e(3)}),
              }),
            ),
          }),
          nextCase: Some({
            val: PInt(10),
            ifMatch: Switch({
              key: 1,
              ids: SI.empty,
              cases: {
                val: PInt(20),
                ifMatch: Switch({
                  key: 2,
                  ids: SI.empty,
                  cases: {
                    val: PInt(30),
                    ifMatch: End({names: MS.empty, exit: e(0)}),
                    nextCase: Some({
                      val: PInt(40),
                      ifMatch: End({names: MS.empty, exit: e(0)}),
                      nextCase: Some({
                        val: PInt(50),
                        ifMatch: End({names: MS.empty, exit: e(2)}),
                        nextCase: None,
                      }),
                    }),
                  },
                  wildcard: Some(End({names: MS.empty, exit: e(3)})),
                }),
                nextCase: None,
              },
              wildcard: Some(
                Wildcard({
                  key: 2,
                  ids: SI.empty,
                  child: End({names: MS.empty, exit: e(3)}),
                }),
              ),
            }),
            nextCase: Some({
              val: PInt(100),
              ifMatch: Switch({
                key: 1,
                ids: SI.empty,
                cases: {
                  val: PInt(102),
                  ifMatch: Switch({
                    key: 2,
                    ids: SI.empty,
                    cases: {
                      val: PInt(103),
                      ifMatch: End({names: MS.empty, exit: e(1)}),
                      nextCase: Some({
                        val: PInt(106),
                        ifMatch: End({names: MS.empty, exit: e(2)}),
                        nextCase: None,
                      }),
                    },
                    wildcard: Some(End({names: MS.empty, exit: e(3)})),
                  }),
                  nextCase: Some({
                    val: PInt(104),
                    ifMatch: Switch({
                      key: 2,
                      ids: SI.empty,
                      cases: {
                        val: PInt(105),
                        ifMatch: End({names: MS.empty, exit: e(1)}),
                        nextCase: None,
                      },
                      wildcard: Some(End({names: MS.empty, exit: e(3)})),
                    }),
                    nextCase: None,
                  }),
                },
                wildcard: Some(
                  Wildcard({
                    key: 2,
                    ids: SI.empty,
                    child: End({names: MS.empty, exit: e(3)}),
                  }),
                ),
              }),
              nextCase: None,
            }),
          }),
        },
        wildcard: Some(
          Wildcard({
            key: 1,
            ids: SI.empty,
            child: Wildcard({
              key: 2,
              ids: SI.empty,
              child: End({names: MS.empty, exit: e(3)}),
            }),
          }),
        ),
      }),
    )
  })

  test("Basic dec tree 2", ({expect, _}) => {
    let nodes1 = [Parser.UText("", NoTrim)]
    let case1 = {
      Parser.patterns: [
        [P.UInt(Loc(0), 10), UInt(Loc(1), 11), UInt(Loc(2), 12)]->ne,
        [P.UBinding(Loc(3), "x"), UInt(Loc(4), 21), UInt(Loc(5), 22)]->ne,
        [P.UInt(Loc(9), 30), UInt(Loc(10), 31), UInt(Loc(11), 32)]->ne,
        [P.UInt(Loc(12), 30), UBinding(Loc(13), "y"), UInt(Loc(14), 42)]->ne,
        [P.UBinding(Loc(18), "a"), UBinding(Loc(19), "b"), UBinding(Loc(20), "c")]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let result = [case1]->makeCases->Matching.make(~name="")
    expect.value(result.tree).toEqual(
      Switch({
        key: 0,
        ids: SI.empty->SI.add(3)->SI.add(18),
        cases: {
          val: PInt(10),
          ifMatch: Switch({
            key: 1,
            ids: SI.fromArray([19]),
            cases: {
              val: PInt(11),
              ifMatch: Switch({
                key: 2,
                ids: SI.fromArray([20]),
                cases: {
                  val: PInt(12),
                  ifMatch: End({names: MS.empty, exit: e(0)}),
                  nextCase: None,
                },
                wildcard: Some(
                  End({
                    exit: e(0),
                    names: MS.empty->MS.set("a", 18)->MS.set("b", 19)->MS.set("c", 20),
                  }),
                ),
              }),
              nextCase: Some({
                val: PInt(21),
                ifMatch: Switch({
                  key: 2,
                  ids: SI.fromArray([20]),
                  cases: {
                    val: PInt(22),
                    ifMatch: End({names: MS.fromArray([("x", 3)]), exit: e(0)}),
                    nextCase: None,
                  },
                  wildcard: Some(
                    End({
                      exit: e(0),
                      names: MS.empty->MS.set("a", 18)->MS.set("b", 19)->MS.set("c", 20),
                    }),
                  ),
                }),
                nextCase: None,
              }),
            },
            wildcard: Some(
              Wildcard({
                key: 2,
                ids: SI.fromArray([20]),
                child: End({
                  exit: e(0),
                  names: MS.empty->MS.set("a", 18)->MS.set("b", 19)->MS.set("c", 20),
                }),
              }),
            ),
          }),
          nextCase: Some({
            val: PInt(30),
            ifMatch: Switch({
              key: 1,
              ids: SI.empty->SI.add(13)->SI.add(19),
              cases: {
                val: PInt(21),
                ifMatch: Switch({
                  key: 2,
                  ids: SI.fromArray([20]),
                  cases: {
                    val: PInt(22),
                    ifMatch: End({names: MS.fromArray([("x", 3)]), exit: e(0)}),
                    nextCase: Some({
                      val: PInt(42),
                      ifMatch: End({names: MS.fromArray([("y", 13)]), exit: e(0)}),
                      nextCase: None,
                    }),
                  },
                  wildcard: Some(
                    End({
                      exit: e(0),
                      names: MS.empty->MS.set("a", 18)->MS.set("b", 19)->MS.set("c", 20),
                    }),
                  ),
                }),
                nextCase: Some({
                  val: PInt(31),
                  ifMatch: Switch({
                    key: 2,
                    ids: SI.fromArray([20]),
                    cases: {
                      val: PInt(32),
                      ifMatch: End({names: MS.empty, exit: e(0)}),
                      nextCase: Some({
                        val: PInt(42),
                        ifMatch: End({names: MS.fromArray([("y", 13)]), exit: e(0)}),
                        nextCase: None,
                      }),
                    },
                    wildcard: Some(
                      End({
                        exit: e(0),
                        names: MS.empty->MS.set("a", 18)->MS.set("b", 19)->MS.set("c", 20),
                      }),
                    ),
                  }),
                  nextCase: None,
                }),
              },
              wildcard: Some(
                Switch({
                  key: 2,
                  ids: SI.fromArray([20]),
                  cases: {
                    val: PInt(42),
                    ifMatch: End({names: MS.fromArray([("y", 13)]), exit: e(0)}),
                    nextCase: None,
                  },
                  wildcard: Some(
                    End({
                      exit: e(0),
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
            key: 1,
            ids: SI.fromArray([19]),
            cases: {
              val: PInt(21),
              ifMatch: Switch({
                key: 2,
                ids: SI.fromArray([20]),
                cases: {
                  val: PInt(22),
                  ifMatch: End({names: MS.fromArray([("x", 3)]), exit: e(0)}),
                  nextCase: None,
                },
                wildcard: Some(
                  End({
                    exit: e(0),
                    names: MS.empty->MS.set("a", 18)->MS.set("b", 19)->MS.set("c", 20),
                  }),
                ),
              }),
              nextCase: None,
            },
            wildcard: Some(
              Wildcard({
                key: 2,
                ids: SI.fromArray([20]),
                child: End({
                  exit: e(0),
                  names: MS.empty->MS.set("a", 18)->MS.set("b", 19)->MS.set("c", 20),
                }),
              }),
            ),
          }),
        ),
      }),
    )
  })
})

describe("Nests", ({test, _}) => {
  test("dec tree tuple", ({expect, _}) => {
    let nodes1 = [Parser.UText("", NoTrim)]
    let case1 = {
      Parser.patterns: [
        [P.UTuple(Loc(0), [UInt(Loc(1), 10), UInt(Loc(2), 12)]), UInt(Loc(3), 13)]->ne,
        [P.UTuple(Loc(4), [UInt(Loc(5), 10), UInt(Loc(6), 22)]), UInt(Loc(7), 23)]->ne,
        [P.UBinding(Loc(8), "_"), UInt(Loc(9), 33)]->ne,
        [P.UBinding(Loc(10), "_"), UBinding(Loc(11), "_")]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let result = [case1]->makeCases->Matching.make(~name="")
    expect.value(result.tree).toEqual(
      Nest({
        key: 0,
        ids: SI.empty,
        extra: Tuple,
        child: IntKeys(
          Switch({
            key: 0,
            ids: SI.empty,
            cases: {
              val: PInt(10),
              ifMatch: Switch({
                key: 1,
                ids: SI.empty,
                cases: {
                  val: PInt(12),
                  ifMatch: End(
                    Switch({
                      key: 1,
                      ids: SI.empty,
                      cases: {
                        val: PInt(13),
                        ifMatch: End({names: MS.empty, exit: e(0)}),
                        nextCase: Some({
                          val: PInt(33),
                          ifMatch: End({names: MS.empty, exit: e(0)}),
                          nextCase: None,
                        }),
                      },
                      wildcard: Some(End({names: MS.empty, exit: e(0)})),
                    }),
                  ),
                  nextCase: Some({
                    val: PInt(22),
                    ifMatch: End(
                      Switch({
                        key: 1,
                        ids: SI.empty,
                        cases: {
                          val: PInt(23),
                          ifMatch: End({names: MS.empty, exit: e(0)}),
                          nextCase: Some({
                            val: PInt(33),
                            ifMatch: End({names: MS.empty, exit: e(0)}),
                            nextCase: None,
                          }),
                        },
                        wildcard: Some(End({names: MS.empty, exit: e(0)})),
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
        ),
        wildcard: Some(
          Switch({
            key: 1,
            ids: SI.empty,
            cases: {
              val: PInt(33),
              ifMatch: End({names: MS.empty, exit: e(0)}),
              nextCase: None,
            },
            wildcard: Some(End({names: MS.empty, exit: e(0)})),
          }),
        ),
      }),
    )
  })

  test("Nests merge into wildcards correctly 1.", ({expect, _}) => {
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
        [
          P.UTuple(Loc(10), [UBinding(Loc(11), "_"), UBinding(Loc(12), "y")]),
          UBinding(Loc(13), "z"),
        ]->ne,
      ]->ne,
      nodes: nodes3,
    }
    let result = [case1, case2, case3]->makeCases->Matching.make(~name="")
    expect.value(result.tree).toEqual(
      Nest({
        key: 0,
        ids: SI.fromArray([0]),
        extra: Tuple,
        child: IntKeys(
          Switch({
            key: 0,
            ids: SI.empty,
            cases: {
              val: PString("a"),
              ifMatch: Switch({
                key: 1,
                ids: SI.fromArray([12]),
                cases: {
                  val: PString("b"),
                  ifMatch: End(
                    Switch({
                      key: 1,
                      ids: SI.fromArray([13]),
                      cases: {
                        val: PInt(1),
                        ifMatch: End({names: MS.fromArray([("x", 0)]), exit: e(0)}),
                        nextCase: Some({
                          val: PInt(10),
                          ifMatch: End({names: MS.empty, exit: e(1)}),
                          nextCase: None,
                        }),
                      },
                      wildcard: Some(
                        End({
                          names: MS.empty->MS.set("y", 12)->MS.set("z", 13),
                          exit: e(2),
                        }),
                      ),
                    }),
                  ),
                  nextCase: None,
                },
                wildcard: Some(
                  End(
                    Switch({
                      key: 1,
                      ids: SI.fromArray([13]),
                      cases: {
                        val: PInt(1),
                        ifMatch: End({names: MS.fromArray([("x", 0)]), exit: e(0)}),
                        nextCase: None,
                      },
                      wildcard: Some(
                        End({
                          names: MS.empty->MS.set("y", 12)->MS.set("z", 13),
                          exit: e(2),
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
                key: 1,
                ids: SI.fromArray([12]),
                child: End(
                  Switch({
                    key: 1,
                    ids: SI.fromArray([13]),
                    cases: {
                      val: PInt(1),
                      ifMatch: End({names: MS.fromArray([("x", 0)]), exit: e(0)}),
                      nextCase: None,
                    },
                    wildcard: Some(
                      End({
                        names: MS.empty->MS.set("y", 12)->MS.set("z", 13),
                        exit: e(2),
                      }),
                    ),
                  }),
                ),
              }),
            ),
          }),
        ),
        wildcard: Some(
          Switch({
            key: 1,
            ids: SI.empty,
            cases: {
              val: PInt(1),
              ifMatch: End({names: MS.fromArray([("x", 0)]), exit: e(0)}),
              nextCase: None,
            },
            wildcard: None,
          }),
        ),
      }),
    )
  })

  test("Nests merge correctly.", ({expect, _}) => {
    let n1 = [Parser.UText("", NoTrim)]
    let c1 = {
      Parser.patterns: [
        [P.UBinding(Loc(0), "_"), UBinding(Loc(1), "_"), UInt((Loc(2), 12))]->ne,
      ]->ne,
      nodes: n1,
    }
    let c2 = {
      Parser.patterns: [
        [
          P.UBinding(Loc(3), "_"),
          UTuple(Loc(4), [UInt(Loc(5), 20), UInt(Loc(6), 21)]),
          UInt(Loc(7), 22),
        ]->ne,
      ]->ne,
      nodes: n1,
    }
    let c3 = {
      Parser.patterns: [
        [
          P.UBinding(Loc(8), "_"),
          UTuple(Loc(9), [UInt(Loc(10), 20), UInt(Loc(11), 21)]),
          UInt(Loc(12), 32),
        ]->ne,
      ]->ne,
      nodes: n1,
    }
    let c4 = {
      Parser.patterns: [
        [
          P.UBinding(Loc(13), "_"),
          UTuple(Loc(14), [UBinding(Loc(15), "_"), UBinding(Loc(16), "_")]),
          UBinding(Loc(17), "_"),
        ]->ne,
      ]->ne,
      nodes: n1,
    }
    let result = [c1, c2, c3, c4]->makeCases->Matching.make(~name="")
    expect.value(result.tree).toEqual(
      Wildcard({
        key: 0,
        ids: SI.empty,
        child: Nest({
          key: 1,
          ids: SI.empty,
          extra: Tuple,
          child: IntKeys(
            Switch({
              key: 0,
              ids: SI.empty,
              cases: {
                val: PInt(20),
                ifMatch: Switch({
                  key: 1,
                  ids: SI.empty,
                  cases: {
                    val: PInt(21),
                    ifMatch: End(
                      Switch({
                        key: 2,
                        ids: SI.empty,
                        cases: {
                          val: PInt(12),
                          ifMatch: End({names: MS.empty, exit: e(0)}),
                          nextCase: Some({
                            val: PInt(22),
                            ifMatch: End({names: MS.empty, exit: e(1)}),
                            nextCase: Some({
                              val: PInt(32),
                              ifMatch: End({names: MS.empty, exit: e(2)}),
                              nextCase: None,
                            }),
                          }),
                        },
                        wildcard: Some(End({names: MS.empty, exit: e(3)})),
                      }),
                    ),
                    nextCase: None,
                  },
                  wildcard: Some(
                    End(
                      Switch({
                        key: 2,
                        ids: SI.empty,
                        cases: {
                          val: PInt(12),
                          ifMatch: End({names: MS.empty, exit: e(0)}),
                          nextCase: None,
                        },
                        wildcard: Some(End({names: MS.empty, exit: e(3)})),
                      }),
                    ),
                  ),
                }),
                nextCase: None,
              },
              wildcard: Some(
                Wildcard({
                  key: 1,
                  ids: SI.empty,
                  child: End(
                    Switch({
                      key: 2,
                      ids: SI.empty,
                      cases: {
                        val: PInt(12),
                        ifMatch: End({names: MS.empty, exit: e(0)}),
                        nextCase: None,
                      },
                      wildcard: Some(End({names: MS.empty, exit: e(3)})),
                    }),
                  ),
                }),
              ),
            }),
          ),
          wildcard: Some(
            Switch({
              key: 2,
              ids: SI.empty,
              cases: {
                val: PInt(12),
                ifMatch: End({names: MS.empty, exit: e(0)}),
                nextCase: None,
              },
              wildcard: None,
            }),
          ),
        }),
      }),
    )
  })

  test("Wildcards merge after nests correctly.", ({expect, _}) => {
    let n1 = [Parser.UText("", NoTrim)]
    let n2 = [Parser.UText("", NoTrim)]
    let n3 = [Parser.UText("", NoTrim)]
    let c1 = {
      Parser.patterns: [[P.UBinding(Loc(3), "x"), UInt(Loc(4), 41)]->ne]->ne,
      nodes: n1,
    }
    let c2 = {
      Parser.patterns: [
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
      Parser.patterns: [[P.UBinding(Loc(5), "y"), UBinding(Loc(6), "z")]->ne]->ne,
      nodes: n3,
    }
    let result = [c1, c2, c3]->makeCases->Matching.make(~name="")
    expect.value(result.tree).toEqual(
      Nest({
        key: 0,
        ids: SI.empty->SI.add(3)->SI.add(5),
        extra: Tuple,
        child: IntKeys(
          Nest({
            key: 0,
            ids: SI.empty,
            extra: Tuple,
            child: IntKeys(
              Switch({
                key: 0,
                ids: SI.empty,
                cases: {
                  val: PInt(10),
                  ifMatch: Switch({
                    key: 1,
                    ids: SI.empty,
                    cases: {
                      val: PInt(20),
                      ifMatch: End(
                        Switch({
                          key: 1,
                          ids: SI.empty,
                          cases: {
                            val: PInt(30),
                            ifMatch: End(
                              Switch({
                                key: 1,
                                ids: SI.fromArray([6]),
                                cases: {
                                  val: PInt(40),
                                  ifMatch: End({names: MS.empty, exit: e(1)}),
                                  nextCase: Some({
                                    val: PInt(41),
                                    ifMatch: End({names: MS.fromArray([("x", 3)]), exit: e(0)}),
                                    nextCase: None,
                                  }),
                                },
                                wildcard: Some(
                                  End({
                                    names: MS.empty->MS.set("y", 5)->MS.set("z", 6),
                                    exit: e(2),
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
            ),
            wildcard: None,
          }),
        ),
        wildcard: Some(
          Switch({
            key: 1,
            ids: SI.fromArray([6]),
            cases: {
              val: PInt(41),
              ifMatch: End({names: MS.fromArray([("x", 3)]), exit: e(0)}),
              nextCase: None,
            },
            wildcard: Some(
              End({
                names: MS.empty->MS.set("y", 5)->MS.set("z", 6),
                exit: e(2),
              }),
            ),
          }),
        ),
      }),
    )
  })

  test("Different-sized lists merge correctly.", ({expect, _}) => {
    let n1 = [Parser.UText("", NoTrim)]
    let n2 = [Parser.UText("", NoTrim)]
    let n3 = [Parser.UText("", NoTrim)]
    let c1 = {
      Parser.patterns: [[P.UList(Loc(0), [])]->ne]->ne,
      nodes: n1,
    }
    let c2 = {
      Parser.patterns: [[P.UList(Loc(0), [UBinding(Loc(2), "x")])]->ne]->ne,
      nodes: n2,
    }
    let c3 = {
      Parser.patterns: [
        [P.UListWithTailBinding(Loc(0), [UBinding(Loc(2), "x")], UBinding(Loc(5), "y"))]->ne,
      ]->ne,
      nodes: n3,
    }
    let result = [c1, c2, c3]->makeCases->Matching.make(~name="")
    expect.value(result.tree).toEqual(
      Construct({
        key: 0,
        ids: SI.empty,
        extra: TList,
        nil: Some(End({names: MS.empty, exit: e(0)})),
        cons: Some(
          Nest({
            key: 0,
            ids: SI.empty,
            extra: Tuple,
            child: IntKeys(
              Wildcard({
                key: 0,
                ids: SI.fromArray([2]),
                child: Construct({
                  key: 1,
                  ids: SI.fromArray([5]),
                  extra: TList,
                  nil: Some(End(End({names: MS.fromArray([("x", 2)]), exit: e(1)}))),
                  cons: Some(
                    Wildcard({
                      key: 1,
                      ids: SI.fromArray([5]),
                      child: End(
                        End({
                          names: MS.empty->MS.set("x", 2)->MS.set("y", 5),
                          exit: e(2),
                        }),
                      ),
                    }),
                  ),
                }),
              }),
            ),
            wildcard: None,
          }),
        ),
      }),
    )
  })

  test("dec tree list", ({expect, _}) => {
    let nodes1 = [Parser.UText("", NoTrim)]
    let nodes2 = [Parser.UText("", NoTrim)]
    let nodes3 = [Parser.UText("", NoTrim)]
    let nodes4 = [Parser.UText("", NoTrim)]
    let nodes5 = [Parser.UText("", NoTrim)]
    let case1 = {
      Parser.patterns: [
        [P.UList(Loc(0), [UInt(Loc(2), 10), UInt(Loc(3), 11)]), UInt(Loc(4), 12)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let case2 = {
      Parser.patterns: [
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
      Parser.patterns: [[P.UList(Loc(10), [UInt(Loc(11), 30)]), UInt(Loc(12), 32)]->ne]->ne,
      nodes: nodes3,
    }
    let case4 = {
      Parser.patterns: [[P.UBinding(Loc(13), "y"), UInt(Loc(14), 42)]->ne]->ne,
      nodes: nodes4,
    }
    let case5 = {
      Parser.patterns: [[P.UBinding(Loc(15), "_"), UBinding(Loc(16), "_")]->ne]->ne,
      nodes: nodes5,
    }
    let result = [case1, case2, case3, case4, case5]->makeCases->Matching.make(~name="")
    expect.value(result.tree).toEqual(
      Construct({
        key: 0,
        ids: SI.fromArray([13]),
        extra: TList,
        cons: Some(
          Nest({
            key: 0,
            ids: SI.fromArray([13]),
            extra: Tuple,
            child: IntKeys(
              Switch({
                key: 0,
                ids: SI.empty,
                cases: {
                  val: PInt(10),
                  ifMatch: Construct({
                    key: 1,
                    ids: SI.empty,
                    extra: TList,
                    cons: Some(
                      Nest({
                        key: 1,
                        ids: SI.empty,
                        extra: Tuple,
                        child: IntKeys(
                          Switch({
                            key: 0,
                            ids: SI.empty,
                            cases: {
                              val: PInt(11),
                              ifMatch: Construct({
                                key: 1,
                                ids: SI.fromArray([8]),
                                extra: TList,
                                nil: Some(
                                  End(
                                    End(
                                      Switch({
                                        key: 1,
                                        ids: SI.empty,
                                        cases: {
                                          val: PInt(12),
                                          ifMatch: End({names: MS.empty, exit: e(0)}),
                                          nextCase: Some({
                                            val: PInt(22),
                                            ifMatch: End({
                                              names: MS.fromArray([("x", 8)]),
                                              exit: e(1),
                                            }),
                                            nextCase: Some({
                                              val: PInt(42),
                                              ifMatch: End({
                                                names: MS.fromArray([("y", 13)]),
                                                exit: e(3),
                                              }),
                                              nextCase: None,
                                            }),
                                          }),
                                        },
                                        wildcard: Some(End({names: MS.empty, exit: e(4)})),
                                      }),
                                    ),
                                  ),
                                ),
                                cons: Some(
                                  Wildcard({
                                    key: 1,
                                    ids: SI.fromArray([8]),
                                    child: End(
                                      End(
                                        Switch({
                                          key: 1,
                                          ids: SI.empty,
                                          cases: {
                                            val: PInt(22),
                                            ifMatch: End({
                                              names: MS.fromArray([("x", 8)]),
                                              exit: e(1),
                                            }),
                                            nextCase: Some({
                                              val: PInt(42),
                                              ifMatch: End({
                                                names: MS.fromArray([("y", 13)]),
                                                exit: e(3),
                                              }),
                                              nextCase: None,
                                            }),
                                          },
                                          wildcard: Some(End({names: MS.empty, exit: e(4)})),
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
                        ),
                        wildcard: None,
                      }),
                    ),
                    nil: None,
                  }),
                  nextCase: Some({
                    val: PInt(30),
                    ifMatch: Construct({
                      key: 1,
                      ids: SI.empty,
                      extra: TList,
                      nil: Some(
                        End(
                          Switch({
                            key: 1,
                            ids: SI.empty,
                            cases: {
                              val: PInt(32),
                              ifMatch: End({names: MS.empty, exit: e(2)}),
                              nextCase: Some({
                                val: PInt(42),
                                ifMatch: End({names: MS.fromArray([("y", 13)]), exit: e(3)}),
                                nextCase: None,
                              }),
                            },
                            wildcard: Some(End({names: MS.empty, exit: e(4)})),
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
            ),
            wildcard: Some(
              Switch({
                key: 1,
                ids: SI.empty,
                cases: {
                  val: PInt(42),
                  ifMatch: End({names: MS.fromArray([("y", 13)]), exit: e(3)}),
                  nextCase: None,
                },
                wildcard: Some(End({names: MS.empty, exit: e(4)})),
              }),
            ),
          }),
        ),
        nil: Some(
          Switch({
            key: 1,
            ids: SI.empty,
            cases: {
              val: PInt(42),
              ifMatch: End({names: MS.fromArray([("y", 13)]), exit: e(3)}),
              nextCase: None,
            },
            wildcard: Some(End({names: MS.empty, exit: e(4)})),
          }),
        ),
      }),
    )
  })

  test("Records sort fields correctly", ({expect, _}) => {
    let l = Debug.Loc(0)
    let nodes1 = [Parser.UText("a", NoTrim)]
    let case1 = {
      Parser.patterns: [
        [P.URecord(l, [("a", UInt(l, 10)), ("b", UInt(l, 11))]), UInt(l, 12)]->ne,
        [P.URecord(l, [("b", UInt(l, 21)), ("a", UInt(l, 20))]), UInt(l, 22)]->ne,
        [P.UBinding(l, "_"), UBinding(l, "_")]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let result = [case1]->makeCases->Matching.make(~name="")
    expect.value(result.tree).toEqual(
      Nest({
        key: 0,
        ids: SI.empty,
        extra: Record,
        child: StringKeys(
          Switch({
            key: "a",
            ids: SI.empty,
            cases: {
              val: PInt(10),
              ifMatch: Switch({
                key: "b",
                ids: SI.empty,
                cases: {
                  val: PInt(11),
                  ifMatch: End(
                    Switch({
                      key: 1,
                      ids: SI.empty,
                      cases: {
                        val: PInt(12),
                        ifMatch: End({names: MS.empty, exit: e(0)}),
                        nextCase: None,
                      },
                      wildcard: Some(End({names: MS.empty, exit: e(0)})),
                    }),
                  ),
                  nextCase: None,
                },
                wildcard: None,
              }),
              nextCase: Some({
                val: PInt(20),
                ifMatch: Switch({
                  key: "b",
                  ids: SI.empty,
                  cases: {
                    val: PInt(21),
                    ifMatch: End(
                      Switch({
                        key: 1,
                        ids: SI.empty,
                        cases: {
                          val: PInt(22),
                          ifMatch: End({names: MS.empty, exit: e(0)}),
                          nextCase: None,
                        },
                        wildcard: Some(End({names: MS.empty, exit: e(0)})),
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
        ),
        wildcard: Some(
          Wildcard({
            key: 1,
            ids: SI.empty,
            child: End({names: MS.empty, exit: e(0)}),
          }),
        ),
      }),
    )
  })

  test("Records: missing fields are automatically wildcards", ({expect, _}) => {
    let l = Debug.Loc(0)
    let nodes1 = [Parser.UText("a", NoTrim)]
    let case1 = {
      Parser.patterns: [
        [P.URecord(l, [("a", UInt(l, 10)), ("b", UInt(l, 11))]), UInt(l, 12)]->ne,
        [P.URecord(l, [("b", UInt(l, 21))]), UInt(l, 22)]->ne,
        [P.UBinding(l, "_"), UBinding(l, "_")]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let result = [case1]->makeCases->Matching.make(~name="")
    expect.value(result.tree).toEqual(
      Nest({
        key: 0,
        ids: SI.empty,
        extra: Record,
        child: StringKeys(
          Switch({
            key: "a",
            ids: SI.empty,
            cases: {
              val: PInt(10),
              ifMatch: Switch({
                key: "b",
                ids: SI.empty,
                cases: {
                  val: PInt(11),
                  ifMatch: End(
                    Switch({
                      key: 1,
                      ids: SI.empty,
                      cases: {
                        val: PInt(12),
                        ifMatch: End({names: MS.empty, exit: e(0)}),
                        nextCase: None,
                      },
                      wildcard: Some(End({names: MS.empty, exit: e(0)})),
                    }),
                  ),
                  nextCase: Some({
                    val: PInt(21),
                    ifMatch: End(
                      Switch({
                        key: 1,
                        ids: SI.empty,
                        cases: {
                          val: PInt(22),
                          ifMatch: End({names: MS.empty, exit: e(0)}),
                          nextCase: None,
                        },
                        wildcard: Some(End({names: MS.empty, exit: e(0)})),
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
                key: "b",
                ids: SI.empty,
                cases: {
                  val: PInt(21),
                  ifMatch: End(
                    Switch({
                      key: 1,
                      ids: SI.empty,
                      cases: {
                        val: PInt(22),
                        ifMatch: End({names: MS.empty, exit: e(0)}),
                        nextCase: None,
                      },
                      wildcard: Some(End({names: MS.empty, exit: e(0)})),
                    }),
                  ),
                  nextCase: None,
                },
                wildcard: None,
              }),
            ),
          }),
        ),
        wildcard: Some(
          Wildcard({
            key: 1,
            ids: SI.empty,
            child: End({names: MS.empty, exit: e(0)}),
          }),
        ),
      }),
    )
  })

  test("Records: new fields expand existing rows", ({expect, _}) => {
    let nodes1 = [Parser.UText("x", NoTrim)]
    let nodes2 = [Parser.UText("y", NoTrim)]
    let nodes3 = [Parser.UText("z", NoTrim)]
    let nodes4 = [Parser.UText("zz", NoTrim)]
    let case1 = {
      Parser.patterns: [[P.URecord(Loc(0), [("b", UInt(Loc(1), 10))])]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      Parser.patterns: [[P.URecord(Loc(2), [("a", UInt(Loc(3), 20))])]->ne]->ne,
      nodes: nodes2,
    }
    let case3 = {
      Parser.patterns: [[P.URecord(Loc(4), [("c", UInt(Loc(5), 30))])]->ne]->ne,
      nodes: nodes3,
    }
    let case4 = {
      Parser.patterns: [[P.UBinding(Loc(6), "x")]->ne]->ne,
      nodes: nodes4,
    }
    let result = [case1, case2, case3, case4]->makeCases->Matching.make(~name="")
    expect.value(result.tree).toEqual(
      Nest({
        key: 0,
        ids: SI.fromArray([6]),
        extra: Record,
        child: StringKeys(
          Switch({
            key: "a",
            ids: SI.empty,
            cases: {
              val: PInt(20),
              ifMatch: Switch({
                key: "b",
                ids: SI.empty,
                cases: {
                  val: PInt(10),
                  ifMatch: Wildcard({
                    key: "c",
                    ids: SI.empty,
                    child: End(End({names: MS.empty, exit: e(0)})),
                  }),
                  nextCase: None,
                },
                wildcard: Some(
                  Wildcard({
                    key: "c",
                    ids: SI.empty,
                    child: End(End({names: MS.empty, exit: e(1)})),
                  }),
                ),
              }),
              nextCase: None,
            },
            wildcard: Some(
              Switch({
                key: "b",
                ids: SI.empty,
                cases: {
                  val: PInt(10),
                  ifMatch: Wildcard({
                    key: "c",
                    ids: SI.empty,
                    child: End(End({names: MS.empty, exit: e(0)})),
                  }),
                  nextCase: None,
                },
                wildcard: Some(
                  Switch({
                    key: "c",
                    ids: SI.empty,
                    cases: {
                      val: PInt(30),
                      ifMatch: End(End({names: MS.empty, exit: e(2)})),
                      nextCase: None,
                    },
                    wildcard: None,
                  }),
                ),
              }),
            ),
          }),
        ),
        wildcard: Some(End({names: MS.fromArray([("x", 6)]), exit: e(3)})),
      }),
    )
  })
})
