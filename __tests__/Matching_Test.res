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
module SetString = Belt.Set.String
let ne = NE.fromArrayExn

describe("Decision tree", ({test, _}) => {
  test("cases are sorted correctly", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let nodes2 = [T.Ast.Text("", NoTrim)]
    let nodes3 = [T.Ast.Text("", NoTrim)]
    let case1 = {
      T.Ast2.pats: [
        [#Int(l(0), 0)]->ne,
        [#Int(l(3), 10)]->ne,
        [#Int(l(3), 20)]->ne,
        [#Int(l(3), 30)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let case2 = {
      T.Ast2.pats: [[#Int(l(0), 15)]->ne]->ne,
      nodes: nodes2,
    }
    let case3 = {
      T.Ast2.pats: [[#Binding(l(0), "_")]->ne]->ne,
      nodes: nodes3,
    }
    let result = Matching.make(ne([case1, case2, case3]), ~loc=Loc(0)).tree->Matching.ParMatch.check
    expect.value(result).toEqual(
      Ok(
        Switch({
          idx: 0,
          key: "",
          names: SetString.empty,
          cases: {
            val: TPat_Int(0),
            ifMatch: End({names: SetString.empty, exit: 0}),
            nextCase: Some({
              val: TPat_Int(10),
              ifMatch: End({names: SetString.empty, exit: 0}),
              nextCase: Some({
                val: TPat_Int(15),
                ifMatch: End({names: SetString.empty, exit: 1}),
                nextCase: Some({
                  val: TPat_Int(20),
                  ifMatch: End({names: SetString.empty, exit: 0}),
                  nextCase: Some({
                    val: TPat_Int(30),
                    ifMatch: End({names: SetString.empty, exit: 0}),
                    nextCase: None,
                  }),
                }),
              }),
            }),
          },
          wildcard: Some(End({names: SetString.empty, exit: 2})),
        }),
      ),
    )
  })

  test("Basic dec tree 1", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let case1 = {
      T.Ast2.pats: [
        [#Int(l(0), 1), #Int(l(1), 2), #Int(l(2), 3)]->ne,
        [#Int(l(3), 1), #Int(l(4), 4), #Int(l(5), 5)]->ne,
        [#Int(l(6), 10), #Int(l(7), 20), #Int(l(8), 30)]->ne,
        [#Int(l(9), 10), #Int(l(10), 20), #Int(l(11), 40)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let nodes2 = [T.Ast.Text("", NoTrim)]
    let case2 = {
      T.Ast2.pats: [
        [#Int(l(12), 100), #Int(l(13), 102), #Int(l(14), 103)]->ne,
        [#Int(l(15), 100), #Int(l(16), 104), #Int(l(17), 105)]->ne,
      ]->ne,
      nodes: nodes2,
    }
    let nodes3 = [T.Ast.Text("", NoTrim)]
    let case3 = {
      T.Ast2.pats: [
        [#Int(l(18), 10), #Int(l(19), 20), #Int(l(20), 50)]->ne,
        [#Int(l(21), 1), #Int(l(22), 2), #Int(l(23), 100)]->ne,
        [#Int(l(24), 1), #Int(l(25), 2), #Int(l(26), 101)]->ne,
        [#Int(l(27), 100), #Int(l(28), 102), #Int(l(29), 106)]->ne,
      ]->ne,
      nodes: nodes3,
    }
    let nodes4 = [T.Ast.Text("", NoTrim)]
    let case4 = {
      T.Ast2.pats: [[#Binding(l(30), "_"), #Binding(l(31), "_"), #Binding(l(32), "_")]->ne]->ne,
      nodes: nodes4,
    }
    let result =
      Matching.make(ne([case1, case2, case3, case4]), ~loc=Loc(0)).tree->Matching.ParMatch.check
    expect.value(result).toEqual(
      Ok(
        Switch({
          idx: 0,
          key: "",
          names: SetString.empty,
          cases: {
            val: TPat_Int(1),
            ifMatch: Switch({
              idx: 1,
              key: "",
              names: SetString.empty,
              cases: {
                val: TPat_Int(2),
                ifMatch: Switch({
                  idx: 2,
                  key: "",
                  names: SetString.empty,
                  cases: {
                    val: TPat_Int(3),
                    ifMatch: End({names: SetString.empty, exit: 0}),
                    nextCase: Some({
                      val: TPat_Int(100),
                      ifMatch: End({names: SetString.empty, exit: 2}),
                      nextCase: Some({
                        val: TPat_Int(101),
                        ifMatch: End({names: SetString.empty, exit: 2}),
                        nextCase: None,
                      }),
                    }),
                  },
                  wildcard: Some(End({names: SetString.empty, exit: 3})),
                }),
                nextCase: Some({
                  val: TPat_Int(4),
                  ifMatch: Switch({
                    idx: 2,
                    key: "",
                    names: SetString.empty,
                    cases: {
                      val: TPat_Int(5),
                      ifMatch: End({names: SetString.empty, exit: 0}),
                      nextCase: None,
                    },
                    wildcard: Some(End({names: SetString.empty, exit: 3})),
                  }),
                  nextCase: None,
                }),
              },
              wildcard: Some(
                Wildcard({
                  idx: 2,
                  key: "",
                  names: SetString.empty,
                  child: End({names: SetString.empty, exit: 3}),
                }),
              ),
            }),
            nextCase: Some({
              val: TPat_Int(10),
              ifMatch: Switch({
                idx: 1,
                key: "",
                names: SetString.empty,
                cases: {
                  val: TPat_Int(20),
                  ifMatch: Switch({
                    idx: 2,
                    key: "",
                    names: SetString.empty,
                    cases: {
                      val: TPat_Int(30),
                      ifMatch: End({names: SetString.empty, exit: 0}),
                      nextCase: Some({
                        val: TPat_Int(40),
                        ifMatch: End({names: SetString.empty, exit: 0}),
                        nextCase: Some({
                          val: TPat_Int(50),
                          ifMatch: End({names: SetString.empty, exit: 2}),
                          nextCase: None,
                        }),
                      }),
                    },
                    wildcard: Some(End({names: SetString.empty, exit: 3})),
                  }),
                  nextCase: None,
                },
                wildcard: Some(
                  Wildcard({
                    idx: 2,
                    key: "",
                    names: SetString.empty,
                    child: End({names: SetString.empty, exit: 3}),
                  }),
                ),
              }),
              nextCase: Some({
                val: TPat_Int(100),
                ifMatch: Switch({
                  idx: 1,
                  key: "",
                  names: SetString.empty,
                  cases: {
                    val: TPat_Int(102),
                    ifMatch: Switch({
                      idx: 2,
                      key: "",
                      names: SetString.empty,
                      cases: {
                        val: TPat_Int(103),
                        ifMatch: End({names: SetString.empty, exit: 1}),
                        nextCase: Some({
                          val: TPat_Int(106),
                          ifMatch: End({names: SetString.empty, exit: 2}),
                          nextCase: None,
                        }),
                      },
                      wildcard: Some(End({names: SetString.empty, exit: 3})),
                    }),
                    nextCase: Some({
                      val: TPat_Int(104),
                      ifMatch: Switch({
                        idx: 2,
                        key: "",
                        names: SetString.empty,
                        cases: {
                          val: TPat_Int(105),
                          ifMatch: End({names: SetString.empty, exit: 1}),
                          nextCase: None,
                        },
                        wildcard: Some(End({names: SetString.empty, exit: 3})),
                      }),
                      nextCase: None,
                    }),
                  },
                  wildcard: Some(
                    Wildcard({
                      idx: 2,
                      key: "",
                      names: SetString.empty,
                      child: End({names: SetString.empty, exit: 3}),
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
              names: SetString.empty,
              child: Wildcard({
                idx: 2,
                key: "",
                names: SetString.empty,
                child: End({names: SetString.empty, exit: 3}),
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
      T.Ast2.pats: [
        [#Int(l(0), 10), #Int(l(1), 11), #Int(l(2), 12)]->ne,
        [#Binding(l(3), "x"), #Int(l(4), 21), #Int(l(5), 22)]->ne,
        [#Int(l(6), 10), #Int(l(7), 11), #Int(l(8), 12)]->ne, // unused
        [#Int(l(9), 30), #Int(l(10), 31), #Int(l(11), 32)]->ne,
        [#Int(l(12), 30), #Binding(l(13), "y"), #Int(l(14), 42)]->ne,
        [#Int(l(15), 30), #Int(l(16), 31), #Int(l(17), 42)]->ne, // unused
        [#Binding(l(18), "a"), #Binding(l(19), "b"), #Binding(l(20), "c")]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let result = Matching.make(ne([case1]), ~loc=Loc(0)).tree->Matching.ParMatch.check
    expect.value(result).toEqual(
      Ok(
        Switch({
          idx: 0,
          key: "",
          names: SetString.fromArray(["a", "x"]),
          cases: {
            val: TPat_Int(10),
            ifMatch: Switch({
              idx: 1,
              key: "",
              names: SetString.fromArray(["b"]),
              cases: {
                val: TPat_Int(11),
                ifMatch: Switch({
                  idx: 2,
                  key: "",
                  names: SetString.fromArray(["c"]),
                  cases: {
                    val: TPat_Int(12),
                    ifMatch: End({names: SetString.empty, exit: 0}),
                    nextCase: None,
                  },
                  wildcard: Some(
                    End({
                      exit: 0,
                      names: SetString.empty
                      ->SetString.add("a")
                      ->SetString.add("b")
                      ->SetString.add("c"),
                    }),
                  ),
                }),
                nextCase: Some({
                  val: TPat_Int(21),
                  ifMatch: Switch({
                    idx: 2,
                    key: "",
                    names: SetString.fromArray(["c"]),
                    cases: {
                      val: TPat_Int(22),
                      ifMatch: End({names: SetString.fromArray(["x"]), exit: 0}),
                      nextCase: None,
                    },
                    wildcard: Some(
                      End({
                        exit: 0,
                        names: SetString.empty
                        ->SetString.add("a")
                        ->SetString.add("b")
                        ->SetString.add("c"),
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
                  names: SetString.fromArray(["c"]),
                  child: End({
                    exit: 0,
                    names: SetString.empty
                    ->SetString.add("a")
                    ->SetString.add("b")
                    ->SetString.add("c"),
                  }),
                }),
              ),
            }),
            nextCase: Some({
              val: TPat_Int(30),
              ifMatch: Switch({
                idx: 1,
                key: "",
                names: SetString.empty->SetString.add("y")->SetString.add("b"),
                cases: {
                  val: TPat_Int(21),
                  ifMatch: Switch({
                    idx: 2,
                    key: "",
                    names: SetString.fromArray(["c"]),
                    cases: {
                      val: TPat_Int(22),
                      ifMatch: End({names: SetString.fromArray(["x"]), exit: 0}),
                      nextCase: Some({
                        val: TPat_Int(42),
                        ifMatch: End({names: SetString.fromArray(["y"]), exit: 0}),
                        nextCase: None,
                      }),
                    },
                    wildcard: Some(
                      End({
                        exit: 0,
                        names: SetString.empty
                        ->SetString.add("a")
                        ->SetString.add("b")
                        ->SetString.add("c"),
                      }),
                    ),
                  }),
                  nextCase: Some({
                    val: TPat_Int(31),
                    ifMatch: Switch({
                      idx: 2,
                      key: "",
                      names: SetString.fromArray(["c"]),
                      cases: {
                        val: TPat_Int(32),
                        ifMatch: End({names: SetString.empty, exit: 0}),
                        nextCase: Some({
                          val: TPat_Int(42),
                          ifMatch: End({names: SetString.fromArray(["y"]), exit: 0}),
                          nextCase: None,
                        }),
                      },
                      wildcard: Some(
                        End({
                          exit: 0,
                          names: SetString.empty
                          ->SetString.add("a")
                          ->SetString.add("b")
                          ->SetString.add("c"),
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
                    names: SetString.fromArray(["c"]),
                    cases: {
                      val: TPat_Int(42),
                      ifMatch: End({names: SetString.fromArray(["y"]), exit: 0}),
                      nextCase: None,
                    },
                    wildcard: Some(
                      End({
                        exit: 0,
                        names: SetString.empty
                        ->SetString.add("a")
                        ->SetString.add("b")
                        ->SetString.add("c"),
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
              names: SetString.fromArray(["b"]),
              cases: {
                val: TPat_Int(21),
                ifMatch: Switch({
                  idx: 2,
                  key: "",
                  names: SetString.fromArray(["c"]),
                  cases: {
                    val: TPat_Int(22),
                    ifMatch: End({names: SetString.fromArray(["x"]), exit: 0}),
                    nextCase: None,
                  },
                  wildcard: Some(
                    End({
                      exit: 0,
                      names: SetString.empty
                      ->SetString.add("a")
                      ->SetString.add("b")
                      ->SetString.add("c"),
                    }),
                  ),
                }),
                nextCase: None,
              },
              wildcard: Some(
                Wildcard({
                  idx: 2,
                  key: "",
                  names: SetString.fromArray(["c"]),
                  child: End({
                    exit: 0,
                    names: SetString.empty
                    ->SetString.add("a")
                    ->SetString.add("b")
                    ->SetString.add("c"),
                  }),
                }),
              ),
            }),
          ),
        }),
      ),
    )
  })

  test("dec tree tuple", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let case1 = {
      T.Ast2.pats: [
        [#Tuple(l(0), [#Int(l(1), 10), #Int(l(2), 12)]), #Int(l(3), 13)]->ne,
        [#Tuple(l(4), [#Int(l(5), 10), #Int(l(6), 22)]), #Int(l(7), 23)]->ne,
        [#Binding(l(8), "_"), #Int(l(9), 33)]->ne,
        [#Binding(l(10), "_"), #Binding(l(11), "_")]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let result = Matching.make(ne([case1]), ~loc=Loc(0)).tree->Matching.ParMatch.check
    expect.value(result).toEqual(
      Ok(
        Nest({
          idx: 0,
          key: "",
          names: SetString.empty,
          kind: Tuple,
          child: Switch({
            idx: 0,
            key: "",
            names: SetString.empty,
            cases: {
              val: TPat_Int(10),
              ifMatch: Switch({
                idx: 1,
                key: "",
                names: SetString.empty,
                cases: {
                  val: TPat_Int(12),
                  ifMatch: End(
                    Switch({
                      idx: 1,
                      key: "",
                      names: SetString.empty,
                      cases: {
                        val: TPat_Int(13),
                        ifMatch: End({names: SetString.empty, exit: 0}),
                        nextCase: Some({
                          val: TPat_Int(33),
                          ifMatch: End({names: SetString.empty, exit: 0}),
                          nextCase: None,
                        }),
                      },
                      wildcard: Some(End({names: SetString.empty, exit: 0})),
                    }),
                  ),
                  nextCase: Some({
                    val: TPat_Int(22),
                    ifMatch: End(
                      Switch({
                        idx: 1,
                        key: "",
                        names: SetString.empty,
                        cases: {
                          val: TPat_Int(23),
                          ifMatch: End({names: SetString.empty, exit: 0}),
                          nextCase: Some({
                            val: TPat_Int(33),
                            ifMatch: End({names: SetString.empty, exit: 0}),
                            nextCase: None,
                          }),
                        },
                        wildcard: Some(End({names: SetString.empty, exit: 0})),
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
              names: SetString.empty,
              cases: {
                val: TPat_Int(33),
                ifMatch: End({names: SetString.empty, exit: 0}),
                nextCase: None,
              },
              wildcard: Some(End({names: SetString.empty, exit: 0})),
            }),
          ),
        }),
      ),
    )
  })

  test("Nests merge into wildcards correctly.", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let nodes2 = [T.Ast.Text("", NoTrim)]
    let nodes3 = [T.Ast.Text("", NoTrim)]
    let nodes4 = [T.Ast.Text("", NoTrim)]
    let case1 = {
      T.Ast2.pats: [[#Binding(l(0), "x"), #Int(l(1), 1)]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      T.Ast2.pats: [
        [#Tuple(l(2), [#String(l(3), "a"), #String(l(4), "b")]), #Int(l(5), 10)]->ne,
      ]->ne,
      nodes: nodes2,
    }
    let case3 = {
      T.Ast2.pats: [
        [#Tuple(l(6), [#String(l(7), "a"), #String(l(8), "b")]), #Int(l(9), 1)]->ne,
      ]->ne,
      nodes: nodes3,
    }
    let case4 = {
      T.Ast2.pats: [
        [#Tuple(l(10), [#Binding(l(11), "_"), #Binding(l(12), "_")]), #Binding(l(13), "_")]->ne,
      ]->ne,
      nodes: nodes4,
    }
    let result =
      Matching.make(ne([case1, case2, case3, case4]), ~loc=Loc(0)).tree->Matching.ParMatch.check
    expect.value(result).toEqual(
      Ok(
        Nest({
          idx: 0,
          key: "",
          names: SetString.fromArray(["x"]),
          kind: Tuple,
          child: Switch({
            idx: 0,
            key: "",
            names: SetString.empty,
            cases: {
              val: TPat_String("a"),
              ifMatch: Switch({
                idx: 1,
                key: "",
                names: SetString.empty,
                cases: {
                  val: TPat_String("b"),
                  ifMatch: End(
                    Switch({
                      idx: 1,
                      key: "",
                      names: SetString.empty,
                      cases: {
                        val: TPat_Int(1),
                        ifMatch: End({names: SetString.fromArray(["x"]), exit: 0}),
                        nextCase: Some({
                          val: TPat_Int(10),
                          ifMatch: End({names: SetString.empty, exit: 1}),
                          nextCase: None,
                        }),
                      },
                      wildcard: Some(End({names: SetString.empty, exit: 3})),
                    }),
                  ),
                  nextCase: None,
                },
                wildcard: Some(
                  End(
                    Switch({
                      idx: 1,
                      key: "",
                      names: SetString.empty,
                      cases: {
                        val: TPat_Int(1),
                        ifMatch: End({names: SetString.fromArray(["x"]), exit: 0}),
                        nextCase: None,
                      },
                      wildcard: Some(End({names: SetString.empty, exit: 3})),
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
                names: SetString.empty,
                child: End(
                  Switch({
                    idx: 1,
                    key: "",
                    names: SetString.empty,
                    cases: {
                      val: TPat_Int(1),
                      ifMatch: End({names: SetString.fromArray(["x"]), exit: 0}),
                      nextCase: None,
                    },
                    wildcard: Some(End({names: SetString.empty, exit: 3})),
                  }),
                ),
              }),
            ),
          }),
          wildcard: Some(
            Switch({
              idx: 1,
              key: "",
              names: SetString.empty,
              cases: {
                val: TPat_Int(1),
                ifMatch: End({names: SetString.fromArray(["x"]), exit: 0}),
                nextCase: None,
              },
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
      T.Ast2.pats: [[#Array(l(0), [#Int(l(2), 10), #Int(l(3), 11)]), #Int(l(4), 12)]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      T.Ast2.pats: [
        [
          #ArrayWithTailBinding(l(5), [#Int(l(6), 10), #Int(l(7), 11)], #Binding(l(8), "x")),
          #Int(l(9), 22),
        ]->ne,
      ]->ne,
      nodes: nodes2,
    }
    let case3 = {
      T.Ast2.pats: [[#Array(l(10), [#Int(l(11), 30)]), #Int(l(12), 32)]->ne]->ne,
      nodes: nodes3,
    }
    let case4 = {
      T.Ast2.pats: [[#Binding(l(13), "y"), #Int(l(14), 42)]->ne]->ne,
      nodes: nodes4,
    }
    let case5 = {
      T.Ast2.pats: [[#Binding(l(15), "_"), #Binding(l(16), "_")]->ne]->ne,
      nodes: nodes5,
    }
    let result =
      Matching.make(
        ne([case1, case2, case3, case4, case5]),
        ~loc=Loc(0),
      ).tree->Matching.ParMatch.check
    expect.value(result).toEqual(
      Ok(
        Construct({
          idx: 0,
          key: "",
          names: SetString.fromArray(["y"]),
          kind: TPat_List,
          cons: Some(
            Nest({
              idx: 0,
              key: "",
              names: SetString.fromArray(["y"]),
              kind: Tuple,
              child: Switch({
                idx: 0,
                key: "",
                names: SetString.empty,
                cases: {
                  val: TPat_Int(10),
                  ifMatch: Construct({
                    idx: 1,
                    key: "",
                    names: SetString.empty,
                    kind: TPat_List,
                    cons: Some(
                      Nest({
                        idx: 1,
                        key: "",
                        names: SetString.empty,
                        kind: Tuple,
                        child: Switch({
                          idx: 0,
                          key: "",
                          names: SetString.empty,
                          cases: {
                            val: TPat_Int(11),
                            ifMatch: Construct({
                              idx: 1,
                              key: "",
                              names: SetString.fromArray(["x"]),
                              kind: TPat_List,
                              nil: Some(
                                End(
                                  End(
                                    Switch({
                                      idx: 1,
                                      key: "",
                                      names: SetString.empty,
                                      cases: {
                                        val: TPat_Int(12),
                                        ifMatch: End({names: SetString.empty, exit: 0}),
                                        nextCase: Some({
                                          val: TPat_Int(22),
                                          ifMatch: End({
                                            names: SetString.fromArray(["x"]),
                                            exit: 1,
                                          }),
                                          nextCase: Some({
                                            val: TPat_Int(42),
                                            ifMatch: End({
                                              names: SetString.fromArray(["y"]),
                                              exit: 3,
                                            }),
                                            nextCase: None,
                                          }),
                                        }),
                                      },
                                      wildcard: Some(End({names: SetString.empty, exit: 4})),
                                    }),
                                  ),
                                ),
                              ),
                              cons: Some(
                                Wildcard({
                                  idx: 1,
                                  key: "",
                                  names: SetString.fromArray(["x"]),
                                  child: End(
                                    End(
                                      Switch({
                                        idx: 1,
                                        key: "",
                                        names: SetString.empty,
                                        cases: {
                                          val: TPat_Int(22),
                                          ifMatch: End({
                                            names: SetString.fromArray(["x"]),
                                            exit: 1,
                                          }),
                                          nextCase: Some({
                                            val: TPat_Int(42),
                                            ifMatch: End({
                                              names: SetString.fromArray(["y"]),
                                              exit: 3,
                                            }),
                                            nextCase: None,
                                          }),
                                        },
                                        wildcard: Some(End({names: SetString.empty, exit: 4})),
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
                      names: SetString.empty,
                      kind: TPat_List,
                      nil: Some(
                        End(
                          Switch({
                            idx: 1,
                            key: "",
                            names: SetString.empty,
                            cases: {
                              val: TPat_Int(32),
                              ifMatch: End({names: SetString.empty, exit: 2}),
                              nextCase: Some({
                                val: TPat_Int(42),
                                ifMatch: End({names: SetString.fromArray(["y"]), exit: 3}),
                                nextCase: None,
                              }),
                            },
                            wildcard: Some(End({names: SetString.empty, exit: 4})),
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
                  names: SetString.empty,
                  cases: {
                    val: TPat_Int(42),
                    ifMatch: End({names: SetString.fromArray(["y"]), exit: 3}),
                    nextCase: None,
                  },
                  wildcard: Some(End({names: SetString.empty, exit: 4})),
                }),
              ),
            }),
          ),
          nil: Some(
            Switch({
              idx: 1,
              key: "",
              names: SetString.empty,
              cases: {
                val: TPat_Int(42),
                ifMatch: End({names: SetString.fromArray(["y"]), exit: 3}),
                nextCase: None,
              },
              wildcard: Some(End({names: SetString.empty, exit: 4})),
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
      T.Ast2.pats: [
        [#Object(l, [("a", #Int(l, 10)), ("b", #Int(l, 11))]), #Int(l, 12)]->ne,
        [#Object(l, [("b", #Int(l, 21)), ("a", #Int(l, 20))]), #Int(l, 22)]->ne,
        [#Binding(l, "_"), #Binding(l, "_")]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let result = Matching.make(ne([case1]), ~loc=Loc(0)).tree->Matching.ParMatch.check
    expect.value(result).toEqual(
      Ok(
        Nest({
          idx: 0,
          key: "",
          names: SetString.empty,
          kind: Record,
          child: Switch({
            idx: 0,
            key: "a",
            names: SetString.empty,
            cases: {
              val: TPat_Int(10),
              ifMatch: Switch({
                idx: 1,
                key: "b",
                names: SetString.empty,
                cases: {
                  val: TPat_Int(11),
                  ifMatch: End(
                    Switch({
                      idx: 1,
                      key: "",
                      names: SetString.empty,
                      cases: {
                        val: TPat_Int(12),
                        ifMatch: End({names: SetString.empty, exit: 0}),
                        nextCase: None,
                      },
                      wildcard: Some(End({names: SetString.empty, exit: 0})),
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
                  names: SetString.empty,
                  cases: {
                    val: TPat_Int(21),
                    ifMatch: End(
                      Switch({
                        idx: 1,
                        key: "",
                        names: SetString.empty,
                        cases: {
                          val: TPat_Int(22),
                          ifMatch: End({names: SetString.empty, exit: 0}),
                          nextCase: None,
                        },
                        wildcard: Some(End({names: SetString.empty, exit: 0})),
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
              names: SetString.empty,
              child: End({names: SetString.empty, exit: 0}),
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
      T.Ast2.pats: [
        [#Object(l, [("a", #Int(l, 10)), ("b", #Int(l, 11))]), #Int(l, 12)]->ne,
        [#Object(l, [("b", #Int(l, 21))]), #Int(l, 22)]->ne,
        [#Binding(l, "_"), #Binding(l, "_")]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let result = Matching.make(ne([case1]), ~loc=Loc(0)).tree->Matching.ParMatch.check
    expect.value(result).toEqual(
      Ok(
        Nest({
          idx: 0,
          key: "",
          names: SetString.empty,
          kind: Record,
          child: Switch({
            idx: 0,
            key: "a",
            names: SetString.empty,
            cases: {
              val: TPat_Int(10),
              ifMatch: Switch({
                idx: 1,
                key: "b",
                names: SetString.empty,
                cases: {
                  val: TPat_Int(11),
                  ifMatch: End(
                    Switch({
                      idx: 1,
                      key: "",
                      names: SetString.empty,
                      cases: {
                        val: TPat_Int(12),
                        ifMatch: End({names: SetString.empty, exit: 0}),
                        nextCase: None,
                      },
                      wildcard: Some(End({names: SetString.empty, exit: 0})),
                    }),
                  ),
                  nextCase: Some({
                    val: TPat_Int(21),
                    ifMatch: End(
                      Switch({
                        idx: 1,
                        key: "",
                        names: SetString.empty,
                        cases: {
                          val: TPat_Int(22),
                          ifMatch: End({names: SetString.empty, exit: 0}),
                          nextCase: None,
                        },
                        wildcard: Some(End({names: SetString.empty, exit: 0})),
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
                names: SetString.empty,
                cases: {
                  val: TPat_Int(21),
                  ifMatch: End(
                    Switch({
                      idx: 1,
                      key: "",
                      names: SetString.empty,
                      cases: {
                        val: TPat_Int(22),
                        ifMatch: End({names: SetString.empty, exit: 0}),
                        nextCase: None,
                      },
                      wildcard: Some(End({names: SetString.empty, exit: 0})),
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
              names: SetString.empty,
              child: End({names: SetString.empty, exit: 0}),
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
    let case1: T.Ast2.case<_> = {
      pats: [[#Object(l(0), [("b", #Int(l(1), 10))])]->ne]->ne,
      nodes: nodes1,
    }
    let case2: T.Ast2.case<_> = {
      pats: [[#Object(l(2), [("a", #Int(l(3), 20))])]->ne]->ne,
      nodes: nodes2,
    }
    let case3: T.Ast2.case<_> = {
      pats: [[#Object(l(4), [("c", #Int(l(5), 30))])]->ne]->ne,
      nodes: nodes3,
    }
    let case4: T.Ast2.case<_> = {
      pats: [[#Binding(l(6), "x")]->ne]->ne,
      nodes: nodes4,
    }
    let result =
      Matching.make(ne([case1, case2, case3, case4]), ~loc=Loc(0)).tree->Matching.ParMatch.check
    expect.value(result).toEqual(
      Ok(
        Nest({
          idx: 0,
          key: "",
          names: SetString.fromArray(["x"]),
          kind: Record,
          child: Switch({
            idx: 0,
            key: "a",
            names: SetString.empty,
            cases: {
              val: TPat_Int(20),
              ifMatch: Switch({
                idx: 1,
                key: "b",
                names: SetString.empty,
                cases: {
                  val: TPat_Int(10),
                  ifMatch: Wildcard({
                    idx: 2,
                    key: "c",
                    names: SetString.empty,
                    child: End(End({names: SetString.empty, exit: 0})),
                  }),
                  nextCase: None,
                },
                wildcard: Some(
                  Wildcard({
                    idx: 2,
                    key: "c",
                    names: SetString.empty,
                    child: End(End({names: SetString.empty, exit: 1})),
                  }),
                ),
              }),
              nextCase: None,
            },
            wildcard: Some(
              Switch({
                idx: 1,
                key: "b",
                names: SetString.empty,
                cases: {
                  val: TPat_Int(10),
                  ifMatch: Wildcard({
                    idx: 2,
                    key: "c",
                    names: SetString.empty,
                    child: End(End({names: SetString.empty, exit: 0})),
                  }),
                  nextCase: None,
                },
                wildcard: Some(
                  Switch({
                    idx: 2,
                    key: "c",
                    names: SetString.empty,
                    cases: {
                      val: TPat_Int(30),
                      ifMatch: End(End({names: SetString.empty, exit: 2})),
                      nextCase: None,
                    },
                    wildcard: None,
                  }),
                ),
              }),
            ),
          }),
          wildcard: Some(End({names: SetString.fromArray(["x"]), exit: 3})),
        }),
      ),
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
    let l = l => T.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let nodes2 = [T.Ast.Text("", NoTrim)]
    let case1: T.Ast2.case<_, _> = {
      pats: [
        [#Int(l(0), 0)]->ne,
        [#Int(l(3), 10)]->ne,
        [#Int(l(3), 20)]->ne,
        [#Int(l(3), 30)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let case2: T.Ast2.case<_, _> = {
      pats: [[#Int(l(0), 15)]->ne]->ne,
      nodes: nodes2,
    }
    let result =
      Matching.ParMatch.check(Matching.make(ne([case1, case2]), ~loc=Loc(0)).tree)->getError
    expect.value(result).toEqual(
      Some(`This pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
1`),
    )
    let case1 = {
      T.Ast2.pats: [[#Array(l(0), [])]->ne, [#Array(l(0), [#Binding(l(1), "_")])]->ne]->ne,
      nodes: nodes1,
    }
    let result = Matching.ParMatch.check(Matching.make(ne([case1]), ~loc=Loc(0)).tree)->getError
    expect.value(result).toEqual(
      Some(`This pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[_, ..._]`),
    )
    let case1 = {
      T.Ast2.pats: [[#Array(l(0), [#Binding(l(1), "_")])]->ne]->ne,
      nodes: nodes1,
    }
    let result = Matching.ParMatch.check(Matching.make(ne([case1]), ~loc=Loc(0)).tree)->getError
    expect.value(result).toEqual(
      Some(`This pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[]`),
    )
    let case1 = {
      T.Ast2.pats: [[#Object(l(0), [("b", #Int(l(1), 10))])]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      T.Ast2.pats: [[#Object(l(2), [("a", #Int(l(3), 20))])]->ne]->ne,
      nodes: nodes2,
    }
    let result =
      Matching.ParMatch.check(Matching.make(ne([case1, case2]), ~loc=Loc(0)).tree)->getError
    expect.value(result).toEqual(
      Some(`This pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{a, b: 0}`),
    )
  })
})
