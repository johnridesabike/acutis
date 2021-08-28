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
module T = Acutis_Types
module TC = TypeChecker
module NE = TC.NonEmpty2
let ne = NE.fromArrayUnsafe

describe("Decision tree", ({test, _}) => {
  test("Basic dec tree 1", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1: T.Ast.nodes<_> = [Text("nodes1", NoTrim)]
    let case1: TC.Ast2.case<_, _> = {
      pats: [
        [#Int(l(0), 1), #Int(l(1), 2), #Int(l(2), 3)]->ne,
        [#Int(l(3), 1), #Int(l(4), 4), #Int(l(5), 5)]->ne,
        [#Int(l(6), 10), #Int(l(7), 20), #Int(l(8), 30)]->ne,
        [#Int(l(9), 10), #Int(l(10), 20), #Int(l(11), 40)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let nodes2: T.Ast.nodes<_> = [Text("nodes2", NoTrim)]
    let case2: TC.Ast2.case<_, _> = {
      pats: [
        [#Int(l(12), 100), #Int(l(13), 102), #Int(l(14), 103)]->ne,
        [#Int(l(15), 100), #Int(l(16), 104), #Int(l(17), 105)]->ne,
      ]->ne,
      nodes: nodes2,
    }
    let nodes3: T.Ast.nodes<_> = [Text("nodes3", NoTrim)]
    let case3: TC.Ast2.case<_, _> = {
      pats: [
        [#Int(l(18), 10), #Int(l(19), 20), #Int(l(20), 50)]->ne,
        [#Int(l(21), 1), #Int(l(22), 2), #Int(l(23), 100)]->ne,
        [#Int(l(24), 1), #Int(l(25), 2), #Int(l(26), 101)]->ne,
        [#Int(l(27), 100), #Int(l(28), 102), #Int(l(29), 106)]->ne,
      ]->ne,
      nodes: nodes3,
    }
    let result = Matching.make2(ne([case1, case2, case3])).tree
    expect.value(result).toEqual(
      Switch({
        key: "",
        names: [],
        wildcard: None,
        cases: {
          val: TInt(1),
          ifMatch: Switch({
            key: "",
            names: [],
            wildcard: None,
            cases: {
              val: TInt(2),
              ifMatch: Switch({
                key: "",
                names: [],
                wildcard: None,
                cases: {
                  val: TInt(3),
                  ifMatch: Leaf({bindings: [], exit: 0}),
                  nextCase: Some({
                    val: TInt(100),
                    ifMatch: Leaf({bindings: [], exit: 2}),
                    nextCase: Some({
                      val: TInt(101),
                      ifMatch: Leaf({bindings: [], exit: 2}),
                      nextCase: None,
                    }),
                  }),
                },
              }),
              nextCase: Some({
                val: TInt(4),
                ifMatch: Switch({
                  key: "",
                  names: [],
                  wildcard: None,
                  cases: {
                    val: TInt(5),
                    ifMatch: Leaf({bindings: [], exit: 0}),
                    nextCase: None,
                  },
                }),
                nextCase: None,
              }),
            },
          }),
          nextCase: Some({
            val: TInt(10),
            ifMatch: Switch({
              key: "",
              names: [],
              wildcard: None,
              cases: {
                val: TInt(20),
                ifMatch: Switch({
                  key: "",
                  names: [],
                  wildcard: None,
                  cases: {
                    val: TInt(30),
                    ifMatch: Leaf({bindings: [], exit: 0}),
                    nextCase: Some({
                      val: TInt(40),
                      ifMatch: Leaf({bindings: [], exit: 0}),
                      nextCase: Some({
                        val: TInt(50),
                        ifMatch: Leaf({bindings: [], exit: 2}),
                        nextCase: None,
                      }),
                    }),
                  },
                }),
                nextCase: None,
              },
            }),
            nextCase: Some({
              val: TInt(100),
              ifMatch: Switch({
                key: "",
                names: [],
                wildcard: None,
                cases: {
                  val: TInt(102),
                  ifMatch: Switch({
                    key: "",
                    names: [],
                    wildcard: None,
                    cases: {
                      val: TInt(103),
                      ifMatch: Leaf({bindings: [], exit: 1}),
                      nextCase: Some({
                        val: TInt(106),
                        ifMatch: Leaf({bindings: [], exit: 2}),
                        nextCase: None,
                      }),
                    },
                  }),
                  nextCase: Some({
                    val: TInt(104),
                    ifMatch: Switch({
                      key: "",
                      names: [],
                      wildcard: None,
                      cases: {
                        val: TInt(105),
                        ifMatch: Leaf({bindings: [], exit: 1}),
                        nextCase: None,
                      },
                    }),
                    nextCase: None,
                  }),
                },
              }),
              nextCase: None,
            }),
          }),
        },
      }),
    )
  })

  test("Basic dec tree 2", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1: T.Ast.nodes<_> = [Text("a", NoTrim)]
    let case1: TC.Ast2.case<_, _> = {
      pats: [
        [#Int(l(0), 10), #Int(l(1), 11), #Int(l(2), 12)]->ne,
        [#Binding(l(3), "x"), #Int(l(4), 21), #Int(l(5), 22)]->ne,
        [#Int(l(6), 10), #Int(l(7), 11), #Int(l(8), 12)]->ne,
        [#Int(l(9), 30), #Int(l(10), 31), #Int(l(11), 32)]->ne,
        [#Int(l(12), 30), #Binding(l(13), "y"), #Int(l(14), 42)]->ne,
        [#Int(l(15), 30), #Int(l(16), 31), #Int(l(17), 42)]->ne,
      ]->ne,
      nodes: nodes1,
    }

    let result = Matching.make2(ne([case1])).tree
    expect.value(result).toEqual(
      Switch({
        key: "",
        names: ["x"],
        cases: {
          val: TInt(10),
          ifMatch: Switch({
            key: "",
            names: [],
            wildcard: None,
            cases: {
              val: TInt(11),
              ifMatch: Switch({
                key: "",
                names: [],
                wildcard: None,
                cases: {
                  val: TInt(12),
                  ifMatch: Leaf({bindings: [], exit: 0}),
                  nextCase: None,
                },
              }),
              nextCase: Some({
                val: TInt(21),
                ifMatch: Switch({
                  key: "",
                  names: [],
                  wildcard: None,
                  cases: {
                    val: TInt(22),
                    ifMatch: Leaf({bindings: ["x"], exit: 0}),
                    nextCase: None,
                  },
                }),
                nextCase: None,
              }),
            },
          }),
          nextCase: Some({
            val: TInt(30),
            ifMatch: Switch({
              key: "",
              names: [],
              cases: {
                val: TInt(21),
                ifMatch: Switch({
                  key: "",
                  names: [],
                  wildcard: None,
                  cases: {
                    val: TInt(22),
                    ifMatch: Leaf({bindings: ["x"], exit: 0}),
                    nextCase: Some({
                      val: TInt(42),
                      ifMatch: Leaf({bindings: ["y"], exit: 0}),
                      nextCase: None,
                    }),
                  },
                }),
                nextCase: Some({
                  val: TInt(31),
                  ifMatch: Switch({
                    key: "",
                    names: [],
                    wildcard: None,
                    cases: {
                      val: TInt(32),
                      ifMatch: Leaf({bindings: [], exit: 0}),
                      nextCase: Some({
                        val: TInt(42),
                        ifMatch: Leaf({bindings: ["y"], exit: 0}),
                        nextCase: None,
                      }),
                    },
                  }),
                  nextCase: None,
                }),
              },
              wildcard: Some(
                Switch({
                  key: "",
                  names: [],
                  wildcard: None,
                  cases: {
                    val: TInt(42),
                    ifMatch: Leaf({bindings: ["y"], exit: 0}),
                    nextCase: None,
                  },
                }),
              ),
            }),
            nextCase: None,
          }),
        },
        wildcard: Some(
          Switch({
            key: "",
            names: [],
            wildcard: None,
            cases: {
              val: TInt(21),
              ifMatch: Switch({
                key: "",
                names: [],
                wildcard: None,
                cases: {
                  val: TInt(22),
                  ifMatch: Leaf({bindings: ["x"], exit: 0}),
                  nextCase: None,
                },
              }),
              nextCase: None,
            },
          }),
        ),
      }),
    )
  })

  test("dec tree tuple", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1: T.Ast.nodes<_> = [Text("a", NoTrim)]
    let case1: TC.Ast2.case<_, _> = {
      pats: [
        [#Tuple(l(0), [#Int(l(1), 10), #Int(l(2), 12)]), #Int(l(3), 13)]->ne,
        [#Tuple(l(4), [#Int(l(5), 10), #Int(l(6), 22)]), #Int(l(7), 23)]->ne,
        [#Binding(l(8), "_"), #Int(l(9), 33)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let result = Matching.make2(ne([case1])).tree
    expect.value(result).toEqual(
      Nest({
        key: "",
        names: [],
        kind: TTuple,
        child: Switch({
          key: "",
          names: [],
          cases: {
            val: TInt(10),
            ifMatch: Switch({
              key: "",
              names: [],
              cases: {
                val: TInt(12),
                ifMatch: End(
                  Switch({
                    key: "",
                    names: [],
                    cases: {
                      val: TInt(13),
                      ifMatch: Leaf({bindings: [], exit: 0}),
                      nextCase: Some({
                        val: TInt(33),
                        ifMatch: Leaf({bindings: [], exit: 0}),
                        nextCase: None,
                      }),
                    },
                    wildcard: None,
                  }),
                ),
                nextCase: Some({
                  val: TInt(22),
                  ifMatch: End(
                    Switch({
                      key: "",
                      names: [],
                      cases: {
                        val: TInt(23),
                        ifMatch: Leaf({bindings: [], exit: 0}),
                        nextCase: Some({
                          val: TInt(33),
                          ifMatch: Leaf({bindings: [], exit: 0}),
                          nextCase: None,
                        }),
                      },
                      wildcard: None,
                    }),
                  ),
                  nextCase: None,
                }),
              },
              wildcard: Some(
                End(
                  Switch({
                    key: "",
                    names: [],
                    cases: {
                      val: TInt(33),
                      ifMatch: Leaf({bindings: [], exit: 0}),
                      nextCase: None,
                    },
                    wildcard: None,
                  }),
                ),
              ),
            }),
            nextCase: None,
          },
          wildcard: Some(
            End(
              Switch({
                key: "",
                names: [],
                cases: {
                  val: TInt(33),
                  ifMatch: Leaf({bindings: [], exit: 0}),
                  nextCase: None,
                },
                wildcard: None,
              }),
            ),
          ),
        }),
      }),
    )
  })

  test("dec tree list", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1: T.Ast.nodes<_> = [Text("a", NoTrim)]
    let nodes2: T.Ast.nodes<_> = [Text("a", NoTrim)]
    let nodes3: T.Ast.nodes<_> = [Text("a", NoTrim)]
    let nodes4: T.Ast.nodes<_> = [Text("a", NoTrim)]
    let case1: TC.Ast2.case<_> = {
      pats: [[#Array(l(0), [#Int(l(2), 10), #Int(l(3), 11)]), #Int(l(4), 12)]->ne]->ne,
      nodes: nodes1,
    }
    let case2: TC.Ast2.case<_> = {
      pats: [
        [
          #ArrayWithTailBinding(l(5), [#Int(l(6), 10), #Int(l(7), 11)], #Binding(l(8), "x")),
          #Int(l(9), 22),
        ]->ne,
      ]->ne,
      nodes: nodes2,
    }
    let case3: TC.Ast2.case<_> = {
      pats: [[#Array(l(10), [#Int(l(11), 30)]), #Int(l(12), 32)]->ne]->ne,
      nodes: nodes3,
    }
    let case4: TC.Ast2.case<_> = {
      pats: [[#Binding(l(13), "y"), #Int(l(14), 42)]->ne]->ne,
      nodes: nodes4,
    }
    let result = Matching.make2(ne([case1, case2, case3, case4])).tree
    expect.value(result).toEqual(
      Variant({
        key: "",
        names: ["y"],
        kind: TPat_List,
        cons: Some(
          Nest({
            key: "",
            names: ["y"],
            kind: TTuple,
            child: Switch({
              key: "",
              names: [],
              cases: {
                val: TInt(10),
                ifMatch: Variant({
                  key: "",
                  names: [],
                  kind: TPat_List,
                  cons: Some(
                    Nest({
                      key: "",
                      names: [],
                      kind: TTuple,
                      child: Switch({
                        key: "",
                        names: [],
                        cases: {
                          val: TInt(11),
                          ifMatch: Variant({
                            key: "",
                            names: ["x"],
                            kind: TPat_List,
                            nil: Some(
                              End(
                                End(
                                  Switch({
                                    key: "",
                                    names: [],
                                    cases: {
                                      val: TInt(12),
                                      ifMatch: Leaf({bindings: [], exit: 0}),
                                      nextCase: Some({
                                        val: TInt(22),
                                        ifMatch: Leaf({bindings: ["x"], exit: 1}),
                                        nextCase: Some({
                                          val: TInt(42),
                                          ifMatch: Leaf({bindings: ["y"], exit: 3}),
                                          nextCase: None,
                                        }),
                                      }),
                                    },
                                    wildcard: None,
                                  }),
                                ),
                              ),
                            ),
                            cons: Some(
                              Wildcard({
                                key: "",
                                names: ["x"],
                                child: End(
                                  End(
                                    Switch({
                                      key: "",
                                      names: [],
                                      cases: {
                                        val: TInt(22),
                                        ifMatch: Leaf({bindings: ["x"], exit: 1}),
                                        nextCase: Some({
                                          val: TInt(42),
                                          ifMatch: Leaf({bindings: ["y"], exit: 3}),
                                          nextCase: None,
                                        }),
                                      },
                                      wildcard: None,
                                    }),
                                  ),
                                ),
                              }),
                            ),
                          }),
                          nextCase: None,
                        },
                        wildcard: Some(
                          End(
                            End(
                              Switch({
                                key: "",
                                names: [],
                                cases: {
                                  val: TInt(42),
                                  ifMatch: Leaf({bindings: ["y"], exit: 3}),
                                  nextCase: None,
                                },
                                wildcard: None,
                              }),
                            ),
                          ),
                        ),
                      }),
                    }),
                  ),
                  nil: Some(
                    Wildcard({
                      key: "",
                      names: [],
                      child: End(
                        Switch({
                          key: "",
                          names: [],
                          cases: {
                            val: TInt(42),
                            ifMatch: Leaf({bindings: ["y"], exit: 3}),
                            nextCase: None,
                          },
                          wildcard: None,
                        }),
                      ),
                    }),
                  ),
                }),
                nextCase: Some({
                  val: TInt(30),
                  ifMatch: Variant({
                    key: "",
                    names: [],
                    kind: TPat_List,
                    nil: Some(
                      End(
                        Switch({
                          key: "",
                          names: [],
                          cases: {
                            val: TInt(32),
                            ifMatch: Leaf({bindings: [], exit: 2}),
                            nextCase: Some({
                              val: TInt(42),
                              ifMatch: Leaf({bindings: ["y"], exit: 3}),
                              nextCase: None,
                            }),
                          },
                          wildcard: None,
                        }),
                      ),
                    ),
                    cons: Some(
                      Wildcard({
                        key: "",
                        names: [],
                        child: End(
                          Switch({
                            key: "",
                            names: [],
                            cases: {
                              val: TInt(42),
                              ifMatch: Leaf({bindings: ["y"], exit: 3}),
                              nextCase: None,
                            },
                            wildcard: None,
                          }),
                        ),
                      }),
                    ),
                  }),
                  nextCase: None,
                }),
              },
              wildcard: Some(
                End(
                  Switch({
                    key: "",
                    names: [],
                    cases: {
                      val: TInt(42),
                      ifMatch: Leaf({bindings: ["y"], exit: 3}),
                      nextCase: None,
                    },
                    wildcard: None,
                  }),
                ),
              ),
            }),
          }),
        ),
        nil: Some(
          Switch({
            key: "",
            names: [],
            cases: {
              val: TInt(42),
              ifMatch: Leaf({bindings: ["y"], exit: 3}),
              nextCase: None,
            },
            wildcard: None,
          }),
        ),
      }),
    )
  })

  test("Records sort fields correctly", ({expect, _}) => {
    let l: T.loc = Loc(0)
    let nodes1: T.Ast.nodes<_> = [Text("a", NoTrim)]
    let case1: TC.Ast2.case<_> = {
      pats: [
        [#Object(l, [("a", #Int(l, 10)), ("b", #Int(l, 11))]), #Int(l, 12)]->ne,
        [#Object(l, [("b", #Int(l, 21)), ("a", #Int(l, 20))]), #Int(l, 22)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let result = Matching.make2(ne([case1])).tree
    expect.value(result).toEqual(
      Nest({
        key: "",
        names: [],
        kind: TRecord,
        child: Switch({
          key: "a",
          names: [],
          cases: {
            val: TInt(10),
            ifMatch: Switch({
              key: "b",
              names: [],
              cases: {
                val: TInt(11),
                ifMatch: End(
                  Switch({
                    key: "",
                    names: [],
                    cases: {
                      val: TInt(12),
                      ifMatch: Leaf({bindings: [], exit: 0}),
                      nextCase: None,
                    },
                    wildcard: None,
                  }),
                ),
                nextCase: None,
              },
              wildcard: None,
            }),
            nextCase: Some({
              val: TInt(20),
              ifMatch: Switch({
                key: "b",
                names: [],
                cases: {
                  val: TInt(21),
                  ifMatch: End(
                    Switch({
                      key: "",
                      names: [],
                      cases: {
                        val: TInt(22),
                        ifMatch: Leaf({bindings: [], exit: 0}),
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
            }),
          },
          wildcard: None,
        }),
      }),
    )
  })

  test("Records: missing fields are automatically wildcards", ({expect, _}) => {
    let l: T.loc = Loc(0)
    let nodes1: T.Ast.nodes<_> = [Text("a", NoTrim)]
    let case1: TC.Ast2.case<_> = {
      pats: [
        [#Object(l, [("a", #Int(l, 10)), ("b", #Int(l, 11))]), #Int(l, 12)]->ne,
        [#Object(l, [("b", #Int(l, 21))]), #Int(l, 22)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let result = Matching.make2(ne([case1])).tree
    expect.value(result).toEqual(
      Nest({
        key: "",
        names: [],
        kind: TRecord,
        child: Switch({
          key: "a",
          names: [],
          cases: {
            val: TInt(10),
            ifMatch: Switch({
              key: "b",
              names: [],
              cases: {
                val: TInt(11),
                ifMatch: End(
                  Switch({
                    key: "",
                    names: [],
                    cases: {
                      val: TInt(12),
                      ifMatch: Leaf({bindings: [], exit: 0}),
                      nextCase: None,
                    },
                    wildcard: None,
                  }),
                ),
                nextCase: Some({
                  val: TInt(21),
                  ifMatch: End(
                    Switch({
                      key: "",
                      names: [],
                      cases: {
                        val: TInt(22),
                        ifMatch: Leaf({bindings: [], exit: 0}),
                        nextCase: None,
                      },
                      wildcard: None,
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
              names: [],
              cases: {
                val: TInt(21),
                ifMatch: End(
                  Switch({
                    key: "",
                    names: [],
                    cases: {
                      val: TInt(22),
                      ifMatch: Leaf({bindings: [], exit: 0}),
                      nextCase: None,
                    },
                    wildcard: None,
                  }),
                ),
                nextCase: None,
              },
              wildcard: None,
            }),
          ),
        }),
      }),
    )
  })

  test("Records: new fields expand existing rows", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1: T.Ast.nodes<_> = [Text("x", NoTrim)]
    let nodes2: T.Ast.nodes<_> = [Text("y", NoTrim)]
    let nodes3: T.Ast.nodes<_> = [Text("z", NoTrim)]
    let nodes4: T.Ast.nodes<_> = [Text("zz", NoTrim)]
    let case1: TC.Ast2.case<_> = {
      pats: [[#Object(l(0), [("b", #Int(l(1), 10))])]->ne]->ne,
      nodes: nodes1,
    }
    let case2: TC.Ast2.case<_> = {
      pats: [[#Object(l(2), [("a", #Int(l(3), 20))])]->ne]->ne,
      nodes: nodes2,
    }
    let case3: TC.Ast2.case<_> = {
      pats: [[#Object(l(4), [("c", #Int(l(5), 30))])]->ne]->ne,
      nodes: nodes3,
    }
    let case4: TC.Ast2.case<_> = {
      pats: [[#Binding(l(6), "x")]->ne]->ne,
      nodes: nodes4,
    }
    let result = Matching.make2(ne([case1, case2, case3, case4])).tree
    expect.value(result).toEqual(
      Nest({
        key: "",
        names: ["x"],
        kind: TRecord,
        child: Switch({
          key: "a",
          names: [],
          cases: {
            val: TInt(20),
            ifMatch: Switch({
              key: "b",
              names: [],
              cases: {
                val: TInt(10),
                ifMatch: Wildcard({key: "c", names: [], child: End(Leaf({bindings: [], exit: 0}))}),
                nextCase: None,
              },
              wildcard: Some(
                Wildcard({key: "c", names: [], child: End(Leaf({bindings: [], exit: 1}))}),
              ),
            }),
            nextCase: None,
          },
          wildcard: Some(
            Switch({
              key: "b",
              names: [],
              cases: {
                val: TInt(10),
                ifMatch: Wildcard({key: "c", names: [], child: End(Leaf({bindings: [], exit: 0}))}),
                nextCase: None,
              },
              wildcard: Some(
                Switch({
                  key: "c",
                  names: [],
                  cases: {
                    val: TInt(30),
                    ifMatch: End(Leaf({bindings: [], exit: 2})),
                    nextCase: None,
                  },
                  wildcard: Some(End(Leaf({bindings: ["x"], exit: 3}))),
                }),
              ),
            }),
          ),
        }),
      }),
    )
  })
})
