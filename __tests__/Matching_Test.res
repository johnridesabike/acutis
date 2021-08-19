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

describe("Decision tree", ({test, _}) => {
  test("Basic dec tree 1", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1: T.Ast.nodes<_> = [Text("nodes1", NoTrim)]
    let case1: T.Ast.case<_> = {
      patterns: NonEmpty(
        NonEmpty(#Int(l(0), 1), [#Int(l(1), 2), #Int(l(2), 3)]),
        [
          NonEmpty(#Int(l(3), 1), [#Int(l(4), 4), #Int(l(5), 5)]),
          NonEmpty(#Int(l(6), 10), [#Int(l(7), 20), #Int(l(8), 30)]),
          NonEmpty(#Int(l(9), 10), [#Int(l(10), 20), #Int(l(11), 40)]),
        ],
      ),
      nodes: nodes1,
    }
    let nodes2: T.Ast.nodes<_> = [Text("nodes2", NoTrim)]
    let case2: T.Ast.case<_> = {
      patterns: NonEmpty(
        NonEmpty(#Int(l(12), 100), [#Int(l(13), 102), #Int(l(14), 103)]),
        [NonEmpty(#Int(l(15), 100), [#Int(l(16), 104), #Int(l(17), 105)])],
      ),
      nodes: nodes2,
    }
    let nodes3: T.Ast.nodes<_> = [Text("nodes3", NoTrim)]
    let case3: T.Ast.case<_> = {
      patterns: NonEmpty(
        NonEmpty(#Int(l(18), 10), [#Int(l(19), 20), #Int(l(20), 50)]),
        [
          NonEmpty(#Int(l(21), 1), [#Int(l(22), 2), #Int(l(23), 100)]),
          NonEmpty(#Int(l(24), 1), [#Int(l(25), 2), #Int(l(26), 101)]),
          NonEmpty(#Int(l(27), 100), [#Int(l(28), 102), #Int(l(29), 106)]),
        ],
      ),
      nodes: nodes3,
    }
    let result = Matching.make(NonEmpty(case1, [case2, case3])).tree
    expect.value(result).toEqual(
      Test({
        key: "",
        ids: [],
        wildcard: None,
        cases: {
          val: TInt(1),
          ifMatch: Test({
            key: "",
            ids: [],
            wildcard: None,
            cases: {
              val: TInt(2),
              ifMatch: Test({
                key: "",
                ids: [],
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
                ifMatch: Test({
                  key: "",
                  ids: [],
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
            ifMatch: Test({
              key: "",
              ids: [],
              wildcard: None,
              cases: {
                val: TInt(20),
                ifMatch: Test({
                  key: "",
                  ids: [],
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
              ifMatch: Test({
                key: "",
                ids: [],
                wildcard: None,
                cases: {
                  val: TInt(102),
                  ifMatch: Test({
                    key: "",
                    ids: [],
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
                    ifMatch: Test({
                      key: "",
                      ids: [],
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
    let case1: T.Ast.case<_> = {
      patterns: NonEmpty(
        NonEmpty(#Int(l(0), 10), [#Int(l(1), 11), #Int(l(2), 12)]),
        [
          NonEmpty(#Binding(l(3), "_"), [#Int(l(4), 21), #Int(l(5), 22)]),
          NonEmpty(#Int(l(6), 10), [#Int(l(7), 11), #Int(l(8), 12)]),
          NonEmpty(#Int(l(9), 30), [#Int(l(10), 31), #Int(l(11), 32)]),
          NonEmpty(#Int(l(12), 30), [#Binding(l(13), "_"), #Int(l(14), 42)]),
          NonEmpty(#Int(l(15), 30), [#Int(l(16), 31), #Int(l(17), 42)]),
        ],
      ),
      nodes: nodes1,
    }

    let result = Matching.make(NonEmpty(case1, [])).tree
    expect.value(result).toEqual(
      Test({
        key: "",
        cases: {
          val: TInt(10),
          ifMatch: Test({
            key: "",
            ids: [],
            wildcard: None,
            cases: {
              val: TInt(11),
              ifMatch: Test({
                key: "",
                ids: [],
                wildcard: None,
                cases: {
                  val: TInt(12),
                  ifMatch: Leaf({bindings: [], exit: 0}),
                  nextCase: None,
                },
              }),
              nextCase: Some({
                val: TInt(21),
                ifMatch: Test({
                  key: "",
                  ids: [],
                  wildcard: None,
                  cases: {
                    val: TInt(22),
                    ifMatch: Leaf({bindings: [3], exit: 0}),
                    nextCase: None,
                  },
                }),
                nextCase: None,
              }),
            },
          }),
          nextCase: Some({
            val: TInt(30),
            ifMatch: Test({
              key: "",
              cases: {
                val: TInt(21),
                ifMatch: Test({
                  key: "",
                  ids: [],
                  wildcard: None,
                  cases: {
                    val: TInt(22),
                    ifMatch: Leaf({bindings: [3], exit: 0}),
                    nextCase: Some({
                      val: TInt(42),
                      ifMatch: Leaf({bindings: [13], exit: 0}),
                      nextCase: None,
                    }),
                  },
                }),
                nextCase: Some({
                  val: TInt(31),
                  ifMatch: Test({
                    key: "",
                    ids: [],
                    wildcard: None,
                    cases: {
                      val: TInt(32),
                      ifMatch: Leaf({bindings: [], exit: 0}),
                      nextCase: Some({
                        val: TInt(42),
                        ifMatch: Leaf({bindings: [13], exit: 0}),
                        nextCase: None,
                      }),
                    },
                  }),
                  nextCase: None,
                }),
              },
              ids: [],
              wildcard: Some(
                Test({
                  key: "",
                  ids: [],
                  wildcard: None,
                  cases: {
                    val: TInt(42),
                    ifMatch: Leaf({bindings: [13], exit: 0}),
                    nextCase: None,
                  },
                }),
              ),
            }),
            nextCase: None,
          }),
        },
        ids: [3],
        wildcard: Some(
          Test({
            key: "",
            ids: [],
            wildcard: None,
            cases: {
              val: TInt(21),
              ifMatch: Test({
                key: "",
                ids: [],
                wildcard: None,
                cases: {
                  val: TInt(22),
                  ifMatch: Leaf({bindings: [3], exit: 0}),
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
    let case1: T.Ast.case<_> = {
      patterns: NonEmpty(
        NonEmpty(#Tuple(l(0), [#Int(l(1), 10), #Int(l(2), 12)]), [#Int(l(3), 13)]),
        [
          NonEmpty(#Tuple(l(4), [#Int(l(5), 10), #Int(l(6), 22)]), [#Int(l(7), 23)]),
          NonEmpty(#Binding(l(8), "_"), [#Int(l(9), 33)]),
        ],
      ),
      nodes: nodes1,
    }
    let result = Matching.make(NonEmpty(case1, [])).tree
    expect.value(result).toEqual(
      Nest({
        key: "",
        ids: [8],
        val: TTuple,
        child: Test({
          key: "",
          cases: {
            val: TInt(10),
            ifMatch: Test({
              key: "",
              cases: {
                val: TInt(12),
                ifMatch: End(
                  Test({
                    key: "",
                    cases: {
                      val: TInt(13),
                      ifMatch: Leaf({bindings: [], exit: 0}),
                      nextCase: Some({
                        val: TInt(33),
                        ifMatch: Leaf({bindings: [8], exit: 0}),
                        nextCase: None,
                      }),
                    },
                    ids: [],
                    wildcard: None,
                  }),
                ),
                nextCase: Some({
                  val: TInt(22),
                  ifMatch: End(
                    Test({
                      key: "",
                      cases: {
                        val: TInt(23),
                        ifMatch: Leaf({bindings: [], exit: 0}),
                        nextCase: Some({
                          val: TInt(33),
                          ifMatch: Leaf({bindings: [8], exit: 0}),
                          nextCase: None,
                        }),
                      },
                      ids: [],
                      wildcard: None,
                    }),
                  ),
                  nextCase: None,
                }),
              },
              ids: [],
              wildcard: Some(
                End(
                  Test({
                    key: "",
                    cases: {
                      val: TInt(33),
                      ifMatch: Leaf({bindings: [8], exit: 0}),
                      nextCase: None,
                    },
                    ids: [],
                    wildcard: None,
                  }),
                ),
              ),
            }),
            nextCase: None,
          },
          ids: [],
          wildcard: Some(
            End(
              Test({
                key: "",
                cases: {
                  val: TInt(33),
                  ifMatch: Leaf({bindings: [8], exit: 0}),
                  nextCase: None,
                },
                ids: [],
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
    let case1: T.Ast.case<_> = {
      patterns: NonEmpty(
        NonEmpty(#Array(l(0), [#Int(l(2), 10), #Int(l(3), 11)]), [#Int(l(4), 12)]),
        [],
      ),
      nodes: nodes1,
    }
    let case2: T.Ast.case<_> = {
      patterns: NonEmpty(
        NonEmpty(
          #ArrayWithTailBinding(l(5), [#Int(l(6), 10), #Int(l(7), 11)], #Binding(l(8), "x")),
          [#Int(l(9), 22)],
        ),
        [],
      ),
      nodes: nodes2,
    }
    let case3: T.Ast.case<_> = {
      patterns: NonEmpty(NonEmpty(#Array(l(10), [#Int(l(11), 30)]), [#Int(l(12), 32)]), []),
      nodes: nodes3,
    }
    let case4: T.Ast.case<_> = {
      patterns: NonEmpty(NonEmpty(#Binding(l(13), "x"), [#Int(l(14), 42)]), []),
      nodes: nodes4,
    }
    let result = Matching.make(NonEmpty(case1, [case2, case3, case4])).tree
    expect.value(result).toEqual(
      Nest({
        key: "",
        ids: [13],
        val: TList,
        child: Test({
          key: "",
          cases: {
            val: TLCons,
            ifMatch: Test({
              key: "",
              cases: {
                val: TInt(10),
                ifMatch: Nest({
                  key: "",
                  ids: [],
                  val: TList,
                  child: Test({
                    key: "",
                    cases: {
                      val: TLCons,
                      ifMatch: Test({
                        key: "",
                        cases: {
                          val: TInt(11),
                          ifMatch: Nest({
                            key: "",
                            ids: [5],
                            val: TList,
                            child: Test({
                              key: "",
                              cases: {
                                val: TLNil,
                                ifMatch: End(
                                  End(
                                    End(
                                      Test({
                                        key: "",
                                        cases: {
                                          val: TInt(12),
                                          ifMatch: Leaf({bindings: [], exit: 0}),
                                          nextCase: Some({
                                            val: TInt(22),
                                            ifMatch: Leaf({bindings: [5], exit: 1}),
                                            nextCase: Some({
                                              val: TInt(42),
                                              ifMatch: Leaf({bindings: [13], exit: 3}),
                                              nextCase: None,
                                            }),
                                          }),
                                        },
                                        ids: [],
                                        wildcard: None,
                                      }),
                                    ),
                                  ),
                                ),
                                nextCase: None,
                              },
                              ids: [],
                              wildcard: Some(
                                End(
                                  End(
                                    End(
                                      Test({
                                        key: "",
                                        cases: {
                                          val: TInt(22),
                                          ifMatch: Leaf({bindings: [5], exit: 1}),
                                          nextCase: Some({
                                            val: TInt(42),
                                            ifMatch: Leaf({bindings: [13], exit: 3}),
                                            nextCase: None,
                                          }),
                                        },
                                        ids: [],
                                        wildcard: None,
                                      }),
                                    ),
                                  ),
                                ),
                              ),
                            }),
                          }),
                          nextCase: None,
                        },
                        ids: [],
                        wildcard: Some(
                          End(
                            End(
                              Test({
                                key: "",
                                cases: {
                                  val: TInt(42),
                                  ifMatch: Leaf({bindings: [13], exit: 3}),
                                  nextCase: None,
                                },
                                ids: [],
                                wildcard: None,
                              }),
                            ),
                          ),
                        ),
                      }),
                      nextCase: None,
                    },
                    ids: [],
                    wildcard: Some(
                      End(
                        End(
                          Test({
                            key: "",
                            cases: {
                              val: TInt(42),
                              ifMatch: Leaf({bindings: [13], exit: 3}),
                              nextCase: None,
                            },
                            ids: [],
                            wildcard: None,
                          }),
                        ),
                      ),
                    ),
                  }),
                }),
                nextCase: Some({
                  val: TInt(30),
                  ifMatch: Nest({
                    key: "",
                    ids: [],
                    val: TList,
                    child: Test({
                      key: "",
                      cases: {
                        val: TLNil,
                        ifMatch: End(
                          End(
                            Test({
                              key: "",
                              cases: {
                                val: TInt(32),
                                ifMatch: Leaf({bindings: [], exit: 2}),
                                nextCase: Some({
                                  val: TInt(42),
                                  ifMatch: Leaf({bindings: [13], exit: 3}),
                                  nextCase: None,
                                }),
                              },
                              ids: [],
                              wildcard: None,
                            }),
                          ),
                        ),
                        nextCase: None,
                      },
                      ids: [],
                      wildcard: Some(
                        End(
                          End(
                            Test({
                              key: "",
                              cases: {
                                val: TInt(42),
                                ifMatch: Leaf({bindings: [13], exit: 3}),
                                nextCase: None,
                              },
                              ids: [],
                              wildcard: None,
                            }),
                          ),
                        ),
                      ),
                    }),
                  }),
                  nextCase: None,
                }),
              },
              ids: [],
              wildcard: Some(
                End(
                  Test({
                    key: "",
                    cases: {
                      val: TInt(42),
                      ifMatch: Leaf({bindings: [13], exit: 3}),
                      nextCase: None,
                    },
                    ids: [],
                    wildcard: None,
                  }),
                ),
              ),
            }),
            nextCase: None,
          },
          ids: [],
          wildcard: Some(
            End(
              Test({
                key: "",
                cases: {val: TInt(42), ifMatch: Leaf({bindings: [13], exit: 3}), nextCase: None},
                ids: [],
                wildcard: None,
              }),
            ),
          ),
        }),
      }),
    )
  })

  test("Records sort fields correctly", ({expect, _}) => {
    let l: T.loc = Loc(0)
    let nodes1: T.Ast.nodes<_> = [Text("a", NoTrim)]
    let case1: T.Ast.case<_> = {
      patterns: NonEmpty(
        NonEmpty(#Object(l, [("a", #Int(l, 10)), ("b", #Int(l, 11))]), [#Int(l, 12)]),
        [NonEmpty(#Object(l, [("b", #Int(l, 21)), ("a", #Int(l, 20))]), [#Int(l, 22)])],
      ),
      nodes: nodes1,
    }
    let result = Matching.make(NonEmpty(case1, [])).tree
    expect.value(result).toEqual(
      Nest({
        key: "",
        ids: [],
        val: TRecord,
        child: Test({
          key: "a",
          cases: {
            val: TInt(10),
            ifMatch: Test({
              key: "b",
              cases: {
                val: TInt(11),
                ifMatch: End(
                  Test({
                    key: "",
                    cases: {val: TInt(12), ifMatch: Leaf({bindings: [], exit: 0}), nextCase: None},
                    ids: [],
                    wildcard: None,
                  }),
                ),
                nextCase: None,
              },
              ids: [],
              wildcard: None,
            }),
            nextCase: Some({
              val: TInt(20),
              ifMatch: Test({
                key: "b",
                cases: {
                  val: TInt(21),
                  ifMatch: End(
                    Test({
                      key: "",
                      cases: {
                        val: TInt(22),
                        ifMatch: Leaf({bindings: [], exit: 0}),
                        nextCase: None,
                      },
                      ids: [],
                      wildcard: None,
                    }),
                  ),
                  nextCase: None,
                },
                ids: [],
                wildcard: None,
              }),
              nextCase: None,
            }),
          },
          ids: [],
          wildcard: None,
        }),
      }),
    )
  })

  test("Records: missing fields are automatically wildcards", ({expect, _}) => {
    let l: T.loc = Loc(0)
    let nodes1: T.Ast.nodes<_> = [Text("a", NoTrim)]
    let case1: T.Ast.case<_> = {
      patterns: NonEmpty(
        NonEmpty(#Object(l, [("a", #Int(l, 10)), ("b", #Int(l, 11))]), [#Int(l, 12)]),
        [NonEmpty(#Object(l, [("b", #Int(l, 21))]), [#Int(l, 22)])],
      ),
      nodes: nodes1,
    }
    let result = Matching.make(NonEmpty(case1, [])).tree
    expect.value(result).toEqual(
      Nest({
        key: "",
        ids: [],
        val: TRecord,
        child: Test({
          key: "a",
          cases: {
            val: TInt(10),
            ifMatch: Test({
              key: "b",
              cases: {
                val: TInt(11),
                ifMatch: End(
                  Test({
                    key: "",
                    cases: {val: TInt(12), ifMatch: Leaf({bindings: [], exit: 0}), nextCase: None},
                    ids: [],
                    wildcard: None,
                  }),
                ),
                nextCase: Some({
                  val: TInt(21),
                  ifMatch: End(
                    Test({
                      key: "",
                      cases: {
                        val: TInt(22),
                        ifMatch: Leaf({bindings: [], exit: 0}),
                        nextCase: None,
                      },
                      ids: [],
                      wildcard: None,
                    }),
                  ),
                  nextCase: None,
                }),
              },
              ids: [],
              wildcard: None,
            }),
            nextCase: None,
          },
          ids: [],
          wildcard: Some(
            Test({
              key: "b",
              cases: {
                val: TInt(21),
                ifMatch: End(
                  Test({
                    key: "",
                    cases: {val: TInt(22), ifMatch: Leaf({bindings: [], exit: 0}), nextCase: None},
                    ids: [],
                    wildcard: None,
                  }),
                ),
                nextCase: None,
              },
              ids: [],
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
    let case1: T.Ast.case<_> = {
      patterns: NonEmpty(NonEmpty(#Object(l(0), [("b", #Int(l(1), 10))]), []), []),
      nodes: nodes1,
    }
    let case2: T.Ast.case<_> = {
      patterns: NonEmpty(NonEmpty(#Object(l(2), [("a", #Int(l(3), 20))]), []), []),
      nodes: nodes2,
    }
    let case3: T.Ast.case<_> = {
      patterns: NonEmpty(NonEmpty(#Object(l(4), [("c", #Int(l(5), 30))]), []), []),
      nodes: nodes3,
    }
    let case4: T.Ast.case<_> = {
      patterns: NonEmpty(NonEmpty(#Binding(l(6), "_"), []), []),
      nodes: nodes4,
    }
    let result = Matching.make(NonEmpty(case1, [case2, case3, case4])).tree
    expect.value(result).toEqual(
      Nest({
        key: "",
        ids: [6],
        val: TRecord,
        child: Test({
          key: "a",
          cases: {
            val: TInt(20),
            ifMatch: Test({
              key: "b",
              cases: {
                val: TInt(10),
                ifMatch: End(Leaf({bindings: [], exit: 0})),
                nextCase: None,
              },
              ids: [],
              wildcard: Some(End(Leaf({bindings: [], exit: 1}))),
            }),
            nextCase: None,
          },
          ids: [],
          wildcard: Some(
            Test({
              key: "b",
              cases: {
                val: TInt(10),
                ifMatch: End(Leaf({bindings: [], exit: 0})),
                nextCase: None,
              },
              ids: [],
              wildcard: Some(
                Test({
                  key: "c",
                  cases: {
                    val: TInt(30),
                    ifMatch: End(Leaf({bindings: [], exit: 2})),
                    nextCase: None,
                  },
                  ids: [],
                  wildcard: Some(End(Leaf({bindings: [6], exit: 3}))),
                }),
              ),
            }),
          ),
        }),
      }),
    )
  })
})
