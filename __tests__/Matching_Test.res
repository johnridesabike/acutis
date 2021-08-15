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
    let l: T.loc = Loc(0)
    let nodes1: T.Ast.nodes<_> = [Text("nodes1", NoTrim)]
    let case1: T.Ast.case<_> = {
      patterns: NonEmpty(
        NonEmpty(#Int(l, 1), [#Int(l, 2), #Int(l, 3)]),
        [
          NonEmpty(#Int(l, 1), [#Int(l, 4), #Int(l, 5)]),
          NonEmpty(#Int(l, 10), [#Int(l, 20), #Int(l, 30)]),
          NonEmpty(#Int(l, 10), [#Int(l, 20), #Int(l, 40)]),
        ],
      ),
      nodes: nodes1,
    }
    let nodes2: T.Ast.nodes<_> = [Text("nodes2", NoTrim)]
    let case2: T.Ast.case<_> = {
      patterns: NonEmpty(
        NonEmpty(#Int(l, 100), [#Int(l, 102), #Int(l, 103)]),
        [NonEmpty(#Int(l, 100), [#Int(l, 104), #Int(l, 105)])],
      ),
      nodes: nodes2,
    }
    let nodes3: T.Ast.nodes<_> = [Text("nodes3", NoTrim)]
    let case3: T.Ast.case<_> = {
      patterns: NonEmpty(
        NonEmpty(#Int(l, 10), [#Int(l, 20), #Int(l, 50)]),
        [
          NonEmpty(#Int(l, 1), [#Int(l, 2), #Int(l, 100)]),
          NonEmpty(#Int(l, 1), [#Int(l, 2), #Int(l, 101)]),
          NonEmpty(#Int(l, 100), [#Int(l, 102), #Int(l, 106)]),
        ],
      ),
      nodes: nodes3,
    }
    let result = Matching2.make(NonEmpty(case1, [case2, case3])).tree
    expect.value(result).toEqual(
      Test({
        key: "",
        wildcard: None,
        tests: {
          val: TInt(1),
          ifmatch: Test({
            key: "",
            wildcard: None,
            tests: {
              val: TInt(2),
              ifmatch: Test({
                key: "",
                wildcard: None,
                tests: {
                  val: TInt(3),
                  ifmatch: Leaf(0),
                  ifnomatch: Some({
                    val: TInt(100),
                    ifmatch: Leaf(2),
                    ifnomatch: Some({
                      val: TInt(101),
                      ifmatch: Leaf(2),
                      ifnomatch: None,
                    }),
                  }),
                },
              }),
              ifnomatch: Some({
                val: TInt(4),
                ifmatch: Test({
                  key: "",
                  wildcard: None,
                  tests: {
                    val: TInt(5),
                    ifmatch: Leaf(0),
                    ifnomatch: None,
                  },
                }),
                ifnomatch: None,
              }),
            },
          }),
          ifnomatch: Some({
            val: TInt(10),
            ifmatch: Test({
              key: "",
              wildcard: None,
              tests: {
                val: TInt(20),
                ifmatch: Test({
                  key: "",
                  wildcard: None,
                  tests: {
                    val: TInt(30),
                    ifmatch: Leaf(0),
                    ifnomatch: Some({
                      val: TInt(40),
                      ifmatch: Leaf(0),
                      ifnomatch: Some({
                        val: TInt(50),
                        ifmatch: Leaf(2),
                        ifnomatch: None,
                      }),
                    }),
                  },
                }),
                ifnomatch: None,
              },
            }),
            ifnomatch: Some({
              val: TInt(100),
              ifmatch: Test({
                key: "",
                wildcard: None,
                tests: {
                  val: TInt(102),
                  ifmatch: Test({
                    key: "",
                    wildcard: None,
                    tests: {
                      val: TInt(103),
                      ifmatch: Leaf(1),
                      ifnomatch: Some({
                        val: TInt(106),
                        ifmatch: Leaf(2),
                        ifnomatch: None,
                      }),
                    },
                  }),
                  ifnomatch: Some({
                    val: TInt(104),
                    ifmatch: Test({
                      key: "",
                      wildcard: None,
                      tests: {
                        val: TInt(105),
                        ifmatch: Leaf(1),
                        ifnomatch: None,
                      },
                    }),
                    ifnomatch: None,
                  }),
                },
              }),
              ifnomatch: None,
            }),
          }),
        },
      }),
    )
  })
  test("Basic dec tree 2", ({expect, _}) => {
    let l: T.loc = Loc(0)
    let nodes1: T.Ast.nodes<_> = [Text("a", NoTrim)]
    let case1: T.Ast.case<_> = {
      patterns: NonEmpty(
        NonEmpty(#Int(l, 10), [#Int(l, 11), #Int(l, 12)]),
        [
          NonEmpty(#Binding(l, "_"), [#Int(l, 21), #Int(l, 22)]),
          NonEmpty(#Int(l, 10), [#Int(l, 11), #Int(l, 12)]),
          NonEmpty(#Int(l, 30), [#Int(l, 31), #Int(l, 32)]),
          NonEmpty(#Int(l, 30), [#Binding(l, "_"), #Int(l, 42)]),
          NonEmpty(#Int(l, 30), [#Int(l, 31), #Int(l, 42)]),
        ],
      ),
      nodes: nodes1,
    }

    let result = Matching2.make(NonEmpty(case1, [])).tree
    expect.value(result).toEqual(
      Test({
        key: "",
        tests: {
          val: TInt(10),
          ifmatch: Test({
            key: "",
            wildcard: None,
            tests: {
              val: TInt(11),
              ifmatch: Test({
                key: "",
                wildcard: None,
                tests: {
                  val: TInt(12),
                  ifmatch: Leaf(0),
                  ifnomatch: None,
                },
              }),
              ifnomatch: Some({
                val: TInt(21),
                ifmatch: Test({
                  key: "",
                  wildcard: None,
                  tests: {
                    val: TInt(22),
                    ifmatch: Leaf(0),
                    ifnomatch: None,
                  },
                }),
                ifnomatch: None,
              }),
            },
          }),
          ifnomatch: Some({
            val: TInt(30),
            ifmatch: Test({
              key: "",
              tests: {
                val: TInt(21),
                ifmatch: Test({
                  key: "",
                  wildcard: None,
                  tests: {
                    val: TInt(22),
                    ifmatch: Leaf(0),
                    ifnomatch: Some({
                      val: TInt(42),
                      ifmatch: Leaf(0),
                      ifnomatch: None,
                    }),
                  },
                }),
                ifnomatch: Some({
                  val: TInt(31),
                  ifmatch: Test({
                    key: "",
                    wildcard: None,
                    tests: {
                      val: TInt(32),
                      ifmatch: Leaf(0),
                      ifnomatch: Some({
                        val: TInt(42),
                        ifmatch: Leaf(0),
                        ifnomatch: None,
                      }),
                    },
                  }),
                  ifnomatch: None,
                }),
              },
              wildcard: Some(
                Test({
                  key: "",
                  wildcard: None,
                  tests: {
                    val: TInt(42),
                    ifmatch: Leaf(0),
                    ifnomatch: None,
                  },
                }),
              ),
            }),
            ifnomatch: None,
          }),
        },
        wildcard: Some(
          Test({
            key: "",
            wildcard: None,
            tests: {
              val: TInt(21),
              ifmatch: Test({
                key: "",
                wildcard: None,
                tests: {
                  val: TInt(22),
                  ifmatch: Leaf(0),
                  ifnomatch: None,
                },
              }),
              ifnomatch: None,
            },
          }),
        ),
      }),
    )
  })

  test("dec tree tuple", ({expect, _}) => {
    let l: T.loc = Loc(0)
    let nodes1: T.Ast.nodes<_> = [Text("a", NoTrim)]
    let case1: T.Ast.case<_> = {
      patterns: NonEmpty(
        NonEmpty(#Tuple(l, [#Int(l, 10), #Int(l, 12)]), [#Int(l, 13)]),
        [
          NonEmpty(#Tuple(l, [#Int(l, 10), #Int(l, 22)]), [#Int(l, 23)]),
          NonEmpty(#Binding(l, "_"), [#Int(l, 33)]),
        ],
      ),
      nodes: nodes1,
    }
    let result = Matching2.make(NonEmpty(case1, [])).tree
    expect.value(result).toEqual(
      Test({
        key: "",
        tests: {
          val: TTuple,
          ifmatch: Test({
            key: "",
            tests: {
              val: TInt(10),
              ifmatch: Test({
                key: "",
                tests: {
                  val: TInt(12),
                  ifmatch: End(
                    Test({
                      key: "",
                      tests: {
                        val: TInt(13),
                        ifmatch: Leaf(0),
                        ifnomatch: Some({
                          val: TInt(33),
                          ifmatch: Leaf(0),
                          ifnomatch: None,
                        }),
                      },
                      wildcard: None,
                    }),
                  ),
                  ifnomatch: Some({
                    val: TInt(22),
                    ifmatch: End(
                      Test({
                        key: "",
                        tests: {
                          val: TInt(23),
                          ifmatch: Leaf(0),
                          ifnomatch: Some({
                            val: TInt(33),
                            ifmatch: Leaf(0),
                            ifnomatch: None,
                          }),
                        },
                        wildcard: None,
                      }),
                    ),
                    ifnomatch: None,
                  }),
                },
                wildcard: Some(
                  End(
                    Test({
                      key: "",
                      tests: {
                        val: TInt(33),
                        ifmatch: Leaf(0),
                        ifnomatch: None,
                      },
                      wildcard: None,
                    }),
                  ),
                ),
              }),
              ifnomatch: None,
            },
            wildcard: Some(
              End(
                Test({
                  key: "",
                  tests: {
                    val: TInt(33),
                    ifmatch: Leaf(0),
                    ifnomatch: None,
                  },
                  wildcard: None,
                }),
              ),
            ),
          }),
          ifnomatch: None,
        },
        wildcard: Some(
          Test({
            key: "",
            tests: {
              val: TInt(33),
              ifmatch: Leaf(0),
              ifnomatch: None,
            },
            wildcard: None,
          }),
        ),
      }),
    )
  })

  test("dec tree list", ({expect, _}) => {
    let l: T.loc = Loc(0)
    let nodes1: T.Ast.nodes<_> = [Text("a", NoTrim)]
    let case1: T.Ast.case<_> = {
      patterns: NonEmpty(
        NonEmpty(#Array(l, [#Int(l, 10), #Int(l, 11)]), [#Int(l, 12)]),
        [
          NonEmpty(
            #ArrayWithTailBinding(l, [#Int(l, 10), #Int(l, 11)], #Binding(l, "x")),
            [#Int(l, 22)],
          ),
          NonEmpty(#Array(l, [#Int(l, 30)]), [#Int(l, 32)]),
          NonEmpty(#Binding(l, "x"), [#Int(l, 42)]),
        ],
      ),
      nodes: nodes1,
    }
    let result = Matching2.make(NonEmpty(case1, [])).tree
    expect.value(result).toEqual(
      Test({
        key: "",
        tests: {
          val: TLCons,
          ifmatch: Test({
            key: "",
            tests: {
              val: TInt(10),
              ifmatch: Test({
                key: "",
                tests: {
                  val: TLCons,
                  ifmatch: Test({
                    key: "",
                    tests: {
                      val: TInt(11),
                      ifmatch: Test({
                        key: "",
                        tests: {
                          val: TLNil,
                          ifmatch: End(
                            End(
                              Test({
                                key: "",
                                tests: {
                                  val: TInt(12),
                                  ifmatch: Leaf(0),
                                  ifnomatch: Some({
                                    val: TInt(22),
                                    ifmatch: Leaf(0),
                                    ifnomatch: Some({
                                      val: TInt(42),
                                      ifmatch: Leaf(0),
                                      ifnomatch: None,
                                    }),
                                  }),
                                },
                                wildcard: None,
                              }),
                            ),
                          ),
                          ifnomatch: None,
                        },
                        wildcard: Some(
                          End(
                            End(
                              Test({
                                key: "",
                                tests: {
                                  val: TInt(22),
                                  ifmatch: Leaf(0),
                                  ifnomatch: Some({
                                    val: TInt(42),
                                    ifmatch: Leaf(0),
                                    ifnomatch: None,
                                  }),
                                },
                                wildcard: None,
                              }),
                            ),
                          ),
                        ),
                      }),
                      ifnomatch: None,
                    },
                    wildcard: Some(
                      End(
                        End(
                          Test({
                            key: "",
                            tests: {val: TInt(42), ifmatch: Leaf(0), ifnomatch: None},
                            wildcard: None,
                          }),
                        ),
                      ),
                    ),
                  }),
                  ifnomatch: None,
                },
                wildcard: Some(
                  End(
                    Test({
                      key: "",
                      tests: {val: TInt(42), ifmatch: Leaf(0), ifnomatch: None},
                      wildcard: None,
                    }),
                  ),
                ),
              }),
              ifnomatch: Some({
                val: TInt(30),
                ifmatch: Test({
                  key: "",
                  tests: {
                    val: TLNil,
                    ifmatch: End(
                      Test({
                        key: "",
                        tests: {
                          val: TInt(32),
                          ifmatch: Leaf(0),
                          ifnomatch: Some({val: TInt(42), ifmatch: Leaf(0), ifnomatch: None}),
                        },
                        wildcard: None,
                      }),
                    ),
                    ifnomatch: None,
                  },
                  wildcard: Some(
                    End(
                      Test({
                        key: "",
                        tests: {val: TInt(42), ifmatch: Leaf(0), ifnomatch: None},
                        wildcard: None,
                      }),
                    ),
                  ),
                }),
                ifnomatch: None,
              }),
            },
            wildcard: Some(
              End(
                Test({
                  key: "",
                  tests: {val: TInt(42), ifmatch: Leaf(0), ifnomatch: None},
                  wildcard: None,
                }),
              ),
            ),
          }),
          ifnomatch: None,
        },
        wildcard: Some(
          Test({
            key: "",
            tests: {val: TInt(42), ifmatch: Leaf(0), ifnomatch: None},
            wildcard: None,
          }),
        ),
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
    let result = Matching2.make(NonEmpty(case1, [])).tree
    expect.value(result).toEqual(
      Test({
        key: "",
        tests: {
          val: TRecord,
          ifmatch: Test({
            key: "a",
            tests: {
              val: TInt(10),
              ifmatch: Test({
                key: "b",
                tests: {
                  val: TInt(11),
                  ifmatch: End(
                    Test({
                      key: "",
                      tests: {val: TInt(12), ifmatch: Leaf(0), ifnomatch: None},
                      wildcard: None,
                    }),
                  ),
                  ifnomatch: None,
                },
                wildcard: None,
              }),
              ifnomatch: Some({
                val: TInt(20),
                ifmatch: Test({
                  key: "b",
                  tests: {
                    val: TInt(21),
                    ifmatch: End(
                      Test({
                        key: "",
                        tests: {val: TInt(22), ifmatch: Leaf(0), ifnomatch: None},
                        wildcard: None,
                      }),
                    ),
                    ifnomatch: None,
                  },
                  wildcard: None,
                }),
                ifnomatch: None,
              }),
            },
            wildcard: None,
          }),
          ifnomatch: None,
        },
        wildcard: None,
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
    let result = Matching2.make(NonEmpty(case1, [])).tree
    expect.value(result).toEqual(
      Test({
        key: "",
        tests: {
          val: TRecord,
          ifmatch: Test({
            key: "a",
            tests: {
              val: TInt(10),
              ifmatch: Test({
                key: "b",
                tests: {
                  val: TInt(11),
                  ifmatch: End(
                    Test({
                      key: "",
                      tests: {val: TInt(12), ifmatch: Leaf(0), ifnomatch: None},
                      wildcard: None,
                    }),
                  ),
                  ifnomatch: Some({
                    val: TInt(21),
                    ifmatch: End(
                      Test({
                        key: "",
                        tests: {val: TInt(22), ifmatch: Leaf(0), ifnomatch: None},
                        wildcard: None,
                      }),
                    ),
                    ifnomatch: None,
                  }),
                },
                wildcard: None,
              }),
              ifnomatch: None,
            },
            wildcard: Some(
              Test({
                key: "b",
                tests: {
                  val: TInt(21),
                  ifmatch: End(
                    Test({
                      key: "",
                      tests: {val: TInt(22), ifmatch: Leaf(0), ifnomatch: None},
                      wildcard: None,
                    }),
                  ),
                  ifnomatch: None,
                },
                wildcard: None,
              }),
            ),
          }),
          ifnomatch: None,
        },
        wildcard: None,
      }),
    )
  })

  test("Records: new fields expand existing rows", ({expect, _}) => {
    let l: T.loc = Loc(0)
    let nodes1: T.Ast.nodes<_> = [Text("x", NoTrim)]
    let nodes2: T.Ast.nodes<_> = [Text("y", NoTrim)]
    let nodes3: T.Ast.nodes<_> = [Text("z", NoTrim)]
    let nodes4: T.Ast.nodes<_> = [Text("zz", NoTrim)]
    let case1: T.Ast.case<_> = {
      patterns: NonEmpty(NonEmpty(#Object(l, [("b", #Int(l, 10))]), []), []),
      nodes: nodes1,
    }
    let case2: T.Ast.case<_> = {
      patterns: NonEmpty(NonEmpty(#Object(l, [("a", #Int(l, 20))]), []), []),
      nodes: nodes2,
    }
    let case3: T.Ast.case<_> = {
      patterns: NonEmpty(NonEmpty(#Object(l, [("c", #Int(l, 30))]), []), []),
      nodes: nodes3,
    }
    let case4: T.Ast.case<_> = {
      patterns: NonEmpty(NonEmpty(#Binding(l, "_"), []), []),
      nodes: nodes4,
    }
    let result = Matching2.make(NonEmpty(case1, [case2, case3, case4])).tree
    expect.value(result).toEqual(
      Test({
        key: "",
        tests: {
          val: TRecord,
          ifmatch: Test({
            key: "a",
            tests: {
              val: TInt(20),
              ifmatch: Test({
                key: "b",
                tests: {
                  val: TInt(10),
                  ifmatch: End(Leaf(0)),
                  ifnomatch: None,
                },
                wildcard: Some(End(Leaf(1))),
              }),
              ifnomatch: None,
            },
            wildcard: Some(
              Test({
                key: "b",
                tests: {
                  val: TInt(10),
                  ifmatch: End(Leaf(0)),
                  ifnomatch: None,
                },
                wildcard: Some(
                  Test({
                    key: "c",
                    tests: {val: TInt(30), ifmatch: End(Leaf(2)), ifnomatch: None},
                    wildcard: Some(End(Leaf(3))),
                  }),
                ),
              }),
            ),
          }),
          ifnomatch: None,
        },
        wildcard: Some(Leaf(3)),
      }),
    )
  })
})
