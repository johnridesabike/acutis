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
module Array = Belt.Array
module T = Acutis_Types
module TC = TypeChecker
module NE = NonEmpty
module Option = Belt.Option
module SetString = Belt.Set.String
let ne = NE.fromArrayExn

describe("Decision tree", ({test, _}) => {
  test("cases are sorted correctly", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let nodes2 = [T.Ast.Text("", NoTrim)]
    let case1 = {
      TC.Ast2.pats: [
        [#Int(l(0), 0)]->ne,
        [#Int(l(3), 10)]->ne,
        [#Int(l(3), 20)]->ne,
        [#Int(l(3), 30)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let case2 = {
      TC.Ast2.pats: [[#Int(l(0), 15)]->ne]->ne,
      nodes: nodes2,
    }
    let result = Matching.make(ne([case1, case2])).tree
    expect.value(result).toEqual(
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
        wildcard: None,
      }),
    )
  })

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
    let result = Matching.make(ne([case1, case2, case3])).tree
    expect.value(result).toEqual(
      Switch({
        idx: 0,
        key: "",
        names: SetString.empty,
        wildcard: None,
        cases: {
          val: TPat_Int(1),
          ifMatch: Switch({
            idx: 1,
            key: "",
            names: SetString.empty,
            wildcard: None,
            cases: {
              val: TPat_Int(2),
              ifMatch: Switch({
                idx: 2,
                key: "",
                names: SetString.empty,
                wildcard: None,
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
              }),
              nextCase: Some({
                val: TPat_Int(4),
                ifMatch: Switch({
                  idx: 2,
                  key: "",
                  names: SetString.empty,
                  wildcard: None,
                  cases: {
                    val: TPat_Int(5),
                    ifMatch: End({names: SetString.empty, exit: 0}),
                    nextCase: None,
                  },
                }),
                nextCase: None,
              }),
            },
          }),
          nextCase: Some({
            val: TPat_Int(10),
            ifMatch: Switch({
              idx: 1,
              key: "",
              names: SetString.empty,
              wildcard: None,
              cases: {
                val: TPat_Int(20),
                ifMatch: Switch({
                  idx: 2,
                  key: "",
                  names: SetString.empty,
                  wildcard: None,
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
                }),
                nextCase: None,
              },
            }),
            nextCase: Some({
              val: TPat_Int(100),
              ifMatch: Switch({
                idx: 1,
                key: "",
                names: SetString.empty,
                wildcard: None,
                cases: {
                  val: TPat_Int(102),
                  ifMatch: Switch({
                    idx: 2,
                    key: "",
                    names: SetString.empty,
                    wildcard: None,
                    cases: {
                      val: TPat_Int(103),
                      ifMatch: End({names: SetString.empty, exit: 1}),
                      nextCase: Some({
                        val: TPat_Int(106),
                        ifMatch: End({names: SetString.empty, exit: 2}),
                        nextCase: None,
                      }),
                    },
                  }),
                  nextCase: Some({
                    val: TPat_Int(104),
                    ifMatch: Switch({
                      idx: 2,
                      key: "",
                      names: SetString.empty,
                      wildcard: None,
                      cases: {
                        val: TPat_Int(105),
                        ifMatch: End({names: SetString.empty, exit: 1}),
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

    let result = Matching.make(ne([case1])).tree
    expect.value(result).toEqual(
      Switch({
        idx: 0,
        key: "",
        names: SetString.fromArray(["x"]),
        cases: {
          val: TPat_Int(10),
          ifMatch: Switch({
            idx: 1,
            key: "",
            names: SetString.empty,
            wildcard: None,
            cases: {
              val: TPat_Int(11),
              ifMatch: Switch({
                idx: 2,
                key: "",
                names: SetString.empty,
                wildcard: None,
                cases: {
                  val: TPat_Int(12),
                  ifMatch: End({names: SetString.empty, exit: 0}),
                  nextCase: None,
                },
              }),
              nextCase: Some({
                val: TPat_Int(21),
                ifMatch: Switch({
                  idx: 2,
                  key: "",
                  names: SetString.empty,
                  wildcard: None,
                  cases: {
                    val: TPat_Int(22),
                    ifMatch: End({names: SetString.fromArray(["x"]), exit: 0}),
                    nextCase: None,
                  },
                }),
                nextCase: None,
              }),
            },
          }),
          nextCase: Some({
            val: TPat_Int(30),
            ifMatch: Switch({
              idx: 1,
              key: "",
              names: SetString.empty,
              cases: {
                val: TPat_Int(21),
                ifMatch: Switch({
                  idx: 2,
                  key: "",
                  names: SetString.empty,
                  wildcard: None,
                  cases: {
                    val: TPat_Int(22),
                    ifMatch: End({names: SetString.fromArray(["x"]), exit: 0}),
                    nextCase: Some({
                      val: TPat_Int(42),
                      ifMatch: End({names: SetString.fromArray(["y"]), exit: 0}),
                      nextCase: None,
                    }),
                  },
                }),
                nextCase: Some({
                  val: TPat_Int(31),
                  ifMatch: Switch({
                    idx: 2,
                    key: "",
                    names: SetString.empty,
                    wildcard: None,
                    cases: {
                      val: TPat_Int(32),
                      ifMatch: End({names: SetString.empty, exit: 0}),
                      nextCase: Some({
                        val: TPat_Int(42),
                        ifMatch: End({names: SetString.fromArray(["y"]), exit: 0}),
                        nextCase: None,
                      }),
                    },
                  }),
                  nextCase: None,
                }),
              },
              wildcard: Some(
                Switch({
                  idx: 2,
                  key: "",
                  names: SetString.empty,
                  wildcard: None,
                  cases: {
                    val: TPat_Int(42),
                    ifMatch: End({names: SetString.fromArray(["y"]), exit: 0}),
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
            idx: 1,
            key: "",
            names: SetString.empty,
            wildcard: None,
            cases: {
              val: TPat_Int(21),
              ifMatch: Switch({
                idx: 2,
                key: "",
                names: SetString.empty,
                wildcard: None,
                cases: {
                  val: TPat_Int(22),
                  ifMatch: End({names: SetString.fromArray(["x"]), exit: 0}),
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
    let result = Matching.make(ne([case1])).tree
    expect.value(result).toEqual(
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
                    wildcard: None,
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
            wildcard: None,
          }),
        ),
      }),
    )
  })

  test("Nests merge into wildcards correctly.", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let nodes2 = [T.Ast.Text("", NoTrim)]
    let nodes3 = [T.Ast.Text("", NoTrim)]
    let case1 = {
      TC.Ast2.pats: [[#Binding(l(0), "x"), #Int(l(1), 1)]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      TC.Ast2.pats: [
        [#Tuple(l(2), [#String(l(3), "a"), #String(l(4), "b")]), #Int(l(5), 10)]->ne,
      ]->ne,
      nodes: nodes2,
    }
    let case3 = {
      TC.Ast2.pats: [
        [#Tuple(l(6), [#String(l(7), "a"), #String(l(8), "b")]), #Int(l(9), 1)]->ne,
      ]->ne,
      nodes: nodes3,
    }
    let {tree, _} = Matching.make(ne([case1, case2, case3]))
    expect.value(tree).toEqual(
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
    let result = Matching.make(ne([case1, case2, case3, case4])).tree
    expect.value(result).toEqual(
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
                                        ifMatch: End({names: SetString.fromArray(["x"]), exit: 1}),
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
                                    wildcard: None,
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
                                        ifMatch: End({names: SetString.fromArray(["x"]), exit: 1}),
                                        nextCase: Some({
                                          val: TPat_Int(42),
                                          ifMatch: End({
                                            names: SetString.fromArray(["y"]),
                                            exit: 3,
                                          }),
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
                          wildcard: None,
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
                wildcard: None,
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
    let result = Matching.make(ne([case1])).tree
    expect.value(result).toEqual(
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
                    wildcard: None,
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
        wildcard: None,
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
    let result = Matching.make(ne([case1])).tree
    expect.value(result).toEqual(
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
                    wildcard: None,
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
                    wildcard: None,
                  }),
                ),
                nextCase: None,
              },
              wildcard: None,
            }),
          ),
        }),
        wildcard: None,
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
    let result = Matching.make(ne([case1, case2, case3, case4])).tree
    expect.value(result).toEqual(
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
    )
  })
})

describe("Partial matching", ({test, _}) => {
  let toString = o => Option.map(o, a => Array.map(a, TC.TypedPattern.toString))
  test("Partial match test 1", ({expect, _}) => {
    let l = l => T.Loc(l)
    let nodes1 = [T.Ast.Text("", NoTrim)]
    let nodes2 = [T.Ast.Text("", NoTrim)]
    let nodes3 = [T.Ast.Text("", NoTrim)]
    let case1: TC.Ast2.case<_, _> = {
      pats: [
        [#Int(l(0), 0)]->ne,
        [#Int(l(3), 10)]->ne,
        [#Int(l(3), 20)]->ne,
        [#Int(l(3), 30)]->ne,
      ]->ne,
      nodes: nodes1,
    }
    let case2: TC.Ast2.case<_, _> = {
      pats: [[#Int(l(0), 15)]->ne]->ne,
      nodes: nodes2,
    }
    let result = Matching.ParMatch.check(Matching.make(ne([case1, case2])).tree)->toString
    expect.value(result).toEqual(Some(["1"]))

    let case3: TC.Ast2.case<_, _> = {
      pats: [[#Binding(l(0), "x")]->ne]->ne,
      nodes: nodes3,
    }
    let result = Matching.ParMatch.check(Matching.make(ne([case1, case2, case3])).tree)->toString
    expect.value(result).toEqual(None)
    let case1 = {
      TC.Ast2.pats: [[#True(l(0))]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      TC.Ast2.pats: [[#False(l(0))]->ne]->ne,
      nodes: nodes2,
    }
    let result = Matching.ParMatch.check(Matching.make(ne([case1, case2])).tree)->toString
    expect.value(result).toEqual(None)
    let case1 = {
      TC.Ast2.pats: [[#Array(l(0), [])]->ne, [#Array(l(0), [#Binding(l(1), "_")])]->ne]->ne,
      nodes: nodes1,
    }
    let result = Matching.ParMatch.check(Matching.make(ne([case1])).tree)->toString
    expect.value(result).toEqual(Some(["[_, ..._]"]))
    let case1 = {
      TC.Ast2.pats: [[#Array(l(0), [#Binding(l(1), "_")])]->ne]->ne,
      nodes: nodes1,
    }
    let result = Matching.ParMatch.check(Matching.make(ne([case1])).tree)->toString
    expect.value(result).toEqual(Some(["[]"]))
    let case1 = {
      TC.Ast2.pats: [[#Object(l(0), [("b", #Int(l(1), 10))])]->ne]->ne,
      nodes: nodes1,
    }
    let case2 = {
      TC.Ast2.pats: [[#Object(l(2), [("a", #Int(l(3), 20))])]->ne]->ne,
      nodes: nodes2,
    }
    let result = Matching.ParMatch.check(Matching.make(ne([case1, case2])).tree)->toString
    expect.value(result).toEqual(Some(["{a, b: 0}"]))
  })
})
