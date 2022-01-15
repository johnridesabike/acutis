/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
open TestFramework
module SI = Belt.Set.Int
module MS = Belt.Map.String

let e = Matching.Exit.unsafe_key

let compile = src => {
  let {nodes, _} = Compile.make(~name="", src, Compile.Components.empty())->Result.getOrElse(e => {
    Js.log(Js.Json.stringifyAny(e))
    assert false
  })
  Belt.Array.reduce(nodes, None, (acc, x) =>
    switch x {
    | OMatch(_, {tree, _}) => Some(tree)
    | _ => acc
    }
  )->Belt.Option.getExn
}

describe("Basic tree", ({test, _}) => {
  test("blah", ({expect, _}) => {
    let src = `{% match a with !a %} {% with null %} {% /match %}`
    let result = compile(src)
    expect.value(result).toEqual(
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
                ids: SI.fromArray([17]),
                child: End(End({names: MS.fromArray([("a", 17)]), exit: e(0)})),
              }),
            ),
            wildcard: None,
          }),
        ),
      }),
    )
  })
  test("cases are sorted correctly", ({expect, _}) => {
    let src = `
      {% match a
         with 0 with 10 with 20 with 30 %} {% with 15 %} {% with _ %}
      {% /match %}`
    let result = compile(src)
    expect.value(result).toEqual(
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
    let src = `
    {% match  a,   b,   c
       with   1,   2,   3
       with   1,   4,   5
       with  10,  20,  30
       with  10,  20,  40 %}
    {% with 100, 102, 103
       with 100, 104, 105 %}
    {% with  10,  20,  50
       with   1,   2, 100
       with   1,   2, 101
       with 100, 102, 106 %}
    {% with   _,   _,   _ %}
    {% /match %}`
    let result = compile(src)
    expect.value(result).toEqual(
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
    let src = `
    {% match a, b, c
       with 10, 11, 12 %}
    {% with  x, 21, 22 %}
    {% with 30, 31, 32 %}
    {% with 30,  y, 42 %}
    {% with  a,  b,  c %}
    {% /match %}`
    let result = compile(src)
    expect.value(result).toEqual(
      Switch({
        key: 0,
        ids: SI.empty->SI.add(61)->SI.add(139),
        cases: {
          val: PInt(10),
          ifMatch: Switch({
            key: 1,
            ids: SI.fromArray([143]),
            cases: {
              val: PInt(11),
              ifMatch: Switch({
                key: 2,
                ids: SI.fromArray([147]),
                cases: {
                  val: PInt(12),
                  ifMatch: End({names: MS.empty, exit: e(0)}),
                  nextCase: None,
                },
                wildcard: Some(
                  End({
                    exit: e(4),
                    names: MS.empty->MS.set("a", 139)->MS.set("b", 143)->MS.set("c", 147),
                  }),
                ),
              }),
              nextCase: Some({
                val: PInt(21),
                ifMatch: Switch({
                  key: 2,
                  ids: SI.fromArray([147]),
                  cases: {
                    val: PInt(22),
                    ifMatch: End({names: MS.fromArray([("x", 61)]), exit: e(1)}),
                    nextCase: None,
                  },
                  wildcard: Some(
                    End({
                      exit: e(4),
                      names: MS.empty->MS.set("a", 139)->MS.set("b", 143)->MS.set("c", 147),
                    }),
                  ),
                }),
                nextCase: None,
              }),
            },
            wildcard: Some(
              Wildcard({
                key: 2,
                ids: SI.fromArray([147]),
                child: End({
                  exit: e(4),
                  names: MS.empty->MS.set("a", 139)->MS.set("b", 143)->MS.set("c", 147),
                }),
              }),
            ),
          }),
          nextCase: Some({
            val: PInt(30),
            ifMatch: Switch({
              key: 1,
              ids: SI.empty->SI.add(117)->SI.add(143),
              cases: {
                val: PInt(21),
                ifMatch: Switch({
                  key: 2,
                  ids: SI.fromArray([147]),
                  cases: {
                    val: PInt(22),
                    ifMatch: End({names: MS.fromArray([("x", 61)]), exit: e(1)}),
                    nextCase: Some({
                      val: PInt(42),
                      ifMatch: End({names: MS.fromArray([("y", 117)]), exit: e(3)}),
                      nextCase: None,
                    }),
                  },
                  wildcard: Some(
                    End({
                      exit: e(4),
                      names: MS.empty->MS.set("a", 139)->MS.set("b", 143)->MS.set("c", 147),
                    }),
                  ),
                }),
                nextCase: Some({
                  val: PInt(31),
                  ifMatch: Switch({
                    key: 2,
                    ids: SI.fromArray([147]),
                    cases: {
                      val: PInt(32),
                      ifMatch: End({names: MS.empty, exit: e(2)}),
                      nextCase: Some({
                        val: PInt(42),
                        ifMatch: End({names: MS.fromArray([("y", 117)]), exit: e(3)}),
                        nextCase: None,
                      }),
                    },
                    wildcard: Some(
                      End({
                        exit: e(4),
                        names: MS.empty->MS.set("a", 139)->MS.set("b", 143)->MS.set("c", 147),
                      }),
                    ),
                  }),
                  nextCase: None,
                }),
              },
              wildcard: Some(
                Switch({
                  key: 2,
                  ids: SI.fromArray([147]),
                  cases: {
                    val: PInt(42),
                    ifMatch: End({names: MS.fromArray([("y", 117)]), exit: e(3)}),
                    nextCase: None,
                  },
                  wildcard: Some(
                    End({
                      exit: e(4),
                      names: MS.empty->MS.set("a", 139)->MS.set("b", 143)->MS.set("c", 147),
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
            ids: SI.fromArray([143]),
            cases: {
              val: PInt(21),
              ifMatch: Switch({
                key: 2,
                ids: SI.fromArray([147]),
                cases: {
                  val: PInt(22),
                  ifMatch: End({names: MS.fromArray([("x", 61)]), exit: e(1)}),
                  nextCase: None,
                },
                wildcard: Some(
                  End({
                    exit: e(4),
                    names: MS.empty->MS.set("a", 139)->MS.set("b", 143)->MS.set("c", 147),
                  }),
                ),
              }),
              nextCase: None,
            },
            wildcard: Some(
              Wildcard({
                key: 2,
                ids: SI.fromArray([147]),
                child: End({
                  exit: e(4),
                  names: MS.empty->MS.set("a", 139)->MS.set("b", 143)->MS.set("c", 147),
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
    let src = `
    {% match a, b
       with (10, 12), 13
       with (10, 22), 23
       with        _, 33
       with        _,  _ %} {% /match %}`
    let result = compile(src)
    expect.value(result).toEqual(
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
    let src = `
    {% match          a,  b
       with           x,  1 %}
    {% with  ("a", "b"), 10 %}
    {% with  (  _,   y),  z %}
    {% /match %}`
    let result = compile(src)
    expect.value(result).toEqual(
      Nest({
        key: 0,
        ids: SI.fromArray([51]),
        extra: Tuple,
        child: IntKeys(
          Switch({
            key: 0,
            ids: SI.empty,
            cases: {
              val: PString("a"),
              ifMatch: Switch({
                key: 1,
                ids: SI.fromArray([112]),
                cases: {
                  val: PString("b"),
                  ifMatch: End(
                    Switch({
                      key: 1,
                      ids: SI.fromArray([117]),
                      cases: {
                        val: PInt(1),
                        ifMatch: End({names: MS.fromArray([("x", 51)]), exit: e(0)}),
                        nextCase: Some({
                          val: PInt(10),
                          ifMatch: End({names: MS.empty, exit: e(1)}),
                          nextCase: None,
                        }),
                      },
                      wildcard: Some(
                        End({
                          names: MS.empty->MS.set("y", 112)->MS.set("z", 117),
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
                      ids: SI.fromArray([117]),
                      cases: {
                        val: PInt(1),
                        ifMatch: End({names: MS.fromArray([("x", 51)]), exit: e(0)}),
                        nextCase: None,
                      },
                      wildcard: Some(
                        End({
                          names: MS.empty->MS.set("y", 112)->MS.set("z", 117),
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
                ids: SI.fromArray([112]),
                child: End(
                  Switch({
                    key: 1,
                    ids: SI.fromArray([117]),
                    cases: {
                      val: PInt(1),
                      ifMatch: End({names: MS.fromArray([("x", 51)]), exit: e(0)}),
                      nextCase: None,
                    },
                    wildcard: Some(
                      End({
                        names: MS.empty->MS.set("y", 112)->MS.set("z", 117),
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
              ifMatch: End({names: MS.fromArray([("x", 51)]), exit: e(0)}),
              nextCase: None,
            },
            wildcard: None,
          }),
        ),
      }),
    )
  })

  test("Nests merge correctly.", ({expect, _}) => {
    let src = `
    {% match a,        b,  c
       with  _,        _, 12 %}
    {% with  _, (20, 21), 22 %}
    {% with  _, (20, 21), 32 %}
    {% with  _, ( _,  _),  _ %}
    {% /match %}`
    let result = compile(src)
    expect.value(result).toEqual(
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
    let src = `
    {% match              a,  b
       with               x, 41 %}
    {% with  ((10, 20), 30), 40 %}
    {% with               y,  z %}
    {% /match %}`
    let result = compile(src)
    expect.value(result).toEqual(
      Nest({
        key: 0,
        ids: SI.empty->SI.add(59)->SI.add(129),
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
                                ids: SI.fromArray([133]),
                                cases: {
                                  val: PInt(40),
                                  ifMatch: End({names: MS.empty, exit: e(1)}),
                                  nextCase: Some({
                                    val: PInt(41),
                                    ifMatch: End({names: MS.fromArray([("x", 59)]), exit: e(0)}),
                                    nextCase: None,
                                  }),
                                },
                                wildcard: Some(
                                  End({
                                    names: MS.empty->MS.set("y", 129)->MS.set("z", 133),
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
            ids: SI.fromArray([133]),
            cases: {
              val: PInt(41),
              ifMatch: End({names: MS.fromArray([("x", 59)]), exit: e(0)}),
              nextCase: None,
            },
            wildcard: Some(
              End({
                names: MS.empty->MS.set("y", 129)->MS.set("z", 133),
                exit: e(2),
              }),
            ),
          }),
        ),
      }),
    )
  })

  test("Different-sized lists merge correctly.", ({expect, _}) => {
    let src = `
    {% match a
       with [] %}
    {% with [x] %}
    {% with [x, ...y] %}
    {% /match %}`
    let result = compile(src)
    expect.value(result).toEqual(
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
                ids: SI.empty->SI.add(47)->SI.add(66),
                child: Construct({
                  key: 1,
                  ids: SI.fromArray([72]),
                  extra: TList,
                  nil: Some(End(End({names: MS.fromArray([("x", 47)]), exit: e(1)}))),
                  cons: Some(
                    Wildcard({
                      key: 1,
                      ids: SI.fromArray([72]),
                      child: End(
                        End({
                          names: MS.empty->MS.set("x", 66)->MS.set("y", 72),
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
    let src = `
    {% match a, b
       with  [10, 11], 12 %}
    {% with  [10, 11, ...x], 22 %}
    {% with  [30], 32 %}
    {% with  y, 42 %}
    {% with  _, _ %}
    {% /match %}`
    let result = compile(src)
    expect.value(result).toEqual(
      Construct({
        key: 0,
        ids: SI.fromArray([121]),
        extra: TList,
        cons: Some(
          Nest({
            key: 0,
            ids: SI.fromArray([121]),
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
                                ids: SI.fromArray([73]),
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
                                              names: MS.fromArray([("x", 73)]),
                                              exit: e(1),
                                            }),
                                            nextCase: Some({
                                              val: PInt(42),
                                              ifMatch: End({
                                                names: MS.fromArray([("y", 121)]),
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
                                    ids: SI.fromArray([73]),
                                    child: End(
                                      End(
                                        Switch({
                                          key: 1,
                                          ids: SI.empty,
                                          cases: {
                                            val: PInt(22),
                                            ifMatch: End({
                                              names: MS.fromArray([("x", 73)]),
                                              exit: e(1),
                                            }),
                                            nextCase: Some({
                                              val: PInt(42),
                                              ifMatch: End({
                                                names: MS.fromArray([("y", 121)]),
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
                                ifMatch: End({names: MS.fromArray([("y", 121)]), exit: e(3)}),
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
                  ifMatch: End({names: MS.fromArray([("y", 121)]), exit: e(3)}),
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
              ifMatch: End({names: MS.fromArray([("y", 121)]), exit: e(3)}),
              nextCase: None,
            },
            wildcard: Some(End({names: MS.empty, exit: e(4)})),
          }),
        ),
      }),
    )
  })

  test("Records sort fields correctly", ({expect, _}) => {
    let src = `
    {% match a, b
       with {a: 10, b: 11}, 12
       with {b: 21, a: 20}, 22
       with _, _ %}
    {% /match %}`
    let result = compile(src)
    expect.value(result).toEqual(
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
    let src = `
    {% match a, b
       with {a: 10, b: 11}, 12
       with {b: 21}, 22
       with _, _ %}
    {% /match %}`
    let result = compile(src)
    expect.value(result).toEqual(
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
    let src = `
    {% match a
       with {b: 10} %}
    {% with {a: 20} %}
    {% with {c: 30} %}
    {% with x %}
    {% /match %}`
    let result = compile(src)
    expect.value(result).toEqual(
      Nest({
        key: 0,
        ids: SI.fromArray([97]),
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
        wildcard: Some(End({names: MS.fromArray([("x", 97)]), exit: e(3)})),
      }),
    )
  })
})
