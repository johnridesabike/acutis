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
open TypeChecker
module T = Acutis_Types
module Queue = Belt.MutableQueue
module MutMapString = Belt.MutableMap.String
module MapString = Belt.Map.String

type rec debug = [
  | #Polymorphic
  | #Boolean
  | #Int
  | #Float
  | #String
  | #Echo
  | #Nullable(debug)
  | #Array(debug)
  | #Tuple(array<debug>)
  | #Dict(debug)
  | #Record(array<(string, debug)>)
]
let rec debug = (x): debug =>
  switch x.contents {
  | Polymorphic => #Polymorphic
  | Boolean => #Boolean
  | Int => #Int
  | Float => #Float
  | String => #String
  | Echo => #Echo
  | Nullable(x) => #Nullable(debug(x))
  | Array(x) => #Array(debug(x))
  | Tuple(x) => #Tuple(Array.map(x.contents, debug))
  | Dict(x) => #Dict(debug(x))
  | Record(x) => #Record(MapString.map(x.contents, debug)->MapString.toArray)
  }

let catch = (f): Result.t<_> =>
  try {
    #ok(f())
  } catch {
  | Debug.Exit(e) => #errors([e])
  }

describe("basic", ({test, _}) => {
  test("pattern", ({expect, _}) => {
    let pat1: T.Ast_Pattern.t = #Object(
      Loc(1),
      [
        ("a", #True(Loc(1))),
        ("b", #String(Loc(1), "lol")),
        ("c", #Null(Loc(1))),
        ("d", #Array(Loc(1), [#True(Loc(1)), #False(Loc(1))])),
      ],
    )
    let pat2: T.Ast_Pattern.t = #Object(
      Loc(1),
      [
        ("a", #False(Loc(1))),
        ("b", #Null(Loc(1))),
        ("c", #Float(Loc(1), 1.0)),
        ("z", #Int(Loc(1), 1)),
      ],
    )
    let (t1, _) = Local.fromPattern(pat1, Context.make())
    let (t2, _) = Local.fromPattern(pat2, Context.make())
    unify(t1, t2, Incomplete, ~loc=Loc(1))
    expect.value(debug(t1)).toEqual(
      #Record([
        ("a", #Boolean),
        ("b", #Nullable(#String)),
        ("c", #Nullable(#Float)),
        ("d", #Array(#Boolean)),
        ("z", #Int),
      ]),
    )
  })
})

describe("match", ({test, _}) => {
  test("basic 1", ({expect, _}) => {
    let src = `
    {% match a with 1 %} {% with null %} {% /match %}
    `
    let nodes = Compile.makeAstInternalExn(~name="test", src)
    let bindings = make(nodes)->MapString.map(debug)->MapString.toArray
    expect.value(bindings).toEqual([("a", #Nullable(#Int))])
  })
  test("Typechecker nested", ({expect, _}) => {
    let src = `
    {% match a with {b} %}
      {% match b with 1 %} {% with null %} {% /match %}
    {% /match %}
    `
    let nodes = Compile.makeAstInternalExn(~name="test", src)
    let bindingsGlobal = make(nodes)->MapString.map(debug)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([("a", #Record([("b", #Nullable(#Int))]))])
    let src = `
    {% match a with {b: {c}, d } %}
      {% match c with 1 %} {{ d }} {% with null %} {% /match %}
    {% /match %}
    `
    let nodes = Compile.makeAstInternalExn(~name="test", src)
    let bindingsGlobal = make(nodes)->MapString.map(debug)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([
      ("a", #Record([("b", #Record([("c", #Nullable(#Int))])), ("d", #Echo)])),
    ])
  })

  test("Typechecker multiple patterns", ({expect, _}) => {
    let src = `
    {% match a, b with {c}, 1 %}
      {% match c with {d: 1} %} {% with null %} {% /match %}
    {% /match %}
    `
    let nodes = Compile.makeAstInternalExn(~name="test", src)
    let bindingsGlobal = make(nodes)->MapString.map(debug)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([
      ("a", #Record([("c", #Nullable(#Record([("d", #Int)])))])),
      ("b", #Int),
    ])
  })

  test("echoes", ({expect, _}) => {
    let src = `
      {{ a ? b }}
      {% match c with {d, e} %}
        {{ d }} {{ e }}
      {% /match %}
      {% match c with {e: 1} %}
      {% /match %}
      {% match c with {f} %}
        {% match f with {g} %}
          {{ g ? "g" }}
        {% with {g: ""} %}
          z
        {% with {g: null} %}
          z
        {% /match %}
      {% /match %}
    `
    let nodes = Compile.makeAstInternalExn(~name="test", src)
    let bindingsGlobal = make(nodes)->MapString.map(debug)->MapString.toArray
    expect.value(bindingsGlobal).toEqual([
      ("a", #Nullable(#Echo)),
      ("b", #Echo),
      ("c", #Record([("d", #Echo), ("e", #Int), ("f", #Record([("g", #Nullable(#String))]))])),
    ])
  })
})

describe("component", ({test, _}) => {
  test("basic component", ({expect, _}) => {
    let src = `
    {% A a=[1, a] b=["b", b] /%}
    `
    let nodes = Compile.makeAstInternalExn(~name="test", src)
    let bindings = make(nodes)->MapString.map(debug)->MapString.toArray
    expect.value(bindings).toEqual([("a", #Int), ("b", #String)])
  })
})

describe("complete vs incomplete", ({test, _}) => {
  test("nullable - global scope", ({expect, _}) => {
    let src = `
    {% match a with 1 %} {% with null %} {% /match %}
    {% match b with 1 %} {% /match %}
    {% match b with null %} {% /match %}
    `
    let bindings = catch(() => {
      let nodes = Compile.makeAstInternalExn(~name="test", src)
      make(nodes)->MapString.map(debug)->MapString.toArray
    })
    expect.value(bindings).toEqual(
      #errors([
        {
          exn: None,
          kind: #Type,
          location: Some({character: 107}),
          message: "This is type int but expected type nullable(polymorphic).",
          path: [],
        },
      ]),
    )
  })
  test("nullable - local scope", ({expect, _}) => {
    let src = `
    {% match a with "a" with null %} {{ a ? "default" }} {% /match %}
    `
    let nodes = Compile.makeAstInternalExn(~name="test", src)
    let bindings = make(nodes)->MapString.map(debug)->MapString.toArray
    expect.value(bindings).toEqual([("a", #Nullable(#String))])
  })
})

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
    let result = Matching.make(NonEmpty(case1, [case2, case3])).tree
    expect.value(result).toEqual(
      Test({
        default: None,
        tests: {
          val: TInt(1),
          ifmatch: Test({
            default: None,
            tests: {
              val: TInt(2),
              ifmatch: Test({
                default: None,
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
                  default: None,
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
              default: None,
              tests: {
                val: TInt(20),
                ifmatch: Test({
                  default: None,
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
                default: None,
                tests: {
                  val: TInt(102),
                  ifmatch: Test({
                    default: None,
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
                      default: None,
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

    let result = Matching.make(NonEmpty(case1, [])).tree
    expect.value(result).toEqual(
      Test({
        tests: {
          val: TInt(10),
          ifmatch: Test({
            default: None,
            tests: {
              val: TInt(11),
              ifmatch: Test({
                default: None,
                tests: {
                  val: TInt(12),
                  ifmatch: Leaf(0),
                  ifnomatch: None,
                },
              }),
              ifnomatch: Some({
                val: TInt(21),
                ifmatch: Test({
                  default: None,
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
              tests: {
                val: TInt(21),
                ifmatch: Test({
                  default: None,
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
                    default: None,
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
              default: Some(
                Test({
                  default: None,
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
        default: Some(
          Test({
            default: None,
            tests: {
              val: TInt(21),
              ifmatch: Test({
                default: None,
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
    let result = Matching.make(NonEmpty(case1, [])).tree
    expect.value(result).toEqual(
      Tuple({
        contents: Test({
          tests: {
            val: TInt(10),
            ifmatch: Test({
              tests: {
                val: TInt(12),
                ifmatch: End(
                  Test({
                    tests: {
                      val: TInt(13),
                      ifmatch: Leaf(0),
                      ifnomatch: Some({
                        val: TInt(33),
                        ifmatch: Leaf(0),
                        ifnomatch: None,
                      }),
                    },
                    default: None,
                  }),
                ),
                ifnomatch: Some({
                  val: TInt(22),
                  ifmatch: End(
                    Test({
                      tests: {
                        val: TInt(23),
                        ifmatch: Leaf(0),
                        ifnomatch: Some({
                          val: TInt(33),
                          ifmatch: Leaf(0),
                          ifnomatch: None,
                        }),
                      },
                      default: None,
                    }),
                  ),
                  ifnomatch: None,
                }),
              },
              default: None,
            }),
            ifnomatch: None,
          },
          default: None,
        }),
        default: Some(
          Test({
            tests: {
              val: TInt(33),
              ifmatch: Leaf(0),
              ifnomatch: None,
            },
            default: None,
          }),
        ),
      }),
    )
  })
})

/*
describe("exhaustive", ({test, _}) => {
  test("nonexaustive tuple 1", ({expect, _}) => {
    let pats: T.NonEmpty.t<T.Ast_Pattern.t> = NonEmpty(
      #Tuple(Loc(0), [#True(Loc(0)), #True(Loc(0))]),
      [#Tuple(Loc(0), [#True(Loc(0)), #False(Loc(0))])],
    )
    let result = Exhaustive.make(pats)
    expect.value(result).toEqual(
      Some(
        Tuple(
          TupleNode({
            val: True,
            child: TupleNode({
              val: True,
              child: TupleNil,
              sibling: TupleNode({
                val: False,
                child: TupleNil,
                sibling: TupleNil,
              }),
            }),
            sibling: TupleNil,
          }),
        ),
      ),
    )
  })
  /*
  test("nonexaustive tuple 2", ({expect, _}) => {
    let pats: T.NonEmpty.t<T.Ast_Pattern.t> = NonEmpty(
      #Tuple(Loc(0), [#True(Loc(0)), #True(Loc(0))]),
      [
        #Tuple(Loc(0), [#True(Loc(0)), #False(Loc(0))]),
        #Tuple(Loc(0), [#True(Loc(0)), #False(Loc(0))]),
      ],
    )
    let result = Exhaustive.make(pats)
    expect.value(result).toEqual(None)
  })
  test("nonexaustive tuple 3", ({expect, _}) => {
    let pats: T.NonEmpty.t<T.Ast_Pattern.t> = NonEmpty(
      #Tuple(Loc(0), [#True(Loc(0)), #True(Loc(0))]),
      [#Tuple(Loc(0), [#Binding(Loc(0), "_"), #False(Loc(0))])],
    )
    let result = Exhaustive.make(pats)
    expect.value(result).toEqual(
      Some(
        Tuple(
          TupleNode({
            val: True,
            child: TupleNode({
              val: True,
              child: TupleNil,
              sibling: TupleNil,
            }),
            sibling: TupleNode({
              val: Exhaustive,
              child: TupleNode({
                val: False,
                child: TupleNil,
                sibling: TupleNil,
              }),
              sibling: TupleNil,
            }),
          }),
        ),
      ),
    )
  })
  test("exhaustive tuple 1", ({expect, _}) => {
    let pats: T.NonEmpty.t<T.Ast_Pattern.t> = NonEmpty(
      #Tuple(Loc(0), [#True(Loc(0)), #True(Loc(0))]),
      [
        #Tuple(Loc(0), [#True(Loc(0)), #False(Loc(0))]),
        #Tuple(Loc(0), [#False(Loc(0)), #False(Loc(0))]),
        #Tuple(Loc(0), [#False(Loc(0)), #True(Loc(0))]),
      ],
    )
    let result = Exhaustive.make(pats)
    expect.value(result).toEqual(Some(Exhaustive))
  })
  test("exhaustive tuple 2", ({expect, _}) => {
    let pats: T.NonEmpty.t<T.Ast_Pattern.t> = NonEmpty(
      #Tuple(Loc(0), [#True(Loc(0)), #True(Loc(0))]),
      [
        #Tuple(Loc(0), [#True(Loc(0)), #False(Loc(0))]),
        #Tuple(Loc(0), [#False(Loc(0)), #False(Loc(0))]),
        #Tuple(Loc(0), [#False(Loc(0)), #True(Loc(0))]),
        #Tuple(Loc(0), [#False(Loc(0)), #True(Loc(0))]),
      ],
    )
    let result = Exhaustive.make(pats)
    expect.value(result).toEqual(None)
  })
  test("exhaustive tuple 2", ({expect, _}) => {
    let pats: T.NonEmpty.t<T.Ast_Pattern.t> = NonEmpty(
      #Tuple(Loc(0), [#True(Loc(0)), #True(Loc(0))]),
      [
        #Tuple(Loc(0), [#True(Loc(0)), #False(Loc(0))]),
        #Tuple(Loc(0), [#False(Loc(0)), #Binding(Loc(0), "_")]),
      ],
    )
    let result = Exhaustive.make(pats)
    expect.value(result).toEqual(Some(Exhaustive))
  })
  test("exhaustive tuple 3", ({expect, _}) => {
    let pats: T.NonEmpty.t<T.Ast_Pattern.t> = NonEmpty(
      #Tuple(Loc(0), [#True(Loc(0)), #True(Loc(0))]),
      [#Tuple(Loc(0), [#Binding(Loc(0), "_"), #Binding(Loc(0), "_")])],
    )
    let result = Exhaustive.make(pats)
    expect.value(result).toEqual(Some(Exhaustive))
  })
 */
})
*/
