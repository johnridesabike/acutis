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

module NonEmpty = Acutis_Types.NonEmpty
module Pattern = Render.Pattern

let parseString = source => {
  let tokens = Lexer.make(~name="", "{% " ++ source ++ "%}")
  Lexer.popExn(tokens)->ignore // skip the opening string
  Compile.Pattern.make(tokens)
}

let json = Js.Json.parseExn
let jsonList = (x): NonEmpty.t<_> => NonEmpty(json(x), [])
let jsonList2 = (x): NonEmpty.t<_> => NonEmpty(x, [])

let patternTest = (patterns, json) =>
  switch Pattern.test(patterns, json, ~loc=Loc(0), ~stack=list{}) {
  | NoMatch => #NoMatch
  | Result(#ok(d)) => #Ok(Belt.Map.String.toArray(d))
  | Result(#errors(_)) => raise(Not_found)
  }

describe("Escaped strings work", ({test, _}) => {
  test("\\\"", ({expect, _}) => {
    expect.value(patternTest(parseString("\"\\\"\""), jsonList2(Js.Json.string("\"")))).toEqual(
      #Ok([]),
    )
  })
  test("\\\\", ({expect, _}) => {
    expect.value(patternTest(parseString("\"\\\\\""), jsonList2(Js.Json.string("\\")))).toEqual(
      #Ok([]),
    )
  })
})

describe("Equality", ({test, _}) => {
  test("One value", ({expect, _}) => {
    expect.value(patternTest(parseString("1"), jsonList("1"))).toEqual(#Ok([]))
    expect.value(patternTest(parseString(`"a"`), jsonList(`"a"`))).toEqual(#Ok([]))
    expect.value(patternTest(parseString("null"), jsonList("null"))).toEqual(#Ok([]))
    expect.value(patternTest(parseString("true"), jsonList("true"))).toEqual(#Ok([]))
    expect.value(patternTest(parseString("false"), jsonList("false"))).toEqual(#Ok([]))
    expect.value(patternTest(parseString("{}"), jsonList("{}"))).toEqual(#Ok([]))
    expect.value(patternTest(parseString("[]"), jsonList("[]"))).toEqual(#Ok([]))
  })

  test("Nested values", ({expect, _}) => {
    expect.value(
      patternTest(
        parseString("{key: true, key2: {key3: false}}"),
        jsonList(`{"key": true, "key2": {"key3": false}}`),
      ),
    ).toEqual(#Ok([]))
    expect.value(patternTest(parseString(`["thing 1"]`), jsonList(`["thing 1"]`))).toEqual(#Ok([]))
    expect.value(
      patternTest(parseString(`["thing 1", "thing 2"]`), jsonList(`["thing 1", "thing 2"]`)),
    ).toEqual(#Ok([]))
    expect.value(
      patternTest(
        parseString(`
          {
            a: true,
            b: false,
            c: null,
            d: 1,
            e: 100.5,
            f: [
              "g",
              null
            ],
            "h": {
              i: "j",
              k: "l"
            },
            "A complex string!": true
          } 
         `),
        jsonList(`
           {
             "a": true,
             "b": false,
             "c": null,
             "d": 1,
             "e": 100.5,
             "f": [
               "g",
               null
             ],
             "h": {
                  "i": "j",
               "k": "l"
             },
             "A complex string!": true
           }
         `),
      ),
    ).toEqual(#Ok([]))
  })
})

describe("Inequality", ({test, _}) => {
  test("One value", ({expect, _}) => {
    expect.value(patternTest(parseString("1"), jsonList("2"))).toEqual(#NoMatch)
    expect.value(patternTest(parseString(`"string"`), jsonList(`"a"`))).toEqual(#NoMatch)
    expect.value(patternTest(parseString("true"), jsonList("false"))).toEqual(#NoMatch)
    expect.value(patternTest(parseString("false"), jsonList("true"))).toEqual(#NoMatch)
    expect.value(patternTest(parseString("{}"), jsonList(`{"a": "b"}`))).toEqual(#NoMatch)
    expect.value(patternTest(parseString(`{a: "b"}`), jsonList("{}"))).toEqual(#NoMatch)
    expect.value(patternTest(parseString("[]"), jsonList("[1]"))).toEqual(#NoMatch)
    expect.value(patternTest(parseString("[1]"), jsonList("[]"))).toEqual(#NoMatch)
    expect.value(patternTest(parseString("[1]"), jsonList("[2]"))).toEqual(#NoMatch)
  })

  test("Nested values", ({expect, _}) => {
    expect.value(
      patternTest(
        parseString(`
          {
            a: true,
            b: false,
            c: null,
            d: 1,
            e: 100.5,
            f: [
              "g",
              null
            ],
            "h": {
              i: "j",
              k: "l"
            },
            "A complex string!": true
          } 
         `),
        jsonList(`
           {
             "a": false,
             "b": false,
             "c": null,
             "d": 1,
             "e": 100.5,
             "f": [
               "g",
               null
             ],
             "h": {
                  "i": "j",
               "k": "l"
             },
             "A complex string!": true
           }
         `),
      ),
    ).toEqual(#NoMatch)
  })
})

describe("Nullables", ({test, _}) => {
  test("Null data", ({expect, _}) => {
    let null = jsonList("null")
    expect.value(patternTest(parseString("1"), null)).toEqual(#NoMatch)
    expect.value(patternTest(parseString(`"a"`), null)).toEqual(#NoMatch)
    expect.value(patternTest(parseString("true"), null)).toEqual(#NoMatch)
    expect.value(patternTest(parseString("false"), null)).toEqual(#NoMatch)
    expect.value(patternTest(parseString("[]"), null)).toEqual(#NoMatch)
    expect.value(patternTest(parseString("[1]"), null)).toEqual(#NoMatch)
    expect.value(patternTest(parseString("[1, ...a]"), null)).toEqual(#NoMatch)
    expect.value(patternTest(parseString("{}"), null)).toEqual(#NoMatch)
    expect.value(patternTest(parseString("{a}"), null)).toEqual(#NoMatch)
  })

  test("Null patterns", ({expect, _}) => {
    let null = parseString("null")
    expect.value(patternTest(null, jsonList("1"))).toEqual(#NoMatch)
    expect.value(patternTest(null, jsonList(`"a"`))).toEqual(#NoMatch)
    expect.value(patternTest(null, jsonList("true"))).toEqual(#NoMatch)
    expect.value(patternTest(null, jsonList("false"))).toEqual(#NoMatch)
    expect.value(patternTest(null, jsonList("[]"))).toEqual(#NoMatch)
    expect.value(patternTest(null, jsonList("{}"))).toEqual(#NoMatch)
  })
})

describe("Binding", ({test, _}) => {
  test("One value", ({expect, _}) => {
    expect.value(patternTest(parseString("x"), jsonList("1"))).toEqual(#Ok([("x", json("1"))]))
  })

  test("Ignore values with `_`", ({expect, _}) => {
    expect.value(patternTest(parseString("[_, _, a]"), jsonList("[1, 2, 3]"))).toEqual(
      #Ok([("a", json("3"))]),
    )
  })

  test("Nested values", ({expect, _}) => {
    expect.value(patternTest(parseString("[x, y]"), jsonList(`["thing 1", "thing 2"]`))).toEqual(
      #Ok([("x", json(`"thing 1"`)), ("y", json(`"thing 2"`))]),
    )
    expect.value(
      patternTest(parseString("{key: x, key2: y}"), jsonList(`{"key": true, "key2": false}`)),
    ).toEqual(#Ok([("x", json("true")), ("y", json("false"))]))
    expect.value(patternTest(parseString("{x, y}"), jsonList(`{"x": true}`))).toEqual(#NoMatch)
    expect.value(
      patternTest(parseString("[x, ...rest]"), jsonList(`["thing 1", "thing 2", "thing 3"]`)),
    ).toEqual(#Ok([("rest", json(`["thing 2", "thing 3"]`)), ("x", json(`"thing 1"`))]))
    expect.value(
      patternTest(
        parseString(`
            {
              a,
              b: _,
              c,
              d: d,
              e: _,
              f: [g, ...rest],
              h: {i, k},
              "A complex string!": j
            } 
          `),
        jsonList(`
            {
              "a": true,
              "b": false,
              "c": null,
              "d": 1,
              "e": 100.5,
              "f": [
                "g",
                null
              ],
              "h": {
                "i": "j",
                "k": null
              },
              "A complex string!": true
            }
      `),
      ),
    ).toEqual(
      #Ok([
        ("a", json("true")),
        ("c", json("null")),
        ("d", json("1")),
        ("g", json(`"g"`)),
        ("i", json(`"j"`)),
        ("j", json("true")),
        ("k", json("null")),
        ("rest", json("[null]")),
      ]),
    )
  })
})

describe("Multiple patterns and data", ({test, _}) => {
  test("Two bindings", ({expect, _}) => {
    expect.value(patternTest(parseString("a, 1"), NonEmpty(json("0"), [json("1")]))).toEqual(
      #Ok([("a", json("0"))]),
    )
  })

  test("Three bindings", ({expect, _}) => {
    expect.value(
      patternTest(parseString("100, a, b"), NonEmpty(json("100"), [json("true"), json("false")])),
    ).toEqual(#Ok([("a", json("true")), ("b", json("false"))]))
    expect.value(
      patternTest(
        parseString("100, true, true"),
        NonEmpty(json("100"), [json("true"), json("false")]),
      ),
    ).toEqual(#NoMatch)
  })

  test("Complex", ({expect, _}) => {
    expect.value(
      patternTest(
        parseString("{a: true} , {b: false} , [c]"),
        NonEmpty(json(`{"a": true}`), [json(`{"b": false}`), json("[true]")]),
      ),
    ).toEqual(#Ok([("c", json("true"))]))
  })
})

let parseString = source => {
  let tokens = Lexer.make(~name="", "{% " ++ source ++ " %}")
  Lexer.popExn(tokens)->ignore // skip the opening string
  let NonEmpty.NonEmpty(result, _) = Compile.Pattern.make(tokens)
  result
}

describe("Encoding to JSON", ({test, _}) => {
  let props = Js.Dict.fromArray([
    ("a", Js.Json.number(1.0)),
    ("b", Js.Json.string("b")),
    ("rest", Js.Json.booleanArray([true, false])),
  ])

  test("Encoding", ({expect, _}) => {
    expect.value(
      parseString(`
          {
            a,
            b,
            c: [],
            d: {},
            e: [1, 2],
            f: {g: 1},
            h: [true, false, null, ...rest],
            i: ["j"]
          }`)->Pattern.toJson(~props, ~stack=list{}),
    ).toEqual(
      #ok(
        json(`
          {
            "a": 1.0,
            "b": "b",
            "c": [],
            "d": {},
            "e": [1, 2],
            "f": {"g": 1},
            "h": [true, false, null, true, false],
            "i": ["j"]
            }`),
      ),
    )
  })

  test("Encoding errors", ({expect, _}) => {
    let pattern = parseString(`[a, ...b]`)
    expect.value(pattern->Pattern.toJson(~props, ~stack=list{})).toEqual(
      #errors([
        {
          message: `"b" is type array but the data is type string.`,
          kind: #Type,
          location: Some({character: 4}),
          path: [],
          exn: None,
        },
      ]),
    )
    let pattern = parseString(`c`)
    expect.value(pattern->Pattern.toJson(~props, ~stack=list{})).toEqual(
      #errors([
        {
          message: `Binding "c" does not exist.`,
          kind: #Render,
          location: Some({character: 4}),
          path: [],
          exn: None,
        },
      ]),
    )
  })
})
