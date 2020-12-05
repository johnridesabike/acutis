/**
   Copyright 2020 John Jackson

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
  let tokens = Lexer.make("{% " ++ source ++ "%}")
  Lexer.skipExn(tokens) // skip the opening string
  Compile.Pattern.make(tokens)
}

let json = Js.Json.parseExn
let jsonList = (x): NonEmpty.t<Js.Json.t> => List(json(x), list{})
let dict = Js.Dict.fromArray
let emptyBindings = Js.Dict.empty()

describe("Equality", ({test, _}) => {
  test("One value", ({expect, _}) => {
    expect.value(Pattern.test(parseString("1"), jsonList("1"))).toEqual(Ok(emptyBindings))
    expect.value(Pattern.test(parseString(`"a"`), jsonList(`"a"`))).toEqual(Ok(emptyBindings))
    expect.value(Pattern.test(parseString("null"), jsonList("null"))).toEqual(Ok(emptyBindings))
    expect.value(Pattern.test(parseString("true"), jsonList("true"))).toEqual(Ok(emptyBindings))
    expect.value(Pattern.test(parseString("false"), jsonList("false"))).toEqual(Ok(emptyBindings))
    expect.value(Pattern.test(parseString("{}"), jsonList("{}"))).toEqual(Ok(emptyBindings))
    expect.value(Pattern.test(parseString("[]"), jsonList("[]"))).toEqual(Ok(emptyBindings))
  })

  test("Nested values", ({expect, _}) => {
    expect.value(
      Pattern.test(
        parseString("{key: true, key2: {key3: false}}"),
        jsonList(`{"key": true, "key2": {"key3": false}}`),
      ),
    ).toEqual(Ok(emptyBindings))
    expect.value(Pattern.test(parseString(`["thing 1"]`), jsonList(`["thing 1"]`))).toEqual(
      Ok(emptyBindings),
    )
    expect.value(
      Pattern.test(parseString(`["thing 1", "thing 2"]`), jsonList(`["thing 1", "thing 2"]`)),
    ).toEqual(Ok(emptyBindings))
    expect.value(
      Pattern.test(
        parseString(
          `
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
         `,
        ),
        jsonList(
          `
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
         `,
        ),
      ),
    ).toEqual(Ok(emptyBindings))
  })
})

describe("Inequality", ({test, _}) => {
  test("One value", ({expect, _}) => {
    expect.value(Pattern.test(parseString("1"), jsonList("2"))).toEqual(Error(NoMatch))
    expect.value(Pattern.test(parseString(`"string"`), jsonList(`"a"`))).toEqual(Error(NoMatch))
    expect.value(Pattern.test(parseString("true"), jsonList("false"))).toEqual(Error(NoMatch))
    expect.value(Pattern.test(parseString("false"), jsonList("true"))).toEqual(Error(NoMatch))
    expect.value(Pattern.test(parseString("{}"), jsonList(`{"a": "b"}`))).toEqual(Error(NoMatch))
    expect.value(Pattern.test(parseString(`{a: "b"}`), jsonList("{}"))).toEqual(Error(NoMatch))
    expect.value(Pattern.test(parseString("[]"), jsonList("[1]"))).toEqual(Error(NoMatch))
    expect.value(Pattern.test(parseString("[1]"), jsonList("[]"))).toEqual(Error(NoMatch))
    expect.value(Pattern.test(parseString("[1]"), jsonList("[2]"))).toEqual(Error(NoMatch))
  })

  test("Nested values", ({expect, _}) => {
    expect.value(
      Pattern.test(
        parseString(
          `
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
         `,
        ),
        jsonList(
          `
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
         `,
        ),
      ),
    ).toEqual(Error(NoMatch))
  })
})

describe("Nullables", ({test, _}) => {
  test("Null data", ({expect, _}) => {
    let null = jsonList("null")
    expect.value(Pattern.test(parseString("1"), null)).toEqual(Error(NoMatch))
    expect.value(Pattern.test(parseString(`"a"`), null)).toEqual(Error(NoMatch))
    expect.value(Pattern.test(parseString("true"), null)).toEqual(Error(NoMatch))
    expect.value(Pattern.test(parseString("false"), null)).toEqual(Error(NoMatch))
    expect.value(Pattern.test(parseString("[]"), null)).toEqual(Error(NoMatch))
    expect.value(Pattern.test(parseString("[1]"), null)).toEqual(Error(NoMatch))
    expect.value(Pattern.test(parseString("[1, ...a]"), null)).toEqual(Error(NoMatch))
    expect.value(Pattern.test(parseString("{}"), null)).toEqual(Error(NoMatch))
    expect.value(Pattern.test(parseString("{a}"), null)).toEqual(Error(NoMatch))
  })

  test("Null patterns", ({expect, _}) => {
    let null = parseString("null")
    expect.value(Pattern.test(null, jsonList("1"))).toEqual(Error(NoMatch))
    expect.value(Pattern.test(null, jsonList(`"a"`))).toEqual(Error(NoMatch))
    expect.value(Pattern.test(null, jsonList("true"))).toEqual(Error(NoMatch))
    expect.value(Pattern.test(null, jsonList("false"))).toEqual(Error(NoMatch))
    expect.value(Pattern.test(null, jsonList("[]"))).toEqual(Error(NoMatch))
    expect.value(Pattern.test(null, jsonList("{}"))).toEqual(Error(NoMatch))
  })
})

describe("Binding", ({test, _}) => {
  test("One value", ({expect, _}) => {
    expect.value(Pattern.test(parseString("x"), jsonList("1"))).toEqual(
      Ok(dict([("x", json("1"))])),
    )
  })

  test("Ignore values with `_`", ({expect, _}) => {
    expect.value(Pattern.test(parseString("[_, _, a]"), jsonList("[1, 2, 3]"))).toEqual(
      Ok(dict([("a", json("3"))])),
    )
  })

  test("Nested values", ({expect, _}) => {
    expect.value(Pattern.test(parseString("[x, y]"), jsonList(`["thing 1", "thing 2"]`))).toEqual(
      Ok(dict([("x", json(`"thing 1"`)), ("y", json(`"thing 2"`))])),
    )
    expect.value(
      Pattern.test(parseString("{key: x, key2: y}"), jsonList(`{"key": true, "key2": false}`)),
    ).toEqual(Ok(dict([("x", json("true")), ("y", json("false"))])))
    expect.value(Pattern.test(parseString("{x, y}"), jsonList(`{"x": true}`))).toEqual(
      Error(NoMatch),
    )
    expect.value(
      Pattern.test(parseString("[x, ...rest]"), jsonList(`["thing 1", "thing 2", "thing 3"]`)),
    ).toEqual(Ok(dict([("x", json(`"thing 1"`)), ("rest", json(`["thing 2", "thing 3"]`))])))
    expect.value(
      Pattern.test(
        parseString(
          `
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
          `,
        ),
        jsonList(
          `
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
      `,
        ),
      ),
    ).toEqual(
      Ok(
        dict([
          ("a", json("true")),
          ("c", json("null")),
          ("d", json("1")),
          ("g", json(`"g"`)),
          ("i", json(`"j"`)),
          ("k", json("null")),
          ("j", json("true")),
          ("rest", json("[null]")),
        ]),
      ),
    )
  })
})

describe("Multiple patterns and data", ({test, _}) => {
  test("Two bindings", ({expect, _}) => {
    expect.value(Pattern.test(parseString("a, 1"), List(json("0"), list{json("1")}))).toEqual(
      Ok(dict([("a", json("0"))])),
    )
  })

  test("Three bindings", ({expect, _}) => {
    expect.value(
      Pattern.test(parseString("100, a, b"), List(json("100"), list{json("true"), json("false")})),
    ).toEqual(Ok(dict([("a", json("true")), ("b", json("false"))])))
  })

  test("Complex", ({expect, _}) => {
    expect.value(
      Pattern.test(
        parseString("{a: true} , {b: false} , [c]"),
        List(json(`{"a": true}`), list{json(`{"b": false}`), json("[true]")}),
      ),
    ).toEqual(Ok(dict([("c", json("true"))])))
  })
})

let parseString = source => {
  let tokens = Lexer.make("{% " ++ source ++ " %}")
  Lexer.skipExn(tokens) // skip the opening string
  Compile.Pattern.parseNode(tokens)
}

describe("Encoding to JSON", ({test, _}) => {
  let props = dict([
    ("a", Js.Json.number(1.0)),
    ("b", Js.Json.string("b")),
    ("rest", Js.Json.booleanArray([true, false])),
  ])

  test("Encoding", ({expect, _}) => {
    expect.value(
      parseString(
        `
          {
            a,
            b,
            c: [],
            d: {},
            e: [1, 2],
            f: {g: 1},
            h: [true, false, null, ...rest],
            i: ["j"]
          }`,
      )->Pattern.toJson(~props),
    ).toEqual(
      Ok(
        json(
          `
          {
            "a": 1.0,
            "b": "b",
            "c": [],
            "d": {},
            "e": [1, 2],
            "f": {"g": 1},
            "h": [true, false, null, true, false],
            "i": ["j"]
            }`,
        ),
      ),
    )
  })

  test("Encoding errors", ({expect, _}) => {
    let pattern = parseString(`[a, ...b]`)
    expect.value(pattern->Pattern.toJson(~props)).toEqual(
      Error(
        BindingTypeMismatchErr({
          data: JSONString("b"),
          pattern: pattern,
          binding: Id("b"),
        }),
      ),
    )
    let pattern = parseString(`c`)
    expect.value(pattern->Pattern.toJson(~props)).toEqual(
      Error(
        BindingDoesNotExistErr({
          loc: Loc(3),
          binding: Id("c"),
        }),
      ),
    )
  })
})
