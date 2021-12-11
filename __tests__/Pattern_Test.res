/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
open TestFramework

module J = Js.Json

let dict = Js.Dict.fromArray

let render = (~name="", ~children=Js.Dict.empty(), src, props, components) =>
  Compile.make(~name, src, Compile.Components.makeExn(components))->Result.flatMap(t =>
    Render.sync(t, props, children)
  )

describe("Escaped strings work", ({test, _}) => {
  test("\\\"", ({expect, _}) => {
    let src = "{% match a with \"\\\"\" ~%} pass {%~ with _ ~%} fail {%~ /match %}"
    let props = dict([("a", J.string("\""))])
    let r = render(src, props, [])
    expect.value(r).toEqual(#ok("pass"))
  })
  test("\\\\", ({expect, _}) => {
    let src = "{% match a with \"\\\\\" ~%} pass {%~ with _ ~%} fail {%~ /match %}"
    let props = dict([("a", J.string("\\"))])
    let r = render(src, props, [])
    expect.value(r).toEqual(#ok("pass"))
  })
})

describe("Equality", ({test, _}) => {
  test("One value", ({expect, _}) => {
    // ???
    // expect.value(patternTest(parseString("{}"), jsonList("{}"))).toEqual(#Ok([]))
    // expect.value(patternTest(parseString("()"), jsonList("[]"))).toEqual(#Ok([]))
    let src = `
      {%~ match a with 1 ~%} pass {% with _ ~%} fail {% /match ~%}
      {%~ match b with "b" ~%} pass {% with _ ~%} fail {% /match ~%}
      {%~ match c with null ~%} pass {% with _ ~%} fail {% /match ~%}
      {%~ match d with true ~%} pass {% with _ ~%} fail {% /match ~%}
      {%~ match e with false ~%} pass {% with _ ~%} fail {% /match ~%}
      {%~ match f with [] ~%} pass {%~ with _ ~%} fail {% /match ~%}
      `
    let props = dict([
      ("a", J.number(1.0)),
      ("b", J.string("b")),
      ("c", J.null),
      ("d", J.boolean(true)),
      ("e", J.boolean(false)),
      ("f", J.array([])),
    ])
    let r = render(src, props, [])
    expect.value(r).toEqual(#ok("pass pass pass pass pass pass"))
  })

  test("Nested values", ({expect, _}) => {
    let src = `
      {%~ match a with {key: true, key2: {key3: false}} ~%} pass {% with _ ~%} fail {%~ /match ~%}
      {%~ match b with <key: {nested: true}, key2: {nested: false}> ~%} pass {% with _ ~%} fail {%~ /match ~%}
      {%~ match c with ["thing 1"] ~%} pass {% with _ ~%} fail {%~ /match ~%}
      {%~ match d with ("thing 1", 2) ~%} pass {% with _ ~%} fail {%~ /match ~%}
      {%~ match e with ["thing 1", "thing 2"] ~%} pass {% with _ ~%} fail {%~ /match ~%}
      {%~ match f with {
        a: true,
        b: false,
        c: null,
        d: 1,
        e: 100.5,
        f: [
          !"g",
          null
        ],
        "h": {
          i: "j",
          k: "l"
        },
        "A complex string!": true
      } ~%} pass {%~ with _ ~%} fail {%~ /match ~%}
    `
    let props = dict([
      ("a", J.parseExn(`{"key": true, "key2": {"key3": false}}`)),
      ("b", J.parseExn(`{"key": {"nested": true}, "key2": {"nested": false}}`)),
      ("c", J.parseExn(`["thing 1"]`)),
      ("d", J.parseExn(`["thing 1", 2]`)),
      ("e", J.parseExn(`["thing 1", "thing 2"]`)),
      (
        "f",
        J.parseExn(`
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
    ])
    let r = render(src, props, [])
    expect.value(r).toEqual(#ok("pass pass pass pass pass pass"))
  })
})

describe("Inequality", ({test, _}) => {
  test("One value", ({expect, _}) => {
    // ???
    // expect.value(patternTest(parseString("{}"), jsonList(`{"a": "b"}`))).toEqual(#NoMatch)
    let src = `
      {%~ match a with 1 ~%} fail {% with _ ~%} pass {% /match ~%}
      {%~ match b with "string" ~%} fail {% with _ ~%} pass {% /match ~%}
      {%~ match c with true ~%} fail {% with _ ~%} pass {% /match ~%}
      {%~ match d with false ~%} fail {% with _ ~%} pass {% /match ~%}
      {%~ match f with [] ~%} fail {% with _ ~%} pass {%~ /match ~%}
      `
    let props = dict([
      ("a", J.number(2.0)),
      ("b", J.string("b")),
      ("c", J.boolean(false)),
      ("d", J.boolean(true)),
      ("f", J.numberArray([1.0])),
    ])
    let r = render(src, props, [])
    expect.value(r).toEqual(#ok("pass pass pass pass pass"))
  })

  test("Nested values", ({expect, _}) => {
    let src = `
      {%~ match a with {a: "b"} ~%} fail {% with _ ~%} pass {% /match ~%}
      {%~ match b with [1] ~%} fail {%~ with _ ~%} pass {% /match ~%}
      {%~ match c with [1] ~%} fail {%~ with _ ~%} pass {% /match ~%}
      {%~ match d with {
        a: true,
        b: false,
        c: null,
        d: 1,
        e: 100.5,
        f: [
          !"g",
          !"NOMATCH"
        ],
        "h": {
          i: "j",
          k: "l"
        },
        "A complex string!": true
      }~%} fail {% with _ ~%} pass {%~ /match ~%}
      `
    let props = dict([
      ("a", J.parseExn(`{"a": "c"}`)),
      ("b", J.array([])),
      ("c", J.numberArray([2.0])),
      (
        "d",
        J.parseExn(`{
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
        }`),
      ),
    ])
    let r = render(src, props, [])
    expect.value(r).toEqual(#ok("pass pass pass pass"))
  })
})

describe("Nullables", ({test, _}) => {
  test("Null data", ({expect, _}) => {
    // ???
    // let null = jsonList("null")
    //expect.value(patternTest(parseString("{}"), null)).toEqual(#NoMatch)
    let src = `
      {%~ match a with !1 ~%} fail {%~ with _ ~%} pass {% /match ~%}
      {%~ match b with !"b" ~%} fail {%~ with _ ~%} pass {% /match ~%}
      {%~ match c with !true ~%} fail {%~ with _ ~%} pass {% /match ~%}
      {%~ match d with !false ~%} fail {%~ with _ ~%} pass {% /match ~%}
      {%~ match e with ![] ~%} fail {%~ with _ ~%} pass {% /match ~%}
      {%~ match f with ![1] ~%} fail {%~ with _ ~%} pass {% /match ~%}
      {%~ match g with ![1, ...g] ~%} fail {%~ with _ ~%} pass {% /match ~%}
      {%~ match h with !{a} ~%} fail {%~ with _ ~%} pass {%~ /match ~%}
    `
    let props = dict([
      ("a", J.null),
      ("b", J.null),
      ("c", J.null),
      ("d", J.null),
      ("e", J.null),
      ("f", J.null),
      ("g", J.null),
      ("h", J.null),
    ])
    let r = render(src, props, [])
    expect.value(r).toEqual(#ok("pass pass pass pass pass pass pass pass"))
  })

  test("Not Null data", ({expect, _}) => {
    let src = `
      {%~ match a with !1 ~%} pass {% with _ ~%} fail {% /match ~%}
      {%~ match b with !"b" ~%} pass {% with _ ~%} fail {% /match ~%}
      {%~ match c with !true ~%} pass {% with _ ~%} fail {% /match ~%}
      {%~ match d with !false ~%} pass {% with _ ~%} fail {% /match ~%}
      {%~ match e with ![] ~%} pass {% with _ ~%} fail {% /match ~%}
      {%~ match f with ![1] ~%} pass {% with _ ~%} fail {% /match ~%}
      {%~ match g with ![1, ...g] ~%} pass {% with _ ~%} fail {% /match ~%}
      {%~ match h with !{a} ~%} pass {%~ with _ ~%} fail {%~ /match ~%}
    `
    let props = dict([
      ("a", J.number(1.0)),
      ("b", J.string("b")),
      ("c", J.boolean(true)),
      ("d", J.boolean(false)),
      ("e", J.array([])),
      ("f", J.numberArray([1.])),
      ("g", J.numberArray([1.])),
      ("h", J.object_(dict([("a", J.null)]))),
    ])
    let r = render(src, props, [])
    expect.value(r).toEqual(#ok("pass pass pass pass pass pass pass pass"))
  })

  test("Null patterns", ({expect, _}) => {
    // ???
    // expect.value(patternTest(null, jsonList("{}"))).toEqual(#NoMatch)
    let src = `
      {%~ match a with null ~%} fail {% with _ ~%} pass {% /match ~%}
      {%~ match b with null ~%} fail {% with _ ~%} pass {% /match ~%}
      {%~ match c with null ~%} fail {% with _ ~%} pass {% /match ~%}
      {%~ match d with null ~%} fail {% with _ ~%} pass {% /match ~%}
      {%~ match e with null ~%} fail {% with _ ~%} pass {%~ /match ~%}
    `
    let props = dict([
      ("a", J.number(1.0)),
      ("b", J.string("b")),
      ("c", J.boolean(true)),
      ("d", J.boolean(false)),
      ("e", J.array([])),
    ])
    let r = render(src, props, [])
    expect.value(r).toEqual(#ok("pass pass pass pass pass"))
  })
})

describe("Binding", ({test, _}) => {
  test("One value", ({expect, _}) => {
    let src = `{%~ match a with x ~%} {{ x }} {%~ /match ~%}`
    let props = dict([("a", J.string("pass"))])
    let r = render(src, props, [])
    expect.value(r).toEqual(#ok("pass"))
  })

  test("Ignore values with `_`", ({expect, _}) => {
    let src = `{%~ match a with [_, _, x] ~%} {{ x }} {%~ with _ ~%} fail {%~ /match ~%}`
    let props = dict([("a", J.stringArray(["fail", "fail", "pass"]))])
    let r = render(src, props, [])
    expect.value(r).toEqual(#ok("pass"))
  })

  test("Nested values", ({expect, _}) => {
    let src = `
      {%~ match a with [x, y] ~%} {{ x }} {{ y }} {% with _ ~%} fail {%~ /match ~%}
      {%~ match b with {key: x, key2: y} ~%} {{ x }} {{ y }} {% with _ ~%} fail {%~ /match ~%}
      {%~ match c with {x, y: !y} ~%} {{ x }} {%~ with _ ~%} pass {% /match ~%}
      {%~ match d with [x, ...rest] ~%}
        {{ x }} {% match rest with [y, z] ~%} {{ y }} {{ z }} {% with _ %} fail {%~ /match ~%}
      {%~ with _ ~%} fail {%~ /match ~%}
      {%~ match e with {
          a,
          b: _,
          c,
          d: d,
          e: _,
          f: [!g, ...rest],
          h: {i, k},
          "A complex string!": j
        } ~%} {{ g }} {{ i }} {%~ with _ ~%} fail {%~ /match ~%}
    `
    let props = dict([
      ("a", J.stringArray(["pass", "pass"])),
      ("b", J.parseExn(`{"key": "pass", "key2": "pass"}`)),
      ("c", J.parseExn(`{"x": "fail"}`)),
      ("d", J.stringArray(["pass", "pass", "pass"])),
      (
        "e",
        J.parseExn(`{
              "a": true,
              "b": false,
              "c": null,
              "d": 1,
              "e": 100.5,
              "f": [
                "pass",
                null
              ],
              "h": {
                "i": "pass",
                "k": null
              },
              "A complex string!": true
            }`),
      ),
    ])
    let r = render(src, props, [])
    expect.value(r).toEqual(#ok("pass pass pass pass pass pass pass pass pass pass"))
  })
})

describe("Multiple patterns and data", ({test, _}) => {
  test("Two bindings", ({expect, _}) => {
    let src = `{% match a, b with 1, x ~%} {{ x }} {%~ with _, _ %} fail {% /match %}`
    let props = dict([("a", J.number(1.0)), ("b", J.string("pass"))])
    let r = render(src, props, [])
    expect.value(r).toEqual(#ok("pass"))
  })

  test("Three bindings", ({expect, _}) => {
    let src = `
      {%~ match a, b, c with 100, x, y ~%} {{ x }} {{ y }} {%~ with _, _, _ %} fail {% /match ~%}
      {%~ match a, b, c with 100, "fail", y ~%} {{ y }} {%~ with _, _, _ %} pass {%~ /match ~%}
    `
    let props = dict([("a", J.number(100.0)), ("b", J.string("pass")), ("c", J.string("pass"))])
    let r = render(src, props, [])
    expect.value(r).toEqual(#ok("pass pass pass"))
  })

  test("Complex", ({expect, _}) => {
    let src = `
      {%~ match a, b, c with {a: true}, {b: false}, [c] ~%} {{ c }} {%~ with _, _, _ %} fail {% /match ~%}
    `
    let props = dict([
      ("a", J.parseExn(`{"a": true}`)),
      ("b", J.parseExn(`{"b": false}`)),
      ("c", J.stringArray(["pass"])),
    ])
    let r = render(src, props, [])
    expect.value(r).toEqual(#ok("pass"))
  })
})
