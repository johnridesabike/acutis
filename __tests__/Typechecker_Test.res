/**
  Copyright (c) 2021 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

open TestFramework
module MapString = Belt.Map.String

let isOk = r =>
  switch r {
  | #ok(_) => true
  | #errors(_) => false
  }

let prop_types = x => x.Compile.prop_types

let get_types = (~components=Compile.Components.empty(), src) =>
  Compile.make(~name="test", src, components)
  ->Result.getOrElse(e => {
    Js.log(e)
    assert false
  })
  ->prop_types
  ->MapString.map(Typescheme.toString)
  ->MapString.toArray

describe("basic", ({test, _}) => {
  test("pattern", ({expect, _}) => {
    let src = `
    {% match a
       with {a: true, b: !"lol", c: null, d: [true, false]}
       with {a: false, b: null, c: !1.0, z: 1} %}
    {% with _ %}
    {% /match %}`
    let result = get_types(src)
    expect.value(result).toEqual([
      ("a", `{"a": false | true, "b": ?string, "c": ?float, "d": [false | true], "z": int}`),
    ])
  })
})

describe("match", ({test, _}) => {
  test("basic 1", ({expect, _}) => {
    let src = `
    {% match a with !1 %} {% with null %} {% with !_ %} {% /match %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([("a", `?int`)])
  })
  test("Typechecker nested", ({expect, _}) => {
    let src = `
    {% match a with {b} %}
      {% match b with !1 %} {% with null %} {% with !_ %} {% /match %}
    {% /match %}
    `
    let bindingsGlobal = get_types(src)
    expect.value(bindingsGlobal).toEqual([("a", `{"b": ?int}`)])
    let src = `
    {% match a with {b: {c}, d } %}
      {% match c with !1 %} {{ d }} {% with null %} {% with !_ %} {% /match %}
    {% /match %}
    `
    let bindingsGlobal = get_types(src)
    expect.value(bindingsGlobal).toEqual([("a", `{"b": {"c": ?int}, "d": echoable}`)])
  })

  test("Typechecker multiple patterns", ({expect, _}) => {
    let src = `
    {% match a, b with {c}, 1 %}
      {% match c with !{d: 1} %} {% with null %} {% with !_ %} {% /match %}
    {% with _, _ %}
    {% /match %}
    `
    let bindingsGlobal = get_types(src)
    expect.value(bindingsGlobal).toEqual([("a", `{"c": ?{"d": int}}`), ("b", `int`)])
  })

  test("echoes", ({expect, _}) => {
    let src = `
      {{ a ? b }}
      {% match c with {d, e} %}
        {{ d }} {{ e }}
      {% /match %}
      {% match c with {e: 1} %} {% with _ %}
      {% /match %}
      {% match c with {f} %}
        {% match f with {g: !""} %}
          z
        {% with {g: null} %}
          z
        {% with {g} %}
          {{ g ? "g" }}
        {% /match %}
      {% /match %}
    `
    let bindingsGlobal = get_types(src)
    expect.value(bindingsGlobal).toEqual([
      ("a", `?echoable`),
      ("b", `echoable`),
      ("c", `{"d": echoable, "e": int, "f": {"g": ?string}}`),
    ])
  })

  test("Type narrowing works when constructing values", ({expect, _}) => {
    let src = `
      {% match x with {a, b} %} {{ a }} {{ b }} {% /match %}
      {% match y with {c} %} {{ c }} {% /match %}
      {% map [{a: "a", b: 1}, {a: "a", c: true}, x, y] with {a} %}
        {{ a }}
      {% /map %}
      `
    let result = get_types(src)
    expect.value(result).toEqual([
      ("x", `{"a": string, "b": echoable}`),
      ("y", `{"a": string, "c": echoable}`),
    ])
  })

  test("Inferrence works for nested types", ({expect, _}) => {
    let src = `{% match a with [{a: 1}, c, {b: "b"}] %} {% with _ %} {% /match %}`
    let bindingsGlobal = get_types(src)
    expect.value(bindingsGlobal).toEqual([("a", `[{"a": int, "b": string}]`)])
    let src = `{% match a with [{@tag: 0, a: 1}, c, {@tag: 1, b: "b"}] %} {% with _ %} {% /match %}`
    let bindingsGlobal = get_types(src)
    expect.value(bindingsGlobal).toEqual([
      ("a", `[{@"tag": 0, "a": int} | {@"tag": 1, "b": string} | ...]`),
    ])
  })
})

describe("enums", ({test, _}) => {
  test("open enums work ", ({expect, _}) => {
    let src = `
    {% match a with @"a" %} {% with @"b" %} {% with _ %} {% /match %}
    {% match b with @1 %} {% with @2 %} {% with _ %} {% /match %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([("a", `@"a" | @"b" | ...`), ("b", "@1 | @2 | ...")])
    let src = `
    {% match a with @"a" %} {% with @"b" %} {% with _ %} {% /match %}
    {% map [@"c", a] with _ %} {% /map %}
    {% match b with @1 %} {% with @2 %} {% with _ %} {% /match %}
    {% map [@3, b] with _ %} {% /map %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([("a", `@"a" | @"b" | @"c" | ...`), ("b", "@1 | @2 | @3 | ...")])
  })

  test("closed enums work", ({expect, _}) => {
    let src = `
    {% match a with @"a" %} {% with @"b" %} {% /match %}
    {% match b with @1 %} {% with @2 %} {% /match %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([("a", `@"a" | @"b"`), ("b", "@1 | @2")])
    let src = `
    {% match a with @"a" %} {% with @"b" %} {% /match %}
    {% match a with @"a" %} {% with _ %} {% /match %}
    {% match b with @1 %} {% with @2 %} {% /match %}
    {% match b with @3 %} {% with _ %} {% /match %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([("a", `@"a" | @"b"`), ("b", "@1 | @2")])
    let src = `
    {% match a with @"a" %} {% with @"b" %} {% /match %}
    {% map [@"c", @"d", a, b] with _ %} {% /map %}
    {% match c with @1 %} {% with @2 %} {% /match %}
    {% map [@3, @4, c, d] with _ %} {% /map %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([
      ("a", `@"a" | @"b"`),
      ("b", `@"a" | @"b" | @"c" | @"d" | ...`),
      ("c", `@1 | @2`),
      ("d", `@1 | @2 | @3 | @4 | ...`),
    ])
    let src = `
    {% map [@"a", a] with @"a" %} {% with @"b" %} {% /map %}
    {% map [@1, b] with @1 %} {% with @2 %} {% /map %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([("a", `@"a" | @"b"`), ("b", "@1 | @2")])
  })

  test("Nested closed enums work", ({expect, _}) => {
    let src = `
    {% match a with {a: @"a"} %} {% with {a: @"b"} %} {% /match %}
    {% match b with {a: @1} %} {% with {a: @2} %} {% /match %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([("a", `{"a": @"a" | @"b"}`), ("b", `{"a": @1 | @2}`)])
    let src = `
    {% match a with {a: @"a"} %} {% with {a: @"b"} %} {% /match %}
    {% map [{a: @"c"}, a, b] with _ %} {% /map %}
    {% match c with {a: @1} %} {% with {a: @2} %} {% /match %}
    {% map [{a: @3}, c, d] with _ %} {% /map %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([
      ("a", `{"a": @"a" | @"b"}`),
      ("b", `{"a": @"a" | @"b" | @"c" | ...}`),
      ("c", `{"a": @1 | @2}`),
      ("d", `{"a": @1 | @2 | @3 | ...}`),
    ])
  })

  test("Booleans", ({expect, _}) => {
    let src = `
    {% match a with true %} {% /match %}
    {% match b with false %} {% /match %}
    {% match c with true %} {% with false %} {% /match %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([("a", "true"), ("b", "false"), ("c", "false | true")])
    let src = `{% match true with true %} {% with false %} {% /match %}`
    let result = isOk(Compile.make(~name="test", src, Compile.Components.empty()))
    expect.bool(result).toBe(true)
    let src = `{% map [true, false] with true %} {% with false %} {% /map %}`
    let result = isOk(Compile.make(~name="test", src, Compile.Components.empty()))
    expect.bool(result).toBe(true)
    let src = `{% match a with true %} {% with _ %} {% /match %}`
    let bindings = get_types(src)
    expect.value(bindings).toEqual([("a", "false | true")])
  })
})

describe("Tagged unions", ({test, _}) => {
  test("open unions work ", ({expect, _}) => {
    let src = `
    {% match a with {@tag: "a", b} %} {{ b }} {% with {@tag: "b", b: 1} %} {% with _ %} {% /match %}
    {% match b with {@tag: 0, b} %} {{ b }} {% with {@tag: 1, b: 1} %} {% with _ %} {% /match %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([
      ("a", `{@"tag": "a", "b": echoable} | {@"tag": "b", "b": int} | ...`),
      ("b", `{@"tag": 0, "b": echoable} | {@"tag": 1, "b": int} | ...`),
    ])
    let src = `
    {% match a with {@tag: "a", b} %} {{ b }} {% with {@tag: "b", b: 1} %} {% with _ %} {% /match %}
    {% map [{@tag: "c", b: 1.5}, a] with _ %} {% /map %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([
      (
        "a",
        `{@"tag": "a", "b": echoable} | {@"tag": "b", "b": int} | {@"tag": "c", "b": float} | ...`,
      ),
    ])
  })
  test("Closed unions work", ({expect, _}) => {
    let src = `
    {% match a
       with {@tag: 1, b: "a"} %} {%
       with {@tag: 2, b: 0} %} {%
       with {@tag: 1} with {@tag: 2} %} {%
      /match %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([("a", `{@"tag": 1, "b": string} | {@"tag": 2, "b": int}`)])
    let src = `
    {% match a
      with {@tag: "a", b: 1} %} {%
      with {@tag: "b", b: "c"} %} {%
      with {@tag: "a"} %} {%
      with {@tag: "b"} %} {%
    /match %}
    {% match a with {@tag: "a", b: 2} %} {% with _ %} {% /match %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([("a", `{@"tag": "a", "b": int} | {@"tag": "b", "b": string}`)])
    let src = `
    {% match a
      with {@tag: "a", b: 1} %} {%
      with {@tag: "b", b: "a"} %} {%
      with {@tag: "a"} %} {%
      with {@tag: "b"} %} {%
    /match %}
    {% map [{@tag: "c", b: 1.5}, {@tag: "d", b: [1]}, a, b] with _ %} {% /map %}
    {% match c
      with {@tag: 0, b: 1} %} {%
      with {@tag: 1, b: "a"} %} {%
      with {@tag: 0} %} {%
      with {@tag: 1} %} {%
    /match %}
    {% map [{@tag: 2, b: 1.5}, {@tag: 3, b: [1]}, c, d] with _ %} {% /map %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([
      ("a", `{@"tag": "a", "b": int} | {@"tag": "b", "b": string}`),
      (
        "b",
        `{@"tag": "a", "b": int} | {@"tag": "b", "b": string} | {@"tag": "c", "b": float} | {@"tag": "d", "b": [int]} | ...`,
      ),
      ("c", `{@"tag": 0, "b": int} | {@"tag": 1, "b": string}`),
      (
        "d",
        `{@"tag": 0, "b": int} | {@"tag": 1, "b": string} | {@"tag": 2, "b": float} | {@"tag": 3, "b": [int]} | ...`,
      ),
    ])
    let src = `
    {% map [{@tag: "a", b: 1}, a] with {@tag: "a", b} %} {% with {@tag: "b", c} %} {{ c }} {% /map %}
    {% map [{@tag: 0, b: 1}, b] with {@tag: 0, b} %} {% with {@tag: 1, c} %} {{ c }} {% /map %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([
      ("a", `{@"tag": "a", "b": int} | {@"tag": "b", "c": echoable}`),
      ("b", `{@"tag": 0, "b": int} | {@"tag": 1, "c": echoable}`),
    ])
  })
  test("Booleans", ({expect, _}) => {
    let src = `
    {% match a with {@tag: true, b} %} {{ b }} {% /match %}
    {% match b with {@tag: false, b} %} {{ b }} {% /match %}
    {% match c with {@tag: true, b} %} {{ b }} {% with {@tag: false, c} %} {{ c }} {% /match %}
    `
    let bindings = get_types(src)
    expect.value(bindings).toEqual([
      ("a", `{@"tag": true, "b": echoable}`),
      ("b", `{@"tag": false, "b": echoable}`),
      ("c", `{@"tag": false, "c": echoable} | {@"tag": true, "b": echoable}`),
    ])
    let src = `{% match a with {@tag: true, b} %} {{ b }} {% with _ %} {% /match %}`
    let bindings = get_types(src)
    expect.value(bindings).toEqual([("a", `{@"tag": false} | {@"tag": true, "b": echoable}`)])
  })
})

describe("component", ({test, _}) => {
  test("basic component", ({expect, _}) => {
    let a = Source.src(
      ~name="A",
      `{% map a with x %} {{ x }} {% /map %}
       {% map b with x %} {{ x }} {% /map %}`,
    )
    let src = `
    {% A a=[1, a] b=["b", b] /%}
    `
    let bindings = get_types(src, ~components=Compile.Components.make([a])->Result.getExn)
    expect.value(bindings).toEqual([("a", `int`), ("b", `string`)])
  })
})
