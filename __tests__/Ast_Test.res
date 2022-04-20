/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

open TestFramework

module MapString = Belt.Map.String
module P = Parser.Pattern

describe("Lexer", ({test, _}) => {
  test("Tokens are generated correctly", ({expect, _}) => {
    expect.value(
      `
{% Abc d e=#%} {{ "f" }} {%/# /%}
{% match g with {h: true, i: ["j"], k} %}
  {{ k }}
{% with {h: false, l: ["m", ...rest]} %}
  {%~ map rest
    \t with {n, o}, 0 ~%}
    First {{ n }} {{ o }}
    {% with {n, o}, index %}
    {{ index }} {{ n }} {{ o }}
  {%~ /map ~%}
{% /match %}
{{~ -1.5 ~}}
{ p %
{* " \\\\ *}
{* nested {* comment *} *}
{{ " \\\\\\\\ " }}
{{ " \\\\" " }}
{{ \t a
}}
{* {q*r *}
{% match t with (1.5, "2") %}
  {{ S }}
{% /match %}`
      ->Lexer.make(~name="")
      ->Lexer.debugToArray,
    ).toMatchSnapshot()
  })
})

describe("Patterns", ({test, _}) => {
  let parseString = source => {
    let tokens = Lexer.make("{% " ++ source ++ "%}", ~name="")
    Lexer.pop(tokens)->ignore // Skip the opening string
    Parser.Pattern.make(tokens)
  }

  test("Enums", ({expect, _}) => {
    expect.value(parseString("null")).toEqual(NonEmpty.one(P.UNullable(Debug.make("", 3), None)))
    expect.value(parseString("!a")).toEqual(
      NonEmpty.one(P.UNullable(Debug.make("", 3), Some(UBinding(Debug.make("", 4), "a")))),
    )
    expect.value(parseString("false")).toEqual(NonEmpty.one(P.UBool(Debug.make("", 3), 0)))
    expect.value(parseString("true")).toEqual(NonEmpty.one(P.UBool(Debug.make("", 3), 1)))
  })

  test("Numbers", ({expect, _}) => {
    expect.value(parseString("1")).toEqual(NonEmpty.one(P.UInt(Debug.make("", 3), 1)))
    expect.value(parseString("1.")).toEqual(NonEmpty.one(P.UFloat(Debug.make("", 3), 1.0)))
    expect.value(parseString("12345")).toEqual(NonEmpty.one(P.UInt(Debug.make("", 3), 12345)))
    expect.value(parseString("1234.5")).toEqual(NonEmpty.one(P.UFloat(Debug.make("", 3), 1234.5)))
    expect.value(parseString("-12345")).toEqual(NonEmpty.one(P.UInt(Debug.make("", 3), -12345)))
    expect.value(parseString("1e+1")).toEqual(NonEmpty.one(P.UInt(Debug.make("", 3), 10)))
    expect.value(parseString("-1E+1")).toEqual(NonEmpty.one(P.UInt(Debug.make("", 3), -10)))
    expect.value(parseString("175.0e-2")).toEqual(NonEmpty.one(P.UFloat(Debug.make("", 3), 1.75)))
    expect.value(parseString("175e-2")).toEqual(NonEmpty.one(P.UInt(Debug.make("", 3), 1)))
  })

  test("Strings", ({expect, _}) => {
    expect.value(parseString(`"a"`)).toEqual(NonEmpty.one(P.UString(Debug.make("", 3), "a")))
    expect.value(parseString(`"a b c"`)).toEqual(
      NonEmpty.one(P.UString(Debug.make("", 3), "a b c")),
    )
    expect.value(parseString("\"a says \\\"b\\\" to c\"")).toEqual(
      NonEmpty.one(P.UString(Debug.make("", 3), `a says "b" to c`)),
    )
  })

  test("Bindings", ({expect, _}) => {
    expect.value(parseString("a")).toEqual(NonEmpty.one(P.UBinding(Debug.make("", 3), "a")))
    expect.value(parseString("_")).toEqual(NonEmpty.one(P.UBinding(Debug.make("", 3), "_")))
    expect.value(parseString("abc123")).toEqual(
      NonEmpty.one(P.UBinding(Debug.make("", 3), "abc123")),
    )
    expect.value(parseString("a_")).toEqual(NonEmpty.one(P.UBinding(Debug.make("", 3), "a_")))
  })

  test("Arrays", ({expect, _}) => {
    expect.value(parseString("[]")).toEqual(NonEmpty.one(P.UList(Debug.make("", 3), [], None)))
    expect.value(parseString("[1]")).toEqual(
      NonEmpty.one(P.UList(Debug.make("", 3), [P.UInt(Debug.make("", 4), 1)], None)),
    )
    expect.value(parseString("[null]")).toEqual(
      NonEmpty.one(P.UList(Debug.make("", 3), [P.UNullable(Debug.make("", 4), None)], None)),
    )
    expect.value(parseString(`["a"]`)).toEqual(
      NonEmpty.one(P.UList(Debug.make("", 3), [P.UString(Debug.make("", 4), "a")], None)),
    )
    expect.value(parseString(`[1, "a", null]`)).toEqual(
      NonEmpty.one(
        P.UList(
          Debug.make("", 3),
          [
            P.UInt(Debug.make("", 4), 1),
            P.UString(Debug.make("", 7), "a"),
            P.UNullable(Debug.make("", 12), None),
          ],
          None,
        ),
      ),
    )
    expect.value(parseString(`["b", x]`)).toEqual(
      NonEmpty.one(
        P.UList(
          Debug.make("", 3),
          [P.UString(Debug.make("", 4), "b"), P.UBinding(Debug.make("", 9), "x")],
          None,
        ),
      ),
    )
    expect.value(parseString(`["b", ...x]`)).toEqual(
      NonEmpty.one(
        P.UList(
          Debug.make("", 3),
          [P.UString(Debug.make("", 4), "b")],
          Some(UBinding(Debug.make("", 12), "x")),
        ),
      ),
    )
    expect.value(parseString(`[["b", 1], x]`)).toEqual(
      NonEmpty.one(
        P.UList(
          Debug.make("", 3),
          [
            P.UList(
              Debug.make("", 4),
              [P.UString(Debug.make("", 5), "b"), P.UInt(Debug.make("", 10), 1)],
              None,
            ),
            P.UBinding(Debug.make("", 14), "x"),
          ],
          None,
        ),
      ),
    )
    expect.value(parseString("[[], x]")).toEqual(
      NonEmpty.one(
        P.UList(
          Debug.make("", 3),
          [P.UList(Debug.make("", 4), [], None), P.UBinding(Debug.make("", 8), "x")],
          None,
        ),
      ),
    )
  })

  test("Objects", ({expect, _}) => {
    expect.value(parseString("{}")).toEqual(
      NonEmpty.one(P.URecord(Debug.make("", 3), None, MapString.empty)),
    )
    expect.value(parseString("{pun}")).toEqual(
      NonEmpty.one(
        P.URecord(
          Debug.make("", 3),
          None,
          MapString.fromArray([("pun", P.UBinding(Debug.make("", 4), "pun"))]),
        ),
      ),
    )
    expect.value(parseString("{pun1, pun2}")).toEqual(
      NonEmpty.one(
        P.URecord(
          Debug.make("", 3),
          None,
          MapString.empty
          ->MapString.set("pun1", P.UBinding(Debug.make("", 4), "pun1"))
          ->MapString.set("pun2", P.UBinding(Debug.make("", 10), "pun2")),
        ),
      ),
    )
    expect.value(parseString("{a: b}")).toEqual(
      NonEmpty.one(
        P.URecord(
          Debug.make("", 3),
          None,
          MapString.fromArray([("a", P.UBinding(Debug.make("", 7), "b"))]),
        ),
      ),
    )
    expect.value(parseString("{a: b, c: d}")).toEqual(
      NonEmpty.one(
        P.URecord(
          Debug.make("", 3),
          None,
          MapString.empty
          ->MapString.set("a", P.UBinding(Debug.make("", 7), "b"))
          ->MapString.set("c", P.UBinding(Debug.make("", 13), "d")),
        ),
      ),
    )
    expect.value(parseString(`{"#illegal": legal, "<%name%>": name}`)).toEqual(
      NonEmpty.one(
        P.URecord(
          Debug.make("", 3),
          None,
          MapString.empty
          ->MapString.set("#illegal", P.UBinding(Debug.make("", 16), "legal"))
          ->MapString.set("<%name%>", P.UBinding(Debug.make("", 35), "name")),
        ),
      ),
    )
    expect.value(parseString(`{a: 1.5, b: "b", c: null, d}`)).toEqual(
      NonEmpty.one(
        P.URecord(
          Debug.make("", 3),
          None,
          MapString.empty
          ->MapString.set("a", P.UFloat(Debug.make("", 7), 1.5))
          ->MapString.set("b", P.UString(Debug.make("", 15), "b"))
          ->MapString.set("c", P.UNullable(Debug.make("", 23), None))
          ->MapString.set("d", P.UBinding(Debug.make("", 29), "d")),
        ),
      ),
    )
    let result = parseString(`
{
  a: bindingA,
  b: 1.5,
  c: [\r
\t\t"item1",
    bindingC,
    ...rest
  ],
  d: {
    "<% illegal %>": null,
    d: bindingD
  },
  e: e
}`)
    expect.value(result).toEqual(
      NonEmpty.one(
        P.URecord(
          Debug.make("", 4),
          None,
          MapString.empty
          ->MapString.set("a", P.UBinding(Debug.make("", 11), "bindingA"))
          ->MapString.set("b", P.UFloat(Debug.make("", 26), 1.5))
          ->MapString.set(
            "c",
            P.UList(
              Debug.make("", 36),
              [P.UString(Debug.make("", 41), "item1"), P.UBinding(Debug.make("", 54), "bindingC")],
              Some(UBinding(Debug.make("", 71), "rest")),
            ),
          )
          ->MapString.set(
            "d",
            P.URecord(
              Debug.make("", 86),
              None,
              MapString.empty
              ->MapString.set("<% illegal %>", P.UNullable(Debug.make("", 109), None))
              ->MapString.set("d", P.UBinding(Debug.make("", 122), "bindingD")),
            ),
          )
          ->MapString.set("e", P.UBinding(Debug.make("", 141), "e")),
        ),
      ),
    )
  })

  test("Multiple patterns", ({expect, _}) => {
    expect.value(parseString(`true, "a", b`)).toEqual(
      NonEmpty.fromArrayExn([
        P.UBool(Debug.make("", 3), 1),
        P.UString(Debug.make("", 9), "a"),
        P.UBinding(Debug.make("", 14), "b"),
      ]),
    )
  })
})

describe("Parser", ({test, _}) => {
  test("Basic syntax", ({expect, _}) => {
    expect.value(
      Lexer.make(
        ~name="",
        `
a
{* b *}
{{ c }}
{{ "d" }}
{{ &e }}
f`,
      )->Parser.make,
    ).toEqual([
      UText("\na\n", NoTrim),
      UText("\n", NoTrim),
      UEcho(Debug.make("", 13), [], EBinding(Debug.make("", 14), "c", Escape)),
      UText("\n", NoTrim),
      UEcho(Debug.make("", 21), [], EString(Debug.make("", 22), "d", Escape)),
      UText("\n", NoTrim),
      UEcho(Debug.make("", 31), [], EBinding(Debug.make("", 33), "e", NoEscape)),
      UText("\nf", NoTrim),
    ])
  })
  test("Matching", ({expect, _}) => {
    expect.value(
      Lexer.make(
        ~name="",
        `
{% match a
   with 1 %}
  b
{% with 2
   with 3 %}
  c
{% with 4 %}
  d
{% /match %}
{% match e, f
   with true, {g} %}
  {{ g }}
{% with false, _ %}
  h
{% /match %}
{% match tuple, dict with (i, "j"), <k: l> %}
  m
{% /match %}
`,
      )->Parser.make,
    ).toMatchSnapshot()
  })
  test("Mapping", ({expect, _}) => {
    expect.value(
      Lexer.make(
        ~name="",
        `
{% map a with {b} %}
  {{ b }}
{% /map %}
{% map c
   with {d: true, e} %}
  {{ e }}
{% with {d: false, f} %}
  {{ f }}
{% /map %}
{% map g with {h}, i %}
  {{ i }} {{ j }}
{% /map %}
`,
      )->Parser.make,
    ).toMatchSnapshot()
  })
  test("Components", ({expect, _}) => {
    expect.value(
      Lexer.make(
        ~name="",
        `
{% A
   b
   c=1
   d="e"
   f=true
   G=#%}
     h {{ i }} {* j *}
   {%/#
   %}
   k
   {% L m={n: o, p: [q], r} / %}
{% /A %}
`,
      )->Parser.make,
    ).toMatchSnapshot()
  })
})

describe("Components", ({test, _}) => {
  test("Components are only compiled once", ({expect, _}) => {
    let a = Source.src(~name="A", "{% B /%}")
    let b = Source.src(~name="B", "{% C /%}")
    let c = Source.src(~name="C", "c")
    let d = Source.src(~name="D", "{% C /%}")
    let result = Compile.Components.make([a, b, c, d])
    // This assertion doesn't really check that the test worked.
    // It exists for code coverage.
    expect.value(Result.map(result, _ => true)).toEqual(#ok(true))
  })
})
