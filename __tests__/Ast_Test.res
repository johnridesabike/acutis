/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

open TestFramework

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
    Lexer.popExn(tokens)->ignore // Skip the opening string
    Parser.Pattern.make(tokens)
  }

  test("Enums", ({expect, _}) => {
    expect.value(parseString("null")).toEqual(NonEmpty.one(P.UNull({char: 3})))
    expect.value(parseString("!a")).toEqual(
      NonEmpty.one(P.USome({char: 3}, P.UBinding({char: 4}, "a"))),
    )
    expect.value(parseString("false")).toEqual(NonEmpty.one(P.UFalse({char: 3})))
    expect.value(parseString("true")).toEqual(NonEmpty.one(P.UTrue({char: 3})))
  })

  test("Numbers", ({expect, _}) => {
    expect.value(parseString("1")).toEqual(NonEmpty.one(P.UInt({char: 3}, 1)))
    expect.value(parseString("1.")).toEqual(NonEmpty.one(P.UFloat({char: 3}, 1.0)))
    expect.value(parseString("12345")).toEqual(NonEmpty.one(P.UInt({char: 3}, 12345)))
    expect.value(parseString("1234.5")).toEqual(NonEmpty.one(P.UFloat({char: 3}, 1234.5)))
    expect.value(parseString("-12345")).toEqual(NonEmpty.one(P.UInt({char: 3}, -12345)))
    expect.value(parseString("1e+1")).toEqual(NonEmpty.one(P.UInt({char: 3}, 10)))
    expect.value(parseString("-1E+1")).toEqual(NonEmpty.one(P.UInt({char: 3}, -10)))
    expect.value(parseString("175.0e-2")).toEqual(NonEmpty.one(P.UFloat({char: 3}, 1.75)))
    expect.value(parseString("175e-2")).toEqual(NonEmpty.one(P.UInt({char: 3}, 1)))
  })

  test("Strings", ({expect, _}) => {
    expect.value(parseString(`"a"`)).toEqual(NonEmpty.one(P.UString({char: 3}, "a")))
    expect.value(parseString(`"a b c"`)).toEqual(NonEmpty.one(P.UString({char: 3}, "a b c")))
    expect.value(parseString("\"a says \\\"b\\\" to c\"")).toEqual(
      NonEmpty.one(P.UString({char: 3}, `a says "b" to c`)),
    )
  })

  test("Bindings", ({expect, _}) => {
    expect.value(parseString("a")).toEqual(NonEmpty.one(P.UBinding({char: 3}, "a")))
    expect.value(parseString("_")).toEqual(NonEmpty.one(P.UBinding({char: 3}, "_")))
    expect.value(parseString("abc123")).toEqual(NonEmpty.one(P.UBinding({char: 3}, "abc123")))
    expect.value(parseString("a_")).toEqual(NonEmpty.one(P.UBinding({char: 3}, "a_")))
  })

  test("Arrays", ({expect, _}) => {
    expect.value(parseString("[]")).toEqual(NonEmpty.one(P.UList({char: 3}, [])))
    expect.value(parseString("[1]")).toEqual(
      NonEmpty.one(P.UList({char: 3}, [P.UInt({char: 4}, 1)])),
    )
    expect.value(parseString("[null]")).toEqual(
      NonEmpty.one(P.UList({char: 3}, [P.UNull({char: 4})])),
    )
    expect.value(parseString(`["a"]`)).toEqual(
      NonEmpty.one(P.UList({char: 3}, [P.UString({char: 4}, "a")])),
    )
    expect.value(parseString(`[1, "a", null]`)).toEqual(
      NonEmpty.one(
        P.UList({char: 3}, [P.UInt({char: 4}, 1), P.UString({char: 7}, "a"), P.UNull({char: 12})]),
      ),
    )
    expect.value(parseString(`["b", x]`)).toEqual(
      NonEmpty.one(P.UList({char: 3}, [P.UString({char: 4}, "b"), P.UBinding({char: 9}, "x")])),
    )
    expect.value(parseString(`["b", ...x]`)).toEqual(
      NonEmpty.one(
        P.UListWithTailBinding({char: 3}, [P.UString({char: 4}, "b")], P.UBinding({char: 12}, "x")),
      ),
    )
    expect.value(parseString(`[["b", 1], x]`)).toEqual(
      NonEmpty.one(
        P.UList(
          {char: 3},
          [
            P.UList({char: 4}, [P.UString({char: 5}, "b"), P.UInt({char: 10}, 1)]),
            P.UBinding({char: 14}, "x"),
          ],
        ),
      ),
    )
    expect.value(parseString("[[], x]")).toEqual(
      NonEmpty.one(P.UList({char: 3}, [P.UList({char: 4}, []), P.UBinding({char: 8}, "x")])),
    )
  })

  test("Objects", ({expect, _}) => {
    expect.value(parseString("{}")).toEqual(NonEmpty.one(P.URecord({char: 3}, [])))
    expect.value(parseString("{pun}")).toEqual(
      NonEmpty.one(P.URecord({char: 3}, [("pun", P.UBinding({char: 4}, "pun"))])),
    )
    expect.value(parseString("{pun1, pun2}")).toEqual(
      NonEmpty.one(
        P.URecord(
          {char: 3},
          [("pun1", P.UBinding({char: 4}, "pun1")), ("pun2", P.UBinding({char: 10}, "pun2"))],
        ),
      ),
    )
    expect.value(parseString("{a: b}")).toEqual(
      NonEmpty.one(P.URecord({char: 3}, [("a", P.UBinding({char: 7}, "b"))])),
    )
    expect.value(parseString("{a: b, c: d}")).toEqual(
      NonEmpty.one(
        P.URecord(
          {char: 3},
          [("a", P.UBinding({char: 7}, "b")), ("c", P.UBinding({char: 13}, "d"))],
        ),
      ),
    )
    expect.value(parseString(`{"#illegal": legal, "<%name%>": name}`)).toEqual(
      NonEmpty.one(
        P.URecord(
          {char: 3},
          [
            ("#illegal", P.UBinding({char: 16}, "legal")),
            ("<%name%>", P.UBinding({char: 35}, "name")),
          ],
        ),
      ),
    )
    expect.value(parseString(`{a: 1.5, b: "b", c: null, d}`)).toEqual(
      NonEmpty.one(
        P.URecord(
          {char: 3},
          [
            ("a", P.UFloat({char: 7}, 1.5)),
            ("b", P.UString({char: 15}, "b")),
            ("c", P.UNull({char: 23})),
            ("d", P.UBinding({char: 29}, "d")),
          ],
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
          {char: 4},
          [
            ("a", P.UBinding({char: 11}, "bindingA")),
            ("b", P.UFloat({char: 26}, 1.5)),
            (
              "c",
              P.UListWithTailBinding(
                {char: 36},
                [P.UString({char: 41}, "item1"), P.UBinding({char: 54}, "bindingC")],
                P.UBinding({char: 71}, "rest"),
              ),
            ),
            (
              "d",
              P.URecord(
                {char: 86},
                [
                  ("<% illegal %>", P.UNull({char: 109})),
                  ("d", P.UBinding({char: 122}, "bindingD")),
                ],
              ),
            ),
            ("e", P.UBinding({char: 141}, "e")),
          ],
        ),
      ),
    )
  })

  test("Multiple patterns", ({expect, _}) => {
    expect.value(parseString(`true, "a", b`)).toEqual(
      NonEmpty.fromArrayExn([
        P.UTrue({char: 3}),
        P.UString({char: 9}, "a"),
        P.UBinding({char: 14}, "b"),
      ]),
    )
  })
})

describe("Parser", ({test, _}) => {
  test("Basic syntax", ({expect, _}) => {
    expect.value(
      Parser.makeExn(
        ~name="",
        `
a
{* b *}
{{ c }}
{{ "d" }}
{{ 1.5 }}
{{ &e }}
f`,
      ),
    ).toEqual([
      UText("\na\n", NoTrim),
      UText("\n", NoTrim),
      UEcho({
        loc: {char: 13},
        nullables: [],
        default: EBinding({char: 14}, "c", Escape),
      }),
      UText("\n", NoTrim),
      UEcho({
        loc: {char: 21},
        nullables: [],
        default: EString({char: 22}, "d", Escape),
      }),
      UText("\n", NoTrim),
      UEcho({
        loc: {char: 31},
        nullables: [],
        default: EFloat({char: 32}, 1.5, Escape),
      }),
      UText("\n", NoTrim),
      UEcho({
        loc: {char: 41},
        nullables: [],
        default: EBinding({char: 43}, "e", NoEscape),
      }),
      UText("\nf", NoTrim),
    ])
  })
  test("Matching", ({expect, _}) => {
    expect.value(
      Parser.makeExn(
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
      ),
    ).toMatchSnapshot()
  })
  test("Mapping", ({expect, _}) => {
    expect.value(
      Parser.makeExn(
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
      ),
    ).toMatchSnapshot()
  })
  test("Components", ({expect, _}) => {
    expect.value(
      Parser.makeExn(
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
      ),
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
