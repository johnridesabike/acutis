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

let getAst = ({ast, _}: Acutis_Types.Ast.t) => ast

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
{{ " \\\\ " }}
{{ " \\\\" " }}
{{ \t a
}}
{* {q*r *}
{{ S }}`
      ->Lexer.make
      ->Lexer.debugToArray,
    ).toMatchSnapshot()
  })
})

describe("Patterns", ({test, _}) => {
  let parseString = source => ("{% " ++ source ++ "%}")->Lexer.make->Compile.Pattern.make

  test("Enums", ({expect, _}) => {
    expect.value(parseString("null")).toEqual(List(Null(Loc(3)), list{}))
    expect.value(parseString("false")).toEqual(List(False(Loc(3)), list{}))
    expect.value(parseString("true")).toEqual(List(True(Loc(3)), list{}))
  })

  test("Numbers", ({expect, _}) => {
    expect.value(parseString("1")).toEqual(List(Number(Loc(3), 1.0), list{}))
    expect.value(parseString("12345")).toEqual(List(Number(Loc(3), 12345.0), list{}))
    expect.value(parseString("1234.5")).toEqual(List(Number(Loc(3), 1234.5), list{}))
    expect.value(parseString("-12345")).toEqual(List(Number(Loc(3), -12345.0), list{}))
    expect.value(parseString("1e+1")).toEqual(List(Number(Loc(3), 10.0), list{}))
    expect.value(parseString("-1E+1")).toEqual(List(Number(Loc(3), -10.0), list{}))
  })

  test("Strings", ({expect, _}) => {
    expect.value(parseString(`"a"`)).toEqual(List(String(Loc(3), "a"), list{}))
    expect.value(parseString(`"a b c"`)).toEqual(List(String(Loc(3), "a b c"), list{}))
    expect.value(parseString("\"a says \\\"b\\\" to c\"")).toEqual(
      List(String(Loc(3), `a says "b" to c`), list{}),
    )
  })

  test("Bindings", ({expect, _}) => {
    expect.value(parseString("a")).toEqual(List(Binding(Loc(3), Id("a")), list{}))
    expect.value(parseString("_")).toEqual(List(Binding(Loc(3), Id("_")), list{}))
    expect.value(parseString("abc123")).toEqual(List(Binding(Loc(3), Id("abc123")), list{}))
    expect.value(parseString("a_")).toEqual(List(Binding(Loc(3), Id("a_")), list{}))
  })

  test("Arrays", ({expect, _}) => {
    expect.value(parseString("[]")).toEqual(List(EmptyArray(Loc(3)), list{}))
    expect.value(parseString("[1]")).toEqual(
      List(Array({loc: Loc(3), hd: Number(Loc(4), 1.0), tl: list{}}), list{}),
    )
    expect.value(parseString("[null]")).toEqual(
      List(Array({loc: Loc(3), hd: Null(Loc(4)), tl: list{}}), list{}),
    )
    expect.value(parseString(`["a"]`)).toEqual(
      List(Array({loc: Loc(3), hd: String(Loc(4), "a"), tl: list{}}), list{}),
    )
    expect.value(parseString(`[1, "a", null]`)).toEqual(
      List(
        Array({loc: Loc(3), hd: Number(Loc(4), 1.0), tl: list{String(Loc(7), "a"), Null(Loc(12))}}),
        list{},
      ),
    )
    expect.value(parseString(`["b", x]`)).toEqual(
      List(
        Array({loc: Loc(3), hd: String(Loc(4), "b"), tl: list{Binding(Loc(9), Id("x"))}}),
        list{},
      ),
    )
    expect.value(parseString(`["b", ...x]`)).toEqual(
      List(
        ArrayWithTailBinding({
          loc: Loc(3),
          hd: String(Loc(4), "b"),
          tl: list{},
          bindLoc: Loc(12),
          binding: Id("x"),
        }),
        list{},
      ),
    )
    expect.value(parseString(`[["b", 1], x]`)).toEqual(
      List(
        Array({
          loc: Loc(3),
          hd: Array({loc: Loc(4), hd: String(Loc(5), "b"), tl: list{Number(Loc(10), 1.0)}}),
          tl: list{Binding(Loc(14), Id("x"))},
        }),
        list{},
      ),
    )
    expect.value(parseString("[[], x]")).toEqual(
      List(
        Array({loc: Loc(3), hd: EmptyArray(Loc(4)), tl: list{Binding(Loc(8), Id("x"))}}),
        list{},
      ),
    )
  })

  test("Objects", ({expect, _}) => {
    expect.value(parseString("{}")).toEqual(List(EmptyObject(Loc(3)), list{}))
    expect.value(parseString("{pun}")).toEqual(
      List(Object({loc: Loc(3), hd: ("pun", Binding(Loc(4), Id("pun"))), tl: list{}}), list{}),
    )
    expect.value(parseString("{pun1, pun2}")).toEqual(
      List(
        Object({
          loc: Loc(3),
          hd: ("pun1", Binding(Loc(4), Id("pun1"))),
          tl: list{("pun2", Binding(Loc(10), Id("pun2")))},
        }),
        list{},
      ),
    )
    expect.value(parseString("{a: b}")).toEqual(
      List(Object({loc: Loc(3), hd: ("a", Binding(Loc(7), Id("b"))), tl: list{}}), list{}),
    )
    expect.value(parseString("{a: b, c: d}")).toEqual(
      List(
        Object({
          loc: Loc(3),
          hd: ("a", Binding(Loc(7), Id("b"))),
          tl: list{("c", Binding(Loc(13), Id("d")))},
        }),
        list{},
      ),
    )
    expect.value(parseString(`{"#illegal": legal, "<%name%>": name}`)).toEqual(
      List(
        Object({
          loc: Loc(3),
          hd: ("#illegal", Binding(Loc(16), Id("legal"))),
          tl: list{("<%name%>", Binding(Loc(35), Id("name")))},
        }),
        list{},
      ),
    )
    expect.value(parseString(`{a: 1.5, b: "b", c: null, d}`)).toEqual(
      List(
        Object({
          loc: Loc(3),
          hd: ("a", Number(Loc(7), 1.5)),
          tl: list{
            ("b", String(Loc(15), "b")),
            ("c", Null(Loc(23))),
            ("d", Binding(Loc(29), Id("d"))),
          },
        }),
        list{},
      ),
    )
    let result = parseString(
      `
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
}`,
    )
    expect.value(result).toEqual(
      List(
        Object({
          loc: Loc(4),
          hd: ("a", Binding(Loc(11), Id("bindingA"))),
          tl: list{
            ("b", Number(Loc(26), 1.5)),
            (
              "c",
              ArrayWithTailBinding({
                loc: Loc(36),
                hd: String(Loc(41), "item1"),
                tl: list{Binding(Loc(54), Id("bindingC"))},
                bindLoc: Loc(71),
                binding: Id("rest"),
              }),
            ),
            (
              "d",
              Object({
                loc: Loc(86),
                hd: ("<% illegal %>", Null(Loc(109))),
                tl: list{("d", Binding(Loc(122), Id("bindingD")))},
              }),
            ),
            ("e", Binding(Loc(141), Id("e"))),
          },
        }),
        list{},
      ),
    )
  })

  test("Multiple patterns", ({expect, _}) => {
    expect.value(parseString(`true, "a", b`)).toEqual(
      List(True(Loc(3)), list{String(Loc(9), "a"), Binding(Loc(14), Id("b"))}),
    )
  })
})

describe("Parser", ({test, _}) => {
  test("Basic syntax", ({expect, _}) => {
    expect.value(
      Compile.makeAst(`
a
{* b *}
{{ c }}
{{ "d" }}
{{ 1.5 }}
{% raw e %}
f`)
      ->getAst
      ->Belt.List.toArray,
    ).toEqual([
      Text("\na\n", NoTrim),
      Text("\n", NoTrim),
      EchoBinding(Loc(14), Id("c")),
      Text("\n", NoTrim),
      EchoString("d"),
      Text("\n", NoTrim),
      EchoNumber(1.5),
      Text("\n", NoTrim),
      Unescaped(Loc(46), Id("e")),
      Text("\nf", NoTrim),
    ])
  })
  test("Matching", ({expect, _}) => {
    expect.value(
      Compile.makeAst(
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
`,
      )
      ->getAst
      ->Belt.List.toArray,
    ).toMatchSnapshot()
  })
  test("Mapping", ({expect, _}) => {
    expect.value(
      Compile.makeAst(
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
{% map g with {h}, index %}
  {{ index }} {{ g }}
{% /map %}
`,
      )
      ->getAst
      ->Belt.List.toArray,
    ).toMatchSnapshot()
  })
  test("Components", ({expect, _}) => {
    expect.value(
      Compile.makeAst(
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
      )
      ->getAst
      ->Belt.List.toArray,
    ).toMatchSnapshot()
  })
})
