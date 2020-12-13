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

let getAst = (x: Acutis_Types.Ast.t) =>
  (x->Acutis_Types.Valid.validate->Belt.Option.getExn->Belt.Result.getExn).ast

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
  let parseString = source => {
    let tokens = Lexer.make("{% " ++ source ++ "%}")
    Lexer.skipExn(tokens) // Skip the opening string
    Compile.Pattern.make(tokens)
  }

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
    expect.value(parseString("a")).toEqual(List(Binding(Loc(3), "a"), list{}))
    expect.value(parseString("_")).toEqual(List(Binding(Loc(3), "_"), list{}))
    expect.value(parseString("abc123")).toEqual(List(Binding(Loc(3), "abc123"), list{}))
    expect.value(parseString("a_")).toEqual(List(Binding(Loc(3), "a_"), list{}))
  })

  test("Arrays", ({expect, _}) => {
    expect.value(parseString("[]")).toEqual(List(Array(Loc(3), list{}), list{}))
    expect.value(parseString("[1]")).toEqual(List(Array(Loc(3), list{Number(Loc(4), 1.0)}), list{}))
    expect.value(parseString("[null]")).toEqual(List(Array(Loc(3), list{Null(Loc(4))}), list{}))
    expect.value(parseString(`["a"]`)).toEqual(
      List(Array(Loc(3), list{String(Loc(4), "a")}), list{}),
    )
    expect.value(parseString(`[1, "a", null]`)).toEqual(
      List(Array(Loc(3), list{Number(Loc(4), 1.0), String(Loc(7), "a"), Null(Loc(12))}), list{}),
    )
    expect.value(parseString(`["b", x]`)).toEqual(
      List(Array(Loc(3), list{String(Loc(4), "b"), Binding(Loc(9), "x")}), list{}),
    )
    expect.value(parseString(`["b", ...x]`)).toEqual(
      List(
        ArrayWithTailBinding({
          loc: Loc(3),
          array: list{String(Loc(4), "b")},
          bindLoc: Loc(12),
          binding: "x",
        }),
        list{},
      ),
    )
    expect.value(parseString(`[["b", 1], x]`)).toEqual(
      List(
        Array(
          Loc(3),
          list{
            Array(Loc(4), list{String(Loc(5), "b"), Number(Loc(10), 1.0)}),
            Binding(Loc(14), "x"),
          },
        ),
        list{},
      ),
    )
    expect.value(parseString("[[], x]")).toEqual(
      List(Array(Loc(3), list{Array(Loc(4), list{}), Binding(Loc(8), "x")}), list{}),
    )
  })

  test("Objects", ({expect, _}) => {
    expect.value(parseString("{}")).toEqual(List(Object(Loc(3), list{}), list{}))
    expect.value(parseString("{pun}")).toEqual(
      List(Object(Loc(3), list{("pun", Binding(Loc(4), "pun"))}), list{}),
    )
    expect.value(parseString("{pun1, pun2}")).toEqual(
      List(
        Object(Loc(3), list{("pun1", Binding(Loc(4), "pun1")), ("pun2", Binding(Loc(10), "pun2"))}),
        list{},
      ),
    )
    expect.value(parseString("{a: b}")).toEqual(
      List(Object(Loc(3), list{("a", Binding(Loc(7), "b"))}), list{}),
    )
    expect.value(parseString("{a: b, c: d}")).toEqual(
      List(Object(Loc(3), list{("a", Binding(Loc(7), "b")), ("c", Binding(Loc(13), "d"))}), list{}),
    )
    expect.value(parseString(`{"#illegal": legal, "<%name%>": name}`)).toEqual(
      List(
        Object(
          Loc(3),
          list{("#illegal", Binding(Loc(16), "legal")), ("<%name%>", Binding(Loc(35), "name"))},
        ),
        list{},
      ),
    )
    expect.value(parseString(`{a: 1.5, b: "b", c: null, d}`)).toEqual(
      List(
        Object(
          Loc(3),
          list{
            ("a", Number(Loc(7), 1.5)),
            ("b", String(Loc(15), "b")),
            ("c", Null(Loc(23))),
            ("d", Binding(Loc(29), "d")),
          },
        ),
        list{},
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
      List(
        Object(
          Loc(4),
          list{
            ("a", Binding(Loc(11), "bindingA")),
            ("b", Number(Loc(26), 1.5)),
            (
              "c",
              ArrayWithTailBinding({
                loc: Loc(36),
                array: list{String(Loc(41), "item1"), Binding(Loc(54), "bindingC")},
                bindLoc: Loc(71),
                binding: "rest",
              }),
            ),
            (
              "d",
              Object(
                Loc(86),
                list{("<% illegal %>", Null(Loc(109))), ("d", Binding(Loc(122), "bindingD"))},
              ),
            ),
            ("e", Binding(Loc(141), "e")),
          },
        ),
        list{},
      ),
    )
  })

  test("Multiple patterns", ({expect, _}) => {
    expect.value(parseString(`true, "a", b`)).toEqual(
      List(True(Loc(3)), list{String(Loc(9), "a"), Binding(Loc(14), "b")}),
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
      EchoBinding(Loc(14), "c"),
      Text("\n", NoTrim),
      EchoString("d"),
      Text("\n", NoTrim),
      EchoNumber(1.5),
      Text("\n", NoTrim),
      Unescaped(Loc(46), "e"),
      Text("\nf", NoTrim),
    ])
  })
  test("Matching", ({expect, _}) => {
    expect.value(
      Compile.makeAst(`
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
`)
      ->getAst
      ->Belt.List.toArray,
    ).toMatchSnapshot()
  })
  test("Mapping", ({expect, _}) => {
    expect.value(
      Compile.makeAst(`
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
`)
      ->getAst
      ->Belt.List.toArray,
    ).toMatchSnapshot()
  })
  test("Components", ({expect, _}) => {
    expect.value(
      Compile.makeAst(`
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
`)
      ->getAst
      ->Belt.List.toArray,
    ).toMatchSnapshot()
  })
})
