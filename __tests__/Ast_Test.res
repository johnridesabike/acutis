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

let getAst = (x: Acutis_Types.Ast.t) =>
  switch Acutis_Types.Valid.validate(x) {
  | Some(#data(x)) => x.ast
  | Some(#errors(_)) | None => raise(Not_found)
  }

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
    Lexer.popExn(tokens)->ignore // Skip the opening string
    Compile.Pattern.make(tokens)
  }

  test("Enums", ({expect, _}) => {
    expect.value(parseString("null")).toEqual(NonEmpty(Null(Loc(3)), list{}))
    expect.value(parseString("false")).toEqual(NonEmpty(False(Loc(3)), list{}))
    expect.value(parseString("true")).toEqual(NonEmpty(True(Loc(3)), list{}))
  })

  test("Numbers", ({expect, _}) => {
    expect.value(parseString("1")).toEqual(NonEmpty(Number(Loc(3), 1.0), list{}))
    expect.value(parseString("12345")).toEqual(NonEmpty(Number(Loc(3), 12345.0), list{}))
    expect.value(parseString("1234.5")).toEqual(NonEmpty(Number(Loc(3), 1234.5), list{}))
    expect.value(parseString("-12345")).toEqual(NonEmpty(Number(Loc(3), -12345.0), list{}))
    expect.value(parseString("1e+1")).toEqual(NonEmpty(Number(Loc(3), 10.0), list{}))
    expect.value(parseString("-1E+1")).toEqual(NonEmpty(Number(Loc(3), -10.0), list{}))
  })

  test("Strings", ({expect, _}) => {
    expect.value(parseString(`"a"`)).toEqual(NonEmpty(String(Loc(3), "a"), list{}))
    expect.value(parseString(`"a b c"`)).toEqual(NonEmpty(String(Loc(3), "a b c"), list{}))
    expect.value(parseString("\"a says \\\"b\\\" to c\"")).toEqual(
      NonEmpty(String(Loc(3), `a says "b" to c`), list{}),
    )
  })

  test("Bindings", ({expect, _}) => {
    expect.value(parseString("a")).toEqual(NonEmpty(Binding(Loc(3), "a"), list{}))
    expect.value(parseString("_")).toEqual(NonEmpty(Binding(Loc(3), "_"), list{}))
    expect.value(parseString("abc123")).toEqual(NonEmpty(Binding(Loc(3), "abc123"), list{}))
    expect.value(parseString("a_")).toEqual(NonEmpty(Binding(Loc(3), "a_"), list{}))
  })

  test("Arrays", ({expect, _}) => {
    expect.value(parseString("[]")).toEqual(NonEmpty(Array(Loc(3), list{}), list{}))
    expect.value(parseString("[1]")).toEqual(
      NonEmpty(Array(Loc(3), list{Number(Loc(4), 1.0)}), list{}),
    )
    expect.value(parseString("[null]")).toEqual(NonEmpty(Array(Loc(3), list{Null(Loc(4))}), list{}))
    expect.value(parseString(`["a"]`)).toEqual(
      NonEmpty(Array(Loc(3), list{String(Loc(4), "a")}), list{}),
    )
    expect.value(parseString(`[1, "a", null]`)).toEqual(
      NonEmpty(
        Array(Loc(3), list{Number(Loc(4), 1.0), String(Loc(7), "a"), Null(Loc(12))}),
        list{},
      ),
    )
    expect.value(parseString(`["b", x]`)).toEqual(
      NonEmpty(Array(Loc(3), list{String(Loc(4), "b"), Binding(Loc(9), "x")}), list{}),
    )
    expect.value(parseString(`["b", ...x]`)).toEqual(
      NonEmpty(
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
      NonEmpty(
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
      NonEmpty(Array(Loc(3), list{Array(Loc(4), list{}), Binding(Loc(8), "x")}), list{}),
    )
  })

  test("Objects", ({expect, _}) => {
    expect.value(parseString("{}")).toEqual(NonEmpty(Object(Loc(3), list{}), list{}))
    expect.value(parseString("{pun}")).toEqual(
      NonEmpty(Object(Loc(3), list{("pun", Binding(Loc(4), "pun"))}), list{}),
    )
    expect.value(parseString("{pun1, pun2}")).toEqual(
      NonEmpty(
        Object(Loc(3), list{("pun1", Binding(Loc(4), "pun1")), ("pun2", Binding(Loc(10), "pun2"))}),
        list{},
      ),
    )
    expect.value(parseString("{a: b}")).toEqual(
      NonEmpty(Object(Loc(3), list{("a", Binding(Loc(7), "b"))}), list{}),
    )
    expect.value(parseString("{a: b, c: d}")).toEqual(
      NonEmpty(
        Object(Loc(3), list{("a", Binding(Loc(7), "b")), ("c", Binding(Loc(13), "d"))}),
        list{},
      ),
    )
    expect.value(parseString(`{"#illegal": legal, "<%name%>": name}`)).toEqual(
      NonEmpty(
        Object(
          Loc(3),
          list{("#illegal", Binding(Loc(16), "legal")), ("<%name%>", Binding(Loc(35), "name"))},
        ),
        list{},
      ),
    )
    expect.value(parseString(`{a: 1.5, b: "b", c: null, d}`)).toEqual(
      NonEmpty(
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
      NonEmpty(
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
      NonEmpty(True(Loc(3)), list{String(Loc(9), "a"), Binding(Loc(14), "b")}),
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
      Echo(Loc(13), NonEmpty(Binding(Loc(14), "c"), list{})),
      Text("\n", NoTrim),
      Echo(Loc(21), NonEmpty(String("d"), list{})),
      Text("\n", NoTrim),
      Echo(Loc(31), NonEmpty(Number(1.5), list{})),
      Text("\n", NoTrim),
      Unescaped(Loc(42), NonEmpty(Binding(Loc(46), "e"), list{})),
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
