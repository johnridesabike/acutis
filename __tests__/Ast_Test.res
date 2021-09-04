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
module T = Acutis_Types

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
    Compile.Pattern.make(tokens)
  }

  test("Enums", ({expect, _}) => {
    expect.value(parseString("null")).toEqual(NonEmpty.one(#Null(T.Loc(3))))
    expect.value(parseString("false")).toEqual(NonEmpty.one(#False(T.Loc(3))))
    expect.value(parseString("true")).toEqual(NonEmpty.one(#True(T.Loc(3))))
  })

  test("Numbers", ({expect, _}) => {
    expect.value(parseString("1")).toEqual(NonEmpty.one(#Int(T.Loc(3), 1)))
    expect.value(parseString("1.")).toEqual(NonEmpty.one(#Float(T.Loc(3), 1.0)))
    expect.value(parseString("12345")).toEqual(NonEmpty.one(#Int(T.Loc(3), 12345)))
    expect.value(parseString("1234.5")).toEqual(NonEmpty.one(#Float(T.Loc(3), 1234.5)))
    expect.value(parseString("-12345")).toEqual(NonEmpty.one(#Int(T.Loc(3), -12345)))
    expect.value(parseString("1e+1")).toEqual(NonEmpty.one(#Int(T.Loc(3), 10)))
    expect.value(parseString("-1E+1")).toEqual(NonEmpty.one(#Int(T.Loc(3), -10)))
    expect.value(parseString("175.0e-2")).toEqual(NonEmpty.one(#Float(T.Loc(3), 1.75)))
    expect.value(parseString("175e-2")).toEqual(NonEmpty.one(#Int(T.Loc(3), 1)))
  })

  test("Strings", ({expect, _}) => {
    expect.value(parseString(`"a"`)).toEqual(NonEmpty.one(#String(T.Loc(3), "a")))
    expect.value(parseString(`"a b c"`)).toEqual(NonEmpty.one(#String(T.Loc(3), "a b c")))
    expect.value(parseString("\"a says \\\"b\\\" to c\"")).toEqual(
      NonEmpty.one(#String(T.Loc(3), `a says "b" to c`)),
    )
  })

  test("Bindings", ({expect, _}) => {
    expect.value(parseString("a")).toEqual(NonEmpty.one(#Binding(T.Loc(3), "a")))
    expect.value(parseString("_")).toEqual(NonEmpty.one(#Binding(T.Loc(3), "_")))
    expect.value(parseString("abc123")).toEqual(NonEmpty.one(#Binding(T.Loc(3), "abc123")))
    expect.value(parseString("a_")).toEqual(NonEmpty.one(#Binding(T.Loc(3), "a_")))
  })

  test("Arrays", ({expect, _}) => {
    expect.value(parseString("[]")).toEqual(NonEmpty.one(#Array(T.Loc(3), [])))
    expect.value(parseString("[1]")).toEqual(NonEmpty.one(#Array(T.Loc(3), [#Int(T.Loc(4), 1)])))
    expect.value(parseString("[null]")).toEqual(NonEmpty.one(#Array(T.Loc(3), [#Null(T.Loc(4))])))
    expect.value(parseString(`["a"]`)).toEqual(
      NonEmpty.one(#Array(T.Loc(3), [#String(T.Loc(4), "a")])),
    )
    expect.value(parseString(`[1, "a", null]`)).toEqual(
      NonEmpty.one(#Array(T.Loc(3), [#Int(T.Loc(4), 1), #String(T.Loc(7), "a"), #Null(T.Loc(12))])),
    )
    expect.value(parseString(`["b", x]`)).toEqual(
      NonEmpty.one(#Array(T.Loc(3), [#String(T.Loc(4), "b"), #Binding(T.Loc(9), "x")])),
    )
    expect.value(parseString(`["b", ...x]`)).toEqual(
      NonEmpty.one(
        #ArrayWithTailBinding(T.Loc(3), [#String(T.Loc(4), "b")], #Binding(T.Loc(12), "x")),
      ),
    )
    expect.value(parseString(`[["b", 1], x]`)).toEqual(
      NonEmpty.one(
        #Array(
          T.Loc(3),
          [
            #Array(T.Loc(4), [#String(T.Loc(5), "b"), #Int(T.Loc(10), 1)]),
            #Binding(T.Loc(14), "x"),
          ],
        ),
      ),
    )
    expect.value(parseString("[[], x]")).toEqual(
      NonEmpty.one(#Array(T.Loc(3), [#Array(T.Loc(4), []), #Binding(T.Loc(8), "x")])),
    )
  })

  test("Objects", ({expect, _}) => {
    expect.value(parseString("{}")).toEqual(NonEmpty.one(#Object(T.Loc(3), [])))
    expect.value(parseString("{pun}")).toEqual(
      NonEmpty.one(#Object(T.Loc(3), [("pun", #Binding(T.Loc(4), "pun"))])),
    )
    expect.value(parseString("{pun1, pun2}")).toEqual(
      NonEmpty.one(
        #Object(
          T.Loc(3),
          [("pun1", #Binding(T.Loc(4), "pun1")), ("pun2", #Binding(T.Loc(10), "pun2"))],
        ),
      ),
    )
    expect.value(parseString("{a: b}")).toEqual(
      NonEmpty.one(#Object(T.Loc(3), [("a", #Binding(T.Loc(7), "b"))])),
    )
    expect.value(parseString("{a: b, c: d}")).toEqual(
      NonEmpty.one(
        #Object(T.Loc(3), [("a", #Binding(T.Loc(7), "b")), ("c", #Binding(T.Loc(13), "d"))]),
      ),
    )
    expect.value(parseString(`{"#illegal": legal, "<%name%>": name}`)).toEqual(
      NonEmpty.one(
        #Object(
          T.Loc(3),
          [("#illegal", #Binding(T.Loc(16), "legal")), ("<%name%>", #Binding(T.Loc(35), "name"))],
        ),
      ),
    )
    expect.value(parseString(`{a: 1.5, b: "b", c: null, d}`)).toEqual(
      NonEmpty.one(
        #Object(
          T.Loc(3),
          [
            ("a", #Float(T.Loc(7), 1.5)),
            ("b", #String(T.Loc(15), "b")),
            ("c", #Null(T.Loc(23))),
            ("d", #Binding(T.Loc(29), "d")),
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
        #Object(
          T.Loc(4),
          [
            ("a", #Binding(T.Loc(11), "bindingA")),
            ("b", #Float(T.Loc(26), 1.5)),
            (
              "c",
              #ArrayWithTailBinding(
                T.Loc(36),
                [#String(T.Loc(41), "item1"), #Binding(T.Loc(54), "bindingC")],
                #Binding(T.Loc(71), "rest"),
              ),
            ),
            (
              "d",
              #Object(
                T.Loc(86),
                [("<% illegal %>", #Null(T.Loc(109))), ("d", #Binding(T.Loc(122), "bindingD"))],
              ),
            ),
            ("e", #Binding(T.Loc(141), "e")),
          ],
        ),
      ),
    )
  })

  test("Multiple patterns", ({expect, _}) => {
    expect.value(parseString(`true, "a", b`)).toEqual(
      NonEmpty.fromArrayExn([#True(T.Loc(3)), #String(T.Loc(9), "a"), #Binding(T.Loc(14), "b")]),
    )
  })
})

describe("Parser", ({test, _}) => {
  test("Basic syntax", ({expect, _}) => {
    expect.value(
      Compile.makeAstInternalExn(
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
      Text("\na\n", NoTrim),
      Text("\n", NoTrim),
      Echo(T.Loc(13), NonEmpty.one(T.Ast.Echo.Binding(T.Loc(14), "c", Escape))),
      Text("\n", NoTrim),
      Echo(T.Loc(21), NonEmpty.one(T.Ast.Echo.String(T.Loc(22), "d", Escape))),
      Text("\n", NoTrim),
      Echo(T.Loc(31), NonEmpty.one(T.Ast.Echo.Float(T.Loc(32), 1.5, Escape))),
      Text("\n", NoTrim),
      Echo(T.Loc(41), NonEmpty.one(T.Ast.Echo.Binding(T.Loc(43), "e", NoEscape))),
      Text("\nf", NoTrim),
    ])
  })
  test("Matching", ({expect, _}) => {
    expect.value(
      Compile.makeAstInternalExn(
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
      Compile.makeAstInternalExn(
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
      Compile.makeAstInternalExn(
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
    let a = Source.string(~name="A", "{% B /%}")
    let b = Source.string(~name="B", "{% C /%}")
    let c = Source.string(~name="C", "c")
    let d = Source.string(~name="D", "{% C /%}")
    let result = Compile.Components.make([a, b, c, d])
    // This assertion doesn't really check that the test worked.
    // It exists for code coverage.
    expect.value(Result.map(result, _ => true)).toEqual(#ok(true))
  })
})
