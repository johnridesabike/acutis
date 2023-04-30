open Acutis
module F = Format

let parse src = Compile.parse ~fname:"<test>" (Lexing.from_string src)
let check = Alcotest.(check (module DebugAst))
let loc = Loc.dummy

let echoes () =
  let src = {|{{ a }} {{ "b" }} {{{ d ? e ? "f\"g" }}}|} in
  check "Echoes parse correctly"
    [
      Text ("", No_trim, No_trim);
      Echo ([], Fmt_string, Echo_var (loc, "a"), Escape);
      Text (" ", No_trim, No_trim);
      Echo ([], Fmt_string, Echo_string (loc, "b"), Escape);
      Text (" ", No_trim, No_trim);
      Echo
        ( [
            (Fmt_string, Echo_var (loc, "d")); (Fmt_string, Echo_var (loc, "e"));
          ],
          Fmt_string,
          Echo_string (loc, "f\"g"),
          No_escape );
      Text ("", No_trim, No_trim);
    ]
    (parse src);
  let src = {|{{ %i i }} {{ %f f }} {{ %b b }}|} in
  check "Echo formats parse correctly"
    [
      Text ("", No_trim, No_trim);
      Echo ([], Fmt_int, Echo_var (loc, "i"), Escape);
      Text (" ", No_trim, No_trim);
      Echo ([], Fmt_float, Echo_var (loc, "f"), Escape);
      Text (" ", No_trim, No_trim);
      Echo ([], Fmt_bool, Echo_var (loc, "b"), Escape);
      Text ("", No_trim, No_trim);
    ]
    (parse src)

let numbers () =
  let src =
    {|{% match a
        with
          {
            int: 01000,
            frac: 010.55,
            negint: -0999,
            negfrac: -012.34,
            exp1: 1.5e2,
            exp2: -10e+2,
            exp3: 20e-2
          }
      %}{% with _ %}{% /match %}|}
  in
  check "Numbers parse correctly"
    [
      Text ("", No_trim, No_trim);
      Match
        ( loc,
          [ Var (loc, "a") ],
          [
            {
              pats =
                [
                  ( loc,
                    [
                      Record
                        ( loc,
                          Untagged
                            Ast.Dict.(
                              empty
                              |> add loc "int" (Ast.Int (loc, 1000))
                              |> add loc "frac" (Ast.Float (loc, 10.55))
                              |> add loc "negint" (Ast.Int (loc, -999))
                              |> add loc "negfrac" (Ast.Float (loc, -12.34))
                              |> add loc "exp1" (Ast.Float (loc, 150.0))
                              |> add loc "exp2" (Ast.Float (loc, -1000.0))
                              |> add loc "exp3" (Ast.Float (loc, 0.2))) );
                    ] );
                ];
              nodes = [ Text ("", No_trim, No_trim) ];
            };
            {
              pats = [ (loc, [ Var (loc, "_") ]) ];
              nodes = [ Text ("", No_trim, No_trim) ];
            };
          ] );
      Text ("", No_trim, No_trim);
    ]
    (parse src)

let trim () =
  let src =
    {|{{~ a }} {{~ b }} {{ c ~}} {{~ d ~}} {{{~ e }}} {{{ f ~}}} {{{~ g ~}}}|}
  in
  check "Trim parses correctly"
    [
      Text ("", No_trim, Trim);
      Echo ([], Fmt_string, Echo_var (loc, "a"), Escape);
      Text (" ", No_trim, Trim);
      Echo ([], Fmt_string, Echo_var (loc, "b"), Escape);
      Text (" ", No_trim, No_trim);
      Echo ([], Fmt_string, Echo_var (loc, "c"), Escape);
      Text (" ", Trim, Trim);
      Echo ([], Fmt_string, Echo_var (loc, "d"), Escape);
      Text (" ", Trim, Trim);
      Echo ([], Fmt_string, Echo_var (loc, "e"), No_escape);
      Text (" ", No_trim, No_trim);
      Echo ([], Fmt_string, Echo_var (loc, "f"), No_escape);
      Text (" ", Trim, Trim);
      Echo ([], Fmt_string, Echo_var (loc, "g"), No_escape);
      Text ("", Trim, No_trim);
    ]
    (parse src)

let comments () =
  let src = "a {* {* *} *}b{* \n *} c" in
  check "Comments parse correctly"
    [ Text ("a b c", No_trim, No_trim) ]
    (parse src)

let matches () =
  let src =
    {|{% match a with 1 with 2 %}{% with 3 %} {% with _ %} {% /match %}|}
  in
  check "Flat matches parse correctly"
    [
      Text ("", No_trim, No_trim);
      Match
        ( loc,
          [ Var (loc, "a") ],
          [
            {
              pats = [ (loc, [ Int (loc, 1) ]); (loc, [ Int (loc, 2) ]) ];
              nodes = [ Text ("", No_trim, No_trim) ];
            };
            {
              pats = [ (loc, [ Int (loc, 3) ]) ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            {
              pats = [ (loc, [ Var (loc, "_") ]) ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
          ] );
      Text ("", No_trim, No_trim);
    ]
    (parse src);
  let src =
    {|{% match b with c %}{% match d, e with f, g %} {% /match %}{% /match %}|}
  in
  check "Nested matches parse correctly"
    [
      Text ("", No_trim, No_trim);
      Match
        ( loc,
          [ Var (loc, "b") ],
          [
            {
              pats = [ (loc, [ Var (loc, "c") ]) ];
              nodes =
                [
                  Text ("", No_trim, No_trim);
                  Match
                    ( loc,
                      [ Var (loc, "d"); Var (loc, "e") ],
                      [
                        {
                          pats = [ (loc, [ Var (loc, "f"); Var (loc, "g") ]) ];
                          nodes = [ Text (" ", No_trim, No_trim) ];
                        };
                      ] );
                  Text ("", No_trim, No_trim);
                ];
            };
          ] );
      Text ("", No_trim, No_trim);
    ]
    (parse src)

let maps () =
  let src =
    {|{% map l with 1 with 2 %}{% with 3, i %} {% with _ %} {% /map %}|}
  in
  check "Map list parses correctly"
    [
      Text ("", No_trim, No_trim);
      Map_list
        ( loc,
          Var (loc, "l"),
          [
            {
              pats = [ (loc, [ Int (loc, 1) ]); (loc, [ Int (loc, 2) ]) ];
              nodes = [ Text ("", No_trim, No_trim) ];
            };
            {
              pats = [ (loc, [ Int (loc, 3); Var (loc, "i") ]) ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            {
              pats = [ (loc, [ Var (loc, "_") ]) ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
          ] );
      Text ("", No_trim, No_trim);
    ]
    (parse src);
  let src =
    {|{% map_dict d with 1 with 2 %}{% with 3, k %} {% with _ %} {% /map_dict %}|}
  in
  check "Map dict parses correctly"
    [
      Text ("", No_trim, No_trim);
      Map_dict
        ( loc,
          Var (loc, "d"),
          [
            {
              pats = [ (loc, [ Int (loc, 1) ]); (loc, [ Int (loc, 2) ]) ];
              nodes = [ Text ("", No_trim, No_trim) ];
            };
            {
              pats = [ (loc, [ Int (loc, 3); Var (loc, "k") ]) ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            {
              pats = [ (loc, [ Var (loc, "_") ]) ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
          ] );
      Text ("", No_trim, No_trim);
    ]
    (parse src)

let components () =
  let src =
    {|{% Template
    a=b
    c
    d=e
    f
    g=#%} {%#
    h=#%}{% match a with b %} {% /match %}{%#
    i=##
  /%}|}
  in
  check "Component with props parses correctly"
    [
      Text ("", No_trim, No_trim);
      Component
        ( loc,
          "Template",
          "Template",
          Ast.Dict.(
            empty
            |> add loc "a" (Ast.Var (loc, "b"))
            |> add loc "c" (Ast.Var (loc, "c"))
            |> add loc "d" (Ast.Var (loc, "e"))
            |> add loc "f" (Ast.Var (loc, "f"))
            |> add loc "g"
                 (Ast.Block (loc, [ Ast.Text (" ", No_trim, No_trim) ]))
            |> add loc "h"
                 (Ast.Block
                    ( loc,
                      [
                        Ast.Text ("", No_trim, No_trim);
                        Match
                          ( loc,
                            [ Var (loc, "a") ],
                            [
                              {
                                pats = [ (loc, [ Var (loc, "b") ]) ];
                                nodes = [ Text (" ", No_trim, No_trim) ];
                              };
                            ] );
                        Ast.Text ("", No_trim, No_trim);
                      ] ))
            |> add loc "i" (Ast.Block (loc, []))) );
      Text ("", No_trim, No_trim);
    ]
    (parse src);
  let src = {|{% Template %} {% /Template %}|} in
  check "Component with implicit children parses correctly"
    [
      Text ("", No_trim, No_trim);
      Component
        ( loc,
          "Template",
          "Template",
          Ast.Dict.singleton "children"
            (Ast.Block (loc, [ Ast.Text (" ", No_trim, No_trim) ])) );
      Text ("", No_trim, No_trim);
    ]
    (parse src)

let patterns () =
  let src =
    {|{% match tuple with (1, 2.5, "a") %} {% with _ %} {% /match %}|}
  in
  check "Tuple patterns parse correctly"
    [
      Text ("", No_trim, No_trim);
      Match
        ( loc,
          [ Var (loc, "tuple") ],
          [
            {
              pats =
                [
                  ( loc,
                    [
                      Tuple
                        ( loc,
                          [ Int (loc, 1); Float (loc, 2.5); String (loc, "a") ]
                        );
                    ] );
                ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            {
              pats = [ (loc, [ Var (loc, "_") ]) ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
          ] );
      Text ("", No_trim, No_trim);
    ]
    (parse src);
  let src =
    "{% match list with [] %} {% with [!a, null] %} {% with [z, ...tl] %}\n\
     {% /match %}"
  in
  check "List patterns parse correctly"
    [
      Text ("", No_trim, No_trim);
      Match
        ( loc,
          [ Var (loc, "list") ],
          [
            {
              pats = [ (loc, [ List (loc, [], None) ]) ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            {
              pats =
                [
                  ( loc,
                    [
                      List
                        ( loc,
                          [
                            Nullable (loc, Some (Var (loc, "a")));
                            Nullable (loc, None);
                          ],
                          None );
                    ] );
                ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            {
              pats =
                [
                  ( loc,
                    [ List (loc, [ Var (loc, "z") ], Some (Var (loc, "tl"))) ]
                  );
                ];
              nodes = [ Text ("\n", No_trim, No_trim) ];
            };
          ] );
      Text ("", No_trim, No_trim);
    ]
    (parse src);
  let src =
    {|{% match record with {a, "!#%@": b} %} {% with _ %} {% /match %}|}
  in
  check "Record patterns parse correctly"
    [
      Text ("", No_trim, No_trim);
      Match
        ( loc,
          [ Var (loc, "record") ],
          [
            {
              pats =
                [
                  ( loc,
                    [
                      Record
                        ( loc,
                          Untagged
                            Ast.Dict.(
                              empty
                              |> add loc "a" (Ast.Var (loc, "a"))
                              |> add loc "!#%@" (Ast.Var (loc, "b"))) );
                    ] );
                ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            {
              pats = [ (loc, [ Var (loc, "_") ]) ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
          ] );
      Text ("", No_trim, No_trim);
    ]
    (parse src);
  let src =
    {|{% match enums with (@"a", @1, true, false) %} {% with _ %} {% /match %}|}
  in
  check "Enum patterns parse correctly"
    [
      Text ("", No_trim, No_trim);
      Match
        ( loc,
          [ Var (loc, "enums") ],
          [
            {
              pats =
                [
                  ( loc,
                    [
                      Tuple
                        ( loc,
                          [
                            Enum_string (loc, "a");
                            Enum_int (loc, 1);
                            Bool (loc, 1);
                            Bool (loc, 0);
                          ] );
                    ] );
                ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            {
              pats = [ (loc, [ Var (loc, "_") ]) ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
          ] );
      Text ("", No_trim, No_trim);
    ]
    (parse src);
  let src =
    "{% match tagged with {@tag: true, a} %} {% with {@tag: false} %}\n\
     {% /match %}"
  in
  check "Tagged union patterns parse correctly"
    [
      Text ("", No_trim, No_trim);
      Match
        ( loc,
          [ Var (loc, "tagged") ],
          [
            {
              pats =
                [
                  ( loc,
                    [
                      Record
                        ( loc,
                          Tagged
                            ( "tag",
                              Tag_bool (loc, 1),
                              Ast.Dict.singleton "a" (Ast.Var (loc, "a")) ) );
                    ] );
                ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            {
              pats =
                [
                  ( loc,
                    [
                      Record
                        (loc, Tagged ("tag", Tag_bool (loc, 0), Ast.Dict.empty));
                    ] );
                ];
              nodes = [ Text ("\n", No_trim, No_trim) ];
            };
          ] );
      Text ("", No_trim, No_trim);
    ]
    (parse src);
  let src = {|{% match dict with <a: 1, b: 2> %} {% with _ %} {% /match %}|} in
  check "Dictionary patterns parse correctly"
    [
      Text ("", No_trim, No_trim);
      Match
        ( loc,
          [ Var (loc, "dict") ],
          [
            {
              pats =
                [
                  ( loc,
                    [
                      Dict
                        ( loc,
                          Ast.Dict.(
                            empty
                            |> add loc "a" (Ast.Int (loc, 1))
                            |> add loc "b" (Ast.Int (loc, 2))) );
                    ] );
                ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            {
              pats = [ (loc, [ Var (loc, "_") ]) ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
          ] );
      Text ("", No_trim, No_trim);
    ]
    (parse src)

let interface () =
  let src =
    {|{% interface
    a = {a: @0 | @1, b: @"a" | @"b"}
    b = {@tag: true, a: [int]} | {@tag: false, a: <?string>}
    c = {@tag: 0} | {@tag: 1, a: (float, true | false)}
    d = {@tag: "a", a: float} | {@tag: "b", a: @0 | @1 | ...}
    e = {@tag: 0, a: _} | {@tag: 1, b: @"a" | @"b" | ...} | ...
    children = string
    optionalChildren = ?string
  / %}|}
  in
  let open Ast.Interface in
  check "Interfaces parse correctly."
    [
      Text ("", No_trim, No_trim);
      Interface
        ( loc,
          [
            {
              loc;
              name = "a";
              ty =
                Record
                  ( [
                      ( loc,
                        Untagged
                          Ast.Dict.(
                            singleton "a" (Enum_int ([ 0; 1 ], `Closed))
                            |> add loc "b" (Enum_string ([ "a"; "b" ], `Closed)))
                      );
                    ],
                    `Closed );
            };
            {
              loc;
              name = "b";
              ty =
                Record
                  ( [
                      ( loc,
                        Tagged
                          ( "tag",
                            Tag_bool (loc, 1),
                            Ast.Dict.singleton "a" (List (Named (loc, "int")))
                          ) );
                      ( loc,
                        Tagged
                          ( "tag",
                            Tag_bool (loc, 0),
                            Ast.Dict.singleton "a"
                              (Dict (Nullable (Named (loc, "string")))) ) );
                    ],
                    `Closed );
            };
            {
              loc;
              name = "c";
              ty =
                Record
                  ( [
                      (loc, Tagged ("tag", Tag_int (loc, 0), Ast.Dict.empty));
                      ( loc,
                        Tagged
                          ( "tag",
                            Tag_int (loc, 1),
                            Ast.Dict.singleton "a"
                              (Tuple
                                 [ Named (loc, "float"); Enum_bool [ 1; 0 ] ])
                          ) );
                    ],
                    `Closed );
            };
            {
              loc;
              name = "d";
              ty =
                Record
                  ( [
                      ( loc,
                        Tagged
                          ( "tag",
                            Tag_string (loc, "a"),
                            Ast.Dict.singleton "a" (Named (loc, "float")) ) );
                      ( loc,
                        Tagged
                          ( "tag",
                            Tag_string (loc, "b"),
                            Ast.Dict.singleton "a" (Enum_int ([ 0; 1 ], `Open))
                          ) );
                    ],
                    `Closed );
            };
            {
              loc;
              name = "e";
              ty =
                Record
                  ( [
                      ( loc,
                        Tagged
                          ( "tag",
                            Tag_int (loc, 0),
                            Ast.Dict.singleton "a" (Named (loc, "_")) ) );
                      ( loc,
                        Tagged
                          ( "tag",
                            Tag_int (loc, 1),
                            Ast.Dict.singleton "b"
                              (Enum_string ([ "a"; "b" ], `Open)) ) );
                    ],
                    `Open );
            };
            { loc; name = "children"; ty = Named (loc, "string") };
            {
              loc;
              name = "optionalChildren";
              ty = Nullable (Named (loc, "string"));
            };
          ] );
      Text ("", No_trim, No_trim);
    ]
    (parse src)

let precedence () =
  let src = "{% match !!a.b.c with !!false %}{% with _ %}{% /match %}" in
  check "! and . precedence works correctly"
    [
      Text ("", No_trim, No_trim);
      Match
        ( loc,
          [
            Nullable
              ( loc,
                Some
                  (Nullable
                     ( loc,
                       Some (Field (loc, Field (loc, Var (loc, "a"), "b"), "c"))
                     )) );
          ],
          [
            {
              pats =
                [
                  ( loc,
                    [
                      Nullable (loc, Some (Nullable (loc, Some (Bool (loc, 0)))));
                    ] );
                ];
              nodes = [ Text ("", No_trim, No_trim) ];
            };
            {
              pats = [ (loc, [ Var (loc, "_") ]) ];
              nodes = [ Text ("", No_trim, No_trim) ];
            };
          ] );
      Text ("", No_trim, No_trim);
    ]
    (parse src)

let edge_cases () =
  let src = {|{% match a with {a: {b}} %} {{ b }} {% /match %}|} in
  check "Patterns with }} parse correctly"
    [
      Text ("", No_trim, No_trim);
      Match
        ( loc,
          [ Var (loc, "a") ],
          [
            {
              pats =
                [
                  ( loc,
                    [
                      Record
                        ( loc,
                          Untagged
                            (Ast.Dict.singleton "a"
                               (Ast.Record
                                  ( loc,
                                    Untagged
                                      (Ast.Dict.singleton "b"
                                         (Ast.Var (loc, "b"))) ))) );
                    ] );
                ];
              nodes =
                [
                  Text (" ", No_trim, No_trim);
                  Echo ([], Fmt_string, Echo_var (loc, "b"), Escape);
                  Text (" ", No_trim, No_trim);
                ];
            };
          ] );
      Text ("", No_trim, No_trim);
    ]
    (parse src)

let () =
  let open Alcotest in
  run "Parser"
    [
      ( "Parser tests",
        [
          test_case "Echoes" `Quick echoes;
          test_case "Numbers" `Quick numbers;
          test_case "Trim" `Quick trim;
          test_case "Comments" `Quick comments;
          test_case "Matches" `Quick matches;
          test_case "Maps" `Quick maps;
          test_case "Components" `Quick components;
          test_case "Patterns" `Quick patterns;
          test_case "Interface" `Quick edge_cases;
          test_case "Precedence" `Quick precedence;
          test_case "Edge cases" `Quick interface;
        ] );
    ]
