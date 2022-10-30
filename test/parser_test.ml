open Acutis
module F = Format

let parse src = Compile.parse ~fname:"<test>" (Lexing.from_string src)
let check = Alcotest.(check (module DebugAst))
let loc = Loc.dummy

let echoes () =
  let src = {|{{ a }} {{ "b" }} {{ C }} {{ &d ? E ? "f\"g" }}|} in
  check "Echoes parse correctly"
    [
      Text ("", No_trim, No_trim);
      Echo ([], Ech_var (loc, "a", Escape));
      Text (" ", No_trim, No_trim);
      Echo ([], Ech_string (loc, "b"));
      Text (" ", No_trim, No_trim);
      Echo ([], Ech_component (loc, "C"));
      Text (" ", No_trim, No_trim);
      Echo
        ( [ Ech_var (loc, "d", No_escape); Ech_component (loc, "E") ],
          Ech_string (loc, "f\"g") );
      Text ("", No_trim, No_trim);
    ]
    (parse src)

let trim () =
  let src = {|{{~ a }} {{~ b }} {{ c ~}} {{~ d ~}}|} in
  check "Trim parses correctly"
    [
      Text ("", No_trim, Trim);
      Echo ([], Ech_var (loc, "a", Escape));
      Text (" ", No_trim, Trim);
      Echo ([], Ech_var (loc, "b", Escape));
      Text (" ", No_trim, No_trim);
      Echo ([], Ech_var (loc, "c", Escape));
      Text (" ", Trim, Trim);
      Echo ([], Ech_var (loc, "d", Escape));
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
    {|{% match b with c %}{% match d, e with f, g %} {% /match /match %}|}
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
    D=E
    F
    G=#%} {%/#
    H=#%}{% match a with b %} {% /match /#
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
            |> add loc "a" (Ast.Pattern.Var (loc, "b"))
            |> add loc "c" (Ast.Pattern.Var (loc, "c"))),
          Ast.Dict.(
            empty
            |> add loc "D" (Ast.Child_name (loc, "E"))
            |> add loc "F" (Ast.Child_name (loc, "F"))
            |> add loc "G" (Ast.Child_block [ Text (" ", No_trim, No_trim) ])
            |> add loc "H"
                 (Ast.Child_block
                    [
                      Text ("", No_trim, No_trim);
                      Match
                        ( loc,
                          [ Var (loc, "a") ],
                          [
                            {
                              pats = [ (loc, [ Var (loc, "b") ]) ];
                              nodes = [ Text (" ", No_trim, No_trim) ];
                            };
                          ] );
                    ])) );
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
          Ast.Dict.empty,
          Ast.Dict.singleton "Children"
            (Ast.Child_block [ Text (" ", No_trim, No_trim) ]) );
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
                              |> add loc "a" (Ast.Pattern.Var (loc, "a"))
                              |> add loc "!#%@" (Ast.Pattern.Var (loc, "b"))) );
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
                              Ast.Dict.singleton "a"
                                (Ast.Pattern.Var (loc, "a")) ) );
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
                            |> add loc "a" (Ast.Pattern.Int (loc, 1))
                            |> add loc "b" (Ast.Pattern.Int (loc, 2))) );
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
    Children
    OptionalChildren = ?
  / %}|}
  in
  let open Ast.Interface in
  check "Interfaces parse correctly."
    [
      Text ("", No_trim, No_trim);
      Interface
        ( loc,
          [
            Type
              ( loc,
                "a",
                Record
                  ( [
                      ( loc,
                        Untagged
                          Ast.Dict.(
                            singleton "a" (Enum_int ([ 0; 1 ], `Closed))
                            |> add loc "b" (Enum_string ([ "a"; "b" ], `Closed)))
                      );
                    ],
                    `Closed ) );
            Type
              ( loc,
                "b",
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
                    `Closed ) );
            Type
              ( loc,
                "c",
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
                    `Closed ) );
            Type
              ( loc,
                "d",
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
                    `Closed ) );
            Type
              ( loc,
                "e",
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
                    `Open ) );
            Child (loc, "Children");
            Child_nullable (loc, "OptionalChildren");
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
                               (Ast.Pattern.Record
                                  ( loc,
                                    Untagged
                                      (Ast.Dict.singleton "b"
                                         (Ast.Pattern.Var (loc, "b"))) ))) );
                    ] );
                ];
              nodes =
                [
                  Text (" ", No_trim, No_trim);
                  Echo ([], Ech_var (loc, "b", Escape));
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
          test_case "Trim" `Quick trim;
          test_case "Comments" `Quick comments;
          test_case "Matches" `Quick matches;
          test_case "Maps" `Quick maps;
          test_case "Components" `Quick components;
          test_case "Patterns" `Quick patterns;
          test_case "Interface" `Quick edge_cases;
          test_case "Edge cases" `Quick interface;
        ] );
    ]
