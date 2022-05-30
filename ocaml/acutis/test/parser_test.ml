open Acutis
module F = Format

let print_position ppf lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  F.fprintf ppf "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse src =
  let state = Lexer.make_state () in
  let lexbuf = Lexing.from_string src in
  try Parser.acutis (Lexer.acutis state) lexbuf with
  | Lexer.SyntaxError as e ->
      F.printf "Lexer.SyntaxError %a" print_position lexbuf;
      raise e
  | Parser.Error i as e ->
      F.printf "Parser.Error %i %a" i print_position lexbuf;
      raise e

let check = Alcotest.(check (module Ast))

let echoes () =
  let src = {|{{ a }} {{ "b" }} {{ C }} {{ &d ? E ? "f" }}|} in
  check "Echoes parse correctly"
    [
      Text ("", No_trim, No_trim);
      Echo ([], Ech_var ("a", Escape));
      Text (" ", No_trim, No_trim);
      Echo ([], Ech_string "b");
      Text (" ", No_trim, No_trim);
      Echo ([], Ech_component "C");
      Text (" ", No_trim, No_trim);
      Echo ([ Ech_var ("d", No_escape); Ech_component "E" ], Ech_string "f");
      Text ("", No_trim, No_trim);
    ]
    (parse src)

let trim () =
  let src = {|{{~ a }} {{~ b }} {{ c ~}} {{~ d ~}}|} in
  check "Trim parses correctly"
    [
      Text ("", No_trim, Trim);
      Echo ([], Ech_var ("a", Escape));
      Text (" ", No_trim, Trim);
      Echo ([], Ech_var ("b", Escape));
      Text (" ", No_trim, No_trim);
      Echo ([], Ech_var ("c", Escape));
      Text (" ", Trim, Trim);
      Echo ([], Ech_var ("d", Escape));
      Text ("", Trim, No_trim);
    ]
    (parse src)

let comments () =
  let src = {|
  a {* {* *} *}b{*
  *} c|} in
  check "Comments parse correctly"
    [ Text ("\n  a b c", No_trim, No_trim) ]
    (parse src)

let matches () =
  let src =
    {|{% match a with 1 with 2 %}{% with 3 %} {% with _ %} {% /match %}|}
  in
  check "Flat matches parse correctly"
    [
      Text ("", No_trim, No_trim);
      Match
        ( [ Var "a" ],
          [
            {
              pats = [ [ Int 1 ]; [ Int 2 ] ];
              nodes = [ Text ("", No_trim, No_trim) ];
            };
            { pats = [ [ Int 3 ] ]; nodes = [ Text (" ", No_trim, No_trim) ] };
            { pats = [ [ Var "_" ] ]; nodes = [ Text (" ", No_trim, No_trim) ] };
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
        ( [ Var "b" ],
          [
            {
              pats = [ [ Var "c" ] ];
              nodes =
                [
                  Text ("", No_trim, No_trim);
                  Match
                    ( [ Var "d"; Var "e" ],
                      [
                        {
                          pats = [ [ Var "f"; Var "g" ] ];
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
        ( Var "l",
          [
            {
              pats = [ [ Int 1 ]; [ Int 2 ] ];
              nodes = [ Text ("", No_trim, No_trim) ];
            };
            {
              pats = [ [ Int 3; Var "i" ] ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            { pats = [ [ Var "_" ] ]; nodes = [ Text (" ", No_trim, No_trim) ] };
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
        ( Var "d",
          [
            {
              pats = [ [ Int 1 ]; [ Int 2 ] ];
              nodes = [ Text ("", No_trim, No_trim) ];
            };
            {
              pats = [ [ Int 3; Var "k" ] ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            { pats = [ [ Var "_" ] ]; nodes = [ Text (" ", No_trim, No_trim) ] };
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
        ( "Template",
          Ast.Dict.(
            empty
            |> add "a" (Ast.Pattern.Var "b")
            |> add "c" (Ast.Pattern.Var "c")),
          Ast.Dict.(
            empty
            |> add "D" (Ast.Child_name "E")
            |> add "F" (Ast.Child_name "F")
            |> add "G" (Ast.Child_block [ Text (" ", No_trim, No_trim) ])
            |> add "H"
                 (Ast.Child_block
                    [
                      Text ("", No_trim, No_trim);
                      Match
                        ( [ Var "a" ],
                          [
                            {
                              pats = [ [ Var "b" ] ];
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
        ( "Template",
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
        ( [ Var "tuple" ],
          [
            {
              pats = [ [ Tuple [ Int 1; Float 2.5; String "a" ] ] ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            { pats = [ [ Var "_" ] ]; nodes = [ Text (" ", No_trim, No_trim) ] };
          ] );
      Text ("", No_trim, No_trim);
    ]
    (parse src);
  let src =
    {|
  {% match list
      with [] %} {% with [!a, null] %} {% with [z, ...tl] %} {% /match %}|}
  in
  check "List patterns parse correctly"
    [
      Text ("\n  ", No_trim, No_trim);
      Match
        ( [ Var "list" ],
          [
            {
              pats = [ [ List ([], None) ] ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            {
              pats =
                [
                  [ List ([ Nullable (Some (Var "a")); Nullable None ], None) ];
                ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            {
              pats = [ [ List ([ Var "z" ], Some (Var "tl")) ] ];
              nodes = [ Text (" ", No_trim, No_trim) ];
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
        ( [ Var "record" ],
          [
            {
              pats =
                [
                  [
                    Record
                      (Untagged
                         Ast.Dict.(
                           empty
                           |> add "a" (Ast.Pattern.Var "a")
                           |> add "!#%@" (Ast.Pattern.Var "b")));
                  ];
                ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            { pats = [ [ Var "_" ] ]; nodes = [ Text (" ", No_trim, No_trim) ] };
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
        ( [ Var "enums" ],
          [
            {
              pats =
                [ [ Tuple [ Enum_string "a"; Enum_int 1; Bool 1; Bool 0 ] ] ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            { pats = [ [ Var "_" ] ]; nodes = [ Text (" ", No_trim, No_trim) ] };
          ] );
      Text ("", No_trim, No_trim);
    ]
    (parse src);
  let src =
    {|
  {% match tagged with {@tag: true, a} %} {% with {@tag: false} %} {% /match %}
  |}
  in
  check "Tagged union patterns parse correctly"
    [
      Text ("\n  ", No_trim, No_trim);
      Match
        ( [ Var "tagged" ],
          [
            {
              pats =
                [
                  [
                    Record
                      (Tagged
                         ( "tag",
                           Bool 1,
                           Ast.Dict.(empty |> add "a" (Ast.Pattern.Var "a")) ));
                  ];
                ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            {
              pats = [ [ Record (Tagged ("tag", Bool 0, Ast.Dict.empty)) ] ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
          ] );
      Text ("\n  ", No_trim, No_trim);
    ]
    (parse src);
  let src = {|{% match dict with <a: 1, b: 2> %} {% with _ %} {% /match %}|} in
  check "Dictionary patterns parse correctly"
    [
      Text ("", No_trim, No_trim);
      Match
        ( [ Var "dict" ],
          [
            {
              pats =
                [
                  [
                    Dict
                      Ast.Dict.(
                        empty
                        |> add "a" (Ast.Pattern.Int 1)
                        |> add "b" (Ast.Pattern.Int 2));
                  ];
                ];
              nodes = [ Text (" ", No_trim, No_trim) ];
            };
            { pats = [ [ Var "_" ] ]; nodes = [ Text (" ", No_trim, No_trim) ] };
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
        ( [ Var "a" ],
          [
            {
              pats =
                [
                  [
                    Record
                      (Untagged
                         (Ast.Dict.singleton "a"
                            (Ast.Pattern.Record
                               (Untagged
                                  (Ast.Dict.singleton "b" (Ast.Pattern.Var "b"))))));
                  ];
                ];
              nodes =
                [
                  Text (" ", No_trim, No_trim);
                  Echo ([], Ech_var ("b", Escape));
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
          test_case "Edge cases" `Quick edge_cases;
        ] );
    ]
