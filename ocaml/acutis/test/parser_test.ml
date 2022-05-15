open Acutis
module F = Format

let print_position fmt lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  F.fprintf fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse src =
  let state = Lexer.make_state () in
  let lexbuf = Lexing.from_string src in
  try Ok (Parser.acutis (Lexer.acutis state) lexbuf) with
  | Lexer.SyntaxError ->
      Error (F.asprintf "Lexer.SyntaxError %a" print_position lexbuf)
  | Parser.Error i ->
      Error (F.asprintf "Parser.Error %i %a" i print_position lexbuf)

let check = Alcotest.(check (result (module Ast) string))

let echoes () =
  let src = {|{{ a }} {{ "b" }} {{ C }} {{ d ? E ? "f" }}|} in
  check "Echoes"
    (Ok
       [
         Text ("", No_trim, No_trim);
         Echo ([], Ech_var "a");
         Text (" ", No_trim, No_trim);
         Echo ([], Ech_string "b");
         Text (" ", No_trim, No_trim);
         Echo ([], Ech_component "C");
         Text (" ", No_trim, No_trim);
         Echo ([ Ech_var "d"; Ech_component "E" ], Ech_string "f");
         Text ("", No_trim, No_trim);
       ])
    (parse src)

let trim () =
  let src = {|{{~ a }} {{~ b }} {{ c ~}} {{~ d ~}}|} in
  check "Trim"
    (Ok
       [
         Text ("", No_trim, Trim);
         Echo ([], Ech_var "a");
         Text (" ", No_trim, Trim);
         Echo ([], Ech_var "b");
         Text (" ", No_trim, No_trim);
         Echo ([], Ech_var "c");
         Text (" ", Trim, Trim);
         Echo ([], Ech_var "d");
         Text ("", Trim, No_trim);
       ])
    (parse src)

let comments () =
  let src = {|
  a {* {* *} *}b{*
  *} c|} in
  check "Comments" (Ok [ Text ("\n  a b c", No_trim, No_trim) ]) (parse src)

let matches () =
  let src =
    {|
  {% match a with 1 with 2 %}{% with 3 %} {% with _ %} {% /match %}
  {% match b with c %}{% match d, e with f, g %} {% /match /match %}|}
  in
  check "Matches"
    (Ok
       [
         Text ("\n  ", No_trim, No_trim);
         Match
           ( [ Var "a" ],
             [
               {
                 pats = [ [ Int 1 ]; [ Int 2 ] ];
                 nodes = [ Text ("", No_trim, No_trim) ];
               };
               {
                 pats = [ [ Int 3 ] ];
                 nodes = [ Text (" ", No_trim, No_trim) ];
               };
               {
                 pats = [ [ Var "_" ] ];
                 nodes = [ Text (" ", No_trim, No_trim) ];
               };
             ] );
         Text ("\n  ", No_trim, No_trim);
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
       ])
    (parse src)

let maps () =
  let src =
    {|
  {% map l with 1 with 2 %}{% with 3, i %} {% with _ %} {% /map %}
  {% map_dict d with 1 with 2 %}{% with 3, k %} {% with _ %} {% /map %}|}
  in
  check "Maps"
    (Ok
       [
         Text ("\n  ", No_trim, No_trim);
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
               {
                 pats = [ [ Var "_" ] ];
                 nodes = [ Text (" ", No_trim, No_trim) ];
               };
             ] );
         Text ("\n  ", No_trim, No_trim);
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
               {
                 pats = [ [ Var "_" ] ];
                 nodes = [ Text (" ", No_trim, No_trim) ];
               };
             ] );
         Text ("", No_trim, No_trim);
       ])
    (parse src)

let components () =
  let src =
    {|
  {% Template
    a=b
    c
    D=E
    F
    G=#%} {%/#
    H=#%}{% match a with b %} {% /match /#
  /%}
  {% Another %} {% /Another %}|}
  in
  check "Components"
    (Ok
       [
         Text ("\n  ", No_trim, No_trim);
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
         Text ("\n  ", No_trim, No_trim);
         Component
           ( "Another",
             Ast.Dict.empty,
             Ast.Dict.singleton "Children"
               (Ast.Child_block [ Text (" ", No_trim, No_trim) ]) );
         Text ("", No_trim, No_trim);
       ])
    (parse src)

let patterns () =
  let src =
    {|
  {% match tuple with (1, 2.5, "a") %} {% with _ %} {% /match %}
  {% match list
      with [] %} {% with [!a, null] %} {% with [z, ...tl] %} {% /match %}
  {% match record with {a, "!#%@": b} %} {% with _ %} {% /match %}
  {% match enums with (@"a", @1, true, false) %} {% with _ %} {% /match %}
  {% match tagged with {@tag: true, a} %} {% with {@tag: false} %} {% /match %}
  {% match dict with <a: 1, b: 2> %} {% with _ %} {% /match %}|}
  in
  check "Patterns"
    (Ok
       [
         Text ("\n  ", No_trim, No_trim);
         Match
           ( [ Var "tuple" ],
             [
               {
                 pats = [ [ Tuple [ Int 1; Float 2.5; String "a" ] ] ];
                 nodes = [ Text (" ", No_trim, No_trim) ];
               };
               {
                 pats = [ [ Var "_" ] ];
                 nodes = [ Text (" ", No_trim, No_trim) ];
               };
             ] );
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
                     [
                       List ([ Nullable (Some (Var "a")); Nullable None ], None);
                     ];
                   ];
                 nodes = [ Text (" ", No_trim, No_trim) ];
               };
               {
                 pats = [ [ List ([ Var "z" ], Some (Var "tl")) ] ];
                 nodes = [ Text (" ", No_trim, No_trim) ];
               };
             ] );
         Text ("\n  ", No_trim, No_trim);
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
               {
                 pats = [ [ Var "_" ] ];
                 nodes = [ Text (" ", No_trim, No_trim) ];
               };
             ] );
         Text ("\n  ", No_trim, No_trim);
         Match
           ( [ Var "enums" ],
             [
               {
                 pats =
                   [ [ Tuple [ Enum_string "a"; Enum_int 1; Bool 1; Bool 0 ] ] ];
                 nodes = [ Text (" ", No_trim, No_trim) ];
               };
               {
                 pats = [ [ Var "_" ] ];
                 nodes = [ Text (" ", No_trim, No_trim) ];
               };
             ] );
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
                              Ast.Dict.(empty |> add "a" (Ast.Pattern.Var "a"))
                            ));
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
               {
                 pats = [ [ Var "_" ] ];
                 nodes = [ Text (" ", No_trim, No_trim) ];
               };
             ] );
         Text ("", No_trim, No_trim);
       ])
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
        ] );
    ]
