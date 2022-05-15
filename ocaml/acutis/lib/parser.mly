(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

%{
open Ast
%}

%token <string> TEXT

(* Expression syntax *)
%token ECHO_BEGIN             (* {{ *)
%token ECHO_END               (* }} *)
%token TILDE_LEFT             (* ~%} or ~}} *)
%token TILDE_RIGHT            (* {%~ or {{~ *)
%token QUESTION               (* ? *)
%token MATCH                  (* match *)
%token MAP                    (* map *)
%token MAP_DICT               (* map_dict *)
%token WITH                   (* with *)
%token BACKSLASH              (* / *)
%token EQUALS                 (* = *)
%token HASH                   (* # *)
%token EOF

(* Pattern syntax *)
%token <string> ID
%token <string> COMPONENT
%token NULL                   (* null *)
%token EXCLAMATION            (* ! *)
%token TRUE                   (* true *)
%token FALSE                  (* false *)
%token <int>    INT           (* e.g. 1 *)
%token <float>  FLOAT         (* e.g. 1.5 *)
%token <string> STRING        (* e.g. "acutis" *)
%token LEFT_BRACK RIGHT_BRACK (* [ ] *)
%token LEFT_BRACE RIGHT_BRACE (* { } *)
%token LEFT_PAREN RIGHT_PAREN (* ( ) *)
%token LEFT_ANGLE RIGHT_ANGLE (* < > *)
%token COLON                  (* : *)
%token AT                     (* @ *)
%token COMMA                  (* , *)
%token ELLIPSIS               (* ... *)

%start <Ast.t> acutis

%%

(* Pattern rules *)
pattern:
  | x = ID;                                     { Var x }
  | TRUE;                                       { Bool 1 }
  | FALSE;                                      { Bool 0 }
  | i = INT;                                    { Int i }
  | i = FLOAT;                                  { Float i }
  | s = STRING;                                 { String s }
  | AT; i = INT;                                { Enum_int i }
  | AT; s = STRING;                             { Enum_string s }
  | NULL;                                       { Nullable None }
  | EXCLAMATION; pat = pattern;                 { Nullable (Some pat) }
  | LEFT_BRACK; l = pattern_list; RIGHT_BRACK;  { let l, tl = l in List (l, tl)}
  | LEFT_PAREN;
    l = pattern_list_nonempty;
    RIGHT_PAREN;                                { Tuple l }
  | LEFT_BRACE; r = record; RIGHT_BRACE;        { Record r }
  | LEFT_ANGLE; d = dict; RIGHT_ANGLE;          { Dict d }

record_field:
  | t = boption(AT); k = ID; COLON; v = pattern;
  | t = boption(AT); k = STRING; COLON; v = pattern; { (t, k, v) }
  | t = boption(AT); k = ID;                         { (t, k, Pattern.Var k ) }

record:
  | x = record_field;
    { let (t, k, v) = x in Record.singleton t k v }
  | m = record; COMMA; x = record_field;
    { let (t, k, v) = x in Record.add t k v m }

dict_field:
  | k = ID; COLON; v = pattern;
  | k = STRING; COLON; v = pattern; { (k, v) }
  | k = ID;                         { (k, Pattern.Var k ) }

dict:
  | x = dict_field;
    { let (k, v) = x in Dict.singleton k v }
  | m = dict; COMMA; x = dict_field;
    { let (k, v) = x in Dict.add k v m }

pattern_list:
  | tl = option(ELLIPSIS; p = pattern { p }); { ([], tl) }
  | l = pattern_list_nonempty; tl = option(COMMA; ELLIPSIS; p = pattern; { p });
    { (l, tl) }

(* Match & map rules *)
%inline pattern_list_nonempty: l = rev_pattern_list_nonempty; { List.rev l }
rev_pattern_list_nonempty:
  | p = pattern;                                       { [p] }
  | l = rev_pattern_list_nonempty; COMMA; p = pattern; { p :: l }

%inline with_pats: l = rev_with_pats; { List.rev l }
rev_with_pats:
  | WITH; ps = pattern_list_nonempty;                    { [ps] }
  | l = rev_with_pats; WITH; ps = pattern_list_nonempty; { ps :: l }

%inline cases: l = rev_cases; { List.rev l }
rev_cases:
  | pats = with_pats; child = nodes;
    { [{pats; nodes = child}] }
  | l = rev_cases; pats = with_pats; child = nodes;
    { {pats; nodes = child} :: l }

(* Component rules *)
props:
  | (* empty *) { (Dict.empty, Dict.empty) }
  | p = props; k = ID; EQUALS; v = pattern;
    { let (pats, childs) = p in (Dict.add k v pats, childs) }
  | p = props; k = ID;
    { let (pats, childs) = p in (Dict.add k (Pattern.Var k) pats, childs) }
  | p = props; k = COMPONENT; EQUALS; HASH; v = nodes; BACKSLASH; HASH;
    { let (pats, childs) = p in (pats, Dict.add k (Child_block v) childs) }
  | p = props; k = COMPONENT; EQUALS; v = COMPONENT;
    { let (pats, childs) = p in (pats, Dict.add k (Child_name v) childs) }
  | p = props; k = COMPONENT;
    { let (pats, childs) = p in (pats, Dict.add k (Child_name k) childs) }

(* Echo rules *)
echo:
  | x = ID;         { Ech_var x }
  | x = COMPONENT;  { Ech_component x }
  | s = STRING;     { Ech_string s }

%inline echoes: e = rev_echoes; { let (l, last) = e in Echo (List.rev l, last) }
rev_echoes:
  | x = echo;
    { ([], x) }
  | echoes = rev_echoes; QUESTION; last = echo;
    { let (tl, hd) = echoes in (hd :: tl, last) }

trim_left:
  | (* empty *)  { No_trim }
  | TILDE_LEFT   { Trim }

trim_right:
  | (* empty *)  { No_trim }
  | TILDE_RIGHT  { Trim }

text:
  | l = trim_left; txt = TEXT; r = trim_right; { Text (txt, l, r) }

(* Putting it all together *)
node:
  | txt = text;
    { txt }
  | ECHO_BEGIN; e = echoes; ECHO_END;
    { e }
  | MATCH; pats = pattern_list_nonempty; child = cases; BACKSLASH; MATCH;
    { Match (pats, child) }
  | MAP; pat = pattern; child = cases; BACKSLASH; MAP;
    { Map_list (pat, child) }
  | MAP_DICT; pat = pattern; child = cases; BACKSLASH; MAP;
    { Map_dict (pat, child) }
  | x = COMPONENT; p = props; BACKSLASH;
    { let (p, c) = p in Component (x, p, c) }
  | x1 = COMPONENT; p = props; n = nodes; BACKSLASH; x2 = COMPONENT;
    {
      assert (String.equal x1 x2);
      let (p, c) = p in
      let children = Child_block n in
      Component (x1, p, Dict.add "Children" children c)
    }

%inline nodes: l = rev_nodes; { List.rev l }
rev_nodes:
  | n = text;                 { [n] }
  | l = rev_nodes; n = node;  { n :: l }

acutis: n = nodes; EOF; { n }
