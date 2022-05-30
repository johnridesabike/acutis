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
%token AMPERSAND              (* & *)
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
  | EXCLAMATION; p = pattern;                   { Nullable (Some p) }
  | LEFT_BRACK; l = pattern_list; RIGHT_BRACK;  { l }
  | LEFT_PAREN; l = tuple; RIGHT_PAREN;         { Tuple l }
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
  | tl = option(ELLIPSIS; p = pattern { p });
    { List ([], tl) }
  | l = pattern_list_nonempty; tl = option(COMMA; ELLIPSIS; p = pattern; { p });
    { Pattern.List (Nonempty.to_list l, tl) }

tuple:
  | (* empty *)                { [] }
  | l = pattern_list_nonempty; { Nonempty.to_list l }

(* Match & map rules *)
%inline pattern_list_nonempty: l = pattern_list_nonempty_rev; { Nonempty.rev l }
pattern_list_nonempty_rev:
  | p = pattern;                                       { [ p ] }
  | l = pattern_list_nonempty_rev; COMMA; p = pattern; { Nonempty.cons p l }

with_pats: l = with_pats_rev; { Nonempty.rev l }
with_pats_rev:
  | WITH; ps = pattern_list_nonempty;                    { [ ps ] }
  | l = with_pats_rev; WITH; ps = pattern_list_nonempty; { Nonempty.cons ps l }

cases: l = cases_rev; { Nonempty.rev l }
cases_rev:
  | pats = with_pats; child = nodes;
    { [ {pats; nodes = child} ] }
  | l = cases_rev; pats = with_pats; child = nodes;
    { Nonempty.cons {pats; nodes = child} l }

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
escape:
  | (* empty *) { Escape }
  | AMPERSAND;  { No_escape }

echo:
  | e = escape; x = ID; { Ech_var (x, e) }
  | x = COMPONENT;      { Ech_component x }
  | s = STRING;         { Ech_string s }

echoes:
  | l = echoes_rev; { let Nonempty.(last :: l) = l in Echo (List.rev l, last) }
echoes_rev:
  | x = echo;                               { [ x ] }
  | l = echoes_rev; QUESTION; last = echo;  { Nonempty.cons last l }

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
  | MAP_DICT; pat = pattern; child = cases; BACKSLASH; MAP_DICT;
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

nodes: l = nodes_rev; { List.rev l }
nodes_rev:
  | n = text;                 { [n] }
  | l = nodes_rev; n = node;  { n :: l }

acutis: n = nodes; EOF; { n }
