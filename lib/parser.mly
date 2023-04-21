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
%token TRIPLE_ECHO_BEGIN      (* {{{ *)
%token ECHO_END               (* }} *)
%token TRIPLE_ECHO_END        (* }}} *)
%token TILDE_LEFT             (* ~%} or ~}} *)
%token TILDE_RIGHT            (* {%~ or {{~ *)
%token QUESTION               (* ? *)
%token MATCH                  (* match *)
%token MAP                    (* map *)
%token MAP_DICT               (* map_dict *)
%token WITH                   (* with *)
%token INTERFACE              (* interface *)
%token BACKSLASH              (* / *)
%token EQUALS                 (* = *)
%token HASH                   (* # *)
%token EOF

(* Pattern syntax *)
%token <string> ID            (* e.g. acutis *)
%token <string> COMPONENT     (* e.g. Acutis *)
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
%token PIPE                   (* | *)
%token DOT                    (* . *)

%nonassoc EXCLAMATION         (* lowest precedence *)
%nonassoc DOT                 (* highest precedence *)

(* Echo format syntax *)
%token PERCENT                (* % *)
%token CHAR_I                 (* i *)
%token CHAR_F                 (* f *)
%token CHAR_E                 (* e *)
%token CHAR_G                 (* g *)
%token CHAR_B                 (* b *)

%start <Ast.t> acutis
%start <Ast.Interface.t list> interface_standalone

%%

(** Pattern rules *)

pattern:
  | x = ID;                                     { Var ($loc, x) }
  | TRUE;                                       { Bool ($loc, 1) }
  | FALSE;                                      { Bool ($loc, 0) }
  | i = INT;                                    { Int ($loc, i) }
  | i = FLOAT;                                  { Float ($loc, i) }
  | s = STRING;                                 { String ($loc, s) }
  | AT; i = INT;                                { Enum_int ($loc, i) }
  | AT; s = STRING;                             { Enum_string ($loc, s) }
  | NULL;                                       { Nullable ($loc, None) }
  | EXCLAMATION; p = pattern;                   { Nullable ($loc, Some p) }
  | LEFT_BRACK; l = pattern_list; RIGHT_BRACK;  { l }
  | LEFT_PAREN; l = tuple; RIGHT_PAREN;         { Tuple ($loc, l) }
  | LEFT_BRACE; r = record; RIGHT_BRACE;        { Record ($loc, r) }
  | LEFT_ANGLE; d = dict; RIGHT_ANGLE;          { Dict ($loc, d) }
  | HASH; x = nodes; HASH;                      { Block ($loc, x) }
  | HASH; HASH;                                 { Block ($loc, []) }
  | p = pattern; DOT; s = record_key;           { Field ($loc, p, s) }

record_tag:
  | i = INT;    { Tag_int ($loc, i) }
  | FALSE;      { Tag_bool ($loc, 0) }
  | TRUE;       { Tag_bool ($loc, 1) }
  | s = STRING; { Record.Tag_string ($loc, s) }

record_key: k = ID; | k = STRING; { k }

record_field:
  | AT; k = record_key; COLON; v = record_tag;  { `Tag (k, v) }
  | k = record_key; COLON; v = pattern;         { `Notag (k, v) }
  | k = ID;                                     { `Notag (k, Var ($loc, k) ) }

record:
  | x = record_field;                     { Record.singleton x }
  | m = record; COMMA; x = record_field;  { Record.add $loc x m }

dict_field:
  | k = record_key; COLON; v = pattern; { (k, v) }
  | k = ID;                             { (k, Var ($loc, k) ) }

dict:
  | x = dict_field;
    { let (k, v) = x in Dict.singleton k v }
  | m = dict; COMMA; x = dict_field;
    { let (k, v) = x in Dict.add $loc k v m }

pattern_list:
  | tl = option(ELLIPSIS; p = pattern { p });
    { List ($loc, [], tl) }
  | l = pattern_list_nonempty; tl = option(COMMA; ELLIPSIS; p = pattern; { p });
    { List ($loc, Nonempty.to_list l, tl) }

tuple:
  | (* empty *)                { [] }
  | l = pattern_list_nonempty; { Nonempty.to_list l }

(** Match & map rules *)

%inline pattern_list_nonempty: l = pattern_list_nonempty_rev; { Nonempty.rev l }
pattern_list_nonempty_rev:
  | p = pattern;                                       { [ p ] }
  | l = pattern_list_nonempty_rev; COMMA; p = pattern; { Nonempty.cons p l }

with_pats:
  | WITH; ps = pattern_list_nonempty;
    { [ ($loc, ps) ] }
  | WITH; ps = pattern_list_nonempty; l = with_pats;
    { Nonempty.cons ($loc, ps) l }

cases:
  | pats = with_pats; child = nodes;
    { [ {pats; nodes = child} ] }
  | pats = with_pats; child = nodes; l = cases;
    { Nonempty.cons {pats; nodes = child} l }

(** Component rules *)

props:
  | (* empty *)
    { Dict.empty }
  | p = props; k = ID; EQUALS; v = pattern;
    { Dict.add $loc k v p }
  | p = props; k = ID;
    { Dict.add $loc k (Var ($loc, k)) p }

(** Echo rules *)

echo_precision:
  | (* empty *)   { 6 }
  | DOT; i = INT; { i }

echo_flag:
  | (* empty *) { No_flag }
  | COMMA;      { Flag_comma }

echo_format:
  | (* empty *)                           { Fmt_string }
  | PERCENT; f = echo_flag; CHAR_I;       { Fmt_int f }
  | PERCENT; i = echo_precision; CHAR_F;  { Fmt_float i }
  | PERCENT; i = echo_precision; CHAR_E;  { Fmt_float_e i }
  | PERCENT; i = echo_precision; CHAR_G;  { Fmt_float_g i }
  | PERCENT; CHAR_B;                      { Fmt_bool }

echo:
  | s = ID;                         { Echo_var ($loc, s) }
  | s = STRING;                     { Echo_string ($loc, s) }
  | e = echo; DOT; s = record_key;  { Echo_field (e, s) }

echoes_rev:
  | fmt = echo_format; e = echo;
    { [ (fmt, e) ] }
  | l = echoes_rev; QUESTION; fmt = echo_format; last = echo;
    { Nonempty.cons (fmt, last) l }

trim_left:
  | (* empty *)  { No_trim }
  | TILDE_LEFT   { Trim }

trim_right:
  | (* empty *)  { No_trim }
  | TILDE_RIGHT  { Trim }

text: l = trim_left; txt = TEXT; r = trim_right; { Text (txt, l, r) }

(** Putting it all together *)

node:
  | txt = text;
    { txt }
  | ECHO_BEGIN; e = echoes_rev; ECHO_END;
    {
      let Nonempty.((fmt, default) :: l) = e in
      Echo (List.rev l, fmt, default, Escape)
    }
  | TRIPLE_ECHO_BEGIN; e = echoes_rev; TRIPLE_ECHO_END;
    {
      let Nonempty.((fmt, default) :: l) = e in
      Echo (List.rev l, fmt, default, No_escape)
    }
  | MATCH; pats = pattern_list_nonempty; child = cases; BACKSLASH; MATCH;
    { Match ($loc, pats, child) }
  | MAP; pat = pattern; child = cases; BACKSLASH; MAP;
    { Map_list ($loc, pat, child) }
  | MAP_DICT; pat = pattern; child = cases; BACKSLASH; MAP_DICT;
    { Map_dict ($loc, pat, child) }
  | x = COMPONENT; p = props; BACKSLASH;
    { Component ($loc, x, x, p) }
  | x1 = COMPONENT; p = props; n = nodes; BACKSLASH; x2 = COMPONENT;
    { Component ($loc, x1, x2, Dict.add $loc "children" (Block ($loc, n)) p) }
  | INTERFACE; i = interface; BACKSLASH;
    { Interface ($loc, i) }

(** Interface rules *)

ty:
  | l = variant(enum_int); r = row;;
    { Enum_int (l, r) }
  | l = variant(bool);
    { Enum_bool l }
  | l = variant(enum_string); r = row;
    { Enum_string (l, r) }
  | l = variant(ty_record); r = row;
    { Record (l, r) }
  | x = ID;
    { Named ($loc, x) }
  | QUESTION; t = ty;
    { Nullable t }
  | LEFT_BRACK; t = ty; RIGHT_BRACK;
    { List t }
  | LEFT_ANGLE; t = ty; RIGHT_ANGLE;
    { Dict t }
  | LEFT_PAREN; l = separated_list(COMMA, ty); RIGHT_PAREN;
    { Interface.Tuple l }

ty_record_field:
  | AT; k = record_key; COLON; v = record_tag;  { `Tag (k, v) }
  | k = record_key; COLON; v = ty;              { `Notag (k, v) }

ty_record_fields:
  | x = ty_record_field;                              { Record.singleton x }
  | m = ty_record_fields; COMMA; x = ty_record_field; { Record.add $loc x m }

ty_record: LEFT_BRACE; x = ty_record_fields; RIGHT_BRACE; { ($loc, x) }

enum_int: AT; i = INT; { i }

enum_string: AT; s = STRING; { s }

bool:
  | FALSE; { 0 }
  | TRUE;  { 1 }

row:
  | (* empty *)     { `Closed }
  | PIPE; ELLIPSIS; { `Open }

%inline variant(X): l = variant_rev(X); { Nonempty.rev l }
variant_rev(X):
  | x = X;                            { [ x ] }
  | l = variant_rev(X); PIPE; x = X;  { Nonempty.cons x l }

interface:
  | (* empty *)
    { [] }
  | k = ID; EQUALS; v = ty; l = interface;
    { { loc = $loc; name = k; ty = v} :: l }

(** Putting it all together *)

nodes:
  | t = text;                             { [ t ] }
  | t1 = text; l = nodes_rev; t2 = text;  { t1 :: List.rev (t2 :: l) }
nodes_rev:
  | (* empty *)               { [] }
  | l = nodes_rev; n = node;  { n :: l }

acutis: n = nodes; EOF; { n }

interface_standalone: l = interface; EOF; { l }
