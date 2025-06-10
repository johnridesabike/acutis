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
%token <string> COMMENT       (* {* *} *)

(* Expression syntax *)
%token UNESCAPE_BEGIN         (* {{% *)
%token UNESCAPE_END           (* %}} *)
%token TILDE                  (* ~ *)
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
%token FMT_I                  (* %i *)
%token FMT_F                  (* %f *)
%token FMT_B                  (* %b *)

%start <Ast.t> acutis
%start <Ast.interface> interface_standalone

%%

(** Helper rules *)

(* We must reverse and inline this to avoid shift/reduce conflicts. *)
%inline nonempty_sep(SEP, X): l = nonempty_sep_rev(SEP, X); { Nonempty.rev l }
nonempty_sep_rev(SEP, X):
  | p = X;                                    { [ p ] }
  | l = nonempty_sep_rev(SEP, X); SEP; p = X; { Nonempty.cons p l }

nonempty_trailing(SEP, X): l = nonempty_sep(SEP, X); SEP?;  { l }

list_sep_trailing(SEP, X):
  | (* empty *)                     { [] }
  | l = nonempty_trailing(SEP, X);  { Nonempty.to_list l }

nonempty(X):
  | p = X;                  { [ p ] }
  | p = X; l = nonempty(X); { Nonempty.cons p l }

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
  | s = STRING; { Tag_string ($loc, s) }

record_key: k = ID; | k = STRING; { k }

record_field:
  | AT; k = record_key; COLON; v = record_tag;
    { ($loc, k, Tag v) }
  | k = record_key; COLON; v = pattern;
    { ($loc, k, Value v) }
  | k = ID;
    { ($loc, k, Value (Var ($loc, k) )) }

record: l = nonempty_trailing(COMMA, record_field); { l }

dict_field:
  | k = record_key; COLON; v = pattern; { ($loc, k, v) }
  | k = ID;                             { ($loc, k, Var ($loc, k) ) }

dict: l = list_sep_trailing(COMMA, dict_field); { l }

list_tail:
  | (* empty *)             { None }
  | ELLIPSIS; p = pattern;  { Some p }

list_comma_tail:
  | (* empty *)             { None }
  | COMMA; tl = list_tail;  { tl }

pattern_list:
  | tl = list_tail;
    { List ($loc, [], tl) }
  | l = nonempty_sep(COMMA, pattern); tl = list_comma_tail;
    { List ($loc, Nonempty.to_list l, tl) }

tuple: l = list_sep_trailing(COMMA, pattern); { l }

(** Match & map rules *)

with_pats_nested: WITH; ps = nonempty_sep(COMMA, pattern);  { ($loc, ps) }

with_pats: l = nonempty(with_pats_nested);  { l }

case: pats = with_pats; child = nodes; { { pats; nodes = child } }

cases: l = nonempty(case);  { l }

(** Component rules *)

prop:
  | k = ID; EQUALS; v = pattern;  { ($loc, k, v) }
  | k = ID;                       { ($loc, k, (Var ($loc, k))) }

props:
  | (* empty *)           { [] }
  | x = prop; p = props;  { x :: p }

children: n = nodes;  { ($loc, "children", Block ($loc, n)) }

(** Echo rules *)

echo_format:
  | (* empty *) { Fmt_string }
  | FMT_I;      { Fmt_int }
  | FMT_F;      { Fmt_float }
  | FMT_B;      { Fmt_bool }

echo_value:
  | s = ID;                               { Echo_var ($loc, s) }
  | s = STRING;                           { Echo_string ($loc, s) }
  | e = echo_value; DOT; s = record_key;  { Echo_field (e, s) }

echo: fmt = echo_format; e = echo_value;  { (fmt, e) }

(* Do this reversed to efficiently take the last item. *)
echoes: l = nonempty_sep_rev(QUESTION, echo); { l }

(** Putting it all together *)

expr:
  | txt = COMMENT;
    { Comment txt }
  | e = echoes;
    { let Nonempty.((fmt, default) :: l) = e in
      Echo (List.rev l, fmt, default, Escape) }
  | UNESCAPE_BEGIN; e = echoes; UNESCAPE_END;
    { let Nonempty.((fmt, default) :: l) = e in
      Echo (List.rev l, fmt, default, No_escape) }
  | MATCH; pats = nonempty_sep(COMMA, pattern); child = cases; BACKSLASH; MATCH;
    { Match ($loc, pats, child) }
  | MAP; pat = pattern; child = cases; BACKSLASH; MAP;
    { Map_list ($loc, pat, child) }
  | MAP_DICT; pat = pattern; child = cases; BACKSLASH; MAP_DICT;
    { Map_dict ($loc, pat, child) }
  | x = COMPONENT; p = props; BACKSLASH;
    { Component ($loc, x, x, p) }
  | x1 = COMPONENT; p = props; c = children; BACKSLASH; x2 = COMPONENT;
    { Component ($loc, x1, x2, c :: p) }
  | INTERFACE; i = interface;
    { Interface ($loc, i) }

(** Interface rules *)

ty:
  | l = nonempty_sep(PIPE, enum_int); r = row;    { Ty_enum_int (l, r) }
  | l = nonempty_sep(PIPE, bool);                 { Ty_enum_bool l }
  | l = nonempty_sep(PIPE, enum_string); r = row; { Ty_enum_string (l, r) }
  | l = nonempty_sep(PIPE, ty_record); r = row;   { Ty_record (l, r) }
  | x = ID;                                       { Ty_named ($loc, x) }
  | QUESTION; t = ty;                             { Ty_nullable t }
  | LEFT_BRACK; t = ty; RIGHT_BRACK;              { Ty_list t }
  | LEFT_ANGLE; t = ty; RIGHT_ANGLE;              { Ty_dict t }
  | LEFT_PAREN; l = ty_tuple; RIGHT_PAREN;        { Ty_tuple l }

ty_record_field:
  | AT; k = record_key; COLON; v = record_tag;  { ($loc, k, Tag v) }
  | k = record_key; COLON; v = ty;              { ($loc, k, Value v) }

ty_record:
  | LEFT_BRACE; x = nonempty_trailing(COMMA, ty_record_field); RIGHT_BRACE;
    { ($loc, x) }

ty_tuple: l = list_sep_trailing(COMMA, ty); { l }

enum_int: AT; i = INT;  { ($loc, i) }

enum_string: AT; s = STRING;  { ($loc, s) }

bool:
  | FALSE;  { ($loc, 0) }
  | TRUE;   { ($loc, 1) }

row:
  | (* empty *)     { ($loc, `Closed) }
  | PIPE; ELLIPSIS; { ($loc, `Open) }

interface_prop: k = ID; EQUALS; v = ty; { { loc = $loc; name = k; ty = v } }
interface:
  | (* empty *)                         { [] }
  | x = interface_prop; l = interface;  { x :: l }

(** Putting it all together *)

trim:
  | (* empty *) { No_trim }
  | TILDE       { Trim }

text: l = trim; txt = TEXT; r = trim; { Text (txt, l, r) }

nodes: l = nodes_rev; { List.rev l }
nodes_rev:
  | t = text                            { [ t ] }
  | l = nodes_rev; e = expr; t = text;  { t :: e :: l }

acutis: n = nodes; EOF; { n }

interface_standalone: l = interface; EOF; { l }
