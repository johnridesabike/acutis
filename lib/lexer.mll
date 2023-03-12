(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

{
module B = Buffer
module L = Lexing
open Parser
exception Error

type mode = Text | Expr | Echo | Echo_format | Queue of token list * mode
type state = mode ref

let make_state () = ref Text

(* If these don't parse correctly (e.g. if a number exceeds the max_int) then
  just return a syntax error. *)
let int_of_string s = try int_of_string s with Failure _ -> raise Error
let float_of_string s = try float_of_string s with Failure _ -> raise Error
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let component = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let digit = ['0'-'9']
let int = '-'? digit+
let frac = '.' digit+
let exp = ['e' 'E'] ['-' '+']? digit+
let float = '-'? digit+ frac? exp?

rule text state buf = parse
  | "{{"
    { state := Queue ([ECHO_BEGIN], Echo); TEXT (B.contents buf) }
  | "{{~"
    { state := Queue ([TILDE_RIGHT; ECHO_BEGIN], Echo); TEXT (B.contents buf) }
  | "{{{"
    { state := Queue ([TRIPLE_ECHO_BEGIN], Echo); TEXT (B.contents buf) }
  | "{{{~"
    {
      state := Queue ([TILDE_RIGHT; TRIPLE_ECHO_BEGIN], Echo);
      TEXT (B.contents buf)
    }
  | "{%"
    { state := Queue ([], Expr); TEXT (B.contents buf) }
  | "{%~"
    { state := Queue ([TILDE_RIGHT], Expr); TEXT (B.contents buf) }
  | "{*"
    { comment 0 lexbuf; text state buf lexbuf; }
  | [^ '{' '}' '\r' '\n']* as s
    { B.add_string buf s; text state buf lexbuf }
  | ['{' '}'] as c
    { B.add_char buf c; text state buf lexbuf }
  | newline as s
    { L.new_line lexbuf; B.add_string buf s; text state buf lexbuf }
  | eof
    { state := Queue ([EOF], Text); TEXT (B.contents buf) }

and comment depth = parse
  | "{*"          { comment (succ depth) lexbuf }
  | "*}"          { if depth = 0 then () else comment (pred depth) lexbuf }
  | newline       { L.new_line lexbuf; comment depth lexbuf }
  | eof           { raise Error }
  | _             { comment depth lexbuf }

and expr state = parse
  | "%}"            { state := Text; text state (B.create 256) lexbuf }
  | "~%}"           { state := Text; TILDE_LEFT }
  | "match"         { MATCH }
  | "with"          { WITH }
  | "map"           { MAP }
  | "map_dict"      { MAP_DICT }
  | "null"          { NULL }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "interface"     { INTERFACE }
  | int as i        { INT (int_of_string i) }
  | float as f      { FLOAT (float_of_string f) }
  | id as s         { ID s }
  | component as s  { COMPONENT s }
  | white           { expr state lexbuf }
  | newline         { L.new_line lexbuf; expr state lexbuf }
  | '"'             { string lexbuf.lex_start_p (B.create 16) lexbuf }
  | '['             { LEFT_BRACK }
  | ']'             { RIGHT_BRACK }
  | '{'             { LEFT_BRACE }
  | '}'             { RIGHT_BRACE }
  | '('             { LEFT_PAREN }
  | ')'             { RIGHT_PAREN }
  | '<'             { LEFT_ANGLE }
  | '>'             { RIGHT_ANGLE }
  | ':'             { COLON }
  | '@'             { AT }
  | '/'             { BACKSLASH }
  | ','             { COMMA }
  | '='             { EQUALS }
  | '#'             { HASH }
  | '!'             { EXCLAMATION }
  | '|'             { PIPE }
  | '?'             { QUESTION }
  | '.'             { DOT }
  | "..."           { ELLIPSIS }
  | eof             { raise Error }
  | _               { raise Error }

(* Echo expressions are specialized to avoid confusion with }} in patterns. *)
and echo state = parse
  | "}}"            { state := Text; ECHO_END }
  | "~}}"           { state := Queue ([TILDE_LEFT], Text); ECHO_END }
  | "}}}"           { state := Text; TRIPLE_ECHO_END }
  | "~}}}"          { state := Queue ([TILDE_LEFT], Text); TRIPLE_ECHO_END }
  | "match"
  | "with"
  | "map"
  | "map_dict"
  | "null"
  | "true"
  | "false"
  | "interface"     { raise Error }
  | id as s         { ID s }
  | component as s  { COMPONENT s }
  | white           { echo state lexbuf }
  | newline         { L.new_line lexbuf; echo state lexbuf }
  | '"'             { string lexbuf.lex_start_p (B.create 16) lexbuf }
  | '?'             { QUESTION }
  | '.'             { DOT }
  | '%'             { state := Echo_format; PERCENT }
  | eof             { raise Error }
  | _               { raise Error }

and echo_format state = parse
  | 'i'         { CHAR_I }
  | 'f'         { CHAR_F }
  | 'e'         { CHAR_E }
  | 'g'         { CHAR_G }
  | 'b'         { CHAR_B }
  | ','         { COMMA }
  | '.'         { DOT }
  | digit+ as i { INT (int_of_string i) }
  | newline     { L.new_line lexbuf; state := Echo; echo state lexbuf }
  | white       { state := Echo; echo state lexbuf }
  | _           { raise Error }

and string pos buf =
  parse
  | '"'                 { lexbuf.lex_start_p <- pos; STRING (B.contents buf) }
  | '\\' '"'            { B.add_char buf '"'; string pos buf lexbuf }
  | '\\' '/'            { B.add_char buf '/'; string pos buf lexbuf }
  | '\\' '\\'           { B.add_char buf '\\'; string pos buf lexbuf }
  | '\\' 'b'            { B.add_char buf '\b'; string pos buf lexbuf }
  | '\\' 'f'            { B.add_char buf '\012'; string pos buf lexbuf }
  | '\\' 'n'            { B.add_char buf '\n'; string pos buf lexbuf }
  | '\\' 'r'            { B.add_char buf '\r'; string pos buf lexbuf }
  | '\\' 't'            { B.add_char buf '\t'; string pos buf lexbuf }
  | [^ '"' '\\']+ as s  { B.add_string buf s; string pos buf lexbuf }
  | eof                 { raise Error }
  | _                   { raise Error }

{
let rec acutis state lexbuf =
  match !state with
  | Text -> text state (B.create 256) lexbuf
  | Expr -> expr state lexbuf
  | Echo -> echo state lexbuf
  | Echo_format -> echo_format state lexbuf
  | Queue ([], state') ->
      state := state';
      acutis state lexbuf
  | Queue (tok :: tl, state') ->
      state := Queue (tl, state');
      tok
}
