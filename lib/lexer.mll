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

type mode = Text | Expr | Echo | Queue of token list * mode
type state = mode ref

let make_state () = ref Text
}

let text = [^ '{' '}' '\r' '\n']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let component = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let digit = ['0'-'9']
let int = '-'? digit digit*
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit+ frac? exp?

rule text state buf = parse
  | "{{"
    { state := Queue ([ECHO_BEGIN], Echo); TEXT (B.contents buf) }
  | "{{~"
    { state := Queue ([TILDE_RIGHT; ECHO_BEGIN], Echo); TEXT (B.contents buf) }
  | "{%"
    { state := Queue ([], Expr); TEXT (B.contents buf) }
  | "{%~"
    { state := Queue ([TILDE_RIGHT], Expr); TEXT (B.contents buf) }
  | "{*"
    { comment 0 lexbuf; text state buf lexbuf; }
  | [^ '{' '}' '\r' '\n']*
    { B.add_string buf (L.lexeme lexbuf); text state buf lexbuf }
  | ['{' '}']
    { B.add_string buf (L.lexeme lexbuf); text state buf lexbuf }
  | newline
    {
      L.new_line lexbuf;
      B.add_string buf (L.lexeme lexbuf);
      text state buf lexbuf
    }
  | eof
    { state := Queue ([EOF], Text); TEXT (B.contents buf) }

and comment depth = parse
  | "{*"          { comment (succ depth) lexbuf }
  | "*}"          { if depth = 0 then () else comment (pred depth) lexbuf }
  | newline       { L.new_line lexbuf; comment depth lexbuf }
  | eof           { raise Error }
  | _             { comment depth lexbuf }

and expr state = parse
  | "%}"          { state := Text; text state (B.create 256) lexbuf }
  | "~%}"         { state := Text; TILDE_LEFT }
  | "match"       { MATCH }
  | "with"        { WITH }
  | "map"         { MAP }
  | "map_dict"    { MAP_DICT }
  | "null"        { NULL }
  | "true"        { TRUE }
  | "false"       { FALSE }
  | int           { INT (int_of_string (L.lexeme lexbuf)) }
  | float         { FLOAT (float_of_string (L.lexeme lexbuf)) }
  | id            { ID (L.lexeme lexbuf) }
  | component     { COMPONENT (L.lexeme lexbuf) }
  | white         { expr state lexbuf }
  | newline       { L.new_line lexbuf; expr state lexbuf }
  | '"'           { read_string (B.create 16) lexbuf }
  | '['           { LEFT_BRACK }
  | ']'           { RIGHT_BRACK }
  | '{'           { LEFT_BRACE }
  | '}'           { RIGHT_BRACE }
  | '('           { LEFT_PAREN }
  | ')'           { RIGHT_PAREN }
  | '<'           { LEFT_ANGLE }
  | '>'           { RIGHT_ANGLE }
  | ':'           { COLON }
  | '@'           { AT }
  | '/'           { BACKSLASH }
  | ','           { COMMA }
  | '='           { EQUALS }
  | '#'           { HASH }
  | '!'           { EXCLAMATION }
  | "..."         { ELLIPSIS }
  | eof           { raise Error }
  | _             { raise Error }

(* Echo expressions are specialized to avoid confusion with }} in patterns. *)
and echo state = parse
  | "}}"          { state := Text; ECHO_END }
  | "~}}"         { state := Queue ([TILDE_LEFT], Text); ECHO_END }
  | "match"
  | "with"
  | "map"
  | "map_dict"
  | "null"
  | "true"
  | "false"       { raise Error }
  | id            { ID (L.lexeme lexbuf) }
  | component     { COMPONENT (L.lexeme lexbuf) }
  | white         { echo state lexbuf }
  | newline       { L.new_line lexbuf; echo state lexbuf }
  | '"'           { read_string (B.create 16) lexbuf }
  | '?'           { QUESTION }
  | '&'           { AMPERSAND }
  | eof           { raise Error }
  | _             { raise Error }

and read_string buf =
  parse
  | '"'           { STRING (B.contents buf) }
  | '\\' '/'      { B.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'     { B.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'      { B.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'      { B.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'      { B.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'      { B.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'      { B.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+ { B.add_string buf (L.lexeme lexbuf); read_string buf lexbuf }
  | eof           { raise Error }
  | _             { raise Error }

{
let rec acutis state lexbuf =
  match !state with
  | Text -> text state (B.create 256) lexbuf
  | Expr -> expr state lexbuf
  | Echo -> echo state lexbuf
  | Queue ([], state') ->
      state := state';
      acutis state lexbuf
  | Queue (tok :: tl, state') ->
      state := Queue (tl, state');
      tok
}
