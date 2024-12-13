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

type mode = Text | Expr | Queue of token * mode
type state = { mutable mode : mode; lexbuf : L.lexbuf }

let make_state lexbuf = { mode = Text; lexbuf; }
let make_state_interface lexbuf = { mode = Expr; lexbuf; }
let loc { lexbuf; _ } = (lexbuf.lex_start_p, lexbuf.lex_curr_p)
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let comp = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let digit = ['0'-'9']
let int = '-'? digit+
let frac = '.' digit+
let exp = ['e' 'E'] ['-' '+']? digit+
let float = '-'? digit+ frac? exp?

(* We reverse tokens for {%~ and ~%} because it's easier to parse tokens like
   TEXT TILDE UNESCAPE_BEGIN instead of TEXT UNESCAPE_BEGIN TILDE. *)

(* Because comments can nest, we include the delimiters in their text. *)

rule text state buf = parse
  | "{%"          { state.mode <- Expr; TEXT (B.contents buf) }
  | "{%~"         { state.mode <- Queue (TILDE, Expr); TEXT (B.contents buf) }
  | "{{%"         { state.mode <- Queue (UNESCAPE_BEGIN, Expr);
                    TEXT (B.contents buf) }
  | "{{%~"        { state.mode <- Queue (TILDE, Queue (UNESCAPE_BEGIN, Expr));
                    TEXT (B.contents buf) }
  | "{*" as s     { let cbuf = B.create 256 in
                    B.add_string cbuf s;
                    comment lexbuf.lex_start_p cbuf lexbuf;
                    state.mode <- Queue (COMMENT (B.contents cbuf), Text);
                    TEXT (B.contents buf) }
  | newline as s  { L.new_line lexbuf;
                    B.add_string buf s;
                    text state buf lexbuf }
  | eof           { state.mode <- Queue (EOF, Text); TEXT (B.contents buf) }
  | _ as c        { B.add_char buf c; text state buf lexbuf }

and comment pos buf = parse
  | "{*" as s     { B.add_string buf s;
                    comment lexbuf.lex_start_p buf lexbuf;
                    comment pos buf lexbuf }
  | newline as s  { L.new_line lexbuf;
                    B.add_string buf s;
                    comment pos buf lexbuf }
  | "*}" as s     { B.add_string buf s }
  | eof           { lexbuf.lex_start_p <- pos;
                    Error.lex_unterminated_comment lexbuf }
  | _ as c        { B.add_char buf c; comment pos buf lexbuf }

and expr state = parse
  | "%}"          { state.mode <- Text; text state (B.create 256) lexbuf }
  | "~%}"         { state.mode <- Text; TILDE }
  | "%}}"         { state.mode <- Text; UNESCAPE_END }
  | "~%}}"        { state.mode <- Queue (TILDE, Text); UNESCAPE_END }
  | "match"       { MATCH }
  | "with"        { WITH }
  | "map"         { MAP }
  | "map_dict"    { MAP_DICT }
  | "null"        { NULL }
  | "true"        { TRUE }
  | "false"       { FALSE }
  | "interface"   { INTERFACE }
  | int as s      { try INT (int_of_string s)
                    with Failure _ -> Error.lex_bad_int lexbuf s }
  | float as s    { FLOAT (float_of_string s) }
  | id as s       { ID s }
  | comp as s     { COMPONENT s }
  | white         { expr state lexbuf }
  | newline       { L.new_line lexbuf; expr state lexbuf }
  | '"'           { string lexbuf.lex_start_p (B.create 16) lexbuf }
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
  | '|'           { PIPE }
  | '?'           { QUESTION }
  | '.'           { DOT }
  | "..."         { ELLIPSIS }
  | "%b"          { FMT_B }
  | "%f"          { FMT_F }
  | "%i"          { FMT_I }
  | eof           { EOF }
  | _ as c        { Error.lex_unexpected lexbuf c }

and string pos buf = parse
  | '"'           { lexbuf.lex_start_p <- pos; STRING (B.contents buf) }
  | '\\' '"'      { B.add_char buf '"'; string pos buf lexbuf }
  | '\\' '/'      { B.add_char buf '/'; string pos buf lexbuf }
  | '\\' '\\'     { B.add_char buf '\\'; string pos buf lexbuf }
  | '\\' 'b'      { B.add_char buf '\b'; string pos buf lexbuf }
  | '\\' 'f'      { B.add_char buf '\012'; string pos buf lexbuf }
  | '\\' 'n'      { B.add_char buf '\n'; string pos buf lexbuf }
  | '\\' 'r'      { B.add_char buf '\r'; string pos buf lexbuf }
  | '\\' 't'      { B.add_char buf '\t'; string pos buf lexbuf }
  | '\\' (_ as c) { Error.lex_unexpected lexbuf c }
  | newline as s  { L.new_line lexbuf;
                    B.add_string buf s;
                    string pos buf lexbuf }
  | eof           { lexbuf.lex_start_p <- pos;
                    Error.lex_unterminated_string lexbuf }
  | _ as c        { B.add_char buf c; string pos buf lexbuf }

{
let supplier state () =
  let token = 
    match state.mode with
    | Text -> text state (B.create 256) state.lexbuf
    | Expr -> expr state state.lexbuf
    | Queue (token, state') -> state.mode <- state'; token
  in
  (token, state.lexbuf.lex_start_p, state.lexbuf.lex_curr_p)
}
