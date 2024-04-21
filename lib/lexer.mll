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

type mode = Text | Expr | Echo | Queue of token * mode

type state = mode ref

let make_state () = ref Text

let make_state_interface () = ref Expr
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

(* We reverse tokens for variants of {{~ because it's easier to parse tokens
   like TEXT TILDE ECHO_BEGIN instead of TEXT ECHO_BEGIN TILDE. *)

rule text state buf = parse
  | "{{"          { state := Queue (ECHO_BEGIN, Echo); TEXT (B.contents buf) }
  | "{{~"         { state := Queue (TILDE_RIGHT, Queue (ECHO_BEGIN, Echo));
                    TEXT (B.contents buf) }
  | "{{{"         { state := Queue (TRIPLE_ECHO_BEGIN, Echo);
                    TEXT (B.contents buf) }
  | "{{{~"        { state :=
                      Queue (TILDE_RIGHT, Queue (TRIPLE_ECHO_BEGIN, Echo));
                    TEXT (B.contents buf) }
  | "{%"          { state := Expr; TEXT (B.contents buf) }
  | "{%~"         { state := Queue (TILDE_RIGHT, Expr); TEXT (B.contents buf) }
  | "{*" as s     { let cbuf = B.create 256 in
                    B.add_string cbuf s;
                    comment lexbuf.lex_start_p cbuf lexbuf;
                    state := Queue (COMMENT (B.contents cbuf), Text);
                    TEXT (B.contents buf) }
  | newline as s  { L.new_line lexbuf;
                    B.add_string buf s;
                    text state buf lexbuf }
  | eof           { state := Queue (EOF, Text); TEXT (B.contents buf) }
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

(** The strings %} ~%} }} }}} ~}} and ~}}} need special treatment. They can
    either mark the end of an expression or they can be part of a pattern, such
    as }} which could end a nested record. We need to compare them to the
    state's current mode to determine which token(s) to return. *)

and expr state = parse
  | "%}"          { match !state with
                    | Expr -> state := Text; text state (B.create 256) lexbuf
                    | mode -> state := Queue (RIGHT_BRACE, mode); PERCENT }
  | "~%}"         { match !state with
                    | Expr -> state := Text; TILDE_LEFT
                    | mode ->
                        state := Queue (PERCENT, Queue (RIGHT_BRACE, mode));
                        TILDE_LEFT }
  | "}}"          { match !state with
                    | Echo -> state := Text; ECHO_END
                    | mode ->
                        state := Queue (RIGHT_BRACE, mode);
                        RIGHT_BRACE }
  | "~}}"         { match !state with
                    | Echo -> state := Queue (TILDE_LEFT, Text); ECHO_END
                    | mode ->
                        state := Queue (RIGHT_BRACE, Queue (RIGHT_BRACE, mode));
                        TILDE_LEFT }
  | "}}}"         { match !state with
                    | Echo -> state := Text; TRIPLE_ECHO_END
                    | mode ->
                        state := Queue (RIGHT_BRACE, Queue (RIGHT_BRACE, mode));
                        RIGHT_BRACE }
  | "~}}}"        { match !state with
                    | Echo -> state := Queue (TILDE_LEFT, Text); TRIPLE_ECHO_END
                    | mode ->
                        state :=
                          Queue (RIGHT_BRACE,
                            Queue (RIGHT_BRACE,
                              Queue (RIGHT_BRACE, mode)));
                        TILDE_LEFT }
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
  | '%' 'b'       { state := Queue(CHAR_B, !state); PERCENT }
  | '%' 'f'       { state := Queue(CHAR_F, !state); PERCENT }
  | '%' 'i'       { state := Queue(CHAR_I, !state); PERCENT }
  | '%'           { PERCENT }
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
let acutis state lexbuf =
  match !state with
  | Text -> text state (B.create 256) lexbuf
  | Expr | Echo -> expr state lexbuf
  | Queue (tok , state') -> state := state'; tok
}
