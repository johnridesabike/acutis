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

type _ Effect.t += Yield : token -> unit Effect.t

let yield x = Effect.perform (Yield x)
let tbuf_create () = B.create 256
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

rule text buf = parse
  | "{%"          { yield (TEXT (B.contents buf)); expr lexbuf }
  | "{%~"         { yield (TEXT (B.contents buf)); yield TILDE; expr lexbuf }
  | "{{%"         { yield (TEXT (B.contents buf));
                    yield UNESCAPE_BEGIN;
                    expr lexbuf }
  | "{{%~"        { yield (TEXT (B.contents buf));
                    yield TILDE;
                    yield UNESCAPE_BEGIN;
                    expr lexbuf }
  | "{*" as s     { yield (TEXT (B.contents buf));
                    let cbuf = B.create 256 in
                    B.add_string cbuf s;
                    comment lexbuf.lex_start_p cbuf lexbuf;
                    yield (COMMENT (B.contents cbuf));
                    text (tbuf_create ()) lexbuf }
  | newline as s  { L.new_line lexbuf; B.add_string buf s; text buf lexbuf }
  | eof           { yield (TEXT (B.contents buf)); EOF }
  | _ as c        { B.add_char buf c; text buf lexbuf }

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

and expr = parse
  | "%}"          { text (tbuf_create ()) lexbuf }
  | "~%}"         { yield TILDE; text (tbuf_create ()) lexbuf }
  | "%}}"         { yield UNESCAPE_END; text (tbuf_create ()) lexbuf }
  | "~%}}"        { yield UNESCAPE_END;
                    yield TILDE;
                    text (tbuf_create ()) lexbuf }
  | "match"       { yield MATCH; expr lexbuf }
  | "with"        { yield WITH; expr lexbuf }
  | "map"         { yield MAP; expr lexbuf }
  | "map_dict"    { yield MAP_DICT; expr lexbuf }
  | "null"        { yield NULL; expr lexbuf }
  | "true"        { yield TRUE; expr lexbuf }
  | "false"       { yield FALSE; expr lexbuf }
  | "interface"   { yield INTERFACE; expr lexbuf }
  | int as s      { match int_of_string s with
                    | i -> yield (INT i); expr lexbuf
                    | exception Failure _ -> Error.lex_bad_int lexbuf s }
  | float as s    { yield (FLOAT (float_of_string s)); expr lexbuf }
  | id as s       { yield (ID s); expr lexbuf }
  | comp as s     { yield (COMPONENT s); expr lexbuf }
  | white         { expr lexbuf }
  | newline       { L.new_line lexbuf; expr lexbuf }
  | '"'           { string lexbuf.lex_start_p (B.create 16) lexbuf;
                    expr lexbuf }
  | '['           { yield LEFT_BRACK; expr lexbuf }
  | ']'           { yield RIGHT_BRACK; expr lexbuf }
  | '{'           { yield LEFT_BRACE; expr lexbuf }
  | '}'           { yield RIGHT_BRACE; expr lexbuf }
  | '('           { yield LEFT_PAREN; expr lexbuf }
  | ')'           { yield RIGHT_PAREN; expr lexbuf }
  | '<'           { yield LEFT_ANGLE; expr lexbuf }
  | '>'           { yield RIGHT_ANGLE; expr lexbuf }
  | ':'           { yield COLON; expr lexbuf }
  | '@'           { yield AT; expr lexbuf }
  | '/'           { yield BACKSLASH; expr lexbuf }
  | ','           { yield COMMA; expr lexbuf }
  | '='           { yield EQUALS; expr lexbuf }
  | '#'           { yield HASH; expr lexbuf }
  | '!'           { yield EXCLAMATION; expr lexbuf }
  | '|'           { yield PIPE; expr lexbuf }
  | '?'           { yield QUESTION; expr lexbuf }
  | '.'           { yield DOT; expr lexbuf }
  | "..."         { yield ELLIPSIS; expr lexbuf }
  | "%b"          { yield FMT_B; expr lexbuf }
  | "%f"          { yield FMT_F; expr lexbuf }
  | "%i"          { yield FMT_I; expr lexbuf }
  | eof           { EOF }
  | _ as c        { Error.lex_unexpected lexbuf c }

and string pos buf = parse
  | '"'           { lexbuf.lex_start_p <- pos; yield (STRING (B.contents buf)) }
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
let eof () = EOF

let generator rule lexbuf =
  let cont = ref (fun () -> rule lexbuf) in
  fun () ->
    let f = !cont in
    cont := eof (* Don't resume a continuation twice. *);
    let tok =
      match f () with
      | tok -> tok
      | effect Yield tok, k ->
          cont := Effect.Deep.continue k;
          tok
    in
    (tok, lexbuf.L.lex_start_p, lexbuf.L.lex_curr_p)

let acutis lexbuf = generator (text (tbuf_create ())) lexbuf
let interface lexbuf = generator expr lexbuf
}
