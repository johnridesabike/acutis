open Acutis
open Lexing
module F = Format

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  F.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  let state = Lexer.make_state () in
  try Some (Parser.acutis (Lexer.acutis state) lexbuf) with
  | Lexer.SyntaxError ->
      F.eprintf "Lexer error. %a\n" print_position lexbuf;
      None
  | Parser.Error i ->
      F.eprintf "Parser error %i. %a\n" i print_position lexbuf;
      exit (-1)

let parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value -> F.printf "%a\n" Ast.pp value
  | None -> ()

let loop () =
  let src = {|{{ a }} {{ "b" }} {{ C }} {{ d ? E ? "f" }}|} in
  let lexbuf = Lexing.from_string src in
  parse_and_print lexbuf

let () = loop ()
