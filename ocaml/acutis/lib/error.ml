(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open StdlibExtra
module F = Format

let text = F.pp_print_text

exception Error of string

let column pos = pos.Lexing.pos_cnum - pos.pos_bol + 1

let print_pos ppf pos =
  Format.fprintf ppf "%d:%d" pos.Lexing.pos_lnum (column pos)

let pp ~kind (start, pos) mess =
  F.asprintf "@[<v>@[File@ %s,@ %a-%a@]@,@[%s.@]@,@[%t@]@]" pos.Lexing.pos_fname
    print_pos start print_pos pos kind mess

let pp_lexbuf ~kind mess lexbuf =
  let start = lexbuf.Lexing.lex_start_p in
  let pos = lexbuf.lex_curr_p in
  pp ~kind (start, pos) mess

(* Lexing and Parsing errors. *)

let lex_error =
  let f = F.dprintf "" in
  fun lexbuf -> raise @@ Error (pp_lexbuf ~kind:"Syntax error" f lexbuf)

let parse_error i lexbuf =
  let f = F.dprintf "%i" i in
  raise @@ Error (pp_lexbuf ~kind:"Parse error" f lexbuf)

let dup_record_key loc key =
  let f = F.dprintf "Duplicate@ field %S." key in
  raise @@ Error (pp ~kind:"Parse error" loc f)

let extra_record_tag =
  let f ppf = text ppf "This tagged record has multiple tags." in
  fun loc -> raise @@ Error (pp ~kind:"Parse error" loc f)
(* Type errors *)

let pp_ty = pp ~kind:"Type error"

let mismatch a b t =
  F.dprintf
    "@[<v>@[Type mismatch.@]@ @[Expected:@ @[<hov 2>%a@]@]@ @[Received:@ \
     @[<hov 2>%a@]@]%t@]"
    Typescheme.pp_ty a Typescheme.pp_ty b t

let type_mismatch loc a b =
  raise @@ Error (pp_ty loc (mismatch a b (F.dprintf "")))

let bad_union_tag loc ty =
  let f =
    F.dprintf "@[<v>@[%a@]@ @[Received:@ @[<hov 2>%a@]@]@]" text
      "Only `int`, `string`, and `boolean` types may be union tags."
      Typescheme.pp_ty ty
  in
  raise @@ Error (pp_ty loc f)

let missing_field loc key ty =
  let f =
    F.dprintf "This is missing key@ `%a`@ of type@ @[%a@]" Pp.field key
      Typescheme.pp_ty ty
  in
  raise @@ Error (pp_ty loc f)

let underscore_in_construct =
  let f ppf = text ppf "Underscore (`_`) is not a valid name." in
  fun loc -> raise @@ Error (pp_ty loc f)

let child_type_mismatch loc a b =
  let f =
    F.dprintf
      "@[<v>@[Child type mismatch.@]@ @[Expected:@ @[<hov 2>`%a`@]@]@ \
       @[Received:@ @[<hov 2>`%a`@]@]@]"
      Typescheme.Child.pp_ty a Typescheme.Child.pp_ty b
  in
  raise @@ Error (pp_ty loc f)

let name_bound_too_many loc s =
  let f =
    F.dprintf "%a`%s`%a" text "The name " s text
      " is already bound in this pattern."
  in
  raise @@ Error (pp_ty loc f)

let var_missing loc v =
  let f =
    F.dprintf "%a`%s`%a" text "Variable " v text
      " must occur in each `with` pattern."
  in
  raise @@ Error (pp_ty loc f)

let pat_num_mismatch =
  let f ppf = text ppf "Pattern count mismatch." in
  fun loc -> raise @@ Error (pp_ty loc f)

let map_pat_num_mismatch =
  let f ppf =
    text ppf
      "Expressions `map` and `map_dict` can only have one or two patterns for \
       each `with` expression."
  in
  fun loc -> raise @@ Error (pp_ty loc f)

let echo_nullable_literal =
  let f ppf = text ppf "Echoed string literals cannot appear before a ?." in
  fun loc -> raise @@ Error (pp_ty loc f)

let extra_child loc ~comp ~child =
  let f = F.dprintf "Component %s does not allow child %s." comp child in
  raise @@ Error (pp_ty loc f)

let missing_child loc s =
  let f = F.dprintf "Missing child: %s." s in
  raise @@ Error (pp_ty loc f)

let child_in_root loc =
  let f ppf = text ppf "Children are not allowed in the root template." in
  raise @@ Error (pp_ty loc f)

let component_name_mismatch loc a b =
  let f =
    F.dprintf
      "@[<v>@[Component name mismatch.@]@ @[Expected: %s.@]@ @[Received: \
       %s.@]@]"
      a b
  in
  raise @@ Error (pp_ty loc f)

(* Matching errors *)
let pp_match = pp ~kind:"Matching error"

let unused_case loc =
  let f = F.dprintf "This match case is unused." in
  raise @@ Error (pp_match loc f)

let parmatch loc pp_pat pat =
  let f =
    F.dprintf "@[<v>@[%a@]@,@[%a@]@]" text
      "This pattern-matching is not exhaustive. Here's an example of a pattern \
       which is not matched:"
      pp_pat pat
  in
  raise @@ Error (pp_match loc f)

(* Dag errors *)
let duplicate_name s =
  let s =
    F.asprintf "Compile error.@ There are multiple components with name `%s`." s
  in
  raise @@ Error s

let pp_sep ppf () = F.fprintf ppf " ->@ "

let cycle stack =
  let s =
    F.asprintf "@[<v>@[Dependency cycle detected.@]@,@[%a@]"
      (F.pp_print_list ~pp_sep F.pp_print_string)
      (List.rev stack)
  in
  raise @@ Error s

let missing_component stack name =
  let s =
    F.asprintf "@[<v>@[Missing template:@ %s.@]@,@[Required by:@ %s.@]" name
      (List.hd stack)
  in
  raise @@ Error s

(* Decode errors *)
module DecodeStack = struct
  type t = Nullable | Index of int | Key of string

  open Format

  let pp ppf = function
    | Nullable -> fprintf ppf "nullable"
    | Index i -> fprintf ppf "@[index: %i@]" i
    | Key s -> fprintf ppf "@[key: %S@]" s

  let pp_sep ppf () = fprintf ppf " ->@ "

  let pp ppf t =
    fprintf ppf "@[[@,%a]@]" (pp_print_list ~pp_sep pp) (List.rev t)
end

let pp stack ty mess =
  F.asprintf
    "@[<v>@[Decode error.@]@,\
     @[Stack:@ @[%a@]@]@,\
     @[Expected type:@ @[%a@]@]@,\
     @[%t@]"
    DecodeStack.pp stack Typescheme.pp_ty ty mess

let decode pp_data ty stack data =
  let f = F.dprintf "Received value:@ @[%a@]" pp_data data in
  raise @@ Error (pp stack ty f)

let missing_key stack ty key =
  let f = F.dprintf "%a%S" text "Input is missing key: " key in
  raise @@ Error (pp stack ty f)

let bad_enum pp_data ty stack data =
  let f =
    F.dprintf "%a@[%a@]" text "This type does not allow the given value: "
      pp_data data
  in
  raise @@ Error (pp stack ty f)
