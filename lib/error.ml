(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module F = Format

type t = F.formatter -> unit

let pp = ( |> )

exception Fatal of t

type _ Effect.t += Warn : t -> unit Effect.t

module ES = Effect.Shallow

type 'a res = t Seq.t * 'a option

let rec continue : type a. t Seq.t -> (unit, a) ES.continuation -> a res =
 fun warnings k ->
  let retc x = (warnings, Some x) in
  let exnc = function
    | Fatal e -> (Seq.append warnings @@ Seq.return e, None)
    | e -> raise e
  in
  let effc : type b. b Effect.t -> ((b, a) ES.continuation -> a res) option =
    function
    | Warn w -> Some (continue (Seq.append warnings @@ Seq.return w))
    | _ -> None
  in
  ES.continue_with k () { retc; exnc; effc }

let handle f x = continue Seq.empty (ES.fiber (fun () -> f x))
let fatal x = raise @@ Fatal x
let warn x = Effect.perform @@ Warn x

let msg ~kind loc t =
  F.dprintf "@[<v>File \"%s\", %a@;%s.@;%t@]" (Loc.fname loc) Loc.pp loc kind t

let loc_of_lexbuf Lexing.{ lex_start_p; lex_curr_p; _ } =
  (lex_start_p, lex_curr_p)

let msg_syntax = msg ~kind:"Syntax error"

let lex_unexpected lexbuf c =
  let f = F.dprintf "Unexpected character: %C" c in
  fatal @@ msg_syntax (loc_of_lexbuf lexbuf) f

let lex_bad_int lexbuf s =
  let f = F.dprintf "Invalid integer: %s" s in
  fatal @@ msg_syntax (loc_of_lexbuf lexbuf) f

let lex_unterminated_comment lexbuf =
  let f = F.dprintf "@[%a@]" F.pp_print_text "Unterminated comment." in
  fatal @@ msg_syntax (loc_of_lexbuf lexbuf) f

let lex_unterminated_string lexbuf =
  let f = F.dprintf "@[%a@]" F.pp_print_text "Unterminated string." in
  fatal @@ msg_syntax (loc_of_lexbuf lexbuf) f

let parse_error i loc =
  let f =
    try
      let mess = Parser_messages.message i in
      F.dprintf "@[%a@]" F.pp_print_text (String.trim mess)
    with Not_found -> F.dprintf "Unexpected token."
  in
  fatal @@ msg ~kind:"Parse error" loc f

let msg_ty = msg ~kind:"Type error"
let msg_ty_warn = msg ~kind:"Type warning"

let dup_record_key loc key =
  let f = F.dprintf "Duplicate field '%a'." Pp.field key in
  fatal @@ msg_ty loc f

let extra_record_tag =
  let f = F.dprintf "This tagged record has multiple tags." in
  fun loc -> fatal @@ msg_ty loc f

let mismatch pp a b t =
  F.dprintf "Type mismatch.@;Expected:@;<1 2>%a@;Received:@;<1 2>%a" pp a pp b t

let type_mismatch loc pp a b = fatal @@ msg_ty loc (mismatch pp a b)

let bad_block loc =
  let f =
    F.dprintf "Template blocks are not allowed in a destructure pattern."
  in
  fatal @@ msg_ty loc f

let bad_field loc =
  let f =
    F.dprintf "Record '.' access is not allowed in a destructure pattern."
  in
  fatal @@ msg_ty loc f

let missing_field loc key pp ty =
  let f =
    F.dprintf "This is missing key '%a' of type:@;<1 2>%a" Pp.field key pp ty
  in
  fatal @@ msg_ty loc f

let underscore_in_construct =
  let f = F.dprintf "Underscore ('_') is not a valid name." in
  fun loc -> fatal @@ msg_ty loc f

let name_bound_too_many loc s =
  let f = F.dprintf "The name '%s' is already bound in this pattern." s in
  fatal @@ msg_ty loc f

let var_missing loc v =
  let f = F.dprintf "Variable '%s' must occur in each 'with' pattern." v in
  fatal @@ msg_ty loc f

let var_unused loc s =
  let f = F.dprintf "This variable is bound but never used:@;<1 2>%s" s in
  warn @@ msg_ty_warn loc f

let pat_num_mismatch =
  let f = F.dprintf "Pattern count mismatch." in
  fun loc -> fatal @@ msg_ty loc f

let map_pat_num_mismatch =
  let f =
    F.dprintf
      "@[Expressions 'map' and 'map_dict' can only have one or two@ patterns@ \
       for@ each@ 'with'@ expression.@]"
  in
  fun loc -> fatal @@ msg_ty loc f

let component_name_mismatch loc a b =
  let f =
    F.dprintf
      "Component name mismatch.@;Expected:@;<1 2>%s.@;Received:@;<1 2>%s." a b
  in
  fatal @@ msg_ty loc f

let component_extra_prop loc name s =
  let f =
    F.dprintf "Component '%s' does not accept this prop:@;<1 2>%a." name
      Pp.field s
  in
  warn @@ msg_ty_warn loc f

let interface_duplicate loc id =
  let f = F.dprintf "Prop '%s' is already defined in the interface." id in
  fatal @@ msg_ty loc f

let interface_bad_name loc id =
  let f = F.dprintf "There is no type named '%s'." id in
  fatal @@ msg_ty loc f

let interface_untagged_union loc =
  let f = F.dprintf "You cannot union records without a '@' tag field." in
  fatal @@ msg_ty loc f

let interface_unmatched_tags loc s1 s2 =
  let f =
    F.dprintf "This record has tag field '@%a' instead of '@%a'." Pp.field s2
      Pp.field s1
  in
  fatal @@ msg_ty loc f

let interface_duplicate_tag loc pp tag =
  let f = F.dprintf "Tag value '%a' is already used in this union." pp tag in
  fatal @@ msg_ty loc f

let interface_open_bool_union loc =
  let f = F.dprintf "Unions with boolean tags cannot be opened with '...'." in
  fatal @@ msg_ty loc f

let interface_type_mismatch loc k pp a b =
  let f =
    F.dprintf
      "This interface does not match the implementation.@;\
       Prop name:@;\
       <1 2>%s@;\
       Interface:@;\
       <1 2>%a@;\
       Implementation:@;\
       <1 2>%a"
      k pp a pp b
  in
  fatal @@ msg_ty loc f

let interface_missing_prop loc k pp ty =
  let f =
    F.dprintf
      "This interface does not match the implementation.@;\
       Missing prop name:@;\
       <1 2>%s@;\
       Of type:@;\
       <1 2>%a"
      k pp ty
  in
  fatal @@ msg_ty loc f

let msg_match = msg ~kind:"Matching error"
let msg_match_warn = msg ~kind:"Matching warning"

let unused_case loc =
  let f = F.dprintf "This match case is unused." in
  warn @@ msg_match_warn loc f

let parmatch loc pp_pat pat =
  let f =
    F.dprintf
      "This pattern-matching is not exhaustive.@;\
       Here's an example of a pattern which is not matched:@;\
       <1 2>%a"
      pp_pat pat
  in
  fatal @@ msg_match loc f

let duplicate_name s =
  let f =
    F.dprintf
      "@[<v>Compile error.@;There are multiple components with the name '%s'.@]"
      s
  in
  fatal @@ f

let msg_compile = F.dprintf "@[<v>Compile error.@;%t@]"
let pp_sep ppf () = F.fprintf ppf "@ -> "

let cycle stack =
  let f =
    F.dprintf "Dependency cycle detected.@;@[%a@]"
      (F.pp_print_list ~pp_sep F.pp_print_string)
      (List.rev stack)
  in
  Fatal (msg_compile f)

let missing_component stack name =
  let f =
    F.dprintf "Missing template:@;<1 2>%s@;Required by:@;<1 2>%s" name
      (List.hd stack)
  in
  Fatal (msg_compile f)

let internal ~__POS__:(file, lnum, cnum, enum) =
  F.kdprintf @@ fun t ->
  fatal
  @@ F.dprintf
       "This is a bug in the compiler. Please contact the Acutis developer.@;\
        @[OCaml source file %S, line %d, characters %d-%d.@]@;\
        @[%t@]"
       file lnum cnum enum t

let raise_fmt x = F.kdprintf fatal x
