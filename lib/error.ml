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

exception Acutis_error of string

let pp ~kind loc mess =
  F.asprintf "@[<v>File \"%s\", %a@;%s.@;%t@]" (Loc.fname loc) Loc.pp loc kind
    mess

let pp_lexbuf ~kind mess lexbuf =
  let start = lexbuf.Lexing.lex_start_p in
  let pos = lexbuf.lex_curr_p in
  pp ~kind (start, pos) mess

let lex_error =
  let f = F.dprintf "%," in
  fun lexbuf -> raise @@ Acutis_error (pp_lexbuf ~kind:"Syntax error" f lexbuf)

let parse_error i lexbuf =
  let f =
    try
      let mess = ParserMessages.message i in
      F.dprintf "@[%a@]" F.pp_print_text mess
    with Not_found -> F.dprintf "Unexpected token."
  in
  raise @@ Acutis_error (pp_lexbuf ~kind:"Parse error" f lexbuf)

let pp_ty = pp ~kind:"Type error"

let dup_record_key loc key =
  let f = F.dprintf "Duplicate field '%a'." Pp.field key in
  raise @@ Acutis_error (pp_ty loc f)

let extra_record_tag =
  let f = F.dprintf "This tagged record has multiple tags." in
  fun loc -> raise @@ Acutis_error (pp_ty loc f)

let mismatch a b t =
  F.dprintf "Type mismatch.@;Expected:@;<1 2>%a@;Received:@;<1 2>%a"
    Typescheme.pp a Typescheme.pp b t

let type_mismatch loc a b = raise @@ Acutis_error (pp_ty loc (mismatch a b))

let bad_block loc =
  let f =
    F.dprintf "Template blocks are not allowed in a destructure pattern."
  in
  raise @@ Acutis_error (pp_ty loc f)

let bad_field loc =
  let f =
    F.dprintf "Record '.' access is not allowed in a destructure pattern."
  in
  raise @@ Acutis_error (pp_ty loc f)

let missing_field loc key ty =
  let f =
    F.dprintf "This is missing key '%a' of type:@;<1 2>%a" Pp.field key
      Typescheme.pp ty
  in
  raise @@ Acutis_error (pp_ty loc f)

let underscore_in_construct =
  let f = F.dprintf "Underscore ('_') is not a valid name." in
  fun loc -> raise @@ Acutis_error (pp_ty loc f)

let name_bound_too_many loc s =
  let f = F.dprintf "The name '%s' is already bound in this pattern." s in
  raise @@ Acutis_error (pp_ty loc f)

let var_missing loc v =
  let f = F.dprintf "Variable '%s' must occur in each 'with' pattern." v in
  raise @@ Acutis_error (pp_ty loc f)

let var_unused loc s =
  let f = F.dprintf "This variable is bound but never used:@;<1 2>%s" s in
  raise @@ Acutis_error (pp_ty loc f)

let pat_num_mismatch =
  let f = F.dprintf "Pattern count mismatch." in
  fun loc -> raise @@ Acutis_error (pp_ty loc f)

let map_pat_num_mismatch =
  let f =
    F.dprintf
      "@[Expressions 'map' and 'map_dict' can only have one or two@ patterns@ \
       for@ each@ 'with'@ expression.@]"
  in
  fun loc -> raise @@ Acutis_error (pp_ty loc f)

let component_name_mismatch loc a b =
  let f =
    F.dprintf
      "Component name mismatch.@;Expected:@;<1 2>%s.@;Received:@;<1 2>%s." a b
  in
  raise @@ Acutis_error (pp_ty loc f)

let component_extra_prop loc name s =
  let f =
    F.dprintf "Component '%s' does not accept this prop:@;<1 2>%a." name
      Pp.field s
  in
  raise @@ Acutis_error (pp_ty loc f)

let interface_duplicate loc id =
  let f = F.dprintf "Prop '%s' is already defined in the interface." id in
  raise @@ Acutis_error (pp_ty loc f)

let interface_bad_name loc id =
  let f = F.dprintf "There is no type named '%s'." id in
  raise @@ Acutis_error (pp_ty loc f)

let interface_untagged_union loc =
  let f = F.dprintf "You cannot union records without a '@' tag field." in
  raise @@ Acutis_error (pp_ty loc f)

let interface_unmatched_tags loc s1 s2 =
  let f =
    F.dprintf "This record has tag field '@%a' instead of '@%a'." Pp.field s2
      Pp.field s1
  in
  raise @@ Acutis_error (pp_ty loc f)

let interface_duplicate_tag loc pp tag =
  let f = F.dprintf "Tag value '%a' is already used in this union." pp tag in
  raise @@ Acutis_error (pp_ty loc f)

let interface_open_bool_union loc =
  let f = F.dprintf "Unions with boolean tags cannot be opened with '...'." in
  raise @@ Acutis_error (pp_ty loc f)

let interface_type_mismatch loc k a b =
  let f =
    F.dprintf
      "This interface does not match the implementation.@;\
       Prop name:@;\
       <1 2>%s@;\
       Interface:@;\
       <1 2>%a@;\
       Implementation:@;\
       <1 2>%a" k Typescheme.pp a Typescheme.pp b
  in
  raise @@ Acutis_error (pp_ty loc f)

let interface_missing_prop loc k ty =
  let f =
    F.dprintf
      "This interface does not match the implementation.@;\
       Missing prop name:@;\
       <1 2>%s@;\
       Of type:@;\
       <1 2>%a" k Typescheme.pp ty
  in
  raise @@ Acutis_error (pp_ty loc f)

let pp_match = pp ~kind:"Matching error"

let unused_case loc =
  let f = F.dprintf "This match case is unused." in
  raise @@ Acutis_error (pp_match loc f)

let parmatch loc pp_pat pat =
  let f =
    F.dprintf
      "This pattern-matching is not exhaustive.@;\
       Here's an example of a pattern which is not matched:@;\
       <1 2>%a" pp_pat pat
  in
  raise @@ Acutis_error (pp_match loc f)

let duplicate_name s =
  let s =
    F.asprintf
      "@[<v>Compile error.@;There are multiple components with the name '%s'.@]"
      s
  in
  raise @@ Acutis_error s

let pp_sep ppf () = F.fprintf ppf "@ -> "

let cycle stack =
  let s =
    F.asprintf "@[<v>Compile error.@;Dependency cycle detected.@;@[%a@]@]"
      (F.pp_print_list ~pp_sep F.pp_print_string)
      (List.rev stack)
  in
  raise @@ Acutis_error s

let missing_component stack name =
  let s =
    F.asprintf
      "@[<v>Compile error.@;Missing template:@;<1 2>%s@;Required by:@;<1 2>%s@]"
      name (List.hd stack)
  in
  raise @@ Acutis_error s

module DecodePath = struct
  type path = Nullable | Index of int | Key of string
  type t = { name : string; path : path list }

  let make name = { name; path = [] }
  let nullable { name; path } = { name; path = Nullable :: path }
  let index i { name; path } = { name; path = Index i :: path }
  let key k { name; path } = { name; path = Key k :: path }

  let pp_path ppf = function
    | Nullable -> F.pp_print_string ppf "nullable"
    | Index i -> F.pp_print_int ppf i
    | Key s -> F.fprintf ppf "%S" s

  let pp_path ppf = function
    | [] -> F.pp_print_string ppf "input"
    | l ->
        F.fprintf ppf "@[input@ -> %a@]"
          (F.pp_print_list ~pp_sep pp_path)
          (List.rev l)
end

let pp { DecodePath.name; path } ty mess =
  F.asprintf
    "@[<v>File \"%s\"@;\
     Render error.@;\
     The data supplied does not match this template's interface.@;\
     Path:@;\
     <1 2>%a.@;\
     Expected type:@;\
     <1 2>%a@;\
     %t@]"
    name DecodePath.pp_path path Typescheme.pp ty mess

let decode pp_data ty stack data =
  let f = F.dprintf "Received value:@;<1 2>%a" pp_data data in
  raise @@ Acutis_error (pp stack ty f)

let missing_key stack ty key =
  let f = F.dprintf "Input is missing key:@;<1 2>%a" Pp.field key in
  raise @@ Acutis_error (pp stack ty f)

let bad_enum pp_data ty stack data =
  let f =
    F.dprintf "This type does not allow the given value:@;<1 2>%a" pp_data data
  in
  raise @@ Acutis_error (pp stack ty f)

let internal (file, lnum, cnum, enum) s =
  let s =
    F.asprintf
      "@[<v>Compile error.@;\
       This is a bug in the compiler. Please contact the Acutis developer.@;\
       @[OCaml source file %S, line %d, characters %d-%d.@]@;\
       @[%a@]@]"
      file lnum cnum enum F.pp_print_text s
  in
  raise @@ Acutis_error s
