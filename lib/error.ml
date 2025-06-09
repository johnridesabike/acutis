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

let msg ~kind loc t =
  F.dprintf "@[<v>%s in file \"%s\", %a.@;%t@]" kind (Loc.fname loc) Loc.pp loc
    t

let msg_nofile ~kind t = F.dprintf "@[<v>%s.@;%t@]" kind t
let fatal f = raise @@ Fatal (f ~kind:"Error")
let warn f = Effect.perform @@ Warn (f ~kind:"Warning")

let loc_of_lexbuf Lexing.{ lex_start_p; lex_curr_p; _ } =
  (lex_start_p, lex_curr_p)

let lex_unexpected lexbuf c =
  fatal @@ msg (loc_of_lexbuf lexbuf) @@ F.dprintf "Unexpected character: %C" c

let lex_bad_int lexbuf s =
  fatal @@ msg (loc_of_lexbuf lexbuf) @@ F.dprintf "Invalid integer: %s" s

let lex_unterminated_comment lexbuf =
  fatal
  @@ msg (loc_of_lexbuf lexbuf)
  @@ F.dprintf "@[%a@]" F.pp_print_text "Unterminated comment."

let lex_unterminated_string lexbuf =
  fatal
  @@ msg (loc_of_lexbuf lexbuf)
  @@ F.dprintf "@[%a@]" F.pp_print_text "Unterminated string."

let parse_error i loc =
  fatal @@ msg loc
  @@
  try
    let mess = Parser_messages.message i in
    F.dprintf "@[%a@]" F.pp_print_text (String.trim mess)
  with Not_found -> F.dprintf "Unexpected token."

let dup_record_key loc key =
  fatal @@ msg loc @@ F.dprintf "Duplicate field '%a'." Pp.field key

let extra_record_tag loc =
  fatal @@ msg loc @@ F.dprintf "This tagged record has multiple tags."

let mismatch pp a b t =
  F.dprintf "Type mismatch.@;Expected:@;<1 2>%a@;Received:@;<1 2>%a" pp a pp b t

let type_mismatch loc pp a b = fatal @@ msg loc (mismatch pp a b)

let bad_block loc =
  fatal @@ msg loc
  @@ F.dprintf "Template blocks are not allowed in a destructure pattern."

let bad_field loc =
  fatal @@ msg loc
  @@ F.dprintf "Record '.' access is not allowed in a destructure pattern."

let missing_field loc key pp ty =
  fatal @@ msg loc
  @@ F.dprintf "This is missing key '%a' of type:@;<1 2>%a" Pp.field key pp ty

let underscore_in_construct loc =
  fatal @@ msg loc @@ F.dprintf "Underscore ('_') is not a valid name."

let name_bound_too_many loc s =
  fatal @@ msg loc
  @@ F.dprintf "The name '%s' is already bound in this pattern." s

let var_missing loc v =
  fatal @@ msg loc
  @@ F.dprintf "Variable '%s' must occur in each 'with' pattern." v

let var_unused loc s =
  warn @@ msg loc
  @@ F.dprintf "This variable is bound but never used:@;<1 2>%s" s

let pat_num_mismatch loc =
  fatal @@ msg loc @@ F.dprintf "Pattern count mismatch."

let map_pat_num_mismatch loc =
  fatal @@ msg loc
  @@ F.dprintf
       "@[Expressions 'map' and 'map_dict' can only have one or two@ patterns@ \
        for@ each@ 'with'@ expression.@]"

let component_name_mismatch loc a b =
  fatal @@ msg loc
  @@ F.dprintf
       "Component name mismatch.@;Expected:@;<1 2>%s.@;Received:@;<1 2>%s." a b

let component_extra_prop loc name s =
  warn @@ msg loc
  @@ F.dprintf "Component '%s' does not accept this prop:@;<1 2>%a." name
       Pp.field s

let interface_duplicate loc id =
  fatal @@ msg loc
  @@ F.dprintf "Prop '%s' is already defined in the interface." id

let interface_bad_name loc id =
  fatal @@ msg loc @@ F.dprintf "There is no type named '%s'." id

let interface_untagged_union loc =
  fatal @@ msg loc
  @@ F.dprintf "You cannot union records without a '@' tag field."

let interface_unmatched_tags loc s1 s2 =
  fatal @@ msg loc
  @@ F.dprintf "This record has tag field '@%a' instead of '@%a'." Pp.field s2
       Pp.field s1

let interface_duplicate_tag loc pp tag =
  fatal @@ msg loc
  @@ F.dprintf "Tag value '%a' is already used in this union." pp tag

let interface_open_bool_union loc =
  fatal @@ msg loc
  @@ F.dprintf "Unions with boolean tags cannot be opened with '...'."

let interface_type_mismatch loc k pp a b =
  fatal @@ msg loc
  @@ F.dprintf
       "This interface does not match the implementation.@;\
        Prop name:@;\
        <1 2>%s@;\
        Interface:@;\
        <1 2>%a@;\
        Implementation:@;\
        <1 2>%a"
       k pp a pp b

let interface_missing_prop loc k pp ty =
  fatal @@ msg loc
  @@ F.dprintf
       "This interface does not match the implementation.@;\
        Missing prop name:@;\
        <1 2>%s@;\
        Of type:@;\
        <1 2>%a"
       k pp ty

let unused_case loc = warn @@ msg loc @@ F.dprintf "This match case is unused."

let parmatch loc pp_pat pat =
  fatal @@ msg loc
  @@ F.dprintf
       "This pattern-matching is not exhaustive.@;\
        Here's an example of a pattern which is not matched:@;\
        <1 2>%a"
       pp_pat pat

let duplicate_name s =
  warn @@ msg_nofile
  @@ F.dprintf "There are multiple components with the name '%s'." s

let pp_sep ppf () = F.fprintf ppf "@ -> "

let cycle loc stack =
  Fatal
    (msg ~kind:"Error" loc
    @@ F.dprintf "Dependency cycle detected.@;@[%a@]"
         (F.pp_print_list ~pp_sep F.pp_print_string)
         (List.rev stack))

let missing_component loc name =
  Fatal (msg ~kind:"Error" loc @@ F.dprintf "Missing template:@;<1 2>%s" name)

let intf_decode_invalid s =
  fatal @@ msg_nofile @@ F.dprintf "'%s' is not a valid type." s

let intf_decode_enum s =
  fatal @@ msg_nofile @@ F.dprintf "'%s' is not valid in this enum." s

let intf_decode_single_param s =
  fatal @@ msg_nofile @@ F.dprintf "Type '%s' only takes one parameter." s

let intf_decode_empty_seq () =
  fatal @@ msg_nofile @@ F.dprintf "Type sequences cannot be empty."

let intf_decode_empty_record () =
  fatal @@ msg_nofile @@ F.dprintf "Records need at least one type."

let internal ~__POS__:(file, lnum, cnum, enum) =
  F.kdprintf @@ fun t ->
  let f =
    F.dprintf
      "This is a bug in the compiler. Please contact the Acutis developer.@;\
       @[OCaml source file %S, line %d, characters %d-%d.@]@;\
       @[%t@]"
      file lnum cnum enum t
  in
  raise (Fatal f)
