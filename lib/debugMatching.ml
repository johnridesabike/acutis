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
open Matching

let pp_const ppf = function
  | Data.Const.Int i -> F.fprintf ppf "(@[<2>Int@ %d@])" i
  | String s -> F.fprintf ppf "(@[<2>String@ %S@])" s
  | Float f -> F.fprintf ppf "(@[<2>Float@ %F@])" f

let pp_row ppf = function
  | `Closed -> F.pp_print_string ppf "`Closed"
  | `Open -> F.pp_print_string ppf "`Open"

let pp_debug_nest_info ppf = function
  | Not_dict -> F.pp_print_string ppf "Not_dict"
  | Dict -> F.pp_print_string ppf "Dict"

let rec pp_tree :
          'leaf 'key.
          (F.formatter -> 'leaf -> unit) ->
          (F.formatter -> 'key -> unit) ->
          F.formatter ->
          ('leaf, 'key) tree ->
          unit =
 fun pp_leaf pp_key ppf -> function
  | Switch { key; ids; cases; wildcard; debug_row } ->
      F.fprintf ppf
        "(@[<2>Switch {@,\
         @[key =@ %a@];@ @[ids =@ %a@];@ @[cases =@ %a@];@ @[wildcard =@ \
         %a@];@ @[debug_row =@ %a@]@]})"
        pp_key key Pp.set_int ids
        (pp_switchcase pp_leaf pp_key)
        cases
        (Pp.option (pp_tree pp_leaf pp_key))
        wildcard pp_row debug_row
  | Nest { key; ids; child; wildcard; debug } ->
      F.fprintf ppf
        "(@[<2>Nest {@,\
         @[key =@ %a@];@ @[ids =@ %a@];@ @[child =@ %a@];@ @[wildcard =@ \
         %a@];@ @[debug =@ %a@]@]})"
        pp_key key Pp.set_int ids (pp_nest pp_leaf pp_key) child
        (Pp.option (pp_tree pp_leaf pp_key))
        wildcard pp_debug_nest_info debug
  | Construct { key; ids; nil; cons } ->
      F.fprintf ppf
        "(@[<2>Construct {@,\
         @[key =@ %a@];@ @[ids =@ %a@];@ @[nil =@ %a@];@ @[cons =@ %a@]@]})"
        pp_key key Pp.set_int ids
        (Pp.option (pp_tree pp_leaf pp_key))
        nil
        (Pp.option (pp_tree pp_leaf pp_key))
        cons
  | Wildcard { key; ids; child } ->
      F.fprintf ppf
        "(@[<2>Wildcard {@,@[key =@ %a@];@ @[ids =@ %a@];@ @[child =@ %a@]@]})"
        pp_key key Pp.set_int ids (pp_tree pp_leaf pp_key) child
  | End a -> F.fprintf ppf "(@[<2>End@ %a@])" pp_leaf a

and pp_nest :
      'leaf 'key.
      (F.formatter -> 'leaf -> unit) ->
      (F.formatter -> 'key -> unit) ->
      F.formatter ->
      ('leaf, 'key) nest ->
      unit =
 fun pp_leaf pp_key ppf -> function
  | Int_keys a ->
      F.fprintf ppf "(@[<2>Int_keys@ %a@])"
        (pp_tree (pp_tree pp_leaf pp_key) F.pp_print_int)
        a
  | String_keys a ->
      F.fprintf ppf "(@[<2>String_keys@ %a@])"
        (pp_tree (pp_tree pp_leaf pp_key) Pp.syntax_string)
        a

and pp_switchcase :
      'leaf 'key.
      (F.formatter -> 'leaf -> unit) ->
      (F.formatter -> 'key -> unit) ->
      F.formatter ->
      ('leaf, 'key) switchcase ->
      unit =
 fun pp_leaf pp_key ppf { data; if_match; next } ->
  F.fprintf ppf
    "@[<2>{ @[data =@ %a@];@ @[if_match =@ %a@];@ @[next =@ %a@]@ }@]" pp_const
    data (pp_tree pp_leaf pp_key) if_match
    (Pp.option (pp_switchcase pp_leaf pp_key))
    next

let pp_leaf ppf { names; exit } =
  F.fprintf ppf "@[<2>{ @[names =@ %a@];@ @[exit =@ %a@]@ }@]"
    (Pp.map_string F.pp_print_int)
    names Exit.pp_key exit

type t = (leaf, int) tree

let equal = equal_tree equal_leaf Int.equal
let pp = pp_tree pp_leaf F.pp_print_int
