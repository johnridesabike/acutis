(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open Typechecker

let map_of_list l = List.to_seq l |> Map.String.of_seq

let nested_lists_to_map l =
  List.to_seq l |> Seq.map (fun (k, v) -> (k, ref (map_of_list v)))

type row = Typechecker.row
type ty = Typechecker.ty
type t = ty Map.String.t

let unknown = ty_unknown
let int = ty_int
let float = ty_float
let string = ty_string
let nullable = ty_nullable
let list = ty_list
let dict = ty_dict
let tuple = ty_tuple
let record l = ty_record (ref (map_of_list l))
let enum_int row l = ty_enum (enum_int (List.to_seq l |> Set.Int.of_seq) row)

let enum_string row l =
  ty_enum (enum_string (List.to_seq l |> Set.String.of_seq) row)

let boolean () = ty_enum (enum_false_and_true ())
let false_only () = ty_enum (enum_false_only ())
let true_only () = ty_enum (enum_true_only ())

let union_int row k l =
  ty_union k (union_int (nested_lists_to_map l |> Map.Int.of_seq) row)

let union_string row k l =
  ty_union k (union_string (nested_lists_to_map l |> Map.String.of_seq) row)

let union_boolean k ~f ~t =
  ty_union k
    (union_false_and_true ~f:(ref (map_of_list f)) ~t:(ref (map_of_list t)))

let union_false_only k l = ty_union k (union_false_only (ref (map_of_list l)))
let union_true_only k l = ty_union k (union_true_only (ref (map_of_list l)))
let make = map_of_list
let empty = Map.String.empty

let pp =
  let equation ppf (k, v) =
    Format.fprintf ppf "@[<hv 2>%a =@ %a@]" Pp.field k pp_ty v
  in
  fun ppf m ->
    Format.fprintf ppf "@[<v>%a@]"
      (Format.pp_print_seq ~pp_sep:Format.pp_print_cut equation)
      (Map.String.to_seq m)
