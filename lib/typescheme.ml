(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module MapInt = Map.Make (Int)
module MapString = Map.Make (String)
module SetInt = Set.Make (Int)
module SetString = Set.Make (String)
open Typechecker.Type

let map_of_list l = List.to_seq l |> MapString.of_seq

let nested_lists_to_map l =
  List.to_seq l |> Seq.map (fun (k, v) -> (k, ref (map_of_list v)))

type row = Typechecker.Type.row
type ty = Typechecker.Type.t
type t = ty MapString.t

let unknown = unknown
let int = int
let float = float
let string = string
let nullable = nullable
let list = list
let dict = dict
let tuple = tuple
let record l = record (ref (map_of_list l))
let enum_int row l = enum_int (sum (SetInt.of_list l) row)
let enum_string row l = enum_string (sum (SetString.of_list l) row)
let boolean = enum_false_and_true
let false_only = enum_false_only
let true_only = enum_true_only

let union_int row k l =
  union_int k (sum (nested_lists_to_map l |> MapInt.of_seq) row)

let union_string row k l =
  union_string k (sum (nested_lists_to_map l |> MapString.of_seq) row)

let union_boolean k ~f ~t =
  union_false_and_true k ~f:(ref (map_of_list f)) ~t:(ref (map_of_list t))

let union_false_only k l = union_false_only k (ref (map_of_list l))
let union_true_only k l = union_true_only k (ref (map_of_list l))
let make = map_of_list
let empty = MapString.empty

let pp ppf m =
  Format.fprintf ppf "@[<v>%a@]"
    (Format.pp_print_seq ~pp_sep:Format.pp_print_cut
       (Pp.equation ~sep:" =" Pp.field Typechecker.Type.pp))
    (MapString.to_seq m)
