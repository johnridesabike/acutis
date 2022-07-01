(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module Ty = Typescheme

module Const = struct
  type t = [ `Int of int | `String of string | `Float of float ]
  [@@deriving eq, ord, show]

  let to_string (t : t) (extra : Ty.Variant.extra) =
    match (t, extra) with
    | `Int 0, `Extra_bool -> "false"
    | _, `Extra_bool -> "true"
    | `String s, _ -> s
    | `Float n, _ -> Printf.sprintf "%g" n
    | `Int i, _ -> string_of_int i
end

type 'a t =
  | Unknown of 'a
  | Null
  | Array of 'a t array
  | Dict of 'a t Map.String.t
  | Const of Const.t * Ty.Variant.extra

let unknown x = Unknown x
let null = Null
let const x e = Const (x, e)
let some x = Array [| x |]
let dict m = Dict m
let tuple a = Array a
let list_cons hd tl = Array [| hd; tl |]

let list_rev =
  let rec aux acc = function
    | Array [| hd; tl |] -> aux (Array [| hd; acc |]) tl
    | _ -> acc
  in
  fun l -> aux Null l

let list_empty = Null
let get_const = function Const (x, _) -> x | _ -> assert false
let get_tuple = function Array t -> t | _ -> assert false
let get_dict = function Dict t -> t | _ -> assert false
let is_null = function Null -> true | _ -> false

let get_nullable = function
  | Null -> None
  | Array [| t |] -> Some t
  | _ -> assert false

let fold_list f acc l =
  let rec aux i acc = function
    | Null -> acc
    | Array [| hd; tl |] ->
        let acc = f ~index:(Const (`Int i, `Extra_none)) acc hd in
        aux (succ i) acc tl
    | _ -> assert false
  in
  aux 0 acc l

let fold_dict f acc = function
  | Dict m ->
      Map.String.fold
        (fun k v acc -> f ~index:(Const (`String k, `Extra_none)) acc v)
        m acc
  | _ -> assert false

let to_string = function
  | Const (x, e) -> Const.to_string x e
  | _ -> assert false
