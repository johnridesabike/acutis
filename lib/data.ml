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
  type t = Int of int | String of string | Float of float

  let compare a b =
    match (a, b) with
    | Int a, Int b -> compare a b
    | String a, String b -> compare a b
    | Float a, Float b -> compare a b
    | Int _, (String _ | Float _) | String _, Float _ -> -1
    | String _, Int _ | Float _, (Int _ | String _) -> 1

  let equal a b = compare a b = 0
end

type 'a t =
  | Other of 'a
  | Nil
  | Array of 'a t array
  | Dict of 'a t Map.String.t
  | Const of Const.t * Ty.Variant.extra

let other x = Other x
let null = Nil
let const x e = Const (x, e)
let int i = Const (Int i, Not_bool)
let bool i = Const (Int i, Bool)
let string s = Const (String s, Not_bool)
let float f = Const (Float f, Not_bool)
let some x = Array [| x |]
let dict m = Dict m
let tuple a = Array a
let list_cons hd tl = Array [| hd; tl |]

let list_rev =
  let rec aux acc = function
    | Array [| hd; tl |] -> aux (Array [| hd; acc |]) tl
    | _ -> acc
  in
  fun l -> aux Nil l

let list_empty = Nil

let rec flat_map f = function
  | Other x -> f x
  | Nil -> Nil
  | Array a -> Array (Array.map (flat_map f) a)
  | Dict d -> Dict (Map.String.map (flat_map f) d)
  | Const (a, b) -> Const (a, b)

let get_const = function Const (x, _) -> x | _ -> assert false
let get_tuple = function Array t -> t | _ -> assert false
let get_dict = function Dict t -> t | _ -> assert false
let is_null = function Nil -> true | _ -> false

let get_nullable = function
  | Nil -> None
  | Array [| t |] -> Some t
  | _ -> assert false

let fold_list f acc l =
  let rec aux i acc = function
    | Nil -> acc
    | Array [| hd; tl |] ->
        let acc = f ~index:(int i) acc hd in
        aux (succ i) acc tl
    | _ -> assert false
  in
  aux 0 acc l

let fold_dict f acc = function
  | Dict m -> Map.String.fold (fun k v acc -> f ~index:(string k) acc v) m acc
  | _ -> assert false

let echo = function
  | Const (Int 0, Bool) -> "false"
  | Const (_, Bool) -> "true"
  | Const (Int i, _) -> string_of_int i
  | Const (String s, _) -> s
  | Const (Float n, _) -> Printf.sprintf "%g" n
  | _ -> assert false
