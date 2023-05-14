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

  let to_sexp = function
    | Int i -> Sexp.int i
    | String s -> Sexp.string s
    | Float f -> Sexp.float f
end

type 'a t =
  | Other of 'a
  | Nil
  | Array of 'a t array
  | Dict of 'a t Map.String.t
  | Const of Const.t

let other x = Other x
let null = Nil
let const x = Const x
let int i = Const (Int i)
let bool i = Const (Int i)
let string s = Const (String s)
let float f = Const (Float f)
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

let rec map f = function
  | Other x -> Other (f x)
  | Nil -> Nil
  | Array a -> Array (Array.map (map f) a)
  | Dict d -> Dict (Map.String.map (map f) d)
  | Const a -> Const a

let get_const = function
  | Const x -> x
  | _ -> Error.internal __POS__ "Expected Const."

let get_tuple = function
  | Array t -> t
  | _ -> Error.internal __POS__ "Expected Array."

let get_dict = function
  | Dict t -> t
  | _ -> Error.internal __POS__ "Expected Dict."

let is_null = function Nil -> true | _ -> false
let get_nullable = function Array a -> Some a.(0) | _ -> None

let fold_list f acc l =
  let rec aux i acc = function
    | Nil -> acc
    | Array [| hd; tl |] ->
        let acc = f ~index:(int i) acc hd in
        aux (succ i) acc tl
    | _ -> Error.internal __POS__ "Lists may only contain Array or Nil."
  in
  aux 0 acc l

let fold_dict f acc = function
  | Dict m -> Map.String.fold (fun k v acc -> f ~index:(string k) acc v) m acc
  | _ -> Error.internal __POS__ "Expected Dict."

let rec to_sexp f = function
  | Other x -> f x
  | Nil -> Sexp.symbol "nil"
  | Array a -> Sexp.make "array" [ Sexp.of_seq (to_sexp f) (Array.to_seq a) ]
  | Dict d -> Sexp.make "dict" [ dict_to_sexp f d ]
  | Const c -> Const.to_sexp c

and dict_to_sexp f d = Sexp.map_string (to_sexp f) d
