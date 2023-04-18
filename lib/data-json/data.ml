(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open Acutis
module Ty = Typescheme
module EPath = Error.DecodePath

type 'a map = 'a Stdlib.Map.Make(String).t

type t =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Assoc of (string * t) list
  | `List of t list ]

let pp = Yojson.Basic.pretty_print ~std:false
let decode_error = Error.decode pp

let boolean ty path cases j =
  let i =
    match j with
    | `Bool false -> 0
    | `Bool true -> 1
    | j -> decode_error ty path j
  in
  if Set.Int.mem i cases then Data.bool i else Error.bad_enum pp ty path j

let string ty path cases = function
  | `String s as j -> (
      match cases with
      | None -> Data.string s
      | Some cases ->
          if Set.String.mem s cases then Data.string s
          else Error.bad_enum pp ty path j)
  | j -> decode_error ty path j

let int ty path cases = function
  | `Int i as j -> (
      match cases with
      | None -> Data.int i
      | Some cases ->
          if Set.Int.mem i cases then Data.int i
          else Error.bad_enum pp ty path j)
  | j -> decode_error ty path j

let float path = function
  | `Float f -> Data.float f
  | `Int i -> Data.float (float_of_int i)
  | j -> decode_error (Ty.float ()) path j

let rec nullable path ty = function
  | `Null -> Data.null
  | j -> Data.some (make (EPath.nullable path) ty j)

and list path ty = function
  | `List l ->
      let rec aux i acc = function
        | [] -> Data.list_rev acc
        | hd :: tl ->
            let acc = Data.list_cons (make (EPath.index i path) ty hd) acc in
            aux (succ i) acc tl
      in
      aux 0 Data.list_empty l
  | j -> decode_error (Ty.list ty) path j

and dict path ty = function
  | `Assoc l ->
      List.fold_left
        (fun map (k, v) -> Map.String.add k (make (EPath.key k path) ty v) map)
        Map.String.empty l
      |> Data.dict
  | j -> decode_error (Ty.dict ty) path j

and tuple ty path tys = function
  | `List l as j -> (
      try
        List.map2 (fun ty x -> (ty, x)) tys l
        |> List.mapi (fun i (ty, x) -> make (EPath.index i path) ty x)
        |> Array.of_list |> Data.tuple
      with Invalid_argument _ -> decode_error ty path j)
  | j -> decode_error ty path j

and record_aux path tys j =
  Map.String.mapi
    (fun k ty ->
      match (ty, List.assoc_opt k j) with
      | { contents = Ty.Nullable _ | Unknown _ }, None -> Data.null
      | ty, Some j -> make (EPath.key k path) ty j
      | _ -> Error.missing_key path (Ty.internal_record (ref tys)) k)
    tys

and record path tys = function
  | `Assoc l -> Data.dict (record_aux path !tys l)
  | j -> decode_error (Ty.internal_record tys) path j

and union path ty key Ty.Variant.{ cases; extra; row } = function
  | `Assoc l as j -> (
      let tag =
        try List.assoc key l with Not_found -> decode_error ty path j
      in
      let tag, tys =
        match (tag, cases, extra) with
        | `Bool false, VInt map, Bool ->
            let tag = 0 in
            (Data.bool tag, Map.Int.find_opt tag map)
        | `Bool true, VInt map, Bool ->
            let tag = 1 in
            (Data.bool tag, Map.Int.find_opt tag map)
        | `Int tag, VInt map, Not_bool ->
            (Data.int tag, Map.Int.find_opt tag map)
        | `String tag, VString map, _ ->
            (Data.string tag, Map.String.find_opt tag map)
        | _ -> decode_error ty path j
      in
      match (tys, row) with
      | Some tys, (`Open | `Closed) ->
          record_aux path !tys l |> Map.String.add key tag |> Data.dict
      | None, `Open -> Map.String.singleton key tag |> Data.dict
      | None, `Closed -> decode_error ty path j)
  | j -> decode_error ty path j

and make path ty j =
  match !ty with
  | Ty.Unknown _ -> Data.other j
  | Nullable ty -> nullable path ty j
  | Enum { extra = Bool; cases = VInt cases; _ } -> boolean ty path cases j
  | String | Enum { row = `Open; cases = VString _; _ } -> string ty path None j
  | Enum { row = `Closed; cases = VString cases; _ } ->
      string ty path (Some cases) j
  | Int | Enum { row = `Open; cases = VInt _; _ } -> int ty path None j
  | Enum { row = `Closed; cases = VInt cases; _ } -> int ty path (Some cases) j
  | Float -> float path j
  | List ty -> list path ty j
  | Dict (ty, _) -> dict path ty j
  | Tuple tys -> tuple ty path tys j
  | Record tys -> record path tys j
  | Union (key, variant) -> union path ty key variant j

let decode ~name tys = function
  | `Assoc l -> record_aux (EPath.make name) tys l
  | j -> decode_error (Ty.internal_record (ref tys)) (EPath.make name) j

let rec record_to_json ty t =
  Map.String.merge
    (fun _ ty t ->
      match (ty, t) with Some ty, Some t -> Some (to_json ty t) | _ -> None)
    ty t
  |> Map.String.bindings

and to_json ty t =
  match (!ty, t) with
  | _, Data.Other j -> j
  | _, Const (Float f) -> `Float f
  | _, Const (String s) -> `String s
  | Ty.Enum { extra = Bool; _ }, Const (Int 0) -> `Bool false
  | Ty.Enum { extra = Bool; _ }, Const (Int _) -> `Bool true
  | (Enum _ | Int | Unknown _), Const (Int i) -> `Int i
  | (Nullable _ | Unknown _), Nil -> `Null
  | Nullable ty, Array [| t |] -> to_json ty t
  | List ty, t ->
      let rec aux acc = function
        | Data.Nil -> `List (List.rev acc)
        | Array [| hd; tl |] -> aux (to_json ty hd :: acc) tl
        | _ -> Error.internal __POS__ "Lists may only contain Array or Nil."
      in
      aux [] t
  | Tuple tys, Array a -> `List (Array.to_list a |> List.map2 to_json tys)
  | Unknown _, Array a -> `List (Array.map (to_json ty) a |> Array.to_list)
  | Dict (ty, _), Dict m ->
      `Assoc (Map.String.map (to_json ty) m |> Map.String.bindings)
  | Unknown _, Dict m ->
      `Assoc (Map.String.map (to_json ty) m |> Map.String.bindings)
  | Record tys, Dict m -> `Assoc (record_to_json !tys m)
  | Union (k, { cases; extra; _ }), Dict m ->
      let tag = Map.String.find k m in
      let record_tys =
        match (cases, tag) with
        | VInt m, Const (Int i) -> Map.Int.find i m
        | VString m, Const (String s) -> Map.String.find s m
        | _ -> Error.internal __POS__ "Type mismatch while encoding a union."
      in
      let tag =
        match tag with
        | Const (String s) -> `String s
        | Const (Int i) -> (
            match (extra, i) with
            | Bool, 0 -> `Bool false
            | Bool, _ -> `Bool true
            | Not_bool, i -> `Int i)
        | _ -> Error.internal __POS__ "Union tags may only be ints or strings."
      in
      let l = record_to_json !record_tys m in
      `Assoc ((k, tag) :: l)
  | ( ( String | Int | Float | Enum _ | Nullable _ | Tuple _ | Dict _ | Record _
      | Union _ ),
      _ ) ->
      Error.internal __POS__ "Type mismatch while encoding data."

let encode tys j = `Assoc (record_to_json tys j)
