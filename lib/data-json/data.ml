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
  if Set.Int.mem i cases then Data.const (`Int i) `Extra_bool
  else Error.bad_enum pp ty path j

let string ty path cases = function
  | `String s as j -> (
      match cases with
      | None -> Data.const j `Extra_none
      | Some cases ->
          if Set.String.mem s cases then Data.const j `Extra_none
          else Error.bad_enum pp ty path j)
  | j -> decode_error ty path j

let int ty path cases = function
  | `Int i as j -> (
      match cases with
      | None -> Data.const j `Extra_none
      | Some cases ->
          if Set.Int.mem i cases then Data.const j `Extra_none
          else Error.bad_enum pp ty path j)
  | j -> decode_error ty path j

let float path = function
  | `Float _ as x -> Data.const x `Extra_none
  | `Int i -> Data.const (`Float (float_of_int i)) `Extra_none
  | j -> decode_error (Ty.float ()) path j

let echo path = function
  | (`String _ | `Int _ | `Float _) as x -> Data.const x `Extra_none
  | `Bool false -> Data.const (`Int 0) `Extra_bool
  | `Bool true -> Data.const (`Int 1) `Extra_bool
  | j -> decode_error (Ty.echo ()) path j

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
  let f k ty =
    match (ty, List.assoc_opt k j) with
    | { contents = Ty.Nullable _ | Unknown _ }, None -> Data.null
    | ty, Some j -> make (EPath.key k path) ty j
    | _ -> Error.missing_key path (Ty.internal_record (ref tys)) k
  in
  Map.String.mapi f tys

and record path tys = function
  | `Assoc l -> Data.dict (record_aux path !tys l)
  | j -> decode_error (Ty.internal_record tys) path j

and union path ty key cases extra = function
  | `Assoc l as j ->
      let tag, tys =
        try
          let tag = List.assoc key l in
          match (tag, cases, extra) with
          | `Bool false, Ty.Variant.Int map, `Extra_bool ->
              let tag = 0 in
              (Data.const (`Int tag) extra, Map.Int.find tag map)
          | `Bool true, Int map, `Extra_bool ->
              let tag = 1 in
              (Data.const (`Int tag) extra, Map.Int.find tag map)
          | (`Int tag as x), Int map, `Extra_none ->
              (Data.const x extra, Map.Int.find tag map)
          | (`String tag as x), String map, `Extra_none ->
              (Data.const x extra, Map.String.find tag map)
          | _ -> raise Not_found
        with Not_found -> decode_error ty path j
      in
      let r = record_aux path !tys l in
      Data.dict (Map.String.add key tag r)
  | j -> decode_error ty path j

and make path ty j =
  match !ty with
  | Ty.Unknown _ -> Data.unknown j
  | Nullable ty -> nullable path ty j
  | Enum { extra = `Extra_bool; cases = Int cases; _ } ->
      boolean ty path cases j
  | String | Enum { row = `Open; cases = String _; _ } -> string ty path None j
  | Enum { row = `Closed; cases = String cases; _ } ->
      string ty path (Some cases) j
  | Int | Enum { row = `Open; cases = Int _; _ } -> int ty path None j
  | Enum { row = `Closed; cases = Int cases; _ } -> int ty path (Some cases) j
  | Float -> float path j
  | Echo -> echo path j
  | List ty -> list path ty j
  | Dict (ty, _) -> dict path ty j
  | Tuple tys -> tuple ty path tys j
  | Record tys -> record path tys j
  | Union (key, { cases; extra; _ }) -> union path ty key cases extra j

let decode ~name tys = function
  | `Assoc l -> record_aux (EPath.make name) tys l
  | j -> decode_error (Ty.internal_record (ref tys)) (EPath.make name) j

let rec record_to_json ty t =
  let f _ ty t =
    match (ty, t) with Some ty, Some t -> Some (to_json ty t) | _ -> None
  in
  let l = Map.String.merge f ty t |> Map.String.bindings in
  l

and to_json ty t =
  match (!ty, t) with
  | _, Data.Unknown j -> j
  | _, Const (`Float f, _) -> `Float f
  | _, Const (`String s, _) -> `String s
  | (Ty.Enum { extra = `Extra_bool; _ } | Echo), Const (`Int 0, `Extra_bool) ->
      `Bool false
  | (Ty.Enum { extra = `Extra_bool; _ } | Echo), Const (`Int _, `Extra_bool) ->
      `Bool true
  | (Enum _ | Int | Echo), Const (`Int i, _) -> `Int i
  | Nullable _, Nil -> `Null
  | Nullable ty, Array [| t |] -> to_json ty t
  | List ty, t ->
      let rec aux acc = function
        | Data.Nil -> `List (List.rev acc)
        | Array [| hd; tl |] -> aux (to_json ty hd :: acc) tl
        | _ -> assert false
      in
      aux [] t
  | Tuple tys, Array a ->
      let l = Array.to_list a |> List.map2 to_json tys in
      `List l
  | Dict (ty, _), Dict m ->
      let l = Map.String.map (to_json ty) m |> Map.String.bindings in
      `Assoc l
  | Record tys, Dict m -> `Assoc (record_to_json !tys m)
  | Union (k, { cases; _ }), Dict m ->
      let tag = Map.String.find k m in
      let record_tys =
        match (cases, tag) with
        | Int m, Const (`Int i, _) -> Map.Int.find i m
        | String m, Const (`String s, _) -> Map.String.find s m
        | _ -> assert false
      in
      let tag =
        match tag with
        | Const (`String s, _) -> `String s
        | Const (`Int 0, `Extra_bool) -> `Bool false
        | Const (`Int _, `Extra_bool) -> `Bool true
        | Const (`Int i, `Extra_none) -> `Int i
        | _ -> assert false
      in
      let l = record_to_json !record_tys m in
      `Assoc ((k, tag) :: l)
  | _ -> assert false

let encode tys j = `Assoc (record_to_json tys j)
