(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open StdlibExtra
module Ty = Typescheme

module Stack = struct
  type t = Nullable | Index of int | Key of string

  open Format

  let pp ppf = function
    | Nullable -> fprintf ppf "nullable"
    | Index i -> fprintf ppf "@[index: %i@]" i
    | Key s -> fprintf ppf "@[key: %S@]" s

  let pp_sep ppf () = fprintf ppf " ->@ "

  let pp ppf t =
    fprintf ppf "@[[@,%a]@]" (pp_print_list ~pp_sep pp) (List.rev t)
end

type t =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Assoc of (string * t) list
  | `List of t list ]

let pp_json = Yojson.Basic.pretty_print ~std:false
let decode_error = Error.decode Stack.pp pp_json

let boolean ty stack cases j =
  let i =
    match j with
    | `Bool false -> 0
    | `Bool true -> 1
    | j -> decode_error ty stack j
  in
  if SetInt.mem i cases then Data.const (`Int i) Extra_bool
  else Error.bad_enum Stack.pp pp_json ty stack j

let string ty stack cases = function
  | `String s as j -> (
      match cases with
      | None -> Data.const j Extra_none
      | Some cases ->
          if SetString.mem s cases then Data.const j Extra_none
          else Error.bad_enum Stack.pp pp_json ty stack j)
  | j -> decode_error ty stack j

let int ty stack cases = function
  | `Int i as j -> (
      match cases with
      | None -> Data.const j Extra_none
      | Some cases ->
          if SetInt.mem i cases then Data.const j Extra_none
          else Error.bad_enum Stack.pp pp_json ty stack j)
  | j -> decode_error ty stack j

let float stack = function
  | `Float _ as x -> Data.const x Extra_none
  | `Int i -> Data.const (`Float (float_of_int i)) Extra_none
  | j -> decode_error (Ty.float ()) stack j

let echo stack = function
  | (`String _ | `Int _ | `Float _) as x -> Data.const x Extra_none
  | `Bool false -> Data.const (`Int 0) Extra_bool
  | `Bool true -> Data.const (`Int 1) Extra_bool
  | j -> decode_error (Ty.echo ()) stack j

let rec nullable stack ty = function
  | `Null -> Data.null
  | j -> Data.some (make (Stack.Nullable :: stack) ty j)

and list stack ty = function
  | `List l ->
      let rec aux i acc = function
        | [] -> Data.list_rev acc
        | hd :: tl ->
            let acc = Data.list_cons (make (Index i :: stack) ty hd) acc in
            aux (succ i) acc tl
      in
      aux 0 Data.list_empty l
  | j -> decode_error (Ty.list ty) stack j

and dict stack ty = function
  | `Assoc l ->
      l |> List.to_seq
      |> Seq.map (fun (k, v) -> (k, make (Key k :: stack) ty v))
      |> MapString.of_seq |> Data.dict
  | j -> decode_error (Ty.dict ty) stack j

and tuple ty stack tys = function
  | `List l as j ->
      let l =
        try
          List.map2 (fun ty x -> (ty, x)) tys l
          |> List.mapi (fun i (ty, x) -> make (Index i :: stack) ty x)
        with Invalid_argument _ -> decode_error ty stack j
      in
      Data.tuple (Array.of_list l)
  | j -> decode_error (Ty.tuple tys) stack j

and record_aux stack tys j =
  let f k ty =
    match (ty, MapString.find_opt k j) with
    | { contents = Ty.Nullable _ | Unknown _ }, None -> Data.null
    | ty, Some j -> make (Key k :: stack) ty j
    | _ -> Error.missing_key Stack.pp stack (Ty.internal_record (ref tys)) k
  in
  MapString.mapi f tys

and record stack tys = function
  | `Assoc l ->
      let map = l |> List.to_seq |> MapString.of_seq in
      Data.dict (record_aux stack !tys map)
  | j -> decode_error (Ty.internal_record tys) stack j

and union stack ty key cases extra = function
  | `Assoc l as j -> (
      let map = l |> List.to_seq |> MapString.of_seq in
      try
        let tag = MapString.find key map in
        let tag, tys =
          match (tag, cases, extra) with
          | `Bool false, Ty.Variant.Int map, Ty.Variant.Extra_bool ->
              let tag = 0 in
              (Data.const (`Int tag) extra, MapInt.find tag map)
          | `Bool true, Int map, Extra_bool ->
              let tag = 1 in
              (Data.const (`Int tag) extra, MapInt.find tag map)
          | (`Int tag as x), Int map, Extra_none ->
              (Data.const x extra, MapInt.find tag map)
          | (`String tag as x), String map, Extra_none ->
              (Data.const x extra, MapString.find tag map)
          | _ -> raise Not_found
        in
        let r = record_aux stack !tys map in
        Data.dict (MapString.add key tag r)
      with Not_found -> decode_error ty stack j)
  | j -> decode_error ty stack j

and make stack ty j =
  match !ty with
  | Ty.Unknown _ -> Data.unknown j
  | Nullable ty -> nullable stack ty j
  | Enum { extra = Extra_bool; cases = Int cases; _ } ->
      boolean ty stack cases j
  | String | Enum { row = `Open; cases = String _; _ } -> string ty stack None j
  | Enum { row = `Closed; cases = String cases; _ } ->
      string ty stack (Some cases) j
  | Int | Enum { row = `Open; cases = Int _; _ } -> int ty stack None j
  | Enum { row = `Closed; cases = Int cases; _ } -> int ty stack (Some cases) j
  | Float -> float stack j
  | Echo -> echo stack j
  | List ty -> list stack ty j
  | Dict (ty, _) -> dict stack ty j
  | Tuple tys -> tuple ty stack tys j
  | Record tys -> record stack tys j
  | Union (key, { cases; extra; _ }) -> union stack ty key cases extra j

let to_data tys = function
  | `Assoc l ->
      let map = l |> List.to_seq |> MapString.of_seq in
      record_aux [] tys map
  | j -> decode_error (Ty.internal_record (ref tys)) [] j

let rec record_to_json ty t =
  let f _ ty t =
    match (ty, t) with Some ty, Some t -> Some (to_json ty t) | _ -> None
  in
  let l = MapString.merge f ty t |> MapString.bindings in
  l

and to_json ty t =
  match (!ty, t) with
  | _, Data.Unknown j -> j
  | _, Const (`Float f, _) -> `Float f
  | _, Const (`String s, _) -> `String s
  | (Ty.Enum { extra = Extra_bool; _ } | Echo), Const (`Int 0, Extra_bool) ->
      `Bool false
  | (Ty.Enum { extra = Extra_bool; _ } | Echo), Const (`Int _, Extra_bool) ->
      `Bool true
  | (Enum _ | Int | Echo), Const (`Int i, _) -> `Int i
  | Nullable _, Null -> `Null
  | Nullable ty, Array [| t |] -> to_json ty t
  | List ty, t ->
      let rec aux acc = function
        | Data.Null -> `List (List.rev acc)
        | Array [| hd; tl |] -> aux (to_json ty hd :: acc) tl
        | _ -> assert false
      in
      aux [] t
  | Tuple tys, Array a ->
      let l = a |> Array.to_list |> List.map2 to_json tys in
      `List l
  | Dict (ty, _), Dict m ->
      let l = m |> MapString.map (to_json ty) |> MapString.bindings in
      `Assoc l
  | Record tys, Dict m -> `Assoc (record_to_json !tys m)
  | Union (k, { cases; _ }), Dict m ->
      let tag = MapString.find k m in
      let record_tys =
        match (cases, tag) with
        | Int m, Const (`Int i, _) -> MapInt.find i m
        | String m, Const (`String s, _) -> MapString.find s m
        | _ -> assert false
      in
      let tag =
        match tag with
        | Const (`String s, _) -> `String s
        | Const (`Int 0, Extra_bool) -> `Bool false
        | Const (`Int _, Extra_bool) -> `Bool true
        | Const (`Int i, Extra_none) -> `Int i
        | _ -> assert false
      in
      let l = record_to_json !record_tys m in
      `Assoc ((k, tag) :: l)
  | _ -> assert false

let of_data tys j = `Assoc (record_to_json tys j)
