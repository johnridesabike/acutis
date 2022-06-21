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
open Js_of_ocaml
module Ty = Typescheme

type t = Js.Unsafe.any

let stringify (j : t) =
  Js.Unsafe.meth_call Js._JSON "stringify" [| j |] |> Js.to_string

let pp ppf j = Format.pp_print_string ppf (stringify j)
let decode_error = Error.decode pp

type tagged =
  | Js_null
  | Js_bool of bool
  | Js_string of string
  | Js_float of float
  | Js_object of (string * t) Seq.t
  | Js_array of t array

let classify j =
  match Js.to_string (Js.typeof j) with
  | "string" -> Js_string (Js.Unsafe.coerce j |> Js.to_string)
  | "number" -> Js_float (Js.Unsafe.coerce j |> Js.float_of_number)
  | "boolean" -> Js_bool (Js.Unsafe.coerce j |> Js.to_bool)
  | _ -> (
      match Js.Opt.to_option (Js.some j) with
      | None -> Js_null
      | Some j ->
          if Js.instanceof j Js.array_empty then
            Js_array (Js.Unsafe.coerce j |> Js.to_array)
          else
            let f s =
              let s = Js.to_string s in
              (s, Js.Unsafe.get j s)
            in
            let seq =
              Js.object_keys j |> Js.to_array |> Array.to_seq |> Seq.map f
            in
            Js_object seq)

let boolean ty stack cases j =
  match classify j with
  | Js_bool b ->
      let i = match b with false -> 0 | true -> 1 in
      if SetInt.mem i cases then Data.const (`Int i) `Extra_bool
      else Error.bad_enum pp ty stack j
  | _ -> decode_error ty stack j

let string ty stack cases j =
  match classify j with
  | Js_string s -> (
      match cases with
      | None -> Data.const (`String s) `Extra_none
      | Some cases ->
          if SetString.mem s cases then Data.const (`String s) `Extra_none
          else Error.bad_enum pp ty stack j)
  | _ -> decode_error ty stack j

let int ty stack cases j =
  match classify j with
  | Js_float f -> (
      let i = int_of_float f in
      match cases with
      | None -> Data.const (`Int i) `Extra_none
      | Some cases ->
          if SetInt.mem i cases then Data.const (`Int i) `Extra_none
          else Error.bad_enum pp ty stack j)
  | _ -> decode_error ty stack j

let float stack j =
  match classify j with
  | Js_float f -> Data.const (`Float f) `Extra_none
  | _ -> decode_error (Ty.float ()) stack j

let echo stack j =
  match classify j with
  | Js_float f -> Data.const (`Float f) `Extra_none
  | Js_string s -> Data.const (`String s) `Extra_none
  | Js_bool b ->
      let i = match b with false -> 0 | true -> 1 in
      Data.const (`Int i) `Extra_bool
  | _ -> decode_error (Ty.echo ()) stack j

let rec nullable stack ty j =
  match classify j with
  | Js_null -> Data.null
  | _ -> Data.some (make (Error.DecodeStack.Nullable :: stack) ty j)

and list stack ty j =
  match classify j with
  | Js_array a ->
      let len = Array.length a in
      let l = ref Data.list_empty in
      for i = 0 to len - 1 do
        let x = Array.unsafe_get a i in
        l := Data.list_cons (make (Index i :: stack) ty x) !l
      done;
      Data.list_rev !l
  | _ -> decode_error (Ty.list ty) stack j

and dict stack ty j =
  match classify j with
  | Js_object o ->
      o
      |> Seq.map (fun (k, v) -> (k, make (Key k :: stack) ty v))
      |> MapString.of_seq |> Data.dict
  | _ -> decode_error (Ty.dict ty) stack j

and tuple ty stack tys j =
  match classify j with
  | Js_array a ->
      let tys = Array.of_list tys in
      if Array.length a = Array.length tys then
        Array.map2 (fun ty x -> (ty, x)) tys a
        |> Array.mapi (fun i (ty, x) -> make (Index i :: stack) ty x)
        |> Data.tuple
      else decode_error ty stack j
  | _ -> decode_error ty stack j

and record_aux stack tys j =
  let f k ty =
    match (ty, MapString.find_opt k j) with
    | { contents = Ty.Nullable _ | Unknown _ }, None -> Data.null
    | ty, Some j -> make (Key k :: stack) ty j
    | _ -> Error.missing_key stack (Ty.internal_record (ref tys)) k
  in
  MapString.mapi f tys

and record stack tys j =
  match classify j with
  | Js_object o -> o |> MapString.of_seq |> record_aux stack !tys |> Data.dict
  | _ -> decode_error (Ty.internal_record tys) stack j

and union stack ty key cases extra j =
  match classify j with
  | Js_object o ->
      let map = MapString.of_seq o in
      let tag, tys =
        try
          let tag = MapString.find key map in
          match (classify tag, cases, extra) with
          | Js_bool false, Ty.Variant.Int map, `Extra_bool ->
              let tag = 0 in
              (Data.const (`Int tag) extra, MapInt.find tag map)
          | Js_bool true, Int map, `Extra_bool ->
              let tag = 1 in
              (Data.const (`Int tag) extra, MapInt.find tag map)
          | Js_float tag, Int map, `Extra_none ->
              let tag = int_of_float tag in
              (Data.const (`Int tag) extra, MapInt.find tag map)
          | Js_string tag, String map, `Extra_none ->
              (Data.const (`String tag) extra, MapString.find tag map)
          | _ -> raise Not_found
        with Not_found -> decode_error ty stack j
      in
      let r = record_aux stack !tys map in
      Data.dict (MapString.add key tag r)
  | _ -> decode_error ty stack j

and make stack ty j =
  match !ty with
  | Ty.Unknown _ -> Data.unknown j
  | Nullable ty -> nullable stack ty j
  | Enum { extra = `Extra_bool; cases = Int cases; _ } ->
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

let decode tys j =
  match classify j with
  | Js_object o ->
      let map = MapString.of_seq o in
      record_aux [] tys map
  | _ -> decode_error (Ty.internal_record (ref tys)) [] j

let coerce = Js.Unsafe.coerce

let rec record_to_js ty t =
  let f _ ty t =
    match (ty, t) with Some ty, Some t -> Some (to_js ty t) | _ -> None
  in
  let l = MapString.merge f ty t |> MapString.bindings in
  l

and to_js ty t =
  match (!ty, t) with
  | _, Data.Unknown j -> j
  | _, Const (`Float f, _) -> Js.number_of_float f |> coerce
  | _, Const (`String s, _) -> Js.string s |> coerce
  | (Ty.Enum { extra = `Extra_bool; _ } | Echo), Const (`Int 0, `Extra_bool) ->
      Js._false |> coerce
  | (Ty.Enum { extra = `Extra_bool; _ } | Echo), Const (`Int _, `Extra_bool) ->
      Js._true |> coerce
  | (Enum _ | Int | Echo), Const (`Int i, _) ->
      i |> float_of_int |> Js.number_of_float |> coerce
  | Nullable _, Null -> Js.null |> Js.Unsafe.inject
  | Nullable ty, Array [| t |] -> to_js ty t
  | List ty, t ->
      let rec aux acc = function
        | Data.Null -> acc |> List.rev |> Array.of_list |> Js.array |> coerce
        | Array [| hd; tl |] -> aux (to_js ty hd :: acc) tl
        | _ -> assert false
      in
      aux [] t
  | Tuple tys, Array a ->
      a |> Array.map2 to_js (Array.of_list tys) |> Js.array |> coerce
  | Dict (ty, _), Dict m ->
      m
      |> MapString.map (to_js ty)
      |> MapString.to_seq |> Array.of_seq |> Js.Unsafe.obj
  | Record tys, Dict m ->
      record_to_js !tys m |> Array.of_list |> Js.Unsafe.obj
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
        | Const (`String s, _) -> Js.string s |> coerce
        | Const (`Int 0, `Extra_bool) -> Js._false |> coerce
        | Const (`Int _, `Extra_bool) -> Js._true |> coerce
        | Const (`Int i, `Extra_none) ->
            i |> float_of_int |> Js.number_of_float |> coerce
        | _ -> assert false
      in
      let l = record_to_js !record_tys m in
      (k, tag) :: l |> Array.of_list |> Js.Unsafe.obj
  | _ -> assert false

let encode tys j = record_to_js tys j |> Array.of_list |> Js.Unsafe.obj
