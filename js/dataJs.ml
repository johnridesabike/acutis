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
open Js_of_ocaml
module Ty = Typescheme
module EPath = Error.DecodePath

type t = Js.Unsafe.any

let stringify (j : t) =
  match Js.to_string (Js.typeof j) with
  | "undefined" -> "undefined"
  | _ -> Js._JSON##stringify j |> Js.to_string

let pp ppf j = Format.pp_print_string ppf (stringify j)
let decode_error = Error.decode pp

module Table : sig
  (** When we convert JavaScript objects into Acutis records, we cannot convert
      every value at once, e.g. by mapping the result of [Js.object_keys].

      JavaScript objects may use getter functions for accessing values, and
      those can cause side-effects.

      By only accessing the precise values that we need, we avoid triggering
      unexpected behavior. *)

  type t

  val unsafe_of_js : Js.Unsafe.any -> t
  val find_opt : string -> t -> Js.Unsafe.any option
  val fold : (string -> Js.Unsafe.any -> 'a -> 'a) -> t -> 'a -> 'a
end = struct
  type t = Js.Unsafe.any

  let unsafe_of_js j = j
  let get : t -> Js.js_string Js.t -> Js.Unsafe.any Js.Optdef.t = Js.Unsafe.get
  let unsafe_get : t -> Js.js_string Js.t -> Js.Unsafe.any = Js.Unsafe.get
  let find_opt k m = get m (Js.string k) |> Js.Optdef.to_option

  let fold f m init =
    Js.object_keys m |> Js.to_array
    |> Array.fold_left
         (fun init k -> f (Js.to_string k) (unsafe_get m k) init)
         init
end

type tagged =
  | Js_null
  | Js_bool of bool
  | Js_string of string
  | Js_float of float
  | Js_object of Table.t
  | Js_array of t array

let classify j =
  match Js.to_string (Js.typeof j) with
  | "string" -> Js_string (Js.Unsafe.coerce j |> Js.to_string)
  | "number" -> Js_float (Js.Unsafe.coerce j |> Js.float_of_number)
  | "boolean" -> Js_bool (Js.Unsafe.coerce j |> Js.to_bool)
  | "undefined" -> Js_null
  | _ -> (
      match Js.Opt.to_option (Js.some j) with
      | None -> Js_null
      | Some j ->
          if Js.instanceof j Js.array_empty then
            Js_array (Js.Unsafe.coerce j |> Js.to_array)
          else Js_object (Table.unsafe_of_js j))

let boolean ty path cases j =
  match classify j with
  | Js_bool b ->
      let i = match b with false -> 0 | true -> 1 in
      if Set.Int.mem i cases then Data.bool i else Error.bad_enum pp ty path j
  | _ -> decode_error ty path j

let string ty path cases j =
  match classify j with
  | Js_string s -> (
      match cases with
      | None -> Data.string s
      | Some cases ->
          if Set.String.mem s cases then Data.string s
          else Error.bad_enum pp ty path j)
  | _ -> decode_error ty path j

let int ty path cases j =
  match classify j with
  | Js_float f -> (
      let i = int_of_float f in
      match cases with
      | None -> Data.int i
      | Some cases ->
          if Set.Int.mem i cases then Data.int i
          else Error.bad_enum pp ty path j)
  | _ -> decode_error ty path j

let float path j =
  match classify j with
  | Js_float f -> Data.float f
  | _ -> decode_error (Ty.float ()) path j

let rec nullable path ty j =
  match classify j with
  | Js_null -> Data.null
  | _ -> Data.some (make (EPath.nullable path) ty j)

and list path ty j =
  match classify j with
  | Js_array a ->
      let len = Array.length a in
      let l = ref Data.list_empty in
      for i = 0 to len - 1 do
        let x = Array.unsafe_get a i in
        l := Data.list_cons (make (EPath.index i path) ty x) !l
      done;
      Data.list_rev !l
  | _ -> decode_error (Ty.list ty) path j

and dict path ty j =
  match classify j with
  | Js_object o ->
      Table.fold
        (fun k v map -> Map.String.add k (make (EPath.key k path) ty v) map)
        o Map.String.empty
      |> Data.dict
  | _ -> decode_error (Ty.dict ty) path j

and tuple ty path tys j =
  match classify j with
  | Js_array a ->
      let tys = Array.of_list tys in
      if Array.length a = Array.length tys then
        Array.map2 (fun ty x -> (ty, x)) tys a
        |> Array.mapi (fun i (ty, x) -> make (EPath.index i path) ty x)
        |> Data.tuple
      else decode_error ty path j
  | _ -> decode_error ty path j

and record_aux path tys j =
  Map.String.mapi
    (fun k ty ->
      match (ty, Table.find_opt k j) with
      | { contents = Ty.Nullable _ | Unknown _ }, None -> Data.null
      | ty, Some j -> make (EPath.key k path) ty j
      | _ -> Error.missing_key path (Ty.internal_record (ref tys)) k)
    tys

and record path tys j =
  match classify j with
  | Js_object o -> record_aux path !tys o |> Data.dict
  | _ -> decode_error (Ty.internal_record tys) path j

and union path ty key Ty.{ cases; extra; row } j =
  match classify j with
  | Js_object o -> (
      let tag =
        match Table.find_opt key o with
        | None -> decode_error ty path j
        | Some tag -> tag
      in
      let tag, tys =
        match (classify tag, cases, extra) with
        | Js_bool false, Ty.Union.Int map, Bool ->
            let tag = 0 in
            (Data.bool tag, Map.Int.find_opt tag map)
        | Js_bool true, Ty.Union.Int map, Bool ->
            let tag = 1 in
            (Data.bool tag, Map.Int.find_opt tag map)
        | Js_float tag, Ty.Union.Int map, Not_bool ->
            let tag = int_of_float tag in
            (Data.int tag, Map.Int.find_opt tag map)
        | Js_string tag, Ty.Union.String map, Not_bool ->
            (Data.string tag, Map.String.find_opt tag map)
        | _ -> decode_error ty path j
      in
      match (tys, row) with
      | Some tys, (`Open | `Closed) ->
          record_aux path !tys o |> Map.String.add key tag |> Data.dict
      | None, `Open -> Map.String.singleton key tag |> Data.dict
      | None, `Closed -> decode_error ty path j)
  | _ -> decode_error ty path j

and make path ty j =
  match !ty with
  | Ty.Unknown _ -> Data.other j
  | Nullable ty -> nullable path ty j
  | Enum { extra = Bool; cases = Int cases; _ } -> boolean ty path cases j
  | String | Enum { row = `Open; cases = String _; _ } -> string ty path None j
  | Enum { row = `Closed; cases = String cases; _ } ->
      string ty path (Some cases) j
  | Int | Enum { row = `Open; cases = Int _; _ } -> int ty path None j
  | Enum { row = `Closed; cases = Int cases; _ } -> int ty path (Some cases) j
  | Float -> float path j
  | List ty -> list path ty j
  | Dict (ty, _) -> dict path ty j
  | Tuple tys -> tuple ty path tys j
  | Record tys -> record path tys j
  | Union (key, variant) -> union path ty key variant j

let decode ~name tys j =
  let path = EPath.make name in
  match classify j with
  | Js_object o -> record_aux path tys o
  | _ -> decode_error (Ty.internal_record (ref tys)) path j

let coerce = Js.Unsafe.coerce

let rec record_to_js ty t =
  Map.String.merge
    (fun _ ty t ->
      match (ty, t) with Some ty, Some t -> Some (to_js ty t) | _ -> None)
    ty t
  |> Map.String.bindings

and to_js ty t =
  match (!ty, t) with
  | _, Data.Other j -> j
  | _, Const (Float f) -> Js.number_of_float f |> coerce
  | _, Const (String s) -> Js.string s |> coerce
  | Ty.Enum { extra = Bool; _ }, Const (Int 0) -> coerce Js._false
  | Ty.Enum { extra = Bool; _ }, Const (Int _) -> coerce Js._true
  | (Enum _ | Int | Unknown _), Const (Int i) ->
      float_of_int i |> Js.number_of_float |> coerce
  | (Nullable _ | Unknown _), Nil -> Js.Unsafe.inject Js.null
  | Nullable ty, Array [| t |] -> to_js ty t
  | List ty, t ->
      let rec aux acc = function
        | Data.Nil -> List.rev acc |> Array.of_list |> Js.array |> coerce
        | Array [| hd; tl |] -> aux (to_js ty hd :: acc) tl
        | _ -> Error.internal __POS__ "Lists may only contain Array or Nil."
      in
      aux [] t
  | Tuple tys, Array a ->
      Array.map2 to_js (Array.of_list tys) a |> Js.array |> coerce
  | Unknown _, Array a -> Array.map (to_js ty) a |> Js.array |> coerce
  | Dict (ty, _), Dict m ->
      Map.String.map (to_js ty) m
      |> Map.String.to_seq |> Array.of_seq |> Js.Unsafe.obj
  | Unknown _, Dict m ->
      Map.String.map (to_js ty) m
      |> Map.String.to_seq |> Array.of_seq |> Js.Unsafe.obj
  | Record tys, Dict m -> record_to_js !tys m |> Array.of_list |> Js.Unsafe.obj
  | Union (k, { cases; extra; _ }), Dict m ->
      let tag = Map.String.find k m in
      let record_tys =
        match (cases, tag) with
        | Ty.Union.Int m, Const (Int i) -> Map.Int.find i m
        | Ty.Union.String m, Const (String s) -> Map.String.find s m
        | _ -> Error.internal __POS__ "Type mismatch while encoding a union."
      in
      let tag =
        match tag with
        | Const (String s) -> coerce @@ Js.string s
        | Const (Int i) -> (
            match (extra, i) with
            | Bool, 0 -> coerce Js._false
            | Bool, _ -> coerce Js._true
            | Not_bool, i -> float_of_int i |> Js.number_of_float |> coerce)
        | _ -> Error.internal __POS__ "Union tags may only be ints or strings."
      in
      let l = record_to_js !record_tys m in
      (k, tag) :: l |> Array.of_list |> Js.Unsafe.obj
  | ( ( String | Int | Float | Enum _ | Nullable _ | Tuple _ | Dict _ | Record _
      | Union _ ),
      _ ) ->
      Error.internal __POS__ "Type mismatch while encoding data."

let encode tys j = record_to_js tys j |> Array.of_list |> Js.Unsafe.obj
