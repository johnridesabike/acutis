(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open Utils
module F = Format

module Variant = struct
  type row = Closed | Open [@@deriving eq]
  type extra = Extra_none | Extra_bool [@@deriving eq]
  type ('a, 'b) ty = Int of 'a | String of 'b [@@deriving eq]

  type ('a, 'b) t = {
    mutable cases : ('a, 'b) ty;
    mutable row : row;
    extra : extra;
  }
  [@@deriving eq]

  let pp_row fmt = function
    | Closed -> F.fprintf fmt ""
    | Open -> F.fprintf fmt " | ..."

  let pp_bool fmt = function
    | 0 -> F.fprintf fmt "false"
    | _ -> F.fprintf fmt "true"

  let pp_types fmt = function
    | `String s -> F.fprintf fmt "%S" s
    | `Int i -> F.fprintf fmt "%i" i
    | `Bool b -> pp_bool fmt b
end

module Enum = struct
  type t = (SetInt.t, SetString.t) Variant.t [@@deriving eq]

  let string l row =
    { Variant.cases = String (SetString.of_list l); row; extra = Extra_none }

  let string_singleton s row =
    { Variant.cases = String (SetString.singleton s); row; extra = Extra_none }

  let int l row =
    { Variant.cases = Int (SetInt.of_list l); row; extra = Extra_none }

  let int_singleton i row =
    { Variant.cases = Int (SetInt.singleton i); row; extra = Extra_none }

  let false_and_true_cases = Variant.Int (SetInt.of_list [ 0; 1 ])
  let false_only = Variant.Int (SetInt.singleton 0)
  let true_only = Variant.Int (SetInt.singleton 1)

  let false_and_true () =
    { Variant.cases = false_and_true_cases; row = Closed; extra = Extra_bool }

  let true_only () =
    { Variant.cases = true_only; row = Closed; extra = Extra_bool }

  let false_only () =
    { Variant.cases = false_only; row = Closed; extra = Extra_bool }
end

module Union = struct
  type 'a t =
    ('a MapString.t ref MapInt.t, 'a MapString.t ref MapString.t) Variant.t
  [@@deriving eq]

  let string l row =
    {
      Variant.cases = String (MapString.of_seq (List.to_seq l));
      row;
      extra = Extra_none;
    }

  let int l row =
    {
      Variant.cases = Int (MapInt.of_seq (List.to_seq l));
      row;
      extra = Extra_none;
    }

  let boolean ~f ~t row =
    {
      Variant.cases = Int (MapInt.singleton 0 f |> MapInt.add 1 t);
      row;
      extra = Extra_bool;
    }
end

type ty' =
  | Unknown
  | Int
  | Float
  | String
  | Echo
  | Nullable of ty
  | List of ty
  | Tuple of ty list
  | Record of ty MapString.t ref
  | Dict of ty * SetString.t ref
  | Enum of Enum.t
  | Union of string * ty Union.t
[@@deriving eq]

and ty = ty' ref [@@deriving eq]

type t = ty MapString.t [@@deriving eq]

let unknown () = ref Unknown
let int () = ref Int
let float () = ref Float
let string () = ref String
let echo () = ref Echo
let nullable t = ref (Nullable t)
let list t = ref (List t)
let tuple l = ref (Tuple l)
let record l = ref (Record (ref (MapString.of_seq (List.to_seq l))))
let dict t = ref (Dict (t, ref SetString.empty))
let enum_int l = ref (Enum (Enum.int l Closed))
let enum_string l = ref (Enum (Enum.string l Closed))
let bool () = ref (Enum (Enum.false_and_true ()))

let nested_lists_to_map l =
  List.map (fun (k, v) -> (k, ref (MapString.of_seq (List.to_seq v)))) l

let union_int k l = ref (Union (k, Union.int (nested_lists_to_map l) Closed))

let union_string k l =
  ref (Union (k, Union.string (nested_lists_to_map l) Closed))

let union_boolean ~f ~t k =
  ref
    (Union
       ( k,
         Union.boolean
           ~f:(ref (MapString.of_seq (List.to_seq f)))
           ~t:(ref (MapString.of_seq (List.to_seq t)))
           Closed ))

let make l = l |> List.to_seq |> MapString.of_seq
let internal_record m = ref (Record m)
let internal_dict_keys t kys = ref (Dict (t, kys))

let rec copy = function
  | (Unknown | Int | Float | String | Echo) as x -> x
  | Enum { cases; row; extra } -> Enum { cases; row; extra }
  | Nullable r -> Nullable (copy_ref r)
  | List r -> List (copy_ref r)
  | Dict (r, fixme) -> Dict (copy_ref r, fixme)
  | Tuple l -> Tuple (List.map copy_ref l)
  | Record r -> Record (ref (internal_copy_record !r))
  | Union (tag, { cases; row; extra }) ->
      let cases =
        match cases with
        | String m ->
            Variant.String
              (MapString.map (fun r -> ref (internal_copy_record !r)) m)
        | Int m -> Int (MapInt.map (fun r -> ref (internal_copy_record !r)) m)
      in
      Union (tag, { cases; row; extra })

and copy_ref r = ref (copy !r)
and internal_copy_record m = MapString.map copy_ref m

let pp_sep_comma fmt () = F.fprintf fmt ",@ "
let pp_sep_pipe fmt () = F.fprintf fmt "@ |"

let rec pp_ty fmt t =
  match !t with
  | Unknown -> F.fprintf fmt "_"
  | Int -> F.fprintf fmt "int"
  | Float -> F.fprintf fmt "float"
  | String -> F.fprintf fmt "string"
  | Echo -> F.fprintf fmt "echoable"
  | Enum { cases; row; extra } -> (
      match cases with
      | String cases ->
          F.fprintf fmt "%a%a"
            (F.pp_print_list ~pp_sep:pp_sep_pipe (fun fmt s ->
                 F.fprintf fmt "%@%S" s))
            (SetString.elements cases) Variant.pp_row row
      | Int cases -> (
          match extra with
          | Extra_none ->
              F.fprintf fmt "%a%a"
                (F.pp_print_list ~pp_sep:pp_sep_pipe (fun fmt i ->
                     F.fprintf fmt "%@%i" i))
                (SetInt.elements cases) Variant.pp_row row
          | Extra_bool ->
              F.fprintf fmt "%a%a"
                (F.pp_print_list ~pp_sep:pp_sep_pipe Variant.pp_bool)
                (SetInt.elements cases) Variant.pp_row row))
  | Nullable x ->
      F.fprintf fmt "?";
      pp_ty fmt x
  | List l -> F.fprintf fmt "[%a]" pp_ty l
  | Dict (t, _) -> F.fprintf fmt "<%a>" pp_ty t
  | Tuple l ->
      F.fprintf fmt "(%a)" (F.pp_print_list ~pp_sep:pp_sep_comma pp_ty) l
  | Record r -> F.fprintf fmt "{%a}" pp_record_rows (MapString.bindings !r)
  | Union (key, { cases; extra; row }) ->
      let cases =
        match cases with
        | String m ->
            m |> MapString.bindings
            |> List.map (fun (tag, m) -> (`String tag, m))
        | Int m -> (
            match extra with
            | Extra_none ->
                m |> MapInt.bindings |> List.map (fun (tag, m) -> (`Int tag, m))
            | Extra_bool ->
                m |> MapInt.bindings
                |> List.map (fun (tag, m) -> (`Bool tag, m)))
      in
      F.fprintf fmt "%a%a"
        (F.pp_print_list ~pp_sep:pp_sep_pipe (pp_union_cases key))
        cases Variant.pp_row row

and pp_union_cases key fmt (tag, m) =
  let rows = MapString.bindings !m in
  F.fprintf fmt "{@%S: %a %a}" key Variant.pp_types tag pp_record_rows rows

and pp_record_rows fmt l =
  F.pp_print_list ~pp_sep:pp_sep_comma
    (fun fmt (k, v) -> F.fprintf fmt "%S: %a" k pp_ty v)
    fmt l

let show_ty ty =
  pp_ty F.str_formatter ty;
  F.flush_str_formatter ()

module Child = struct
  type ty' = Child | NullableChild [@@deriving eq, show]
  type ty = ty' ref [@@deriving eq, show]
  type t = ty MapString.t [@@deriving eq]

  let make l = l |> List.to_seq |> MapString.of_seq
  let child x = (x, ref Child)
  let nullable x = (x, ref NullableChild)
  let is_nullable t = match !t with NullableChild -> true | Child -> false
end
