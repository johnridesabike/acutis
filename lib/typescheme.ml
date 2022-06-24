(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module F = Format

module Variant = struct
  type row = [ `Closed | `Open ] [@@deriving eq]
  type extra = [ `Extra_none | `Extra_bool ] [@@deriving eq, show]
  type ('a, 'b) ty = Int of 'a | String of 'b [@@deriving eq]

  type ('a, 'b) t = {
    mutable cases : ('a, 'b) ty;
    mutable row : row;
    extra : extra;
  }
  [@@deriving eq]

  let pp_row ppf = function
    | `Closed -> F.fprintf ppf ""
    | `Open -> F.fprintf ppf "@ @[| ...@]"

  let pp_bool ppf = function
    | 0 -> F.fprintf ppf "false"
    | _ -> F.fprintf ppf "true"

  let pp_types ppf = function
    | `String s -> F.fprintf ppf "%S" s
    | `Int i -> F.fprintf ppf "%i" i
    | `Bool b -> pp_bool ppf b
end

module Enum = struct
  open Variant

  type t = (Set.Int.t, Set.String.t) Variant.t [@@deriving eq]

  let string l row =
    { cases = String (Set.String.of_list l); row; extra = `Extra_none }

  let string_singleton s row =
    { cases = String (Set.String.singleton s); row; extra = `Extra_none }

  let int l row = { cases = Int (Set.Int.of_list l); row; extra = `Extra_none }

  let int_singleton i row =
    { cases = Int (Set.Int.singleton i); row; extra = `Extra_none }

  let false_and_true_cases = Int (Set.Int.of_list [ 0; 1 ])
  let false_only = Int (Set.Int.singleton 0)
  let true_only = Int (Set.Int.singleton 1)

  let false_and_true () =
    { cases = false_and_true_cases; row = `Closed; extra = `Extra_bool }

  let true_only () = { cases = true_only; row = `Closed; extra = `Extra_bool }
  let false_only () = { cases = false_only; row = `Closed; extra = `Extra_bool }
end

module Union = struct
  open Variant

  type 'a t =
    ('a Map.String.t ref Map.Int.t, 'a Map.String.t ref Map.String.t) Variant.t
  [@@deriving eq]

  let int_singleton i x row extra =
    { cases = Int (Map.Int.singleton i x); row; extra }

  let string_singleton s x row =
    { cases = String (Map.String.singleton s x); row; extra = `Extra_none }

  let string l row =
    {
      cases = String (Map.String.of_seq (List.to_seq l));
      row;
      extra = `Extra_none;
    }

  let int l row =
    { cases = Int (Map.Int.of_seq (List.to_seq l)); row; extra = `Extra_none }

  let boolean ~f ~t =
    {
      cases = Int (Map.Int.singleton 0 f |> Map.Int.add 1 t);
      row = `Closed;
      extra = `Extra_bool;
    }

  let false_only l =
    { cases = Int (Map.Int.singleton 0 l); row = `Closed; extra = `Extra_bool }

  let true_only l =
    { cases = Int (Map.Int.singleton 1 l); row = `Closed; extra = `Extra_bool }
end

type ty =
  | Unknown of Variant.row ref
  | Int
  | Float
  | String
  | Echo
  | Nullable of t
  | List of t
  | Tuple of t list
  | Record of t Map.String.t ref
  | Dict of t * Set.String.t ref
  | Enum of Enum.t
  | Union of string * t Union.t
[@@deriving eq]

and t = ty ref [@@deriving eq]

let internal_record m = ref (Record m)
let internal_dict_keys t kys = ref (Dict (t, kys))
let unknown () = ref (Unknown (ref `Closed))
let int () = ref Int
let float () = ref Float
let string () = ref String
let echo () = ref Echo
let nullable t = ref (Nullable t)
let list t = ref (List t)
let tuple l = ref (Tuple l)
let record l = internal_record (ref (Map.String.of_seq (List.to_seq l)))
let dict t = ref (Dict (t, ref Set.String.empty))
let enum_int row l = ref (Enum (Enum.int l row))
let enum_string row l = ref (Enum (Enum.string l row))
let boolean () = ref (Enum (Enum.false_and_true ()))
let false_only () = ref (Enum (Enum.false_only ()))
let true_only () = ref (Enum (Enum.true_only ()))

let nested_lists_to_map l =
  List.map (fun (k, v) -> (k, ref (Map.String.of_seq (List.to_seq v)))) l

let union_int row k l = ref (Union (k, Union.int (nested_lists_to_map l) row))

let union_string row k l =
  ref (Union (k, Union.string (nested_lists_to_map l) row))

let union_boolean k ~f ~t =
  ref
    (Union
       ( k,
         Union.boolean
           ~f:(ref (Map.String.of_seq (List.to_seq f)))
           ~t:(ref (Map.String.of_seq (List.to_seq t))) ))

let union_false_only k l =
  ref (Union (k, Union.false_only (ref (Map.String.of_seq (List.to_seq l)))))

let union_true_only k l =
  ref (Union (k, Union.true_only (ref (Map.String.of_seq (List.to_seq l)))))

let make l = l |> List.to_seq |> Map.String.of_seq
let empty = Map.String.empty

let rec copy = function
  | (Int | Float | String | Echo) as x -> x
  | Unknown r -> Unknown (ref !r)
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
              (Map.String.map (fun r -> ref (internal_copy_record !r)) m)
        | Int m -> Int (Map.Int.map (fun r -> ref (internal_copy_record !r)) m)
      in
      Union (tag, { cases; row; extra })

and copy_ref r = ref (copy !r)
and internal_copy_record m = Map.String.map copy_ref m

let pp_sep_pipe ppf () = F.fprintf ppf "@ | "

let rec pp ppf t =
  match !t with
  | Unknown _ -> F.pp_print_string ppf "_"
  | Int -> F.pp_print_string ppf "int"
  | Float -> F.pp_print_string ppf "float"
  | String -> F.pp_print_string ppf "string"
  | Echo -> F.pp_print_string ppf "echoable"
  | Enum { cases = String cases; row; _ } ->
      F.fprintf ppf "@[| %a@]@[%a@]"
        (F.pp_print_list ~pp_sep:pp_sep_pipe (fun ppf s ->
             F.fprintf ppf "%@%S" s))
        (Set.String.elements cases)
        Variant.pp_row row
  | Enum { cases = Int cases; row; extra = `Extra_none } ->
      (F.fprintf ppf "@[| %a@]@[%a@]"
         (F.pp_print_list ~pp_sep:pp_sep_pipe (fun ppf i ->
              F.fprintf ppf "%@%i" i)))
        (Set.Int.elements cases) Variant.pp_row row
  | Enum { cases = Int cases; row; extra = `Extra_bool } ->
      F.fprintf ppf "@[| %a@]@[%a@]"
        (F.pp_print_list ~pp_sep:pp_sep_pipe Variant.pp_bool)
        (Set.Int.elements cases) Variant.pp_row row
  | Nullable x -> F.fprintf ppf "?%a" pp x
  | List l -> F.fprintf ppf "[@[@,@[%a@]@,]@]" pp l
  | Dict (t, _) -> F.fprintf ppf "<@[%a@]>" pp t
  | Tuple l ->
      F.fprintf ppf "(@[%a@])" (F.pp_print_list ~pp_sep:Pp.sep_comma pp) l
  | Record r -> pp_record ppf !r
  | Union (key, { cases; extra; row }) ->
      let cases =
        match cases with
        | String m ->
            m |> Map.String.to_seq |> Seq.map (fun (tag, m) -> (`String tag, m))
        | Int m -> (
            match extra with
            | `Extra_none ->
                m |> Map.Int.to_seq |> Seq.map (fun (tag, m) -> (`Int tag, m))
            | `Extra_bool ->
                m |> Map.Int.to_seq |> Seq.map (fun (tag, m) -> (`Bool tag, m)))
      in
      F.fprintf ppf "@[| %a@]@[%a@]"
        (F.pp_print_seq ~pp_sep:pp_sep_pipe (pp_union_cases key))
        cases Variant.pp_row row

and pp_union_cases key ppf (tag, m) =
  F.fprintf ppf "@[{@,@[@@%a:@ %a" Pp.field key Variant.pp_types tag;
  if not (Map.String.is_empty !m) then
    F.fprintf ppf ",@ %a" pp_record_rows (Map.String.to_seq !m);
  F.fprintf ppf "@]@,}@]"

and pp_record_rows ppf l =
  F.pp_print_seq ~pp_sep:Pp.sep_comma
    (fun ppf (k, v) -> F.fprintf ppf "%a:@ @[%a@]" Pp.field k pp v)
    ppf l

and pp_record ppf r =
  F.fprintf ppf "{@[%a@]}" pp_record_rows (Map.String.to_seq r)

module Child = struct
  type ty = Child | Nullable_child
  type t = ty ref

  let equal a b = !a = !b
  let make l = l |> List.to_seq |> Map.String.of_seq
  let child x = (x, ref Child)
  let nullable x = (x, ref Nullable_child)
  let is_nullable t = match !t with Nullable_child -> true | Child -> false

  let pp ppf = function
    | { contents = Child } -> F.pp_print_string ppf "child"
    | { contents = Nullable_child } -> F.pp_print_string ppf "nullable child"

  let empty = Map.String.empty
end
