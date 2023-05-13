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

let map_of_list l =
  List.fold_left (fun map (k, v) -> Map.String.add k v map) Map.String.empty l

let int_map_of_list l =
  List.fold_left (fun map (k, v) -> Map.Int.add k v map) Map.Int.empty l

module Variant = struct
  type row = [ `Closed | `Open ]
  type extra = Not_bool | Bool
  type ('a, 'b) ty = VInt of 'a | VString of 'b

  type ('a, 'b) t = {
    mutable cases : ('a, 'b) ty;
    mutable row : row;
    extra : extra;
  }

  let equal_row (a : row) (b : row) =
    match (a, b) with `Closed, `Closed | `Open, `Open -> true | _ -> false

  let equal_extra a b =
    match (a, b) with Not_bool, Not_bool | Bool, Bool -> true | _ -> false

  let pp_sep ppf () = F.fprintf ppf " |@ "
  let pp_row ppf = function `Closed -> () | `Open -> F.fprintf ppf " |@ ..."

  let pp_bool ppf = function
    | 0 -> F.pp_print_string ppf "false"
    | _ -> F.pp_print_string ppf "true"

  let pp ppf pp_case cases row =
    F.fprintf ppf "@[<hv 0>";
    F.pp_print_seq ~pp_sep pp_case ppf cases;
    pp_row ppf row;
    F.fprintf ppf "@]"

  let row_to_sexp = function
    | `Closed -> Sexp.symbol "closed"
    | `Open -> Sexp.symbol "open"
end

module Enum = struct
  open Variant

  type t = (Set.Int.t, Set.String.t) Variant.t

  let string l row =
    { cases = VString (Set.String.of_list l); row; extra = Not_bool }

  let string_singleton s row =
    { cases = VString (Set.String.singleton s); row; extra = Not_bool }

  let int l row = { cases = VInt (Set.Int.of_list l); row; extra = Not_bool }

  let int_singleton i row =
    { cases = VInt (Set.Int.singleton i); row; extra = Not_bool }

  let false_and_true_cases = VInt (Set.Int.of_list [ 0; 1 ])
  let false_only = VInt (Set.Int.singleton 0)
  let true_only = VInt (Set.Int.singleton 1)

  let false_and_true () =
    { cases = false_and_true_cases; row = `Closed; extra = Bool }

  let true_only () = { cases = true_only; row = `Closed; extra = Bool }
  let false_only () = { cases = false_only; row = `Closed; extra = Bool }

  let bool_of_list l =
    { cases = VInt (Set.Int.of_list l); row = `Closed; extra = Bool }

  let pp_string ppf s = F.fprintf ppf "%@%S" s
  let pp_int ppf i = F.fprintf ppf "%@%i" i
end

module Union = struct
  open Variant

  type 'a t =
    ('a Map.String.t ref Map.Int.t, 'a Map.String.t ref Map.String.t) Variant.t

  let int_singleton i x row extra =
    { cases = VInt (Map.Int.singleton i x); row; extra }

  let string_singleton s x row =
    { cases = VString (Map.String.singleton s x); row; extra = Not_bool }

  let string l row = { cases = VString (map_of_list l); row; extra = Not_bool }
  let int l row = { cases = VInt (int_map_of_list l); row; extra = Not_bool }

  let boolean ~f ~t =
    {
      cases = VInt (Map.Int.singleton 0 f |> Map.Int.add 1 t);
      row = `Closed;
      extra = Bool;
    }

  let false_only l =
    { cases = VInt (Map.Int.singleton 0 l); row = `Closed; extra = Bool }

  let true_only l =
    { cases = VInt (Map.Int.singleton 1 l); row = `Closed; extra = Bool }

  let pp_tag_field ppf s = F.fprintf ppf "%@%a" Pp.field s
end

type ty =
  | Unknown of Variant.row ref
  | Int
  | Float
  | String
  | Nullable of t
  | List of t
  | Tuple of t list
  | Record of t Map.String.t ref
  | Dict of t * Set.String.t ref
  | Enum of Enum.t
  | Union of string * t Union.t

and t = ty ref

let internal_record m = ref (Record m)
let internal_dict_keys t kys = ref (Dict (t, kys))
let internal_bool l = ref (Enum (Enum.bool_of_list l))
let unknown () = ref (Unknown (ref `Closed))
let int () = ref Int
let float () = ref Float
let string () = ref String
let nullable t = ref (Nullable t)
let list t = ref (List t)
let tuple l = ref (Tuple l)
let record l = internal_record (ref (map_of_list l))
let dict t = ref (Dict (t, ref Set.String.empty))
let enum_int row l = ref (Enum (Enum.int l row))
let enum_string row l = ref (Enum (Enum.string l row))
let boolean () = ref (Enum (Enum.false_and_true ()))
let false_only () = ref (Enum (Enum.false_only ()))
let true_only () = ref (Enum (Enum.true_only ()))
let nested_lists_to_map l = List.map (fun (k, v) -> (k, ref (map_of_list v))) l
let union_int row k l = ref (Union (k, Union.int (nested_lists_to_map l) row))

let union_string row k l =
  ref (Union (k, Union.string (nested_lists_to_map l) row))

let union_boolean k ~f ~t =
  ref
    (Union (k, Union.boolean ~f:(ref (map_of_list f)) ~t:(ref (map_of_list t))))

let union_false_only k l =
  ref (Union (k, Union.false_only (ref (map_of_list l))))

let union_true_only k l = ref (Union (k, Union.true_only (ref (map_of_list l))))
let make = map_of_list
let empty = Map.String.empty

let rec copy = function
  | (Int | Float | String) as x -> x
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
        | VString m ->
            Variant.VString
              (Map.String.map (fun r -> ref (internal_copy_record !r)) m)
        | VInt m ->
            VInt (Map.Int.map (fun r -> ref (internal_copy_record !r)) m)
      in
      Union (tag, { cases; row; extra })

and copy_ref r = ref (copy !r)
and internal_copy_record m = Map.String.map copy_ref m

let surround ~left ~right ppf f x =
  F.fprintf ppf "@[<hv>%c@[<hv 2>@,%a@]@,%c@]" left f x right

let pp_record =
  let field pp_k pp_v ppf (k, v) =
    F.fprintf ppf "@[<hv 2>%a:@ %a@]" pp_k k pp_v v
  in
  let record ?tag pp ppf m =
    (match tag with
    | None -> ()
    | Some (pp_tag, tag) ->
        field Union.pp_tag_field pp_tag ppf tag;
        if not (Map.String.is_empty !m) then Pp.sep_comma ppf ());
    F.pp_print_seq ~pp_sep:Pp.sep_comma (field Pp.field pp) ppf
      (Map.String.to_seq !m)
  in
  fun ?tag pp ppf m -> surround ~left:'{' ~right:'}' ppf (record ?tag pp) m

let rec pp ppf t =
  match !t with
  | Unknown _ -> F.pp_print_string ppf "_"
  | Int -> F.pp_print_string ppf "int"
  | Float -> F.pp_print_string ppf "float"
  | String -> F.pp_print_string ppf "string"
  | Nullable x -> F.fprintf ppf "?@[<hv 1>@,%a@]" pp x
  | List t -> surround ~left:'[' ~right:']' ppf pp t
  | Dict (t, _) -> surround ~left:'<' ~right:'>' ppf pp t
  | Record r -> pp_record pp ppf r
  | Tuple l ->
      surround ~left:'(' ~right:')' ppf
        (F.pp_print_list ~pp_sep:Pp.sep_comma pp)
        l
  | Enum { cases = VString cases; row; _ } ->
      Variant.pp ppf Enum.pp_string (Set.String.to_seq cases) row
  | Enum { cases = VInt cases; extra = Not_bool; row } ->
      Variant.pp ppf Enum.pp_int (Set.Int.to_seq cases) row
  | Enum { cases = VInt cases; extra = Bool; row } ->
      Variant.pp ppf Variant.pp_bool (Set.Int.to_seq cases) row
  | Union (key, union) -> (
      let aux pp_tag ppf (tag, fields) =
        pp_record ~tag:(pp_tag, (key, tag)) pp ppf fields
      in
      match union with
      | { cases = VString cases; row; _ } ->
          Variant.pp ppf (aux Pp.syntax_string) (Map.String.to_seq cases) row
      | { cases = VInt cases; extra = Not_bool; row } ->
          Variant.pp ppf (aux F.pp_print_int) (Map.Int.to_seq cases) row
      | { cases = VInt cases; extra = Bool; row } ->
          Variant.pp ppf (aux Variant.pp_bool) (Map.Int.to_seq cases) row)

let pp_interface =
  let equals ppf (k, v) = F.fprintf ppf "@[<hv 2>%a =@ %a@]" Pp.field k pp v in
  fun ppf m ->
    F.fprintf ppf "@[<v>%a@]"
      (F.pp_print_seq ~pp_sep:F.pp_print_space equals)
      (Map.String.to_seq m)
