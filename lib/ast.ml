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

module Dict = struct
  type 'a t = 'a Map.String.t

  let fail_if_dup_key loc k m =
    if Map.String.mem k m then Error.dup_record_key loc k

  let add loc k v m =
    Map.String.update k
      (function None -> Some v | Some _ -> Error.dup_record_key loc k)
      m

  let empty = Map.String.empty
  let singleton = Map.String.singleton
  let equal = Map.String.equal
  let pp = Pp.map_string
  let to_map m = m
end

module Record = struct
  type tag =
    | Tag_int of Loc.t * int
    | Tag_bool of Loc.t * int
    | Tag_string of Loc.t * string

  let pp_tag ppf = function
    | Tag_int (_, i) -> Format.pp_print_int ppf i
    | Tag_bool (_, 0) -> Format.pp_print_string ppf "false"
    | Tag_bool _ -> Format.pp_print_string ppf "true"
    | Tag_string (_, s) -> Format.fprintf ppf "%S" s

  type 'a t = Untagged of 'a Dict.t | Tagged of string * tag * 'a Dict.t

  let add loc tag m =
    match (tag, m) with
    | `Tag _, Tagged _ -> Error.extra_record_tag loc
    | `Notag (k, v), Tagged (k', v', m) ->
        if k = k' then Error.dup_record_key loc k;
        Tagged (k', v', Dict.add loc k v m)
    | `Tag (k, v), Untagged m ->
        Dict.fail_if_dup_key loc k m;
        Tagged (k, v, m)
    | `Notag (k, v), Untagged m -> Untagged (Dict.add loc k v m)

  let singleton tag =
    match tag with
    | `Tag (k, v) -> Tagged (k, v, Dict.empty)
    | `Notag (k, v) -> Untagged (Dict.singleton k v)
end

module Interface = struct
  type ty =
    | Named of Loc.t * string
    | Nullable of ty
    | List of ty
    | Dict of ty
    | Enum_int of int Nonempty.t * Typescheme.Variant.row
    | Enum_bool of int Nonempty.t
    | Enum_string of string Nonempty.t * Typescheme.Variant.row
    | Record of (Loc.t * ty Record.t) Nonempty.t * Typescheme.Variant.row
    | Tuple of ty list

  type t = { loc : Loc.t; name : string; ty : ty }
end

type trim = No_trim | Trim
type escape = No_escape | Escape
type echo_flag = No_flag | Flag_comma

type echo_format =
  | Fmt_string
  | Fmt_int of echo_flag
  | Fmt_float of int
  | Fmt_float_e of int
  | Fmt_float_g of int
  | Fmt_bool

type echo =
  | Echo_var of Loc.t * string
  | Echo_string of Loc.t * string
  | Echo_field of echo * string

type pat =
  | Var of Loc.t * string
  | Bool of Loc.t * int
  | Int of Loc.t * int
  | Float of Loc.t * float
  | String of Loc.t * string
  | Nullable of Loc.t * pat option
  | Enum_string of Loc.t * string
  | Enum_int of Loc.t * int
  | List of Loc.t * pat list * pat option
  | Tuple of Loc.t * pat list
  | Record of Loc.t * pat Record.t
  | Dict of Loc.t * pat Dict.t
  | Block of Loc.t * t
  | Field of Loc.t * pat * string

and node =
  | Text of string * trim * trim
  | Echo of (echo_format * echo) list * echo_format * echo * escape
  | Match of Loc.t * pat Nonempty.t * case Nonempty.t
  | Map_list of Loc.t * pat * case Nonempty.t
  | Map_dict of Loc.t * pat * case Nonempty.t
  | Component of Loc.t * string * string * pat Dict.t
  | Interface of Loc.t * Interface.t list

and case = { pats : (Loc.t * pat Nonempty.t) Nonempty.t; nodes : t }
and t = node list