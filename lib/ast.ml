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
  [@@deriving eq]

  let pp_tag ppf = function
    | Tag_int (_, i) -> Format.pp_print_int ppf i
    | Tag_bool (_, 0) -> Format.pp_print_string ppf "false"
    | Tag_bool _ -> Format.pp_print_string ppf "true"
    | Tag_string (_, s) -> Format.fprintf ppf "%S" s

  type 'a t = Untagged of 'a Dict.t | Tagged of string * tag * 'a Dict.t
  [@@deriving show, eq]

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
  type row = [ `Closed | `Open ] [@@deriving show, eq]

  type ty =
    | Named of Loc.t * string
    | Nullable of ty
    | List of ty
    | Dict of ty
    | Enum_int of int Nonempty.t * row
    | Enum_bool of int Nonempty.t
    | Enum_string of string Nonempty.t * row
    | Record of (Loc.t * ty Record.t) Nonempty.t * row
    | Tuple of ty list
  [@@deriving show, eq]

  type t =
    | Type of Loc.t * string * ty
    | Child of Loc.t * string
    | Child_nullable of Loc.t * string
  [@@deriving show, eq]
end

module Pattern = struct
  type t =
    | Var of Loc.t * string
    | Bool of Loc.t * int
    | Int of Loc.t * int
    | Float of Loc.t * float
    | String of Loc.t * string
    | Nullable of Loc.t * t option
    | Enum_string of Loc.t * string
    | Enum_int of Loc.t * int
    | List of Loc.t * t list * t option
    | Tuple of Loc.t * t list
    | Record of Loc.t * t Record.t
    | Dict of Loc.t * t Dict.t
  [@@deriving show, eq]
end

type trim = No_trim | Trim [@@deriving show, eq]
type escape = No_escape | Escape [@@deriving show, eq]

type echo =
  | Ech_var of Loc.t * string * escape
  | Ech_component of Loc.t * string
  | Ech_string of Loc.t * string
[@@deriving show, eq]

type node =
  | Text of string * trim * trim
  | Echo of echo list * echo
  | Match of Loc.t * Pattern.t Nonempty.t * case Nonempty.t
  | Map_list of Loc.t * Pattern.t * case Nonempty.t
  | Map_dict of Loc.t * Pattern.t * case Nonempty.t
  | Component of Loc.t * string * string * Pattern.t Dict.t * child Dict.t
  | Interface of Loc.t * Interface.t list

and case = { pats : (Loc.t * Pattern.t Nonempty.t) Nonempty.t; nodes : t }
and child = Child_name of Loc.t * string | Child_block of t
and t = node list [@@deriving show, eq]
