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

module Dict = struct
  type 'a t = 'a MapString.t

  let fail_if_dup_key k m = assert (not (MapString.mem k m))

  let add k v m =
    fail_if_dup_key k m;
    MapString.add k v m

  let empty = MapString.empty
  let singleton = MapString.singleton
  let equal = MapString.equal
  let to_map m = m
  let pp_sep_comma ppf () = F.fprintf ppf ",@ "
  let pp_binding pp_v ppf (k, v) = F.fprintf ppf "@[%S:@ %a@]" k pp_v v

  let pp pp_v ppf m =
    F.fprintf ppf "dict{@[%a@]}"
      (F.pp_print_list ~pp_sep:pp_sep_comma (pp_binding pp_v))
      (MapString.bindings m)
end

module Record = struct
  type 'a t = Tagged of string * 'a * 'a Dict.t | Untagged of 'a Dict.t
  [@@deriving show, eq]

  let add tag k v = function
    | Tagged (k', v', m) ->
        assert (not tag);
        assert (not (String.equal k k'));
        Tagged (k', v', Dict.add k v m)
    | Untagged m ->
        if tag then (
          Dict.fail_if_dup_key k m;
          Tagged (k, v, m))
        else Untagged (Dict.add k v m)

  let singleton tag k v =
    if tag then Tagged (k, v, Dict.empty) else Untagged (Dict.singleton k v)
end

module Pattern = struct
  type t =
    | Var of string
    | Bool of int
    | Int of int
    | Float of float
    | String of string
    | Nullable of t option
    | Enum_string of string
    | Enum_int of int
    | List of t list * t option
    | Tuple of t list
    | Record of t Record.t
    | Dict of t Dict.t
  [@@deriving show, eq]
end

type trim = No_trim | Trim [@@deriving show, eq]
type escape = No_escape | Escape [@@deriving show, eq]

type echo =
  | Ech_var of string * escape
  | Ech_string of string
  | Ech_component of string
[@@deriving show, eq]

type node =
  | Text of string * trim * trim
  | Echo of echo list * echo
  | Match of Pattern.t Nonempty.t * case Nonempty.t
  | Map_list of Pattern.t * case Nonempty.t
  | Map_dict of Pattern.t * case Nonempty.t
  | Component of string * Pattern.t Dict.t * child Dict.t

and case = { pats : Pattern.t Nonempty.t Nonempty.t; nodes : t }
and child = Child_name of string | Child_block of t
and t = node list [@@deriving show, eq]
