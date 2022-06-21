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
    fail_if_dup_key loc k m;
    Map.String.add k v m

  let empty = Map.String.empty
  let singleton = Map.String.singleton
  let equal = Map.String.equal
  let pp = Pp.map_string
  let to_map m = m
end

module Record = struct
  type 'a t = Untagged of 'a Dict.t | Tagged of string * 'a * 'a Dict.t
  [@@deriving show, eq]

  let add loc tag k v m =
    match (tag, m) with
    | `Tag, Tagged _ -> Error.extra_record_tag loc
    | `Notag, Tagged (k', v', m) ->
        if k = k' then Error.dup_record_key loc k;
        Tagged (k', v', Dict.add loc k v m)
    | `Tag, Untagged m ->
        Dict.fail_if_dup_key loc k m;
        Tagged (k, v, m)
    | `Notag, Untagged m -> Untagged (Dict.add loc k v m)

  let singleton tag k v =
    match tag with
    | `Tag -> Tagged (k, v, Dict.empty)
    | `Notag -> Untagged (Dict.singleton k v)
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

and case = { pats : (Loc.t * Pattern.t Nonempty.t) Nonempty.t; nodes : t }
and child = Child_name of Loc.t * string | Child_block of t
and t = node list [@@deriving show, eq]
