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
module F = Format

type loc = Error.loc

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)
let pp_loc ppf _ = Format.fprintf ppf "<loc>"
let equal_loc _ _ = true (* Do not use location for equality in testing. *)

module Dict = struct
  type 'a t = 'a MapString.t

  let fail_if_dup_key loc k m =
    if MapString.mem k m then Error.dup_record_key loc k

  let add loc k v m =
    fail_if_dup_key loc k m;
    MapString.add k v m

  let empty = MapString.empty
  let singleton = MapString.singleton
  let equal = MapString.equal
  let pp = MapString.pp
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
    | Var of loc * string
    | Bool of loc * int
    | Int of loc * int
    | Float of loc * float
    | String of loc * string
    | Nullable of loc * t option
    | Enum_string of loc * string
    | Enum_int of loc * int
    | List of loc * t list * t option
    | Tuple of loc * t list
    | Record of loc * t Record.t
    | Dict of loc * t Dict.t
  [@@deriving show, eq]

  (*
  let loc = function
    | Var (l, _)
    | Bool (l, _)
    | Int (l, _)
    | Float (l, _)
    | String (l, _)
    | Nullable (l, _)
    | Enum_string (l, _)
    | Enum_int (l, _)
    | List (l, _, _)
    | Tuple (l, _)
    | Record (l, _)
    | Dict (l, _) ->
        l
        *)
end

type trim = No_trim | Trim [@@deriving show, eq]
type escape = No_escape | Escape [@@deriving show, eq]

type echo =
  | Ech_var of loc * string * escape
  | Ech_component of loc * string
  | Ech_string of loc * string
[@@deriving show, eq]

type node =
  | Text of string * trim * trim
  | Echo of echo list * echo
  | Match of loc * Pattern.t Nonempty.t * case Nonempty.t
  | Map_list of loc * Pattern.t * case Nonempty.t
  | Map_dict of loc * Pattern.t * case Nonempty.t
  | Component of loc * string * string * Pattern.t Dict.t * child Dict.t

and case = { pats : (loc * Pattern.t Nonempty.t) Nonempty.t; nodes : t }
and child = Child_name of loc * string | Child_block of t
and t = node list [@@deriving show, eq]
