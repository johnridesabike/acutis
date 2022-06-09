(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

type loc = Error.loc

val dummy_loc : loc

module Dict : sig
  type 'a t

  val empty : _ t
  val add : loc -> string -> 'a -> 'a t -> 'a t
  val singleton : string -> 'a -> 'a t
  val to_map : 'a t -> 'a StdlibExtra.MapString.t
end

module Record : sig
  type 'a t = Untagged of 'a Dict.t | Tagged of string * 'a * 'a Dict.t

  val add : loc -> [ `Tag | `Notag ] -> string -> 'a -> 'a t -> 'a t
  val singleton : [ `Tag | `Notag ] -> string -> 'a -> 'a t
end

module Pattern : sig
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
end

type trim = No_trim | Trim
type escape = No_escape | Escape

type echo =
  | Ech_var of loc * string * escape
  | Ech_component of loc * string
  | Ech_string of loc * string

type node =
  | Text of string * trim * trim
  | Echo of echo list * echo
  | Match of loc * Pattern.t Nonempty.t * case Nonempty.t
  | Map_list of loc * Pattern.t * case Nonempty.t
  | Map_dict of loc * Pattern.t * case Nonempty.t
  | Component of loc * string * string * Pattern.t Dict.t * child Dict.t

and case = { pats : (loc * Pattern.t Nonempty.t) Nonempty.t; nodes : t }
and child = Child_name of loc * string | Child_block of t
and t = node list

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
