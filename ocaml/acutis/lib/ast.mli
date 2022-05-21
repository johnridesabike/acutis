(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module Dict : sig
  type 'a t

  val empty : _ t
  val add : string -> 'a -> 'a t -> 'a t
  val singleton : string -> 'a -> 'a t
  val to_map : 'a t -> 'a Utils.MapString.t
end

module Record : sig
  type 'a t = Tagged of string * 'a * 'a Dict.t | Untagged of 'a Dict.t

  val add : bool -> string -> 'a -> 'a t -> 'a t
  val singleton : bool -> string -> 'a -> 'a t
end

module Pattern : sig
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
end

type trim = No_trim | Trim
type escape = No_escape | Escape

type echo =
  | Ech_var of string * escape
  | Ech_string of string
  | Ech_component of string

type node =
  | Text of string * trim * trim
  | Echo of echo list * echo
  | Match of Pattern.t Nonempty.t * case Nonempty.t
  | Map_list of Pattern.t * case Nonempty.t
  | Map_dict of Pattern.t * case Nonempty.t
  | Component of string * Pattern.t Dict.t * child Dict.t

and case = { pats : Pattern.t Nonempty.t Nonempty.t; nodes : t }
and child = Child_name of string | Child_block of t
and t = node list

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
