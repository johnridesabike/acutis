(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** Type-checks the untyped {!Ast.t} and constructs a typed tree. *)

type 'a map_string := 'a Map.Make(String).t
type 'a map_int := 'a Map.Make(Int).t
type set_int := Set.Make(Int).t
type set_string := Set.Make(String).t

module Type : sig
  (** Manages the type definitions. *)

  type row = [ `Closed | `Open ]
  type int_bool = Not_bool | Bool

  type 'a sum = { mutable cases : 'a; mutable row : row }
  (** Common fields shared by the enum and union types. *)

  type ty =
    | Unknown of row ref
        (** The row is for unification with sum types during destructuring. *)
    | Int
    | Float
    | String
    | Nullable of t
    | List of t
    | Tuple of t list
    | Record of record
    | Dict of t * set_string ref
        (** The set tracks which keys have been used so they can build
            pattern-matching decision trees. *)
    | Enum_int of set_int sum * int_bool
    | Enum_string of set_string sum
    | Union_int of string * sum_union_int * int_bool
    | Union_string of string * sum_union_string

  and sum_union_int = record map_int sum
  and sum_union_string = record map_string sum
  and record = t map_string ref
  and t = ty ref

  type interface = t map_string

  val pp : Format.formatter -> t -> unit
  val pp_interface : Format.formatter -> interface -> unit
end

type echo = [ `Var of string | `String of string | `Field of echo * string ]
type scalar = [ `Int of int | `Float of float | `String of string ]

type scalar_sum =
  | Scalar_sum_none
  | Scalar_sum_int of set_int Type.sum
  | Scalar_sum_string of set_string Type.sum

type union_tag =
  | Union_tag_none
  | Union_tag_int of string * int * Type.sum_union_int
  | Union_tag_string of string * string * Type.sum_union_string

(** We use a GADT to prove that certain patterns may only appear when
    constructing and certain patterns may only appear when destructuring. *)
type _ pat =
  | Scalar : scalar * scalar_sum -> 'a pat
  | Nil : 'a pat
  | Cons : 'a pat -> 'a pat  (** This always contains a {!Tuple}. *)
  | Tuple : 'a pat list -> 'a pat
  | Record : union_tag * 'a pat map_string * Type.record -> 'a pat
  | Dict : 'a pat map_string * set_string ref -> 'a pat
      (** The set is part of a {!Type.Dict}. *)
  | Var : string -> 'a pat
  | Block : nodes -> [ `Construct ] pat
  | Field : [ `Construct ] pat * string -> [ `Construct ] pat
  | Any : [ `Destruct ] pat

and node =
  | Text of string * Ast.trim * Ast.trim
  | Echo of (Ast.echo_format * echo) list * Ast.echo_format * echo * Ast.escape
  | Match of
      Loc.t
      * [ `Construct ] pat Nonempty.t
      * Type.t Nonempty.t
      * case Nonempty.t
  | Map_list of Loc.t * [ `Construct ] pat * Type.t Nonempty.t * case Nonempty.t
  | Map_dict of Loc.t * [ `Construct ] pat * Type.t Nonempty.t * case Nonempty.t
  | Component of string * [ `Construct ] pat map_string

and case = {
  pats : (Loc.t * [ `Destruct ] pat Nonempty.t) Nonempty.t;
  bindings : string list;
      (** The binding list is needed to help produce runtime instructions. *)
  nodes : nodes;
}

and nodes = node list

type t = { nodes : nodes; types : Type.interface }

type ('a, 'b) source =
  | Src of string * 'a
  | Fun of string * Type.interface * 'b

val make_components : (Ast.t, 'a) source map_string -> (t, 'a) source map_string
val make : root:string -> (t, 'a) source map_string -> Ast.t -> t
val make_interface_standalone : Ast.interface -> Type.interface
