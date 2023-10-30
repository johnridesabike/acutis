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
    | Dict of t * Set.String.t ref
        (** The set tracks which keys have been used so they can build
            pattern-matching decision trees. *)
    | Enum_int of Set.Int.t sum * int_bool
    | Enum_string of Set.String.t sum
    | Union_int of string * sum_union_int * int_bool
    | Union_string of string * sum_union_string

  and sum_union_int = record Map.Int.t sum
  and sum_union_string = record Map.String.t sum
  and record = t Map.String.t ref
  and t = ty ref

  val unknown : unit -> t
  val int : unit -> t
  val float : unit -> t
  val string : unit -> t
  val nullable : t -> t
  val list : t -> t
  val tuple : t list -> t
  val record : record -> t
  val dict : t -> t
  val sum : 'a -> row -> 'a sum
  val enum_int : Set.Int.t sum -> t
  val enum_string : Set.String.t sum -> t
  val enum_false_and_true : unit -> t
  val enum_false_only : unit -> t
  val enum_true_only : unit -> t
  val union_int : string -> sum_union_int -> t
  val union_string : string -> sum_union_string -> t
  val union_false_and_true : string -> f:record -> t:record -> t
  val union_false_only : string -> record -> t
  val union_true_only : string -> record -> t
  val pp : Format.formatter -> t -> unit
end

type echo = [ `Var of string | `String of string | `Field of echo * string ]
type scalar = [ `Int of int | `Float of float | `String of string ]

type scalar_sum =
  | Scalar_sum_none
  | Scalar_sum_int of Set.Int.t Type.sum
  | Scalar_sum_string of Set.String.t Type.sum

type union_tag =
  | Union_tag_none
  | Union_tag_int of string * int * Type.sum_union_int
  | Union_tag_string of string * string * Type.sum_union_string

type construct = private Construct_tag
type destruct = private Destruct_tag

(** We use a GADT to prove that certain patterns may only appear when
    constructing and certain patterns may only appear when destructuring. *)
type _ pat =
  | Scalar : scalar * scalar_sum -> 'a pat
  | Nil : 'a pat
  | Cons : 'a pat -> 'a pat  (** This always contains a {!Tuple}. *)
  | Tuple : 'a pat list -> 'a pat
  | Record : union_tag * 'a pat Map.String.t * Type.record -> 'a pat
  | Dict : 'a pat Map.String.t * Set.String.t ref -> 'a pat
      (** The set is part of a {!Type.Dict}. *)
  | Var : string -> 'a pat
  | Block : nodes -> construct pat
  | Field : construct pat * string -> construct pat
  | Any : destruct pat

and node =
  | Text of string * Ast.trim * Ast.trim
  | Echo of (Ast.echo_format * echo) list * Ast.echo_format * echo * Ast.escape
  | Match of
      Loc.t * construct pat Nonempty.t * Type.t Nonempty.t * case Nonempty.t
  | Map_list of Loc.t * construct pat * Type.t Nonempty.t * case Nonempty.t
  | Map_dict of Loc.t * construct pat * Type.t Nonempty.t * case Nonempty.t
  | Component of string * construct pat Map.String.t

and case = {
  pats : (Loc.t * destruct pat Nonempty.t) Nonempty.t;
  nodes : nodes;
}

and nodes = node list

type t = { nodes : nodes; types : Type.t Map.String.t }

type ('a, 'b) source =
  | Src of string * 'a
  | Fun of string * Type.t Map.String.t * 'b

val make_components :
  (Ast.t, 'a) source Map.String.t -> (t, 'a) source Map.String.t

val make : root:string -> (t, 'a) source Map.String.t -> Ast.t -> t
val make_interface_standalone : Ast.interface -> Type.t Map.String.t
