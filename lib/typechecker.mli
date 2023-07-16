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

type row = [ `Closed | `Open ]
type sum_extra = Not_bool | Bool

type 'a sum = { mutable cases : 'a; mutable row : row; extra : sum_extra }
(** Common fields shared by the enum and union types. *)

type enum = Enum_int of Set.Int.t | Enum_string of Set.String.t

type ty_ =
  | Unknown of row ref
      (** The row is for unification with sum types during destructuring. *)
  | Int
  | Float
  | String
  | Nullable of ty
  | List of ty
  | Tuple of ty list
  | Record of ty Map.String.t ref
  | Dict of ty * Set.String.t ref
      (** The set tracks which keys have been used so they can build
          pattern-matching decision trees. *)
  | Enum of enum sum
  | Union of string * union sum

and union =
  | Union_int of ty Map.String.t ref Map.Int.t
  | Union_string of ty Map.String.t ref Map.String.t

and ty = ty_ ref

type echo = [ `Var of string | `String of string | `Field of echo * string ]
type scalar = [ `Int of int | `Float of float | `String of string ]
type tag = string * [ `Int of int | `String of string ] * union sum
type construct = private Construct_tag
type destruct = private Destruct_tag

(** We use a GADT to prove that certain patterns may only appear when
    constructing and certain patterns may only appear when destructuring. *)
type _ pat =
  | TScalar : scalar * enum sum option -> 'a pat
  | TNil : 'a pat
  | TCons : 'a pat -> 'a pat  (** This always contains a [TTuple]. *)
  | TTuple : 'a pat list -> 'a pat
  | TRecord : tag option * 'a pat Map.String.t * ty Map.String.t ref -> 'a pat
  | TDict : 'a pat Map.String.t * Set.String.t ref -> 'a pat
  | TVar : string -> 'a pat
  | TBlock : nodes -> construct pat
  | TField : construct pat * string -> construct pat
  | TAny : destruct pat

and node =
  | TText of string * Ast.trim * Ast.trim
  | TEcho of (Ast.echo_format * echo) list * Ast.echo_format * echo * Ast.escape
  | TMatch of Loc.t * construct pat Nonempty.t * ty Nonempty.t * case Nonempty.t
  | TMap_list of Loc.t * construct pat * ty Nonempty.t * case Nonempty.t
  | TMap_dict of Loc.t * construct pat * ty Nonempty.t * case Nonempty.t
  | TComponent of string * construct pat Map.String.t

and case = {
  pats : (Loc.t * destruct pat Nonempty.t) Nonempty.t;
  nodes : nodes;
}

and nodes = node list

type t = { nodes : nodes; types : ty Map.String.t }

type ('a, 'b) source =
  | Src of string * 'a
  | Fun of string * ty Map.String.t * 'b

val make_components :
  (Ast.t, 'a) source Map.String.t -> (t, 'a) source Map.String.t

val make : root:string -> (t, 'a) source Map.String.t -> Ast.t -> t
val make_interface_standalone : Ast.interface -> ty Map.String.t

(** Type functions. *)

val ty_unknown : unit -> ty
val ty_int : unit -> ty
val ty_float : unit -> ty
val ty_string : unit -> ty
val ty_nullable : ty -> ty
val ty_list : ty -> ty
val ty_tuple : ty list -> ty
val ty_record : ty Map.String.t ref -> ty
val ty_dict : ty -> ty
val ty_enum : enum sum -> ty
val ty_union : string -> union sum -> ty
val enum_string : Set.String.t -> row -> enum sum
val enum_int : Set.Int.t -> row -> enum sum
val enum_false_and_true : unit -> enum sum
val enum_false_only : unit -> enum sum
val enum_true_only : unit -> enum sum
val union_int : ty Map.String.t ref Map.Int.t -> row -> union sum
val union_string : ty Map.String.t ref Map.String.t -> row -> union sum

val union_false_and_true :
  f:ty Map.String.t ref -> t:ty Map.String.t ref -> union sum

val union_false_only : ty Map.String.t ref -> union sum
val union_true_only : ty Map.String.t ref -> union sum
val pp_ty : Format.formatter -> ty -> unit
