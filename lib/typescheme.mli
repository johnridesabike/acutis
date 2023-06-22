(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** Defines the types (integer, record, etc.). *)

type row = [ `Closed | `Open ]

val row_to_sexp : row -> Sexp.t

type sum_extra = Not_bool | Bool

val equal_sum_extra : sum_extra -> sum_extra -> bool

type 'a sum = { mutable cases : 'a; mutable row : row; extra : sum_extra }
(** This is a common type shared by the sum types, {!module-Enum} and
    {!module-Union}. *)

module Enum : sig
  type cases = Int of Set.Int.t | String of Set.String.t
  type t = cases sum

  val string_singleton : string -> row -> t
  val int_singleton : int -> row -> t
  val false_and_true_cases : cases
  val false_and_true : unit -> t
  val false_only : unit -> t
  val true_only : unit -> t
end

module Union : sig
  type 'a cases =
    | Int of 'a Map.String.t ref Map.Int.t
    | String of 'a Map.String.t ref Map.String.t

  type 'a t = 'a cases sum

  val int_singleton : int -> 'a Map.String.t ref -> row -> 'a t
  val bool_singleton : int -> 'a Map.String.t ref -> row -> 'a t
  val string_singleton : string -> 'a Map.String.t ref -> row -> 'a t
end

type ty =
  | Unknown of row ref
      (** The row is for unification with variant types during destructuring. *)
  | Int
  | Float
  | String
  | Nullable of t
  | List of t
  | Tuple of t list
  | Record of t Map.String.t ref
  | Dict of t * Set.String.t ref
      (** The set tracks which keys have been used so they can build
          pattern-matching decision trees. *)
  | Enum of Enum.t
  | Union of string * t Union.t

and t = ty ref

(** {1 Public API for declaring type schemes.} *)

val unknown : unit -> t
val int : unit -> t
val float : unit -> t
val string : unit -> t
val nullable : t -> t
val list : t -> t
val tuple : t list -> t
val record : (string * t) list -> t
val dict : t -> t
val enum_int : row -> int list -> t
val enum_string : row -> string list -> t
val boolean : unit -> t
val false_only : unit -> t
val true_only : unit -> t
val union_int : row -> string -> (int * (string * t) list) list -> t
val union_string : row -> string -> (string * (string * t) list) list -> t
val union_boolean : string -> f:(string * t) list -> t:(string * t) list -> t
val union_false_only : string -> (string * t) list -> t
val union_true_only : string -> (string * t) list -> t
val make : (string * t) list -> t Map.String.t
val empty : t Map.String.t

(** {1 Internal utilities.} *)

val internal_record : t Map.String.t ref -> t
val internal_dict_keys : t -> Set.String.t ref -> t
val internal_copy_record : t Map.String.t -> t Map.String.t
val internal_bool : int list -> t
val pp : Format.formatter -> t -> unit
val pp_interface : Format.formatter -> t Map.String.t -> unit
