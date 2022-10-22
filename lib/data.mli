(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** The internal representation of runtime data. *)

module Const : sig
  type t = Int of int | String of string | Float of float

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

(** The boxed representation of runtime data. *)
type 'a t = private
  | Unknown of 'a
      (** Any values without a concrete type are preserved as-is. *)
  | Nil  (** Represents both [null] and [[]]. *)
  | Array of 'a t array  (** Tuples are compiled to arrays. *)
  | Dict of 'a t Map.String.t
      (** Records and dictionaries are compiled to string maps. *)
  | Const of Const.t * Typescheme.Variant.extra
      (** Integers, strings, and floats must be wrapped to accommodate the
          polymorphic echo. *)

(** {1 Constructing data} *)

val unknown : 'a -> 'a t
val null : _ t
val const : Const.t -> Typescheme.Variant.extra -> _ t
val int : int -> _ t
val bool : int -> _ t
val string : string -> _ t
val float : float -> _ t
val some : 'a t -> 'a t
val dict : 'a t Map.String.t -> 'a t
val tuple : 'a t array -> 'a t
val list_cons : 'a t -> 'a t -> 'a t
val list_rev : 'a t -> 'a t
val list_empty : _ t

(** {1 Deconstructing data} *)

val get_const : _ t -> Const.t
val get_tuple : 'a t -> 'a t array
val get_dict : 'a t -> 'a t Map.String.t
val is_null : _ t -> bool
val get_nullable : 'a t -> 'a t option
val fold_list : (index:'a t -> 'b -> 'a t -> 'b) -> 'b -> 'a t -> 'b
val fold_dict : (index:'a t -> 'b -> 'a t -> 'b) -> 'b -> 'a t -> 'b

(** {1 Echoing data} *)

val to_string : _ t -> string
(** This only works on constants. *)
