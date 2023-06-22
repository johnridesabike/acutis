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

  val of_int : int -> t
  val of_string : string -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_sexp : t -> Sexp.t
end

(** The boxed representation of runtime data. *)
type 'a t =
  | Other of 'a
      (** At runtime, this stores any raw input with an unknown type.
          In a compiled template, this stores the names of variables and
          template blocks. *)
  | Nil  (** Represents both [null] and [[]]. *)
  | Array of 'a t array  (** Tuples are compiled to arrays. *)
  | Dict of 'a t Map.String.t
      (** Records and dictionaries are compiled to string maps. *)
  | Const of Const.t

(** {1 Constructing data} *)

val other : 'a -> 'a t
val null : _ t
val const : Const.t -> _ t
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

val map : ('a -> 'b) -> 'a t -> 'b t
(** Transform the contents of [Unknown] values to a different type of data. *)

(** {1 Deconstructing data} *)

val get_const : _ t -> Const.t
val get_tuple : 'a t -> 'a t array
val get_dict : 'a t -> 'a t Map.String.t
val is_null : _ t -> bool
val get_nullable : 'a t -> 'a t option
val fold_list : (index:'a t -> 'b -> 'a t -> 'b) -> 'b -> 'a t -> 'b
val fold_dict : (index:'a t -> 'b -> 'a t -> 'b) -> 'b -> 'a t -> 'b
val to_sexp : ('a -> Sexp.t) -> 'a t -> Sexp.t
val dict_to_sexp : ('a -> Sexp.t) -> 'a t Map.String.t -> Sexp.t
