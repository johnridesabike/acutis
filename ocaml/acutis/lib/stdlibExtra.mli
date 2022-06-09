(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module Pp : sig
  open Format

  val sep_comma : formatter -> unit -> unit
  (** Outputs [,@ ]. *)

  val field : formatter -> string -> unit
  (** Outputs either [field] or ["string field"] depending on whether the field
      name is a valid identifier. *)
end

module Loc : sig
  type t = Lexing.position * Lexing.position

  val dummy : t
  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool
  (** Always returns [true]. Do not use location information to test
      equivalency. *)
end

module type MAP = sig
  include Map.S

  val pp_key : Format.formatter -> key -> unit
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module MapString : MAP with type key = string
module MapInt : MAP with type key = int

module type SET = sig
  include Set.S

  val pp_elt : Format.formatter -> elt -> unit
  val pp : Format.formatter -> t -> unit
end

module SetString : SET with type elt = string
module SetInt : SET with type elt = int

module StringExtra : sig
  val ltrim : string -> string
  val rtrim : string -> string
end

module Nonempty : sig
  type 'a t = ( :: ) of 'a * 'a list

  val to_list : 'a t -> 'a list
  val cons : 'a -> 'a t -> 'a t
  val hd : 'a t -> 'a
  val rev : 'a t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end
