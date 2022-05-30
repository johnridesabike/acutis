(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

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

module DagMap : sig
  type ('a, 'b) t

  val make : f:(('a, 'b) t -> 'a -> 'b) -> 'a MapString.t -> ('a, 'b) t
  (** Use [get] inside the [f] callback. *)

  val prelinked : 'a MapString.t -> ('a, 'a) t
  val get : string -> ('a, 'b) t -> 'b
  val link_all : ('a, 'b) t -> 'b MapString.t
end
