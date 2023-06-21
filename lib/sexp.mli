(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2023 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** Serialize internal data into S-expressions for debugging. *)

type t
(** The internal S-expression type. *)

val make : string -> t list -> t
(** [make atom [a; b; c;]] returns the S-exp [(atom a b c)].*)

val empty : t
(** Equivalent to the expression [()]. *)

val noop : t
(** This is always ignored when printing. *)

val symbol : string -> t
val string : string -> t
val int : int -> t
val bool : int -> t
val float : float -> t
val of_seq : ('a -> t) -> 'a Seq.t -> t
val pp : Format.formatter -> t -> unit

(** Convenience functions *)

val option : ('a -> t) -> 'a option -> t
val pair : ('a -> t) -> ('b -> t) -> 'a * 'b -> t
val triple : ('a -> t) -> ('b -> t) -> ('c -> t) -> 'a * 'b * 'c -> t
val map_string : ('a -> t) -> 'a Map.String.t -> t
