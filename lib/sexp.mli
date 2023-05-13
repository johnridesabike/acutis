(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2023 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

type t
(** The internal S-expression type. We use it as a serialization format for
    debugging things like the AST. *)

val symbol : string -> t
val string : string -> t
val int : int -> t
val bool : int -> t
val float : float -> t
val list : t list -> t
val seq : t Seq.t -> t
val empty : t
val pair : ('a -> t) -> ('b -> t) -> 'a * 'b -> t
val set_int : Set.Int.t -> t
val map_string : ('a -> t) -> 'a Map.String.t -> t
val pp : Format.formatter -> t -> unit
