(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** Lists with at least one item. *)

(** We can use the standard list syntax: [[1, 2, 3]] and [1 :: 2 :: 3 :: []]. *)
type 'a t = ( :: ) of 'a * 'a list

val to_list : 'a t -> 'a list
val cons : 'a -> 'a t -> 'a t

val hd : 'a t -> 'a
(** Like [Stdlib.List.hd], except guaranteed to return. *)

val rev : 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t
(** Calls [Stdlib.List.map]. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Calls [Stdlib.List.map2]. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Calls [Stdlib.List.equal]. *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
