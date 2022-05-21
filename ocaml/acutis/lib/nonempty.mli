(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

type 'a t = ( :: ) of 'a * 'a list

val to_list : 'a t -> 'a list
val cons : 'a -> 'a t -> 'a t
val hd : 'a t -> 'a
val rev : 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
