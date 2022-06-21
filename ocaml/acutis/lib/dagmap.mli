(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open StdlibExtra

type ('a, 'b) t

val make :
  f:(('a, 'b) t -> 'a -> 'b) -> ?root:string -> 'a MapString.t -> ('a, 'b) t
(** Use [get] inside the [f] callback. *)

val prelinked : string -> 'a MapString.t -> ('a, 'a) t
val get : string -> ('a, 'b) t -> 'b
val link_all : ('a, 'b) t -> 'b MapString.t
