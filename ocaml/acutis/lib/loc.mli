(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** This defines the locations of expressions in a source file. *)

type t = Lexing.position * Lexing.position
(** This type is equivalent to Menhir's [$loc] keyword. *)

val dummy : t
val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool
(** Always returns [true]. Do not use location information to test
      equivalency. *)
