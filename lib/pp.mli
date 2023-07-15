(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** Helpers for [Format] functions. *)

open Format

val sep_comma : formatter -> unit -> unit
(** Prints [,@ ]. *)

val syntax_string : formatter -> string -> unit
(** Equivalent to the format string ["%S"]. *)

val field : formatter -> string -> unit
(** Prints either [field] or ["string field"] depending on whether the input is
    a valid identifier. *)

val bool : formatter -> int -> unit
(** Prints [false] if the input is 0, or prints [true] otherwise. *)

val surround :
  left:char ->
  right:char ->
  (formatter -> 'a -> unit) ->
  formatter ->
  'a ->
  unit
(** [surround ~left ~right f x] prints the result of [f x] with [left] and
    [right] printed before and after it. *)
