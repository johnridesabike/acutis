(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** This contains helpers for [Format] functions. *)

open Format

val sep_comma : formatter -> unit -> unit
(** Outputs [,@ ]. *)

val map_string :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a Map.String.t ->
  unit

val set_int : Format.formatter -> Set.Int.t -> unit

val field : formatter -> string -> unit
(** Outputs either [field] or ["string field"] depending on whether the field
      name is a valid identifier. *)
