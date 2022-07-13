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
(** Outputs [,@ ]. *)

val sep_semicolon : formatter -> unit -> unit
(** Outputs [;@ ]. *)

val map_string :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a Map.String.t ->
  unit
(** The function that [Stdlib.Map.S] is missing. *)

val set_int : Format.formatter -> Set.Int.t -> unit
(** The function that [Stdlib.Set.S] is missing. *)

val field : formatter -> string -> unit
(** Outputs either [field] or ["string field"] depending on whether the input is
    a valid identifier. *)
