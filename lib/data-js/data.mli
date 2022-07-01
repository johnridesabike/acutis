(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** Converts JavaScript values to and from {!Acutis.Data.t}. *)

open Acutis

type !'a map = 'a Stdlib.Map.Make(String).t
type t = Js_of_ocaml.Js.Unsafe.any

val decode : Typescheme.t map -> t -> t Data.t map
val encode : Typescheme.t map -> t Data.t map -> t
