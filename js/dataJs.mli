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

type t = Js_of_ocaml.Js.Unsafe.any

val decode :
  name:string -> Typescheme.t Map.String.t -> t -> t Data.t Map.String.t

val encode : Typescheme.t Map.String.t -> t Data.t Map.String.t -> t
