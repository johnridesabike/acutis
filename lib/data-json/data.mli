(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** Converts JSON strings to and from {!Acutis.Data.t}. *)

open Acutis

type 'a map = 'a Stdlib.Map.Make(String).t

type t =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Assoc of (string * t) list
  | `List of t list ]

val decode : name:string -> Typescheme.t map -> t -> t Data.t map
val encode : Typescheme.t map -> t Data.t map -> t
