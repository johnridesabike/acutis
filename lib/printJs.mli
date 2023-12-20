(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2023 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** Format compiled templates as self-contained JavaScript modules. *)

type jsfun
(** Information about how to import an external function. *)

val jsfun : module_path:string -> function_path:string -> jsfun
val pp_jsfun : Format.formatter -> jsfun -> unit

type t = jsfun Compile.t

val cjs : Format.formatter -> t -> unit
(** Produce a CommonJS module. *)

val esm : Format.formatter -> t -> unit
(** Produce an ECMAScript module. *)
