(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** The public API for declaring type schemes. *)

type row = [ `Open | `Closed ]
type ty = Typechecker.ty
type t = ty Map.String.t

val unknown : unit -> ty
val int : unit -> ty
val float : unit -> ty
val string : unit -> ty
val nullable : ty -> ty
val list : ty -> ty
val tuple : ty list -> ty
val record : (string * ty) list -> ty
val dict : ty -> ty
val enum_int : row -> int list -> ty
val enum_string : row -> string list -> ty
val boolean : unit -> ty
val false_only : unit -> ty
val true_only : unit -> ty
val union_int : row -> string -> (int * (string * ty) list) list -> ty
val union_string : row -> string -> (string * (string * ty) list) list -> ty
val union_boolean : string -> f:(string * ty) list -> t:(string * ty) list -> ty
val union_false_only : string -> (string * ty) list -> ty
val union_true_only : string -> (string * ty) list -> ty
val make : (string * ty) list -> t
val empty : t
val pp : Format.formatter -> t -> unit
