(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open Utils

module Variant : sig
  type row = [ `Closed | `Open ]
  type extra = Extra_none | Extra_bool
  type ('a, 'b) ty = Int of 'a | String of 'b

  type ('a, 'b) t = {
    mutable cases : ('a, 'b) ty;
    mutable row : row;
    extra : extra;
  }

  val equal :
    ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool

  val pp_row : Format.formatter -> row -> unit
end

module Enum : sig
  type t = (SetInt.t, SetString.t) Variant.t

  val string : string list -> Variant.row -> t
  val string_singleton : string -> Variant.row -> t
  val int : int list -> Variant.row -> t
  val int_singleton : int -> Variant.row -> t
  val false_and_true_cases : (SetInt.t, _) Variant.ty
  val false_and_true : unit -> t
  val true_only : unit -> t
  val false_only : unit -> t
end

module Union : sig
  type 'a t =
    ('a MapString.t ref MapInt.t, 'a MapString.t ref MapString.t) Variant.t
end

type ty' =
  (* This row is for unification with variant types during destructuring. *)
  | Unknown of Variant.row ref
  | Int
  | Float
  | String
  | Echo
  | Nullable of ty
  | List of ty
  | Tuple of ty list
  | Record of ty MapString.t ref
  | Dict of ty * SetString.t ref
  | Enum of Enum.t
  | Union of string * ty Union.t

and ty = ty' ref

type t = ty MapString.t

(* Public API for declaring type schemes: *)
val unknown : unit -> ty
val int : unit -> ty
val float : unit -> ty
val string : unit -> ty
val echo : unit -> ty
val nullable : ty -> ty
val list : ty -> ty
val tuple : ty list -> ty
val record : (string * ty) list -> ty
val dict : ty -> ty
val enum_int : Variant.row -> int list -> ty
val enum_string : Variant.row -> string list -> ty
val bool : unit -> ty
val false_only : unit -> ty
val true_only : unit -> ty
val union_int : Variant.row -> string -> (int * (string * ty) list) list -> ty

val union_string :
  Variant.row -> string -> (string * (string * ty) list) list -> ty

val union_boolean : string -> f:(string * ty) list -> t:(string * ty) list -> ty
val union_false_only : string -> (string * ty) list -> ty
val union_true_only : string -> (string * ty) list -> ty
val make : (string * ty) list -> t

(* Utilities *)
val internal_record : ty MapString.t ref -> ty
val internal_dict_keys : ty -> SetString.t ref -> ty
val internal_copy_record : t -> t
val pp_ty : Format.formatter -> ty -> unit
val pp : Format.formatter -> t -> unit
val show_ty : ty -> string
val equal : t -> t -> bool

module Child : sig
  type ty
  type t = ty MapString.t

  val make : (string * ty) list -> t
  val child : string -> string * ty
  val nullable : string -> string * ty
  val is_nullable : ty -> bool
  val equal_ty : ty -> ty -> bool
  val equal : t -> t -> bool
  val show_ty : ty -> string
end
