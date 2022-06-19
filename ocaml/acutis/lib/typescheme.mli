(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open StdlibExtra

module Variant : sig
  type row = [ `Closed | `Open ]
  type extra = [ `Extra_none | `Extra_bool ]

  val equal_extra : extra -> extra -> bool
  val pp_extra : Format.formatter -> extra -> unit

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
  val equal : t -> t -> bool
end

module Union : sig
  type 'a t =
    ('a MapString.t ref MapInt.t, 'a MapString.t ref MapString.t) Variant.t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

type ty =
  (* This row is for unification with variant types during destructuring. *)
  | Unknown of Variant.row ref
  | Int
  | Float
  | String
  | Echo
  | Nullable of t
  | List of t
  | Tuple of t list
  | Record of t MapString.t ref
  | Dict of t * SetString.t ref
  | Enum of Enum.t
  | Union of string * t Union.t

and t = ty ref

(* Public API for declaring type schemes: *)
val unknown : unit -> t
val int : unit -> t
val float : unit -> t
val string : unit -> t
val echo : unit -> t
val nullable : t -> t
val list : t -> t
val tuple : t list -> t
val record : (string * t) list -> t
val dict : t -> t
val enum_int : Variant.row -> int list -> t
val enum_string : Variant.row -> string list -> t
val bool : unit -> t
val false_only : unit -> t
val true_only : unit -> t
val union_int : Variant.row -> string -> (int * (string * t) list) list -> t

val union_string :
  Variant.row -> string -> (string * (string * t) list) list -> t

val union_boolean : string -> f:(string * t) list -> t:(string * t) list -> t
val union_false_only : string -> (string * t) list -> t
val union_true_only : string -> (string * t) list -> t
val make : (string * t) list -> t MapString.t
val empty : t MapString.t

(* Utilities *)
val internal_record : t MapString.t ref -> t
val internal_dict_keys : t -> SetString.t ref -> t
val internal_copy_record : t MapString.t -> t MapString.t
val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool

module Child : sig
  type t

  val make : (string * t) list -> t MapString.t
  val child : string -> string * t
  val nullable : string -> string * t
  val is_nullable : t -> bool
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val empty : t MapString.t
end
