(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** All of the error messages. *)

exception Acutis_error of string
(** Every function in this module raises this exception. *)

(** {1 Lexing and Parsing errors.} *)

val lex_error : Lexing.lexbuf -> _
val parse_error : int -> Lexing.lexbuf -> _
val dup_record_key : Loc.t -> string -> _
val extra_record_tag : Loc.t -> _

(** {1 Type errors.} *)

val type_mismatch : Loc.t -> Typescheme.t -> Typescheme.t -> _
val missing_field : Loc.t -> string -> Typescheme.t -> _
val underscore_in_construct : Loc.t -> _
val child_type_mismatch : Loc.t -> Typescheme.Child.t -> Typescheme.Child.t -> _
val name_bound_too_many : Loc.t -> string -> _
val var_missing : Loc.t -> string -> _
val var_unused : Loc.t -> string -> _
val pat_num_mismatch : Loc.t -> _
val map_pat_num_mismatch : Loc.t -> _
val echo_nullable_literal : Loc.t -> _
val extra_child : Loc.t -> comp:string -> child:string -> _
val missing_child : Loc.t -> string -> _
val child_in_root : Loc.t -> _
val component_name_mismatch : Loc.t -> string -> string -> _

(** {2 Interface errors.} *)

val interface_duplicate : Loc.t -> string -> _
val interface_bad_name : Loc.t -> string -> _
val interface_untagged_union : Loc.t -> _
val interface_unmatched_tags : Loc.t -> string -> string -> _

val interface_duplicate_tag :
  Loc.t -> (Format.formatter -> 'a -> unit) -> 'a -> _

val interface_type_mismatch :
  Loc.t -> string -> Typescheme.t -> Typescheme.t -> _

val interface_child_mismatch :
  Loc.t -> string -> Typescheme.Child.t -> Typescheme.Child.t -> _

val interface_missing_prop : Loc.t -> string -> Typescheme.t -> _
val interface_missing_child : Loc.t -> string -> _

(** {1 Matching errors.} *)

val unused_case : Loc.t -> _
val parmatch : Loc.t -> (Format.formatter -> 'a -> unit) -> 'a -> _
val duplicate_name : string -> _

(** {1 DAG errors.} *)

val cycle : string list -> _
val missing_component : string list -> string -> _

(** {1 Decode errors.} *)

module DecodePath : sig
  type t

  val make : string -> t
  val nullable : t -> t
  val index : int -> t -> t
  val key : string -> t -> t
end

val decode :
  (Format.formatter -> 'data -> unit) ->
  Typescheme.t ->
  DecodePath.t ->
  'data ->
  _

val missing_key : DecodePath.t -> Typescheme.t -> string -> _

val bad_enum :
  (Format.formatter -> 'data -> unit) ->
  Typescheme.t ->
  DecodePath.t ->
  'data ->
  _
