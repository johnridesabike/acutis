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
val type_mismatch : Loc.t -> Typescheme.t -> Typescheme.t -> _

(** {1 Type errors.} *)

val bad_union_tag : Loc.t -> Typescheme.t -> _
val missing_field : Loc.t -> string -> Typescheme.t -> _
val underscore_in_construct : Loc.t -> _
val child_type_mismatch : Loc.t -> Typescheme.Child.t -> Typescheme.Child.t -> _
val name_bound_too_many : Loc.t -> string -> _
val var_missing : Loc.t -> string -> _
val pat_num_mismatch : Loc.t -> _
val map_pat_num_mismatch : Loc.t -> _
val echo_nullable_literal : Loc.t -> _
val extra_child : Loc.t -> comp:string -> child:string -> _
val missing_child : Loc.t -> string -> _
val child_in_root : Loc.t -> _
val component_name_mismatch : Loc.t -> string -> string -> _
val unused_case : Loc.t -> _

(** {1 Matching errors.} *)

val parmatch : Loc.t -> (Format.formatter -> 'a -> unit) -> 'a -> _
val duplicate_name : string -> _

(** {1 DAG errors.} *)

val cycle : string list -> _
val missing_component : string list -> string -> _

(** {1 Decode errors.} *)

module DecodeStack : sig
  type t = Nullable | Index of int | Key of string
end

val decode :
  (Format.formatter -> 'data -> unit) ->
  Typescheme.t ->
  DecodeStack.t list ->
  'data ->
  _

val missing_key : DecodeStack.t list -> Typescheme.t -> string -> _

val bad_enum :
  (Format.formatter -> 'data -> unit) ->
  Typescheme.t ->
  DecodeStack.t list ->
  'data ->
  _
