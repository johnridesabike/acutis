(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

exception Error of string

type loc = Lexing.position * Lexing.position

(* Lexing and Parsing errors. *)
val lex_error : Lexing.lexbuf -> _
val parse_error : int -> Lexing.lexbuf -> _
val dup_record_key : loc -> string -> _
val extra_record_tag : loc -> _

(* Type errors *)
val type_mismatch : loc -> Typescheme.ty -> Typescheme.ty -> _
val bad_union_tag : loc -> Typescheme.ty -> _
val missing_field : loc -> string -> Typescheme.ty -> _
val underscore_in_construct : loc -> _
val child_type_mismatch : loc -> Typescheme.Child.ty -> Typescheme.Child.ty -> _
val name_bound_too_many : loc -> string -> _
val var_missing : loc -> string -> _
val pat_num_mismatch : loc -> _
val map_pat_num_mismatch : loc -> _
val echo_nullable_literal : loc -> _
val extra_child : loc -> comp:string -> child:string -> _
val missing_child : loc -> string -> _
val child_in_root : loc -> _
val component_name_mismatch : loc -> string -> string -> _

(* Matching errors *)
val unused_case : loc -> _
val parmatch : loc -> (Format.formatter -> 'a -> unit) -> 'a -> _

(* Dag errors *)
val duplicate_name : string -> _
val cycle : string list -> _
val missing_component : string list -> string -> _

(* Decode errors *)
val decode :
  (Format.formatter -> 'a -> unit) -> 'a -> Typescheme.ty -> Yojson.Basic.t -> _

val missing_key :
  (Format.formatter -> 'a -> unit) -> 'a -> Typescheme.ty -> string -> _

val bad_enum :
  (Format.formatter -> 'a -> unit) -> 'a -> Typescheme.ty -> Yojson.Basic.t -> _
