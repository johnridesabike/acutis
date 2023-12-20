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

val lex_unexpected : Lexing.lexbuf -> char -> _
val lex_bad_int : Lexing.lexbuf -> string -> _
val lex_unterminated_comment : Lexing.lexbuf -> _
val lex_unterminated_string : Lexing.lexbuf -> _
val parse_error : int -> Lexing.lexbuf -> _

(** {1 Type errors.} *)

val dup_record_key : Loc.t -> string -> _
val extra_record_tag : Loc.t -> _
val bad_block : Loc.t -> _
val bad_field : Loc.t -> _

val type_mismatch :
  Loc.t -> (Format.formatter -> 'ty -> unit) -> 'ty -> 'ty -> _

val missing_field :
  Loc.t -> string -> (Format.formatter -> 'ty -> unit) -> 'ty -> _

val underscore_in_construct : Loc.t -> _
val name_bound_too_many : Loc.t -> string -> _
val var_missing : Loc.t -> string -> _
val var_unused : Loc.t -> string -> _
val pat_num_mismatch : Loc.t -> _
val map_pat_num_mismatch : Loc.t -> _
val component_name_mismatch : Loc.t -> string -> string -> _
val component_extra_prop : Loc.t -> string -> string -> _

(** {2 Interface errors.} *)

val interface_duplicate : Loc.t -> string -> _
val interface_bad_name : Loc.t -> string -> _
val interface_untagged_union : Loc.t -> _
val interface_unmatched_tags : Loc.t -> string -> string -> _

val interface_duplicate_tag :
  Loc.t -> (Format.formatter -> 'a -> unit) -> 'a -> _

val interface_open_bool_union : Loc.t -> _

val interface_type_mismatch :
  Loc.t -> string -> (Format.formatter -> 'ty -> unit) -> 'ty -> 'ty -> _

val interface_missing_prop :
  Loc.t -> string -> (Format.formatter -> 'ty -> unit) -> 'ty -> _

(** {1 Matching errors.} *)

val unused_case : Loc.t -> _
val parmatch : Loc.t -> (Format.formatter -> 'a -> unit) -> 'a -> _

(** {1 Other compile errors.} *)

val duplicate_name : string -> _
val cycle : string list -> _
val missing_component : string list -> string -> _

(** {1 Decode errors.} *)

val decode :
  fname:string -> stack:'a -> ty:'a -> input:'a -> (string -> 'a) -> 'a array

val missing_keys :
  fname:string -> stack:'a -> ty:'a -> keys:'a -> (string -> 'a) -> 'a array

(** {1 Internal errors.} *)

val internal : string * int * int * int -> string -> _
(** Use this instead of [assert false] when an internal invariant breaks. It
    indicates a bug in the compiler. The first argument is the type for
    [__POS__]. *)
