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

type t
(** A message. *)

val pp : Format.formatter -> t -> unit

exception Acutis_error of t
(** All of the following functions raise or return this exception. *)

(** {1 Lexing and Parsing errors.} *)

val lex_unexpected : Lexing.lexbuf -> char -> _
val lex_bad_int : Lexing.lexbuf -> string -> _
val lex_unterminated_comment : Lexing.lexbuf -> _
val lex_unterminated_string : Lexing.lexbuf -> _
val parse_error : int -> Loc.t -> _

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
val cycle : string list -> exn
val missing_component : string list -> string -> exn

(** {1 Internal errors.} *)

val internal :
  __POS__:string * int * int * int ->
  ('a, Format.formatter, unit, _) format4 ->
  'a
(** Use this instead of [assert false] when an internal invariant breaks. It
    indicates a bug in the compiler. *)

val raise_fmt : ('a, Format.formatter, unit, _) format4 -> 'a
(** Raise a custom message. *)
