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

val handle : ('a -> 'b) -> 'a -> t Seq.t * 'b option
(** Use this to handle any functions which may raise an error. *)

type loc = Lexing.position * Lexing.position
(** This is equal to {!Ast.type-loc}. *)

(** {1 Lexing and Parsing errors.} *)

val lex_unexpected : Lexing.lexbuf -> char -> _
val lex_bad_int : Lexing.lexbuf -> string -> _
val lex_unterminated_comment : Lexing.lexbuf -> _
val lex_unterminated_string : Lexing.lexbuf -> _
val parse_error : int -> loc -> _

(** {1 Type errors.} *)

val dup_record_key : loc -> string -> _
val extra_record_tag : loc -> _
val bad_block : loc -> _
val bad_field : loc -> _
val type_mismatch : loc -> (Format.formatter -> 'ty -> unit) -> 'ty -> 'ty -> _

val missing_field :
  loc -> string -> (Format.formatter -> 'ty -> unit) -> 'ty -> _

val underscore_in_construct : loc -> _
val name_bound_too_many : loc -> string -> _
val var_missing : loc -> string -> _
val var_unused : loc -> string -> unit
val pat_num_mismatch : loc -> _
val map_pat_num_mismatch : loc -> _
val component_name_mismatch : loc -> string -> string -> _
val component_extra_prop : loc -> string -> string -> unit

(** {2 Interface errors.} *)

val interface_duplicate : loc -> string -> _
val interface_bad_name : loc -> string -> _
val interface_untagged_union : loc -> _
val interface_unmatched_tags : loc -> string -> string -> _
val interface_duplicate_tag : loc -> (Format.formatter -> 'a -> unit) -> 'a -> _

val interface_duplicate_enum :
  loc -> (Format.formatter -> 'a -> unit) -> 'a -> unit

val interface_open_bool_union : loc -> _

val interface_type_mismatch :
  loc -> string -> (Format.formatter -> 'ty -> unit) -> 'ty -> 'ty -> _

val interface_missing_prop :
  loc -> string -> (Format.formatter -> 'ty -> unit) -> 'ty -> _

(** {1 Matching errors.} *)

val unused_case : loc -> unit
val parmatch : loc -> (Format.formatter -> 'a -> unit) -> 'a -> _

(** {1 Other compile errors.} *)

val duplicate_name : string -> unit
val cycle : loc -> string list -> exn
val missing_component : loc -> string -> exn

(** {1 Decode combinator errors.} *)

val intf_decode_invalid : string -> _
val intf_decode_enum : string -> _
val intf_decode_single_param : string -> _
val intf_decode_empty_seq : unit -> _
val intf_decode_empty_record : unit -> _

(** {1 Internal errors.} *)

val internal :
  __POS__:string * int * int * int ->
  ('a, Format.formatter, unit, _) format4 ->
  'a
(** Use this instead of [assert false] when an internal invariant breaks. It
    indicates a bug in the compiler. *)
