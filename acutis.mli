(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2024 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** This is the main API wrapper around the compiler internals
    ({!Acutis_internals}). *)

type message
(** An error message. *)

type 'a compile_result := message list * 'a option
(** The result of a compile computation. The first value is a list of warnings
    and error messages. If the second value is [Some] then the computation
    succeeded. If [None], then it failed for the reasons in the messages. A
    successful computation may have warning messages. *)

type interface
(** Type interfaces for templates. *)

type parsed
(** A template that's been parsed but not type-checked, optimized, or linked
    yet. *)

type 'a comp
(** A template component. Parameter ['a] is the type of external components,
    which may either be a function or a {!type-js_import}. *)

type 'a comps_compiled
(** A group of compiled and linked components. *)

type 'a compiled
(** A completely compiled template. *)

(** {1 Compiling templates.} *)

val comp_of_parsed : string -> parsed -> 'a comp
(** Convert a parsed Acutis template into a component. The string is the name
    the component is called in Acutis code. *)

val comp_of_fun : string -> interface -> 'a -> 'a comp
(** Convert a function or a {!type-js_import} into a template component. The
    string is the name the component is called in Acutis code. *)

val comps_compile : 'a comp Seq.t -> 'a comps_compiled compile_result
(** Type-check, optimize, and link the components. *)

val comps_empty : 'a comps_compiled

val parse : Lexing.lexbuf -> parsed compile_result
(** Parse a component. This does not type-check, optimize, or link yet. *)

val compile : 'a comps_compiled -> parsed -> 'a compiled compile_result
(** Type-check, optimize, and link the template with its components. *)

val compile_interface : Lexing.lexbuf -> interface compile_result
(** Parse and type-check a type interface with no template. *)

val get_interface : 'a compiled -> interface

(** {1 Working with decodable data and rendering templates in OCaml.} *)

module type DECODABLE = sig
  (** A specification for decoding and encoding input data. *)

  type t

  type 'a assoc
  (** A key-value container such as an association list or a string map. *)

  (** {1 Decoding} *)

  val get_int : t -> int option
  val get_string : t -> string option
  val get_float : t -> float option
  val get_bool : t -> bool option
  val get_some : t -> t option
  val get_seq : t -> t Seq.t option
  val get_assoc : t -> t assoc option
  val assoc_find : string -> 'a assoc -> 'a
  val assoc_mem : string -> 'a assoc -> bool
  val assoc_to_seq : 'a assoc -> (string * 'a) Seq.t

  (** {1 Encoding} *)

  val null : t
  val some : t -> t
  val of_float : float -> t
  val of_string : string -> t
  val of_bool : bool -> t
  val of_int : int -> t
  val of_seq : t Seq.t -> t
  val of_seq_assoc : (string * t) Seq.t -> t

  (** {1 Debugging} *)

  val to_string : t -> string
  val marshal : 'a -> t
end

module Of_decodable (D : DECODABLE) : sig
  (** A functor that builds functions to render templates and construct type
      interfaces from decodable data. *)

  val apply : (D.t -> string) compiled -> D.t -> (string, message list) result
  (** Apply data to a template. [Ok str] is the rendered output. [Error str] is
      an error message. *)

  val interface : D.t -> interface compile_result
  (** Compile a type interface from a generic format that can be represented
      with any decodable data. For example, if we use JSON, where [assoc] values
      are objects and [seq] values are arrays:

      - Simple types are represented by their names: ["int"], ["float"],
        ["string"], and ["_"].
      - Records are objects, e.g. [{"a": "int", "b": "string"}].
      - Parameterized types and tagged record fields are arrays with two values,
        the name of the type and the parameter, e.g. [["nullable", "int"]].
      - Tuples, enumerations, and unions are parameterized types with any number
        of values for their cases, e.g. [["enum", false, true]].

      The parameterized type names are:
      - [dict]
      - [enum]
      - [enum_open]
      - [list]
      - [nullable]
      - [tag]
      - [tuple]
      - [union]
      - [union_open] *)
end

(** {1 Printing templates as JavaScript modules.} *)

type js_import
(** Information to import an external JavaScript function. *)

val js_import : module_path:string -> function_path:string -> js_import

val cjs : Format.formatter -> js_import compiled -> unit
(** Print a template as a CommonJS module. *)

val esm : Format.formatter -> js_import compiled -> unit
(** Print a template as an ECMAScript module. *)

(** {1 Handling errors and printing debug information.} *)

val pp_message : Format.formatter -> message -> unit
(** Pretty-print an error message. *)

val pp_interface : Format.formatter -> interface -> unit
(** Pretty-print a type interface in Acutis syntax. *)

val pp_ast : Format.formatter -> parsed -> unit
(** Pretty-print a template's abstract syntax tree in S-expression syntax. *)

val pp_compiled : Format.formatter -> _ compiled -> unit
(** Pretty-print a fully-compiled template in S-expression syntax. *)

val pp_instructions :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a compiled -> unit
(** Pretty-print runtime instructions in S-expression syntax. *)

val pp_js_import : Format.formatter -> js_import -> unit
(** Pretty-print JavaScript import data in S-expression syntax. *)
