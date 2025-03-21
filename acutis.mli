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

type error = private Acutis_internals.Error.t
(** An error message. *)

exception Acutis_error of error
(** This is raised if any part of the process fails. *)

(** {1 Declaring type schemes.} *)

(** These are used primarily for component functions. The examples are in Acutis
    syntax. *)

type ty = private Acutis_internals.Typechecker.Type.t
type typescheme = private Acutis_internals.Typechecker.Type.scheme

val typescheme : (string * ty) Seq.t -> typescheme
val typescheme_empty : typescheme

val unknown : unit -> ty
(** The Acutis type [_], compatible with any other type. *)

(** {2 Primitive types.} *)

val int : unit -> ty
(** Creates the primitive type [int]. *)

val float : unit -> ty
(** Creates the primitive type [float]. *)

val string : unit -> ty
(** Creates the primitive type [string]. *)

(** {2 Parameterized types.} *)

val nullable : ty -> ty
(** Wraps the given type as a nullable, e.g. [?int]. *)

val list : ty -> ty
(** Wraps the given type as a list, e.g. [[int]]. *)

val dict : ty -> ty
(** Wraps the given type as a dictionary, e.g. [<int>]. *)

(** {2 Product types.} *)

val tuple : ty Seq.t -> ty
(** Create a tuple from the sequence of types, e.g. [(int, string, float)]. *)

val record : (string * ty) Seq.t -> ty
(** Create a record from the sequence of key-value pairs, e.g.
    [{a: int, b: string, c: float}]. *)

(** {2 Sum types.} *)

(** For types that accept an [[ `Open | `Closed ]] parameter, using [`Open]
    makes the type compatible with additional values, whereas [`Closed]
    restricts the type to only the values specified. *)

val enum_int : [ `Open | `Closed ] -> int Seq.t -> ty
(** Create an enumeration from the sequence of integers, e.g. [@1 | @2 | @3]. *)

val enum_string : [ `Open | `Closed ] -> string Seq.t -> ty
(** Create an enumeration from the sequence of strings, e.g.
    [@"a" | @"b" | @"c"]. *)

val boolean : unit -> ty
(** Create the enumeration type [false | true]. *)

val false_only : unit -> ty
(** Create a type that can only be [false]. *)

val true_only : unit -> ty
(** Create a type that can only be [true]. *)

val union_string :
  [ `Open | `Closed ] -> string -> (string * (string * ty) Seq.t) Seq.t -> ty
(** [union_int row field sequence] creates a record where [field] is used to
    discriminate between different records in [sequence]. Each pair in
    [sequence] contains a value for [field] and then another sequence of
    key-value pairs that specify the rest of the record associated with that
    field. E.g.:

    {v
    {@shape: "circle", radius: int} |
    {@shape: "rectagle", height: int, width: int}
    v} *)

val union_int :
  [ `Open | `Closed ] -> string -> (int * (string * ty) Seq.t) Seq.t -> ty
(** The same as {!union_string}, except that the field's values are integers. *)

val union_boolean :
  string -> f:(string * ty) Seq.t -> t:(string * ty) Seq.t -> ty
(** [union_boolean field ~f ~t] creates a record where if [field] is [false]
    then the record has the shape of [f] and if [field] is [true] then the
    record has the shape of [t]. *)

val union_false_only : string -> (string * ty) Seq.t -> ty
(** This is similar to [union_boolean] except that the field can only be
    [false]. *)

val union_true_only : string -> (string * ty) Seq.t -> ty
(** This is similar to [union_boolean] except that the field can only be [true].
*)

(** {1 Compiling templates.}*)

type 'a comp = private 'a Acutis_internals.Compile.Components.source
(** A template component. It is either from Acutis source code or it is ['a].
    ['a] may be a function or data to load a function, depending on how the
    template is to be rendered. *)

val comp_parse : fname:string -> name:string -> Lexing.lexbuf -> 'a comp
(** Parses Acutis source into a template component but doesn't type-check yet.
    [fname] is the file path and used here for debugging. [name] is the name the
    component is called in Acutis code.*)

val comp_fun : name:string -> typescheme -> 'a -> 'a comp
(** Convert a function (or possibly data for loading a function) into a template
    component. *)

type 'a comps_compiled = private 'a Acutis_internals.Compile.Components.t
(** A group of compiled and linked components. *)

val comps_compile : 'a comp Seq.t -> 'a comps_compiled
(** Type-checks, optimizes, and links the components. *)

val comps_empty : 'a comps_compiled

type parsed
(** A template that's been parsed but not type-checked, optimized, or linked
    yet. *)

val parse : fname:string -> Lexing.lexbuf -> parsed

type 'a compiled = private 'a Acutis_internals.Compile.t
(** A completely compiled template. *)

val compile : 'a comps_compiled -> parsed -> 'a compiled
(** Type-checks, optimizes, and links the template with its components. *)

val compile_interface : fname:string -> Lexing.lexbuf -> typescheme
(** Parses and type-checks an interface with no template. *)

val get_typescheme : 'a compiled -> typescheme

(** {1 Rendering templates with OCaml.} *)

module type PROMISE = sig
  (** A promise interface for async operations. *)

  type 'a t

  val return : 'a -> 'a t
  val error : exn -> 'a t
  val await : 'a t -> 'a
end

module type DECODABLE = sig
  (** Decode and encode input data. *)

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

(** A functor that builds a render implementation for a given promise interface
    and a given decodable input type.

    We need to use a functor because parameterized types, such as in {!PROMISE},
    are not supported in first-class modules. *)
module Render (Promise : PROMISE) (Data : DECODABLE) : sig
  val apply :
    (Data.t -> string Promise.t) compiled -> Data.t -> string Promise.t
  (** Apply data to a template and return the rendered output. *)
end

val render_string :
  (module DECODABLE with type t = 'a) -> ('a -> string) compiled -> 'a -> string
(** A simpler version of {!Render} that only requires a decodable module and
    outputs a string. *)

(** {1 Printing templates as JavaScript modules.} *)

type js_import
(** Information to import an external JavaScript function. *)

val js_import : module_path:string -> function_path:string -> js_import

val cjs : Format.formatter -> js_import compiled -> unit
(** Print a template as a CommonJS module. *)

val esm : Format.formatter -> js_import compiled -> unit
(** Print a template as an ECMAScript module. *)

(** {1 Printing debugging information.} *)

val pp_error : Format.formatter -> error -> unit
(** Pretty-print an error message. *)

val pp_typescheme : Format.formatter -> typescheme -> unit
(** Pretty-print a type scheme in Acutis syntax. *)

val pp_ast : Format.formatter -> parsed -> unit
(** Pretty-print a template's abstract syntax tree in S-expression syntax. *)

val pp_compiled : Format.formatter -> 'a compiled -> unit
(** Pretty-print a fully-compiled template in S-expression syntax. *)

val pp_instructions :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a compiled -> unit
(** Pretty-print runtime instructions in S-expression syntax. *)

val pp_js_import : Format.formatter -> js_import -> unit
(** Pretty-print JavaScript import data in S-expression syntax. *)
