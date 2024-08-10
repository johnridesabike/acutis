(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2024 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** This is the main API wrapper around the compiler internals. *)

exception Acutis_error of string [@warn_on_literal_pattern]
(** This is raised with a message if any part of the process fails. *)

(** {1 Declaring type schemes.} *)

(** This is used primarily for component functions. *)

type ty = private Acutis_internals.Typechecker.Type.t
type typescheme = ty Map.Make(String).t

val unknown : unit -> ty
val int : unit -> ty
val float : unit -> ty
val string : unit -> ty
val nullable : ty -> ty
val list : ty -> ty
val tuple : ty list -> ty
val record : (string * ty) Seq.t -> ty
val dict : ty -> ty
val enum_int : [ `Open | `Closed ] -> int Seq.t -> ty
val enum_string : [ `Open | `Closed ] -> string Seq.t -> ty
val boolean : unit -> ty
val false_only : unit -> ty
val true_only : unit -> ty

val union_int :
  [ `Open | `Closed ] -> string -> (int * (string * ty) Seq.t) Seq.t -> ty

val union_string :
  [ `Open | `Closed ] -> string -> (string * (string * ty) Seq.t) Seq.t -> ty

val union_boolean :
  string -> f:(string * ty) Seq.t -> t:(string * ty) Seq.t -> ty

val union_false_only : string -> (string * ty) Seq.t -> ty
val union_true_only : string -> (string * ty) Seq.t -> ty
val typescheme : (string * ty) Seq.t -> typescheme
val typescheme_empty : typescheme

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
(** Convert a function (or possibly data for loading a function) into a
    template component. *)

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

module type MONAD = sig
  (** A monad interface for async operations. *)

  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val error : string -> 'a t
end

module type DECODABLE = sig
  (** Decode and encode input data. *)

  (** {1 Container types.} *)

  type 'a linear
  (** A linear container such as a list or array. *)

  val length : 'a linear -> int
  val iteri : (int -> 'a -> unit) -> 'a linear -> unit

  type 'a assoc
  (** A key-value container such as an association list or a string map. *)

  val assoc_find : string -> 'a assoc -> 'a
  val assoc_mem : string -> 'a assoc -> bool
  val assoc_iter : (string -> 'a -> unit) -> 'a assoc -> unit

  type t

  (** {1 Encoding} *)

  val null : t
  val some : t -> t
  val of_float : float -> t
  val of_string : string -> t
  val of_bool : bool -> t
  val of_int : int -> t
  val of_array : t array -> t
  val of_assoc : (string * t) Seq.t -> t

  (** {1 Decoding} *)

  val decode_int : t -> int option
  val decode_string : t -> string option
  val decode_float : t -> float option
  val decode_bool : t -> bool option
  val decode_some : t -> t option
  val decode_linear : t -> t linear option
  val decode_assoc : t -> t assoc option

  (** {1 Debugging} *)

  val to_string : t -> string
end

module type RENDER = sig
  type data
  type output

  val apply : (data -> output) compiled -> data -> output
  (** Apply data to a template and return the rendered output. *)
end

(** A functor that builds a render implementation for a given monad interface
    and a given decodable input type.

    We need to use a functor because parameterized types, such as in {!MONAD},
    are not supported in first-class modules. *)
module Render (M : MONAD) (D : DECODABLE) :
  RENDER with type data = D.t and type output = string M.t

(** A simpler version of {!Render} that only requires a decodable module and
    outputs a string. *)
module RenderString (D : DECODABLE) :
  RENDER with type data = D.t and type output = string

(** {1 Printing templates as JavaScript modules.} *)

type js_import
(** Information to import an external JavaScript function. *)

val js_import : module_path:string -> function_path:string -> js_import

val cjs : Format.formatter -> js_import compiled -> unit
(** Print a template as a CommonJS module. *)

val esm : Format.formatter -> js_import compiled -> unit
(** Print a template as an ECMAScript module. *)

(** {1 Printing debug information.} *)

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
