(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** Orchestrate the {!Lexer}, {!Parser}, {!Typechecker}, and {!Matching}
    to produce the final template. *)

val parse : fname:string -> Lexing.lexbuf -> Ast.t
(** @raise Error.Acutis_error *)

type escape = Ast.escape = No_escape | Escape

type echo_format = Ast.echo_format =
  | Fmt_string
  | Fmt_int
  | Fmt_float
  | Fmt_bool

type echo = [ `Var of string | `String of string | `Field of echo * string ]

type data =
  [ `Null
  | `Int of int
  | `Float of float
  | `String of string
  | `Array of data array
  | `Assoc of data Map.String.t
  | `Var of string
  | `Field of data * string
  | `Block of int
    (** To separate the data from the rest of the tree, we take any template
        blocks and place them into a {!blocks}. At runtime, the [`Block]
        constructors will get their rendered content based on their indices. *)
  ]

type blocks
(** A sequence of {!nodes}, indexed by integers. *)

type node =
  | Text of string
  | Echo of (echo_format * echo) list * echo_format * echo * escape
  | Match of blocks * data array * nodes Matching.t
  | Map_list of blocks * data * nodes Matching.t
  | Map_dict of blocks * data * nodes Matching.t
  | Component of string * blocks * data Map.String.t

and nodes = node list

val blocks_length : blocks -> int
val blocks_to_seq : blocks -> (int * nodes) Seq.t

module Components : sig
  type 'a source

  val from_src : fname:string -> name:string -> Lexing.lexbuf -> _ source
  (** Parses the input but doesn't type-check yet.
      @param fname The filename (for error messages).
      @param name The component name when called inside a template.
      @raise Error.Acutis_error *)

  val from_fun : name:string -> Typescheme.t -> 'a -> 'a source

  type 'a t

  val empty : _ t

  val of_seq : 'a source Seq.t -> 'a t
  (** Type-checks and optimizes the components.
      @raise Error.Acutis_error *)
end

type 'a t = {
  name : string;
  types : Typescheme.t;
  nodes : nodes;
  components : nodes Map.String.t;
  externals : (Typescheme.t * 'a) Map.String.t;
}

val make : fname:string -> 'a Components.t -> Lexing.lexbuf -> 'a t
val make_interface : fname:string -> Lexing.lexbuf -> Typescheme.t
val to_sexp : nodes -> Sexp.t
