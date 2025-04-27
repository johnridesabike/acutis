(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** Orchestrate the {!Lexer}, {!Parser}, {!Typechecker}, and {!Matching} to
    produce the final template. *)

type 'a map_string := 'a Map.Make(String).t

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
  | `Assoc of data map_string
  | `Var of string
  | `Field of data * string
  | `Block of int
    (** To separate the data from the rest of the tree, we take any template
        blocks and place them into a {!blocks}. At runtime, the [`Block]
        constructors will get their rendered content based on their indices. *)
  ]

type blocks
(** A sequence of {!type-nodes}, indexed by integers. *)

type node =
  | Text of string
  | Echo of (echo_format * echo) list * echo_format * echo * escape
  | Match of blocks * data array * nodes Matching.t
  | Map_list of blocks * data * nodes Matching.t
  | Map_dict of blocks * data * nodes Matching.t
  | Component of string * blocks * data map_string

and nodes = node list

val blocks_length : blocks -> int
val blocks_to_seq : blocks -> (int * nodes) Seq.t

module Components : sig
  type 'a source

  val from_src : fname:string -> name:string -> Lexing.lexbuf -> _ source
  val from_fun : name:string -> Typechecker.Type.scheme -> 'a -> 'a source

  type 'a t

  val empty : _ t
  val of_seq : 'a source Seq.t -> 'a t
end

type 'a t = {
  name : string;
  types : Typechecker.Type.scheme;
  components : (string * nodes) list;
      (** Components are topologically ordered. *)
  externals : (string * Typechecker.Type.scheme * 'a) list;
  nodes : nodes;
}

val make : fname:string -> 'a Components.t -> Ast.t -> 'a t
val make_interface : fname:string -> Lexing.lexbuf -> Typechecker.Type.scheme

module TyRepr : sig
  val nodes : nodes -> Pp.TyRepr.t
end
