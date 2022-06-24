(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** This orchestrates the {!Lexer}, {!Parser}, {!Typechecker}, and {!Matching}
    to produce the final template. *)

type 'a node =
  | Text of string
  | Echo of Typechecker.echo list * Typechecker.echo
  | Match of Typechecker.Pattern.t array * 'a nodes Matching.t
  | Map_list of Typechecker.Pattern.t * 'a nodes Matching.t
  | Map_dict of Typechecker.Pattern.t * 'a nodes Matching.t
  | Component of 'a * Typechecker.Pattern.t Map.String.t * 'a child Map.String.t

and 'a child = Child_name of string | Child_block of 'a nodes
and 'a nodes = 'a node list

val make_nodes : Typechecker.t -> string nodes

type 'a template =
  | Src of 'a template nodes
  | Fun of Typescheme.t Map.String.t * 'a

type 'a t = {
  prop_types : Typescheme.t Map.String.t;
  nodes : 'a template nodes;
}

val parse_string : filename:string -> string -> Ast.t

module Components : sig
  type ('a, 'b) source =
    [ `Src of string * 'a
    | `Fun of
      string * Typescheme.t Map.String.t * Typescheme.Child.t Map.String.t * 'b
    ]

  val src : name:string -> string -> (string, 'a) source

  val fn :
    name:string ->
    Typescheme.t Map.String.t ->
    Typescheme.Child.t Map.String.t ->
    'a ->
    (_, 'a) source

  type 'a t

  val empty : 'a t
  val make : (string, 'a) source list -> 'a t
end

val make : filename:string -> 'a Components.t -> string -> 'a t
