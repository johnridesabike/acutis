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

val parse : name:string -> Lexing.lexbuf -> Ast.t

type 'a node =
  | Text of string
  | Echo of Typechecker.echo list * Typechecker.echo
  | Match of Typechecker.Pattern.t array * 'a nodes Matching.t
  | Map_list of Typechecker.Pattern.t * 'a nodes Matching.t
  | Map_dict of Typechecker.Pattern.t * 'a nodes Matching.t
  | Component of 'a * Typechecker.Pattern.t Map.String.t * 'a child Map.String.t

and 'a child = Child_name of string | Child_block of 'a nodes
and 'a nodes = 'a node list

type 'a template =
  | Src of 'a template nodes
  | Fun of Typescheme.t Map.String.t * 'a

type 'a t = {
  prop_types : Typescheme.t Map.String.t;
  nodes : 'a template nodes;
}

module Components : sig
  type 'a source

  val parse_string : name:string -> string -> _ source
  val parse_channel : name:string -> in_channel -> _ source

  val from_fun :
    name:string ->
    Typescheme.t Map.String.t ->
    Typescheme.Child.t Map.String.t ->
    'a ->
    'a source

  type 'a t

  val empty : _ t
  val make : 'a source list -> 'a t
end

val make : name:string -> 'a Components.t -> Lexing.lexbuf -> 'a t
val from_string : name:string -> 'a Components.t -> string -> 'a t
val from_channel : name:string -> 'a Components.t -> in_channel -> 'a t
