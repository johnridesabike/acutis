(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open StdlibExtra

type 'a node =
  | Text of string
  | Echo of Typechecker.echo list * Typechecker.echo
  | Match of Typechecker.Pattern.t array * 'a nodes Matching.t
  | Map_list of Typechecker.Pattern.t * 'a nodes Matching.t
  | Map_dict of Typechecker.Pattern.t * 'a nodes Matching.t
  | Component of 'a * Typechecker.Pattern.t MapString.t * 'a child MapString.t

and 'a child = Child_name of string | Child_block of 'a nodes
and 'a nodes = 'a node list

val make_nodes : Typechecker.t -> string nodes

type 'a t = { prop_types : Typescheme.t; nodes : 'a nodes }

type 'a template =
  | Acutis of string * 'a template nodes
  | Function of string * Typescheme.t * 'a

val parse_string : filename:string -> string -> Ast.t
(**
   @raises [Error.Error] on syntax error.
*)

module Components : sig
  type 'a t

  val empty : 'a t

  val make : (string, 'a) Source.t list -> 'a t
  (**
    @raises [Error.Error] on syntax error.
  *)
end

val make : filename:string -> 'a Components.t -> string -> 'a template t
(**
   @raises [Error.Error] on syntax error.
*)
