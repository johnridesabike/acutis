(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** Type-checks the untyped {!Ast.t} and constructs a typed tree. *)

type echo =
  | Echo_var of string
  | Echo_string of string
  | Echo_field of echo * string

type construct = TList | TNullable

type pat =
  | TConst of Data.Const.t * Typescheme.Enum.t option
  | TConstruct of construct * pat option
  | TTuple of pat list
  | TRecord of
      (string * Data.Const.t * Typescheme.t Typescheme.Union.t) option
      * pat Map.String.t
      * Typescheme.t Map.String.t ref
  | TDict of pat Map.String.t * Set.String.t ref
  | TVar of string
  | TBlock of nodes
  | TField of pat * string
  | TAny

and node =
  | TText of string * Ast.trim * Ast.trim
  | TEcho of (Ast.echo_format * echo) list * Ast.echo_format * echo * Ast.escape
  | TMatch of Loc.t * pat Nonempty.t * Typescheme.t Nonempty.t * case Nonempty.t
  | TMap_list of Loc.t * pat * Typescheme.t Nonempty.t * case Nonempty.t
  | TMap_dict of Loc.t * pat * Typescheme.t Nonempty.t * case Nonempty.t
  | TComponent of string * pat Map.String.t

and case = { pats : (Loc.t * pat Nonempty.t) Nonempty.t; nodes : nodes }
and nodes = node list

type t = { nodes : nodes; types : Typescheme.t Map.String.t }

type ('a, 'b) source =
  | Src of string * 'a
  | Fun of string * Typescheme.t Map.String.t * 'b

val make_components :
  (Ast.t, 'a) source Map.String.t -> (t, 'a) source Map.String.t

val make : root:string -> (t, 'a) source Map.String.t -> Ast.t -> t
val make_interface_standalone : Ast.interface -> Typescheme.t Map.String.t
