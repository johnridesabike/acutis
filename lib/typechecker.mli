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

type echo = [ `Var of string | `String of string | `Field of echo * string ]
type scalar = [ `Int of int | `Float of float | `String of string ]

type tag =
  string * [ `Int of int | `String of string ] * Typescheme.t Typescheme.Union.t

type construct = private Construct_tag
type destruct = private Destruct_tag

(** We use a GADT to prove that certain patterns may only appear when
    constructing and certain patterns may only appear when destructuring. *)
type _ pat =
  | TScalar : scalar * Typescheme.Enum.t option -> 'a pat
  | TNil : 'a pat
  | TCons : 'a pat -> 'a pat  (** This always contains a [TTuple]. *)
  | TTuple : 'a pat list -> 'a pat
  | TRecord :
      tag option * 'a pat Map.String.t * Typescheme.t Map.String.t ref
      -> 'a pat
  | TDict : 'a pat Map.String.t * Set.String.t ref -> 'a pat
  | TVar : string -> 'a pat
  | TBlock : nodes -> construct pat
  | TField : construct pat * string -> construct pat
  | TAny : destruct pat

and node =
  | TText of string * Ast.trim * Ast.trim
  | TEcho of (Ast.echo_format * echo) list * Ast.echo_format * echo * Ast.escape
  | TMatch of
      Loc.t
      * construct pat Nonempty.t
      * Typescheme.t Nonempty.t
      * case Nonempty.t
  | TMap_list of
      Loc.t * construct pat * Typescheme.t Nonempty.t * case Nonempty.t
  | TMap_dict of
      Loc.t * construct pat * Typescheme.t Nonempty.t * case Nonempty.t
  | TComponent of string * construct pat Map.String.t

and case = {
  pats : (Loc.t * destruct pat Nonempty.t) Nonempty.t;
  nodes : nodes;
}

and nodes = node list

type t = { nodes : nodes; types : Typescheme.t Map.String.t }

type ('a, 'b) source =
  | Src of string * 'a
  | Fun of string * Typescheme.t Map.String.t * 'b

val make_components :
  (Ast.t, 'a) source Map.String.t -> (t, 'a) source Map.String.t

val make : root:string -> (t, 'a) source Map.String.t -> Ast.t -> t
val make_interface_standalone : Ast.interface -> Typescheme.t Map.String.t
