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

module Pattern : sig
  type constant = [ `Int of int | `String of string | `Float of float ]

  val equal_constant : constant -> constant -> bool

  type construct = TList | TNullable

  val pp_construct : Format.formatter -> construct -> unit
  val equal_construct : construct -> construct -> bool

  type t =
    | TConst of constant * Typescheme.Enum.t option
    | TConstruct of construct * t option
    | TTuple of t list
    | TRecord of
        (string * constant * Typescheme.t Typescheme.Union.t) option
        * t Map.String.t
        * Typescheme.t Map.String.t ref
    | TDict of t Map.String.t * Set.String.t ref
    | TVar of string
    | TAny

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

type echo =
  | Ech_var of string * Ast.escape
  | Ech_component of string
  | Ech_string of string

type node =
  | TText of string * Ast.trim * Ast.trim
  | TEcho of echo list * echo
  | TMatch of
      Loc.t * Pattern.t Nonempty.t * Typescheme.t Nonempty.t * case Nonempty.t
  | TMap_list of Loc.t * Pattern.t * Typescheme.t Nonempty.t * case Nonempty.t
  | TMap_dict of Loc.t * Pattern.t * Typescheme.t Nonempty.t * case Nonempty.t
  | TComponent of string * Pattern.t Map.String.t * child Map.String.t

and case = { pats : (Loc.t * Pattern.t Nonempty.t) Nonempty.t; nodes : nodes }
and child = TChild_name of string | TChild_block of nodes
and nodes = node list

type t = {
  nodes : nodes;
  prop_types : Typescheme.t Map.String.t;
  child_types : Typescheme.Child.t Map.String.t;
}

type ('a, 'b) source =
  | Src of string * 'a
  | Fun of
      string * Typescheme.t Map.String.t * Typescheme.Child.t Map.String.t * 'b

val make_components :
  (Ast.t, 'a) source Map.String.t -> (t, 'a) source Map.String.t

val make : root:string -> (t, 'a) source Map.String.t -> Ast.t -> t
