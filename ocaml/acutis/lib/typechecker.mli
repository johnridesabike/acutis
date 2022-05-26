(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open Utils

module Pattern : sig
  type constant = TString of string | TInt of int | TFloat of float

  val equal_constant : constant -> constant -> bool

  type construct = TList | TNullable
  val pp_construct: Format.formatter -> construct -> unit

  val equal_construct : construct -> construct -> bool

  type t =
    | TConst of constant * Typescheme.Enum.t option
    | TConstruct of construct * t option
    | TTuple of t list
    | TRecord of
        (string * constant * Typescheme.ty Typescheme.Union.t) option
        * t MapString.t
        * Typescheme.ty MapString.t ref
    | TDict of t MapString.t * SetString.t ref
    | TVar of string
    | TOptionalVar of string
    | TAny

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

type node =
  | TText of string * Ast.trim * Ast.trim
  | TEcho of Ast.echo list * Ast.echo
  | TMatch of Pattern.t Nonempty.t * case Nonempty.t
  | TMap_list of Pattern.t * case Nonempty.t
  | TMap_dict of Pattern.t * case Nonempty.t
  | TComponent of string * Pattern.t MapString.t * child MapString.t

and case = { pats : Pattern.t Nonempty.t Nonempty.t; nodes : nodes }
and child = TChild_name of string | TChild_block of nodes
and nodes = node list

type t = {
  nodes : nodes;
  prop_types : Typescheme.t;
  child_types : Typescheme.Child.t;
}

val make_components :
  (Ast.t, 'a Source.fn) Source.t MapString.t ->
  (t, 'a Source.fn) Source.t MapString.t

val make : (t, 'a Source.fn) Source.t MapString.t -> Ast.t -> t
