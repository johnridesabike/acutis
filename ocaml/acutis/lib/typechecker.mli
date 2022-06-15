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
        (string * constant * Typescheme.ty Typescheme.Union.t) option
        * t Map.Make(String).t
        * Typescheme.ty Map.Make(String).t ref
    | TDict of t Map.Make(String).t * Set.Make(String).t ref
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
  | TMatch of Loc.t * Pattern.t Nonempty.t * case Nonempty.t
  | TMap_list of Loc.t * Pattern.t * case Nonempty.t
  | TMap_dict of Loc.t * Pattern.t * case Nonempty.t
  | TComponent of string * Pattern.t Map.Make(String).t * child Map.Make(String).t

and case = { pats : (Loc.t * Pattern.t Nonempty.t) Nonempty.t; nodes : nodes }
and child = TChild_name of string | TChild_block of nodes
and nodes = node list

type t = {
  nodes : nodes;
  prop_types : Typescheme.t;
  child_types : Typescheme.Child.t;
}

val make_components :
  (Ast.t, 'a) Source.t Map.Make(String).t -> (t, 'a) Source.t Map.Make(String).t

val make : (t, 'a) Source.t Map.Make(String).t -> Ast.t -> t
