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

(* This is extra information for debugging. *)
type extra_nest_info =
  | Tuple
  | Dict
  | Record
  | Union of Typescheme.Variant.extra

type extra_switch_info = Extra_none | Extra_enum_closed

type ('leaf, 'key) tree =
  (*
    A Switch represents a list of discreet values to test (i.e., 1, "a", etc.).
    If none of the values match the input, then the wildcard is used.
 *)
  | Switch of {
      key : 'key;
      ids : Set.Make(Int).t;
      cases : ('leaf, 'key) switchcase;
      extra : extra_switch_info;
      wildcard : ('leaf, 'key) tree option;
    }
  (*
    A Nest represents a structure such as tuple or a record.
 *)
  | Nest of {
      key : 'key;
      ids : Set.Make(Int).t;
      child : ('leaf, 'key) nest;
      wildcard : ('leaf, 'key) tree option;
      extra : extra_nest_info;
    }
  (*
    A Construct represents one of the built-in variant types: lists and
    nullables.
    nil represents an empty list or a null value. It is like a wildcard in that
    it always points to the *next* node, whatever that may be.
    cons always either points to a wildcard node or a Nest + Tuple node.
 *)
  | Construct of {
      key : 'key;
      ids : Set.Make(Int).t;
      nil : ('leaf, 'key) tree option;
      cons : ('leaf, 'key) tree option;
      extra : Typechecker.Pattern.construct;
    }
  (*
    Wildcards simply point to the next node in the tree.
 *)
  | Wildcard of {
      key : 'key;
      ids : Set.Make(Int).t;
      child : ('leaf, 'key) tree;
    }
  | End of 'leaf

and ('leaf, 'key) nest =
  | IntKeys of (('leaf, 'key) tree, int) tree
  | StringKeys of (('leaf, 'key) tree, string) tree

(*
  The switch cases work like linked lists of values. If an input matches a
  value, then we follow its associated tree. If not, we try the next case.
*)
and ('leaf, 'key) switchcase = {
  data : Data.Const.t;
  if_match : ('leaf, 'key) tree;
  next_case : ('leaf, 'key) switchcase option;
}

val equal_tree :
  ('leaf -> 'leaf -> bool) ->
  ('key -> 'key -> bool) ->
  ('leaf, 'key) tree ->
  ('leaf, 'key) tree ->
  bool

val pp_tree :
  (Format.formatter -> 'leaf -> unit) ->
  (Format.formatter -> 'key -> unit) ->
  Format.formatter ->
  ('leaf, 'key) tree ->
  unit

module Exit : sig
  type key
  type 'a t

  val get : 'a t -> key -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val unsafe_key : int -> key
end

type leaf = { names : int Map.Make(String).t; exit : Exit.key }
type 'a t = { tree : (leaf, int) tree; exits : 'a Exit.t }

val equal_leaf : leaf -> leaf -> bool
val pp_leaf : Format.formatter -> leaf -> unit
val make : Typechecker.case Nonempty.t -> Typechecker.nodes t
val partial_match_check : Loc.t -> ('a, int) tree -> unit
