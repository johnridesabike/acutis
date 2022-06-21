(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** This compiles patterns into decision trees. *)

(** Every pattern represents a one-dimensional path across a multi-dimensional
  data structure. A list of patterns is a two-dimensional matrix of paths. In
  order to transverse these paths efficiently, we need to combine them into a
  tree.

  We take advantage of a few properties that can make our tree simpler. We only
  test discrete static values, integers, strings, etc. We sort record fields and
  replace ommited fields with wildcards, so every pattern "lines up" with the
  others. We also sort the tested values (the integers, strings, etc.).

  Every node has a set of integer "IDs." These keep track of bindings. If a
  pattern has a binding, then we store that binding's ID in its node. Due to
  merging and expanding trees, nodes can have multiple IDs. Each leaf contains a
  map of binding names to their IDs. This is necessary because multiple patterns
  may merge that use different or overlapping binding names.

  The most complicated part of this tree is how we handle wildcards. When we
  merge trees, each wildcard "expands" into its joining node. All of the nodes
  that come after the wildcard will also expand into all of the nodes after the
  other node. This has tradeoffs. One advantage is that we can guarantee that
  every node is only visited once at runtime. One disadvantage is that some
  patterns may produce extremely large trees.

  One feature that this structure gives us is that redundant patterns are
  automatically detected because merging them with an existing tree fails. The
  tree type is a polymorphic "nested" type. Each tree can use itself as its own
  type variable, i.e. ['a tree tree]. This allows the end nodes to be fully
  polymorphic. They can either lead to a leaf or back to their containing tree.
  This nesting corresponds to the nesting of patterns.

  Nested types are simple to create, but complicated to manipulate. Functions
  cannot consume these types under normal polymorphism rules. We need to use
  explicitly polymorphic type annotations and GADTs.
*)

(** This is extra information for debugging. *)
type extra_nest_info =
  | Tuple
  | Dict
  | Record
  | Union of Typescheme.Variant.extra

type extra_switch_info = Extra_none | Extra_enum_closed

type ('leaf, 'key) tree =
  | Switch of {
      key : 'key;
      ids : Set.Int.t;
      cases : ('leaf, 'key) switchcase;
      extra : extra_switch_info;
      wildcard : ('leaf, 'key) tree option;
    }
      (**
    A Switch represents a list of discreet values to test (i.e., 1, "a", etc.).
    If none of the values match the input, then the wildcard is used.
 *)
  | Nest of {
      key : 'key;
      ids : Set.Int.t;
      child : ('leaf, 'key) nest;
      wildcard : ('leaf, 'key) tree option;
      extra : extra_nest_info;
    }  (** A Nest represents a structure such as tuple or a record. *)
  | Construct of {
      key : 'key;
      ids : Set.Int.t;
      nil : ('leaf, 'key) tree option;
      cons : ('leaf, 'key) tree option;
      extra : Typechecker.Pattern.construct;
    }
      (** A Construct represents one of the built-in variant types: lists and
    nullables. nil represents an empty list or a null value. It is like a
    wildcard in that it always points to the *next* node, whatever that may be.
    cons always either points to a wildcard node or a Nest + Tuple node.*)
  | Wildcard of { key : 'key; ids : Set.Int.t; child : ('leaf, 'key) tree }
      (** Wildcards simply point to the next node in the tree.*)
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
  (** This maps interger codes to exit nodes. *)

  type key
  type 'a t

  val get : 'a t -> key -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val unsafe_key : int -> key
end

type leaf = { names : int Map.String.t; exit : Exit.key }
type 'a t = { tree : (leaf, int) tree; exits : 'a Exit.t }

val equal_leaf : leaf -> leaf -> bool
val pp_leaf : Format.formatter -> leaf -> unit
val make : Typechecker.case Nonempty.t -> Typechecker.nodes t
val partial_match_check : Loc.t -> ('a, int) tree -> unit
(** Searches the tree for a counter-example to prove it does not cover
    a case. Raises {!Error.Error} if one is found. *)
