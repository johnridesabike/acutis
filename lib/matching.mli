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

    We take advantage of a few properties that can make our tree simpler. We
    only test discrete static values, integers, strings, etc. We sort record
    fields and replace ommited fields with wildcards, so every pattern "lines
    up" with the others. We also sort the tested values (the integers, strings,
    etc.).

    Every node has a set of integer "IDs." These keep track of bindings. If a
    pattern has a binding, then we store that binding's ID in its node. Due to
    merging and expanding trees, nodes can have multiple IDs. Each leaf contains
    a map of binding names to their IDs. This is necessary because multiple
    patterns may merge that use different or overlapping binding names.

    The most complicated part of this tree is how we handle wildcards. When we
    merge trees, each wildcard "expands" into its joining node. All of the nodes
    that come after the wildcard will also expand into all of the nodes after
    the other node. This has tradeoffs. One advantage is that we can guarantee
    that every node is only visited once at runtime. One disadvantage is that
    some patterns may produce extremely large trees.

    One feature that this structure gives us is that redundant patterns are
    automatically detected because merging them with an existing tree fails. The
    tree type is a polymorphic "nested" type. Each tree can use itself as its
    own type variable, i.e. ['a tree tree]. This allows the end nodes to be
    fully polymorphic. They can either lead to a leaf or back to their
    containing tree. This nesting corresponds to the nesting of patterns.
  
    Nested types are simple to create, but complicated to manipulate. Functions
    cannot consume these types under normal polymorphism rules. We need to use
    explicitly polymorphic type annotations and GADTs. *)

(** {1 Example patterns and their resulting trees.}

    Here are a few example patterns juxtaposed with the trees they produce. The
    trees are written in pesudo-code, since the real trees are much more
    verbose.

    {2 A basic list of integers.}

{v
{% match a
   with 0  %} a
{% with 10 %} b
{% with 20 %} c
{% with _  %} d
{% /match %}
v}

{v
|- case 0 -> a
|- case 10 -> b
|- case 20 -> c
|- wildcard -> d
v}

    {2 A record nested in a tuple. }

    (Note: internally, all arguments passed to a [match] statement are
    implicitly wrapped in a tuple-like construct.)

{v
{% match a, b
   with {a: 10, b: 11}, 12 %} a
{% with {b: 21, a: 20}, 22 %} b
{% with _, _ %} c
{% /match %}|}
v}

{v
key 0
  |- begin nest
      |- key "a"
      |   |- case 10
      |   |   |- key "b"
      |   |       |- case 11
      |   |           |- end nest
      |   |               |- key 1
      |   |                   |- case 12 -> a
      |   |                   |- wildcard -> c
      |   |- case 20
      |       |- key "b"
      |           |- case 21
      |               |- end nest
      |                   |- key 1
      |                       |- case 22 -> b
      |                       |- wildcard -> c
      |- wildcard
          |----------------- key 1
                              |- wildcard -> c
v}

    {2 A list. }

    Remember that the [[1, 2, ...tl]] list syntax is basically sugar for
    [!(1, !(2, tl))].

    {v
{% match a
   with [] %} a
{% with [x] %} b
{% with [x, ...y] %} c
{% /match %}|}
v}

{v
|- nil -> a
|- cons
    |- begin nest
        |- key 0
            |- wildcard
                |- key 1
                    |- nil
                    |  |- end nest -> b
                    |
                    |- cons
                        |- wildcard
                            |- end nest -> c
v}

    {2 Nested tuples with wildcards.}

    We can see the effect of wildcard expansion. Some paths are duplicated
    throughout the tree. For larger, more complex patterns, this can create
    unexpectedly enormous trees.

{v
{% match              a,  b
   with               x, 41 %} a
{% with  ((10, 20), 30), 40 %} b
{% with               y,  z %} c
{% /match %}|}
v}

{v
key 0
  |- begin nest
      |- key 0
      |   |- begin nest
      |       |- key 0
      |           |- case 10
      |               |- key 1
      |                   |- case 20
      |                       |- end nest
      |                           |- key 1
      |                               |- case 30
      |                                   |- end nest
      |                                       |- key 1
      |                                           |- case 40 -> b
      |                                           |- case 41 -> a
      |                                           |- wildcard -> c
      |- wildcard
          |------------------------------------- key 1
                                                  |- case 41 -> a
                                                  |- wildcard -> c

v}
    *)

(** {1 Type definitions.}*)

type debug_nest_info =
  | Tuple
  | Dict
  | Record
  | Union of Typescheme.Variant.extra

type ('leaf, 'key) tree =
  | Switch of {
      key : 'key;
      ids : Set.Int.t;
      cases : ('leaf, 'key) switchcase;
      row : [ `Open | `Closed ];
      wildcard : ('leaf, 'key) tree option;
    }
      (** A Switch represents a list of discreet values to test (i.e., [1],
          ["a"], etc.). If none of the values match the input, then the wildcard
          is used. *)
  | Nest of {
      key : 'key;
      ids : Set.Int.t;
      child : ('leaf, 'key) nest;
      wildcard : ('leaf, 'key) tree option;
      debug : debug_nest_info;
    }  (** A Nest represents a structure such as tuple or a record. *)
  | Construct of {
      key : 'key;
      ids : Set.Int.t;
      nil : ('leaf, 'key) tree option;
      cons : ('leaf, 'key) tree option;
      debug : Typechecker.Pattern.construct;
    }
      (** A Construct represents one of the built-in variant types: lists and 
          nullables. [nil] represents an empty list or a null value. It is like
          a wildcard in that it always points to the {b next} node, whatever
          that may be. [cons] always points to a {!Wildcard} or {!Nest} node.*)
  | Wildcard of { key : 'key; ids : Set.Int.t; child : ('leaf, 'key) tree }
      (** Wildcards simply point to the next node in the tree.*)
  | End of 'leaf

(** A nest creates a "nested data type," a {!tree} with another [tree] defined
 as its own ['leaf] type parameter. The [tree] parameter represents the base
 tree that contains this new tree. When the new tree ends, we return to the
 base tree. *)
and ('leaf, 'key) nest =
  | Int_keys of (('leaf, 'key) tree, int) tree
  | String_keys of (('leaf, 'key) tree, string) tree

and ('leaf, 'key) switchcase = {
  data : Data.Const.t;
  if_match : ('leaf, 'key) tree;
  next_case : ('leaf, 'key) switchcase option;
}
(** The switch cases work like linked lists of values. If an input matches a
  value, then we follow its associated tree. If not, we try the next case. *)

module Exit : sig
  (** Each "exit" is given an integer key which we can use to look up the AST
      nodes to follow after the tree. We use integers as a level of indirection
      because exits can be copied when trees expand, and we don't want to
      duplicate entire ASTs. *)

  type key
  (** Internally: [int] *)

  type 'a t

  val get : 'a t -> key -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val unsafe_key : int -> key
end

type leaf = { names : int Map.String.t; exit : Exit.key }
type 'a t = { tree : (leaf, int) tree; exits : 'a Exit.t }

(** {1 Functions.} *)

val make : Typechecker.case Nonempty.t -> Typechecker.nodes t

val partial_match_check : Loc.t -> (_, int) tree -> unit
(** Searches the tree for a counter-example to prove it does not cover
    a case. Raises {!Error.Error} if one is found. *)

(** {1 Functions for tests.} *)

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

val equal_leaf : leaf -> leaf -> bool
val pp_leaf : Format.formatter -> leaf -> unit
