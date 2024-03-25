(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** Compile patterns into decision trees. *)

(** Every pattern represents a one-dimensional path across a multi-dimensional
    data structure. A list of patterns is a two-dimensional matrix of paths. In
    order to transverse these paths efficiently, we need to combine them into a
    tree.

    We take advantage of a few properties that can make our tree simpler. We
    only test discrete static values: integers, strings, etc. We sort record
    fields and replace omitted fields with wildcards, so every pattern "lines
    up" with the others in 2D space. We also sort the tested values (the
    integers, strings, etc.).

    Every node has a set of integer IDs. These keep track of which values could
    potentially be bound to an identifier. Due to the merging and expanding of
    trees, nodes may have multiple IDs. Each leaf at the end of the branches
    contains a map of names to their IDs. This is necessary because multiple
    patterns may merge that use different or overlapping names for bindings.

    The most complicated part is how we handle wildcards. When we merge trees,
    each wildcard "expands" into its joining node. All of the nodes that come
    after the wildcard will also expand into all of the nodes after the other
    node. This has trade-offs. One advantage is that we can guarantee that every
    node is only visited once at runtime. One disadvantage is that some patterns
    may produce extremely large trees.

    Detecting redundant patterns is almost "free" with this strategy because
    merging a redundant tree fails to produce a new, different tree.
*)

(** {1 Example patterns and their resulting trees.}

    Here are a few example patterns juxtaposed with the trees they produce. The
    trees are written in pseudo-code, since the real trees are much more
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
    implicitly wrapped in a tuple-like structure.)

{v
{% match a, b
   with {a: 10, b: 11}, 12 %} a
{% with {b: 21, a: 20}, 22 %} b
{% with _, _ %} c
{% /match %}
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
{% /match %}
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
{% /match %}
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

(** {1 Type definitions.} *)

type internal_check_cases

(** This is a polymorphic "nested data type." Each tree can use itself as its
    own type variable, i.e. [(('a, 'key) tree, 'key) tree]. This allows the
    [End] nodes to be fully polymorphic. They can either lead to a {!leaf} or
    back to their containing tree. This type nesting corresponds to the nesting
    of physical patterns.

    One way to think about this is that our input patterns are non-nested types,
    but their physical structure can be nested in many dimensions. Our trees are
    the inverse. We need a tree that is always structurally two-dimensional, but
    its type can be nested on multiple dimensions.

    Nested types are simple to create, but complicated to manipulate. Functions
    cannot consume these types under normal polymorphism rules. We need to use
    explicitly polymorphic type annotations and GADTs. *)
type ('leaf, 'key) tree =
  | Switch of {
      key : 'key;
      ids : Set.Int.t;
      cases : ('leaf, 'key) switchcase;
      wildcard : ('leaf, 'key) tree option;
      check_cases : internal_check_cases;
    }
      (** A Switch represents a list of discreet values to test (i.e., [1],
          ["a"], etc.). If none of the values match the input, then the wildcard
          is used. *)
  | Nest of {
      key : 'key;
      ids : Set.Int.t;
      child : ('leaf, 'key) nest;
      wildcard : ('leaf, 'key) tree option;
    }  (** A Nest represents a structure such as tuple or a record. *)
  | Nil of { key : 'key; ids : Set.Int.t; child : ('leaf, 'key) tree }
      (** A [null] or [[]]. The [child] points to the next node in the tree. *)
  | Cons of { key : 'key; ids : Set.Int.t; child : ('leaf, 'key) tree }
      (** A not-null value or a non-empty list. The [child] points to a node
          representing the "current" data, either a {!Wildcard} or a {!Nest}. *)
  | Nil_or_cons of {
      key : 'key;
      ids : Set.Int.t;
      nil : ('leaf, 'key) tree;
      cons : ('leaf, 'key) tree;
    }  (** An exhaustive combination of {!Nil} and {!Cons}. *)
  | Wildcard of { key : 'key; ids : Set.Int.t; child : ('leaf, 'key) tree }
      (** Wildcards simply point to the next node in the tree.*)
  | Optional of { child : ('leaf, 'key) tree; next : ('leaf, 'key) tree option }
      (** Optionals are only used, and always used, inside of dictionary nests.
          They denote that the item in [child] does not need to be present
          during runtime. If following a [child] path fails, then follow the
          [next] path instead. [next] is analogous to a wildcard except that it
          cannot bind a value to an ID. *)
  | End of 'leaf

and ('leaf, 'key) nest =
  | Int_keys of (('leaf, 'key) tree, int) tree
  | String_keys of (('leaf, 'key) tree, string) tree

and ('leaf, 'key) switchcase = {
  data : [ `Int of int | `Float of float | `String of string ];
  if_match : ('leaf, 'key) tree;
  next : ('leaf, 'key) switchcase option;
}
(** The switch cases work like linked lists of values to test. If an input
    matches a value, then we follow its associated tree. If not, we try the next
    case. *)

module Exit : sig
  (** Each "exit" is given an integer key which we can use to look up the AST
      nodes to follow after the tree. We use integers because exits can be
      copied when trees merge, and we don't want to duplicate entire ASTs. *)

  type key
  (** Internally: [int] *)

  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val key_to_int : key -> int
  val to_seq : 'a t -> 'a Seq.t
  val to_seqi : 'a t -> (key * 'a) Seq.t
end

type leaf = { names : int Map.String.t; exit : Exit.key }
type 'a t = { tree : (leaf, int) tree; exits : 'a Exit.t }

(** {1 Functions.} *)

val make : Typechecker.case Nonempty.t -> Typechecker.nodes t

val partial_match_check :
  Loc.t -> Typechecker.Type.t list -> (leaf, int) tree -> unit
(** Searches the tree for a counterexample to prove it does not cover
    a case. Raises {!Error.Acutis_error} if it finds one. *)

val to_sexp : ('a -> Sexp.t) -> 'a t -> Sexp.t
