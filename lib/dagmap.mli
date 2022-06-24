(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** This is a utility to help {!Typechecker} and {!Compile} enforce that
 templates form a directed acyclic graph. *)

type ('a, 'b) t
(** ['a] is the type of values before linking into the graph. ['b] is the type
 of values after they've been linked.*)

val make :
  f:(('a, 'b) t -> 'a -> 'b) -> ?root:string -> 'a Map.String.t -> ('a, 'b) t
(**
   @param f Transforms ['a] to ['b]. Use {!get} inside the [f].
   @param root The root component, if one is defined. *)

val get : string -> ('a, 'b) t -> 'b
(** Gets the ['b] value associated with a string key.

 @raise Error.Error when a key does not exist or if the graph forms a cycle.*)

val prelinked : string -> 'a Map.String.t -> ('a, 'a) t
val link_all : ('a, 'b) t -> 'b Map.String.t
