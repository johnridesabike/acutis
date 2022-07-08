(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

type ('a, 'b) t = {
  queue : string list;
  mutable notlinked : 'a Map.String.t;
  mutable linked : 'b Map.String.t;
  stack : string list;
  f : ('a, 'b) t -> 'a -> 'b;
}

let id _ a = a
let key (k, _) = k

let make ~f ?root m =
  {
    queue = Map.String.bindings m |> List.map key;
    notlinked = m;
    linked = Map.String.empty;
    stack = (match root with Some s -> [ s ] | None -> []);
    f;
  }

let prelinked root m =
  {
    queue = [];
    notlinked = Map.String.empty;
    linked = m;
    stack = [ root ];
    f = id;
  }

let get k g =
  match Map.String.find_opt k g.linked with
  | Some x -> x (* It was linked already in a previous search. *)
  | None -> (
      match Map.String.find_opt k g.notlinked with
      | Some x ->
          (* Remove it form the unlinked map so a cycle isn't possible. *)
          g.notlinked <- Map.String.remove k g.notlinked;
          let x = g.f { g with stack = k :: g.stack } x in
          g.linked <- Map.String.add k x g.linked;
          x
      | None ->
          (* It is either being linked (in a cycle) or it doesn't exist. *)
          if List.exists (String.equal k) g.stack then Error.cycle (k :: g.stack)
          else Error.missing_component g.stack k)

let link_all g =
  let f k =
    match Map.String.find_opt k g.notlinked with
    | Some x ->
        g.notlinked <- Map.String.remove k g.notlinked;
        g.linked <-
          Map.String.add k (g.f { g with stack = k :: g.stack } x) g.linked
    | None -> () (* It was already processed by a dependent. *)
  in
  List.iter f g.queue;
  g.linked
