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

type ('a, 'b) t = {
  queue : string list;
  mutable notlinked : 'a MapString.t;
  mutable linked : 'b MapString.t;
  stack : string list;
  f : ('a, 'b) t -> 'a -> 'b;
}

let id _ a = a
let key (k, _) = k

let make ~f m =
  {
    queue = MapString.bindings m |> List.map key;
    notlinked = m;
    linked = MapString.empty;
    stack = [];
    f;
  }

let prelinked m =
  { queue = []; notlinked = MapString.empty; linked = m; stack = []; f = id }

let get k g =
  match MapString.find_opt k g.linked with
  | Some x -> x (* It was linked already in a previous search. *)
  | None -> (
      match MapString.find_opt k g.notlinked with
      | Some x ->
          (* Remove it form the unlinked map so a cycle isn't possible. *)
          g.notlinked <- MapString.remove k g.notlinked;
          let x = g.f { g with stack = k :: g.stack } x in
          g.linked <- MapString.add k x g.linked;
          x
      | None ->
          (* It is either being linked (in a cycle) or it doesn't exist. *)
          if List.exists (String.equal k) g.stack then Error.cycle (k :: g.stack)
          else Error.missing_component g.stack k)

let link_all g =
  let f k =
    match MapString.find_opt k g.notlinked with
    | Some x ->
        g.notlinked <- MapString.remove k g.notlinked;
        g.linked <-
          MapString.add k (g.f { g with stack = k :: g.stack } x) g.linked
    | None -> () (* It was already processed by a dependent. *)
  in
  List.iter f g.queue;
  g.linked
