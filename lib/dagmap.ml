(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module M = Map.String

type ('a, 'b) t =
  | Not_linked of {
      not_linked : 'a M.t ref;
      linked : 'b M.t ref;
      stack : string list;
      f : ('a, 'b) t -> 'a -> 'b;
    }
  | Linked of string * 'b Map.String.t

let make ~f ?root not_linked =
  Not_linked
    {
      not_linked = ref not_linked;
      linked = ref M.empty;
      stack = Option.to_list root;
      f;
    }

let prelinked root linked = Linked (root, linked)

(** A template is either being linked (in a cycle) or it doesn't exist. *)
let error name stack =
  if List.exists (String.equal name) stack then Error.cycle (name :: stack)
  else Error.missing_component stack name

let get k = function
  | Linked (root, m) -> (
      match M.find_opt k m with Some x -> x | None -> error k [ root ])
  | Not_linked ({ linked; not_linked; stack; f } as g) -> (
      match M.find_opt k !linked with
      | Some x -> x (* It was linked already in a previous search. *)
      | None -> (
          match M.find_opt k !not_linked with
          | Some x ->
              (* Remove it from the unlinked map so a cycle isn't possible. *)
              not_linked := M.remove k !not_linked;
              (* Guarantee that [f] evaluates BEFORE [!linked] does. *)
              let x = f (Not_linked { g with stack = k :: stack }) x in
              linked := M.add k x !linked;
              x
          | None -> error k stack))

let link_all = function
  | Linked (_, m) -> m
  | Not_linked ({ linked; not_linked; stack; f } as g) ->
      M.iter
        (fun k _ ->
          match M.find_opt k !not_linked with
          | None -> () (* It was already processed by a dependent. *)
          | Some x ->
              not_linked := M.remove k !not_linked;
              (* Guarantee that [f] evaluates BEFORE [!linked] does. *)
              let x = f (Not_linked { g with stack = k :: stack }) x in
              linked := M.add k x !linked)
        !not_linked;
      !linked

let linked = function Linked (_, m) -> m | Not_linked { linked; _ } -> !linked
