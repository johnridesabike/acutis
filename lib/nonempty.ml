(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

type 'a t = ( :: ) of 'a * 'a list

let cons a (hd :: tl) = a :: hd :: tl
let hd (hd :: _) = hd

let rev =
  let rec aux h l1 l2 =
    match l1 with [] -> h :: l2 | h' :: l1 -> aux h' l1 List.(h :: l2)
  in
  fun (hd :: tl) -> aux hd tl []

let map f (hd :: tl) =
  (* Preserve evaluation order. *)
  let hd = f hd in
  hd :: List.map f tl

let map2 f (hd1 :: tl1) (hd2 :: tl2) =
  (* Preserve evaluation order. *)
  let hd = f hd1 hd2 in
  hd :: List.map2 f tl1 tl2

let to_list (hd :: tl) = List.( :: ) (hd, tl)
let to_seq (hd :: tl) () = Seq.Cons (hd, List.to_seq tl)
let to_sexp f l = Sexp.of_seq f (to_seq l)
