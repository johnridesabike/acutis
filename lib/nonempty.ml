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

let to_list (hd :: tl) = List.( :: ) (hd, tl)
let cons a (hd :: tl) = a :: hd :: tl
let hd (hd :: _) = hd

let rev (hd :: tl) =
  let rec aux l1 l2 = match l1 with [] -> l2 | a :: l -> aux l (cons a l2) in
  aux tl [ hd ]

let map f (hd :: tl) =
  (* Preserve evaluation order. *)
  let hd = f hd in
  hd :: List.map f tl

let map2 f (hd1 :: tl1) (hd2 :: tl2) =
  (* Preserve evaluation order. *)
  let hd = f hd1 hd2 in
  hd :: List.map2 f tl1 tl2

let equal f (h1 :: t1) (h2 :: t2) = f h1 h2 && List.equal f t1 t2

let pp pp_a ppf l =
  let open Format in
  fprintf ppf "@[<2>[%a@,]@]"
    (pp_print_list ~pp_sep:Pp.sep_semicolon pp_a)
    (to_list l)

let to_sexp f (hd :: tl) =
  Sexp.seq (Seq.cons (f hd) (List.to_seq tl |> Seq.map f))
