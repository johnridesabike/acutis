(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2023 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Symbol of string
  | Int of int
  | String of string
  | Float of float
  | Noop
  | List of t Seq.t
      (** Since a lot of data types will need to convert to this, we'll just
          use Seq.t internally. *)

let make hd l = List (fun () -> Cons (Symbol hd, List.to_seq l))
let symbol s = Symbol s
let string s = String s
let int i = Int i
let bool = function 0 -> Symbol "false" | _ -> Symbol "true"
let float f = Float f
let of_seq f l = List (Seq.map f l)
let noop = Noop
let option f = function None -> Symbol "none" | Some x -> f x

let pair f g (a, b) =
  List (fun () -> Cons (f a, fun () -> Cons (g b, fun () -> Nil)))

let triple f g h (a, b, c) =
  List
    (fun () ->
      Cons (f a, fun () -> Cons (g b, fun () -> Cons (h c, fun () -> Nil))))

let map_string f m = of_seq (pair string f) (Map.String.to_seq m)

let rec pp ppf = function
  | Symbol s -> Format.pp_print_string ppf s
  | String s -> Pp.syntax_string ppf s
  | Int i -> Format.pp_print_int ppf i
  | Float f -> Format.pp_print_float ppf f
  | Noop -> ()
  | List l ->
      Format.(
        fprintf ppf "@[<hv 1>(%a)@]"
          (pp_print_seq ~pp_sep:pp_print_space pp)
          (Seq.filter (function Noop -> false | _ -> true) l))
