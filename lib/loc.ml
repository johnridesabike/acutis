(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

type t = Lexing.position * Lexing.position

let fname (start, _) = start.Lexing.pos_fname
let column pos = pos.Lexing.pos_cnum - pos.pos_bol + 1
let pp_pos ppf pos = Format.fprintf ppf "%d:%d" pos.Lexing.pos_lnum (column pos)
let pp ppf (start, end_) = Format.fprintf ppf "%a-%a" pp_pos start pp_pos end_
let dummy = (Lexing.dummy_pos, Lexing.dummy_pos)

(** Don't show locations. In development, this can be modified to show
    information as needed. *)
let to_sexp _ = Sexp.noop
