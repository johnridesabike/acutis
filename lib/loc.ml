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

let dummy = (Lexing.dummy_pos, Lexing.dummy_pos)
let pp ppf _ = Format.fprintf ppf "<loc>"
let equal _ _ = true (* Do not use location for equality in testing. *)