(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open Utils

module Const : sig
  type t = [ `Int of int | `String of string | `Float of float ]

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

type t

val make : Typescheme.t -> (string * Source.json) list -> t MapString.t
val constant : t -> Const.t
val tuple : t -> t array
val dict : t -> t MapString.t
val is_null : t -> bool
val nullable : t -> t option
val of_pattern : vars:t MapString.t -> Typechecker.Pattern.t -> t
val iter_list : (index:t -> t -> unit) -> t -> unit
val iter_dict : (index:t -> t -> unit) -> t -> unit
val to_string : t -> string
val to_json : Typescheme.t -> t MapString.t -> (string * Source.json) list
