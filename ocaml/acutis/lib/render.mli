(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type DATA = sig
  type t

  val decode : Typescheme.t -> t -> t Data.t Map.Make(String).t
  val encode : Typescheme.t -> t Data.t Map.Make(String).t -> t
end

module Make (M : MONAD) (D : DATA) : sig
  type t = string M.t
  type data = D.t

  val make :
    (data -> t Map.Make(String).t -> t) Compile.template Compile.t -> data -> t
end
