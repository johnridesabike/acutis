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

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type DATA = sig
  type t

  val decode : Typescheme.t -> t -> t Data.t MapString.t
  val encode : Typescheme.t -> t Data.t MapString.t -> t
end

module type S = sig
  type t
  type data
  type component = data -> t MapString.t -> t

  val make : component Compile.t -> data -> t
end

module Make (M : MONAD) (D : DATA) :
  S with type t = string M.t and type data = D.t
