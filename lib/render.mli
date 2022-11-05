(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** The main runtime for executing templates. *)

module type MONAD = sig
  (** Your standard monad type. *)

  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type DATA = sig
  (** A module type that can decode and encode data with {!Data.t}. *)

  type t

  val decode :
    name:string -> Typescheme.t Map.String.t -> t -> t Data.t Map.String.t

  val encode : Typescheme.t Map.String.t -> t Data.t Map.String.t -> t
end

module type S = sig
  (** Output signature for {!Make}. *)

  type t
  type data

  val make : (data -> t) Compile.t -> data -> t
  (** Apply data to a template and return the rendered output. *)
end

(** A functor that builds an implementation for a given monadic output type and
    a given data input type. *)
module Make (M : MONAD) (D : DATA) :
  S with type t = string M.t and type data = D.t
