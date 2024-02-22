(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2023 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** The main runtime for executing templates. *)

module type CONCURRENT = sig
  (** A concurrency interface. *)

  type 'a promise
  type buffer

  val promise : 'a -> 'a promise
  val bind : 'a promise -> ('a -> 'b promise) -> 'b promise
  val promise_array : 'a promise array -> 'a array promise
  val buffer_create : unit -> buffer
  val buffer_add_string : buffer -> string -> unit
  val buffer_add_promise : buffer -> string promise -> unit
  val buffer_to_promise : buffer -> string promise
end

module type DECODABLE = sig
  (** Decode and encode input data. *)

  module Linear : sig
    (** A linear container such as a list or array. *)

    type 'a t

    val length : 'a t -> int
    val iteri : (int -> 'a -> unit) -> 'a t -> unit
  end

  module Assoc : sig
    (** A key-value container such as an association list, a string map, etc. *)

    type 'a t

    val find : string -> 'a t -> 'a
    val mem : string -> 'a t -> bool
    val iter : (string -> 'a -> unit) -> 'a t -> unit
  end

  type t

  (** Decoding *)

  val classify :
    t ->
    [ `Null
    | `Bool of bool
    | `Int of int
    | `Float of float
    | `String of string
    | `Linear of t Linear.t
    | `Assoc of t Assoc.t ]

  (** Encoding *)

  val null : t
  val some : t -> t
  val of_float : float -> t
  val of_string : string -> t
  val of_bool : bool -> t
  val of_int : int -> t
  val of_seq : t Seq.t -> t
  val of_assoc : (string * t) Seq.t -> t

  (** Debugging *)

  val pp : Format.formatter -> t -> unit
end

module type S = sig
  (** Output signature for {!Make}. *)

  type t
  type data

  val eval : (data -> t) Compile.t -> data -> t
  (** Apply data to a template and return the rendered output. *)
end

(** A functor that builds an implementation for a given concurrency interface
    and a given decodable input type. *)
module Make (C : CONCURRENT) (D : DECODABLE) :
  S with type t = string C.promise and type data = D.t

(** A simpler version of {!Make} that only requires a decodable module and
    outputs a string. *)
module MakeString (D : DECODABLE) : S with type t = string and type data = D.t
