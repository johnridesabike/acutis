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

module type MONAD = sig
  (** A standard monad interface for async operations. *)

  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val error : string -> 'a t
end

module type DECODABLE = sig
  (** Decode and encode input data. *)

  (** {1 Container types} *)

  type 'a linear
  (** A linear container such as a list or array. *)

  val length : 'a linear -> int
  val iteri : (int -> 'a -> unit) -> 'a linear -> unit

  type 'a assoc
  (** A key-value container such as an association list or a string map. *)

  val assoc_find : string -> 'a assoc -> 'a
  val assoc_mem : string -> 'a assoc -> bool
  val assoc_iter : (string -> 'a -> unit) -> 'a assoc -> unit

  type t

  (** {1 Encoding} *)

  val null : t
  val some : t -> t
  val of_float : float -> t
  val of_string : string -> t
  val of_bool : bool -> t
  val of_int : int -> t
  val of_array : t array -> t
  val of_assoc : (string * t) Seq.t -> t

  (** {1 Decoding} *)

  val decode_int : t -> int option
  val decode_string : t -> string option
  val decode_float : t -> float option
  val decode_bool : t -> bool option
  val decode_some : t -> t option
  val decode_linear : t -> t linear option
  val decode_assoc : t -> t assoc option

  (** {1 Debugging} *)

  val to_string : t -> string
end

module type S = sig
  (** Output signature for {!Make}. *)

  type t
  type data

  val eval : (data -> t) Compile.t -> data -> t
  (** Apply data to a template and return the rendered output. *)
end

(** A functor that builds an implementation for a given monad interface and a
    given decodable input type. *)
module Make (M : MONAD) (D : DECODABLE) :
  S with type t = string M.t and type data = D.t

(** A simpler version of {!Make} that only requires a decodable module and
    outputs a string. *)
module MakeString (D : DECODABLE) : S with type t = string and type data = D.t
