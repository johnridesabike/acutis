(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** Helpers for [Format] functions. *)

open Format

val comma : formatter -> unit -> unit
(** Prints [,@ ]. *)

val syntax_string : formatter -> string -> unit
(** Equivalent to the format string ["%S"]. *)

val at : (formatter -> 'a -> unit) -> formatter -> 'a -> unit
(** [at pp ppf a] prints [@a]. *)

val ellipsis : formatter -> unit -> unit
(** Prints [...] *)

val field : formatter -> string -> unit
(** Prints either [field] or ["string field"] depending on whether the input is
    a valid identifier. *)

val bool : formatter -> int -> unit
(** Prints [false] if the input is 0, or prints [true] otherwise. *)

val surround :
  left:char ->
  right:char ->
  (formatter -> 'a -> unit) ->
  formatter ->
  'a ->
  unit
(** [surround ~left ~right f ppf x] prints [x] with [left] and [right] printed
    before and after it, e.g. [{x}]. *)

val equation :
  sep:string ->
  (formatter -> 'key -> unit) ->
  (formatter -> 'value -> unit) ->
  formatter ->
  'key * 'value ->
  unit
(** [equation ~sep pp_k pp_v ppf (k, v)] prints [k sep v], e.g. [a = 1]. *)

module Ty_repr : sig
  (** Pretty-print typed representations of OCaml values. *)

  type t
  (** An intermediate type representation suitable for printing. *)

  type repr := t

  module type REPRABLE = sig
    type t

    val t : t -> repr
  end

  module type REPRABLE1 = sig
    type 'a t

    val t : ('a -> repr) -> 'a t -> repr
  end

  val ignore : _ -> t
  (** This never prints its argument. *)

  (** {1 Scalar types.} *)

  val unit : t
  val bool : bool -> t
  val int : int -> t
  val string : string -> t
  val float : float -> t
  val char : char -> t

  (** {1 Linear sequences.} *)

  val seq : ('a -> t) -> 'a Seq.t -> t

  module Linear (M : sig
    type 'a t

    val to_seq : 'a t -> 'a Seq.t
  end) : REPRABLE1 with type 'a t = 'a M.t

  (** {1 Product types.} *)

  val tuple2 : ('a -> t) -> ('b -> t) -> 'a * 'b -> t
  val tuple3 : ('a -> t) -> ('b -> t) -> ('c -> t) -> 'a * 'b * 'c -> t

  type fields
  (** Labeled arguments for record constructors. *)

  val fields : string -> t -> fields
  val field : string -> t -> fields -> fields
  val record : fields -> t

  (** {1 Sum types.} *)

  type args
  (** Positional arguments for variant constructors. *)

  val args : t -> args
  val ( * ) : args -> t -> args
  val variant : string -> args -> t
  val variant0 : string -> t
  val polyvar : string -> args -> t
  val polyvar0 : string -> t
  val variantr : string -> fields -> t

  (** {1 Abstract types.} *)

  val abstract : string -> args -> t
  val abstract0 : string -> t

  (** {1 Pretty-printing.} *)

  val pp : Format.formatter -> t -> unit

  (** {1 Stdlib conveniences.} *)

  val option : ('a -> t) -> 'a option -> t
  val list : ('a -> t) -> 'a list -> t
  val array : ('a -> t) -> 'a array -> t

  module type ORDERED_REPR = sig
    module Impl : Stdlib.Map.OrderedType
    include REPRABLE with type t = Impl.t
  end

  module String : sig
    module Impl = String
    include REPRABLE with type t = string
  end

  module Int : sig
    module Impl = Int
    include REPRABLE with type t = int
  end

  module Set (M : ORDERED_REPR) : REPRABLE with type t = Set.Make(M.Impl).t

  module Map (M : ORDERED_REPR) :
    REPRABLE1 with type 'a t = 'a Map.Make(M.Impl).t
end
