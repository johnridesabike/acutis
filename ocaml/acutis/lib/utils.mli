module MapString : Map.S with type key = string
module MapInt : Map.S with type key = int
module SetString : Set.S with type elt = string
module SetInt : Set.S with type elt = int

module DagMap : sig
  type ('a, 'b) t

  (** Use [get] inside the [f] callback. *)
  val make : f:('a -> ('a, 'b) t -> 'b) -> 'a MapString.t -> ('a, 'b) t
  val prelinked : 'a MapString.t -> ('a, 'a) t
  val get : string -> ('a, 'b) t -> 'b
  val link_all : ('a, 'b) t -> 'b MapString.t
end
