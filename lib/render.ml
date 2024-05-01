(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2023 John Jackson.                     *)
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
  val error : string -> 'a t
end

module type DECODABLE = sig
  type 'a linear

  val length : 'a linear -> int
  val iteri : (int -> 'a -> unit) -> 'a linear -> unit

  type 'a assoc

  val assoc_find : string -> 'a assoc -> 'a
  val assoc_mem : string -> 'a assoc -> bool
  val assoc_iter : (string -> 'a -> unit) -> 'a assoc -> unit

  type t

  val null : t
  val some : t -> t
  val of_float : float -> t
  val of_string : string -> t
  val of_bool : bool -> t
  val of_int : int -> t
  val of_array : t array -> t
  val of_assoc : (string * t) Seq.t -> t
  val decode_int : t -> int option
  val decode_string : t -> string option
  val decode_float : t -> float option
  val decode_bool : t -> bool option
  val decode_some : t -> t option
  val decode_linear : t -> t linear option
  val decode_assoc : t -> t assoc option
  val to_string : t -> string
end

module type S = sig
  type t
  type data

  val eval : (data -> t) Compile.t -> data -> t
end

module Make (M : MONAD) (D : DECODABLE) :
  S with type t = string M.t and type data = D.t = struct
  type t = string M.t
  type data = D.t

  include Instruct.Make (struct
    type 'a stmt = 'a

    let ( |: ) a b = a; b

    type 'a exp = 'a

    let return = Fun.id
    let stmt = Fun.id
    let ( let$ ) (_, x) f = f x

    type 'a mut = 'a ref

    let ( let& ) (_, x) f = f (ref x)
    let deref = ( ! )
    let ( := ) = ( := )
    let incr = incr
    let lambda = Fun.id
    let ( @@ ) = ( @@ )
    let if_ b ~then_ = if b then then_ ()
    let if_else b ~then_ ~else_ = if b then then_ () else else_ ()

    let while_ f b g =
      while f !b do
        g ()
      done

    let unit = ()
    let not = not
    let int = Fun.id
    let float = Fun.id
    let string = Fun.id
    let bool = Fun.id
    let equal_int = Int.equal
    let equal_string = String.equal
    let string_of_int = string_of_int
    let float_of_int = float_of_int
    let string_of_float = string_of_float
    let string_of_bool = string_of_bool

    type 'a obs = 'a

    let observe = Fun.id
    let array = Fun.id
    let array_make = Array.make
    let ( .%() ) = Array.get
    let ( .%()<- ) = Array.set

    module Tbl = Hashtbl.Make (String)

    type 'a hashtbl = 'a Tbl.t

    let hashtbl = Tbl.of_seq
    let hashtbl_create () = Tbl.create 16
    let ( .%{} ) = Tbl.find
    let ( .%{}<- ) = Tbl.replace (* Use [replace] so serialization works. *)
    let hashtbl_mem = Tbl.mem
    let hashtbl_copy = Tbl.copy
    let hashtbl_iter x f = Tbl.iter f x

    type buffer = Buffer.t

    let buffer_create () = Buffer.create 1024
    let buffer_add_string = Buffer.add_string
    let buffer_add_buffer = Buffer.add_buffer

    let buffer_add_escape b s =
      String.iter
        (function
          | '&' -> Buffer.add_string b "&amp;"
          | '"' -> Buffer.add_string b "&quot;"
          | '\'' -> Buffer.add_string b "&apos;"
          | '>' -> Buffer.add_string b "&gt;"
          | '<' -> Buffer.add_string b "&lt;"
          | '/' -> Buffer.add_string b "&#x2F;"
          | '`' -> Buffer.add_string b "&#x60;"
          | '=' -> Buffer.add_string b "&#x3D;"
          | c -> Buffer.add_char b c)
        s

    let buffer_contents = Buffer.contents
    let buffer_clear = Buffer.clear
    let buffer_length = Buffer.length

    type 'a promise = 'a M.t

    let promise = M.return
    let bind = M.bind
    let error = M.error

    module External = struct
      include D

      let of_hashtbl x = Tbl.to_seq x |> D.of_assoc

      type _ classify =
        | Int : int classify
        | String : string classify
        | Float : float classify
        | Bool : bool classify
        | Not_null : t classify
        | Linear : t linear classify
        | Assoc : t assoc classify

      let classify_opt : type a. a classify -> t -> a option = function
        | Int -> decode_int
        | String -> decode_string
        | Float -> decode_float
        | Bool -> decode_bool
        | Linear -> decode_linear
        | Assoc -> decode_assoc
        | Not_null -> decode_some

      let classify c t ~ok ~error =
        match classify_opt c t with Some x -> ok x | None -> error ()
    end

    module Data = struct
      type t =
        | Int of int
        | Float of float
        | String of string
        | Array of t array
        | Hashtbl of t hashtbl
        | Unknown of External.t

      let int x = Int x
      let float x = Float x
      let string x = String x
      let array x = Array x
      let hashtbl x = Hashtbl x
      let unknown x = Unknown x

      let to_int = function
        | Int x -> x
        | _ -> Error.internal ~__POS__ "Expected Int."

      let to_float = function
        | Float x -> x
        | _ -> Error.internal ~__POS__ "Expected Float."

      let to_string = function
        | String x -> x
        | _ -> Error.internal ~__POS__ "Expected String."

      let to_array = function
        | Array x -> x
        | _ -> Error.internal ~__POS__ "Expected Array."

      let to_hashtbl = function
        | Hashtbl x -> x
        | _ -> Error.internal ~__POS__ "Expected Hashtbl."

      let rec to_external_untyped = function
        | Unknown x -> x
        | Int x -> External.of_int x
        | Float x -> External.of_float x
        | String x -> External.of_string x
        | Array x -> Array.map to_external_untyped x |> External.of_array
        | Hashtbl x ->
            Tbl.to_seq x
            |> Seq.map (fun (k, v) -> (k, to_external_untyped v))
            |> External.of_assoc

      let rec equal a b =
        match (a, b) with
        | Int a, Int b -> Int.equal a b
        | Float a, Float b -> Float.equal a b
        | String a, String b -> String.equal a b
        | Array a, Array b -> Seq.equal equal (Array.to_seq a) (Array.to_seq b)
        | Hashtbl a, Hashtbl b ->
            Seq.equal
              (fun (k1, v1) (k2, v2) -> String.equal k1 k2 && equal v1 v2)
              (Tbl.to_seq a) (Tbl.to_seq b)
        | Int _, _
        | Float _, _
        | String _, _
        | Array _, _
        | Hashtbl _, _
        | Unknown _, _ ->
            false
    end

    type import = External.t -> string promise

    let import = ( |> )
    let export = Fun.id
  end)
end

module MakeString = Make (struct
  type 'a t = 'a

  let return = Fun.id
  let bind = ( |> )
  let error s = raise (Error.Acutis_error s)
end)
