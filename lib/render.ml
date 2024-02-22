(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2023 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module type CONCURRENT = sig
  type 'a promise
  type buffer

  val promise : 'a -> 'a promise
  val bind_array : 'a promise array -> ('a array -> 'b promise) -> 'b promise
  val buffer_create : unit -> buffer
  val buffer_add_string : buffer -> string -> unit
  val buffer_add_promise : buffer -> string promise -> unit
  val buffer_to_promise : buffer -> string promise
end

module type DECODABLE = sig
  module Linear : sig
    type 'a t

    val length : 'a t -> int
    val iteri : (int -> 'a -> unit) -> 'a t -> unit
  end

  module Assoc : sig
    type 'a t

    val find : string -> 'a t -> 'a
    val mem : string -> 'a t -> bool
    val iter : (string -> 'a -> unit) -> 'a t -> unit
  end

  type t

  val classify :
    t ->
    [ `Null
    | `Bool of bool
    | `Int of int
    | `Float of float
    | `String of string
    | `Linear of t Linear.t
    | `Assoc of t Assoc.t ]

  val null : t
  val some : t -> t
  val of_float : float -> t
  val of_string : string -> t
  val of_bool : bool -> t
  val of_int : int -> t
  val of_seq : t Seq.t -> t
  val of_assoc : (string * t) Seq.t -> t
  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type t
  type data

  val eval : (data -> t) Compile.t -> data -> t
end

module Make (C : CONCURRENT) (D : DECODABLE) :
  S with type t = string C.promise and type data = D.t = struct
  type t = string C.promise
  type data = D.t

  include Instruct.Make (struct
    include C

    type 'a stmt = 'a

    let ( |: ) a b = a; b

    type 'a exp = 'a

    let return = Fun.id
    let ( let$ ) (_, x) f = f x

    type 'a mut = 'a ref

    let ( let& ) (_, x) f = f (ref x)
    let deref = ( ! )
    let ( := ) = ( := )
    let incr = incr
    let lambda = Fun.id
    let ( @@ ) = ( @@ )

    let if_ b ~then_ ~else_ =
      if b then then_ () else match else_ with None -> () | Some f -> f ()

    let while_ f g =
      while f () do
        g ()
      done

    type external_data = D.t
    type import = D.t -> string promise

    let import = ( |> )
    let export = Fun.id

    type 'a obs = 'a

    let observe = Fun.id
    let unit = ()
    let not = not
    let int = Fun.id
    let float = Fun.id
    let string = Fun.id
    let bool = Fun.id
    let pair = Fun.id
    let equal_int = Int.equal
    let equal_string = String.equal
    let int_to_string = Int.to_string
    let int_to_float = Float.of_int
    let float_to_string = Float.to_string
    let bool_to_string = function 0 -> "false" | _ -> "true"
    let array = Fun.id
    let array_init = Array.make
    let ( .%() ) = Array.get
    let ( .%()<- ) = Array.set

    let concat_seq seq sep =
      match seq () with
      | Seq.Nil -> ""
      | Seq.Cons (hd, tl) ->
          let b = Buffer.create 16 in
          Buffer.add_string b hd;
          Seq.iter (fun s -> Buffer.add_string b sep; Buffer.add_string b s) tl;
          Buffer.contents b

    let array_concat x = concat_seq (Array.to_seq x)

    module Tbl = Hashtbl.Make (String)

    type 'a hashtbl = 'a Tbl.t

    let hashtbl = Tbl.of_seq
    let hashtbl_create () = Tbl.create 16
    let ( .%{} ) = Tbl.find
    let ( .%{}<- ) = Tbl.replace
    let hashtbl_mem = Tbl.mem
    let hashtbl_copy = Tbl.copy
    let hashtbl_iter x f = Tbl.iter f x

    let escape s =
      let b = Buffer.create (String.length s) in
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
        s;
      Buffer.contents b

    type 'a stack = 'a Stack.t

    let stack_create = Stack.create
    let stack_is_empty = Stack.is_empty
    let stack_push x a = Stack.push a x
    let stack_drop x = Stack.pop x |> ignore
    let stack_concat x = concat_seq (Stack.to_seq x)

    type error = exn

    let raise = raise
    let error s = Error.Acutis_error s

    type data =
      | Int of int
      | Float of float
      | String of string
      | Array of data array
      | Hashtbl of data hashtbl
      | Unknown of external_data

    module Data = struct
      type t = data exp

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

    module External = struct
      module Linear = struct
        type 'a t = 'a D.Linear.t

        let length = D.Linear.length
        let iteri x f = D.Linear.iteri f x
      end

      module Assoc = struct
        type 'a t = 'a D.Assoc.t

        let find x k = D.Assoc.find k x
        let mem x k = D.Assoc.mem k x
        let iter f x = D.Assoc.iter x f
      end

      type t = external_data exp

      let null = D.null
      let some = D.some
      let of_int = D.of_int
      let of_bool = function 0 -> D.of_bool false | _ -> D.of_bool true
      let of_string = D.of_string
      let of_float = D.of_float
      let of_array x = D.of_seq (Array.to_seq x)
      let of_hashtbl x = D.of_assoc (Tbl.to_seq x)

      let rec of_untyped = function
        | Unknown x -> x
        | Int x -> of_int x
        | Float x -> of_float x
        | String x -> of_string x
        | Array x -> D.of_seq @@ Seq.map of_untyped @@ Array.to_seq x
        | Hashtbl x ->
            D.of_assoc
            @@ Seq.map (fun (k, v) -> (k, of_untyped v))
            @@ Tbl.to_seq x

      type _ classify =
        | Int : int classify
        | String : string classify
        | Float : float classify
        | Bool : bool classify
        | Linear : external_data Linear.t classify
        | Assoc : external_data Assoc.t classify

      let classify (type a) (c : a classify) t ~(ok : a exp -> 'b stmt) ~error =
        match (c, D.classify t) with
        | Int, `Int x -> ok x
        | String, `String x -> ok x
        | Float, `Float x -> ok x
        | Bool, `Bool x -> ok x
        | Linear, `Linear x -> ok x
        | Assoc, `Assoc x -> ok x
        | _ -> error ()

      let is_null x = match D.classify x with `Null -> true | _ -> false

      let show =
        let b = Buffer.create 64 in
        let ppf = Format.formatter_of_buffer b in
        fun x ->
          D.pp ppf x;
          Format.pp_print_flush ppf ();
          let s = Buffer.contents b in
          Buffer.clear b; s
    end
  end)
end

module MakeString = Make (struct
  type 'a promise = 'a
  type buffer = Buffer.t

  let promise = Fun.id
  let bind_array = ( |> )
  let buffer_create () = Buffer.create 1024
  let buffer_add_string = Buffer.add_string
  let buffer_add_promise = Buffer.add_string
  let buffer_to_promise = Buffer.contents
end)
