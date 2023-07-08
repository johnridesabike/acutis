(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module Data = struct
  type 'a t =
    [ `Int of int
    | `Float of float
    | `String of string
    | `Array of 'a t array
    | `Assoc of 'a t Map.String.t
    | `Unknown of 'a ]

  let null = `Int 0
  let some x = `Array [| x |]
  let list_empty = null
  let list_cons hd tl = `Array [| hd; tl |]

  let list_rev =
    let rec aux acc = function
      | `Array [| hd; tl |] -> aux (`Array [| hd; acc |]) tl
      | _ -> acc
    in
    fun l -> aux list_empty l

  let get_tuple = function
    | `Array t -> t
    | _ -> Error.internal __POS__ "Expected Array."

  let get_assoc = function
    | `Assoc t -> t
    | _ -> Error.internal __POS__ "Expected Assoc."

  let fold_list f acc l =
    let rec aux i acc = function
      | `Array [| hd; tl |] ->
          let acc = f ~index:(`Int i) acc hd in
          aux (succ i) acc tl
      | _ -> acc
    in
    aux 0 acc l

  let fold_assoc f m acc =
    Map.String.fold
      (fun k v acc -> f ~index:(`String k) acc v)
      (get_assoc m) acc
end

let rec test_case ~wildcard arg Matching.{ data; if_match; next } =
  let is_equal =
    match (arg, data) with
    | `String a, `String b -> String.equal a b
    | `Int a, `Int b -> Int.equal a b
    | `Float a, `Float b -> Float.equal a b
    | _ -> false
  in
  if is_equal then Some if_match
  else
    match next with
    | Some case -> test_case ~wildcard arg case
    | None -> wildcard

let bind_names data ids map =
  Set.Int.fold (fun id map -> Map.Int.add id data map) ids map

(* Use the [Not_found] exception to indicate that a nested path failed or that
   an optional field doesn't exist, and to seek an alternate path. *)

let tuple_get i a = a.(i)
let assoc_get = Map.String.find

let rec eval_match :
          'leaf 'args 'key.
          'args ->
          ('key -> 'args -> 'data Data.t) ->
          'data Data.t Map.Int.t ->
          ('leaf, 'key) Matching.tree ->
          'data Data.t Map.Int.t * 'leaf =
 fun args get vars -> function
  | End x -> (vars, x)
  | Switch { key; cases; wildcard; ids; _ } -> (
      let data = get key args in
      let vars = bind_names data ids vars in
      match test_case ~wildcard data cases with
      | Some tree -> eval_match args get vars tree
      | None -> raise_notrace Not_found)
  | Wildcard { key; ids; child } ->
      let data = get key args in
      let vars = bind_names data ids vars in
      eval_match args get vars child
  | Construct { key; ids; nil; cons } -> (
      let data = get key args in
      let vars = bind_names data ids vars in
      match (data, cons, nil) with
      | `Array _, Some tree, _ | _, _, Some tree ->
          eval_match args get vars tree
      | _ -> raise_notrace Not_found)
  | Nest { key; ids; child; wildcard } ->
      let data = get key args in
      let vars = bind_names data ids vars in
      let vars, tree =
        try
          match child with
          | Int_keys child ->
              let tuple = Data.get_tuple data in
              eval_match tuple tuple_get vars child
          | String_keys child ->
              let assoc = Data.get_assoc data in
              eval_match assoc assoc_get vars child
        with Not_found -> (
          match wildcard with
          | Some tree -> (vars, tree)
          | None -> raise_notrace Not_found)
      in
      eval_match args get vars tree
  | Optional { child; next } -> (
      try eval_match args get vars child
      with Not_found -> (
        match next with
        | Some t -> eval_match args get vars t
        | None -> raise_notrace Not_found))

let eval_match args Matching.{ tree; exits } =
  try
    let vars, Matching.{ names; exit } =
      eval_match args tuple_get Map.Int.empty tree
    in
    let bindings = Map.String.map (fun id -> Map.Int.find id vars) names in
    (bindings, Matching.Exit.get exits exit)
  with Not_found -> Error.internal __POS__ "Matching failed to find a match."

let add_escape b = function
  | '&' -> Buffer.add_string b "&amp;"
  | '"' -> Buffer.add_string b "&quot;"
  | '\'' -> Buffer.add_string b "&apos;"
  | '>' -> Buffer.add_string b "&gt;"
  | '<' -> Buffer.add_string b "&lt;"
  | '/' -> Buffer.add_string b "&#x2F;"
  | '`' -> Buffer.add_string b "&#x60;"
  | '=' -> Buffer.add_string b "&#x3D;"
  | c -> Buffer.add_char b c

let echo_format fmt data =
  match (fmt, data) with
  | Compile.Fmt_int, `Int i -> Int.to_string i
  | Fmt_float, `Float f -> Float.to_string f
  | Fmt_bool, `Int 0 -> "false"
  | Fmt_bool, _ -> "true"
  | Fmt_string, `String s -> s
  | _ ->
      Error.internal __POS__ "Type mismatch while formatting an echo statement."

let rec get_echo props = function
  | `Var var -> Map.String.find var props
  | `Field (var, field) ->
      get_echo props var |> Data.get_assoc |> Map.String.find field
  | `String _ as x -> x

let rec get_echo_list props fmt default = function
  | [] -> get_echo props default |> echo_format fmt
  | (fmt, var) :: tl -> (
      match get_echo props var with
      | `Array [| data |] -> echo_format fmt data
      | _ -> get_echo_list props fmt default tl)

let map_merge a b = Map.String.union (fun _ _ b -> Some b) a b

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type DATA = sig
  module Linear : sig
    type 'a t

    val length : 'a t -> int
    val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  end

  module Assoc : sig
    type 'a t

    val find_opt : string -> 'a t -> 'a option
    val fold : (string -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  end

  type t

  val classify :
    t ->
    [ `Null
    | `Bool of bool
    | `Int of int
    | `Float of float
    | `String of string
    | `Assoc of t Assoc.t
    | `List of t Linear.t ]

  val null : t
  val some : t -> t
  val of_float : float -> t
  val of_string : string -> t
  val of_bool : bool -> t
  val of_int : int -> t
  val of_seq : t Seq.t -> t
  val of_map : t Map.String.t -> t
  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type t
  type data

  val eval : (data -> t) Compile.t -> data -> t
end

module Make (M : MONAD) (D : DATA) = struct
  module Ty = Typescheme
  module EPath = Error.DecodePath

  type data = D.t
  type internal_data = D.t Data.t

  let decode_error = Error.decode D.pp
  let enum_error = Error.bad_enum D.pp

  let decode_boolean ty path cases j =
    let i =
      match D.classify j with
      | `Bool false -> 0
      | `Bool true -> 1
      | _ -> decode_error ty path j
    in
    if Set.Int.mem i cases then `Int i else enum_error ty path j

  let decode_string ty path cases j =
    match D.classify j with
    | `String s as x -> (
        match cases with
        | None -> x
        | Some cases ->
            if Set.String.mem s cases then x else enum_error ty path j)
    | _ -> decode_error ty path j

  let decode_int ty path cases j =
    match D.classify j with
    | `Int i as x -> (
        match cases with
        | None -> x
        | Some cases -> if Set.Int.mem i cases then x else enum_error ty path j)
    | _ -> decode_error ty path j

  let decode_float path j =
    match D.classify j with
    | `Float _ as x -> x
    | `Int i -> `Float (float_of_int i)
    | _ -> decode_error (Ty.float ()) path j

  let rec decode_nullable path ty j =
    match D.classify j with
    | `Null -> Data.null
    | _ -> Data.some (decode (EPath.nullable path) ty j)

  and decode_list path ty j =
    match D.classify j with
    | `List l ->
        D.Linear.fold_left
          (fun (i, acc) x ->
            (succ i, Data.list_cons (decode (EPath.index i path) ty x) acc))
          (0, Data.list_empty) l
        |> snd |> Data.list_rev
    | _ -> decode_error (Ty.list ty) path j

  and decode_assoc path ty j =
    match D.classify j with
    | `Assoc l ->
        `Assoc
          (D.Assoc.fold
             (fun k v map ->
               Map.String.add k (decode (EPath.key k path) ty v) map)
             l Map.String.empty)
    | _ -> decode_error (Ty.dict ty) path j

  and decode_tuple ty path tys j =
    match D.classify j with
    | `List l -> (
        let len = D.Linear.length l in
        let result = Array.make len Data.null in
        let _, extra_tys =
          D.Linear.fold_left
            (fun (i, tys) j ->
              match tys with
              | [] -> decode_error ty path j
              | ty :: tys ->
                  result.(i) <- decode (EPath.index i path) ty j;
                  (succ i, tys))
            (0, tys) l
        in
        match extra_tys with
        | [] -> `Array result
        | _ :: _ -> decode_error ty path j)
    | _ -> decode_error ty path j

  and decode_record_aux path tys j =
    Map.String.mapi
      (fun k ty ->
        match (ty, D.Assoc.find_opt k j) with
        | { contents = Ty.Nullable _ | Unknown _ }, None -> Data.null
        | ty, Some j -> decode (EPath.key k path) ty j
        | _ -> Error.missing_key path (Ty.internal_record (ref tys)) k)
      tys

  and decode_record path tys j =
    match D.classify j with
    | `Assoc m -> `Assoc (decode_record_aux path !tys m)
    | _ -> decode_error (Ty.internal_record tys) path j

  and decode_union path ty key Ty.{ cases; extra; row } j =
    match D.classify j with
    | `Assoc m -> (
        let tag =
          match D.Assoc.find_opt key m with
          | Some tag -> tag
          | None -> decode_error ty path j
        in
        let tag, tys =
          match (D.classify tag, cases, extra) with
          | `Bool false, Ty.Union.Int map, Bool ->
              let tag = 0 in
              (`Int tag, Map.Int.find_opt tag map)
          | `Bool true, Ty.Union.Int map, Bool ->
              let tag = 1 in
              (`Int tag, Map.Int.find_opt tag map)
          | (`Int i as tag), Ty.Union.Int map, Not_bool ->
              (tag, Map.Int.find_opt i map)
          | (`String s as tag), Ty.Union.String map, _ ->
              (tag, Map.String.find_opt s map)
          | _ -> decode_error ty path j
        in
        match (tys, row) with
        | Some tys, (`Open | `Closed) ->
            `Assoc (decode_record_aux path !tys m |> Map.String.add key tag)
        | None, `Open -> `Assoc (Map.String.singleton key tag)
        | None, `Closed -> decode_error ty path j)
    | _ -> decode_error ty path j

  and decode : EPath.t -> Ty.t -> data -> internal_data =
   fun path ty j ->
    match !ty with
    | Ty.Unknown _ -> `Unknown j
    | Nullable ty -> decode_nullable path ty j
    | Enum { extra = Bool; cases = Int cases; _ } ->
        decode_boolean ty path cases j
    | String | Enum { row = `Open; cases = String _; _ } ->
        decode_string ty path None j
    | Enum { row = `Closed; cases = String cases; _ } ->
        decode_string ty path (Some cases) j
    | Int | Enum { row = `Open; cases = Int _; _ } -> decode_int ty path None j
    | Enum { row = `Closed; cases = Int cases; _ } ->
        decode_int ty path (Some cases) j
    | Float -> decode_float path j
    | List ty -> decode_list path ty j
    | Dict (ty, _) -> decode_assoc path ty j
    | Tuple tys -> decode_tuple ty path tys j
    | Record tys -> decode_record path tys j
    | Union (key, variant) -> decode_union path ty key variant j

  let decode ~name tys j =
    match D.classify j with
    | `Assoc l -> decode_record_aux (EPath.make name) tys l
    | _ -> decode_error (Ty.internal_record (ref tys)) (EPath.make name) j

  let rec encode_record tys t =
    Map.String.merge
      (fun _ ty t ->
        match (ty, t) with Some ty, Some t -> Some (encode ty t) | _ -> None)
      tys t

  and encode_list ty l () =
    match l with
    | `Array [| hd; tl |] -> Seq.Cons (encode ty hd, encode_list ty tl)
    | _ -> Seq.Nil

  and encode : Ty.t -> internal_data -> data =
   fun ty t ->
    match (!ty, t) with
    | _, `Unknown j -> j
    | _, `Float f -> D.of_float f
    | _, `String s -> D.of_string s
    | Ty.Enum { extra = Bool; _ }, `Int 0 -> D.of_bool false
    | Enum { extra = Bool; _ }, `Int _ -> D.of_bool true
    | (Enum _ | Int | Unknown _), `Int i -> D.of_int i
    | Nullable ty, `Array [| t |] -> D.some @@ encode ty t
    | Nullable _, _ -> D.null
    | List ty, t -> D.of_seq @@ encode_list ty t
    | Tuple tys, `Array a ->
        D.of_seq @@ Seq.map2 encode (List.to_seq tys) (Array.to_seq a)
    | Unknown _, `Array a -> D.of_seq @@ Seq.map (encode ty) (Array.to_seq a)
    | Dict (ty, _), `Assoc m -> D.of_map @@ Map.String.map (encode ty) m
    | Unknown _, `Assoc m -> D.of_map @@ Map.String.map (encode ty) m
    | Record tys, `Assoc m -> D.of_map @@ encode_record !tys m
    | Union (k, { cases; extra; _ }), `Assoc m ->
        let tag = Map.String.find k m in
        let tys =
          match (cases, tag) with
          | Ty.Union.Int m, `Int i -> Map.Int.find i m
          | Ty.Union.String m, `String s -> Map.String.find s m
          | _ -> Error.internal __POS__ "Type mismatch while encoding a union."
        in
        let tag =
          match (extra, tag) with
          | Bool, `Int 0 -> D.of_bool false
          | Bool, `Int _ -> D.of_bool true
          | Not_bool, `Int i -> D.of_int i
          | _, `String s -> D.of_string s
          | _ ->
              Error.internal __POS__ "Union tags may only be ints or strings."
        in
        encode_record !tys m |> Map.String.add k tag |> D.of_map
    | _ -> Error.internal __POS__ "Type mismatch while encoding data."

  let encode tys j = D.of_map @@ encode_record tys j

  type t = string M.t

  let ( let* ) = M.bind

  let rec eval_data :
      blocks:string array ->
      vars:internal_data Map.String.t ->
      Compile.data ->
      internal_data =
   fun ~blocks ~vars -> function
    | `Null -> Data.null
    | (`Int _ | `String _ | `Float _) as x -> x
    | `Array a -> `Array (Array.map (eval_data ~blocks ~vars) a)
    | `Assoc d -> `Assoc (Map.String.map (eval_data ~blocks ~vars) d)
    | `Var x -> Map.String.find x vars
    | `Block i -> `String blocks.(i)
    | `Field (data, field) ->
        eval_data ~blocks ~vars data |> Data.get_assoc |> Map.String.find field

  let rec eval b vars nodes =
    List.fold_left
      (fun b -> function
        | Compile.Echo (nullables, fmt, default, esc) ->
            let* b = b in
            let str = get_echo_list vars fmt default nullables in
            (match esc with
            | Escape -> String.iter (add_escape b) str
            | No_escape -> Buffer.add_string b str);
            M.return b
        | Text s ->
            let* b = b in
            Buffer.add_string b s;
            M.return b
        | Match (blocks, args, tree) ->
            let* blocks = eval_array vars blocks in
            let args = Array.map (eval_data ~blocks ~vars) args in
            let vars', nodes = eval_match args tree in
            let vars = map_merge vars vars' in
            eval b vars nodes
        | Map_list (blocks, arg, tree) ->
            let* blocks = eval_array vars blocks in
            let l = eval_data ~blocks ~vars arg in
            Data.fold_list
              (fun ~index b arg ->
                let vars', nodes = eval_match [| arg; index |] tree in
                let vars = map_merge vars vars' in
                eval b vars nodes)
              b l
        | Map_dict (blocks, arg, tree) ->
            let* blocks = eval_array vars blocks in
            let d = eval_data ~blocks ~vars arg in
            Data.fold_assoc
              (fun ~index b arg ->
                let vars', nodes = eval_match [| arg; index |] tree in
                let vars = map_merge vars vars' in
                eval b vars nodes)
              d b
        | Component (_, comp, blocks, args) -> (
            let* blocks = eval_array vars blocks in
            let vars = Map.String.map (eval_data ~blocks ~vars) args in
            match comp with
            | Compile.Src nodes -> eval b vars nodes
            | Fun (types, f) ->
                let* result = f (encode types vars) in
                let* b = b in
                Buffer.add_string b result;
                M.return b))
      b nodes

  and eval_array vars a =
    let result = Array.make (Array.length a) String.empty in
    let* _idx =
      Array.fold_left
        (fun idx nodes ->
          let b = M.return @@ Buffer.create 1024 in
          let* idx = idx in
          let* b = eval b vars nodes in
          result.(idx) <- Buffer.contents b;
          M.return @@ succ idx)
        (M.return 0) a
    in
    M.return result

  let eval { Compile.nodes; types; name; _ } props =
    (* Wrap the props in a monad so it can catch decode exceptions. *)
    let* props = M.return props in
    let vars = decode ~name types props in
    let b = M.return @@ Buffer.create 2048 in
    let* result = eval b vars nodes in
    M.return @@ Buffer.contents result
end
