(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

let rec test_case ~wildcard arg Matching.{ data; if_match; next } =
  if Data.Const.equal arg data then Some if_match
  else
    match next with
    | Some case -> test_case ~wildcard arg case
    | None -> wildcard

let bind_names data ids map =
  Set.Int.fold (fun id map -> Map.Int.add id data map) ids map

(* Use the [Not_found] exception to indicate that a nested path failed or that
   an optional field doesn't exist, and to seek an alternate path. *)

let tuple_get i a = a.(i)
let dict_get = Map.String.find

let rec make_match :
          'a 'args 'key.
          'args ->
          ('key -> 'args -> 'data Data.t) ->
          'data Data.t Map.Int.t ->
          ('a, 'key) Matching.tree ->
          'data Data.t Map.Int.t * 'a =
 fun args get vars -> function
  | End x -> (vars, x)
  | Switch { key; cases; wildcard; ids; _ } -> (
      let data = get key args in
      let vars = bind_names data ids vars in
      let data = Data.get_const data in
      match test_case ~wildcard data cases with
      | Some tree -> make_match args get vars tree
      | None -> raise_notrace Not_found)
  | Wildcard { key; ids; child } ->
      let data = get key args in
      let vars = bind_names data ids vars in
      make_match args get vars child
  | Construct { key; ids; nil; cons } -> (
      let data = get key args in
      let vars = bind_names data ids vars in
      match if Data.is_null data then nil else cons with
      | Some tree -> make_match args get vars tree
      | None -> raise_notrace Not_found)
  | Nest { key; ids; child; wildcard } ->
      let data = get key args in
      let vars = bind_names data ids vars in
      let vars, tree =
        try
          match child with
          | Int_keys child ->
              let tuple = Data.get_tuple data in
              make_match tuple tuple_get vars child
          | String_keys child ->
              let dict = Data.get_dict data in
              make_match dict dict_get vars child
        with Not_found -> (
          match wildcard with
          | Some tree -> (vars, tree)
          | None -> raise_notrace Not_found)
      in
      make_match args get vars tree
  | Optional { child; next } -> (
      try
        match child with
        | Some t -> make_match args get vars t
        | None -> raise_notrace Not_found
      with Not_found -> (
        match next with
        | Some t -> make_match args get vars t
        | None -> raise_notrace Not_found))

let make_match args Matching.{ tree; exits } =
  try
    let vars, Matching.{ names; exit } =
      make_match args tuple_get Map.Int.empty tree
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
  | Compile.Fmt_int, Data.Const (Int i) -> Int.to_string i
  | Fmt_float, Const (Float f) -> Float.to_string f
  | Fmt_bool, Const (Int 0) -> "false"
  | Fmt_bool, _ -> "true"
  | Fmt_string, Const (String s) -> s
  | _ ->
      Error.internal __POS__ "Type mismatch while formatting an echo statement."

let rec get_echo props = function
  | Compile.Echo_var var -> Map.String.find var props
  | Echo_field (var, field) ->
      get_echo props var |> Data.get_dict |> Map.String.find field
  | Echo_string s -> Data.string s

let rec get_echo_list props fmt default = function
  | [] -> get_echo props default |> echo_format fmt
  | (fmt, var) :: tl -> (
      match Data.get_nullable (get_echo props var) with
      | Some data -> echo_format fmt data
      | None -> get_echo_list props fmt default tl)

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

  val make : (data -> t) Compile.t -> data -> t
end

module Make (M : MONAD) (D : DATA) = struct
  module Ty = Typescheme
  module EPath = Error.DecodePath

  type data = D.t

  let decode_error = Error.decode D.pp
  let enum_error = Error.bad_enum D.pp

  let decode_boolean ty path cases j =
    let i =
      match D.classify j with
      | `Bool false -> 0
      | `Bool true -> 1
      | _ -> decode_error ty path j
    in
    if Set.Int.mem i cases then Data.bool i else enum_error ty path j

  let decode_string ty path cases j =
    match D.classify j with
    | `String s -> (
        match cases with
        | None -> Data.string s
        | Some cases ->
            if Set.String.mem s cases then Data.string s
            else enum_error ty path j)
    | _ -> decode_error ty path j

  let decode_int ty path cases j =
    match D.classify j with
    | `Int i -> (
        match cases with
        | None -> Data.int i
        | Some cases ->
            if Set.Int.mem i cases then Data.int i else enum_error ty path j)
    | _ -> decode_error ty path j

  let decode_float path j =
    match D.classify j with
    | `Float f -> Data.float f
    | `Int i -> Data.float (float_of_int i)
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

  and decode_dict path ty j =
    match D.classify j with
    | `Assoc l ->
        D.Assoc.fold
          (fun k v map -> Map.String.add k (decode (EPath.key k path) ty v) map)
          l Map.String.empty
        |> Data.dict
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
        | [] -> Data.array result
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
    | `Assoc m -> decode_record_aux path !tys m |> Data.dict
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
              (Data.bool tag, Map.Int.find_opt tag map)
          | `Bool true, Ty.Union.Int map, Bool ->
              let tag = 1 in
              (Data.bool tag, Map.Int.find_opt tag map)
          | `Int tag, Ty.Union.Int map, Not_bool ->
              (Data.int tag, Map.Int.find_opt tag map)
          | `String tag, Ty.Union.String map, _ ->
              (Data.string tag, Map.String.find_opt tag map)
          | _ -> decode_error ty path j
        in
        match (tys, row) with
        | Some tys, (`Open | `Closed) ->
            decode_record_aux path !tys m |> Map.String.add key tag |> Data.dict
        | None, `Open -> Map.String.singleton key tag |> Data.dict
        | None, `Closed -> decode_error ty path j)
    | _ -> decode_error ty path j

  and decode path ty j =
    match !ty with
    | Ty.Unknown _ -> Data.other j
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
    | Dict (ty, _) -> decode_dict path ty j
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
    | Data.Nil -> Seq.Nil
    | Data.Array [| hd; tl |] -> Seq.Cons (encode ty hd, encode_list ty tl)
    | _ -> Error.internal __POS__ "Lists may only contain Array or Nil."

  and encode ty t =
    match (!ty, t) with
    | _, Other j -> j
    | _, Const (Float f) -> D.of_float f
    | _, Const (String s) -> D.of_string s
    | Ty.Enum { extra = Bool; _ }, Const (Int 0) -> D.of_bool false
    | Enum { extra = Bool; _ }, Const (Int _) -> D.of_bool true
    | (Enum _ | Int | Unknown _), Const (Int i) -> D.of_int i
    | (Nullable _ | Unknown _), Nil -> D.null
    | Nullable ty, Array [| t |] -> D.some @@ encode ty t
    | List ty, t -> D.of_seq @@ encode_list ty t
    | Tuple tys, Array a ->
        D.of_seq @@ Seq.map2 encode (List.to_seq tys) (Array.to_seq a)
    | Unknown _, Array a -> D.of_seq @@ Seq.map (encode ty) (Array.to_seq a)
    | Dict (ty, _), Dict m -> D.of_map @@ Map.String.map (encode ty) m
    | Unknown _, Dict m -> D.of_map @@ Map.String.map (encode ty) m
    | Record tys, Dict m -> D.of_map @@ encode_record !tys m
    | Union (k, { cases; extra; _ }), Dict m ->
        let tag = Map.String.find k m in
        let tys =
          match (cases, tag) with
          | Ty.Union.Int m, Const (Int i) -> Map.Int.find i m
          | Ty.Union.String m, Const (String s) -> Map.String.find s m
          | _ -> Error.internal __POS__ "Type mismatch while encoding a union."
        in
        let tag =
          match (extra, tag) with
          | Bool, Const (Int 0) -> D.of_bool false
          | Bool, Const (Int _) -> D.of_bool true
          | Not_bool, Const (Int i) -> D.of_int i
          | _, Const (String s) -> D.of_string s
          | _ ->
              Error.internal __POS__ "Union tags may only be ints or strings."
        in
        encode_record !tys m |> Map.String.add k tag |> D.of_map
    | ( ( String | Int | Float | Enum _ | Nullable _ | Tuple _ | Dict _
        | Record _ | Union _ ),
        _ ) ->
        Error.internal __POS__ "Type mismatch while encoding data."

  let encode tys j = D.of_map @@ encode_record tys j

  type t = string M.t

  let ( let* ) = M.bind

  let all_array a =
    let result = Array.make (Array.length a) Data.null in
    let* _idx =
      Array.fold_left
        (fun idx data ->
          let* idx = idx in
          let* data = data in
          result.(idx) <- data;
          M.return @@ succ idx)
        (M.return 0) a
    in
    M.return result

  let all_map m =
    Map.String.fold
      (fun key data result ->
        let* result = result in
        let* data = data in
        M.return @@ Map.String.add key data result)
      m
      (M.return Map.String.empty)

  let rec eval_data vars = function
    | Data.Other (Compile.Var x) -> M.return @@ Map.String.find x vars
    | Other (Block nodes) ->
        let b = M.return @@ Buffer.create 1024 in
        let* result = make b nodes vars in
        M.return @@ Data.string (Buffer.contents result)
    | Other (Field (data, field)) ->
        let* data = eval_data vars data in
        Data.get_dict data |> Map.String.find field |> M.return
    | Nil -> M.return Data.null
    | Array a ->
        let* result = all_array (Array.map (eval_data vars) a) in
        M.return @@ Data.array result
    | Dict d ->
        let* result = all_map (Map.String.map (eval_data vars) d) in
        M.return @@ Data.dict result
    | Const a -> M.return @@ Data.const a

  and make b nodes vars =
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
        | Match (args, tree) ->
            let* args = all_array (Array.map (eval_data vars) args) in
            let vars', nodes = make_match args tree in
            let vars = map_merge vars vars' in
            make b nodes vars
        | Map_list (arg, tree) ->
            let* l = eval_data vars arg in
            Data.fold_list
              (fun ~index b arg ->
                let vars', nodes = make_match [| arg; index |] tree in
                let vars = map_merge vars vars' in
                make b nodes vars)
              b l
        | Map_dict (arg, tree) ->
            let* d = eval_data vars arg in
            Data.fold_dict
              (fun ~index b arg ->
                let vars', nodes = make_match [| arg; index |] tree in
                let vars = map_merge vars vars' in
                make b nodes vars)
              b d
        | Component (_, comp, args) -> (
            let* vars = all_map (Map.String.map (eval_data vars) args) in
            match comp with
            | Compile.Src nodes -> make b nodes vars
            | Fun (types, f) ->
                let* result = f (encode types vars) in
                let* b = b in
                Buffer.add_string b result;
                M.return b))
      b nodes

  let make { Compile.nodes; types; name; _ } props =
    (* Wrap the props in a monad so it can catch decode exceptions. *)
    let* props = M.return props in
    let vars = decode ~name types props in
    let b = M.return @@ Buffer.create 1024 in
    let* result = make b nodes vars in
    M.return @@ Buffer.contents result
end
