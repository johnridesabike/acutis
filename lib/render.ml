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

(** Only dicts can return [None] during pattern matching. To simplify the
    code, we also make tuples and records return [option] values. This could
    probably be optimized to avoid unnecessary [option] allocations for those
    types. *)

let tuple_get i a = Some a.(i)

let rec make_match :
          'a 'args 'key.
          'args ->
          ('key -> 'args -> 'data Data.t option) ->
          'data Data.t Map.Int.t ->
          ('a, 'key) Matching.tree ->
          ('data Data.t Map.Int.t * 'a) option =
 fun args get vars -> function
  | End x -> Some (vars, x)
  | Switch { key; cases; wildcard; ids; _ } -> (
      match get key args with
      | Some data -> (
          let vars = bind_names data ids vars in
          let data = Data.get_const data in
          match test_case ~wildcard data cases with
          | Some tree -> make_match args get vars tree
          | None -> None)
      | None -> None)
  | Wildcard { key; ids; child } -> (
      match get key args with
      | Some data ->
          let vars = bind_names data ids vars in
          make_match args get vars child
      | None -> None)
  | Construct { key; ids; nil; cons; _ } -> (
      match get key args with
      | Some data -> (
          let vars = bind_names data ids vars in
          let child = if Data.is_null data then nil else cons in
          match child with
          | Some tree -> make_match args get vars tree
          | None -> None)
      | None -> None)
  | Nest { key; ids; child; wildcard; _ } -> (
      match get key args with
      | Some data -> (
          let vars = bind_names data ids vars in
          let result =
            match child with
            | Int_keys child ->
                let tuple = Data.get_tuple data in
                make_match tuple tuple_get vars child
            | String_keys child ->
                let dict = Data.get_dict data in
                make_match dict Map.String.find_opt vars child
          in
          match result with
          | Some (vars, tree) -> make_match args get vars tree
          | None -> (
              match wildcard with
              | Some tree -> make_match args get vars tree
              | None -> None))
      | None -> None)

let make_match args Matching.{ tree; exits } =
  match make_match args tuple_get Map.Int.empty tree with
  | Some (vars, { names; exit }) ->
      let bindings = Map.String.map (fun id -> Map.Int.find id vars) names in
      (bindings, Matching.Exit.get exits exit)
  | None -> assert false

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
  | Compile.Fmt_int No_flag, Data.Const (Int i) -> Printf.sprintf "%i" i
  | Fmt_int Flag_comma, Const (Int i) ->
      Printf.sprintf "%#i" i |> String.map (function '_' -> ',' | c -> c)
  | Fmt_float pad, Const (Float i) -> Printf.sprintf "%.*f" pad i
  | Fmt_float_e pad, Const (Float i) -> Printf.sprintf "%.*e" pad i
  | Fmt_float_g pad, Const (Float i) -> Printf.sprintf "%.*g" pad i
  | Fmt_bool, Const (Int 0) -> "false"
  | Fmt_bool, _ -> "true"
  | Fmt_string, Const (String s) -> s
  | _ -> assert false

let rec get_echo props default = function
  | [] -> (
      match default with
      | Compile.Ech_var (fmt, var) ->
          Map.String.find var props |> echo_format fmt
      | Ech_string s -> s)
  | (fmt, var) :: tl -> (
      match Data.get_nullable (Map.String.find var props) with
      | Some data -> echo_format fmt data
      | None -> get_echo props default tl)

let map_merge a b = Map.String.union (fun _ _ b -> Some b) a b

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type DATA = sig
  type t

  val decode :
    name:string -> Typescheme.t Map.String.t -> t -> t Data.t Map.String.t

  val encode : Typescheme.t Map.String.t -> t Data.t Map.String.t -> t
end

module type S = sig
  type t
  type data

  val make : (data -> t) Compile.t -> data -> t
end

module Make (M : MONAD) (D : DATA) = struct
  type t = string M.t
  type data = D.t

  let ( let* ) = M.bind

  let all_array a =
    let result = Array.make (Array.length a) Data.Nil in
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
    | Nil -> M.return Data.Nil
    | Array a ->
        let* result = all_array (Array.map (eval_data vars) a) in
        M.return @@ Data.Array result
    | Dict d ->
        let* result = all_map (Map.String.map (eval_data vars) d) in
        M.return @@ Data.Dict result
    | Const a -> M.return @@ Data.Const a

  and make b nodes vars =
    let f b = function
      | Compile.Echo (nullables, default, esc) ->
          let* b = b in
          let str = get_echo vars default nullables in
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
          let f ~index b arg =
            let vars', nodes = make_match [| arg; index |] tree in
            let vars = map_merge vars vars' in
            make b nodes vars
          in
          Data.fold_list f b l
      | Map_dict (arg, tree) ->
          let* d = eval_data vars arg in
          let f ~index b arg =
            let vars', nodes = make_match [| arg; index |] tree in
            let vars = map_merge vars vars' in
            make b nodes vars
          in
          Data.fold_dict f b d
      | Component (comp, args) -> (
          let* vars = all_map (Map.String.map (eval_data vars) args) in
          match comp with
          | Compile.Src nodes -> make b nodes vars
          | Fun (types, f) ->
              let* result = f (D.encode types vars) in
              let* b = b in
              Buffer.add_string b result;
              M.return b)
    in
    List.fold_left f b nodes

  let make { Compile.nodes; types; name } props =
    (* Wrap the props in a monad so it can catch decode exceptions. *)
    let* props = M.return props in
    let vars = D.decode ~name types props in
    let b = M.return @@ Buffer.create 1024 in
    let* result = make b nodes vars in
    M.return @@ Buffer.contents result
end
