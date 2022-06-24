(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

let rec test_case ~wildcard data case =
  if Data.Const.equal data case.Matching.data then Some case.if_match
  else
    match case.next_case with
    | Some case -> test_case ~wildcard data case
    | None -> wildcard

let bind_names data ids map =
  Set.Int.fold (fun id map -> Map.Int.add id data map) ids map

let array_get i a =
  if i >= 0 && i < Array.length a then Some (Array.unsafe_get a i) else None

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
                make_match tuple array_get vars child
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
  match make_match args array_get Map.Int.empty tree with
  | Some (vars, { names; exit }) ->
      let bindings = Map.String.map (fun id -> Map.Int.find id vars) names in
      Some (bindings, Matching.Exit.get exits exit)
  | None -> None

let escape b = function
  | '&' -> Buffer.add_string b "&amp;"
  | '"' -> Buffer.add_string b "&quot;"
  | '\'' -> Buffer.add_string b "&apos;"
  | '>' -> Buffer.add_string b "&gt;"
  | '<' -> Buffer.add_string b "&lt;"
  | '/' -> Buffer.add_string b "&#x2F;"
  | '`' -> Buffer.add_string b "&#x60;"
  | '=' -> Buffer.add_string b "&#x3D;"
  | c -> Buffer.add_char b c

let echo_var b esc str =
  match esc with
  | Ast.Escape -> String.iter (escape b) str
  | No_escape -> Buffer.add_string b str

let map_merge =
  let f _ a b =
    match (a, b) with
    | None, None -> None
    | _, (Some _ as x) | (Some _ as x), None -> x
  in
  fun a b -> Map.String.merge f a b

let rec pattern_to_data ~vars = function
  | Typechecker.Pattern.TConst (x, Some { extra = `Extra_bool; _ }) ->
      Data.const x `Extra_bool
  | TConst (x, _) -> Data.const x `Extra_none
  | TVar x -> Map.String.find x vars
  | TConstruct (_, Some x) -> pattern_to_data ~vars x
  | TConstruct (_, None) -> Data.null
  | TTuple l ->
      let a = l |> Array.of_list |> Array.map (pattern_to_data ~vars) in
      Data.tuple a
  | TRecord (Some (k, v, { extra; _ }), x, _) ->
      x
      |> Map.String.map (pattern_to_data ~vars)
      |> Map.String.add k (Data.const v extra)
      |> Data.dict
  | TRecord (None, x, _) | TDict (x, _) ->
      Data.dict (Map.String.map (pattern_to_data ~vars) x)
  | TAny -> assert false

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type DATA = sig
  type t

  val decode : Typescheme.t Map.String.t -> t -> t Data.t Map.String.t
  val encode : Typescheme.t Map.String.t -> t Data.t Map.String.t -> t
end

module type S = sig
  type t
  type data
  type component = data -> t Map.String.t -> t

  val make : component Compile.t -> data -> t
end

module Make (M : MONAD) (D : DATA) = struct
  type t = string M.t
  type data = D.t
  type component = data -> t Map.String.t -> t

  let ( let* ) = M.bind

  let echo_not_null b props children = function
    | Typechecker.Ech_var (var, esc) ->
        Map.String.find var props |> Data.to_string |> echo_var b esc;
        M.return b
    | Ech_component child ->
        let* child = Map.String.find child children in
        Buffer.add_string b child;
        M.return b
    | Ech_string s ->
        Buffer.add_string b s;
        M.return b

  let echo_nullable b props children = function
    | Typechecker.Ech_var (var, esc) -> (
        match Data.get_nullable (Map.String.find var props) with
        | None -> None
        | Some x ->
            echo_var b esc (Data.to_string x);
            Some (M.return b))
    | Ech_component child -> (
        match Map.String.find_opt child children with
        | None -> None
        | Some child ->
            let b =
              let* child = child in
              Buffer.add_string b child;
              M.return b
            in
            Some b)
    | Ech_string s ->
        Buffer.add_string b s;
        Some (M.return b)

  let rec echo b props children default = function
    | [] -> echo_not_null b props children default
    | hd :: tl -> (
        match echo_nullable b props children hd with
        | Some x -> x
        | None -> echo b props children default tl)

  let rec make b nodes vars children =
    let f b = function
      | Compile.Echo (nullables, default) ->
          let* b = b in
          echo b vars children default nullables
      | Text s ->
          let* b = b in
          Buffer.add_string b s;
          M.return b
      | Match (args, tree) -> (
          let args = Array.map (pattern_to_data ~vars) args in
          match make_match args tree with
          | None -> assert false
          | Some (vars', nodes) ->
              let vars = map_merge vars vars' in
              make b nodes vars children)
      | Map_list (arg, tree) ->
          let l = pattern_to_data ~vars arg in
          let f ~index b arg =
            match make_match [| arg; index |] tree with
            | None -> assert false
            | Some (vars', nodes) ->
                let vars = map_merge vars vars' in
                make b nodes vars children
          in
          Data.fold_list f b l
      | Map_dict (arg, tree) ->
          let l = pattern_to_data ~vars arg in
          let f ~index b arg =
            match make_match [| arg; index |] tree with
            | None -> assert false
            | Some (vars', nodes) ->
                let vars = map_merge vars vars' in
                make b nodes vars children
          in
          Data.fold_dict f b l
      | Component (data, comp_vars, comp_children) -> (
          let f = function
            | Compile.Child_block nodes ->
                let b = M.return (Buffer.create 1024) in
                let* result = make b nodes vars children in
                M.return (Buffer.contents result)
            | Child_name child -> Map.String.find child children
          in
          let children = Map.String.map f comp_children in
          let vars = Map.String.map (pattern_to_data ~vars) comp_vars in
          match data with
          | Compile.Src nodes -> make b nodes vars children
          | Fun (prop_types, f) ->
              let* result = f (D.encode prop_types vars) children in
              let* b = b in
              Buffer.add_string b result;
              M.return b)
    in
    List.fold_left f b nodes

  let make Compile.{ nodes; prop_types } props =
    let b = M.return (Buffer.create 1024) in
    let vars = D.decode prop_types props in
    let* result = make b nodes vars Map.String.empty in
    Buffer.contents result |> M.return
end
