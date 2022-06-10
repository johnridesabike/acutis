(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open StdlibExtra

let rec test_case ~wildcard data case =
  if Data.Const.equal data case.Matching.data then Some case.if_match
  else
    match case.next_case with
    | Some case -> test_case ~wildcard data case
    | None -> wildcard

let bind_names data ids map =
  SetInt.fold (fun id map -> MapInt.add id data map) ids map

let array_get i a =
  if i >= 0 && i < Array.length a then Some (Array.unsafe_get a i) else None

let rec make_match :
          'a 'args 'key.
          'args ->
          ('key -> 'args -> 'data Data.t option) ->
          'data Data.t MapInt.t ->
          ('a, 'key) Matching.tree ->
          ('data Data.t MapInt.t * 'a) option =
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
            | IntKeys child ->
                let tuple = Data.get_tuple data in
                make_match tuple array_get vars child
            | StringKeys child ->
                let dict = Data.get_dict data in
                make_match dict MapString.find_opt vars child
          in
          match result with
          | Some (vars, tree) -> make_match args get vars tree
          | None -> (
              match wildcard with
              | Some tree -> make_match args get vars tree
              | None -> None))
      | None -> None)

let make_match args Matching.{ tree; exits } =
  match make_match args array_get MapInt.empty tree with
  | Some (vars, { names; exit }) ->
      let bindings = MapString.map (fun id -> MapInt.find id vars) names in
      Some (bindings, Matching.Exit.get exits exit)
  | None -> None

let escape_aux str =
  let b = Buffer.create (String.length str) in
  let f = function
    | '&' -> Buffer.add_string b "&amp;"
    | '"' -> Buffer.add_string b "&quot;"
    | '\'' -> Buffer.add_string b "&apos;"
    | '>' -> Buffer.add_string b "&gt;"
    | '<' -> Buffer.add_string b "&lt;"
    | '/' -> Buffer.add_string b "&#x2F;"
    | '`' -> Buffer.add_string b "&#x60;"
    | '=' -> Buffer.add_string b "&#x3D;"
    | c -> Buffer.add_char b c
  in
  String.iter f str;
  Buffer.contents b

let escape esc str =
  match esc with Ast.Escape -> escape_aux str | No_escape -> str

let echo_not_null props children return = function
  | Typechecker.Ech_var (var, esc) ->
      let x = MapString.find var props in
      return (escape esc (Data.to_string x))
  | Ech_component child -> MapString.find child children
  | Ech_string s -> return s

let echo_nullable props children return = function
  | Typechecker.Ech_var (var, esc) -> (
      match Data.get_nullable (MapString.find var props) with
      | None -> None
      | Some x -> Some (return (escape esc (Data.to_string x))))
  | Ech_component child -> MapString.find_opt child children
  | Ech_string s -> Some (return s)

let rec echo props children return default = function
  | [] -> echo_not_null props children return default
  | hd :: tl -> (
      match echo_nullable props children return hd with
      | Some x -> x
      | None -> echo props children return default tl)

let map_merge =
  let f _ a b =
    match (a, b) with
    | None, None -> None
    | _, (Some _ as x) | (Some _ as x), None -> x
  in
  fun a b -> MapString.merge f a b

let rec pattern_to_data ~vars = function
  | Typechecker.Pattern.TConst (x, Some { extra = Extra_bool; _ }) ->
      Data.const x Extra_bool
  | TConst (x, _) -> Data.const x Extra_none
  | TVar x -> MapString.find x vars
  | TConstruct (_, Some x) -> pattern_to_data ~vars x
  | TConstruct (_, None) -> Data.null
  | TTuple l ->
      let a = l |> Array.of_list |> Array.map (pattern_to_data ~vars) in
      Data.tuple a
  | TRecord (Some (k, v, { extra; _ }), x, _) ->
      x
      |> MapString.map (pattern_to_data ~vars)
      |> MapString.add k (Data.const v extra)
      |> Data.dict
  | TRecord (None, x, _) | TDict (x, _) ->
      Data.dict (MapString.map (pattern_to_data ~vars) x)
  | TAny -> assert false

let rec make :
    type a data.
    nodes:(data -> a MapString.t -> a) Compile.template Compile.nodes ->
    vars:data Data.t MapString.t ->
    children:a MapString.t ->
    env:(a, data) Source.env ->
    a Queue.t =
 fun ~nodes ~vars ~children ~env ->
  let queue = Queue.create () in
  let (module Env) = env in
  let f = function
    | Compile.Echo (nullables, default) ->
        Queue.add (echo vars children Env.return default nullables) queue
    | Text s -> Queue.add (Env.return s) queue
    | Match (args, tree) -> (
        let args = Array.map (pattern_to_data ~vars) args in
        match make_match args tree with
        | None -> assert false
        | Some (vars', nodes) ->
            let vars = map_merge vars vars' in
            let result = make ~nodes ~vars ~children ~env in
            Queue.transfer result queue)
    | Map_list (arg, tree) ->
        let l = pattern_to_data ~vars arg in
        let f ~index arg =
          match make_match [| arg; index |] tree with
          | None -> assert false
          | Some (vars', nodes) ->
              let vars = map_merge vars vars' in
              let result = make ~nodes ~vars ~children ~env in
              Queue.transfer result queue
        in
        Data.iter_list f l
    | Map_dict (arg, tree) ->
        let l = pattern_to_data ~vars arg in
        let f ~index arg =
          match make_match [| arg; index |] tree with
          | None -> assert false
          | Some (vars', nodes) ->
              let vars = map_merge vars vars' in
              let result = make ~nodes ~vars ~children ~env in
              Queue.transfer result queue
        in
        Data.iter_dict f l
    | Component (data, comp_vars, comp_children) -> (
        let f = function
          | Compile.Child_block nodes ->
              let result = make ~nodes ~vars ~children ~env in
              Env.render result
          | Child_name child -> MapString.find child children
        in
        let children = MapString.map f comp_children in
        let vars = MapString.map (pattern_to_data ~vars) comp_vars in
        match data with
        | Acutis (_, nodes) ->
            let result = make ~nodes ~vars ~children ~env in
            Queue.transfer result queue
        | Function (_, prop_types, f) ->
            let result = f (Env.export_data prop_types vars) children in
            Queue.add result queue)
  in
  List.iter f nodes;
  queue

let make (type a data) (env : (a, data) Source.env)
    Compile.{ nodes; prop_types } props =
  let (module Env) = env in
  let vars = Env.parse_data prop_types props in
  make ~nodes ~vars ~children:MapString.empty ~env |> Env.render

module Json = struct
  type t = string
  type data = DataYojson.t

  let return s = s

  let render queue =
    let b = Buffer.create 1024 in
    Queue.iter (Buffer.add_string b) queue;
    Buffer.contents b

  let parse_data = DataYojson.to_data
  let export_data = DataYojson.of_data
end

let json = make (module Json)
