(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

let parse ~fname lexbuf =
  let state = Lexer.make_state () in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with pos_fname = fname };
  try Parser.acutis (Lexer.acutis state) lexbuf with
  | Lexer.Error -> Error.lex_error lexbuf
  | Parser.Error i -> Error.parse_error i lexbuf

let parse_interface ~fname lexbuf =
  let state = Lexer.make_state_interface () in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with pos_fname = fname };
  try Parser.interface_standalone (Lexer.acutis state) lexbuf with
  | Lexer.Error -> Error.lex_error lexbuf
  | Parser.Error i -> Error.parse_error i lexbuf

module StringExtra = struct
  (* The [ltrim] and [rtrim] functions are vendored from the Containers library.
     https://github.com/c-cube/ocaml-containers/blob/70703b351235b563f060ef494461e678e896da49/src/core/CCString.ml
  *)
  module S = String

  let drop_while f s =
    let i = ref 0 in
    while !i < S.length s && f (S.unsafe_get s !i) do
      incr i
    done;
    if !i > 0 then S.sub s !i (S.length s - !i) else s

  let rdrop_while f s =
    let i = ref (S.length s - 1) in
    while !i >= 0 && f (S.unsafe_get s !i) do
      decr i
    done;
    if !i < S.length s - 1 then S.sub s 0 (!i + 1) else s

  (* notion of whitespace for trim *)
  let is_space = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false

  let ltrim s = drop_while is_space s
  let rtrim s = rdrop_while is_space s
end

type escape = Ast.escape = No_escape | Escape
type echo_flag = Ast.echo_flag = No_flag | Flag_comma

type echo_format = Ast.echo_format =
  | Fmt_string
  | Fmt_int of echo_flag
  | Fmt_float of int
  | Fmt_float_e of int
  | Fmt_float_g of int
  | Fmt_bool

type echo = Typechecker.echo =
  | Echo_var of string
  | Echo_string of string
  | Echo_field of echo * string

type 'a node =
  | Text of string
  | Echo of (echo_format * echo) list * echo_format * echo * escape
  | Match of 'a eval Data.t array * 'a nodes Matching.t
  | Map_list of 'a eval Data.t * 'a nodes Matching.t
  | Map_dict of 'a eval Data.t * 'a nodes Matching.t
  | Component of 'a * 'a eval Data.t Map.String.t

and 'a eval =
  | Var of string
  | Block of 'a nodes
  | Field of 'a eval Data.t * string

and 'a nodes = 'a node list

let rec make_data = function
  | Typechecker.TConst (x, _) -> Data.const x
  | TVar x -> Data.other (Var x)
  | TBlock x -> Data.other (Block (make_nodes x))
  | TConstruct (_, Some x) -> make_data x
  | TConstruct (_, None) -> Data.null
  | TTuple l ->
      let a = Array.of_list l |> Array.map make_data in
      Data.tuple a
  | TRecord (Some (k, v, _), x, _) ->
      Map.String.map make_data x |> Map.String.add k (Data.const v) |> Data.dict
  | TRecord (None, x, _) | TDict (x, _) ->
      Data.dict (Map.String.map make_data x)
  | TField (node, field) -> Data.other (Field (make_data node, field))
  | TAny ->
      Error.internal __POS__
        "TAny should not appear in data constructs. This means the typechecker \
         failed."

and make_nodes l =
  List.map
    (function
      | Typechecker.TText (s, No_trim, No_trim) -> Text s
      | TText (s, Trim, No_trim) -> Text (StringExtra.ltrim s)
      | TText (s, No_trim, Trim) -> Text (StringExtra.rtrim s)
      | TText (s, Trim, Trim) -> Text (String.trim s)
      | TEcho (nullables, fmt, default, esc) ->
          Echo (nullables, fmt, default, esc)
      | TMatch (loc, hd :: tl, tys, cases) ->
          let pats = Array.of_list (hd :: tl) |> Array.map make_data in
          Match (pats, make_match loc tys cases)
      | TMap_list (loc, pat, tys, cases) ->
          Map_list (make_data pat, make_match loc tys cases)
      | TMap_dict (loc, pat, tys, cases) ->
          Map_dict (make_data pat, make_match loc tys cases)
      | TComponent (name, props) ->
          Component (name, Map.String.map make_data props))
    l

and make_match loc tys cases =
  let Matching.{ tree; exits } = Matching.make cases in
  Matching.partial_match_check loc (Nonempty.to_list tys) tree;
  let exits = Matching.Exit.map make_nodes exits in
  { tree; exits }

let make_nodes Typechecker.{ nodes; _ } = make_nodes nodes

type 'a template =
  | Src of 'a template nodes
  | Fun of Typescheme.t Map.String.t * 'a

type 'a t = {
  types : Typescheme.t Map.String.t;
  nodes : 'a template nodes;
  name : string;
}

module Components = struct
  module T = Typechecker

  type 'a source = (Ast.t, 'a) T.source

  let parse_string ~fname ~name src =
    T.Src (name, parse ~fname (Lexing.from_string src))

  let parse_channel ~fname ~name src =
    T.Src (name, parse ~fname (Lexing.from_channel src))

  let from_fun ~name props f = T.Fun (name, props, f)

  type 'a t = {
    typed : (T.t, 'a) T.source Map.String.t;
    optimized : (string nodes, 'a) T.source Map.String.t;
  }

  let empty = { typed = Map.String.empty; optimized = Map.String.empty }

  let make l =
    let untyped =
      List.fold_left
        (fun acc -> function
          | T.Src (name, src) ->
              if Map.String.mem name acc then Error.duplicate_name name;
              Map.String.add name (T.Src (name, src)) acc
          | Fun (name, p, f) ->
              if Map.String.mem name acc then Error.duplicate_name name;
              Map.String.add name (T.Fun (name, p, f)) acc)
        Map.String.empty l
    in
    let typed = T.make_components untyped in
    let optimized =
      Map.String.map
        (function
          | T.Src (name, src) -> T.Src (name, make_nodes src)
          | Fun (name, p, f) -> Fun (name, p, f))
        typed
    in
    { typed; optimized }
end

let rec link_nodes graph nodes =
  List.map
    (function
      | (Text _ | Echo _) as x -> x
      | Match (pats, t) ->
          let pats = Array.map (Data.map (link_data graph)) pats in
          let exits = Matching.Exit.map (link_nodes graph) t.exits in
          Match (pats, { t with exits })
      | Map_list (pats, t) ->
          let pats = Data.map (link_data graph) pats in
          let exits = Matching.Exit.map (link_nodes graph) t.exits in
          Map_list (pats, { t with exits })
      | Map_dict (pats, t) ->
          let pats = Data.map (link_data graph) pats in
          let exits = Matching.Exit.map (link_nodes graph) t.exits in
          Map_dict (pats, { t with exits })
      | Component (name, pats) ->
          let pats = Map.String.map (Data.map (link_data graph)) pats in
          let data = Dagmap.get name graph in
          Component (data, pats))
    nodes

and link_data graph = function
  | Var _ as x -> x
  | Block nodes -> Block (link_nodes graph nodes)
  | Field (data, field) -> Field (Data.map (link_data graph) data, field)

let link_src graph = function
  | Typechecker.Src (_, nodes) -> Src (link_nodes graph nodes)
  | Fun (_, props, f) -> Fun (props, f)

let make ~fname components src =
  let nodes = parse ~fname src in
  let ast = Typechecker.make ~root:fname components.Components.typed nodes in
  let g = Dagmap.make ~f:link_src ~root:fname components.optimized in
  let nodes = make_nodes ast |> link_nodes g in
  { types = ast.types; nodes; name = fname }

let from_string ~fname components src =
  make ~fname components (Lexing.from_string src)

let from_channel ~fname components src =
  make ~fname components (Lexing.from_channel src)

let interface_from_string ~fname src =
  parse_interface ~fname (Lexing.from_string src)
  |> Typechecker.make_interface_standalone

type jsfun = { module_path : string; function_path : string }

type t2 = {
  types_nolink : Typescheme.t Map.String.t;
  nodes_nolink : string nodes;
  name_nolink : string;
  components_nolink : (string nodes, jsfun) Typechecker.source Map.String.t;
}

let make_nolink ~fname components src =
  let nodes = parse ~fname src in
  let ast = Typechecker.make ~root:fname components.Components.typed nodes in
  let nodes = make_nodes ast in
  {
    types_nolink = ast.types;
    nodes_nolink = nodes;
    name_nolink = fname;
    components_nolink = components.optimized;
  }

let from_string_nolink ~fname components src =
  make_nolink ~fname components (Lexing.from_string src)
