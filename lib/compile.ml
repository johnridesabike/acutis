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
  | Ech_var of echo_format * string
  | Ech_string of string

type 'a node =
  | Text of string
  | Echo of (echo_format * string) list * echo * escape
  | Match of 'a data Data.t array * 'a nodes Matching.t
  | Map_list of 'a data Data.t * 'a nodes Matching.t
  | Map_dict of 'a data Data.t * 'a nodes Matching.t
  | Component of 'a * 'a data Data.t Map.String.t

and 'a data = Var of string | Block of 'a nodes
and 'a nodes = 'a node list

let rec make_data = function
  | Typechecker.TConst (x, _) -> Data.const x
  | TVar x -> Data.other (Var x)
  | TBlock (_, x) -> Data.other (Block (make_nodes x))
  | TConstruct (_, Some x) -> make_data x
  | TConstruct (_, None) -> Data.null
  | TTuple l ->
      let a = Array.of_list l |> Array.map make_data in
      Data.tuple a
  | TRecord (Some (k, v, _), x, _) ->
      Map.String.map make_data x |> Map.String.add k (Data.const v) |> Data.dict
  | TRecord (None, x, _) | TDict (x, _) ->
      Data.dict (Map.String.map make_data x)
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
      | TEcho (nullables, default, esc) -> Echo (nullables, default, esc)
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
