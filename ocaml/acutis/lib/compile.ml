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

type 'a node =
  | Text of string
  | Echo of Typechecker.echo list * Typechecker.echo
  | Match of Typechecker.Pattern.t array * 'a nodes Matching.t
  | Map_list of Typechecker.Pattern.t * 'a nodes Matching.t
  | Map_dict of Typechecker.Pattern.t * 'a nodes Matching.t
  | Component of 'a * Typechecker.Pattern.t MapString.t * 'a child MapString.t

and 'a child = Child_name of string | Child_block of 'a nodes
and 'a nodes = 'a node list

let rec make_nodes =
  let f = function
    | Typechecker.TText (s, No_trim, No_trim) -> Text s
    | TText (s, Trim, No_trim) -> Text (StringExtra.ltrim s)
    | TText (s, No_trim, Trim) -> Text (StringExtra.rtrim s)
    | TText (s, Trim, Trim) -> Text (String.trim s)
    | TEcho (nullables, default) -> Echo (nullables, default)
    | TMatch (loc, hd :: tl, cases) ->
        Match (Array.of_list (hd :: tl), make_match loc cases)
    | TMap_list (loc, pat, cases) -> Map_list (pat, make_match loc cases)
    | TMap_dict (loc, pat, cases) -> Map_dict (pat, make_match loc cases)
    | TComponent (name, props, children) ->
        let children = MapString.map make_children children in
        Component (name, props, children)
  in
  fun l -> List.map f l

and make_match loc cases =
  let Matching.{ tree; exits } = Matching.make cases in
  Matching.partial_match_check loc tree;
  let exits = Matching.Exit.map make_nodes exits in
  { tree; exits }

and make_children = function
  | Typechecker.TChild_name s -> Child_name s
  | TChild_block n -> Child_block (make_nodes n)

let make_nodes Typechecker.{ nodes; _ } = make_nodes nodes

type 'a t = { prop_types : Typescheme.t; nodes : 'a nodes }

type 'a template =
  | Acutis of string * 'a template nodes
  | Function of string * Typescheme.t * 'a

let parse_string ~filename src =
  let state = Lexer.make_state () in
  let lexbuf = Lexing.from_string src in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try Parser.acutis (Lexer.acutis state) lexbuf with
  | Lexer.Error -> Error.lex_error lexbuf
  | Parser.Error i -> Error.parse_error i lexbuf

module Components = struct
  type 'a t = {
    typed : (Typechecker.t, 'a) Source.t MapString.t;
    optimized : (string nodes, 'a) Source.t MapString.t;
  }

  let empty = { typed = MapString.empty; optimized = MapString.empty }

  let rec make_aux_parse m = function
    | [] -> m
    | Source.Acutis (name, src) :: l ->
        if MapString.mem name m then Error.duplicate_name name;
        let c = Source.src ~name (parse_string ~filename:name src) in
        make_aux_parse (MapString.add name c m) l
    | Function (name, p, c, f) :: l ->
        if MapString.mem name m then Error.duplicate_name name;
        make_aux_parse (MapString.add name (Source.fn ~name p c f) m) l

  let make_aux_optimize _ v optimized =
    match v with
    | Source.Acutis (name, src) ->
        MapString.add name (Source.src ~name (make_nodes src)) optimized
    | Function (name, p, c, f) ->
        MapString.add name (Source.fn ~name p c f) optimized

  let make l =
    let untyped = make_aux_parse MapString.empty l in
    let typed = Typechecker.make_components untyped in
    let optimized = MapString.fold make_aux_optimize typed MapString.empty in
    { typed; optimized }
end

let rec link_nodes graph nodes =
  let f = function
    | (Text _ | Echo _) as x -> x
    | Match (p, t) ->
        let exits = Matching.Exit.map (link_nodes graph) t.exits in
        Match (p, { t with exits })
    | Map_list (p, t) ->
        let exits = Matching.Exit.map (link_nodes graph) t.exits in
        Map_list (p, { t with exits })
    | Map_dict (p, t) ->
        let exits = Matching.Exit.map (link_nodes graph) t.exits in
        Map_dict (p, { t with exits })
    | Component (name, props, children) ->
        let f = function
          | Child_name _ as child -> child
          | Child_block nodes -> Child_block (link_nodes graph nodes)
        in
        let children = MapString.map f children in
        let data = Dagmap.get name graph in
        Component (data, props, children)
  in
  List.map f nodes

let link_src graph = function
  | Source.Acutis (name, nodes) -> Acutis (name, link_nodes graph nodes)
  | Function (name, props, _, f) -> Function (name, props, f)

let make ~filename components src =
  let nodes = parse_string ~filename src in
  let ast = Typechecker.make components.Components.typed nodes in
  let g = Dagmap.make ~f:link_src components.optimized in
  let nodes = make_nodes ast |> link_nodes g in
  { prop_types = ast.prop_types; nodes }
