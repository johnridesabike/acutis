(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module MapString = Map.Make (String)
module I = Parser.MenhirInterpreter
module T = Typechecker

let parse_fail state = function
  | I.HandlingError env ->
      Error.parse_error (I.current_state_number env) (Lexer.loc state)
  | _ -> Error.internal ~__POS__ "Parser error handling failed somehow."

let parse ~fname lexbuf =
  Lexing.set_filename lexbuf fname;
  let state = Lexer.make_state lexbuf in
  I.loop_handle Fun.id (parse_fail state) (Lexer.supplier state)
    (Parser.Incremental.acutis lexbuf.lex_curr_p)

let parse_interface ~fname lexbuf =
  Lexing.set_filename lexbuf fname;
  let state = Lexer.make_state_interface lexbuf in
  I.loop_handle Fun.id (parse_fail state) (Lexer.supplier state)
    (Parser.Incremental.interface_standalone lexbuf.lex_curr_p)

let is_space = function ' ' | '\012' | '\n' | '\r' | '\t' -> true | _ -> false

let ltrim s =
  let i = ref 0 in
  while !i < String.length s && is_space (String.unsafe_get s !i) do
    incr i
  done;
  if !i > 0 then String.sub s !i (String.length s - !i) else s

let rtrim s =
  let i = ref (String.length s - 1) in
  while !i >= 0 && is_space (String.unsafe_get s !i) do
    decr i
  done;
  if !i < String.length s - 1 then String.sub s 0 (!i + 1) else s

type escape = Ast.escape = No_escape | Escape

type echo_format = Ast.echo_format =
  | Fmt_string
  | Fmt_int
  | Fmt_float
  | Fmt_bool

type echo = [ `Var of string | `String of string | `Field of echo * string ]

type data =
  [ `Null
  | `Int of int
  | `Float of float
  | `String of string
  | `Array of data array
  | `Assoc of data MapString.t
  | `Var of string
  | `Field of data * string
  | `Block of int ]

type node =
  | Text of string
  | Echo of (echo_format * echo) list * echo_format * echo * escape
  | Match of blocks * data array * nodes Matching.t
  | Map_list of blocks * data * nodes Matching.t
  | Map_dict of blocks * data * nodes Matching.t
  | Component of string * blocks * data MapString.t

and blocks = nodes Queue.t
and nodes = node list

let blocks_length = Queue.length
let blocks_to_seq q = Seq.zip (Seq.ints 0) (Queue.to_seq q)
let text = function "" -> None | s -> Some (Text s)

let rec make_data blocks = function
  | T.Scalar (x, _) -> (x :> data)
  | T.Var x -> `Var x
  | T.Block x ->
      let i = Queue.length blocks in
      Queue.push (make_nodes x) blocks;
      `Block i
  | T.Nil -> `Null
  | T.Cons x -> make_data blocks x
  | T.Tuple l -> `Array (Array.of_list l |> Array.map (make_data blocks))
  | T.Record (Union_tag_int (k, v, _), x, _) ->
      `Assoc MapString.(map (make_data blocks) x |> add k (`Int v))
  | T.Record (Union_tag_string (k, v, _), x, _) ->
      `Assoc MapString.(map (make_data blocks) x |> add k (`String v))
  | T.Record (Union_tag_none, x, _) | Dict (x, _) ->
      `Assoc (MapString.map (make_data blocks) x)
  | T.Field (node, field) -> `Field (make_data blocks node, field)

and make_nodes l =
  List.filter_map
    (function
      | T.Text (s, No_trim, No_trim) -> text s
      | T.Text (s, Trim, No_trim) -> text (ltrim s)
      | T.Text (s, No_trim, Trim) -> text (rtrim s)
      | T.Text (s, Trim, Trim) -> text (String.trim s)
      | T.Echo (nullables, fmt, default, esc) ->
          Some (Echo (nullables, fmt, default, esc))
      | T.Match (loc, hd :: tl, tys, cases) ->
          let blocks = Queue.create () in
          let pats = Array.of_list (hd :: tl) |> Array.map (make_data blocks) in
          Some (Match (blocks, pats, make_match loc tys cases))
      | T.Map_list (loc, pat, tys, cases) ->
          let blocks = Queue.create () in
          let pat = make_data blocks pat in
          Some (Map_list (blocks, pat, make_match loc tys cases))
      | T.Map_dict (loc, pat, tys, cases) ->
          let blocks = Queue.create () in
          let pat = make_data blocks pat in
          Some (Map_dict (blocks, pat, make_match loc tys cases))
      | T.Component (name, props) ->
          let blocks = Queue.create () in
          let props = MapString.map (make_data blocks) props in
          Some (Component (name, blocks, props)))
    l

and make_match loc tys cases =
  let Matching.{ tree; exits } = Matching.make loc tys cases in
  let exits = Matching.Exits.map make_nodes exits in
  { tree; exits }

module Components = struct
  type 'a source = (Ast.t, 'a) T.source

  let from_src ~fname ~name src = T.Src (name, parse ~fname src)
  let from_fun ~name props f = T.Fun (name, props, f)

  type 'a t = {
    typed : (T.t, 'a) T.source MapString.t;
    optimized : (nodes, 'a) T.source MapString.t;
  }

  let empty = { typed = MapString.empty; optimized = MapString.empty }

  let of_seq l =
    let untyped =
      Seq.fold_left
        (fun acc -> function
          | T.Src (name, src) ->
              if MapString.mem name acc then Error.duplicate_name name;
              MapString.add name (T.Src (name, src)) acc
          | T.Fun (name, p, f) ->
              if MapString.mem name acc then Error.duplicate_name name;
              MapString.add name (T.Fun (name, p, f)) acc)
        MapString.empty l
    in
    let typed = T.make_components untyped in
    let optimized =
      MapString.map
        (function
          | T.Src (name, src) -> T.Src (name, make_nodes src.T.nodes)
          | T.Fun (name, p, f) -> T.Fun (name, p, f))
        typed
    in
    { typed; optimized }
end

type 'a t = {
  name : string;
  types : Typechecker.Type.scheme;
  components : (string * nodes) list;
  externals : (string * Typechecker.Type.scheme * 'a) list;
  nodes : nodes;
}

module SetString = Set.Make (String)

type 'a linked_components = {
  components : (string * nodes) list;
  externals : (string * Typechecker.Type.scheme * 'a) list;
  set : SetString.t;
  stack : string list;
}

let empty_linked =
  { components = []; externals = []; set = SetString.empty; stack = [] }

let make ~fname components_src nodes =
  let typed = T.make ~root:fname components_src.Components.typed nodes in
  let nodes = make_nodes typed.nodes in
  (* Only retrieve the components that need to be linked. *)
  let rec get_components linked nodes =
    List.fold_left
      (fun linked -> function
        | Text _ | Echo _ -> linked
        | Match (blocks, _, m) ->
            let linked = Queue.fold get_components linked blocks in
            Matching.Exits.nodes m.exits |> Seq.fold_left get_components linked
        | Map_list (blocks, _, m) ->
            let linked = Queue.fold get_components linked blocks in
            Matching.Exits.nodes m.exits |> Seq.fold_left get_components linked
        | Map_dict (blocks, _, m) ->
            let linked = Queue.fold get_components linked blocks in
            Matching.Exits.nodes m.exits |> Seq.fold_left get_components linked
        | Component (name, blocks, _) -> (
            let linked = Queue.fold get_components linked blocks in
            if SetString.mem name linked.set then linked
            else if List.exists (String.equal name) linked.stack then
              raise @@ Error.cycle (name :: linked.stack)
            else
              match MapString.find_opt name components_src.optimized with
              | None -> raise @@ Error.missing_component linked.stack name
              | Some (Src (_, nodes)) ->
                  let { components; externals; set; _ } =
                    get_components
                      { linked with stack = name :: linked.stack }
                      nodes
                  in
                  let set = SetString.add name set in
                  let components = (name, nodes) :: components in
                  { linked with components; externals; set }
              | Some (Fun (_, props, f)) ->
                  let set = SetString.add name linked.set in
                  let externals = (name, props, f) :: linked.externals in
                  { linked with externals; set }))
      linked nodes
  in
  let { components; externals; _ } = get_components empty_linked nodes in
  let components = List.rev components in
  { name = fname; types = typed.types; components; externals; nodes }

let make_interface ~fname src =
  parse_interface ~fname src |> T.make_interface_standalone

module TyRepr = struct
  open Pp.TyRepr
  module MapString = Map (String)
  module Ast = Ast.TyRepr
  module Matching = Matching.TyRepr

  let rec data = function
    | `Null -> polyvar0 "Null"
    | `Int i -> polyvar "Int" (args (int i))
    | `String s -> polyvar "String" (args (string s))
    | `Float f -> polyvar "Float" (args (float f))
    | `Array a -> polyvar "Array" (args (array data a))
    | `Assoc d -> polyvar "Assoc" (args (MapString.t data d))
    | `Var s -> polyvar "Var" (args (string s))
    | `Block n -> polyvar "Block" (args (int n))
    | `Field (d, f) -> polyvar "Field" (args (data d) * string f)

  let rec node = function
    | Text s -> variant "Text" (args (string s))
    | Echo (l, fmt, ech, esc) ->
        variant "Echo"
          (args (list (tuple2 Ast.echo_format data) l)
          * Ast.echo_format fmt * data ech * Ast.escape esc)
    | Match (blocks', data', matching) ->
        variant "Match"
          (args (blocks blocks')
          * (array data) data'
          * Matching.t nodes matching)
    | Map_list (blocks', data', matching) ->
        variant "Map_list"
          (args (blocks blocks') * data data' * Matching.t nodes matching)
    | Map_dict (blocks', data', matching) ->
        variant "Map_dict"
          (args (blocks blocks') * data data' * Matching.t nodes matching)
    | Component (name, blocks', props) ->
        variant "Component"
          (args (blocks blocks') * string name * MapString.t data props)

  and blocks b = seq (tuple2 int nodes) (blocks_to_seq b)
  and nodes l = list node l
end
