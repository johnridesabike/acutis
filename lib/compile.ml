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
module T = Typechecker

let parse ~fname lexbuf =
  Lexing.set_filename lexbuf fname;
  let state = Lexer.make_state () in
  try Parser.acutis (Lexer.acutis state) lexbuf
  with Parser.Error i -> Error.parse_error i lexbuf

let parse_interface ~fname lexbuf =
  Lexing.set_filename lexbuf fname;
  let state = Lexer.make_state_interface () in
  try Parser.interface_standalone (Lexer.acutis state) lexbuf
  with Parser.Error i -> Error.parse_error i lexbuf

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

let rec data_to_sexp = function
  | `Null -> Sexp.symbol "null"
  | `Int i -> Sexp.int i
  | `String s -> Sexp.string s
  | `Float f -> Sexp.float f
  | `Array a -> Sexp.make "array" [ Sexp.of_seq data_to_sexp (Array.to_seq a) ]
  | `Assoc d -> Sexp.make "assoc" [ assoc_to_sexp d ]
  | `Var s -> Sexp.make "var" [ Sexp.string s ]
  | `Block n -> Sexp.make "block" [ Sexp.int n ]
  | `Field (d, f) -> Sexp.make "field" [ data_to_sexp d; Sexp.string f ]

and assoc_to_sexp d = Sexp.map_string data_to_sexp d

let rec node_to_sexp = function
  | Text s -> Sexp.make "text" [ Sexp.string s ]
  | Echo (l, fmt, ech, esc) ->
      Sexp.make "echo"
        [
          Sexp.of_seq
            (Sexp.pair Ast.echo_format_to_sexp data_to_sexp)
            (List.to_seq l);
          Ast.echo_format_to_sexp fmt;
          data_to_sexp ech;
          Ast.escape_to_sexp esc;
        ]
  | Match (blocks, data, matching) ->
      Sexp.make "match"
        [
          Sexp.of_seq (Sexp.pair Sexp.int to_sexp) (blocks_to_seq blocks);
          Sexp.of_seq data_to_sexp (Array.to_seq data);
          Matching.to_sexp to_sexp matching;
        ]
  | Map_list (blocks, data, matching) ->
      Sexp.make "map_list"
        [
          Sexp.of_seq (Sexp.pair Sexp.int to_sexp) (blocks_to_seq blocks);
          data_to_sexp data;
          Matching.to_sexp to_sexp matching;
        ]
  | Map_dict (blocks, data, matching) ->
      Sexp.make "map_dict"
        [
          Sexp.of_seq (Sexp.pair Sexp.int to_sexp) (blocks_to_seq blocks);
          data_to_sexp data;
          Matching.to_sexp to_sexp matching;
        ]
  | Component (name, blocks, props) ->
      Sexp.make "component"
        [
          Sexp.of_seq (Sexp.pair Sexp.int to_sexp) (blocks_to_seq blocks);
          Sexp.string name;
          assoc_to_sexp props;
        ]

and to_sexp l = Sexp.of_seq node_to_sexp (List.to_seq l)
