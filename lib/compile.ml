(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module Map_string = Map.Make (String)
module I = Parser.MenhirInterpreter
module T = Typechecker

type parsed = { fname : string; ast : Ast.t }

let rec parse_aux supplier checkpoint =
  match checkpoint with
  | I.InputNeeded _ -> supplier () |> I.offer checkpoint |> parse_aux supplier
  | I.Shifting _ | I.AboutToReduce _ ->
      I.resume checkpoint |> parse_aux supplier
  | I.HandlingError env ->
      Error.parse_error (I.current_state_number env) (I.positions env)
  | I.Accepted x -> x
  | I.Rejected ->
      Error.internal ~__POS__ "Parser error handling failed somehow."

let parse lexbuf =
  let fname = lexbuf.Lexing.lex_curr_p.pos_fname in
  let ast =
    parse_aux (Lexer.acutis lexbuf)
      (Parser.Incremental.acutis lexbuf.lex_curr_p)
  in
  { fname; ast }

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
  | `Assoc of data Map_string.t
  | `Var of string
  | `Field of data * string
  | `Block of int ]

type node =
  | Text of string
  | Echo of (echo_format * echo) list * echo_format * echo * escape
  | Match of blocks * data array * nodes Matching.t
  | Map_list of blocks * data * nodes Matching.t
  | Map_dict of blocks * data * nodes Matching.t
  | Component of string * blocks * data Map_string.t

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
      `Assoc Map_string.(map (make_data blocks) x |> add k (`Int v))
  | T.Record (Union_tag_string (k, v, _), x, _) ->
      `Assoc Map_string.(map (make_data blocks) x |> add k (`String v))
  | T.Record (Union_tag_none, x, _) | Dict (x, _) ->
      `Assoc (Map_string.map (make_data blocks) x)
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
          let props = Map_string.map (make_data blocks) props in
          Some (Component (name, blocks, props)))
    l

and make_match loc tys cases =
  let Matching.{ tree; exits } = Matching.make loc tys cases in
  let exits = Matching.Exits.map make_nodes exits in
  { tree; exits }

module Components = struct
  type 'a source = (parsed, 'a) T.source

  let of_parsed name parsed = T.Src (name, parsed)
  let of_fun name props f = T.Fun (name, props, f)

  type 'a t = {
    typed : (T.t, 'a) T.source Map_string.t;
    optimized : (nodes, 'a) T.source Map_string.t;
  }

  let empty = { typed = Map_string.empty; optimized = Map_string.empty }

  let of_seq l =
    let untyped =
      Seq.fold_left
        (fun acc -> function
          | T.Src (name, parsed) ->
              if Map_string.mem name acc then Error.duplicate_name name;
              Map_string.add name (T.Src (name, parsed.ast)) acc
          | T.Fun (name, p, f) ->
              if Map_string.mem name acc then Error.duplicate_name name;
              Map_string.add name (T.Fun (name, p, f)) acc)
        Map_string.empty l
    in
    let typed = T.make_components untyped in
    let optimized =
      Map_string.map
        (function
          | T.Src (name, typed) -> T.Src (name, make_nodes typed.T.nodes)
          | T.Fun (name, p, f) -> T.Fun (name, p, f))
        typed
    in
    { typed; optimized }
end

type 'a t = {
  fname : string;
  types : Typechecker.Type.interface;
  components : (string * nodes) list;
  externals : (string * Typechecker.Type.interface * 'a) list;
  nodes : nodes;
}

module Set_string = Set.Make (String)

type 'a linked_components = {
  components : (string * nodes) list;
  externals : (string * Typechecker.Type.interface * 'a) list;
  set : Set_string.t;
  stack : string list;
}

let empty_linked =
  { components = []; externals = []; set = Set_string.empty; stack = [] }

let make components_src { fname; ast } =
  let typed = T.make ~root:fname components_src.Components.typed ast in
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
            if Set_string.mem name linked.set then linked
            else if List.exists (String.equal name) linked.stack then
              raise @@ Error.cycle (name :: linked.stack)
            else
              match Map_string.find_opt name components_src.optimized with
              | None -> raise @@ Error.missing_component linked.stack name
              | Some (Src (_, nodes)) ->
                  let { components; externals; set; _ } =
                    get_components
                      { linked with stack = name :: linked.stack }
                      nodes
                  in
                  let set = Set_string.add name set in
                  let components = (name, nodes) :: components in
                  { linked with components; externals; set }
              | Some (Fun (_, props, f)) ->
                  let set = Set_string.add name linked.set in
                  let externals = (name, props, f) :: linked.externals in
                  { linked with externals; set }))
      linked nodes
  in
  let { components; externals; _ } = get_components empty_linked nodes in
  let components = List.rev components in
  { fname; types = typed.types; components; externals; nodes }

let interface lexbuf =
  parse_aux (Lexer.interface lexbuf)
    (Parser.Incremental.interface_standalone lexbuf.lex_curr_p)
  |> T.make_interface_standalone

module Ty_repr = struct
  open Pp.Ty_repr
  module Map_string = Map (String)
  module Ast = Ast.Ty_repr
  module Matching = Matching.Ty_repr

  let rec data = function
    | `Null -> polyvar0 "Null"
    | `Int i -> polyvar "Int" (args (int i))
    | `String s -> polyvar "String" (args (string s))
    | `Float f -> polyvar "Float" (args (float f))
    | `Array a -> polyvar "Array" (args (array data a))
    | `Assoc d -> polyvar "Assoc" (args (Map_string.t data d))
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
          (args (blocks blocks') * string name * Map_string.t data props)

  and blocks b = seq (tuple2 int nodes) (blocks_to_seq b)
  and nodes l = list node l
end
