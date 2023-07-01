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
  Lexing.set_filename lexbuf fname;
  let state = Lexer.make_state () in
  try Parser.acutis (Lexer.acutis state) lexbuf with
  | Lexer.Error -> Error.lex_error lexbuf
  | Parser.Error i -> Error.parse_error i lexbuf

let parse_interface ~fname lexbuf =
  Lexing.set_filename lexbuf fname;
  let state = Lexer.make_state_interface () in
  try Parser.interface_standalone (Lexer.acutis state) lexbuf with
  | Lexer.Error -> Error.lex_error lexbuf
  | Parser.Error i -> Error.parse_error i lexbuf

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
  | `Assoc of data Map.String.t
  | `Var of string
  | `Field of data * string
  | `Block of int ]

type 'a node =
  | Text of string
  | Echo of (echo_format * echo) list * echo_format * echo * escape
  | Match of 'a blocks * data array * 'a nodes Matching.t
  | Map_list of 'a blocks * data * 'a nodes Matching.t
  | Map_dict of 'a blocks * data * 'a nodes Matching.t
  | Component of string * 'a * 'a nodes array * data Map.String.t

and 'a blocks = 'a nodes array
and 'a nodes = 'a node list

let text = function "" -> None | s -> Some (Text s)

let rec make_echo = function
  | Typechecker.Echo_var x -> `Var x
  | Echo_string s -> `String s
  | Echo_field (e, s) -> `Field (make_echo e, s)

let rec make_data block_queue = function
  | Typechecker.TScalar (x, _) -> (x :> data)
  | TVar x -> `Var x
  | TBlock x ->
      let i = Queue.length block_queue in
      Queue.push (make_nodes x) block_queue;
      `Block i
  | TConstruct (_, Some x) -> make_data block_queue x
  | TConstruct (_, None) -> `Null
  | TTuple l -> `Array (Array.of_list l |> Array.map (make_data block_queue))
  | TRecord (Some (k, v, _), x, _) ->
      `Assoc
        (Map.String.map (make_data block_queue) x
        |> Map.String.add k (v :> data))
  | TRecord (None, x, _) | TDict (x, _) ->
      `Assoc (Map.String.map (make_data block_queue) x)
  | TField (node, field) -> `Field (make_data block_queue node, field)
  | TAny ->
      Error.internal __POS__
        "TAny should not appear in data constructs. This means the typechecker \
         failed."

and make_nodes l =
  List.filter_map
    (function
      | Typechecker.TText (s, No_trim, No_trim) -> text s
      | TText (s, Trim, No_trim) -> text (ltrim s)
      | TText (s, No_trim, Trim) -> text (rtrim s)
      | TText (s, Trim, Trim) -> text (String.trim s)
      | TEcho (l, fmt, default, esc) ->
          let nullables = List.map (fun (fmt, ech) -> (fmt, make_echo ech)) l in
          let default = make_echo default in
          Some (Echo (nullables, fmt, default, esc))
      | TMatch (loc, hd :: tl, tys, cases) ->
          let q = Queue.create () in
          let pats = Array.of_list (hd :: tl) |> Array.map (make_data q) in
          let blocks = Queue.to_seq q |> Array.of_seq in
          Some (Match (blocks, pats, make_match loc tys cases))
      | TMap_list (loc, pat, tys, cases) ->
          let q = Queue.create () in
          let pat = make_data q pat in
          let blocks = Queue.to_seq q |> Array.of_seq in
          Some (Map_list (blocks, pat, make_match loc tys cases))
      | TMap_dict (loc, pat, tys, cases) ->
          let q = Queue.create () in
          let pat = make_data q pat in
          let blocks = Queue.to_seq q |> Array.of_seq in
          Some (Map_dict (blocks, pat, make_match loc tys cases))
      | TComponent (name, props) ->
          let q = Queue.create () in
          let props = Map.String.map (make_data q) props in
          let blocks = Queue.to_seq q |> Array.of_seq in
          Some (Component (name, (), blocks, props)))
    l

and make_match loc tys cases =
  let Matching.{ tree; exits } = Matching.make cases in
  Matching.partial_match_check loc (Nonempty.to_list tys) tree;
  let exits = Matching.Exit.map make_nodes exits in
  { tree; exits }

let make_nodes Typechecker.{ nodes; _ } = make_nodes nodes

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
    optimized : (unit nodes, 'a) T.source Map.String.t;
  }

  let empty = { typed = Map.String.empty; optimized = Map.String.empty }

  let of_seq l =
    let untyped =
      Seq.fold_left
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

type 'a template =
  | Src of 'a template nodes
  | Fun of Typescheme.t Map.String.t * 'a

type 'a t = {
  name : string;
  types : Typescheme.t Map.String.t;
  nodes : 'a template nodes;
  components : 'a template Map.String.t;
}

let rec link_nodes graph nodes =
  List.map
    (function
      | (Text _ | Echo _) as x -> x
      | Match (blocks, pats, t) ->
          let blocks = Array.map (link_nodes graph) blocks in
          let exits = Matching.Exit.map (link_nodes graph) t.exits in
          Match (blocks, pats, { t with exits })
      | Map_list (blocks, pats, t) ->
          let blocks = Array.map (link_nodes graph) blocks in
          let exits = Matching.Exit.map (link_nodes graph) t.exits in
          Map_list (blocks, pats, { t with exits })
      | Map_dict (blocks, pats, t) ->
          let blocks = Array.map (link_nodes graph) blocks in
          let exits = Matching.Exit.map (link_nodes graph) t.exits in
          Map_dict (blocks, pats, { t with exits })
      | Component (name, (), blocks, pats) ->
          let blocks = Array.map (link_nodes graph) blocks in
          let data = Dagmap.get name graph in
          Component (name, data, blocks, pats))
    nodes

let link_src graph = function
  | Typechecker.Src (_, nodes) -> Src (link_nodes graph nodes)
  | Fun (_, props, f) -> Fun (props, f)

let make ~fname components src =
  let nodes = parse ~fname src in
  let typed = Typechecker.make ~root:fname components.Components.typed nodes in
  let g = Dagmap.make ~f:link_src ~root:fname components.optimized in
  let nodes = make_nodes typed |> link_nodes g in
  { name = fname; types = typed.types; nodes; components = Dagmap.linked g }

let from_string ~fname components src =
  make ~fname components (Lexing.from_string src)

let from_channel ~fname components src =
  make ~fname components (Lexing.from_channel src)

let interface_from_string ~fname src =
  parse_interface ~fname (Lexing.from_string src)
  |> Typechecker.make_interface_standalone

let interface_from_channel ~fname src =
  parse_interface ~fname (Lexing.from_channel src)
  |> Typechecker.make_interface_standalone

let rec node_to_sexp = function
  | Text s -> Sexp.make "text" [ Sexp.string s ]
  | Echo (l, fmt, ech, esc) ->
      Sexp.make "echo"
        [
          Sexp.of_seq
            (Sexp.pair Ast.echo_format_to_sexp echo_to_sexp)
            (List.to_seq l);
          Ast.echo_format_to_sexp fmt;
          echo_to_sexp ech;
          Ast.escape_to_sexp esc;
        ]
  | Match (blocks, data, matching) ->
      Sexp.make "match"
        [
          Sexp.of_seq (Sexp.pair Sexp.int to_sexp) (Array.to_seqi blocks);
          Sexp.of_seq data_to_sexp (Array.to_seq data);
          Matching.to_sexp to_sexp matching;
        ]
  | Map_list (blocks, data, matching) ->
      Sexp.make "map_list"
        [
          Sexp.of_seq (Sexp.pair Sexp.int to_sexp) (Array.to_seqi blocks);
          data_to_sexp data;
          Matching.to_sexp to_sexp matching;
        ]
  | Map_dict (blocks, data, matching) ->
      Sexp.make "map_dict"
        [
          Sexp.of_seq (Sexp.pair Sexp.int to_sexp) (Array.to_seqi blocks);
          data_to_sexp data;
          Matching.to_sexp to_sexp matching;
        ]
  | Component (name, _function, blocks, props) ->
      Sexp.make "component"
        [
          Sexp.of_seq (Sexp.pair Sexp.int to_sexp) (Array.to_seqi blocks);
          Sexp.string name;
          assoc_to_sexp props;
        ]

and echo_to_sexp = function
  | `Field (e, s) -> Sexp.make "field" [ echo_to_sexp e; Sexp.string s ]
  | (`Var _ | `String _) as x -> data_to_sexp x

and data_to_sexp = function
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
and to_sexp l = Sexp.of_seq node_to_sexp (List.to_seq l)
