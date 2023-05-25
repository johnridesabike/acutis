(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2023 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module C = Compile
module D = Data
module M = Matching
module Ty = Typescheme

type filepath = Filepath of string [@@unboxed]

module Id : sig
  (** This module controls variable IDs to keep them consistent across the
      program. *)

  type t
  type id = t

  val component : string -> t
  val error : string -> t
  val arg : int -> t
  val resolved : int -> t
  val index : t
  val exit : t
  val result : t
  val entry : t
  val runtime_escape : t
  val runtime_main : t

  module Safe : sig
    (** This creates names that are unique for each instance of [t]. It's
        necessary for generating data decoders. *)

    type t

    val create : unit -> t
    val tuple : t -> id
    val nullable : t -> id
    val record : t -> id
    val union : t -> id
    val key : t -> id
    val input_hd : t -> id
    val dict : t -> id
    val dst_base : t -> id
    val dst : t -> id
    val dst_new : t -> id
    val input : t -> id
    val array : t -> id
  end

  module Data : sig
    (** This is the main variable which holds the program's data. For each level
        of nesting in the program, we need to add a scope to this. *)

    type t

    val initial : t
    val add_scope : t -> t
    val to_id : t -> id
  end

  module Map : Stdlib.Map.S with type key = t

  val add_unique_namespace : filepath -> filepath Map.t -> t * filepath Map.t
  (** Takes a given module path and a map of all existing imported module
      paths. Returns a JavaScript namespace and an updated map. Guarantees that
      all import namespaces are unique and that each module is only imported
      once. *)

  val pp : Format.formatter -> t -> unit
end = struct
  type t = string
  type id = t

  let component name = Printf.sprintf "template_%s" name
  let error name = Printf.sprintf "error_%s" name
  let arg i = Printf.sprintf "arg%i" i
  let resolved i = Printf.sprintf "resolved%i" i
  let index = "index"
  let exit = "exit"
  let result = "result"
  let entry = "entry"
  let runtime_escape = "acutis_escape"
  let runtime_main = "main"

  module Safe = struct
    type t = (string, int) Hashtbl.t

    let create () = Hashtbl.create 16

    let make name env =
      match Hashtbl.find_opt env name with
      | None ->
          Hashtbl.add env name 1;
          Printf.sprintf "%s1" name
      | Some i ->
          let i = succ i in
          Hashtbl.add env name i;
          Printf.sprintf "%s%i" name i

    let tuple = make "tuple"
    let key = make "key"
    let dict = make "dict"
    let dst_base = make "dst_base"
    let dst = make "dst"
    let dst_new = make "dst_new"
    let input_hd = make "input_hd"
    let nullable = make "nullable"
    let record = make "record"
    let union = make "union"
    let input = make "input"
    let array = make "array"
  end

  module Data = struct
    type t = int

    let initial = 0
    let add_scope = succ
    let to_id = function 0 -> "data" | i -> Printf.sprintf "data%i" i
  end

  module Map = Map.String

  let rec add_unique_namespace_aux i namespace (Filepath module_path) map =
    let namespace =
      match i with
      | 0 -> Printf.sprintf "External_%s" namespace
      | i -> Printf.sprintf "External_%s%i" namespace i
    in
    match Map.find_opt namespace map with
    | None -> (namespace, Map.add namespace (Filepath module_path) map)
    | Some (Filepath filename) when String.equal filename module_path ->
        (namespace, map)
    | Some _ ->
        add_unique_namespace_aux (succ i) namespace (Filepath module_path) map

  let add_unique_namespace (Filepath module_path) map =
    let namespace =
      Filename.basename module_path |> Filename.remove_extension
    in
    add_unique_namespace_aux 0 namespace (Filepath module_path) map

  let pp = Format.pp_print_string
end

type keyword = Null | Undefined | False | True
type obj = Object | Array | Map | Promise

(* Sometimes these use list and sometimes these use Seq.t depending on whether
   we need to convert intermediate values to a Seq.t already. *)

type expr =
  | Prim of keyword
  | Var of Id.t
  | Obj of obj
  | Not_eq of expr * expr
  | Eq of expr * expr
  | Or of expr * expr
  | And of expr * expr
  | String of string
  | Int of int
  | Float of float
  | Arr of expr Seq.t
  | Field of expr * expr
  | Tern of expr * expr * expr
  | Fun_expr of Id.t list * statement list
  | App of expr * expr list
  | New of obj * expr list
  | App_expr of expr * expr list
  | Await of expr
  | Typeof of expr
  | Instanceof of expr * obj
  | To_int of expr
  | In of string * expr

and statement =
  | Let of Id.t * expr
  | Set of expr * expr
  | Switch of expr * (expr * statement list) Seq.t * statement list
  | If_else of expr * statement list * statement list
  | While of expr * statement list
  | For_in of Id.t * expr * statement list
  | For_of of Id.t * expr * statement list
  | Expr of expr
  | Incr of Id.t
  | Error of Id.t
  | Return of expr
  | Fun of Id.t * Id.t list * statement list
  | Export_default of statement
  | Comment of string
  | Import of Id.t * string
  | Raw of string

type jstype = CommonJs | ESModule

let pp_comma ppf () = Format.fprintf ppf ",@ "

let pp_trailing_comma =
  Format.pp_print_custom_break ~fits:("", 0, "") ~breaks:(",", -2, "")

(** See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String#escape_sequences *)
let pp_js_str ppf s =
  let open Format in
  let l = String.length s in
  pp_print_char ppf '"';
  for i = 0 to l - 1 do
    let c = s.[i] in
    match c with
    | '\n' -> fprintf ppf "\\n\\\n"
    | '\b' -> pp_print_string ppf "\\b"
    | '\t' -> pp_print_string ppf "\\t"
    | '\012' -> pp_print_string ppf "\\f"
    | '\r' -> pp_print_string ppf "\\r"
    | '\\' -> pp_print_string ppf "\\\\"
    | '"' -> pp_print_string ppf "\\\""
    | c -> pp_print_char ppf c
  done;
  pp_print_char ppf '"'

let keyword_to_string = function
  | Null -> "null"
  | Undefined -> "undefined"
  | False -> "false"
  | True -> "true"

let obj_to_string = function
  | Object -> "Object"
  | Array -> "Array"
  | Map -> "Map"
  | Promise -> "Promise"

let pp_obj ppf x = Format.pp_print_string ppf (obj_to_string x)

let is_js_id =
  let id_start_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' | '$' -> true
    | _ -> false
  in
  let id_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '$' -> true
    | _ -> false
  in
  fun s -> id_start_char s.[0] && String.for_all id_char s

let pp_statement ty =
  let open Format in
  let pp_import =
    match ty with
    | CommonJs -> fun ppf -> fprintf ppf "const %a = require(%a);"
    | ESModule -> fun ppf -> fprintf ppf "import * as %a from %a;"
  in
  let pp_export =
    match ty with
    | CommonJs -> fun ppf -> fprintf ppf "module.exports = %a"
    | ESModule -> fun ppf -> fprintf ppf "export default %a"
  in
  let rec pp_expr ppf = function
    | Eq (a, b) -> fprintf ppf "%a ===@ %a" pp_expr a pp_expr b
    | Not_eq (a, b) -> fprintf ppf "%a !== %a" pp_expr a pp_expr b
    | Or (a, b) -> fprintf ppf "@[<hv>%a ||@]@ @[<hv>%a@]" pp_expr a pp_expr b
    | And (a, b) -> fprintf ppf "@[<hv>%a &&@]@ @[<hv>%a@]" pp_expr a pp_expr b
    | Var x -> Id.pp ppf x
    | Prim x -> pp_print_string ppf (keyword_to_string x)
    | Obj x -> pp_obj ppf x
    | String s -> pp_js_str ppf s
    | Int i -> fprintf ppf "%i" i
    | Float f -> pp_print_float ppf f
    | Field (a, String s) when is_js_id s -> fprintf ppf "%a.%s" pp_expr a s
    | Field (a, b) -> fprintf ppf "%a[%a]" pp_expr a pp_expr b
    | Tern (cond, i, e) ->
        fprintf ppf "%a@ ? %a@ : %a" pp_expr cond pp_expr i pp_expr e
    | Fun_expr (args, statements) ->
        fprintf ppf "(async function (%a) {@ @[<v 0>%a@]@;<1 -2>})"
          (pp_print_list ~pp_sep:pp_comma Id.pp)
          args pp_statements statements
    | App (name, [ arg ]) -> fprintf ppf "%a(%a)" pp_expr name pp_expr arg
    | App (name, args) ->
        fprintf ppf "@[<hv 2>%a(@,%a@;<0 -2>)@]" pp_expr name
          (pp_print_list ~pp_sep:pp_comma (fun ppf expr ->
               fprintf ppf "@[<hv 2>%a@]" pp_expr expr))
          args
    | New (name, [ arg ]) -> fprintf ppf "new %a(%a)" pp_obj name pp_expr arg
    | New (name, args) ->
        fprintf ppf "@[<hv 2>new %a(@,%a@;<0 -2>)@]" pp_obj name
          (pp_print_list ~pp_sep:pp_comma (fun ppf expr ->
               fprintf ppf "@[<hv 2>%a@]" pp_expr expr))
          args
    | App_expr (expr, args) ->
        fprintf ppf "%a(%a)" pp_expr expr
          (pp_print_list ~pp_sep:pp_comma pp_expr)
          args
    | Await expr -> fprintf ppf "(await %a)" pp_expr expr
    | Arr a ->
        fprintf ppf "[@,%a%t]"
          (pp_print_seq ~pp_sep:pp_comma (fun ppf expr ->
               fprintf ppf "@[<hv 2>%a@]" pp_expr expr))
          a pp_trailing_comma
    | Typeof expr -> fprintf ppf "typeof %a" pp_expr expr
    | Instanceof (a, b) -> fprintf ppf "%a instanceof %a" pp_expr a pp_obj b
    | To_int expr -> fprintf ppf "%a | 0" pp_expr expr
    | In (key, expr) -> fprintf ppf "%a in %a" pp_js_str key pp_expr expr
  and pp_statement ppf = function
    | Let (ident, expr) ->
        fprintf ppf "@[<hv 2>let %a = %a;@]" Id.pp ident pp_expr expr
    | Set (subj, pred) ->
        fprintf ppf "@[<hv 2>%a =@ @[<hv 2>%a@];@]" pp_expr subj pp_expr pred
    | Switch (expr, cases, default) ->
        fprintf ppf "@[<v 2>@[<hv 2>switch (%a)@] {@ " pp_expr expr;
        pp_print_seq ~pp_sep:pp_print_cut
          (fun ppf (expr, statements) ->
            match statements with
            | [ (Return _ as statement) ] ->
                fprintf ppf "@[<v 2>case %a:@ %a@]" pp_expr expr pp_statement
                  statement
            | statements ->
                fprintf ppf "@[<v 2>case %a:@ %a@ break;@]" pp_expr expr
                  pp_statements statements)
          ppf cases;
        (match default with
        | [] -> ()
        | l ->
            fprintf ppf "@ @[<v 2>default:@ ";
            pp_statements ppf l;
            fprintf ppf "@]");
        fprintf ppf "@;<1 -2>}@]"
    | If_else (expr, ifs, elses) ->
        fprintf ppf "@[<v 2>@[<hv 2>if (@,%a@;<0 -2>)@] {@ " pp_expr expr;
        pp_statements ppf ifs;
        (match elses with
        | [] -> ()
        | l ->
            fprintf ppf "@;<1 -2>} else {@ ";
            pp_statements ppf l);
        fprintf ppf "@;<1 -2>}@]"
    | While (cond, statements) ->
        fprintf ppf "@[<v 2>while (%a) {@ %a@;<1 -2>}@]" pp_expr cond
          pp_statements statements
    | For_in (prop, expr, statements) ->
        fprintf ppf "@[<v 2>for (let %a in %a) {@ %a@;<1 -2>}@]" Id.pp prop
          pp_expr expr pp_statements statements
    | For_of (prop, expr, statements) ->
        fprintf ppf "@[<v 2>for (let %a of %a) {@ %a@;<1 -2>}@]" Id.pp prop
          pp_expr expr pp_statements statements
    | Expr expr -> fprintf ppf "@[<hv 2>%a;@]" pp_expr expr
    | Incr id -> fprintf ppf "%a++;" Id.pp id
    | Error s -> fprintf ppf "throw new Error(%a);" Id.pp s
    | Return expr -> fprintf ppf "@[<hv 2>return %a;@]" pp_expr expr
    | Fun (name, args, statements) ->
        fprintf ppf "async function %a(%a) {@;<0 2>@[<v 0>%a@]@;<1 -2>}" Id.pp
          name
          (pp_print_list ~pp_sep:pp_comma Id.pp)
          args pp_statements statements
    | Export_default statement -> pp_export ppf pp_statement statement
    | Comment s -> fprintf ppf "/* %s */" s
    | Import (namespace, filename) ->
        pp_import ppf Id.pp namespace pp_js_str filename
    | Raw s -> fprintf ppf "@[%a@]" pp_print_text s
  and pp_statements ppf l =
    pp_print_list ~pp_sep:pp_print_cut pp_statement ppf l
  in
  pp_statement

let pp_statement ty ppf x =
  pp_statement ty ppf x;
  Format.pp_print_cut ppf ();
  Format.pp_print_cut ppf ()

let meth_get x k = App (Field (x, String "get"), [ k ])
let meth_set x k v = App (Field (x, String "set"), [ k; v ])
let meth_join x = App (Field (x, String "join"), [ String "" ])
let meth_push x v = App (Field (Var x, String "push"), v)
let promise_all x = Await (App (Field (Obj Promise, String "all"), [ x ]))

let of_const = function
  | Data.Const.String s -> String s
  | Data.Const.Int i -> Int i
  | Data.Const.Float f -> Float f

module Err = struct
  let decode_bool = Id.error "decode_bool"
  let decode_bool_msg = String "This field must be a boolean."
  let decode_str = Id.error "decode_str"
  let decode_str_msg = String "This field must be a string."
  let decode_str_enum = Id.error "decode_str_enum"
  let decode_str_enum_msg = String "This field must be a string enum."
  let decode_int = Id.error "decode_int"
  let decode_int_msg = String "This field must be an int."
  let decode_int_enum = Id.error "decode_int_enum"
  let decode_int_enum_msg = String "This field must be an int enum."
  let decode_float = Id.error "decode_float"
  let decode_float_msg = String "This field must be a float."
  let decode_array = Id.error "decode_array"
  let decode_array_msg = String "This field must be an array."
  let decode_missing_field = Id.error "decode_missing_field"
  let decode_missing_field_msg = String "This object is missing a field."
  let decode_bad_union_key = Id.error "decode_bad_union_key"
  let decode_bad_union_key_msg = String "This object is missing a field."
  let pattern_failure = Id.error "pattern_failure"

  let pattern_failure_msg =
    String
      "This pattern-matching failed to find a path. This probably means \
       there's a problem with the compiler."

  let defs =
    [
      (decode_bool, decode_bool_msg);
      (decode_str, decode_str_msg);
      (decode_str_enum, decode_str_enum_msg);
      (decode_int, decode_int_msg);
      (decode_int_enum, decode_int_enum_msg);
      (decode_float, decode_float_msg);
      (decode_array, decode_array_msg);
      (decode_missing_field, decode_missing_field_msg);
      (decode_bad_union_key, decode_bad_union_key_msg);
      (pattern_failure, pattern_failure_msg);
    ]
end

let fmt x = function
  | C.Fmt_string -> x
  | C.Fmt_int | C.Fmt_float -> App (Field (x, String "toString"), [])
  | C.Fmt_bool -> Tern (x, String "true", String "false")

let escape x = function
  | C.No_escape -> x
  | C.Escape -> App (Var Id.runtime_escape, [ x ])

let rec echo data_id = function
  | C.Echo_var s -> meth_get (Var (Id.Data.to_id data_id)) (String s)
  | C.Echo_string s -> String s
  | C.Echo_field (e, s) -> meth_get (echo data_id e) (String s)

let rec echoes data_id default default_fmt = function
  | [] -> fmt (echo data_id default) default_fmt
  | (f, e) :: tl ->
      let e = echo data_id e in
      Tern
        (Not_eq (e, Prim Null), fmt e f, echoes data_id default default_fmt tl)

let add_vars ids arg vars =
  Set.Int.fold (fun id vars -> Map.Int.add id arg vars) ids vars

let entry_index = Field (Var Id.entry, Int 0)
let entry_value = Field (Var Id.entry, Int 1)
let list_hd = Field (Var (Id.arg 0), Int 0)
let list_tl = Field (Var (Id.arg 0), Int 1)
let list_index = Var Id.index
let arg_map = function 0 -> list_hd | _ -> list_index
let arg_map_dict = function 0 -> entry_value | _ -> entry_index
let arg_match key = Var (Id.arg key)
let arg_str id key = meth_get id (String key)
let arg_int id key = Field (id, Int key)

let rec match_tree :
          'leaf 'key.
          leafstmt:(vars:expr Map.Int.t -> 'leaf -> statement list) ->
          arg:('key -> expr) ->
          vars:expr Map.Int.t ->
          ('leaf, 'key) M.tree ->
          statement list =
 fun ~leafstmt ~arg ~vars -> function
  | M.Switch { key; ids; cases; wildcard; _ } ->
      let arg' = arg key in
      let vars = add_vars ids arg' vars in
      [
        Switch
          ( arg',
            switchcase ~leafstmt ~arg ~vars cases,
            match wildcard with
            | None -> []
            | Some l -> match_tree ~leafstmt ~arg ~vars l );
      ]
  | M.Wildcard { key; ids; child } ->
      let arg' = arg key in
      let vars = add_vars ids arg' vars in
      match_tree ~leafstmt ~arg ~vars child
  | M.Nest { key; ids; child; wildcard; _ } ->
      let arg' = arg key in
      let vars = add_vars ids arg' vars in
      List.concat
        [
          (match child with
          | Int_keys tree ->
              match_tree
                ~leafstmt:(match_tree ~leafstmt ~arg)
                ~arg:(arg_int arg') ~vars tree
          | String_keys tree ->
              match_tree
                ~leafstmt:(match_tree ~leafstmt ~arg)
                ~arg:(arg_str arg') ~vars tree);
          (match wildcard with
          | None -> []
          | Some tree ->
              [
                If_else
                  ( Eq (Var Id.exit, Prim Null),
                    match_tree ~leafstmt ~arg ~vars tree,
                    [] );
              ]);
        ]
  | M.Construct { key; ids; nil; cons } -> (
      let arg' = arg key in
      let vars = add_vars ids arg' vars in
      match (nil, cons) with
      | Some nil, Some cons ->
          [
            If_else
              ( Eq (arg', Prim Null),
                match_tree ~leafstmt ~arg ~vars nil,
                match_tree ~leafstmt ~arg ~vars cons );
          ]
      | Some nil, None -> match_tree ~leafstmt ~arg ~vars nil
      | None, Some cons -> match_tree ~leafstmt ~arg ~vars cons
      | None, None -> [])
  | M.End leaf -> leafstmt ~vars leaf

and switchcase :
      'leaf 'key.
      leafstmt:(vars:expr Map.Int.t -> 'leaf -> statement list) ->
      arg:('key -> expr) ->
      vars:expr Map.Int.t ->
      ('leaf, 'key) M.switchcase ->
      (expr * statement list) Seq.t =
 fun ~leafstmt ~arg ~vars M.{ data; if_match; next } ->
  Seq.cons (of_const data, match_tree ~leafstmt ~arg ~vars if_match)
  @@
  match next with
  | None -> Seq.empty
  | Some l -> switchcase ~leafstmt ~arg ~vars l

let match_leaf data_id ~vars M.{ names; exit } =
  Set (Var Id.exit, Int (M.Exit.key_to_int exit))
  :: Map.String.fold
       (fun key id acc ->
         Expr
           (meth_set
              (Var (Id.Data.to_id data_id))
              (String key) (Map.Int.find id vars))
         :: acc)
       names []

let rec nodes_array data_id l = Arr (List.to_seq l |> Seq.map (node data_id))
and nodes_string data_id l = meth_join (promise_all (nodes_array data_id l))

and node data_id = function
  | C.Text s -> String s
  | C.Echo (echs, f, default, esc) -> escape (echoes data_id default f echs) esc
  | C.Match (args, tree) ->
      App_expr (Fun_expr ([], match_ data_id args tree), [])
  | C.Map_list (arg, tree) ->
      App_expr (Fun_expr ([], map_list data_id arg tree), [])
  | C.Map_dict (arg, tree) ->
      App_expr (Fun_expr ([], map_dict data_id arg tree), [])
  | C.Component (name, _, dict) ->
      App_expr
        ( Fun_expr
            ( [],
              construct_data_dict data_id dict
              @ [ Return (App (Var (Id.component name), [ Var (Id.arg 0) ])) ]
            ),
          [] )

and match_ data_id args M.{ tree; exits } =
  let data_id' = Id.Data.add_scope data_id in
  List.concat
    [
      [
        Let (Id.Data.to_id data_id', New (Map, [ Var (Id.Data.to_id data_id) ]));
        Let (Id.exit, Prim Null);
      ];
      construct_data data_id args;
      match_tree ~leafstmt:(match_leaf data_id') ~arg:arg_match
        ~vars:Map.Int.empty tree;
      [
        Switch
          ( Var Id.exit,
            M.Exit.to_seqi exits
            |> Seq.map (fun (i, l) ->
                   ( Int (M.Exit.key_to_int i),
                     [ Return (nodes_string data_id' l) ] )),
            [ Error Err.pattern_failure ] );
      ];
    ]

and map_list data_id arg M.{ tree; exits } =
  let data_id' = Id.Data.add_scope data_id in
  List.concat
    [
      [ Let (Id.result, Arr Seq.empty); Let (Id.index, Int 0) ];
      construct_datum data_id arg;
      [
        While
          ( Not_eq (Var (Id.arg 0), Prim Null),
            List.concat
              [
                [
                  Let
                    ( Id.Data.to_id data_id',
                      New (Map, [ Var (Id.Data.to_id data_id) ]) );
                  Let (Id.exit, Prim Null);
                ];
                match_tree ~leafstmt:(match_leaf data_id') ~arg:arg_map
                  ~vars:Map.Int.empty tree;
                [
                  Switch
                    ( Var Id.exit,
                      M.Exit.to_seqi exits
                      |> Seq.map (fun (i, l) ->
                             ( Int (M.Exit.key_to_int i),
                               [
                                 Expr
                                   (meth_push Id.result
                                      (List.map (node data_id') l));
                               ] )),
                      [ Error Err.pattern_failure ] );
                  Incr Id.index;
                  Set (Var (Id.arg 0), list_tl);
                ];
              ] );
      ];
      [ Return (meth_join (promise_all (Var Id.result))) ];
    ]

and map_dict data_id arg M.{ tree; exits } =
  let data_id' = Id.Data.add_scope data_id in
  List.concat
    [
      [ Let (Id.result, Arr Seq.empty) ];
      construct_datum data_id arg;
      [
        For_of
          ( Id.entry,
            Var (Id.arg 0),
            List.concat
              [
                [
                  Let
                    ( Id.Data.to_id data_id',
                      New (Map, [ Var (Id.Data.to_id data_id) ]) );
                  Let (Id.exit, Prim Null);
                ];
                match_tree ~leafstmt:(match_leaf data_id') ~arg:arg_map_dict
                  ~vars:Map.Int.empty tree;
                [
                  Switch
                    ( Var Id.exit,
                      M.Exit.to_seqi exits
                      |> Seq.map (fun (i, l) ->
                             ( Int (M.Exit.key_to_int i),
                               [
                                 Expr
                                   (meth_push Id.result
                                      (List.map (node data_id') l));
                               ] )),
                      [ Error Err.pattern_failure ] );
                ];
              ] );
      ];
      [ Return (meth_join (promise_all (Var Id.result))) ];
    ]

and construct_data_aux data_id async_queue = function
  | D.Nil -> Prim Null
  | D.Array a ->
      Arr (Array.map (construct_data_aux data_id async_queue) a |> Array.to_seq)
  | D.Dict d ->
      New
        ( Map,
          [
            Arr
              (Map.String.map (construct_data_aux data_id async_queue) d
              |> Map.String.to_seq
              |> Seq.map (fun (k, v) ->
                     Arr (Seq.cons (String k) @@ Seq.cons v @@ Seq.empty)));
          ] )
  | D.Const c -> of_const c
  | D.Other (C.Var s) -> meth_get (Var (Id.Data.to_id data_id)) (String s)
  | D.Other (C.Block l) ->
      Queue.add (nodes_string data_id l) async_queue;
      Var (Id.resolved (pred (Queue.length async_queue)))
  | D.Other (C.Field (d, s)) ->
      meth_get (construct_data_aux data_id async_queue d) (String s)

and construct_data data_id args =
  let async_queue = Queue.create () in
  let args =
    Array.mapi
      (fun i arg -> Let (Id.arg i, construct_data_aux data_id async_queue arg))
      args
    |> Array.to_list
  in
  Seq.fold_lefti
    (fun l i nodes -> Let (Id.resolved i, nodes) :: l)
    args (Queue.to_seq async_queue)

and construct_datum data_id arg =
  let async_queue = Queue.create () in
  let arg = Let (Id.arg 0, construct_data_aux data_id async_queue arg) in
  Seq.fold_lefti
    (fun l i nodes -> Let (Id.resolved i, nodes) :: l)
    [ arg ] (Queue.to_seq async_queue)

and construct_data_dict data_id dict =
  let async_queue = Queue.create () in
  let arg =
    Let (Id.arg 0, construct_data_aux data_id async_queue (D.Dict dict))
  in
  Seq.fold_lefti
    (fun l i nodes -> Let (Id.resolved i, nodes) :: l)
    [ arg ] (Queue.to_seq async_queue)

let decode_boolean ~set input cases =
  Switch
    ( input,
      Seq.concat
        (Seq.cons
           (if Set.Int.mem 0 cases then Seq.return (Prim False, [ set (Int 0) ])
            else Seq.empty)
        @@ Seq.return
             (if Set.Int.mem 1 cases then Seq.return (Prim True, [ set (Int 1) ])
              else Seq.empty)),
      [ Error Err.decode_bool ] )

let decode_string ~set input =
  If_else
    (Eq (Typeof input, String "string"), [ set input ], [ Error Err.decode_str ])

let decode_string_enum ~set input cases =
  Switch
    ( input,
      Set.String.to_seq cases
      |> Seq.map (fun s -> (String s, [ set (String s) ])),
      [ Error Err.decode_str_enum ] )

let decode_int ~set input =
  If_else
    ( Eq (Typeof input, String "number"),
      [ set (To_int input) ],
      [ Error Err.decode_int ] )

let decode_int_enum ~set input cases =
  Switch
    ( input,
      Set.Int.to_seq cases |> Seq.map (fun i -> (Int i, [ set (Int i) ])),
      [ Error Err.decode_int_enum ] )

let decode_float ~set input =
  If_else
    ( Eq (Typeof input, String "number"),
      [ set input ],
      [ Error Err.decode_float ] )

let rec decode_typescheme ~set input env ty =
  match !ty with
  | Ty.Unknown _ -> [ set input ]
  | Enum { extra = Bool; cases = VInt cases; _ } ->
      [ decode_boolean ~set input cases ]
  | String | Enum { row = `Open; cases = VString _; _ } ->
      [ decode_string ~set input ]
  | Enum { row = `Closed; cases = VString cases; _ } ->
      [ decode_string_enum ~set input cases ]
  | Int | Enum { row = `Open; cases = VInt _; _ } -> [ decode_int ~set input ]
  | Enum { row = `Closed; cases = VInt cases; _ } ->
      [ decode_int_enum ~set input cases ]
  | Float -> [ decode_float ~set input ]
  | Nullable ty -> [ decode_nullable ~set input env ty ]
  | List ty -> [ decode_list ~set input env ty ]
  | Tuple tys -> [ decode_tuple ~set input env tys ]
  | Record tys -> decode_record ~set input env !tys
  | Dict (ty, _) -> decode_dict ~set input env ty
  | Union (key, variant) -> decode_union ~set input env key variant

and decode_tuple ~set input env tys =
  let tuple = Id.Safe.tuple env in
  let length = List.length tys in
  If_else
    ( And
        ( Instanceof (input, Array),
          Eq (Field (input, String "length"), Int length) ),
      Let (tuple, New (Array, [ Int length ]))
      :: set (Var tuple)
      :: List.concat
           (List.mapi
              (fun index ty ->
                let input' = Id.Safe.input env in
                Let (input', Field (input, Int index))
                :: decode_typescheme
                     ~set:(fun id -> Set (Field (Var tuple, Int index), id))
                     (Var input') env ty)
              tys),
      [ Error Err.decode_array ] )

and decode_dict ~set input env ty =
  let key = Id.Safe.key env in
  let dict = Id.Safe.dict env in
  let input' = Id.Safe.input env in
  [
    Let (dict, New (Map, []));
    set (Var dict);
    For_in
      ( key,
        input,
        Let (input', Field (input, Var key))
        :: decode_typescheme
             ~set:(fun id -> Expr (meth_set (Var dict) (Var key) id))
             (Var input') env ty );
  ]

and decode_list ~set input env ty =
  let dst_base = Id.Safe.dst_base env in
  let dst = Id.Safe.dst env in
  let dst_new = Id.Safe.dst_new env in
  let input_hd = Id.Safe.input_hd env in
  let new_cell = New (Array, [ Int 2 ]) in
  let hd = Field (Var dst_new, Int 0) in
  If_else
    ( Instanceof (input, Array),
      [
        Let (dst_base, new_cell);
        Let (dst, Var dst_base);
        For_of
          ( input_hd,
            input,
            List.concat
              [
                [ Let (dst_new, new_cell) ];
                decode_typescheme
                  ~set:(fun id -> Set (hd, id))
                  (Var input_hd) env ty;
                [
                  Set (Field (Var dst, Int 1), Var dst_new);
                  Set (Var dst, Var dst_new);
                ];
              ] );
        Set (Field (Var dst, Int 1), Prim Null);
        set (Field (Var dst_base, Int 1));
      ],
      [ Error Err.decode_array ] )

and decode_nullable ~set input env ty =
  let nullable = Id.Safe.nullable env in
  let nullable_value = Field (Var nullable, Int 0) in
  If_else
    ( Or (Eq (input, Prim Null), Eq (input, Prim Undefined)),
      [ set (Prim Null) ],
      Let (nullable, New (Array, [ Int 1 ]))
      :: set (Var nullable)
      :: decode_typescheme
           ~set:(fun id -> Set (nullable_value, id))
           input env ty )

and decode_record_aux ~data input env tys =
  Map.String.to_seq tys
  |> Seq.map (fun (k, ty) ->
         let set id = Expr (meth_set data (String k) id) in
         let input' = Id.Safe.input env in
         If_else
           ( In (k, input),
             Let (input', Field (input, String k))
             :: decode_typescheme ~set (Var input') env ty,
             match !ty with
             | Nullable _ | Unknown _ -> [ set (Prim Null) ]
             | _ -> [ Error Err.decode_missing_field ] ))
  |> List.of_seq

and decode_record ~set input env tys =
  let record = Id.Safe.record env in
  Let (record, New (Map, []))
  :: set (Var record)
  :: decode_record_aux ~data:(Var record) input env tys

and decode_union ~set input env key variant =
  let union = Id.Safe.union env in
  let set_data_key x = Expr (meth_set (Var union) (String key) x) in
  let input_key = Id.Safe.input env in
  [
    Let (union, New (Map, []));
    set (Var union);
    Let (input_key, Field (input, String key));
    Switch
      ( Var input_key,
        (match variant with
        | { cases = VInt map; extra = Bool; _ } ->
            Map.Int.to_seq map
            |> Seq.map (fun (k, v) ->
                   match k with
                   | 0 ->
                       ( Prim False,
                         set_data_key (Int k)
                         :: decode_record_aux ~data:(Var union) input env !v )
                   | k ->
                       ( Prim True,
                         set_data_key (Int k)
                         :: decode_record_aux ~data:(Var union) input env !v ))
        | { cases = VInt map; _ } ->
            Map.Int.to_seq map
            |> Seq.map (fun (k, v) ->
                   ( Int k,
                     set_data_key (Int k)
                     :: decode_record_aux ~data:(Var union) input env !v ))
        | { cases = VString map; _ } ->
            Map.String.to_seq map
            |> Seq.map (fun (k, v) ->
                   ( String k,
                     set_data_key (String k)
                     :: decode_record_aux ~data:(Var union) input env !v ))),
        match variant with
        | { cases = VInt _; row = `Open; _ } ->
            decode_typescheme
              ~set:(fun id -> Expr (meth_set (Var union) (String key) id))
              (Var input_key) env (Ty.int ())
        | { cases = VString _; row = `Open; _ } ->
            decode_typescheme
              ~set:(fun id -> Expr (meth_set (Var union) (String key) id))
              (Var input_key) env (Ty.string ())
        | { row = `Closed; _ } -> [ Error Err.decode_bad_union_key ] );
  ]

let rec encode_typescheme ~set input env ty =
  match !ty with
  | Ty.Unknown _ -> [ set input ]
  | Enum { extra = Bool; _ } ->
      [ If_else (input, [ set (Prim True) ], [ set (Prim False) ]) ]
  | String | Int | Float | Enum _ -> [ set input ]
  | Nullable ty -> [ encode_nullable ~set input env ty ]
  | List ty -> encode_list ~set input env ty
  | Tuple tys -> encode_tuple ~set input env tys
  | Record tys -> encode_record ~set input env !tys
  | Dict (ty, _) -> encode_dict ~set input env ty
  | Union (key, variant) -> encode_union ~set input env key variant

and encode_nullable ~set input env ty =
  let input' = Id.Safe.input env in
  If_else
    ( Eq (input, Prim Null),
      [ set (Prim Null) ],
      Let (input', Field (input, Int 0))
      :: encode_typescheme ~set (Var input') env ty )

and encode_list ~set input env ty =
  let array = Id.Safe.array env in
  let input' = Id.Safe.input env in
  [
    Let (array, New (Array, []));
    set (Var array);
    While
      ( Not_eq (input, Prim Null),
        List.concat
          [
            [ Let (input', Field (input, Int 0)) ];
            encode_typescheme
              ~set:(fun input -> Expr (meth_push array [ input ]))
              (Var input') env ty;
            [ Set (input, Field (input, Int 1)) ];
          ] );
  ]

and encode_tuple ~set input env tys =
  let array = Id.Safe.array env in
  Let (array, New (Array, [ Int (List.length tys) ]))
  :: set (Var array)
  :: (List.mapi
        (fun i ty ->
          let input' = Id.Safe.input env in
          Let (input', Field (input, Int i))
          :: encode_typescheme
               ~set:(fun input -> Set (Field (Var array, Int i), input))
               (Var input') env ty)
        tys
     |> List.concat)

and encode_record_aux ~data input env tys =
  Map.String.to_seq tys
  |> Seq.map (fun (k, ty) ->
         let set id = Set (Field (data, String k), id) in
         let input' = Id.Safe.input env in
         Let (input', meth_get input (String k))
         :: encode_typescheme ~set (Var input') env ty)
  |> List.of_seq |> List.concat

and encode_record ~set input env tys =
  let record = Id.Safe.record env in
  Let (record, New (Object, []))
  :: set (Var record)
  :: encode_record_aux ~data:(Var record) input env tys

and encode_dict ~set input env ty =
  let dict = Id.Safe.dict env in
  [
    Let (dict, New (Object, []));
    set (Var dict);
    For_of
      ( Id.entry,
        input,
        encode_typescheme
          ~set:(fun input -> Set (Field (Var dict, entry_index), input))
          entry_value env ty );
  ]

and encode_union ~set input env key variant =
  let union = Id.Safe.union env in
  let input_key = Id.Safe.input env in
  let set_data_key x = Set (Field (Var union, String key), x) in
  [
    Let (union, New (Object, []));
    set (Var union);
    Let (input_key, meth_get input (String key));
    Switch
      ( Var input_key,
        (match variant with
        | { cases = VInt map; extra = Bool; _ } ->
            Map.Int.to_seq map
            |> Seq.map (fun (k, v) ->
                   match k with
                   | 0 ->
                       ( Int 0,
                         set_data_key (Prim False)
                         :: encode_record_aux ~data:(Var union) input env !v )
                   | k ->
                       ( Int k,
                         set_data_key (Prim True)
                         :: encode_record_aux ~data:(Var union) input env !v ))
        | { cases = VInt map; _ } ->
            Map.Int.to_seq map
            |> Seq.map (fun (k, v) ->
                   ( Int k,
                     set_data_key (Int k)
                     :: encode_record_aux ~data:(Var union) input env !v ))
        | { cases = VString map; _ } ->
            Map.String.to_seq map
            |> Seq.map (fun (k, v) ->
                   ( String k,
                     set_data_key (String k)
                     :: encode_record_aux ~data:(Var union) input env !v ))),
        [ set_data_key (Var input_key) ] );
  ]

type jsfun = { module_path : string; function_path : string }

let jsfun ~module_path ~function_path = { module_path; function_path }

type t = jsfun Compile.t

type namespaced_jsfun = {
  name : string;
  namespace : Id.t;
  typescheme : Ty.t Map.String.t;
  function_path : string;
}

type components = {
  import_map : filepath Id.Map.t;  (** Maps JS namespaces to module paths. *)
  components_imports : namespaced_jsfun list;
  components : (string * jsfun C.template C.nodes) list;
}

let make_js_imports C.{ components; _ } =
  Map.String.fold
    (fun name v acc ->
      match v with
      | C.Src nodes -> { acc with components = (name, nodes) :: acc.components }
      | C.Fun (typescheme, { module_path; function_path }) ->
          let namespace, import_map =
            Id.add_unique_namespace (Filepath module_path) acc.import_map
          in
          let components_imports =
            { name; namespace; typescheme; function_path }
            :: acc.components_imports
          in
          { acc with import_map; components_imports })
    components
    { import_map = Id.Map.empty; components_imports = []; components = [] }

let raw_functions =
  {|let escapes = {
  "&": "&amp;",
  '"': "&quot;",
  "'": "&apos;",
  ">": "&gt;",
  "<": "&lt;",
  "/": "&#x2F;",
  "`": "&#x60;",
  "=": "&#x3D;",
};

function acutis_escape(str) {
  let result = "";
  for (let c of str) {
    result += escapes[c] || c;
  }
  return result;
}|}

let pp ty ppf compiled =
  let { import_map; components; components_imports } =
    make_js_imports compiled
  in
  Format.fprintf ppf "@[<v>";
  pp_statement ty ppf (Comment "THIS FILE WAS GENERATED BY ACUTIS.");
  Id.Map.iter
    (fun namespace (Filepath filename) ->
      pp_statement ty ppf @@ Import (namespace, filename))
    import_map;
  List.iter (fun (id, str) -> pp_statement ty ppf @@ Let (id, str)) Err.defs;
  pp_statement ty ppf (Raw raw_functions);
  List.iter
    (fun { name; typescheme; namespace; function_path } ->
      let env = Id.Safe.create () in
      let input = Id.Safe.input env in
      pp_statement ty ppf
      @@ Fun
           ( Id.component name,
             [ input ],
             List.concat
               [
                 [ Let (Id.Data.(to_id initial), New (Object, [])) ];
                 encode_record_aux
                   ~data:(Var Id.Data.(to_id initial))
                   (Var input) env typescheme;
                 [
                   Return
                     (App
                        ( Field (Var namespace, String function_path),
                          [ Var Id.Data.(to_id initial) ] ));
                 ];
               ] ))
    components_imports;
  List.iter
    (fun (name, nodes) ->
      pp_statement ty ppf
      @@ Fun
           ( Id.component name,
             [ Id.Data.(to_id initial) ],
             [ Return (nodes_string Id.Data.initial nodes) ] ))
    components;
  let env = Id.Safe.create () in
  let input = Id.Safe.input env in
  pp_statement ty ppf
    (Export_default
       (Fun
          ( Id.runtime_main,
            [ input ],
            List.concat
              [
                [ Let (Id.Data.(to_id initial), New (Map, [])) ];
                decode_record_aux
                  ~data:(Var Id.Data.(to_id initial))
                  (Var input) env compiled.types;
                [ Return (nodes_string Id.Data.initial compiled.nodes) ];
              ] )));
  Format.fprintf ppf "@]"

let cjs = pp CommonJs
let esm = pp ESModule
