(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module F = Format

module Dict = struct
  type 'a t = 'a Map.String.t

  let fail_if_dup_key loc k m =
    if Map.String.mem k m then Error.dup_record_key loc k

  let add loc k v m =
    Map.String.update k
      (function None -> Some v | Some _ -> Error.dup_record_key loc k)
      m

  let empty = Map.String.empty
  let singleton = Map.String.singleton
  let to_map m = m
  let to_sexp = Sexp.map_string
end

module Record = struct
  type tag =
    | Tag_int of Loc.t * int
    | Tag_bool of Loc.t * int
    | Tag_string of Loc.t * string

  let pp_tag ppf = function
    | Tag_int (_, i) -> Format.pp_print_int ppf i
    | Tag_bool (_, 0) -> Format.pp_print_string ppf "false"
    | Tag_bool _ -> Format.pp_print_string ppf "true"
    | Tag_string (_, s) -> Format.fprintf ppf "%S" s

  type 'a t = Untagged of 'a Dict.t | Tagged of string * tag * 'a Dict.t

  let add loc tag m =
    match (tag, m) with
    | `Tag _, Tagged _ -> Error.extra_record_tag loc
    | `Notag (k, v), Tagged (k', v', m) ->
        if k = k' then Error.dup_record_key loc k;
        Tagged (k', v', Dict.add loc k v m)
    | `Tag (k, v), Untagged m ->
        Dict.fail_if_dup_key loc k m;
        Tagged (k, v, m)
    | `Notag (k, v), Untagged m -> Untagged (Dict.add loc k v m)

  let singleton tag =
    match tag with
    | `Tag (k, v) -> Tagged (k, v, Dict.empty)
    | `Notag (k, v) -> Untagged (Dict.singleton k v)

  let tag_to_sexp = function
    | Tag_int (loc, i) -> Sexp.make "tag_int" [ Loc.to_sexp loc; Sexp.int i ]
    | Tag_bool (loc, i) -> Sexp.make "tag_bool" [ Loc.to_sexp loc; Sexp.bool i ]
    | Tag_string (loc, s) ->
        Sexp.make "tag_string" [ Loc.to_sexp loc; Sexp.string s ]

  let to_sexp f = function
    | Untagged m -> Sexp.make "untagged" [ Dict.to_sexp f m ]
    | Tagged (s, tag, m) ->
        Sexp.make "tagged" [ Sexp.string s; tag_to_sexp tag; Dict.to_sexp f m ]
end

module Interface = struct
  type ty =
    | Named of Loc.t * string
    | Nullable of ty
    | List of ty
    | Dict of ty
    | Enum_int of int Nonempty.t * Typescheme.Variant.row
    | Enum_bool of int Nonempty.t
    | Enum_string of string Nonempty.t * Typescheme.Variant.row
    | Record of (Loc.t * ty Record.t) Nonempty.t * Typescheme.Variant.row
    | Tuple of ty list

  type prop = { loc : Loc.t; name : string; ty : ty }
  type t = prop list

  let rec ty_to_sexp = function
    | Named (loc, s) -> Sexp.make "named" [ Loc.to_sexp loc; Sexp.string s ]
    | Nullable t -> Sexp.make "nullable" [ ty_to_sexp t ]
    | List t -> Sexp.make "list" [ ty_to_sexp t ]
    | Dict t -> Sexp.make "dict" [ ty_to_sexp t ]
    | Enum_int (l, row) ->
        Sexp.make "enum_int"
          [ Typescheme.Variant.row_to_sexp row; Nonempty.to_sexp Sexp.int l ]
    | Enum_bool l -> Sexp.make "enum_bool" [ Nonempty.to_sexp Sexp.bool l ]
    | Enum_string (l, row) ->
        Sexp.make "enum_string"
          [ Typescheme.Variant.row_to_sexp row; Nonempty.to_sexp Sexp.string l ]
    | Record (l, row) ->
        Sexp.make "record"
          [
            Typescheme.Variant.row_to_sexp row;
            Nonempty.to_sexp
              (Sexp.pair Loc.to_sexp (Record.to_sexp ty_to_sexp))
              l;
          ]
    | Tuple l -> Sexp.make "tuple" [ Sexp.of_seq ty_to_sexp (List.to_seq l) ]

  let prop_to_sexp { loc; name; ty } =
    Sexp.make "prop" [ Loc.to_sexp loc; Sexp.string name; ty_to_sexp ty ]

  let to_sexp l = Sexp.of_seq prop_to_sexp (List.to_seq l)
end

type trim = No_trim | Trim
type escape = No_escape | Escape
type echo_format = Fmt_string | Fmt_int | Fmt_float | Fmt_bool

type echo =
  | Echo_var of Loc.t * string
  | Echo_string of Loc.t * string
  | Echo_field of echo * string

type pat =
  | Var of Loc.t * string
  | Bool of Loc.t * int
  | Int of Loc.t * int
  | Float of Loc.t * float
  | String of Loc.t * string
  | Nullable of Loc.t * pat option
  | Enum_string of Loc.t * string
  | Enum_int of Loc.t * int
  | List of Loc.t * pat list * pat option
  | Tuple of Loc.t * pat list
  | Record of Loc.t * pat Record.t
  | Dict of Loc.t * pat Dict.t
  | Block of Loc.t * t
  | Field of Loc.t * pat * string

and node =
  | Text of string * trim * trim
  | Echo of (echo_format * echo) list * echo_format * echo * escape
  | Match of Loc.t * pat Nonempty.t * case Nonempty.t
  | Map_list of Loc.t * pat * case Nonempty.t
  | Map_dict of Loc.t * pat * case Nonempty.t
  | Component of Loc.t * string * string * pat Dict.t
  | Interface of Loc.t * Interface.t

and case = { pats : (Loc.t * pat Nonempty.t) Nonempty.t; nodes : t }
and t = node list

let trim_to_sexp = function
  | No_trim -> Sexp.symbol "no_trim"
  | Trim -> Sexp.symbol "trim"

let escape_to_sexp = function
  | No_escape -> Sexp.symbol "no_escape"
  | Escape -> Sexp.symbol "escape"

let echo_format_to_sexp = function
  | Fmt_string -> Sexp.symbol "fmt_string"
  | Fmt_int -> Sexp.symbol "fmt_int"
  | Fmt_float -> Sexp.symbol "fmt_float"
  | Fmt_bool -> Sexp.symbol "fmt_bool"

let rec echo_to_sexp = function
  | Echo_var (loc, s) -> Sexp.make "echo_var" [ Loc.to_sexp loc; Sexp.string s ]
  | Echo_string (loc, s) ->
      Sexp.make "echo_string" [ Loc.to_sexp loc; Sexp.string s ]
  | Echo_field (x, field) ->
      Sexp.make "echo_field" [ Sexp.string field; echo_to_sexp x ]

let rec pat_to_sexp = function
  | Var (loc, s) -> Sexp.make "var" [ Loc.to_sexp loc; Sexp.string s ]
  | Bool (loc, i) -> Sexp.make "bool" [ Loc.to_sexp loc; Sexp.bool i ]
  | Int (loc, i) -> Sexp.make "int" [ Loc.to_sexp loc; Sexp.int i ]
  | Float (loc, f) -> Sexp.make "float" [ Loc.to_sexp loc; Sexp.float f ]
  | String (loc, s) -> Sexp.make "string" [ Loc.to_sexp loc; Sexp.string s ]
  | Nullable (loc, t) ->
      Sexp.make "nullable"
        [
          Loc.to_sexp loc;
          (match t with None -> Sexp.symbol "null" | Some t -> pat_to_sexp t);
        ]
  | Enum_string (loc, s) ->
      Sexp.make "enum_string" [ Loc.to_sexp loc; Sexp.string s ]
  | Enum_int (loc, i) -> Sexp.make "enum_int" [ Loc.to_sexp loc; Sexp.int i ]
  | List (loc, l, None) ->
      Sexp.make "list"
        [ Loc.to_sexp loc; Sexp.of_seq pat_to_sexp (List.to_seq l) ]
  | List (loc, l, Some t) ->
      Sexp.make "list"
        [
          Loc.to_sexp loc;
          Sexp.of_seq pat_to_sexp (List.to_seq l);
          pat_to_sexp t;
        ]
  | Tuple (loc, l) ->
      Sexp.make "tuple"
        [ Loc.to_sexp loc; Sexp.of_seq pat_to_sexp (List.to_seq l) ]
  | Record (loc, m) ->
      Sexp.make "record" [ Loc.to_sexp loc; Record.to_sexp pat_to_sexp m ]
  | Dict (loc, m) ->
      Sexp.make "dict" [ Loc.to_sexp loc; Dict.to_sexp pat_to_sexp m ]
  | Block (loc, x) -> Sexp.make "block" [ Loc.to_sexp loc; to_sexp x ]
  | Field (loc, x, s) ->
      Sexp.make "field" [ Loc.to_sexp loc; pat_to_sexp x; Sexp.string s ]

and node_to_sexp = function
  | Text (s, triml, trimr) ->
      Sexp.make "text" [ trim_to_sexp triml; Sexp.string s; trim_to_sexp trimr ]
  | Echo (l, fmt, ech, esc) ->
      Sexp.make "echo"
        [
          Sexp.of_seq
            (Sexp.pair echo_format_to_sexp echo_to_sexp)
            (List.to_seq l);
          echo_format_to_sexp fmt;
          echo_to_sexp ech;
          escape_to_sexp esc;
        ]
  | Match (loc, pats, cases) ->
      Sexp.make "match"
        [
          Loc.to_sexp loc;
          Nonempty.to_sexp pat_to_sexp pats;
          Nonempty.to_sexp case_to_sexp cases;
        ]
  | Map_list (loc, pat, cases) ->
      Sexp.make "map"
        [
          Loc.to_sexp loc; pat_to_sexp pat; Nonempty.to_sexp case_to_sexp cases;
        ]
  | Map_dict (loc, pat, cases) ->
      Sexp.make "map_dict"
        [
          Loc.to_sexp loc; pat_to_sexp pat; Nonempty.to_sexp case_to_sexp cases;
        ]
  | Component (loc, s1, s2, pats) ->
      Sexp.make "component"
        [
          Loc.to_sexp loc;
          Sexp.string s1;
          Dict.to_sexp pat_to_sexp pats;
          Sexp.string s2;
        ]
  | Interface (loc, i) ->
      Sexp.make "interface" [ Loc.to_sexp loc; Interface.to_sexp i ]

and case_to_sexp { pats; nodes } =
  Sexp.make "case"
    [
      Sexp.make "pats"
        [
          Nonempty.to_sexp
            (Sexp.pair Loc.to_sexp (Nonempty.to_sexp pat_to_sexp))
            pats;
        ];
      Sexp.make "nodes" [ to_sexp nodes ];
    ]

and to_sexp l = Sexp.of_seq node_to_sexp (List.to_seq l)
