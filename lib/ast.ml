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
    | Tag_int (_, i) -> Sexp.int i
    | Tag_bool (_, i) -> Sexp.bool i
    | Tag_string (_, s) -> Sexp.string s

  let to_sexp f = function
    | Untagged m -> Dict.to_sexp f m
    | Tagged (s, tag, m) ->
        Sexp.list
          [
            Sexp.symbol "tagged";
            Sexp.list [ Sexp.string s; tag_to_sexp tag ];
            Dict.to_sexp f m;
          ]
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

  type t = { loc : Loc.t; name : string; ty : ty }

  let rec ty_to_sexp = function
    | Named (_, s) -> Sexp.list [ Sexp.symbol "named"; Sexp.string s ]
    | Nullable t -> Sexp.list [ Sexp.symbol "nullable"; ty_to_sexp t ]
    | List t -> Sexp.list [ Sexp.symbol "list"; ty_to_sexp t ]
    | Dict t -> Sexp.list [ Sexp.symbol "dict"; ty_to_sexp t ]
    | Enum_int (l, row) ->
        Sexp.list
          [
            Sexp.symbol "enum_int";
            Typescheme.Variant.row_to_sexp row;
            Nonempty.to_sexp Sexp.int l;
          ]
    | Enum_bool l ->
        Sexp.list [ Sexp.symbol "enum_bool"; Nonempty.to_sexp Sexp.bool l ]
    | Enum_string (l, row) ->
        Sexp.list
          [
            Sexp.symbol "enum_string";
            Typescheme.Variant.row_to_sexp row;
            Nonempty.to_sexp Sexp.string l;
          ]
    | Record (l, row) ->
        Sexp.list
          [
            Sexp.symbol "record";
            Typescheme.Variant.row_to_sexp row;
            Nonempty.to_sexp (fun (_, r) -> Record.to_sexp ty_to_sexp r) l;
          ]
    | Tuple l -> Sexp.seq (List.to_seq l |> Seq.map ty_to_sexp)

  let to_sexp { loc = _; name; ty } =
    Sexp.list [ Sexp.string name; ty_to_sexp ty ]
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
  | Interface of Loc.t * Interface.t list

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
  | Echo_var (_, s) -> Sexp.list [ Sexp.symbol "echo_var"; Sexp.string s ]
  | Echo_string (_, s) -> Sexp.list [ Sexp.symbol "echo_string"; Sexp.string s ]
  | Echo_field (x, field) ->
      Sexp.list [ Sexp.symbol "echo_field"; Sexp.string field; echo_to_sexp x ]

let rec pat_to_sexp = function
  | Var (_, s) -> Sexp.list [ Sexp.symbol "var"; Sexp.string s ]
  | Bool (_, i) -> Sexp.bool i
  | Int (_, i) -> Sexp.int i
  | Float (_, f) -> Sexp.float f
  | String (_, s) -> Sexp.string s
  | Nullable (_, None) -> Sexp.symbol "null"
  | Nullable (_, Some t) -> Sexp.list [ Sexp.symbol "nullable"; pat_to_sexp t ]
  | Enum_string (_, s) -> Sexp.list [ Sexp.symbol "enum_string"; Sexp.string s ]
  | Enum_int (_, i) -> Sexp.list [ Sexp.symbol "enum_int"; Sexp.int i ]
  | List (_, l, None) ->
      Sexp.list [ Sexp.symbol "list"; Sexp.list (List.map pat_to_sexp l) ]
  | List (_, l, Some t) ->
      Sexp.list
        [
          Sexp.symbol "list"; Sexp.list (List.map pat_to_sexp l); pat_to_sexp t;
        ]
  | Tuple (_, l) -> Sexp.list (List.map pat_to_sexp l)
  | Record (_, m) ->
      Sexp.list [ Sexp.symbol "record"; Record.to_sexp pat_to_sexp m ]
  | Dict (_, m) -> Sexp.list [ Sexp.symbol "dict"; Dict.to_sexp pat_to_sexp m ]
  | Block (_, x) -> Sexp.list [ Sexp.symbol "block"; to_sexp x ]
  | Field (_, x, s) ->
      Sexp.list [ Sexp.symbol "field"; pat_to_sexp x; Sexp.string s ]

and node_to_sexp = function
  | Text (s, triml, trimr) ->
      Sexp.list
        [
          Sexp.symbol "text";
          trim_to_sexp triml;
          Sexp.string s;
          trim_to_sexp trimr;
        ]
  | Echo (_, fmt, ech, esc) ->
      Sexp.list
        [
          Sexp.symbol "echo";
          echo_format_to_sexp fmt;
          echo_to_sexp ech;
          escape_to_sexp esc;
        ]
  | Match (_, pats, cases) ->
      Sexp.list
        [
          Sexp.symbol "match";
          Nonempty.to_sexp pat_to_sexp pats;
          Nonempty.to_sexp case_to_sexp cases;
        ]
  | Map_list (_, pat, cases) ->
      Sexp.list
        [
          Sexp.symbol "map";
          pat_to_sexp pat;
          Nonempty.to_sexp case_to_sexp cases;
        ]
  | Map_dict (_, pat, cases) ->
      Sexp.list
        [
          Sexp.symbol "map_dict";
          pat_to_sexp pat;
          Nonempty.to_sexp case_to_sexp cases;
        ]
  | Component (_, s1, s2, pats) ->
      Sexp.list
        [
          Sexp.symbol "component";
          Sexp.string s1;
          Dict.to_sexp pat_to_sexp pats;
          Sexp.string s2;
        ]
  | Interface (_, l) ->
      Sexp.list
        [ Sexp.symbol "interface"; Sexp.list (List.map Interface.to_sexp l) ]

and case_to_sexp { pats; nodes } =
  Sexp.list
    [
      Sexp.list
        [
          Sexp.symbol "pats";
          Nonempty.to_sexp
            (fun (_, pat) -> Nonempty.to_sexp pat_to_sexp pat)
            pats;
        ];
      Sexp.list [ Sexp.symbol "nodes"; to_sexp nodes ];
    ]

and to_sexp x = Sexp.list (List.map node_to_sexp x)
