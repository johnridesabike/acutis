(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

type 'a assoc = (Loc.t * string * 'a) list
type 'a assoc_nonempty = (Loc.t * string * 'a) Nonempty.t

let assoc_to_sexp f l =
  Sexp.of_seq (Sexp.triple Loc.to_sexp Sexp.string f) (List.to_seq l)

type tag =
  | Tag_int of Loc.t * int
  | Tag_bool of Loc.t * int
  | Tag_string of Loc.t * string

type 'a value = Tag of tag | Value of 'a
type 'a record = 'a value assoc_nonempty

let value_to_sexp f = function
  | Tag (Tag_int (loc, i)) -> Sexp.make "tag" [ Loc.to_sexp loc; Sexp.int i ]
  | Tag (Tag_bool (loc, i)) -> Sexp.make "tag" [ Loc.to_sexp loc; Sexp.bool i ]
  | Tag (Tag_string (loc, s)) ->
      Sexp.make "tag" [ Loc.to_sexp loc; Sexp.string s ]
  | Value v -> f v

let record_to_sexp f l =
  Nonempty.to_sexp (Sexp.triple Loc.to_sexp Sexp.string (value_to_sexp f)) l

type ty =
  | Ty_named of Loc.t * string
  | Ty_nullable of ty
  | Ty_list of ty
  | Ty_dict of ty
  | Ty_enum_int of int Nonempty.t * Typescheme.row
  | Ty_enum_bool of int Nonempty.t
  | Ty_enum_string of string Nonempty.t * Typescheme.row
  | Ty_record of (Loc.t * ty record) Nonempty.t * Typescheme.row
  | Ty_tuple of ty list

type prop = { loc : Loc.t; name : string; ty : ty }
type interface = prop list

let rec ty_to_sexp = function
  | Ty_named (loc, s) -> Sexp.make "named" [ Loc.to_sexp loc; Sexp.string s ]
  | Ty_nullable t -> Sexp.make "nullable" [ ty_to_sexp t ]
  | Ty_list t -> Sexp.make "list" [ ty_to_sexp t ]
  | Ty_dict t -> Sexp.make "dict" [ ty_to_sexp t ]
  | Ty_enum_int (l, row) ->
      Sexp.make "enum_int"
        [ Typescheme.row_to_sexp row; Nonempty.to_sexp Sexp.int l ]
  | Ty_enum_bool l -> Sexp.make "enum_bool" [ Nonempty.to_sexp Sexp.bool l ]
  | Ty_enum_string (l, row) ->
      Sexp.make "enum_string"
        [ Typescheme.row_to_sexp row; Nonempty.to_sexp Sexp.string l ]
  | Ty_record (l, row) ->
      Sexp.make "record"
        [
          Typescheme.row_to_sexp row;
          Nonempty.to_sexp (Sexp.pair Loc.to_sexp (record_to_sexp ty_to_sexp)) l;
        ]
  | Ty_tuple l -> Sexp.make "tuple" [ Sexp.of_seq ty_to_sexp (List.to_seq l) ]

let prop_to_sexp { loc; name; ty } =
  Sexp.make "prop" [ Loc.to_sexp loc; Sexp.string name; ty_to_sexp ty ]

let interface_to_sexp l = Sexp.of_seq prop_to_sexp (List.to_seq l)

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
  | Record of Loc.t * pat record
  | Dict of Loc.t * pat assoc
  | Block of Loc.t * t
  | Field of Loc.t * pat * string

and node =
  | Text of string * trim * trim
  | Echo of (echo_format * echo) list * echo_format * echo * escape
  | Match of Loc.t * pat Nonempty.t * case Nonempty.t
  | Map_list of Loc.t * pat * case Nonempty.t
  | Map_dict of Loc.t * pat * case Nonempty.t
  | Component of Loc.t * string * string * pat assoc
  | Interface of Loc.t * interface

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
      Sexp.make "record" [ Loc.to_sexp loc; record_to_sexp pat_to_sexp m ]
  | Dict (loc, m) ->
      Sexp.make "dict" [ Loc.to_sexp loc; assoc_to_sexp pat_to_sexp m ]
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
          assoc_to_sexp pat_to_sexp pats;
          Sexp.string s2;
        ]
  | Interface (loc, i) ->
      Sexp.make "interface" [ Loc.to_sexp loc; interface_to_sexp i ]

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

module F = Format

let pp_sep = Pp.sep_comma

let pp_tag ppf = function
  | Tag_bool (_, 0) -> Pp.false_ ppf
  | Tag_bool _ -> Pp.true_ ppf
  | Tag_int (_, i) -> F.pp_print_int ppf i
  | Tag_string (_, s) -> Pp.syntax_string ppf s

let rec pp_pat ppf = function
  | Var (_, s) -> F.pp_print_string ppf s
  | Bool (_, 0) -> Pp.false_ ppf
  | Bool (_, _) -> Pp.true_ ppf
  | Int (_, i) -> F.pp_print_int ppf i
  | Float (_, f) -> F.pp_print_float ppf f
  | String (_, s) -> Pp.syntax_string ppf s
  | Nullable (_, None) -> F.pp_print_string ppf "null"
  | Nullable (_, Some pat) -> F.fprintf ppf "!@[<hv 1>@,%a@]" pp_pat pat
  | Enum_string (_, s) -> F.fprintf ppf "%@%a" Pp.syntax_string s
  | Enum_int (_, i) -> F.fprintf ppf "%@%i" i
  | List (_, [], Some tl) -> pp_pat ppf tl
  | List (_, l, tl) ->
      Pp.surround ~left:'[' ~right:']'
        (fun ppf () ->
          (F.pp_print_list ~pp_sep pp_pat) ppf l;
          (F.pp_print_option (fun ppf pat ->
               F.fprintf ppf "%a...%a" pp_sep () pp_pat pat))
            ppf tl)
        ppf ()
  | Tuple (_, l) ->
      Pp.surround ~left:'(' ~right:')' (F.pp_print_list ~pp_sep pp_pat) ppf l
  | Record (_, l) ->
      Pp.surround ~left:'{' ~right:'}'
        (F.pp_print_list ~pp_sep pp_record_keyvalue)
        ppf (Nonempty.to_list l)
  | Dict (_, l) ->
      Pp.surround ~left:'<' ~right:'>'
        (F.pp_print_list ~pp_sep pp_keyvalue)
        ppf l
  | Block _ -> F.pp_print_string ppf "#%}...{%#"
  | Field (_, pat, field) -> F.fprintf ppf "%a.%s" pp_pat pat field

and pp_keyvalue ppf (_, k, v) =
  F.fprintf ppf "@[<hv 2>%a:@ %a@]" Pp.field k pp_pat v

and pp_record_keyvalue ppf (l, k, v) =
  match v with
  | Tag t -> F.fprintf ppf "@[<hv 2>%@%a:@ %a@]" Pp.field k pp_tag t
  | Value v -> pp_keyvalue ppf (l, k, v)
