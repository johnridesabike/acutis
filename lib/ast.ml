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

type tag =
  | Tag_int of Loc.t * int
  | Tag_bool of Loc.t * int
  | Tag_string of Loc.t * string

type 'a value = Tag of tag | Value of 'a
type 'a record = 'a value assoc_nonempty
type row = Loc.t * [ `Closed | `Open ]

type ty =
  | Ty_named of Loc.t * string
  | Ty_nullable of ty
  | Ty_list of ty
  | Ty_dict of ty
  | Ty_enum_int of int Nonempty.t * row
  | Ty_enum_bool of int Nonempty.t
  | Ty_enum_string of string Nonempty.t * row
  | Ty_record of (Loc.t * ty record) Nonempty.t * row
  | Ty_tuple of ty list

type prop = { loc : Loc.t; name : string; ty : ty }
type interface = prop list
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
  | Comment of string

and case = { pats : (Loc.t * pat Nonempty.t) Nonempty.t; nodes : t }
and t = node list

let dummy_var = Var (Loc.dummy, "_")

module Ty_repr = struct
  open Pp.Ty_repr
  module Loc = Loc.Ty_repr
  module Nonempty = Linear (Nonempty)

  let assoc_triple f = tuple3 Loc.t string f
  let assoc f l = list (assoc_triple f) l
  let assoc_nonempty f l = Nonempty.t (assoc_triple f) l

  let tag = function
    | Tag_int (l, i) -> variant "Tag_int" (args (Loc.t l) * int i)
    | Tag_bool (l, i) -> variant "Tag_bool" (args (Loc.t l) * int i)
    | Tag_string (l, s) -> variant "Tag_string" (args (Loc.t l) * string s)

  let value f = function
    | Tag t -> variant "Tag" (args (tag t))
    | Value v -> variant "Value" (args (f v))

  let record f l = assoc_nonempty (value f) l
  let trim = function No_trim -> variant0 "No_trim" | Trim -> variant0 "Trim"

  let escape = function
    | No_escape -> variant0 "No_escape"
    | Escape -> variant0 "Escape"

  let row =
    tuple2 Loc.t (function
      | `Closed -> polyvar0 "Closed"
      | `Open -> polyvar0 "Open")

  let rec ty = function
    | Ty_named (loc, s) -> variant "Ty_named" (args (Loc.t loc) * string s)
    | Ty_nullable t -> variant "Ty_nullable" (args (ty t))
    | Ty_list t -> variant "Ty_list" (args (ty t))
    | Ty_dict t -> variant "Ty_dict" (args (ty t))
    | Ty_enum_int (l, r) ->
        variant "Ty_enum_int" (args (Nonempty.t int l) * row r)
    | Ty_enum_bool l -> variant "Ty_enum_bool" (args (Nonempty.t int l))
    | Ty_enum_string (l, r) ->
        variant "Ty_enum_string" (args (Nonempty.t string l) * row r)
    | Ty_record (l, r) ->
        variant "Ty_record"
          (args (Nonempty.t (tuple2 Loc.t (record ty)) l) * row r)
    | Ty_tuple l -> variant "Ty_tuple" (args (list ty l))

  let prop { loc; name; ty = ty' } =
    Pp.Ty_repr.record
      (fields "loc" (Loc.t loc)
      |> field "name" (string name)
      |> field "ty" (ty ty'))

  let interface l = list prop l

  let echo_format = function
    | Fmt_string -> variant0 "Fmt_string"
    | Fmt_int -> variant0 "Fmt_int"
    | Fmt_float -> variant0 "Fmt_float"
    | Fmt_bool -> variant0 "Fmt_bool"

  let rec echo = function
    | Echo_var (loc, s) -> variant "Echo_var" (args (Loc.t loc) * string s)
    | Echo_string (loc, s) -> variant "Echo_string" (args (Loc.t loc) * string s)
    | Echo_field (x, field) ->
        variant "Echo_field" (args (echo x) * string field)

  let rec pat = function
    | Var (loc, s) -> variant "Var" (args (Loc.t loc) * string s)
    | Bool (loc, i) -> variant "Bool" (args (Loc.t loc) * int i)
    | Int (loc, i) -> variant "Int" (args (Loc.t loc) * int i)
    | Float (loc, f) -> variant "Float" (args (Loc.t loc) * float f)
    | String (loc, s) -> variant "String" (args (Loc.t loc) * string s)
    | Nullable (loc, t) -> variant "Nullable" (args (Loc.t loc) * option pat t)
    | Enum_string (loc, s) -> variant "Enum_string" (args (Loc.t loc) * string s)
    | Enum_int (loc, i) -> variant "Enum_int" (args (Loc.t loc) * int i)
    | List (loc, l, tl) ->
        variant "List" (args (Loc.t loc) * list pat l * option pat tl)
    | Tuple (loc, l) -> variant "Tuple" (args (Loc.t loc) * list pat l)
    | Record (loc, m) -> variant "Record" (args (Loc.t loc) * record pat m)
    | Dict (loc, m) -> variant "Dict" (args (Loc.t loc) * assoc pat m)
    | Block (loc, x) -> variant "Block" (args (Loc.t loc) * t x)
    | Field (loc, x, s) -> variant "Field" (args (Loc.t loc) * pat x * string s)

  and node = function
    | Text (s, triml, trimr) ->
        variant "Text" (args (trim triml) * string s * trim trimr)
    | Echo (l, fmt, ech, esc) ->
        variant "Echo"
          (args (list (tuple2 echo_format echo) l)
          * echo_format fmt * echo ech * escape esc)
    | Match (loc, pats, cases) ->
        variant "Match"
          (args (Loc.t loc) * Nonempty.t pat pats * Nonempty.t case cases)
    | Map_list (loc, p, cases) ->
        variant "Map" (args (Loc.t loc) * pat p * Nonempty.t case cases)
    | Map_dict (loc, p, cases) ->
        variant "Map_dict" (args (Loc.t loc) * pat p * Nonempty.t case cases)
    | Component (loc, s1, s2, pats) ->
        variant "Component"
          (args (Loc.t loc) * string s1 * string s2 * assoc pat pats)
    | Interface (loc, i) -> variant "Interface" (args (Loc.t loc) * interface i)
    | Comment s -> variant "Comment" (args (string s))

  and case { pats; nodes } =
    Pp.Ty_repr.record
      (fields "pats" (Nonempty.t (tuple2 Loc.t (Nonempty.t pat)) pats)
      |> field "nodes" (t nodes))

  and t l = list node l
end

module F = Format

let pp_sep = Pp.comma

let pp_tag ppf = function
  | Tag_bool (_, i) -> Pp.bool ppf i
  | Tag_int (_, i) -> F.pp_print_int ppf i
  | Tag_string (_, s) -> Pp.syntax_string ppf s

let rec pp_pat ppf = function
  | Var (_, s) -> F.pp_print_string ppf s
  | Bool (_, i) -> Pp.bool ppf i
  | Int (_, i) -> F.pp_print_int ppf i
  | Float (_, f) -> F.pp_print_float ppf f
  | String (_, s) -> Pp.syntax_string ppf s
  | Nullable (_, None) -> F.pp_print_string ppf "null"
  | Nullable (_, Some pat) -> F.fprintf ppf "!@[<hv 1>@,%a@]" pp_pat pat
  | Enum_string (_, s) -> Pp.at Pp.syntax_string ppf s
  | Enum_int (_, i) -> Pp.at F.pp_print_int ppf i
  | List (_, [], Some tl) -> pp_pat ppf tl
  | List (_, l, None) ->
      Pp.surround '[' ']' (F.pp_print_list ~pp_sep pp_pat) ppf l
  | List (_, l, Some tl) ->
      Pp.surround '[' ']'
        (fun ppf () ->
          F.pp_print_list ~pp_sep pp_pat ppf l;
          pp_sep ppf ();
          Pp.ellipsis ppf ();
          pp_pat ppf tl)
        ppf ()
  | Tuple (_, l) -> Pp.surround '(' ')' (F.pp_print_list ~pp_sep pp_pat) ppf l
  | Record (_, l) ->
      Pp.surround '{' '}'
        (F.pp_print_seq ~pp_sep pp_record_keyvalue)
        ppf (Nonempty.to_seq l)
  | Dict (_, l) ->
      Pp.surround '<' '>'
        (F.pp_print_list ~pp_sep (Pp.equation Pp.field ":" pp_pat))
        ppf
        (List.map (fun (_, k, v) -> (k, v)) l)
  | Block _ -> F.pp_print_string ppf "#%}...{%#"
  | Field (_, pat, field) -> F.fprintf ppf "%a.%s" pp_pat pat field

and pp_record_keyvalue ppf (_, k, v) =
  match v with
  | Tag t -> Pp.equation (Pp.at Pp.field) ":" pp_tag ppf (k, t)
  | Value v -> Pp.equation Pp.field ":" pp_pat ppf (k, v)
