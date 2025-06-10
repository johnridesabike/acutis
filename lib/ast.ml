(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

type loc = Lexing.position * Lexing.position

let loc_dummy = (Lexing.dummy_pos, Lexing.dummy_pos)

type 'a assoc = (loc * string * 'a) list
type 'a assoc_nonempty = (loc * string * 'a) Nonempty.t

type tag =
  | Tag_int of loc * int
  | Tag_bool of loc * int
  | Tag_string of loc * string

type 'a value = Tag of tag | Value of 'a
type 'a record = 'a value assoc_nonempty
type row = loc * [ `Closed | `Open ]

type ty =
  | Ty_named of loc * string
  | Ty_nullable of ty
  | Ty_list of ty
  | Ty_dict of ty
  | Ty_enum_int of (loc * int) Nonempty.t * row
  | Ty_enum_bool of (loc * int) Nonempty.t
  | Ty_enum_string of (loc * string) Nonempty.t * row
  | Ty_record of (loc * ty record) Nonempty.t * row
  | Ty_tuple of ty list

type prop = { loc : loc; name : string; ty : ty }
type interface = prop list
type trim = No_trim | Trim
type escape = No_escape | Escape
type echo_format = Fmt_string | Fmt_int | Fmt_float | Fmt_bool

type echo =
  | Echo_var of loc * string
  | Echo_string of loc * string
  | Echo_field of echo * string

type pat =
  | Var of loc * string
  | Bool of loc * int
  | Int of loc * int
  | Float of loc * float
  | String of loc * string
  | Nullable of loc * pat option
  | Enum_string of loc * string
  | Enum_int of loc * int
  | List of loc * pat list * pat option
  | Tuple of loc * pat list
  | Record of loc * pat record
  | Dict of loc * pat assoc
  | Block of loc * t
  | Field of loc * pat * string

and node =
  | Text of string * trim * trim
  | Echo of (echo_format * echo) list * echo_format * echo * escape
  | Match of loc * pat Nonempty.t * case Nonempty.t
  | Map_list of loc * pat * case Nonempty.t
  | Map_dict of loc * pat * case Nonempty.t
  | Component of loc * string * string * pat assoc
  | Interface of loc * interface
  | Comment of string

and case = { pats : (loc * pat Nonempty.t) Nonempty.t; nodes : t }
and t = node list

let dummy_var = Var (loc_dummy, "_")

module Ty_repr = struct
  open Pp.Ty_repr
  module Nonempty = Linear (Nonempty)

  (** Don't show locations. In development, this can be modified to show
      information as needed. *)
  let loc = Pp.Ty_repr.ignore

  let assoc_triple f = tuple3 loc string f
  let assoc f l = list (assoc_triple f) l
  let assoc_nonempty f l = Nonempty.t (assoc_triple f) l

  let tag = function
    | Tag_int (loc', i) -> variant "Tag_int" (args (loc loc') * int i)
    | Tag_bool (loc', i) -> variant "Tag_bool" (args (loc loc') * int i)
    | Tag_string (loc', s) -> variant "Tag_string" (args (loc loc') * string s)

  let value f = function
    | Tag t -> variant "Tag" (args (tag t))
    | Value v -> variant "Value" (args (f v))

  let record f l = assoc_nonempty (value f) l
  let trim = function No_trim -> variant0 "No_trim" | Trim -> variant0 "Trim"

  let escape = function
    | No_escape -> variant0 "No_escape"
    | Escape -> variant0 "Escape"

  let row =
    tuple2 loc (function
      | `Closed -> polyvar0 "Closed"
      | `Open -> polyvar0 "Open")

  let rec ty = function
    | Ty_named (loc', s) -> variant "Ty_named" (args (loc loc') * string s)
    | Ty_nullable t -> variant "Ty_nullable" (args (ty t))
    | Ty_list t -> variant "Ty_list" (args (ty t))
    | Ty_dict t -> variant "Ty_dict" (args (ty t))
    | Ty_enum_int (l, r) ->
        variant "Ty_enum_int" (args (Nonempty.t (tuple2 loc int) l) * row r)
    | Ty_enum_bool l ->
        variant "Ty_enum_bool" (args (Nonempty.t (tuple2 loc int) l))
    | Ty_enum_string (l, r) ->
        variant "Ty_enum_string"
          (args (Nonempty.t (tuple2 loc string) l) * row r)
    | Ty_record (l, r) ->
        variant "Ty_record"
          (args (Nonempty.t (tuple2 loc (record ty)) l) * row r)
    | Ty_tuple l -> variant "Ty_tuple" (args (list ty l))

  let prop { loc = loc'; name; ty = ty' } =
    Pp.Ty_repr.record
      (fields "loc" (loc loc')
      |> field "name" (string name)
      |> field "ty" (ty ty'))

  let interface l = list prop l

  let echo_format = function
    | Fmt_string -> variant0 "Fmt_string"
    | Fmt_int -> variant0 "Fmt_int"
    | Fmt_float -> variant0 "Fmt_float"
    | Fmt_bool -> variant0 "Fmt_bool"

  let rec echo = function
    | Echo_var (loc', s) -> variant "Echo_var" (args (loc loc') * string s)
    | Echo_string (loc', s) -> variant "Echo_string" (args (loc loc') * string s)
    | Echo_field (x, field) ->
        variant "Echo_field" (args (echo x) * string field)

  let rec pat = function
    | Var (loc', s) -> variant "Var" (args (loc loc') * string s)
    | Bool (loc', i) -> variant "Bool" (args (loc loc') * int i)
    | Int (loc', i) -> variant "Int" (args (loc loc') * int i)
    | Float (loc', f) -> variant "Float" (args (loc loc') * float f)
    | String (loc', s) -> variant "String" (args (loc loc') * string s)
    | Nullable (loc', t) -> variant "Nullable" (args (loc loc') * option pat t)
    | Enum_string (loc', s) -> variant "Enum_string" (args (loc loc') * string s)
    | Enum_int (loc', i) -> variant "Enum_int" (args (loc loc') * int i)
    | List (loc', l, tl) ->
        variant "List" (args (loc loc') * list pat l * option pat tl)
    | Tuple (loc', l) -> variant "Tuple" (args (loc loc') * list pat l)
    | Record (loc', m) -> variant "Record" (args (loc loc') * record pat m)
    | Dict (loc', m) -> variant "Dict" (args (loc loc') * assoc pat m)
    | Block (loc', x) -> variant "Block" (args (loc loc') * t x)
    | Field (loc', x, s) -> variant "Field" (args (loc loc') * pat x * string s)

  and node = function
    | Text (s, triml, trimr) ->
        variant "Text" (args (trim triml) * string s * trim trimr)
    | Echo (l, fmt, ech, esc) ->
        variant "Echo"
          (args (list (tuple2 echo_format echo) l)
          * echo_format fmt * echo ech * escape esc)
    | Match (loc', pats, cases) ->
        variant "Match"
          (args (loc loc') * Nonempty.t pat pats * Nonempty.t case cases)
    | Map_list (loc', p, cases) ->
        variant "Map" (args (loc loc') * pat p * Nonempty.t case cases)
    | Map_dict (loc', p, cases) ->
        variant "Map_dict" (args (loc loc') * pat p * Nonempty.t case cases)
    | Component (loc', s1, s2, pats) ->
        variant "Component"
          (args (loc loc') * string s1 * string s2 * assoc pat pats)
    | Interface (loc', i) -> variant "Interface" (args (loc loc') * interface i)
    | Comment s -> variant "Comment" (args (string s))

  and case { pats; nodes } =
    Pp.Ty_repr.record
      (fields "pats" (Nonempty.t (tuple2 loc (Nonempty.t pat)) pats)
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
