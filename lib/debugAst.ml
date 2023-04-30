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
open Ast

let equal_pair eq_a eq_b (a1, b1) (a2, b2) = eq_a a1 a2 && eq_b b1 b2

module Record = struct
  open Ast.Record

  let equal_tag a b =
    match (a, b) with
    | Tag_int (_, a), Tag_int (_, b) | Tag_bool (_, a), Tag_bool (_, b) -> a = b
    | Tag_string (_, a), Tag_string (_, b) -> a = b
    | _ -> false

  let pp pp ppf = function
    | Untagged x -> F.fprintf ppf "(@[<2>Untagged@ %a@])" (Ast.Dict.pp pp) x
    | Tagged (s, tag, m) ->
        F.fprintf ppf "(@[<2>Tagged (@,%S,@ %a,@ %a@,))@]" s pp_tag tag
          (Ast.Dict.pp pp) m

  let equal eq a b =
    match (a, b) with
    | Untagged a, Untagged b -> Ast.Dict.equal eq a b
    | Tagged (a_s, a_tag, a_m), Tagged (b_s, b_tag, b_m) ->
        a_s = b_s && equal_tag a_tag b_tag && Ast.Dict.equal eq a_m b_m
    | _ -> false
end

module Interface = struct
  open Ast.Interface

  let rec pp_ty ppf = function
    | Named (loc, s) ->
        F.fprintf ppf "(@[<2>Named (@,%a,@ %S@,))@]" Loc.pp loc s
    | Nullable t -> F.fprintf ppf "(@[<2>Nullable@ %a@])" pp_ty t
    | List t -> F.fprintf ppf "(@[<2>List@ %a@])" pp_ty t
    | Dict t -> F.fprintf ppf "(@[<2>Dict@ %a@])" pp_ty t
    | Enum_int (l, row) ->
        F.fprintf ppf "(@[<2>Enum_int (@,%a,@ %a@,))@]"
          (Nonempty.pp F.pp_print_int)
          l Typescheme.Variant.pp_row row
    | Enum_bool l ->
        F.fprintf ppf "(@[<2>Enum_bool@ %a@])" (Nonempty.pp F.pp_print_int) l
    | Enum_string (l, row) ->
        F.fprintf ppf "(@[<2>Enum_string (@,%a,@ %a@,))@]"
          (Nonempty.pp Pp.syntax_string)
          l Typescheme.Variant.pp_row row
    | Record (l, row) ->
        F.fprintf ppf "(@[<2>Record (@,%a,@ %a@,))@]"
          (Nonempty.pp (fun ppf (loc, m) ->
               F.fprintf ppf "(@[%a,@ %a@])" Loc.pp loc (Record.pp pp_ty) m))
          l Typescheme.Variant.pp_row row
    | Tuple l -> F.fprintf ppf "(@[<2>Tuple@ %a@])" (Pp.list pp_ty) l

  let rec equal_ty a b =
    match (a, b) with
    | Named (_, a), Named (_, b) -> a = b
    | Nullable a, Nullable b | List a, List b | Dict a, Dict b -> equal_ty a b
    | Enum_int (a_l, a_row), Enum_int (b_l, b_row) ->
        Nonempty.equal Int.equal a_l b_l
        && Typescheme.Variant.equal_row a_row b_row
    | Enum_bool a, Enum_bool b -> Nonempty.equal Int.equal a b
    | Enum_string (a_l, a_row), Enum_string (b_l, b_row) ->
        Nonempty.equal String.equal a_l b_l
        && Typescheme.Variant.equal_row a_row b_row
    | Record (a_l, a_row), Record (b_l, b_row) ->
        Nonempty.equal (equal_pair Loc.equal (Record.equal equal_ty)) a_l b_l
        && Typescheme.Variant.equal_row a_row b_row
    | Tuple a, Tuple b -> List.equal equal_ty a b
    | _ -> false

  let pp ppf { loc; name; ty } =
    F.fprintf ppf "(@[<2>Type (@,%a,@ %S,@ %a@,))@]" Loc.pp loc name pp_ty ty

  let equal a { loc = _; name; ty } = a.name = name && equal_ty a.ty ty
end

type t = Ast.t

let pp_trim ppf = function
  | No_trim -> F.pp_print_string ppf "No_trim"
  | Trim -> F.pp_print_string ppf "Trim"

let equal_trim a b =
  match (a, b) with No_trim, No_trim | Trim, Trim -> true | _ -> false

let pp_escape ppf = function
  | No_escape -> F.pp_print_string ppf "No_escape"
  | Escape -> F.pp_print_string ppf "Escape"

let equal_escape a b =
  match (a, b) with No_escape, No_escape | Escape, Escape -> true | _ -> false

let pp_echo_format ppf = function
  | Fmt_string -> F.pp_print_string ppf "Fmt_string"
  | Fmt_int -> F.pp_print_string ppf "Fmt_int"
  | Fmt_float -> F.pp_print_string ppf "Fmt_float"
  | Fmt_bool -> F.pp_print_string ppf "Fmt_bool"

let rec pp_echo ppf = function
  | Echo_var (loc, s) ->
      F.fprintf ppf "(@[<2>Echo_var (@,%a,@ %S@,))@]" Loc.pp loc s
  | Echo_string (loc, s) ->
      F.fprintf ppf "(@[<2>Echo_string (@,%a,@ %S@,))@]" Loc.pp loc s
  | Echo_field (x, field) ->
      F.fprintf ppf "(@[<2>Echo_field (@,%a,@ %S@,))@]" pp_echo x field

let equal_echo_format a b =
  match (a, b) with
  | Fmt_string, Fmt_string
  | Fmt_bool, Fmt_bool
  | Fmt_int, Fmt_int
  | Fmt_float, Fmt_float ->
      true
  | _ -> false

let rec equal_echo a b =
  match (a, b) with
  | Echo_var (_, a), Echo_var (_, b) -> a = b
  | Echo_string (_, a), Echo_string (_, b) -> a = b
  | Echo_field (a, a_field), Echo_field (b, b_field) ->
      a_field = b_field && equal_echo a b
  | _ -> false

let rec pp_pat ppf = function
  | Var (loc, s) -> F.fprintf ppf "(@[<2>Var (@,%a,@ %S@,))@]" Loc.pp loc s
  | Bool (loc, i) -> F.fprintf ppf "(@[<2>Bool (@,%a,@ %d@,))@]" Loc.pp loc i
  | Int (loc, i) -> F.fprintf ppf "(@[<2>Int (@,%a,@ %d@,))@]" Loc.pp loc i
  | Float (loc, f) -> F.fprintf ppf "(@[<2>Float (@,%a,@ %F@,))@]" Loc.pp loc f
  | String (loc, s) ->
      F.fprintf ppf "(@[<2>String (@,%a,@ %S@,))@]" Loc.pp loc s
  | Nullable (loc, t) ->
      F.fprintf ppf "(@[<2>Nullable (@,%a,@ %a@,))@]" Loc.pp loc
        (Pp.option pp_pat) t
  | Enum_string (loc, s) ->
      F.fprintf ppf "(@[<2>Enum_string (@,%a,@ %S@,))@]" Loc.pp loc s
  | Enum_int (loc, i) ->
      F.fprintf ppf "(@[<2>Enum_int (@,%a,@ %d@,))@]" Loc.pp loc i
  | List (loc, l, t) ->
      F.fprintf ppf "(@[<2>List (@,%a,@ %a,@ %a@,))@]" Loc.pp loc
        (Pp.list pp_pat) l (Pp.option pp_pat) t
  | Tuple (loc, t) ->
      F.fprintf ppf "(@[<2>Tuple (@,%a,@ %a@,))@]" Loc.pp loc (Pp.list pp_pat) t
  | Record (loc, m) ->
      F.fprintf ppf "(@[<2>Record (@,%a,@ %a@,))@]" Loc.pp loc
        (Record.pp pp_pat) m
  | Dict (loc, m) ->
      F.fprintf ppf "(@[<2>Dict (@,%a,@ %a@,))@]" Loc.pp loc
        (Ast.Dict.pp pp_pat) m
  | Block (loc, x) ->
      F.fprintf ppf "(@[<2>Block (@,%a,@ %a@,))@]" Loc.pp loc pp x
  | Field (loc, x, s) ->
      F.fprintf ppf "(@[<2>Field (@,%a,@ %a,@ %S@,))@]" Loc.pp loc pp_pat x s

and pp_node ppf = function
  | Text (s, triml, trimr) ->
      F.fprintf ppf "(@[<2>Text (@,%S,@ %a,@ %a@,))@]" s pp_trim triml pp_trim
        trimr
  | Echo (l, fmt, ech, esc) ->
      F.fprintf ppf "(@[<2>Echo (@,%a,@ %a,@ %a,@ %a@,))@]"
        (Pp.list (Pp.pair pp_echo_format pp_echo))
        l pp_echo_format fmt pp_echo ech pp_escape esc
  | Match (loc, pats, cases) ->
      F.fprintf ppf "(@[<2>Match (@,%a,@ %a,@ %a@,))@]" Loc.pp loc
        (Nonempty.pp pp_pat) pats (Nonempty.pp pp_case) cases
  | Map_list (loc, pat, cases) ->
      F.fprintf ppf "(@[<2>Map_list (@,%a,@ %a,@ %a@,))@]" Loc.pp loc pp_pat pat
        (Nonempty.pp pp_case) cases
  | Map_dict (loc, pat, cases) ->
      F.fprintf ppf "(@[<2>Map_dict (@,%a,@ %a,@ %a@,))@]" Loc.pp loc pp_pat pat
        (Nonempty.pp pp_case) cases
  | Component (loc, s1, s2, pats) ->
      F.fprintf ppf "(@[<2>Component (@,%a,@ %S,@ %S,@ %a@,))@]" Loc.pp loc s1
        s2 (Dict.pp pp_pat) pats
  | Interface (loc, l) ->
      F.fprintf ppf "(@[<2>Interface (@,%a,@ %a@,))@]" Loc.pp loc
        (Pp.list Interface.pp) l

and pp_case ppf { pats; nodes } =
  F.fprintf ppf "@[<2>{ @[pats =@ %a@];@ @[nodes =@ %a@]@ }@]"
    (Nonempty.pp (Pp.pair Loc.pp (Nonempty.pp pp_pat)))
    pats pp nodes

and pp ppf x = Pp.list pp_node ppf x

let rec equal_pat a b =
  match (a, b) with
  | Var (_, a), Var (_, b)
  | String (_, a), String (_, b)
  | Enum_string (_, a), Enum_string (_, b) ->
      a = b
  | Bool (_, a), Bool (_, b)
  | Int (_, a), Int (_, b)
  | Enum_int (_, a), Enum_int (_, b) ->
      a = b
  | Float (_, a), Float (_, b) -> a = b
  | Nullable (_, a), Nullable (_, b) -> Option.equal equal_pat a b
  | List (_, a_l, a_t), List (_, b_l, b_t) ->
      List.equal equal_pat a_l b_l && Option.equal equal_pat a_t b_t
  | Tuple (_, a), Tuple (_, b) -> List.equal equal_pat a b
  | Record (_, a), Record (_, b) -> Record.equal equal_pat a b
  | Dict (_, a), Dict (_, b) -> Ast.Dict.equal equal_pat a b
  | Block (_, a), Block (_, b) -> equal a b
  | Field (_, a_pat, a_field), Field (_, b_pat, b_field) ->
      a_field = b_field && equal_pat a_pat b_pat
  | _ -> false

and equal_node a b =
  match (a, b) with
  | Text (a, a_triml, a_trimr), Text (b, b_triml, b_trimr) ->
      a = b && equal_trim a_triml b_triml && equal_trim a_trimr b_trimr
  | Echo (a_l, a_fmt, a_ech, a_esc), Echo (b_l, b_fmt, b_ech, b_esc) ->
      List.equal (equal_pair equal_echo_format equal_echo) a_l b_l
      && equal_echo_format a_fmt b_fmt
      && equal_echo a_ech b_ech && equal_escape a_esc b_esc
  | Match (_, a_pats, a_cases), Match (_, b_pats, b_cases) ->
      Nonempty.equal equal_pat a_pats b_pats
      && Nonempty.equal equal_case a_cases b_cases
  | Map_list (_, a_pat, a_cases), Map_list (_, b_pat, b_cases) ->
      equal_pat a_pat b_pat && Nonempty.equal equal_case a_cases b_cases
  | Map_dict (_, a_pat, a_cases), Map_dict (_, b_pat, b_cases) ->
      equal_pat a_pat b_pat && Nonempty.equal equal_case a_cases b_cases
  | Component (_, a_s1, a_s2, a_pats), Component (_, b_s1, b_s2, b_pats) ->
      a_s1 = b_s1 && a_s2 = b_s2 && Dict.equal equal_pat a_pats b_pats
  | Interface (_, a), Interface (_, b) -> List.equal Interface.equal a b
  | _ -> false

and equal_case a { pats; nodes } =
  Nonempty.equal (equal_pair Loc.equal (Nonempty.equal equal_pat)) a.pats pats
  && equal a.nodes nodes

and equal a b = List.equal equal_node a b
