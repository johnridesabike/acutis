(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open Format

let comma ppf () = fprintf ppf ",@ "
let syntax_string ppf k = fprintf ppf "%S" k
let at pp ppf x = fprintf ppf "%@%a" pp x
let ellipsis ppf () = pp_print_string ppf "..."

(* Sync with lexer*)
let id_start_char = function 'a' .. 'z' | '_' -> true | _ -> false

let id_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_id s = id_start_char s.[0] && String.for_all id_char s
let field ppf k = if is_id k then fprintf ppf "%s" k else fprintf ppf "%S" k

let bool ppf = function
  | 0 -> pp_print_string ppf "false"
  | _ -> pp_print_string ppf "true"

let surround ~left ~right f ppf x =
  fprintf ppf "@[<hv 2>%c@;<0 0>%a@;<0 -2>%c@]" left f x right

let equation ~sep pp_k pp_v ppf (k, v) =
  fprintf ppf "@[<hv 2>%a%s@ %a@]" pp_k k sep pp_v v
