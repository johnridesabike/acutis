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

let sep_comma ppf () = fprintf ppf ",@ "
let syntax_string ppf k = fprintf ppf "%S" k

(* Sync with lexer*)
let id_start_char = function 'a' .. 'z' | '_' -> true | _ -> false

let id_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_id s = id_start_char s.[0] && String.for_all id_char s
let field ppf k = if is_id k then fprintf ppf "%s" k else fprintf ppf "%S" k
