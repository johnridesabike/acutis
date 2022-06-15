(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module Pp = struct
  open Format

  let sep_comma ppf () = fprintf ppf ",@ "

  (* Sync with lexer*)
  let id_start_char = function 'a' .. 'z' | '_' -> true | _ -> false

  let id_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false

  let is_id s = id_start_char s.[0] && String.for_all id_char s
  let field ppf k = if is_id k then fprintf ppf "%s" k else fprintf ppf "%S" k
end

module Loc = struct
  type t = Lexing.position * Lexing.position

  let dummy = (Lexing.dummy_pos, Lexing.dummy_pos)
  let pp ppf _ = Format.fprintf ppf "<loc>"
  let equal _ _ = true (* Do not use location for equality in testing. *)
end

module type MAP = sig
  include Map.S

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

let pp_bindings pp_k pp_v ppf (k, v) =
  Format.fprintf ppf "@[%a@ -> @[%a@]@]" pp_k k pp_v v

module MapString = struct
  include Map.Make (String)

  let pp_key ppf k = Format.fprintf ppf "%S" k

  let pp pp_a ppf m =
    Format.fprintf ppf "MapString.[@[@,%a@,@]]"
      (Format.pp_print_seq ~pp_sep:Pp.sep_comma (pp_bindings pp_key pp_a))
      (to_seq m)
end

module MapInt = struct
  include Map.Make (Int)

  let pp_key = Format.pp_print_int

  let pp pp_a ppf m =
    Format.fprintf ppf "MapInt.[@[@,%a@,@]]"
      (Format.pp_print_seq ~pp_sep:Pp.sep_comma (pp_bindings pp_key pp_a))
      (to_seq m)
end

module type SET = sig
  include Set.S

  val pp : Format.formatter -> t -> unit
end

module SetString = struct
  include Set.Make (String)

  let pp_elt ppf e = Format.fprintf ppf "%S" e

  let pp ppf s =
    Format.fprintf ppf "SetString.[@[@,%a@,@]]"
      (Format.pp_print_seq ~pp_sep:Pp.sep_comma pp_elt)
      (to_seq s)
end

module SetInt = struct
  include Set.Make (Int)

  let pp_elt = Format.pp_print_int

  let pp ppf s =
    Format.fprintf ppf "SetString.[@[@,%a@,@]]"
      (Format.pp_print_seq ~pp_sep:Pp.sep_comma pp_elt)
      (to_seq s)
end

module StringExtra = struct
  (* The [ltrim] and [rtrim] functions are vendored from the Containers library.
     https://github.com/c-cube/ocaml-containers/blob/70703b351235b563f060ef494461e678e896da49/src/core/CCString.ml
  *)
  module S = String

  let drop_while f s =
    let i = ref 0 in
    while !i < S.length s && f (S.unsafe_get s !i) do
      incr i
    done;
    if !i > 0 then S.sub s !i (S.length s - !i) else s

  let rdrop_while f s =
    let i = ref (S.length s - 1) in
    while !i >= 0 && f (S.unsafe_get s !i) do
      decr i
    done;
    if !i < S.length s - 1 then S.sub s 0 (!i + 1) else s

  (* notion of whitespace for trim *)
  let is_space = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false

  let ltrim s = drop_while is_space s
  let rtrim s = rdrop_while is_space s
end

module Nonempty = struct
  type 'a t = ( :: ) of 'a * 'a list

  let to_list (hd :: tl) = List.( :: ) (hd, tl)
  let cons a (hd :: tl) = a :: hd :: tl
  let hd (hd :: _) = hd

  let rev (hd :: tl) =
    let rec aux l1 l2 =
      match l1 with [] -> l2 | a :: l -> aux l (cons a l2)
    in
    aux tl [ hd ]

  let map f (hd :: tl) =
    (* Preserve evaluation order. *)
    let hd = f hd in
    hd :: List.map f tl

  let map2 f (hd1 :: tl1) (hd2 :: tl2) =
    (* Preserve evaluation order. *)
    let hd = f hd1 hd2 in
    hd :: List.map2 f tl1 tl2

  let equal f (h1 :: t1) (h2 :: t2) = f h1 h2 && List.equal f t1 t2
  let pp_sep ppf () = Format.fprintf ppf ";@ "

  let pp pp_a ppf l =
    let open Format in
    fprintf ppf "@[<2>Nonempty.[%a@,]@]"
      (pp_print_list ~pp_sep pp_a)
      (to_list l)
end
