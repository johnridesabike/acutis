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

let is_id s =
  String.length s <> 0 && id_start_char s.[0] && String.for_all id_char s

let field ppf k = if is_id k then fprintf ppf "%s" k else fprintf ppf "%S" k

let bool ppf = function
  | 0 -> pp_print_string ppf "false"
  | _ -> pp_print_string ppf "true"

let surround ~left ~right f ppf x =
  fprintf ppf "@[<hv 2>%c@;<0 0>%a@;<0 -2>%c@]" left f x right

let equation ~sep pp_k pp_v ppf (k, v) =
  fprintf ppf "@[<hv 2>%a%s@ %a@]" pp_k k sep pp_v v

module TyRepr = struct
  type t =
    | Ignored
    | Atom of (formatter -> unit)
    | Seq of t Seq.t
    | Field of string * t

  type repr = t

  module type REPRABLE = sig
    type t

    val t : t -> repr
  end

  module type REPRABLE1 = sig
    type 'a t

    val t : ('a -> repr) -> 'a t -> repr
  end

  let ignore _ = Ignored
  let unit = Atom (dprintf "()")
  let bool x = Atom (dprintf "%B" x)
  let int x = Atom (dprintf "%i" x)
  let string x = Atom (dprintf "%S" x)
  let float x = Atom (dprintf "%G" x)
  let char x = Atom (dprintf "%C" x)
  let symbol x = Atom (dprintf "%s" x)
  let seq f s = Seq (Seq.map f s)

  module Linear (M : sig
    type 'a t

    val to_seq : 'a t -> 'a Seq.t
  end) =
  struct
    type 'a t = 'a M.t

    let t f l = M.to_seq l |> seq f
  end

  let tuple2 f1 f2 (v1, v2) = Seq (Seq.cons (f1 v1) @@ Seq.return (f2 v2))

  let tuple3 f1 f2 f3 (v1, v2, v3) =
    Seq (Seq.cons (f1 v1) @@ Seq.cons (f2 v2) @@ Seq.return (f3 v3))

  type args = Args of t Seq.t [@@unboxed]

  let args t = Args (Seq.return t)
  let ( * ) (Args args) t = Args (Seq.append args (Seq.return t))
  let variant name (Args args) = Seq (Seq.cons (symbol name) args)
  let variant0 = symbol
  let polyvar_aux name = Atom (dprintf "`%s" name)
  let polyvar name (Args args) = Seq (Seq.cons (polyvar_aux name) args)
  let polyvar0 = polyvar_aux

  type fields = args

  let fields k v = args (Field (k, v))
  let field k v r = r * Field (k, v)
  let record (Args r) = Seq r
  let variantr s r = Seq (Seq.cons (symbol s) @@ Seq.return (record r))
  let abstract s (Args args) = Seq (Seq.cons (symbol s) args)
  let abstract0 = symbol

  let rec pp_aux = function
    | Ignored -> None
    | Atom f -> Some f
    | Field (k, v) -> (
        match pp_aux v with
        | Some v -> Some (dprintf "(@[<hv>%s@ %t@])" k v)
        | None -> None)
    | Seq x ->
        Some
          (dprintf "(@[<hv>%a@])"
             (pp_print_seq ~pp_sep:pp_print_space ( |> ))
             (Seq.filter_map pp_aux x))

  let pp ppf t =
    match pp_aux t with None -> fprintf ppf "(all ignored)" | Some t -> t ppf

  let option f = function
    | None -> variant0 "None"
    | Some x -> variant "Some" (args (f x))

  module List = Linear (List)

  let list = List.t

  module Array = Linear (Array)

  let array = Array.t

  module type ORDERED_REPR = sig
    module Impl : Stdlib.Map.OrderedType
    include REPRABLE with type t = Impl.t
  end

  module String = struct
    module Impl = String

    type t = string

    let t = string
  end

  module Int = struct
    module Impl = Int

    type t = int

    let t = int
  end

  module Set (M : ORDERED_REPR) = struct
    include Set.Make (M.Impl)

    let t x = to_seq x |> seq M.t |> args |> abstract "Set"
  end

  module Map (M : ORDERED_REPR) = struct
    include Map.Make (M.Impl)

    let t f x = to_seq x |> seq (tuple2 M.t f) |> args |> abstract "Map"
  end
end
