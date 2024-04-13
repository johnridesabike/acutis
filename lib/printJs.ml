(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2023 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module F = Format

module State : sig
  type t
  (** This tracks variable names used across JavaScript scopes so let-bindings
      are safe. JavaScript block scope is not equivalent to our native scope. *)

  val make : unit -> t
  val var : string -> t -> F.formatter -> t -> unit
  val add_block : t -> t
end = struct
  type t = (string * int) list ref

  let make () = ref []

  let var v state =
    let i = try List.assoc v !state |> succ with Not_found -> 0 in
    state := (v, i) :: !state;
    fun ppf _ -> F.fprintf ppf "%s$%i" v i

  let add_block state = ref !state
end

type import = { module_path : string; function_path : string }

let import ~module_path ~function_path = { module_path; function_path }

let pp_import ppf { module_path; function_path } =
  Format.fprintf ppf "(@[%S %S@])" module_path function_path

type t = import Compile.t
type js = F.formatter -> State.t -> unit

let trailing_comma =
  F.pp_print_custom_break ~fits:("", 0, "") ~breaks:(",", -2, "")

(** See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String#escape_sequences *)
let pp_string ppf s =
  let l = String.length s in
  let newline =
    if l < 60 then fun () -> F.pp_print_string ppf "\\n"
    else fun () -> F.fprintf ppf "\\n\\\n"
  in
  F.pp_print_char ppf '"';
  for i = 0 to l - 1 do
    let c = s.[i] in
    match c with
    | '\n' -> newline ()
    | '\b' -> F.pp_print_string ppf "\\b"
    | '\t' -> F.pp_print_string ppf "\\t"
    | '\012' -> F.pp_print_string ppf "\\f"
    | '\r' -> F.pp_print_string ppf "\\r"
    | '\\' -> F.pp_print_string ppf "\\\\"
    | '"' -> F.pp_print_string ppf "\\\""
    | c -> F.pp_print_char ppf c
  done;
  F.pp_print_char ppf '"'

(** Define common functions in this submodule so it can be easily included in
    a functor argument. *)
module Javascript = struct
  type 'a exp = js
  type 'a stmt = js
  type 'a mut = js

  let stmt x ppf state = F.fprintf ppf "@[<hv 2>%a;@]" x state
  let ( |: ) a b ppf state = F.fprintf ppf "%a@ %a" a state b state

  let ( @@ ) f e ppf state =
    F.fprintf ppf "@[<hv 2>%a(@,@[<hv 2>%a@]@;<0 -2>)@]" f state e state

  let ( let$ ) (name, x) f ppf state =
    let name = State.var name state in
    (stmt (fun ppf state ->
         F.fprintf ppf "let %a =@ @[<hv 2>%a@]" name state x state)
    |: f name)
      ppf state

  let ( let& ) = ( let$ )

  let set a b =
    stmt (fun ppf state -> F.fprintf ppf "%a =@ @[<hv 2>%a@]" a state b state)

  let ( + ) a b ppf state = F.fprintf ppf "@[%a +@ %a@]" a state b state
  let ( .%() ) a b ppf state = F.fprintf ppf "%a[%a]" a state b state
  let ( .%()<- ) a i b = set a.%(i) b
  let ( .!() ) a b ppf state = F.fprintf ppf "%a.%s" a state b
  let ( .!()<- ) a i b = set a.!(i) b
  let string x ppf _ = pp_string ppf x
  let global x ppf _ = F.pp_print_string ppf x
  let comment ppf str = F.fprintf ppf "/* %s */@," str
  let to_string x = global "String" @@ x

  let obj l ppf state =
    F.fprintf ppf "@[<hv 2>{@,%a@;<0 -2>}@]"
      (F.pp_print_list ~pp_sep:Pp.comma (fun ppf (k, v) ->
           F.fprintf ppf "@[<hv 2>%s:@ %a@]" k v state))
      l

  let lambda f ppf state =
    let state = State.add_block state in
    let arg = State.var "arg" state in
    F.fprintf ppf "(%a) => {@ %a@;<1 -2>}" arg state (f arg) state

  let return x =
    stmt (fun ppf -> F.fprintf ppf "return (@,@[<hv 2>%a@]@;<0 -2>)" x)

  let for_ expr stmts ppf state =
    let state' = State.add_block state in
    let i = State.var "i" state' in
    F.fprintf ppf "@[<hv 2>for (let %a = 0; %a < %a; %a++) {@ %a@;<0 -2>}@]" i
      state' i state' expr state i state' (stmts i) state'

  let switch exp cases default ppf state =
    F.fprintf ppf "@[<v 2>@[<hv 2>switch (%a)@] {@ " exp state;
    let state = State.add_block state in
    F.pp_print_list ~pp_sep:F.pp_print_cut
      (fun ppf (exp, stmts) ->
        F.fprintf ppf "@[<hv 2>case %a:@ %a@ break;@]" exp state stmts state)
      ppf cases;
    F.fprintf ppf "@ @[<hv 2>default:@ %a@]" default state;
    F.fprintf ppf "@;<1 -2>}@]"

  let seq s ppf state =
    F.fprintf ppf "[@,%a%t]"
      (F.pp_print_seq ~pp_sep:Pp.comma (fun ppf x ->
           F.fprintf ppf "@[<hv 2>%a@]" x state))
      s trailing_comma

  let apply_n f args ppf state =
    F.fprintf ppf "@[<hv 2>%a(@,%a@;<0 -2>)@]" f state
      (F.pp_print_list ~pp_sep:Pp.comma (fun ppf x ->
           F.fprintf ppf "@[<hv 2>%a@]" x state))
      args

  let new_ name args ppf state =
    F.fprintf ppf "@[<hv 2>new %s(@,@[<hv 2>%a@]@;<0 -2>)@]" name
      (F.pp_print_list ~pp_sep:Pp.comma (fun ppf x ->
           F.fprintf ppf "@[<hv 2>%a@]" x state))
      args

  let for_of expr stmts ppf state =
    let state' = State.add_block state in
    let x = State.var "x" state' in
    F.fprintf ppf "@[<hv 2>for (let %a of %a) {@ %a@;<0 -2>}@]" x state' expr
      state (stmts x) state'

  let and_ a b ppf state =
    F.fprintf ppf "@[<hv>%a &&@]@ @[<hv>%a@]" a state b state

  let equal a b ppf state = F.fprintf ppf "%a ===@ %a" a state b state
  let typeof expr ppf state = F.fprintf ppf "typeof %a" expr state
end

open Javascript

module type JSMODULE = sig
  val import : import -> (js -> js) -> js
  val export : js -> js
end

module Esm : JSMODULE = struct
  let import { module_path; function_path } f ppf state =
    let import = State.var "import" state in
    (stmt (fun ppf state ->
         F.fprintf ppf "import {%a as %a} from %a" pp_string function_path
           import state pp_string module_path)
    |: f import)
      ppf state

  let export x = stmt (fun ppf -> F.fprintf ppf "export default %a" x)
end

module Cjs : JSMODULE = struct
  let import { module_path; function_path } f =
    let$ import = ("import", global "require" @@ string module_path) in
    f import.%(string function_path)

  let export x = (global "module").!("exports") <- x
end

module RemoveIdsAndUnits (F : Instruct.SEM) :
  Instruct.SEM with type 'a obs = 'a F.obs and type import = F.import = struct
  module Trans = struct
    type 'a from_exp = 'a F.exp

    type 'a exp = { from : 'a from_exp; identity : bool }
    (** The identity property should track which functions are implemented as
        [Fun.id] in the main runtime below. *)

    let fwde x = { from = x; identity = false }
    let bwde x = x.from

    type 'a from_stmt = 'a F.stmt
    type _ stmt = Unit : unit stmt | Unk : 'a F.stmt -> 'a stmt

    let fwds x = Unk x

    let bwds : type a. a stmt -> a from_stmt = function
      | Unit -> F.unit
      | Unk x -> x
  end

  open Trans
  module M = Instruct.MakeTrans (Trans) (F)
  include M

  let ( |: ) : type a. unit stmt -> a stmt -> a stmt =
   fun a b ->
    match (a, b) with
    | Unit, x -> x
    | x, Unit -> x
    | Unk a, Unk b -> Unk F.(a |: b)

  let unit = Unit

  let if_else :
      type a.
      bool exp -> then_:(unit -> a stmt) -> else_:(unit -> a stmt) -> a stmt =
   fun x ~then_ ~else_ ->
    match (then_ (), else_ ()) with
    | Unit, Unit -> Unit
    | Unit, Unk else_ -> fwds (F.if_ (bwde (not x)) ~then_:(fun () -> else_))
    | Unk then_, Unit -> fwds (F.if_ (bwde x) ~then_:(fun () -> then_))
    | Unk then_, Unk else_ ->
        fwds
          (F.if_else (bwde x) ~then_:(fun () -> then_) ~else_:(fun () -> else_))

  let ( let$ ) (name, x) f =
    if x.identity then f x
    else
      fwds
        (F.( let$ ) (name, x.from) (fun x ->
             bwds (f { from = x; identity = true })))

  let lambda f =
    fwde (F.lambda (fun x -> bwds (f { from = x; identity = true })))

  let float_of_int x = { x with from = F.float_of_int x.from }
  let deref x = { from = F.deref x; identity = true }

  module Data = struct
    include M.Data

    let int x = { x with from = F.Data.int x.from }
    let float x = { x with from = F.Data.float x.from }
    let string x = { x with from = F.Data.string x.from }
    let array x = { x with from = F.Data.array x.from }
    let hashtbl x = { x with from = F.Data.hashtbl x.from }
    let unknown x = { x with from = F.Data.unknown x.from }
    let to_int x = { x with from = F.Data.to_int x.from }
    let to_float x = { x with from = F.Data.to_float x.from }
    let to_string x = { x with from = F.Data.to_string x.from }
    let to_array x = { x with from = F.Data.to_array x.from }
    let to_hashtbl x = { x with from = F.Data.to_hashtbl x.from }
  end

  module External = struct
    include M.External

    let some x = { x with from = F.External.some x.from }
    let of_int x = { x with from = F.External.of_int x.from }
    let of_float x = { x with from = F.External.of_float x.from }
    let of_string x = { x with from = F.External.of_string x.from }
    let of_bool x = { x with from = F.External.of_bool x.from }
    let of_array x = { x with from = F.External.of_array x.from }
    let of_untyped x = { x with from = F.External.of_untyped x.from }
  end
end

let buffer_add_string =
  lambda (fun b ->
      return (lambda (fun s -> set b.!("contents") (b.!("contents") + s))))

let buffer_add_buffer =
  lambda (fun b ->
      return
        (lambda (fun s ->
             set b.!("contents") (b.!("contents") + s.!("contents")))))

let buffer_add_escape =
  lambda (fun b ->
      return
        (lambda (fun s ->
             let buffer_add s = set b.!("contents") (b.!("contents") + s) in
             for_ s.!("length") (fun i ->
                 let$ c = ("c", s.%(i)) in
                 switch c
                   [
                     (string "&", buffer_add (string "&amp;"));
                     (string "\"", buffer_add (string "&quot;"));
                     (string "'", buffer_add (string "&apos;"));
                     (string ">", buffer_add (string "&gt;"));
                     (string "<", buffer_add (string "&lt;"));
                     (string "/", buffer_add (string "&#x2F;"));
                     (string "`", buffer_add (string "&#x60;"));
                     (string "=", buffer_add (string "&#x3D;"));
                   ]
                   (buffer_add c)))))

let pp (module Jsmod : JSMODULE) ppf c =
  let state = State.make () in
  let instructions =
    let$ buffer_add_string = ("buffer_add_string", buffer_add_string) in
    let$ buffer_add_buffer = ("buffer_add_buffer", buffer_add_buffer) in
    let$ buffer_add_escape = ("buffer_add_escape", buffer_add_escape) in
    let module Runtime :
      Instruct.SEM with type 'a obs = js and type import = import = struct
      include Jsmod
      include Javascript

      type 'a obs = 'a stmt

      let observe = Fun.id
      let deref = Fun.id

      let ( := ) a b =
        stmt (fun ppf state ->
            F.fprintf ppf "%a =@ @[<hv 2>%a@]" a state b state)

      let incr a = stmt (fun ppf -> F.fprintf ppf "%a++" a)

      let if_ b ~then_ ppf state =
        let state' = State.add_block state in
        F.fprintf ppf "@[<hv 2>@[<hv 2>if (@,%a@;<0 -2>)@] {@ %a@;<1 -2>}@]" b
          state (then_ ()) state'

      let if_else b ~then_ ~else_ ppf state =
        let state' = State.add_block state in
        F.fprintf ppf "@[<hv 2>@[<hv 2>if (@,%a@;<0 -2>)@] {@ %a" b state
          (then_ ()) state';
        let state' = State.add_block state in
        F.fprintf ppf "@;<1 -2>} else {@ %a@;<1 -2>}@]" (else_ ()) state'

      let while_ cond mut stmts ppf state =
        F.fprintf ppf "@[<hv 2>while (%a) " (cond mut) state;
        let state = State.add_block state in
        F.fprintf ppf "{@ %a@;<1 -2>}@]" (stmts ()) state

      let unit _ _ = ()
      let not x ppf state = F.fprintf ppf "@[<hv 2>!(%a)@]" x state
      let int x ppf _ = F.pp_print_int ppf x
      let float x ppf _ = F.pp_print_float ppf x
      let bool x ppf _ = F.pp_print_bool ppf x
      let equal_int = equal
      let equal_string = equal
      let string_of_int = to_string
      let float_of_int = Fun.id
      let string_of_float = to_string
      let string_of_bool = to_string
      let array a = seq (Array.to_seq a)

      let array_make i x =
        apply_n
          (global "Array").!("from")
          [ obj [ ("length", i) ]; lambda (fun _ -> return x) ]

      type 'a hashtbl

      let pair (a, b) = seq Seq.(fun () -> Cons (a, fun () -> Cons (b, empty)))
      let hashtbl s = new_ "Map" [ seq (Seq.map pair s) ]
      let hashtbl_create () = new_ "Map" [ unit ]
      let ( .%{} ) x k = x.!("get") @@ k
      let ( .%{}<- ) x k v = stmt (apply_n x.!("set") [ k; v ])
      let hashtbl_mem x k = x.!("has") @@ k
      let hashtbl_copy x = new_ "Map" [ x ]

      let hashtbl_iter x f =
        for_of x (fun entry -> f entry.%(int 0) entry.%(int 1))

      type buffer

      let buffer_create () = obj [ ("contents", string "") ]
      let buffer_add_escape b s = stmt ((buffer_add_escape @@ b) @@ s)
      let buffer_add_string b s = stmt ((buffer_add_string @@ b) @@ s)
      let buffer_add_buffer b s = stmt ((buffer_add_buffer @@ b) @@ s)
      let buffer_contents b = b.!("contents")
      let buffer_clear b = set b.!("contents") (string "")
      let buffer_length b = b.!("contents").!("length")

      type 'a promise

      let promise x = (global "Promise").!("resolve") @@ x
      let bind p f = p.!("then") @@ f
      let error s = (global "Promise").!("reject") @@ new_ "Error" [ s ]

      type external_data
      type nonrec import = import

      module Data = struct
        type t

        let int = Fun.id
        let float = Fun.id
        let string = Fun.id
        let array = Fun.id
        let hashtbl = Fun.id
        let unknown = Fun.id
        let to_int = Fun.id
        let to_float = Fun.id
        let to_string = Fun.id
        let to_array = Fun.id
        let to_hashtbl = Fun.id
        let equal = equal
      end

      module External = struct
        module Linear = struct
          type 'a t = js

          let length a = a.!("length")
          let iteri a f = for_ (length a) (fun i -> f i a.%(i))
        end

        module Assoc = struct
          type 'a t = js

          let find x k = x.%(k)
          let mem x k = apply_n (global "Object").!("hasOwn") [ x; k ]

          let iter x f =
            for_of
              ((global "Object").!("keys") @@ x)
              (fun key -> f key x.%(key))
        end

        type t = external_data

        let null = global "null"
        let some = Fun.id
        let of_int = Fun.id
        let of_float = Fun.id
        let of_string = Fun.id
        let of_bool = Fun.id
        let of_array = Fun.id
        let of_hashtbl x = (global "Object").!("fromEntries") @@ x
        let of_untyped = Fun.id

        type _ classify =
          | Int : int classify
          | String : string classify
          | Float : float classify
          | Bool : bool classify
          | Not_null : t classify
          | Linear : t Linear.t classify
          | Assoc : t Assoc.t classify

        let classify (type a) (c : a classify) x ~ok ~error =
          let cond =
            match c with
            | Int -> (global "Number").!("isInteger") @@ x
            | String -> equal (typeof x) (string "string")
            | Float -> equal (typeof x) (string "number")
            | Bool -> equal (typeof x) (string "boolean")
            | Not_null ->
                and_ (not (equal x null)) (not (equal x (global "undefined")))
            | Linear -> (global "Array").!("isArray") @@ x
            | Assoc ->
                and_ (equal (typeof x) (string "object")) (not (equal x null))
          in
          if_else cond ~then_:(fun () -> ok x) ~else_:error

        let show = to_string
      end
    end in
    let module I = Instruct.Make (RemoveIdsAndUnits (Runtime)) in
    I.eval c
  in
  F.fprintf ppf "@[<v>";
  comment ppf "THIS FILE WAS GENERATED BY ACUTIS.";
  instructions ppf state;
  F.fprintf ppf "@]"

let esm = pp (module Esm)
let cjs = pp (module Cjs)
