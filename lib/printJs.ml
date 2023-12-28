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

module State = struct
  (** This tracks variable names used across JavaScript scopes so let-bindings
      are safe. JavaScript block scope is not equivalent to our native scope. *)

  type t = (string * int) list ref

  let make () = ref []

  let var v state =
    let i = try List.assoc v !state |> succ with Not_found -> 0 in
    state := (v, i) :: !state;
    fun ppf _ -> F.fprintf ppf "%s$%i" v i

  let add_block state = ref !state
end

type jsfun = { module_path : string; function_path : string }

let jsfun ~module_path ~function_path = { module_path; function_path }

let pp_jsfun ppf { module_path; function_path } =
  Format.fprintf ppf "(@[%S %S@])" module_path function_path

type t = jsfun Compile.t
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

let set a b =
  stmt (fun ppf state -> F.fprintf ppf "%a =@ @[<hv 2>%a@]" a state b state)

let ( .%() ) a b ppf state = F.fprintf ppf "%a[%a]" a state b state
let ( .%()<- ) a i b = set a.%(i) b
let ( .!() ) a b ppf state = F.fprintf ppf "%a.%s" a state b
let ( .!()<- ) a i b = set a.!(i) b
let string x ppf _ = pp_string ppf x
let global x ppf _ = F.pp_print_string ppf x
let comment ppf str = F.fprintf ppf "/* %s */@," str

module type JSMODULE = sig
  val import : jsfun -> (js -> js) -> js
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

module MakeJavaScript (M : JSMODULE) = struct
  let seq s ppf state =
    F.fprintf ppf "[@,%a%t]"
      (F.pp_print_seq ~pp_sep:Pp.comma (fun ppf x ->
           F.fprintf ppf "@[<hv 2>%a@]" x state))
      s trailing_comma

  let tern cond i e ppf state =
    F.fprintf ppf "%a@ ? @[<hv 2>%a@]@ : @[<hv 2>%a@]" cond state i state e
      state

  let apply_n f args ppf state =
    F.fprintf ppf "@[<hv 2>%a(@,%a@;<0 -2>)@]" f state
      (F.pp_print_list ~pp_sep:Pp.comma (fun ppf x ->
           F.fprintf ppf "@[<hv 2>%a@]" x state))
      args

  let obj l ppf state =
    F.fprintf ppf "@[<hv 2>{@,%a@;<0 -2>}@]"
      (F.pp_print_list ~pp_sep:Pp.comma (fun ppf (k, v) ->
           F.fprintf ppf "@[<hv 2>%s:@ %a@]" k v state))
      l

  let new_ name args ppf state =
    F.fprintf ppf "@[<hv 2>new %s(@,@[<hv 2>%a@]@;<0 -2>)@]" name
      (F.pp_print_list ~pp_sep:Pp.comma (fun ppf x ->
           F.fprintf ppf "@[<hv 2>%a@]" x state))
      args

  let for_ expr stmts ppf state =
    let state' = State.add_block state in
    let i = State.var "i" state' in
    F.fprintf ppf "@[<hv 2>for (let %a = 0; %a < %a; %a++) {@ %a@;<0 -2>}@]" i
      state' i state' expr state i state' (stmts i) state'

  let for_of expr stmts ppf state =
    let state' = State.add_block state in
    let x = State.var "x" state' in
    F.fprintf ppf "@[<hv 2>for (let %a of %a) {@ %a@;<0 -2>}@]" x state' expr
      state (stmts x) state'

  let switch exp cases default ppf state =
    F.fprintf ppf "@[<v 2>@[<hv 2>switch (%a)@] {@ " exp state;
    let state = State.add_block state in
    F.pp_print_list ~pp_sep:F.pp_print_cut
      (fun ppf (exp, stmts) ->
        F.fprintf ppf "@[<hv 2>case %a:@ %a@ break;@]" exp state stmts state)
      ppf cases;
    F.fprintf ppf "@ @[<hv 2>default:@ %a@]" default state;
    F.fprintf ppf "@;<1 -2>}@]"

  let and_ a b ppf state =
    F.fprintf ppf "@[<hv>%a &&@]@ @[<hv>%a@]" a state b state

  let or_ a b ppf state =
    F.fprintf ppf "@[<hv>%a ||@]@ @[<hv>%a@]" a state b state

  let equal a b ppf state = F.fprintf ppf "%a ===@ %a" a state b state
  let typeof expr ppf state = F.fprintf ppf "typeof %a" expr state
  let to_int expr ppf state = F.fprintf ppf "(%a | 0)" expr state

  include M

  type 'a exp = js

  let return x =
    stmt (fun ppf -> F.fprintf ppf "return (@,@[<hv 2>%a@]@;<0 -2>)" x)

  let ( let$ ) = ( let$ )

  type 'a stmt = js
  type 'a obs = js

  let observe = Fun.id
  let ( |: ) = ( |: )

  type 'a mut = js

  let ( let& ) = ( let$ )
  let deref = Fun.id

  let ( := ) a b =
    stmt (fun ppf state -> F.fprintf ppf "%a =@ @[<hv 2>%a@]" a state b state)

  let incr a = stmt (fun ppf -> F.fprintf ppf "%a++" a)

  let lambda f ppf state =
    let state = State.add_block state in
    let arg = State.var "arg" state in
    F.fprintf ppf "(%a) => {@ %a@;<1 -2>}" arg state (f arg) state

  let ( @@ ) = ( @@ )

  let if_ exp ~then_ ~else_ ppf state =
    let state' = State.add_block state in
    F.fprintf ppf "@[<hv 2>@[<hv 2>if (@,%a@;<0 -2>)@] {@ %a" exp state
      (then_ ()) state';
    (match else_ with
    | None -> ()
    | Some e ->
        let state' = State.add_block state in
        F.fprintf ppf "@;<1 -2>} else {@ %a" (e ()) state');
    F.fprintf ppf "@;<1 -2>}@]"

  let while_ cond stmts ppf state =
    F.fprintf ppf "@[<hv 2>while (%a) " (cond ()) state;
    let state = State.add_block state in
    F.fprintf ppf "{@ %a@;<1 -2>}@]" (stmts ()) state

  type external_data
  type 'a promise
  type import = jsfun

  let unit _ _ = ()
  let not x ppf state = F.fprintf ppf "@[<hv 2>!(%a)@]" x state
  let int x ppf _ = F.pp_print_int ppf x
  let float x ppf _ = F.pp_print_float ppf x
  let string = string
  let bool x ppf _ = F.pp_print_bool ppf x
  let pair (a, b) = seq Seq.(fun () -> Cons (a, fun () -> Cons (b, empty)))
  let equal_int = equal
  let equal_string = equal
  let int_to_string x = x.!("toString") @@ unit
  let int_to_float = Fun.id
  let float_to_string = int_to_string
  let bool_to_string x = tern x (string "true") (string "false")
  let array a = seq (Array.to_seq a)

  let array_init i x =
    apply_n
      (global "Array").!("from")
      [ obj [ ("length", i) ]; lambda (fun _ -> return x) ]

  let ( .%() ) = ( .%() )
  let ( .%()<- ) = ( .%()<- )
  let array_concat a s = a.!("join") @@ s

  type 'a hashtbl

  let hashtbl s = new_ "Map" [ seq s ]
  let hashtbl_create () = new_ "Map" [ unit ]
  let ( .%{} ) x k = x.!("get") @@ k
  let ( .%{}<- ) x k v = stmt (apply_n x.!("set") [ k; v ])
  let hashtbl_mem x k = x.!("has") @@ k
  let hashtbl_copy x = new_ "Map" [ x ]
  let hashtbl_iter x f = for_of x (fun entry -> f entry.%(int 0) entry.%(int 1))
  let promise = ( @@ ) (global "Promise").!("resolve")
  let bind_array a f = ((global "Promise").!("all") @@ a).!("then") @@ f

  type buffer

  let buffer_create () = array [||]
  let buffer_add_string b s = stmt (b.!("push") @@ s)
  let buffer_add_promise = buffer_add_string

  let buffer_to_promise =
    lambda (fun a ->
        return
          (bind_array a (lambda (fun x -> return (array_concat x (string ""))))))

  let escape =
    let add_set a b =
      stmt (fun ppf state -> F.fprintf ppf "%a +=@ %a" a state b state)
    in
    lambda (fun str ->
        let& result = ("result", string "") in
        for_ str.!("length") (fun i ->
            let$ c = ("c", str.%(i)) in
            switch c
              [
                (string "&", add_set result (string "&amp;"));
                (string "\"", add_set result (string "&quot;"));
                (string "'", add_set result (string "&apos;"));
                (string ">", add_set result (string "&gt;"));
                (string "<", add_set result (string "&lt;"));
                (string "/", add_set result (string "&#x2F;"));
                (string "`", add_set result (string "&#x60;"));
                (string "=", add_set result (string "&#x3D;"));
              ]
              (add_set result c))
        |: return result)

  type 'a stack

  let stack_create () = array [||]
  let stack_is_empty x = equal x.!("length") (int 0)
  let stack_push x y = stmt (x.!("unshift") @@ y)
  let stack_drop x = stmt (x.!("shift") @@ unit)
  let stack_concat = array_concat

  type error = js

  let raise = return
  let error s = (global "Promise").!("reject") @@ new_ "Error" [ s ]

  type data

  module Data = struct
    type t = data exp

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
        for_of ((global "Object").!("keys") @@ x) (fun key -> f key x.%(key))
    end

    type t = external_data exp

    let null = global "null"
    let some = Fun.id
    let of_bool x = tern x (bool true) (bool false)
    let of_int = Fun.id
    let of_float = Fun.id
    let of_string = Fun.id
    let of_array = Fun.id
    let of_hashtbl x = (global "Object").!("fromEntries") @@ x
    let of_untyped = Fun.id

    let to_int x ~ok ~error =
      if_
        (and_
           (equal (typeof x) (string "number"))
           ((global "Number").!("isInteger") @@ x))
        ~then_:(fun () -> ok (to_int x))
        ~else_:(Some error)

    let to_string x ~ok ~error =
      if_
        (equal (typeof x) (string "string"))
        ~then_:(fun () -> ok x)
        ~else_:(Some error)

    let to_float x ~ok ~error =
      if_
        (equal (typeof x) (string "number"))
        ~then_:(fun () -> ok x)
        ~else_:(Some error)

    let to_bool x ~ok ~error =
      if_
        (equal (typeof x) (string "boolean"))
        ~then_:(fun () -> ok x)
        ~else_:(Some error)

    let to_linear x ~ok ~error =
      if_
        ((global "Array").!("isArray") @@ x)
        ~then_:(fun () -> ok x)
        ~else_:(Some error)

    let is_null x = or_ (equal x null) (equal x (global "undefined"))

    let to_assoc x ~ok ~error =
      if_
        (and_ (equal (typeof x) (string "object")) (not (equal x null)))
        ~then_:(fun () -> ok x)
        ~else_:(Some error)

    let show x = global "String" @@ x
  end
end

module RemoveIdsAndUnits (F : Instruct.SEM) :
  Instruct.SEM with type 'a obs = 'a F.obs and type import = F.import = struct
  module Trans = struct
    type 'a from_exp = 'a F.exp
    type 'a exp = { from : 'a from_exp; identity : bool }

    let fwd x = { from = x; identity = false }
    let bwd x = x.from

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

  let if_ x ~then_ ~else_ =
    match (then_ (), else_) with
    | Unit, None -> Unit
    | Unit, Some else_ -> (
        match else_ () with
        | Unit -> Unit
        | Unk _ ->
            fwds
              (F.if_ (bwd (not x))
                 ~then_:(fun () -> bwds (else_ ()))
                 ~else_:None))
    | Unk _, Some else_ -> (
        match else_ () with
        | Unit ->
            fwds (F.if_ (bwd x) ~then_:(fun () -> bwds (then_ ())) ~else_:None)
        | Unk _ ->
            fwds
              (F.if_ (bwd x)
                 ~then_:(fun () -> bwds (then_ ()))
                 ~else_:(Some (fun () -> bwds (else_ ())))))
    | Unk _, None ->
        fwds (F.if_ (bwd x) ~then_:(fun () -> bwds (then_ ())) ~else_:None)

  let ( let$ ) : string * 'a exp -> ('a exp -> 'b stmt) -> 'b stmt =
   fun (name, x) f ->
    if x.identity then f x
    else
      fwds
        (F.( let$ ) (name, x.from) (fun x ->
             bwds (f { from = x; identity = true })))

  let int_to_float x = { x with from = F.int_to_float x.from }

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
    let of_array x = { x with from = F.External.of_array x.from }
    let of_untyped x = { x with from = F.External.of_untyped x.from }
  end
end

let pp (module Jsmod : JSMODULE) ppf c =
  let module I = Instruct.Make (RemoveIdsAndUnits (MakeJavaScript (Jsmod))) in
  F.fprintf ppf "@[<v>";
  comment ppf "THIS FILE WAS GENERATED BY ACUTIS.";
  I.eval c ppf (State.make ());
  F.fprintf ppf "@]"

let esm = pp (module Esm)
let cjs = pp (module Cjs)
