(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2024 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module A = Acutis_internals

type error = A.Error.t

exception Acutis_error = A.Error.Acutis_error

type typescheme = A.Typechecker.Type.scheme
type 'a comp = 'a A.Compile.Components.source

let comp_parse = A.Compile.Components.from_src
let comp_fun = A.Compile.Components.from_fun

type 'a comps_compiled = 'a A.Compile.Components.t

let comps_compile = A.Compile.Components.of_seq
let comps_empty = A.Compile.Components.empty

type parsed = { fname : string; ast : A.Ast.t }

let parse ~fname lexbuf = { fname; ast = A.Compile.parse ~fname lexbuf }

type 'a compiled = 'a A.Compile.t

let compile components { fname; ast } = A.Compile.make ~fname components ast
let compile_interface = A.Compile.make_interface
let get_typescheme x = x.A.Compile.types

module type DECODABLE = sig
  type t
  type 'a assoc

  val get_int : t -> int option
  val get_string : t -> string option
  val get_float : t -> float option
  val get_bool : t -> bool option
  val get_some : t -> t option
  val get_seq : t -> t Seq.t option
  val get_assoc : t -> t assoc option
  val assoc_find : string -> 'a assoc -> 'a
  val assoc_mem : string -> 'a assoc -> bool
  val assoc_to_seq : 'a assoc -> (string * 'a) Seq.t
  val null : t
  val some : t -> t
  val of_float : float -> t
  val of_string : string -> t
  val of_bool : bool -> t
  val of_int : int -> t
  val of_seq : t Seq.t -> t
  val of_seq_assoc : (string * t) Seq.t -> t
  val to_string : t -> string
  val marshal : 'a -> t
end

module InterfaceCombinators (M : DECODABLE) = struct
  open A.Ast
  open M

  let raise_str = A.Error.raise_fmt "@[<v>Interface decode error.@;@[%(%s%)@]@]"
  let raise = A.Error.raise_fmt "@[<v>Interface decode error.@;@[%(%)@]@]"
  let error_str = raise_str "'%s' is not a valid type."
  let else_error x = error_str (to_string x)
  let error_enum x = raise_str "'%s' is not valid in this enum." (to_string x)
  let error_uncons_single = raise_str "Type '%s' only takes one parameter."
  let error_seq () = raise "Type sequences cannot be empty."
  let error_record () = raise "Records need at least one type."
  let loc = A.Loc.dummy
  let opt f g h x = match f x with Some x -> g x | None -> h x

  let uncons f x =
    match x () with Seq.Cons (hd, tl) -> f hd tl | Seq.Nil -> error_seq ()

  let uncons_single ~name f =
    uncons (fun hd tl ->
        match tl () with
        | Seq.Cons _ -> error_uncons_single name
        | Seq.Nil -> f hd)

  let rec list_enum f s =
    match s () with
    | Seq.Cons (hd, tl) -> (
        match f hd with
        | Some hd -> hd :: list_enum f tl
        | None -> error_enum hd)
    | Seq.Nil -> []

  let rec list f s =
    match s () with Seq.Cons (hd, tl) -> f hd :: list f tl | Seq.Nil -> []

  let nonempty_record f s =
    match s () with
    | Seq.Cons (h, tl) -> A.Nonempty.(f h :: list f tl)
    | Seq.Nil -> error_record ()

  let get_bool x = get_bool x |> Option.map Bool.to_int
  let get_assoc x = get_assoc x |> Option.map M.assoc_to_seq
  let if_int f g x = opt get_int f g x
  let if_bool f g x = opt get_bool f g x
  let if_string f g x = opt get_string f g x
  let if_seq f g x = opt get_seq f g x
  let if_assoc f g x = opt get_assoc f g x
  let tag_int x = Tag (Tag_int (loc, x))
  let tag_bool x = Tag (Tag_bool (loc, x))
  let tag_string x = Tag (Tag_string (loc, x))
  let ty_named x = Ty_named (loc, x)
  let row_closed = (loc, `Closed)
  let row_open = (loc, `Open)
  let ty_enum_int r hd tl = Ty_enum_int (hd :: list_enum get_int tl, r)
  let ty_enum_bool hd tl = Ty_enum_bool (hd :: list_enum get_bool tl)
  let ty_enum_string r hd tl = Ty_enum_string (hd :: list_enum get_string tl, r)
  let value f x = Value (f x)

  let rec ty_nullable x = Ty_nullable (ty x)
  and ty_list x = Ty_list (ty x)
  and ty_dict x = Ty_dict (ty x)
  and ty_tuple l = Ty_tuple (list ty l)
  and ty_record x = Ty_record ([ record x ], row_closed)

  and ty_record_list r x =
    Ty_record (nonempty_record (if_assoc record else_error) x, r)

  and record l = (loc, nonempty_record record_item l)
  and record_item (k, v) = (loc, k, record_value v)

  and tag_or_value name =
    match name with
    | "tag" ->
        uncons_single ~name
          (if_int tag_int (if_bool tag_bool (if_string tag_string else_error)))
    | name -> value (ty_arg name)

  and record_value x =
    if_string (value ty_named)
      (if_assoc (value ty_record)
         (if_seq (uncons (if_string tag_or_value else_error)) else_error))
      x

  and ty x =
    if_string ty_named
      (if_assoc ty_record
         (if_seq (uncons (if_string ty_arg else_error)) else_error))
      x

  and ty_arg name =
    match name with
    | "nullable" -> uncons_single ~name ty_nullable
    | "list" -> uncons_single ~name ty_list
    | "dict" -> uncons_single ~name ty_dict
    | "tuple" -> ty_tuple
    | "enum" ->
        uncons
          (if_int (ty_enum_int row_closed)
             (if_string
                (ty_enum_string row_closed)
                (if_bool ty_enum_bool else_error)))
    | "enum_open" ->
        uncons
          (if_int (ty_enum_int row_open)
             (if_string (ty_enum_string row_open) else_error))
    | "union" -> ty_record_list row_closed
    | "union_open" -> ty_record_list row_open
    | name -> error_str name

  let prop (name, x) = { loc; name; ty = ty x }

  let interface l =
    if_assoc (list prop) else_error l |> A.Typechecker.make_interface_standalone
end

let interface (type a) (module D : DECODABLE with type t = a) =
  let module M = InterfaceCombinators (D) in
  M.interface

module Render (D : DECODABLE) = struct
  include A.Instruct.Make (struct
    include Stdlib

    type 'a stm = 'a
    type 'a exp = 'a

    let return = Fun.id
    let raise = A.Error.raise_fmt "%s"
    let stm = Fun.id
    let ( let| ) a f = a; f ()
    let ( let$ ) (_, x) f = f x
    let ( let& ) (_, x) f = f (ref x)
    let lambda = Fun.id
    let if_else b ~then_ ~else_ = if b then then_ () else else_ ()

    let while_ f b g =
      while f !b do
        g ()
      done

    let unit = ()
    let int = Fun.id
    let float = Fun.id
    let string = Fun.id
    let bool = Fun.id
    let pair = Fun.id
    let unpair = Fun.id

    type 'a obs = 'a

    let observe = Fun.id

    let uncons seq ~nil ~cons =
      match seq () with Seq.Nil -> nil () | Seq.Cons (x, s) -> cons x s

    let generator : type a. ((a -> unit) -> unit) -> a Seq.t =
     fun f ->
      let module M = struct
        type _ Effect.t += Yield : a -> unit Effect.t
      end in
      let yield v = Effect.perform (M.Yield v) in
      fun () ->
        match f yield with
        | () -> Seq.Nil
        | effect M.Yield v, k -> Seq.Cons (v, Effect.Deep.continue k)

    let iter s f = Seq.iter f s
    let string_to_seq = String.to_seq
    let match_char = ( |> )
    let array = Fun.id
    let array_make = Array.make
    let ( .%() ) = Array.get
    let ( .%()<- ) = Array.set

    module Tbl = Hashtbl.MakeSeeded (String)

    let hashtbl = Tbl.of_seq
    let hashtbl_create () = Tbl.create 16
    let ( .%{} ) = Tbl.find
    let ( .%{}<- ) = Tbl.add
    let hashtbl_mem = Tbl.mem
    let hashtbl_to_seq = Tbl.to_seq
    let buffer_create () = Buffer.create 1024
    let buffer_add_string = Buffer.add_string
    let buffer_add_char = Buffer.add_char
    let buffer_contents = Buffer.contents
    let buffer_length = Buffer.length

    type 'a promise = 'a

    let await = Fun.id
    let async_lambda = Fun.id

    type untyped = ..

    module type UNTYPED = sig
      type t

      val inject : t exp -> untyped exp
      val project : untyped exp -> t exp
      val test : untyped exp -> bool exp
    end

    let untyped (type a) name (f : (module UNTYPED with type t = a) -> _) =
      f
        (module struct
          type t = a
          type untyped += Untyped of a

          let inject a = Untyped a

          let project = function
            | Untyped a -> a
            | _ -> A.Error.internal ~__POS__ "Expected %s" name

          let test = function Untyped _ -> true | _ -> false
        end)

    module External = struct
      include D

      type 'a decoder = t -> 'a option

      let decode f t ~ok ~error =
        match f t with Some x -> ok x | None -> error ()
    end

    type import = External.t -> string promise

    let import = ( |> )
    let export = Fun.id
  end)
end

let render (type a) (module D : DECODABLE with type t = a) =
  let module R = Render (D) in
  R.eval

module PrintJs = struct
  module F = Format

  module State : sig
    type t
    (** This tracks variable names used across JavaScript scopes so let-bindings
        are safe. JavaScript scope is not equivalent to our native scope. *)

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

  type t = F.formatter -> State.t -> unit

  let trailing_comma =
    F.pp_print_custom_break ~fits:("", 0, "") ~breaks:(",", -2, "")

  (** See
      https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String#escape_sequences
  *)
  let pp_char_aux ~newline ppf = function
    | '\n' -> F.fprintf ppf "\\n%a" newline ()
    | '\b' -> F.pp_print_string ppf "\\b"
    | '\t' -> F.pp_print_string ppf "\\t"
    | '\012' -> F.pp_print_string ppf "\\f"
    | '\r' -> F.pp_print_string ppf "\\r"
    | '\\' -> F.pp_print_string ppf "\\\\"
    | '"' -> F.pp_print_string ppf "\\\""
    | c -> F.pp_print_char ppf c

  let pp_string ppf s =
    let newline =
      if String.length s < 60 then F.pp_print_nothing
      else fun ppf () -> F.pp_print_string ppf "\\\n"
    in
    let pp_sep = F.pp_print_nothing in
    F.fprintf ppf "\"%a\""
      (F.pp_print_iter ~pp_sep String.iter (pp_char_aux ~newline))
      s

  let pp_char ppf c =
    F.fprintf ppf "\"%a\"" (pp_char_aux ~newline:F.pp_print_nothing) c

  (** Common functions used amongst the different modules. *)
  module JavascriptShared = struct
    let stm x ppf state = F.fprintf ppf "@[<hv 2>%a;@]" x state
    let ( let| ) a f ppf state = F.fprintf ppf "%a@ %a" a state (f ()) state

    let ( let$ ) (name, x) f ppf state =
      let name = State.var name state in
      (let| () =
         stm (fun ppf state ->
             F.fprintf ppf "let %a =@ @[<hv 2>%a@]" name state x state)
       in
       f name)
        ppf state

    let apply_n f args ppf state =
      F.fprintf ppf "@[<hv 2>%a(@,%a@;<0 -2>)@]" f state
        (F.pp_print_list ~pp_sep:A.Pp.comma (fun ppf x ->
             F.fprintf ppf "@[<hv 2>%a@]" x state))
        args

    let ( @@ ) f e = apply_n f [ e ]
    let string x ppf _ = pp_string ppf x

    let set a b =
      stm (fun ppf state -> F.fprintf ppf "%a =@ @[<hv 2>%a@]" a state b state)

    let ( .%() ) a b ppf state = F.fprintf ppf "%a[%a]" a state b state
    let ( .%()<- ) a i b = set a.%(i) b
    let ( .!() ) a b ppf state = F.fprintf ppf "%a.%s" a state b
    let ( .!()<- ) a i b = set a.!(i) b
    let global x ppf _ = F.pp_print_string ppf x
    let comment ppf str = F.fprintf ppf "/* %s */@," str
  end

  module type JSMODULE = sig
    type nonrec import = import

    val import : import -> (t -> t) -> t
    val export : t -> t
  end

  module Esm : JSMODULE = struct
    open JavascriptShared

    type nonrec import = import

    let import { module_path; function_path } f ppf state =
      let import = State.var "import" state in
      (let| () =
         stm (fun ppf state ->
             F.fprintf ppf "import {%a as %a} from %a" pp_string function_path
               import state pp_string module_path)
       in
       f import)
        ppf state

    let export x = stm (fun ppf -> F.fprintf ppf "export default %a" x)
  end

  module Cjs : JSMODULE = struct
    open JavascriptShared

    type nonrec import = import

    let import { module_path; function_path } f =
      let$ import = ("import", global "require" @@ string module_path) in
      f import.%(string function_path)

    let export x = (global "module").!("exports") <- x
  end

  (** Instruction semantics with extra JavaScript printing features. *)
  module type SEM_JAVASCRIPT = sig
    include A.Instruct.SEM

    val if_ : bool exp -> then_:(unit -> unit stm) -> unit stm
  end

  module JsSem (JsMod : JSMODULE) :
    SEM_JAVASCRIPT with type 'a obs = t and type import = import = struct
    include JavascriptShared
    include JsMod

    type 'a stm = t
    type 'a obs = t

    let observe = Fun.id

    type 'a exp = t

    let return x =
      stm (fun ppf -> F.fprintf ppf "return (@,@[<hv 2>%a@]@;<0 -2>)" x)

    let new_ name args ppf state =
      F.fprintf ppf "@[<hv 2>new %a(@,@[<hv 2>%a@]@;<0 -2>)@]" name state
        (F.pp_print_list ~pp_sep:A.Pp.comma (fun ppf x ->
             F.fprintf ppf "@[<hv 2>%a@]" x state))
        args

    let throw x =
      stm (fun ppf -> F.fprintf ppf "throw (@,@[<hv 2>%a@]@;<0 -2>)" x)

    let raise s = throw (new_ (global "Error") [ s ])

    type 'a ref = t

    let ( let& ) = ( let$ )
    let ( ! ) = Fun.id
    let incr a = stm (fun ppf -> F.fprintf ppf "%a++" a)

    let ( := ) a b =
      stm (fun ppf state -> F.fprintf ppf "%a =@ @[<hv 2>%a@]" a state b state)

    let lambda_aux async f ppf state =
      let state = State.add_block state in
      let arg = State.var "arg" state in
      let async = match async with `Async -> "async " | `Sync -> "" in
      F.fprintf ppf "%s(%a) => {@ %a@;<1 -2>}" async arg state (f arg) state

    let lambda = lambda_aux `Sync

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

    let array_of_seq seq ppf state =
      F.fprintf ppf "[@,%a%t]"
        (F.pp_print_seq ~pp_sep:A.Pp.comma (fun ppf x ->
             F.fprintf ppf "@[<hv 2>%a@]" x state))
        seq trailing_comma

    let unit _ _ = ()
    let not x ppf state = F.fprintf ppf "@[<hv 2>!(%a)@]" x state
    let int x ppf _ = F.pp_print_int ppf x
    let float x ppf _ = F.pp_print_float ppf x
    let char x ppf _ = pp_char ppf x
    let bool x ppf _ = F.pp_print_bool ppf x
    let ( = ) a b ppf state = F.fprintf ppf "%a ===@ %a" a state b state
    let pair (a, b) = array_of_seq (Seq.cons a (Seq.cons b Seq.empty))
    let unpair x = (x.%(int 0), x.%(int 1))
    let to_string x = global "String" @@ x
    let string_of_int = to_string
    let string_of_float = to_string
    let string_of_bool = to_string

    let obj l ppf state =
      F.fprintf ppf "@[<hv 2>{@,%a@;<0 -2>}@]"
        (F.pp_print_list ~pp_sep:A.Pp.comma (fun ppf (k, v) ->
             F.fprintf ppf "@[<hv 2>%s:@ %a@]" k v state))
        l

    (** Sequences use the JS iterator protocol. **)
    let uncons seq ~nil ~cons =
      let$ next = ("next", apply_n seq.!("next") []) in
      if_else next.!("done") ~then_:nil ~else_:(fun () ->
          cons next.!("value") seq)

    let yield x =
      stm (fun ppf -> F.fprintf ppf "yield (@,@[<hv 2>%a@]@;<0 -2>)" x)

    let generator f ppf state =
      let state = State.add_block state in
      F.fprintf ppf "(function* () {@ %a@;<1 -2>})()" (f yield) state

    let iter seq f ppf state =
      let state' = State.add_block state in
      let item = State.var "item" state' in
      F.fprintf ppf "@[<hv 2>for (let %a of %a) {@ %a@;<0 -2>}@]" item state'
        seq state (f item) state'

    let string_to_seq x = apply_n x.%((global "Symbol").!("iterator")) []

    let switch exp cases default ppf state =
      F.fprintf ppf "@[<v 2>@[<hv 2>switch (%a)@] {@ " exp state;
      let state = State.add_block state in
      F.pp_print_list ~pp_sep:F.pp_print_cut
        (fun ppf (exp, stmts) ->
          F.fprintf ppf "@[<hv 2>case %a:@ %a@ break;@]" exp state stmts state)
        ppf cases;
      F.fprintf ppf "@ @[<hv 2>default:@ %a@]" default state;
      F.fprintf ppf "@;<1 -2>}@]"

    let match_char c f =
      switch c
        [
          (char '&', f '&');
          (char '"', f '"');
          (char '\'', f '\'');
          (char '>', f '>');
          (char '<', f '<');
          (char '/', f '/');
          (char '`', f '`');
          (char '=', f '=');
        ]
        (f '\x00')

    let array a = array_of_seq (Array.to_seq a)
    let array_make i x = array_of_seq (Seq.init i (Fun.const x))
    let hashtbl s = new_ (global "Map") [ array_of_seq (Seq.map pair s) ]
    let hashtbl_create () = new_ (global "Map") [ unit ]
    let ( .%{} ) x k = x.!("get") @@ k
    let ( .%{}<- ) x k v = stm (apply_n x.!("set") [ k; v ])
    let hashtbl_mem x k = x.!("has") @@ k
    let hashtbl_to_seq x = apply_n x.!("entries") []

    let ( += ) a b =
      stm (fun ppf state -> F.fprintf ppf "%a +=@ @[<hv 2>%a@]" a state b state)

    let buffer_create () = obj [ ("contents", string "") ]
    let buffer_add_string b s = b.!("contents") += s
    let buffer_add_char = buffer_add_string
    let buffer_contents b = b.!("contents")
    let buffer_length b = b.!("contents").!("length")

    type 'a promise

    let await p ppf state = F.fprintf ppf "@[<hv 2>await@ %a@]" p state
    let async_lambda = lambda_aux `Async

    let and_ a b ppf state =
      F.fprintf ppf "@[<hv>%a &&@]@ @[<hv>%a@]" a state b state

    let typeof expr ppf state = F.fprintf ppf "typeof %a" expr state

    let instanceof a b ppf state =
      F.fprintf ppf "%a instanceof %a" a state b state

    type untyped

    module type UNTYPED = sig
      type t

      val inject : t exp -> untyped exp
      val project : untyped exp -> t exp
      val test : untyped exp -> bool exp
    end

    let untyped (type a) name (f : (module UNTYPED with type t = a) -> _) ppf
        state =
      let name = State.var (String.capitalize_ascii name) state in
      let state' = State.add_block state in
      let arg = State.var "arg" state' in
      F.fprintf ppf "function %a(%a) {@[<hv 2>@ %a@;<1 -2>@]}@ %a" name state
        arg state'
        ((global "this").!("v") := arg)
        state'
        (f
           (module struct
             type t = a

             let inject a = new_ name [ a ]
             let project a = a.!("v")
             let test a = instanceof a name
           end))
        state

    module External = struct
      type t

      let null = global "null"
      let some = Fun.id
      let of_int = Fun.id
      let of_float = Fun.id
      let of_string = Fun.id
      let of_bool = Fun.id
      let of_seq x = (global "Array").!("from") @@ x
      let of_seq_assoc x = (global "Object").!("fromEntries") @@ x

      type 'a assoc

      let assoc_find k x = x.%(k)
      let assoc_mem k x = apply_n (global "Object").!("hasOwn") [ x; k ]
      let array_values a = apply_n a.!("values") []
      let assoc_to_seq x = array_values ((global "Object").!("entries") @@ x)

      type 'a decoder = {
        test : t exp -> bool exp;
        convert : 'b. t exp -> ('a exp -> 'b stm) -> 'b stm;
      }

      let get_int =
        {
          test = (fun x -> (global "Number").!("isInteger") @@ x);
          convert = ( |> );
        }

      let get_string =
        { test = (fun x -> typeof x = string "string"); convert = ( |> ) }

      let get_float =
        { test = (fun x -> typeof x = string "number"); convert = ( |> ) }

      let get_bool =
        { test = (fun x -> typeof x = string "boolean"); convert = ( |> ) }

      let get_some =
        {
          test = (fun x -> and_ (not (x = null)) (not (x = global "undefined")));
          convert = ( |> );
        }

      let get_seq =
        {
          test = (fun x -> (global "Array").!("isArray") @@ x);
          convert = (fun x f -> ( let$ ) ("seq", array_values x) f);
        }

      let get_assoc =
        {
          test = (fun x -> and_ (typeof x = string "object") (not (x = null)));
          convert = ( |> );
        }

      let decode { test; convert } x ~ok ~error =
        if_else (test x) ~then_:(fun () -> convert x ok) ~else_:error

      let to_string = to_string
      let marshal = Fun.id
    end
  end

  (** Remove identity bindings, extra unit statements, etc. *)
  module Optimize (F : SEM_JAVASCRIPT) :
    A.Instruct.SEM with type 'a obs = 'a F.obs and type import = F.import =
  struct
    module Trans = struct
      type 'a from_exp = 'a F.exp

      type 'a exp = { from : 'a from_exp; identity : bool }
      (** The identity property should track which functions are implemented as
          [Fun.id] in the main runtime below. *)

      let fwde x = { from = x; identity = false }
      let bwde x = x.from

      type 'a from_stm = 'a F.stm
      type _ stm = Unit : unit stm | Unk : 'a F.stm -> 'a stm

      let fwds x = Unk x

      let bwds : type a. a stm -> a from_stm = function
        | Unit -> F.unit
        | Unk x -> x
    end

    open Trans
    module M = A.Instruct.MakeTrans (Trans) (F)
    include M

    let ( let| ) : type a. unit stm -> (unit -> a stm) -> a stm =
     fun a f ->
      match (a, f ()) with
      | Unit, x -> x
      | x, Unit -> x
      | Unk a, Unk b -> Unk F.(( let| ) a (fun () -> b))

    let unit = Unit

    let if_else : type a.
        bool exp -> then_:(unit -> a stm) -> else_:(unit -> a stm) -> a stm =
     fun x ~then_ ~else_ ->
      match (then_ (), else_ ()) with
      | Unit, Unit -> Unit
      | Unit, Unk else_ -> fwds (F.if_ (bwde (not x)) ~then_:(fun () -> else_))
      | Unk then_, Unit -> fwds (F.if_ (bwde x) ~then_:(fun () -> then_))
      | Unk then_, Unk else_ ->
          fwds
            (F.if_else (bwde x)
               ~then_:(fun () -> then_)
               ~else_:(fun () -> else_))

    let ( let$ ) (name, x) f =
      if x.identity then f x
      else
        fwds
          (F.( let$ ) (name, x.from) (fun x ->
               bwds (f { from = x; identity = true })))

    let lambda f =
      fwde (F.lambda (fun x -> bwds (f { from = x; identity = true })))

    let ( ! ) x = { from = F.(!x); identity = true }

    module External = struct
      include M.External

      let some x = { x with from = F.External.some x.from }
      let of_int x = { x with from = F.External.of_int x.from }
      let of_float x = { x with from = F.External.of_float x.from }
      let of_string x = { x with from = F.External.of_string x.from }
      let of_bool x = { x with from = F.External.of_bool x.from }
      let marshal x = { x with from = F.External.marshal x.from }
    end
  end

  let pp (module JsMod : JSMODULE) ppf c =
    let module I = A.Instruct.Make (Optimize (JsSem (JsMod))) in
    let state = State.make () in
    F.fprintf ppf "@[<v>";
    JavascriptShared.comment ppf "THIS FILE WAS GENERATED BY ACUTIS.";
    I.eval c ppf state;
    F.fprintf ppf "@]"
end

type js_import = PrintJs.import

let js_import = PrintJs.import
let esm = PrintJs.pp (module PrintJs.Esm)
let cjs = PrintJs.pp (module PrintJs.Cjs)
let pp_error = A.Error.pp
let pp_typescheme = A.Typechecker.Type.pp_scheme
let pp_ast ppf parsed = A.Pp.TyRepr.pp ppf (A.Ast.TyRepr.t parsed.ast)

let pp_compiled ppf x =
  A.Pp.TyRepr.pp ppf (A.Compile.TyRepr.nodes x.A.Compile.nodes)

let pp_instructions = A.Instruct.pp

let pp_js_import ppf PrintJs.{ module_path; function_path } =
  A.Pp.TyRepr.(
    pp ppf
      (record
         (fields "module_path" (string module_path)
         |> field "function_path" (string function_path))))
