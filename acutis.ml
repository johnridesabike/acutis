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
module MapInt = Map.Make (Int)
module MapString = Map.Make (String)
module SetInt = Set.Make (Int)
module SetString = Set.Make (String)

type error = A.Error.t

exception Acutis_error = A.Error.Acutis_error

module T = A.Typechecker.Type

type ty = T.t
type typescheme = T.scheme

let typescheme = MapString.of_seq
let typescheme_empty = MapString.empty
let unknown = T.unknown
let int = T.int
let float = T.float
let string = T.string
let nullable = T.nullable
let list = T.list
let dict = T.dict
let tuple l = T.tuple (List.of_seq l)
let record l = T.record (ref (MapString.of_seq l))
let enum_int row l = T.enum_int (T.sum (SetInt.of_seq l) row)
let enum_string row l = T.enum_string (T.sum (SetString.of_seq l) row)
let boolean = T.enum_false_and_true
let false_only = T.enum_false_only
let true_only = T.enum_true_only

let nested_seqs_to_map l =
  Seq.map (fun (k, v) -> (k, ref (MapString.of_seq v))) l

let union_int row k l =
  T.union_int k (T.sum (nested_seqs_to_map l |> MapInt.of_seq) row)

let union_string row k l =
  T.union_string k (T.sum (nested_seqs_to_map l |> MapString.of_seq) row)

let union_boolean k ~f ~t =
  T.union_false_and_true k
    ~f:(ref (MapString.of_seq f))
    ~t:(ref (MapString.of_seq t))

let union_false_only k l = T.union_false_only k (ref (MapString.of_seq l))
let union_true_only k l = T.union_true_only k (ref (MapString.of_seq l))

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

let escape_list =
  [
    ('&', "&amp;");
    ('"', "&quot;");
    ('\'', "&apos;");
    ('>', "&gt;");
    ('<', "&lt;");
    ('/', "&#x2F;");
    ('`', "&#x60;");
    ('=', "&#x3D;");
  ]

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val error : string -> 'a t
end

module type DECODABLE = sig
  type 'a linear

  val length : 'a linear -> int
  val iteri : (int -> 'a -> unit) -> 'a linear -> unit

  type 'a assoc

  val assoc_find : string -> 'a assoc -> 'a
  val assoc_mem : string -> 'a assoc -> bool
  val assoc_iter : (string -> 'a -> unit) -> 'a assoc -> unit

  type t

  val null : t
  val some : t -> t
  val of_float : float -> t
  val of_string : string -> t
  val of_bool : bool -> t
  val of_int : int -> t
  val of_array : t array -> t
  val of_assoc : (string * t) Seq.t -> t
  val decode_int : t -> int option
  val decode_string : t -> string option
  val decode_float : t -> float option
  val decode_bool : t -> bool option
  val decode_some : t -> t option
  val decode_linear : t -> t linear option
  val decode_assoc : t -> t assoc option
  val to_string : t -> string
end

module Render (M : MONAD) (D : DECODABLE) : sig
  val apply : (D.t -> string M.t) compiled -> D.t -> string M.t
end = struct
  module I = A.Instruct.Make (struct
    include Stdlib

    type 'a stmt = 'a
    type 'a exp = 'a

    let return = Fun.id
    let stmt = Fun.id
    let ( let| ) a f = a; f ()
    let ( let$ ) (_, x) f = f x
    let ( let& ) (_, x) f = f (ref x)
    let lambda = Fun.id
    let if_ b ~then_ = if b then then_ ()
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

    type 'a obs = 'a

    let observe = Fun.id
    let array = Fun.id
    let array_make = Array.make
    let ( .%() ) = Array.get
    let ( .%()<- ) = Array.set

    module Tbl = Hashtbl.Make (String)

    type 'a hashtbl = 'a Tbl.t

    let hashtbl = Tbl.of_seq
    let hashtbl_create () = Tbl.create 16
    let ( .%{} ) = Tbl.find
    let ( .%{}<- ) = Tbl.add
    let hashtbl_mem = Tbl.mem
    let hashtbl_iter x f = Tbl.iter f x

    type buffer = Buffer.t

    let buffer_create () = Buffer.create 1024
    let buffer_add_string = Buffer.add_string
    let buffer_add_buffer = Buffer.add_buffer

    let buffer_add_escape b s =
      let rec aux escape_list c =
        match escape_list with
        | (c', s) :: l ->
            if Char.equal c c' then Buffer.add_string b s else aux l c
        | [] -> Buffer.add_char b c
      in
      String.iter (aux escape_list) s

    let buffer_contents = Buffer.contents
    let buffer_clear = Buffer.clear
    let buffer_length = Buffer.length

    type 'a promise = 'a M.t

    let promise = M.return
    let bind = M.bind
    let error = M.error

    module External = struct
      include D

      let of_hashtbl x = Tbl.to_seq x |> D.of_assoc

      type _ classify =
        | Int : int classify
        | String : string classify
        | Float : float classify
        | Bool : bool classify
        | Not_null : t classify
        | Linear : t linear classify
        | Assoc : t assoc classify

      let classify_opt : type a. a classify -> t -> a option = function
        | Int -> decode_int
        | String -> decode_string
        | Float -> decode_float
        | Bool -> decode_bool
        | Linear -> decode_linear
        | Assoc -> decode_assoc
        | Not_null -> decode_some

      let classify c t ~ok ~error =
        match classify_opt c t with Some x -> ok x | None -> error ()
    end

    module Data = struct
      type t =
        | Int of int
        | Float of float
        | String of string
        | Array of t array
        | Hashtbl of t hashtbl
        | Unknown of External.t

      let int x = Int x
      let float x = Float x
      let string x = String x
      let array x = Array x
      let hashtbl x = Hashtbl x
      let unknown x = Unknown x

      let to_int = function
        | Int x -> x
        | _ -> A.Error.internal ~__POS__ "Expected Int."

      let to_float = function
        | Float x -> x
        | _ -> A.Error.internal ~__POS__ "Expected Float."

      let to_string = function
        | String x -> x
        | _ -> A.Error.internal ~__POS__ "Expected String."

      let to_array = function
        | Array x -> x
        | _ -> A.Error.internal ~__POS__ "Expected Array."

      let to_hashtbl = function
        | Hashtbl x -> x
        | _ -> A.Error.internal ~__POS__ "Expected Hashtbl."

      let rec to_external_untyped = function
        | Unknown x -> x
        | Int x -> External.of_int x
        | Float x -> External.of_float x
        | String x -> External.of_string x
        | Array x -> Array.map to_external_untyped x |> External.of_array
        | Hashtbl x ->
            Tbl.to_seq x
            |> Seq.map (fun (k, v) -> (k, to_external_untyped v))
            |> External.of_assoc

      let rec equal a b =
        match (a, b) with
        | Int a, Int b -> Int.equal a b
        | Float a, Float b -> Float.equal a b
        | String a, String b -> String.equal a b
        | Array a, Array b -> Seq.equal equal (Array.to_seq a) (Array.to_seq b)
        | Hashtbl a, Hashtbl b ->
            Seq.equal
              (fun (k1, v1) (k2, v2) -> String.equal k1 k2 && equal v1 v2)
              (Tbl.to_seq a) (Tbl.to_seq b)
        | Int _, _
        | Float _, _
        | String _, _
        | Array _, _
        | Hashtbl _, _
        | Unknown _, _ ->
            false
    end

    type import = External.t -> string promise

    let import = ( |> )
    let export = Fun.id
  end)

  let apply = I.eval
end

module Id = struct
  type 'a t = 'a

  let return = Fun.id
  let bind = ( |> )
  let error = A.Error.raise_string
end

let render_string (type a) (module D : DECODABLE with type t = a) =
  let module R = Render (Id) (D) in
  R.apply

module PrintJs = struct
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

  type t = F.formatter -> State.t -> unit

  let trailing_comma =
    F.pp_print_custom_break ~fits:("", 0, "") ~breaks:(",", -2, "")

  (** See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String#escape_sequences *)
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
    F.fprintf ppf "\"%a\""
      (F.pp_print_iter ~pp_sep:F.pp_print_nothing String.iter
         (pp_char_aux ~newline))
      s

  let pp_char ppf c =
    F.fprintf ppf "\"%a\"" (pp_char_aux ~newline:F.pp_print_nothing) c

  (** Define common functions in this submodule so it can be easily included in
    a functor argument. *)
  module Javascript = struct
    type 'a exp = t
    type 'a stmt = t
    type 'a ref = t

    let stmt x ppf state = F.fprintf ppf "@[<hv 2>%a;@]" x state

    let ( @@ ) f e ppf state =
      F.fprintf ppf "@[<hv 2>%a(@,@[<hv 2>%a@]@;<0 -2>)@]" f state e state

    let ( let| ) a f ppf state = F.fprintf ppf "%a@ %a" a state (f ()) state

    let ( let$ ) (name, x) f ppf state =
      let name = State.var name state in
      (let| () =
         stmt (fun ppf state ->
             F.fprintf ppf "let %a =@ @[<hv 2>%a@]" name state x state)
       in
       f name)
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
    let char x ppf _ = pp_char ppf x
    let global x ppf _ = F.pp_print_string ppf x
    let comment ppf str = F.fprintf ppf "/* %s */@," str
    let to_string x = global "String" @@ x

    let obj l ppf state =
      F.fprintf ppf "@[<hv 2>{@,%a@;<0 -2>}@]"
        (F.pp_print_list ~pp_sep:A.Pp.comma (fun ppf (k, v) ->
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

    let array_of_iter iter s ppf state =
      F.fprintf ppf "[@,%a%t]"
        (F.pp_print_iter ~pp_sep:A.Pp.comma iter (fun ppf x ->
             F.fprintf ppf "@[<hv 2>%a@]" x state))
        s trailing_comma

    let apply_n f args ppf state =
      F.fprintf ppf "@[<hv 2>%a(@,%a@;<0 -2>)@]" f state
        (F.pp_print_list ~pp_sep:A.Pp.comma (fun ppf x ->
             F.fprintf ppf "@[<hv 2>%a@]" x state))
        args

    let new_ name args ppf state =
      F.fprintf ppf "@[<hv 2>new %s(@,@[<hv 2>%a@]@;<0 -2>)@]" name
        (F.pp_print_list ~pp_sep:A.Pp.comma (fun ppf x ->
             F.fprintf ppf "@[<hv 2>%a@]" x state))
        args

    let for_of expr stmts ppf state =
      let state' = State.add_block state in
      let x = State.var "x" state' in
      F.fprintf ppf "@[<hv 2>for (let %a of %a) {@ %a@;<0 -2>}@]" x state' expr
        state (stmts x) state'

    let and_ a b ppf state =
      F.fprintf ppf "@[<hv>%a &&@]@ @[<hv>%a@]" a state b state

    let ( = ) a b ppf state = F.fprintf ppf "%a ===@ %a" a state b state
    let typeof expr ppf state = F.fprintf ppf "typeof %a" expr state
  end

  open Javascript

  module type JSMODULE = sig
    val import : import -> (t -> t) -> t
    val export : t -> t
  end

  module Esm : JSMODULE = struct
    let import { module_path; function_path } f ppf state =
      let import = State.var "import" state in
      (let| () =
         stmt (fun ppf state ->
             F.fprintf ppf "import {%a as %a} from %a" pp_string function_path
               import state pp_string module_path)
       in
       f import)
        ppf state

    let export x = stmt (fun ppf -> F.fprintf ppf "export default %a" x)
  end

  module Cjs : JSMODULE = struct
    let import { module_path; function_path } f =
      let$ import = ("import", global "require" @@ string module_path) in
      f import.%(string function_path)

    let export x = (global "module").!("exports") <- x
  end

  module RemoveIdsAndUnits (F : A.Instruct.SEM) :
    A.Instruct.SEM with type 'a obs = 'a F.obs and type import = F.import =
  struct
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
    module M = A.Instruct.MakeTrans (Trans) (F)
    include M

    let ( let| ) : type a. unit stmt -> (unit -> a stmt) -> a stmt =
     fun a f ->
      match (a, f ()) with
      | Unit, x -> x
      | x, Unit -> x
      | Unk a, Unk b -> Unk F.(( let| ) a (fun () -> b))

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

    let float_of_int x = { x with from = F.float_of_int x.from }
    let ( ! ) x = { from = F.(!x); identity = true }

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

      let to_external_untyped x =
        { x with from = F.Data.to_external_untyped x.from }
    end

    module External = struct
      include M.External

      let some x = { x with from = F.External.some x.from }
      let of_int x = { x with from = F.External.of_int x.from }
      let of_float x = { x with from = F.External.of_float x.from }
      let of_string x = { x with from = F.External.of_string x.from }
      let of_bool x = { x with from = F.External.of_bool x.from }
      let of_array x = { x with from = F.External.of_array x.from }
    end
  end

  let pp (module Jsmod : JSMODULE) ppf c =
    let state = State.make () in
    let instructions =
      let$ buffer_add_escape =
        ( "buffer_add_escape",
          lambda (fun b ->
              return
                (lambda (fun s ->
                     let buffer_add s =
                       set b.!("contents") (b.!("contents") + s)
                     in
                     for_ s.!("length") (fun i ->
                         let$ c = ("c", s.%(i)) in
                         switch c
                           (List.map
                              (fun (symbol, esc_symbol) ->
                                (char symbol, buffer_add (string esc_symbol)))
                              escape_list)
                           (buffer_add c))))) )
      in
      let module Runtime :
        A.Instruct.SEM with type 'a obs = t and type import = import = struct
        include Javascript

        type 'a obs = t

        let observe = Fun.id
        let ( ! ) = Fun.id

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
        let string_of_int = to_string
        let float_of_int = Fun.id
        let string_of_float = to_string
        let string_of_bool = to_string
        let array = array_of_iter Array.iter

        let array_make i x =
          apply_n
            (global "Array").!("from")
            [ obj [ ("length", i) ]; lambda (fun _ -> return x) ]

        type 'a hashtbl

        let pair = array_of_iter (fun f (a, b) -> f a; f b)
        let hashtbl s = new_ "Map" [ array_of_iter Seq.iter (Seq.map pair s) ]
        let hashtbl_create () = new_ "Map" [ unit ]
        let ( .%{} ) x k = x.!("get") @@ k
        let ( .%{}<- ) x k v = stmt (apply_n x.!("set") [ k; v ])
        let hashtbl_mem x k = x.!("has") @@ k

        let hashtbl_iter x f =
          for_of x (fun entry -> f entry.%(int 0) entry.%(int 1))

        type buffer

        let buffer_create () = obj [ ("contents", string "") ]
        let buffer_add_escape b s = stmt ((buffer_add_escape @@ b) @@ s)
        let buffer_add_string b s = set b.!("contents") (b.!("contents") + s)
        let buffer_add_buffer b1 b2 = buffer_add_string b1 b2.!("contents")
        let buffer_contents b = b.!("contents")
        let buffer_clear b = set b.!("contents") (string "")
        let buffer_length b = b.!("contents").!("length")

        type 'a promise

        let promise x = (global "Promise").!("resolve") @@ x
        let bind p f = p.!("then") @@ f
        let error s = (global "Promise").!("reject") @@ new_ "Error" [ s ]

        module External = struct
          type 'a linear

          let length a = a.!("length")
          let iteri f a = for_ (length a) (fun i -> f i a.%(i))

          type 'a assoc

          let assoc_find k x = x.%(k)
          let assoc_mem k x = apply_n (global "Object").!("hasOwn") [ x; k ]

          let assoc_iter f x =
            for_of
              ((global "Object").!("keys") @@ x)
              (fun key -> f key x.%(key))

          type t

          let null = global "null"
          let some = Fun.id
          let of_int = Fun.id
          let of_float = Fun.id
          let of_string = Fun.id
          let of_bool = Fun.id
          let of_array = Fun.id
          let of_hashtbl x = (global "Object").!("fromEntries") @@ x

          type _ classify =
            | Int : int classify
            | String : string classify
            | Float : float classify
            | Bool : bool classify
            | Not_null : t classify
            | Linear : t linear classify
            | Assoc : t assoc classify

          let classify (type a) (c : a classify) x ~ok ~error =
            let cond =
              match c with
              | Int -> (global "Number").!("isInteger") @@ x
              | String -> typeof x = string "string"
              | Float -> typeof x = string "number"
              | Bool -> typeof x = string "boolean"
              | Not_null -> and_ (not (x = null)) (not (x = global "undefined"))
              | Linear -> (global "Array").!("isArray") @@ x
              | Assoc -> and_ (typeof x = string "object") (not (x = null))
            in
            if_else cond ~then_:(fun () -> ok x) ~else_:error

          let to_string = to_string
        end

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
          let to_external_untyped = Fun.id
          let equal = ( = )
        end

        type nonrec import = import

        include Jsmod
      end in
      let module I = A.Instruct.Make (RemoveIdsAndUnits (Runtime)) in
      I.eval c
    in
    F.fprintf ppf "@[<v>";
    comment ppf "THIS FILE WAS GENERATED BY ACUTIS.";
    instructions ppf state;
    F.fprintf ppf "@]"
end

type js_import = PrintJs.import

let js_import = PrintJs.import
let esm = PrintJs.pp (module PrintJs.Esm)
let cjs = PrintJs.pp (module PrintJs.Cjs)
let pp_error = A.Error.pp

let pp_typescheme ppf x =
  Format.fprintf ppf "@[<v>%a@]"
    (Format.pp_print_seq ~pp_sep:Format.pp_print_cut
       (A.Pp.equation ~sep:" =" A.Pp.field T.pp))
    (MapString.to_seq x)

let pp_ast ppf parsed = A.Ast.to_sexp parsed.ast |> A.Sexp.pp ppf
let pp_compiled ppf x = A.Compile.to_sexp x.A.Compile.nodes |> A.Sexp.pp ppf
let pp_instructions = A.Instruct.pp

let pp_js_import ppf PrintJs.{ module_path; function_path } =
  Format.fprintf ppf "(@[%S %S@])" module_path function_path
