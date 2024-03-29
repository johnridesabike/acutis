(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2023 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

(** Define the instructions to evaluate a {!Compile} result at runtime. *)

(** To evaluate a {!Compile.t}, we define an abstract language in "tagless
    final" style by using a module type. Each language construct is defined as
    an OCaml function so we automatically inherit the full type safety of any
    OCaml program.

    We can evaluate the language in multiple ways. For example we can interpret
    it as an OCaml program, we can pretty-print it, or we can output it in a
    concrete language like JavaScript (which is just another form of
    pretty-printing). We can also add modular optimizations.

    Our abstract language is roughly based on JavaScript. It has expressions,
    statements, loops, let-bindings, mutable references, hash tables,
    first-class functions, and asynchronous monads (i.e. promises). *)

module type SEM = sig
  (** Define the semantics of our abstract language. *)

  (** {1 Statements and expressions.} *)

  type 'a stmt
  (** A statement, generally some side-effecting code. *)

  type 'a obs
  (** The final evaluation result. *)

  val observe : 'a stmt -> 'a obs
  (** Observe the evaluation result after all transformations have applied. *)

  val ( |: ) : unit stmt -> 'a stmt -> 'a stmt
  (** Sequence statements. {b Evaluation order is unspecified in OCaml}. To
      guarantee that statements are evaluated in your intended order, you must
      let-bind each one before applying them to [|:]. *)

  type 'a exp
  (** An expression. *)

  val return : 'a exp -> 'a stmt
  (** Return value ['a] from a function. *)

  val ( let$ ) : string * 'a exp -> ('a exp -> 'b stmt) -> 'b stmt
  (** Define a new immutable binding. The string is used for pretty-printing. *)

  type 'a mut
  (** A mutable reference variable. *)

  val ( let& ) : string * 'a exp -> ('a mut -> 'b stmt) -> 'b stmt
  (** Define a new reference variable. The string is used for pretty-printing. *)

  val deref : 'a mut -> 'a exp
  val ( := ) : 'a mut -> 'a exp -> unit stmt
  val incr : int mut -> unit stmt

  (** {1 Functions.} *)

  val lambda : ('a exp -> 'b stmt) -> ('a -> 'b) exp
  val ( @@ ) : ('a -> 'b) exp -> 'a exp -> 'b exp

  (** {1 Control flow.} *)

  val if_ : bool exp -> then_:(unit -> unit stmt) -> unit stmt

  val if_else :
    bool exp -> then_:(unit -> 'a stmt) -> else_:(unit -> 'a stmt) -> 'a stmt

  val while_ : (unit -> bool exp) -> (unit -> unit stmt) -> unit stmt

  type external_data
  (** Data from the outside world that we need to decode. *)

  type 'a promise
  (** An asynchronous monad. *)

  type import
  (** Information to import a function from external code. *)

  val import :
    import -> ((external_data -> string promise) exp -> 'a stmt) -> 'a stmt
  (** The import semantics are abstract because runtimes vary. *)

  val export : 'a exp -> 'a stmt

  (** {1 Standard values.} *)

  val unit : unit stmt
  val not : bool exp -> bool exp
  val int : int -> int exp
  val float : float -> float exp
  val string : string -> string exp
  val bool : bool -> bool exp
  val pair : 'a exp * 'b exp -> ('a * 'b) exp
  val equal_int : int exp -> int exp -> bool exp
  val equal_string : string exp -> string exp -> bool exp
  val int_to_string : int exp -> string exp
  val int_to_float : int exp -> float exp
  val float_to_string : float exp -> string exp
  val bool_to_string : int exp -> string exp

  (** {1 Arrays.} *)

  val array : 'a exp array -> 'a array exp
  val array_init : int exp -> 'a exp -> 'a array exp
  val ( .%() ) : 'a array exp -> int exp -> 'a exp
  val ( .%()<- ) : 'a array exp -> int exp -> 'a exp -> unit stmt
  val array_concat : string array exp -> string exp -> string exp

  (** {1 Hash tables.} *)

  type 'a hashtbl
  (** A mutable map of string keys to ['a] values. *)

  val hashtbl : (string * 'a) exp Seq.t -> 'a hashtbl exp
  val hashtbl_create : unit -> 'a hashtbl exp
  val ( .%{} ) : 'a hashtbl exp -> string exp -> 'a exp
  val ( .%{}<- ) : 'a hashtbl exp -> string exp -> 'a exp -> unit stmt
  val hashtbl_mem : 'a hashtbl exp -> string exp -> bool exp
  val hashtbl_copy : 'a hashtbl exp -> 'a hashtbl exp

  val hashtbl_iter :
    'a hashtbl exp -> (string exp -> 'a exp -> unit stmt) -> unit stmt

  (** {1 Promises.} *)

  val promise : 'a exp -> 'a promise exp
  val bind : 'a promise exp -> ('a -> 'b promise) exp -> 'b promise exp
  val error : string exp -> 'a promise exp

  (** {1 Buffers.} *)

  type buffer
  (** A buffer of concurrent string promises. *)

  val buffer_create : unit -> buffer exp
  val buffer_append : buffer exp -> string promise exp -> unit stmt

  (** These are lambdas to minimize generated code. *)

  val buffer_contents : (buffer -> string promise) exp
  val escape : (string -> string) exp

  (** {1 Mutable stacks.} *)

  type 'a stack

  val stack_create : unit -> 'a stack exp
  val stack_is_empty : 'a stack exp -> bool exp
  val stack_push : 'a stack exp -> 'a exp -> unit stmt
  val stack_drop : 'a stack exp -> unit stmt
  val stack_concat : string stack exp -> string exp -> string exp

  (** {1 Runtime data.} *)

  type data
  (** Boxed runtime data. Either a string, an integer, a float, an array, or a
      hash table. *)

  module Data : sig
    type t = data exp

    val int : int exp -> t
    val float : float exp -> t
    val string : string exp -> t
    val array : data array exp -> t
    val hashtbl : data hashtbl exp -> t
    val unknown : external_data exp -> t
    val to_int : t -> int exp
    val to_float : t -> float exp
    val to_string : t -> string exp
    val to_array : t -> data array exp
    val to_hashtbl : t -> data hashtbl exp
    val equal : t -> t -> bool exp
  end

  module External : sig
    (** The foreign data before it's parsed into [Data.t]. *)

    module Linear : sig
      (** Some kind of one-dimensional data container, e.g. an array. *)

      type 'a t

      val length : external_data t exp -> int exp

      val iteri :
        external_data t exp ->
        (int exp -> external_data exp -> unit stmt) ->
        unit stmt
    end

    module Assoc : sig
      (** Some kind of key-value store, e.g a hash table. *)

      type 'a t

      val find : external_data t exp -> string exp -> external_data exp
      val mem : external_data t exp -> string exp -> bool exp

      val iter :
        external_data t exp ->
        (string exp -> external_data exp -> unit stmt) ->
        unit stmt
    end

    type t = external_data exp

    val null : t
    val some : t -> t
    val of_int : int exp -> t
    val of_bool : int exp -> t
    val of_float : float exp -> t
    val of_string : string exp -> t
    val of_array : external_data array exp -> t
    val of_hashtbl : external_data hashtbl exp -> t
    val of_untyped : Data.t -> t

    type _ classify =
      | Int : int classify
      | String : string classify
      | Float : float classify
      | Bool : bool classify
      | Linear : external_data Linear.t classify
      | Assoc : external_data Assoc.t classify

    val classify :
      'a classify ->
      t ->
      ok:('a exp -> 'b stmt) ->
      error:(unit -> 'b stmt) ->
      'b stmt

    val is_null : t -> bool exp
    val show : t -> string exp
  end
end

(** Create evaluation instructions for a given language implementation. *)
module Make (I : SEM) : sig
  open I

  val eval : import Compile.t -> (external_data -> string promise) obs
  (** Evaluate a template with the language implemented by {!I}. *)
end = struct
  module C = Compile
  module M = Matching
  module T = Typechecker.Type
  open I

  let nil_value = Data.int (int 0)
  let false_value = nil_value
  let true_value = Data.int (int 1)
  let unset_exit = int (-1)
  let list_hd e = e.%(int 0)
  let list_tl e = e.%(int 1)
  let get_nullable e = list_hd (Data.to_array e)
  let is_nil x = Data.equal x nil_value
  let is_not_nil x = not (is_nil x)

  type runtime = {
    comps : (data hashtbl -> string promise) hashtbl exp;
    buffer_contents : (buffer -> string promise) exp;
    escape : (string -> string) exp;
  }

  let join_stmts seq =
    match seq () with
    | Seq.Nil -> unit
    | Seq.Cons (s, seq) -> Seq.fold_left ( |: ) s seq

  let parse_escape runtime buf esc x =
    match esc with
    | C.No_escape -> buffer_append buf (promise x)
    | C.Escape -> buffer_append buf (promise (runtime.escape @@ x))

  let fmt runtime buf esc x = function
    | C.Fmt_string -> parse_escape runtime buf esc (Data.to_string x)
    | C.Fmt_int -> parse_escape runtime buf esc (int_to_string (Data.to_int x))
    | C.Fmt_float ->
        parse_escape runtime buf esc (float_to_string (Data.to_float x))
    | C.Fmt_bool ->
        parse_escape runtime buf esc (bool_to_string (Data.to_int x))

  let rec echo props (ech : C.echo) =
    match ech with
    | `Var s -> props.%{string s}
    | `String s -> Data.string (string s)
    | `Field (e, s) -> (echo props e |> Data.to_hashtbl).%{string s}

  let rec echoes runtime buf props esc default default_fmt = function
    | [] -> fmt runtime buf esc (echo props default) default_fmt
    | (f, e) :: tl ->
        let$ nullable = ("nullable", echo props e) in
        if_else (is_not_nil nullable)
          ~then_:(fun () -> fmt runtime buf esc (get_nullable nullable) f)
          ~else_:(fun () -> echoes runtime buf props esc default default_fmt tl)

  let rec construct_data blocks props (data : C.data) =
    match data with
    | `Null -> nil_value
    | `Int i -> Data.int (int i)
    | `String s -> Data.string (string s)
    | `Float f -> Data.float (float f)
    | `Var s -> props.%{string s}
    | `Array a -> construct_data_array blocks props a |> Data.array
    | `Assoc d -> construct_data_hashtbl blocks props d |> Data.hashtbl
    | `Block i -> blocks.(i) |> Data.string
    | `Field (d, s) ->
        (construct_data blocks props d |> Data.to_hashtbl).%{string s}

  and construct_data_array blocks props a =
    array (Array.map (construct_data blocks props) a)

  and construct_data_hashtbl blocks props d =
    Map.String.to_seq d
    |> Seq.map (fun (k, v) -> pair (string k, construct_data blocks props v))
    |> hashtbl

  let add_vars ids arg vars =
    Set.Int.fold (fun id vars -> Map.Int.add id arg vars) ids vars

  let of_scalar = function
    | `String s -> Data.string (string s)
    | `Int i -> Data.int (int i)
    | `Float f -> Data.float (float f)

  let arg_indexed arg index ~optional:_ i f =
    match i with 0 -> f arg | _ -> f index

  let arg_int arg ~optional:_ i f = ( let$ ) ("match_arg", arg.%(int i)) f

  let arg_str arg ~optional key f =
    if optional then
      if_
        (hashtbl_mem arg (string key))
        ~then_:(fun () -> ( let$ ) ("match_arg", arg.%{string key}) f)
    else ( let$ ) ("match_arg", arg.%{string key}) f

  let ( let@ ) = Stdlib.( @@ )

  let rec match_tree :
        'leaf 'key.
        exit:int mut ->
        leafstmt:(vars:Data.t Map.Int.t -> 'leaf -> unit stmt) ->
        get_arg:(optional:bool -> 'key -> (Data.t -> unit stmt) -> unit stmt) ->
        vars:Data.t Map.Int.t ->
        ?optional:bool ->
        ('leaf, 'key) M.tree ->
        unit stmt =
   fun ~exit ~leafstmt ~get_arg ~vars ?(optional = false) -> function
    | M.Switch { key; ids; cases; wildcard; _ } ->
        let@ arg = get_arg ~optional key in
        let vars = add_vars ids arg vars in
        switchcase ~exit ~leafstmt ~get_arg ~vars ~arg ~wildcard cases
    | M.Wildcard { key; ids; child } ->
        let@ arg = get_arg ~optional key in
        let vars = add_vars ids arg vars in
        match_tree ~exit ~leafstmt ~get_arg ~vars child
    | M.Nest { key; ids; child; wildcard } -> (
        let@ arg = get_arg ~optional key in
        let vars = add_vars ids arg vars in
        let s1 =
          let leafstmt ~vars t = match_tree ~exit ~leafstmt ~get_arg ~vars t in
          match child with
          | Int_keys tree ->
              match_tree ~exit ~leafstmt
                ~get_arg:(arg_int (Data.to_array arg))
                ~vars tree
          | String_keys tree ->
              match_tree ~exit ~leafstmt
                ~get_arg:(arg_str (Data.to_hashtbl arg))
                ~vars tree
        in
        match wildcard with
        | None -> s1
        | Some tree ->
            let s2 =
              if_
                (equal_int (deref exit) unset_exit)
                ~then_:(fun () ->
                  match_tree ~exit ~leafstmt ~get_arg ~vars tree)
            in
            s1 |: s2)
    | M.Nil { key; ids; child } ->
        let@ arg = get_arg ~optional key in
        let vars = add_vars ids arg vars in
        if_ (is_nil arg) ~then_:(fun () ->
            match_tree ~exit ~leafstmt ~get_arg ~vars child)
    | M.Cons { key; ids; child } ->
        let@ arg = get_arg ~optional key in
        let vars = add_vars ids arg vars in
        if_ (is_not_nil arg) ~then_:(fun () ->
            match_tree ~exit ~leafstmt ~get_arg ~vars child)
    | M.Nil_or_cons { key; ids; nil; cons } ->
        let@ arg = get_arg ~optional key in
        let vars = add_vars ids arg vars in
        if_else (is_nil arg)
          ~then_:(fun () -> match_tree ~exit ~leafstmt ~get_arg ~vars nil)
          ~else_:(fun () -> match_tree ~exit ~leafstmt ~get_arg ~vars cons)
    | M.Optional { child; next } -> (
        let s1 =
          match_tree ~exit ~leafstmt ~get_arg ~optional:true ~vars child
        in
        match next with
        | None -> s1
        | Some next ->
            let s2 =
              if_
                (equal_int (deref exit) unset_exit)
                ~then_:(fun () ->
                  match_tree ~exit ~leafstmt ~get_arg ~vars next)
            in
            s1 |: s2)
    | M.End leaf -> leafstmt ~vars leaf

  and switchcase ~exit ~leafstmt ~get_arg ~vars ~arg ~wildcard
      M.{ data; if_match; next } =
    if_else
      (Data.equal arg (of_scalar data))
      ~then_:(fun () -> match_tree ~exit ~leafstmt ~get_arg ~vars if_match)
      ~else_:
        (match next with
        | Some l ->
            fun () -> switchcase ~exit ~leafstmt ~get_arg ~vars ~arg ~wildcard l
        | None -> (
            match wildcard with
            | Some l -> fun () -> match_tree ~exit ~leafstmt ~get_arg ~vars l
            | None -> fun () -> unit))

  let match_leaf exitvar props ~vars M.{ names; exit } =
    let s1 =
      Map.String.to_seq names
      |> Seq.map (fun (key, id) -> props.%{string key} <- Map.Int.find id vars)
      |> join_stmts
    in
    let s2 = exitvar := int (M.Exit.key_to_int exit) in
    s1 |: s2

  let make_exits exit exits f =
    match M.Exit.to_seqi exits () with
    | Seq.Nil -> Error.internal ~__POS__ "No exits."
    | Seq.Cons (hd, tl) ->
        let rec aux (i, v) seq =
          match seq () with
          | Seq.Nil -> f v
          | Seq.Cons (hd, tl) ->
              if_else
                (equal_int (deref exit) (int (M.Exit.key_to_int i)))
                ~then_:(fun () -> f v)
                ~else_:(fun () -> aux hd tl)
        in
        aux hd tl

  let rec node runtime buffer props = function
    | C.Text s -> buffer_append buffer (promise (string s))
    | C.Echo (echs, fmt, default, esc) ->
        echoes runtime buffer props esc default fmt echs
    | C.Match (blocks, data, { tree; exits }) ->
        construct_blocks runtime buffer blocks props (fun blocks buffer ->
            let$ arg_match =
              ("arg_match", construct_data_array blocks props data)
            in
            let$ props = ("props", hashtbl_copy props) in
            let& exit = ("exit", unset_exit) in
            let s1 =
              match_tree ~exit ~leafstmt:(match_leaf exit props)
                ~get_arg:(arg_int arg_match) ~vars:Map.Int.empty tree
            in
            let s2 =
              make_exits exit exits (fun l -> nodes runtime buffer props l)
            in
            s1 |: s2)
    | C.Map_list (blocks, data, { tree; exits }) ->
        construct_blocks runtime buffer blocks props (fun blocks buffer ->
            let& index = ("index", int 0) in
            let& cell = ("cell", construct_data blocks props data) in
            while_
              (fun () -> is_not_nil (deref cell))
              (fun () ->
                let$ props = ("props", hashtbl_copy props) in
                let$ list = ("list", deref cell |> Data.to_array) in
                let$ head = ("head", list_hd list) in
                let& exit = ("exit", unset_exit) in
                let s1 =
                  match_tree ~exit ~leafstmt:(match_leaf exit props)
                    ~get_arg:(arg_indexed head (Data.int (deref index)))
                    ~vars:Map.Int.empty tree
                in
                let s2 =
                  make_exits exit exits (fun l -> nodes runtime buffer props l)
                in
                let s3 = incr index in
                let s4 = cell := list_tl list in
                s1 |: s2 |: s3 |: s4))
    | C.Map_dict (blocks, data, { tree; exits }) ->
        construct_blocks runtime buffer blocks props (fun blocks buffer ->
            let$ match_arg = ("match_arg", construct_data blocks props data) in
            hashtbl_iter (Data.to_hashtbl match_arg) (fun k v ->
                let$ props = ("props", hashtbl_copy props) in
                let& exit = ("exit", unset_exit) in
                let s1 =
                  match_tree ~exit ~leafstmt:(match_leaf exit props)
                    ~get_arg:(arg_indexed v (Data.string k))
                    ~vars:Map.Int.empty tree
                in
                let s2 =
                  make_exits exit exits (fun l -> nodes runtime buffer props l)
                in
                s1 |: s2))
    | Component (name, blocks, dict) ->
        construct_blocks runtime buffer blocks props (fun blocks buffer ->
            buffer_append buffer
              (runtime.comps.%{string name}
              @@ construct_data_hashtbl blocks props dict))

  and construct_blocks runtime buffer raw_blocks props f =
    match Array.to_seqi raw_blocks () with
    (* With no blocks, just continue evaluating with a dummy value. *)
    | Seq.Nil -> f [||] buffer
    (* From the first block, we construct a chain of binded promises. *)
    | Seq.Cons ((i, block), seq) ->
        let blocks = Array.make (Array.length raw_blocks) (string "") in
        let rec aux seq =
          match seq () with
          | Seq.Cons ((i, block), seq) ->
              let$ block_buffer = ("block_buffer", buffer_create ()) in
              let s1 = nodes runtime block_buffer props block in
              let s2 =
                return
                  (bind
                     (runtime.buffer_contents @@ block_buffer)
                     (lambda (fun block ->
                          blocks.(i) <- block;
                          aux seq)))
              in
              s1 |: s2
          | Seq.Nil ->
              let$ buffer = ("buffer", buffer_create ()) in
              let s1 = f blocks buffer in
              let s2 = return (runtime.buffer_contents @@ buffer) in
              s1 |: s2
        in
        let$ block_buffer = ("block_buffer", buffer_create ()) in
        let s1 = nodes runtime block_buffer props block in
        let s2 =
          buffer_append buffer
            (bind
               (runtime.buffer_contents @@ block_buffer)
               (lambda (fun block ->
                    blocks.(i) <- block;
                    aux seq)))
        in
        s1 |: s2

  and nodes runtime buffer props l =
    List.to_seq l |> Seq.map (node runtime buffer props) |> join_stmts

  type decode_runtime = {
    stack : string stack exp;
    errors : string stack exp;
    decode_error : (string -> external_data -> string) exp;
    key_error : (string -> string stack -> string) exp;
  }

  let show_type =
    let b = Buffer.create 128 in
    let ppf = Format.formatter_of_buffer b in
    fun ty ->
      T.pp ppf ty;
      Format.pp_print_flush ppf ();
      let s = Buffer.contents b in
      Buffer.clear b; string s

  let push_error debug ty_str input =
    stack_push debug.errors ((debug.decode_error @@ ty_str) @@ input)

  type 'a union_helper = {
    equal : 'a exp -> 'a exp -> bool exp;
    to_data : 'a exp -> Data.t;
    of_data : Data.t -> 'a exp;
    to_extern : 'a exp -> external_data exp;
    classify : 'a External.classify;
  }

  let union_helper_string =
    {
      equal = equal_string;
      to_data = Data.string;
      of_data = Data.to_string;
      to_extern = External.of_string;
      classify = String;
    }

  let union_helper_int =
    {
      equal = equal_int;
      to_data = Data.int;
      of_data = Data.to_int;
      to_extern = External.of_int;
      classify = Int;
    }

  let union_helper_bool = { union_helper_int with to_extern = External.of_bool }

  let rec decode ~set ~debug input ty =
    let$ ty_str = ("type", show_type ty) in
    match !ty with
    | T.Unknown _ -> set (Data.unknown input)
    | T.Enum_int ({ cases; _ }, Bool) ->
        External.classify Bool input
          ~ok:(fun b ->
            if_else b
              ~then_:(fun () ->
                if Set.Int.mem 1 cases then set (Data.int (int 1))
                else push_error debug ty_str input)
              ~else_:(fun () ->
                if Set.Int.mem 0 cases then set (Data.int (int 0))
                else push_error debug ty_str input))
          ~error:(fun () -> push_error debug ty_str input)
    | T.String | T.Enum_string { row = `Open; _ } ->
        External.classify String input
          ~ok:(fun s -> set (Data.string s))
          ~error:(fun () -> push_error debug ty_str input)
    | T.Enum_string { row = `Closed; cases } ->
        External.classify String input
          ~ok:(fun s ->
            let rec aux seq =
              match seq () with
              | Seq.Nil -> push_error debug ty_str input
              | Seq.Cons (case, seq) ->
                  if_else
                    (equal_string s (string case))
                    ~then_:(fun () -> set (Data.string s))
                    ~else_:(fun () -> aux seq)
            in
            aux (Set.String.to_seq cases))
          ~error:(fun () -> push_error debug ty_str input)
    | T.Int | T.Enum_int ({ row = `Open; _ }, _) ->
        External.classify Int input
          ~ok:(fun i -> set (Data.int i))
          ~error:(fun () -> push_error debug ty_str input)
    | T.Enum_int ({ row = `Closed; cases }, _) ->
        External.classify Int input
          ~ok:(fun s ->
            let rec aux seq =
              match seq () with
              | Seq.Nil -> push_error debug ty_str input
              | Seq.Cons (case, seq) ->
                  if_else
                    (equal_int s (int case))
                    ~then_:(fun () -> set (Data.int s))
                    ~else_:(fun () -> aux seq)
            in
            aux (Set.Int.to_seq cases))
          ~error:(fun () -> push_error debug ty_str input)
    | T.Float ->
        External.classify Float input
          ~ok:(fun f -> set (Data.float f))
          ~error:(fun () ->
            External.classify Int input
              ~ok:(fun i -> set (Data.float (int_to_float i)))
              ~error:(fun () -> push_error debug ty_str input))
    | T.Nullable ty ->
        if_else (External.is_null input)
          ~then_:(fun () -> set nil_value)
          ~else_:(fun () ->
            let$ decoded = ("decoded", array [| nil_value |]) in
            let s1 = stack_push debug.stack (string "<nullable>") in
            let s2 =
              decode ~set:(fun data -> decoded.%(int 0) <- data) ~debug input ty
            in
            let s3 = set (Data.array decoded) in
            let s4 = stack_drop debug.stack in
            s1 |: s2 |: s3 |: s4)
    | T.List ty ->
        External.classify Linear input
          ~ok:(fun l ->
            let$ decoded = ("decoded", array [| nil_value; nil_value |]) in
            let& decode_dst = ("decode_dst", decoded) in
            let s1 =
              External.Linear.iteri l (fun i input ->
                  let$ decode_dst_new =
                    ("decode_dst_new", array [| nil_value; nil_value |])
                  in
                  let s1 = stack_push debug.stack (int_to_string i) in
                  let s2 =
                    decode
                      ~set:(fun data -> decode_dst_new.%(int 0) <- data)
                      ~debug input ty
                  in
                  let s3 =
                    (deref decode_dst).%(int 1) <- Data.array decode_dst_new
                  in
                  let s4 = decode_dst := decode_dst_new in
                  let s5 = stack_drop debug.stack in
                  s1 |: s2 |: s3 |: s4 |: s5)
            in
            let s2 = set (list_tl decoded) in
            s1 |: s2)
          ~error:(fun () -> push_error debug ty_str input)
    | T.Tuple tys ->
        let length = int (List.length tys) in
        External.classify Linear input
          ~ok:(fun l ->
            if_else
              (equal_int (External.Linear.length l) length)
              ~then_:(fun () ->
                let$ decoded = ("decoded", array_init length nil_value) in
                External.Linear.iteri l (fun i input ->
                    let rec aux i' = function
                      | [] -> push_error debug ty_str input
                      | ty :: tl ->
                          if_else
                            (equal_int i (int i'))
                            ~then_:(fun () ->
                              decode
                                ~set:(fun data -> decoded.%(i) <- data)
                                ~debug input ty)
                            ~else_:(fun () -> aux (succ i') tl)
                    in
                    let s1 = stack_push debug.stack (int_to_string i) in
                    let s2 = aux 0 tys in
                    let s3 = set (Data.array decoded) in
                    let s4 = stack_drop debug.stack in
                    s1 |: s2 |: s3 |: s4))
              ~else_:(fun () -> push_error debug ty_str input))
          ~error:(fun () -> push_error debug ty_str input)
    | T.Record tys ->
        External.classify Assoc input
          ~ok:(fun input' ->
            let$ decoded = ("decoded", hashtbl_create ()) in
            let s1 = decode_record_aux ~debug decoded input' !tys ty_str in
            let s2 = set (Data.hashtbl decoded) in
            s1 |: s2)
          ~error:(fun () -> push_error debug ty_str input)
    | T.Dict (ty, _) ->
        External.classify Assoc input
          ~ok:(fun a ->
            let$ decoded = ("decoded", hashtbl_create ()) in
            External.Assoc.iter a (fun k input ->
                let s1 = stack_push debug.stack k in
                let s2 =
                  decode ~set:(fun data -> decoded.%{k} <- data) ~debug input ty
                in
                let s3 = set (Data.hashtbl decoded) in
                let s4 = stack_drop debug.stack in
                s1 |: s2 |: s3 |: s4))
          ~error:(fun () -> push_error debug ty_str input)
    | T.Union_int (key, { cases; row = _ }, Bool) ->
        let key = string key in
        External.classify Assoc input
          ~ok:(fun input' ->
            let aux i v () =
              match Map.Int.find_opt i cases with
              | Some tys ->
                  let$ decoded = ("decoded", hashtbl_create ()) in
                  let s1 = decoded.%{key} <- v in
                  let s2 =
                    decode_record_aux ~debug decoded input' !tys ty_str
                  in
                  let s3 = set (Data.hashtbl decoded) in
                  s1 |: s2 |: s3
              | None -> push_error debug ty_str input
            in
            if_else
              (External.Assoc.mem input' key)
              ~then_:(fun () ->
                External.classify Bool
                  (External.Assoc.find input' key)
                  ~ok:(fun b ->
                    if_else b ~then_:(aux 1 true_value)
                      ~else_:(aux 0 false_value))
                  ~error:(fun () -> push_error debug ty_str input))
              ~else_:(fun () -> push_error debug ty_str input))
          ~error:(fun () -> push_error debug ty_str input)
    | T.Union_int (key, { cases; row }, Not_bool) ->
        decode_union union_helper_int
          (Map.Int.to_seq cases |> Seq.map (fun (k, v) -> (int k, v)))
          ~set ~debug key input row ty_str
    | T.Union_string (key, { cases; row }) ->
        decode_union union_helper_string
          (Map.String.to_seq cases |> Seq.map (fun (k, v) -> (string k, v)))
          ~set ~debug key input row ty_str

  and decode_union : 'a. 'a union_helper -> ('a exp * T.record) Seq.t -> _ =
   fun { equal; to_data; classify; _ } seq ~set ~debug key input row ty_str ->
    let key' = string key in
    External.classify Assoc input
      ~ok:(fun input' ->
        if_else
          (External.Assoc.mem input' key')
          ~then_:(fun () ->
            External.classify classify
              (External.Assoc.find input' key')
              ~ok:(fun i ->
                let rec aux seq =
                  match seq () with
                  | Seq.Nil -> (
                      match row with
                      | `Open ->
                          let$ decoded = ("decoded", hashtbl_create ()) in
                          let s1 = decoded.%{key'} <- to_data i in
                          let s2 = set (Data.hashtbl decoded) in
                          s1 |: s2
                      | `Closed -> push_error debug ty_str input)
                  | Seq.Cons ((ty_i, tys), seq) ->
                      if_else (equal i ty_i)
                        ~then_:(fun () ->
                          let$ decoded = ("decoded", hashtbl_create ()) in
                          let s1 = decoded.%{key'} <- to_data i in
                          let s2 =
                            decode_record_aux ~debug decoded input' !tys ty_str
                          in
                          let s3 = set (Data.hashtbl decoded) in
                          s1 |: s2 |: s3)
                        ~else_:(fun () -> aux seq)
                in
                aux seq)
              ~error:(fun () -> push_error debug ty_str input))
          ~else_:(fun () -> push_error debug ty_str input))
      ~error:(fun () -> push_error debug ty_str input)

  and decode_record_aux ~debug decoded input tys ty_str =
    let$ missing_keys = ("missing_keys", stack_create ()) in
    let s1 =
      Map.String.to_seq tys
      |> Seq.map (fun (k, ty) ->
             let k' = string k in
             if_else
               (External.Assoc.mem input k')
               ~then_:(fun () ->
                 let$ input = ("input", External.Assoc.find input k') in
                 let s1 = stack_push debug.stack k' in
                 let s2 =
                   decode
                     ~set:(fun data -> decoded.%{k'} <- data)
                     ~debug input ty
                 in
                 let s3 = stack_drop debug.stack in
                 s1 |: s2 |: s3)
               ~else_:(fun () ->
                 match !ty with
                 | Nullable _ | Unknown _ -> decoded.%{k'} <- nil_value
                 | _ -> stack_push missing_keys k'))
      |> join_stmts
    in
    let s2 =
      if_
        (not (stack_is_empty missing_keys))
        ~then_:(fun () ->
          stack_push debug.errors ((debug.key_error @@ ty_str) @@ missing_keys))
    in
    s1 |: s2

  let rec encode ~set props ty =
    match !ty with
    | T.Unknown _ -> set (External.of_untyped props)
    | T.Enum_int (_, Bool) -> set (External.of_bool (Data.to_int props))
    | T.String | T.Enum_string _ ->
        set (External.of_string (Data.to_string props))
    | T.Int | T.Enum_int _ -> set (External.of_int (Data.to_int props))
    | T.Float -> set (External.of_float (Data.to_float props))
    | T.Nullable ty ->
        if_else (is_nil props)
          ~then_:(fun () -> set External.null)
          ~else_:(fun () ->
            let$ props = ("props", get_nullable props) in
            encode ~set:(fun x -> set (External.some x)) props ty)
    | T.List ty ->
        let& index = ("index", int 0) in
        let& cell = ("cell", props) in
        let s1 =
          while_
            (fun () -> not (is_nil (deref cell)))
            (fun () ->
              let s1 = incr index in
              let s2 = cell := list_tl (Data.to_array (deref cell)) in
              s1 |: s2)
        in
        let s2 =
          let$ encoded = ("encoded", array_init (deref index) External.null) in
          let s1 = cell := props in
          let s2 = index := int 0 in
          let s3 =
            while_
              (fun () -> not (is_nil (deref cell)))
              (fun () ->
                let$ props =
                  ("props", deref cell |> Data.to_array |> list_hd)
                in
                let s1 =
                  encode ~set:(fun x -> encoded.%(deref index) <- x) props ty
                in
                let s2 = incr index in
                let s3 = cell := list_tl (Data.to_array (deref cell)) in
                s1 |: s2 |: s3)
          in
          let s4 = set (External.of_array encoded) in
          s1 |: s2 |: s3 |: s4
        in
        s1 |: s2
    | T.Tuple tys ->
        let$ props = ("props", Data.to_array props) in
        let$ encoded =
          ("encoded", array_init (int (List.length tys)) External.null)
        in
        let s1 =
          List.to_seq tys
          |> Seq.mapi (fun i ty ->
                 let i = int i in
                 let$ props = ("props", props.%(i)) in
                 encode ~set:(fun x -> encoded.%(i) <- x) props ty)
          |> join_stmts
        in
        let s2 = set (External.of_array encoded) in
        s1 |: s2
    | T.Record tys ->
        let$ encoded = ("encoded", hashtbl_create ()) in
        let s1 = encode_record_aux encoded (Data.to_hashtbl props) !tys in
        let s2 = set (External.of_hashtbl encoded) in
        s1 |: s2
    | T.Dict (ty, _) ->
        let$ encoded = ("encoded", hashtbl_create ()) in
        let s1 =
          hashtbl_iter (Data.to_hashtbl props) (fun k props ->
              encode ~set:(fun x -> encoded.%{k} <- x) props ty)
        in
        let s2 = set (External.of_hashtbl encoded) in
        s1 |: s2
    | T.Union_int (key, { cases; row }, Bool) ->
        encode_union union_helper_bool
          (Map.Int.to_seq cases |> Seq.map (fun (k, v) -> (int k, v)))
          row ~set key props
    | T.Union_int (key, { cases; row }, Not_bool) ->
        encode_union union_helper_int
          (Map.Int.to_seq cases |> Seq.map (fun (k, v) -> (int k, v)))
          row ~set key props
    | T.Union_string (key, { cases; row }) ->
        encode_union union_helper_string
          (Map.String.to_seq cases |> Seq.map (fun (k, v) -> (string k, v)))
          row ~set key props

  and encode_union : 'a. 'a union_helper -> ('a exp * T.record) Seq.t -> _ =
   fun { equal; of_data; to_extern; _ } cases row ~set key props ->
    let key = string key in
    let$ props = ("props", Data.to_hashtbl props) in
    let$ tag = ("tag", props.%{key} |> of_data) in
    let rec aux (tag', tys) seq =
      if_else (equal tag tag')
        ~then_:(fun () ->
          let$ encoded = ("encoded", hashtbl_create ()) in
          let s1 = encoded.%{key} <- to_extern tag in
          let s2 = encode_record_aux encoded props !tys in
          let s3 = set (External.of_hashtbl encoded) in
          s1 |: s2 |: s3)
        ~else_:
          (match seq () with
          | Seq.Cons (hd, seq) -> fun () -> aux hd seq
          | Seq.Nil -> (
              match row with
              | `Closed -> fun () -> unit
              | `Open ->
                  fun () ->
                    let$ encoded = ("encoded", hashtbl_create ()) in
                    let s1 = encoded.%{key} <- to_extern tag in
                    let s2 = set (External.of_hashtbl encoded) in
                    s1 |: s2))
    in
    match cases () with Seq.Nil -> unit | Seq.Cons (hd, seq) -> aux hd seq

  and encode_record_aux encoded props tys =
    Map.String.to_seq tys
    |> Seq.map (fun (k, ty) ->
           let k = string k in
           let$ props = ("props", props.%{k}) in
           encode ~set:(fun x -> encoded.%{k} <- x) props ty)
    |> join_stmts

  let eval compiled =
    let$ escape = ("acutis_escape", escape) in
    let$ buffer_contents = ("buffer_contents", buffer_contents) in
    let$ comps = ("components", hashtbl_create ()) in
    let runtime = { escape; comps; buffer_contents } in
    let s1 =
      Map.String.to_seq compiled.C.externals
      |> Seq.map (fun (k, (tys, v)) ->
             import v (fun import ->
                 comps.%{string k} <-
                   lambda (fun props ->
                       let$ encoded = ("encoded", hashtbl_create ()) in
                       let s1 = encode_record_aux encoded props tys in
                       let s2 =
                         return (import @@ External.of_hashtbl encoded)
                       in
                       s1 |: s2)))
      |> join_stmts
    in
    let s2 =
      Map.String.to_seq compiled.components
      |> Seq.map (fun (k, v) ->
             comps.%{string k} <-
               lambda (fun props ->
                   let$ buffer = ("buffer", buffer_create ()) in
                   let s1 = nodes runtime buffer props v in
                   let s2 = return (buffer_contents @@ buffer) in
                   s1 |: s2))
      |> join_stmts
    in
    let s3 =
      export
        (lambda (fun input ->
             let$ stack = ("stack", stack_create ()) in
             let$ errors = ("errors", stack_create ()) in
             let$ decode_error =
               ( "decode_error",
                 lambda (fun ty ->
                     return
                       (lambda (fun input ->
                            return
                              (array_concat
                                 (array
                                    (Error.decode ~fname:compiled.name
                                       ~stack:
                                         (stack_concat stack (string " <- "))
                                       ~ty ~input:(External.show input) string))
                                 (string ""))))) )
             in
             let$ key_error =
               ( "key_error",
                 lambda (fun ty ->
                     return
                       (lambda (fun keys ->
                            return
                              (array_concat
                                 (array
                                    (Error.missing_keys ~fname:compiled.name
                                       ~stack:
                                         (stack_concat stack (string " <- "))
                                       ~ty
                                       ~keys:(stack_concat keys (string ", "))
                                       string))
                                 (string ""))))) )
             in
             let debug = { errors; stack; decode_error; key_error } in
             let$ props = ("props", hashtbl_create ()) in
             let s1 = stack_push stack (string "<input>") in
             let s2 =
               let$ ty_str =
                 ("type", show_type (T.record (ref compiled.types)))
               in
               External.classify Assoc input
                 ~ok:(fun input ->
                   decode_record_aux ~debug props input compiled.types ty_str)
                 ~error:(fun () ->
                   stack_push errors ((decode_error @@ ty_str) @@ input))
             in
             let s3 =
               if_else (stack_is_empty errors)
                 ~then_:(fun () ->
                   let$ buffer = ("buffer", buffer_create ()) in
                   let s1 = nodes runtime buffer props compiled.nodes in
                   let s2 = return (buffer_contents @@ buffer) in
                   s1 |: s2)
                 ~else_:(fun () ->
                   return (error (stack_concat errors (string "\n\n"))))
             in
             s1 |: s2 |: s3))
    in
    s1 |: s2 |: s3

  let eval compiled = observe (eval compiled)
end

module type TRANS = sig
  (** To transform the language's output, such as for an optimization, we need to
    define a module that can translate terms "forward" to the transformed state
    and "backward" to the original representation. *)

  type 'a from_exp
  type 'a exp

  val fwde : 'a from_exp -> 'a exp
  val bwde : 'a exp -> 'a from_exp

  type 'a from_stmt
  type 'a stmt

  val fwds : 'a from_stmt -> 'a stmt
  val bwds : 'a stmt -> 'a from_stmt
end

(** Apply a transformation module and a semantics module to produce a new
    semantics module that uses the transformation state.

    The module this creates won't do any transformations by itself, and its
    values are defined as identity functions. You need to override specific
    functions to apply transformations. This is used in {!PrintJs} to optimize
    its output. *)
module MakeTrans
    (T : TRANS)
    (F : SEM with type 'a exp = 'a T.from_exp and type 'a stmt = 'a T.from_stmt) :
  SEM
    with type 'a stmt = 'a T.stmt
     and type 'a obs = 'a F.obs
     and type 'a exp = 'a T.exp
     and type 'a mut = 'a F.mut
     and type 'a promise = 'a F.promise
     and type external_data = F.external_data
     and type import = F.import
     and type 'a hashtbl = 'a F.hashtbl
     and type buffer = F.buffer
     and type 'a stack = 'a F.stack
     and type data = F.data
     and type 'a External.Linear.t = 'a F.External.Linear.t
     and type 'a External.Assoc.t = 'a F.External.Assoc.t
     and type 'a External.classify = 'a F.External.classify = struct
  open T
  include F

  type 'a exp = 'a T.exp
  type 'a stmt = 'a T.stmt

  let observe x = F.observe (bwds x)
  let ( |: ) a b = fwds F.(bwds a |: bwds b)
  let return x = fwds (F.return (bwde x))

  let ( let$ ) (s, x) f =
    fwds (F.( let$ ) (s, bwde x) (fun x -> bwds (f (fwde x))))

  let ( let& ) (s, x) f = fwds (F.( let& ) (s, bwde x) (fun x -> bwds (f x)))
  let deref x = fwde (F.deref x)
  let ( := ) a x = fwds F.(a := bwde x)
  let incr x = fwds (F.incr x)
  let lambda f = fwde (F.lambda (fun x -> bwds (f (fwde x))))
  let ( @@ ) f x = fwde F.(bwde f @@ bwde x)
  let if_ b ~then_ = fwds (F.if_ (bwde b) ~then_:(fun () -> bwds (then_ ())))

  let if_else b ~then_ ~else_ =
    fwds
      (F.if_else (bwde b)
         ~then_:(fun () -> bwds (then_ ()))
         ~else_:(fun () -> bwds (else_ ())))

  let while_ b f =
    fwds (F.while_ (fun () -> bwde (b ())) (fun () -> bwds (f ())))

  let import i f = fwds (F.import i (fun fi -> bwds (f (fwde fi))))
  let export x = fwds (F.export (bwde x))
  let unit = fwds F.unit
  let not x = fwde (F.not (bwde x))
  let int x = fwde (F.int x)
  let float x = fwde (F.float x)
  let string x = fwde (F.string x)
  let bool x = fwde (F.bool x)
  let pair (a, b) = fwde (F.pair (bwde a, bwde b))
  let equal_int a b = fwde (F.equal_int (bwde a) (bwde b))
  let equal_string a b = fwde (F.equal_string (bwde a) (bwde b))
  let int_to_string x = fwde (F.int_to_string (bwde x))
  let int_to_float x = fwde (F.int_to_float (bwde x))
  let float_to_string x = fwde (F.float_to_string (bwde x))
  let bool_to_string x = fwde (F.bool_to_string (bwde x))
  let array x = fwde (F.array (Array.map bwde x))
  let array_init i x = fwde (F.array_init (bwde i) (bwde x))
  let ( .%() ) a i = fwde F.((bwde a).%(bwde i))
  let ( .%()<- ) a i x = fwds F.((bwde a).%(bwde i) <- bwde x)
  let array_concat a s = fwde (F.array_concat (bwde a) (bwde s))
  let hashtbl x = fwde (F.hashtbl (Seq.map bwde x))
  let hashtbl_create () = fwde (F.hashtbl_create ())
  let ( .%{} ) h k = fwde F.((bwde h).%{bwde k})
  let ( .%{}<- ) h k x = fwds F.((bwde h).%{bwde k} <- bwde x)
  let hashtbl_mem h k = fwde (F.hashtbl_mem (bwde h) (bwde k))
  let hashtbl_copy h = fwde (F.hashtbl_copy (bwde h))

  let hashtbl_iter h f =
    fwds (F.hashtbl_iter (bwde h) (fun k v -> bwds (f (fwde k) (fwde v))))

  let promise x = fwde (F.promise (bwde x))
  let bind a f = fwde (F.bind (bwde a) (bwde f))
  let error s = fwde (F.error (bwde s))
  let buffer_create () = fwde (F.buffer_create ())
  let buffer_append b s = fwds (F.buffer_append (bwde b) (bwde s))
  let buffer_contents = fwde F.buffer_contents
  let escape = fwde F.escape
  let stack_create () = fwde (F.stack_create ())
  let stack_is_empty s = fwde (F.stack_is_empty (bwde s))
  let stack_push s x = fwds (F.stack_push (bwde s) (bwde x))
  let stack_drop s = fwds (F.stack_drop (bwde s))
  let stack_concat s x = fwde (F.stack_concat (bwde s) (bwde x))

  module Data = struct
    type t = data exp

    let int x = fwde (F.Data.int (bwde x))
    let float x = fwde (F.Data.float (bwde x))
    let string x = fwde (F.Data.string (bwde x))
    let array x = fwde (F.Data.array (bwde x))
    let hashtbl x = fwde (F.Data.hashtbl (bwde x))
    let unknown x = fwde (F.Data.unknown (bwde x))
    let to_int x = fwde (F.Data.to_int (bwde x))
    let to_float x = fwde (F.Data.to_float (bwde x))
    let to_string x = fwde (F.Data.to_string (bwde x))
    let to_array x = fwde (F.Data.to_array (bwde x))
    let to_hashtbl x = fwde (F.Data.to_hashtbl (bwde x))
    let equal a b = fwde (F.Data.equal (bwde a) (bwde b))
  end

  module External = struct
    include F.External

    module Linear = struct
      include F.External.Linear

      let length t = fwde (F.External.Linear.length (bwde t))

      let iteri t f =
        fwds
          (F.External.Linear.iteri (bwde t) (fun k v ->
               bwds (f (fwde k) (fwde v))))
    end

    module Assoc = struct
      include F.External.Assoc

      let find t s = fwde (F.External.Assoc.find (bwde t) (bwde s))
      let mem t s = fwde (F.External.Assoc.mem (bwde t) (bwde s))

      let iter t f =
        fwds
          (F.External.Assoc.iter (bwde t) (fun k v ->
               bwds (f (fwde k) (fwde v))))
    end

    type t = external_data exp

    let null = fwde F.External.null
    let some x = fwde (F.External.some (bwde x))
    let of_int x = fwde (F.External.of_int (bwde x))
    let of_bool x = fwde (F.External.of_bool (bwde x))
    let of_float x = fwde (F.External.of_float (bwde x))
    let of_string x = fwde (F.External.of_string (bwde x))
    let of_array x = fwde (F.External.of_array (bwde x))
    let of_hashtbl x = fwde (F.External.of_hashtbl (bwde x))
    let of_untyped x = fwde (F.External.of_untyped (bwde x))

    let classify c x ~ok ~error =
      fwds
        (F.External.classify c (bwde x)
           ~ok:(fun x -> bwds (ok (fwde x)))
           ~error:(fun () -> bwds (error ())))

    let is_null x = fwde (F.External.is_null (bwde x))
    let show x = fwde (F.External.show (bwde x))
  end
end

(** Pretty-print the instructions for a compiled template. *)
let pp (type a) pp_import ppf c =
  let module F = Format in
  let module M = Make (struct
    module Tbl = Hashtbl.Make (String)

    let var =
      let tbl = Tbl.create 128 in
      fun v ->
        let i = try Tbl.find tbl v with Not_found -> 0 in
        Tbl.add tbl v (succ i);
        F.dprintf "%s/%i" v i

    type 'a stmt = F.formatter -> unit
    type 'a obs = F.formatter -> unit

    let observe = Fun.id
    let ( |: ) = F.dprintf "%t@ %t"

    type 'a exp = F.formatter -> unit

    let return = F.dprintf "(@[return@ %t@])"

    let ( let$ ) (v, e) f =
      let v = var v in
      F.dprintf "(@[@[let$@ %t@ =@]@ %t@])@ %t" v e (f v)

    type 'a mut = F.formatter -> unit

    let ( let& ) (v, e) f =
      let v = var v in
      F.dprintf "(@[@[let&@ %t@ =@]@ %t@])@ %t" v e (f v)

    let deref = F.dprintf "(@[deref@ %t@])"
    let ( := ) = F.dprintf "(@[%t@ :=@ %t@])"
    let incr = F.dprintf "(@[incr@ %t@])"

    let lambda f =
      let arg = var "arg" in
      F.dprintf "(@[lambda@ %t@ (@[<hv>%t@])@])" arg (f arg)

    let ( @@ ) = F.dprintf "(@[%t@ %@%@ %t@])"

    let if_ b ~then_ =
      F.dprintf "(@[<hv>@[if@ %t@]@ (@[<hv>then@ %t@])@])" b (then_ ())

    let if_else b ~then_ ~else_ =
      F.dprintf
        "(@[<hv>@[if_else@ %t@]@ (@[<hv>then@ %t@])@ (@[<hv>else@ %t@])@])" b
        (then_ ()) (else_ ())

    let while_ b x = F.dprintf "(@[while@ %t@ (@[<hv>%t@])@])" (b ()) (x ())

    type external_data
    type 'a promise
    type import = a

    let import i f =
      let name = var "import" in
      F.dprintf "(@[import@ %t@ from@ %a@])@ %t" name pp_import i (f name)

    let export = F.dprintf "(@[export@ %t@])"
    let unit = F.dprintf "(unit)"
    let not = F.dprintf "(@[not@ %t@])"
    let int = F.dprintf "%i"
    let float = F.dprintf "%g"
    let string = F.dprintf "%S"
    let bool = F.dprintf "%B"
    let pair (a, b) = F.dprintf "(@[%t,@ %t@])" a b
    let equal_int = F.dprintf "(@[equal_int@ %t@ %t@])"
    let equal_string = F.dprintf "(@[equal_string@ %t@ %t@])"
    let int_to_string = F.dprintf "(@[int_to_string@ %t@])"
    let int_to_float = F.dprintf "(@[int_to_float@ %t@])"
    let float_to_string = F.dprintf "(@[float_to_string@ %t@])"
    let bool_to_string = F.dprintf "(@[bool_to_string@ %t@])"

    let array a =
      F.dprintf "[@[<hv>%a@]]"
        (F.pp_print_seq ~pp_sep:Pp.comma ( |> ))
        (Array.to_seq a)

    let array_init = F.dprintf "(@[array_init@ %t@ %t@])"
    let bindop_get = F.dprintf "(@[%t@,.%%%c@[@,%t@,@]%c@])"
    let bindop_set = F.dprintf "(@[%t@,.%%%c@[@,%t@,@]%c@ <-@ %t@])"
    let ( .%() ) a i = bindop_get a '(' i ')'
    let ( .%()<- ) a i v = bindop_set a '(' i ')' v
    let array_concat = F.dprintf "(@[array_concat@ %t@ %t@])"

    type 'a hashtbl

    let hashtbl =
      F.dprintf "(@[hashtbl@ [@[<hv>%a@]]@])"
        (F.pp_print_seq ~pp_sep:Pp.comma ( |> ))

    let hashtbl_create () = F.dprintf "(hashtbl_create)"
    let ( .%{} ) t k = bindop_get t '{' k '}'
    let ( .%{}<- ) t k v = bindop_set t '{' k '}' v
    let hashtbl_mem = F.dprintf "(@[hashtbl_mem@ %t@ %t@])"
    let hashtbl_copy = F.dprintf "(@[hashtbl_copy@ %t @])"

    let hashtbl_iter t f =
      let arg_k = var "key" in
      let arg_v = var "value" in
      F.dprintf "(@[hashtbl_iter@ %t@ %t@ %t@ %t@])" t arg_k arg_v
        (f arg_k arg_v)

    let promise = F.dprintf "(@[promise@ %t@])"
    let bind = F.dprintf "(@[bind@ %t@ %t@])"
    let error = F.dprintf "(@[error@ %t@])"

    type buffer

    let buffer_create () = F.dprintf "(buffer_create)"
    let buffer_append = F.dprintf "(@[buffer_append@ %t@ %t@])"
    let buffer_contents = F.dprintf "(buffer_contents)"
    let escape = F.dprintf "(escape)"

    type 'a stack

    let stack_create () = F.dprintf "(stack_create)"
    let stack_is_empty = F.dprintf "(@[stack_is_empty@ %t@])"
    let stack_push = F.dprintf "(@[stack_push@ %t@ %t@])"
    let stack_drop = F.dprintf "(@[stack_drop@ %t@])"
    let stack_concat = F.dprintf "(@[stack_concat@ %t@ %t@])"

    type data

    module Data = struct
      type t = data exp

      let int = F.dprintf "(@[Data.int@ %t@])"
      let float = F.dprintf "(@[Data.float@ %t@])"
      let string = F.dprintf "(@[Data.string@ %t@])"
      let array = F.dprintf "(@[Data.array@ %t@])"
      let hashtbl = F.dprintf "(@[Data.hashtbl@ %t@])"
      let unknown = F.dprintf "(@[Data.unknown@ %t@])"
      let to_int = F.dprintf "(@[Data.to_int@ %t@])"
      let to_float = F.dprintf "(@[Data.to_float@ %t@])"
      let to_string = F.dprintf "(@[Data.to_string@ %t@])"
      let to_array = F.dprintf "(@[Data.to_array@ %t@])"
      let to_hashtbl = F.dprintf "(@[Data.to_hashtbl@ %t@])"
      let equal = F.dprintf "(@[Data.equal@ %t@ %t@])"
    end

    module External = struct
      module Linear = struct
        type 'a t = F.formatter -> unit

        let length = F.dprintf "(@[External.Linear.length@ %t@])"

        let iteri a f =
          let arg_k = var "key" in
          let arg_v = var "value" in
          F.dprintf "(@[External.Linear.iteri@ %t@ %t@ %t@ %t@])" a arg_k arg_v
            (f arg_k arg_v)
      end

      module Assoc = struct
        type 'a t = F.formatter -> unit

        let find = F.dprintf "(@[External.Assoc.find@ %t@ %t@])"
        let mem = F.dprintf "(@[External.Assoc.mem@ %t@ %t@])"

        let iter a f =
          let arg_k = var "key" in
          let arg_v = var "value" in
          F.dprintf "(@[External.Assoc.iter@ %t@ %t@ %t@ %t@])" a arg_k arg_v
            (f arg_k arg_v)
      end

      type t = external_data exp

      let null = F.dprintf "null"
      let some = F.dprintf "(@[External.some@ %t@])"
      let of_bool = F.dprintf "(@[External.of_bool@ %t@])"
      let of_int = F.dprintf "(@[External.of_int@ %t@])"
      let of_string = F.dprintf "(@[External.of_string@ %t@])"
      let of_float = F.dprintf "(@[External.of_float@ %t@])"
      let of_array = F.dprintf "(@[External.of_array@ %t@])"
      let of_hashtbl = F.dprintf "(@[External.of_hashtbl@ %t@])"
      let of_untyped = F.dprintf "(@[External.of_untyped@ %t@])"

      type _ classify =
        | Int : int classify
        | String : string classify
        | Float : float classify
        | Bool : bool classify
        | Linear : external_data Linear.t classify
        | Assoc : external_data Assoc.t classify

      let classify_to_string : type a. a classify -> string = function
        | Int -> "(int)"
        | String -> "(string)"
        | Float -> "(float)"
        | Bool -> "(bool)"
        | Linear -> "(linear)"
        | Assoc -> "(assoc)"

      let classify c t ~ok ~error =
        let arg = var "classified" in
        F.dprintf
          "(@[External.classify@ %s@ %t@ %t@ (@[<hv>ok@ %t@])@ (@[<hv>error@ \
           %t@])@])"
          (classify_to_string c) t arg (ok arg) (error ())

      let is_null = F.dprintf "(@[External.is_null@ %t@])"
      let show = F.dprintf "(@[External.show@ %t@])"
    end
  end) in
  F.fprintf ppf "@[<hv>%t@]" (M.eval c)
