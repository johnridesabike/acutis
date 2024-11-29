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

  type 'a exp
  (** An expression. *)

  val return : 'a exp -> 'a stmt
  (** Return value ['a] from a function. *)

  val stmt : 'a exp -> 'a stmt

  type 'a ref
  (** A mutable reference variable. This is not compatible with expressions. *)

  val ( let| ) : unit stmt -> (unit -> 'a stmt) -> 'a stmt
  (** Evaluate a unit statement. *)

  val ( let$ ) : string * 'a exp -> ('a exp -> 'b stmt) -> 'b stmt
  (** Define a new immutable binding. The string is used for pretty-printing. *)

  val ( let& ) : string * 'a exp -> ('a ref -> 'b stmt) -> 'b stmt
  (** Define a new reference variable. The string is used for pretty-printing. *)

  val ( ! ) : 'a ref -> 'a exp
  val ( := ) : 'a ref -> 'a exp -> unit stmt
  val incr : int ref -> unit stmt

  (** {1 Functions.} *)

  val lambda : ('a exp -> 'b stmt) -> ('a -> 'b) exp
  val ( @@ ) : ('a -> 'b) exp -> 'a exp -> 'b exp

  (** {1 Control flow.} *)

  val if_else :
    bool exp -> then_:(unit -> 'a stmt) -> else_:(unit -> 'a stmt) -> 'a stmt

  val while_ :
    ('a exp -> bool exp) -> 'a ref -> (unit -> unit stmt) -> unit stmt

  (** {1 Standard values.} *)

  val unit : unit stmt
  val not : bool exp -> bool exp
  val int : int -> int exp
  val float : float -> float exp
  val string : string -> string exp
  val bool : bool -> bool exp
  val ( = ) : 'a exp -> 'a exp -> bool exp
  val pair : 'a exp * 'b exp -> ('a * 'b) exp
  val unpair : ('a * 'b) exp -> 'a exp * 'b exp
  val string_of_int : int exp -> string exp
  val string_of_float : float exp -> string exp
  val string_of_bool : bool exp -> string exp

  (** {1 Sequences.} *)

  val uncons :
    'a Seq.t exp ->
    nil:(unit -> 'b stmt) ->
    cons:('a exp -> 'a Seq.t exp -> 'b stmt) ->
    'b stmt

  val generator : (('a exp -> unit stmt) -> unit stmt) -> 'a Seq.t exp
  val iter : 'a Seq.t exp -> ('a exp -> unit stmt) -> unit stmt

  (** {1 Strings and characters.} *)

  val string_to_seq : string exp -> char Seq.t exp

  val match_char : char exp -> (char -> 'a stmt) -> 'a stmt
  (** The given function applies to the following characters and it ignores all
      others: [ '&' '"' '\'' '>' '<' '/' '`' '=' ]. *)

  (** {1 Arrays.} *)

  val array : 'a exp array -> 'a array exp
  val array_make : int -> 'a exp -> 'a array exp
  val ( .%() ) : 'a array exp -> int exp -> 'a exp
  val ( .%()<- ) : 'a array exp -> int exp -> 'a exp -> unit stmt

  (** {1 Hash tables.} *)

  type 'a hashtbl := 'a Hashtbl.MakeSeeded(String).t

  val hashtbl : (string exp * 'a exp) Seq.t -> 'a hashtbl exp
  val hashtbl_create : unit -> 'a hashtbl exp
  val ( .%{} ) : 'a hashtbl exp -> string exp -> 'a exp
  val ( .%{}<- ) : 'a hashtbl exp -> string exp -> 'a exp -> unit stmt
  val hashtbl_mem : 'a hashtbl exp -> string exp -> bool exp
  val hashtbl_to_seq : 'a hashtbl exp -> (string * 'a) Seq.t exp

  (** {1 Mutable string buffers.} *)

  val buffer_create : unit -> Buffer.t exp
  val buffer_add_string : Buffer.t exp -> string exp -> unit stmt
  val buffer_add_char : Buffer.t exp -> char exp -> unit stmt
  val buffer_contents : Buffer.t exp -> string exp
  val buffer_length : Buffer.t exp -> int exp

  (** {1 Promises.} *)

  type 'a promise
  (** An asynchronous monad. *)

  val promise : 'a exp -> 'a promise exp
  val await : 'a promise exp -> 'a exp
  val error : string exp -> 'a promise exp

  val async_lambda : ('a exp -> 'b promise stmt) -> ('a -> 'b promise) exp
  (** This is necessary for JavaScript async/await syntax compatibility. *)

  (** {1 Data} *)

  module External : sig
    (** Data from the outside world that we need to decode. *)

    type t

    val null : t exp
    val some : t exp -> t exp
    val of_int : int exp -> t exp
    val of_float : float exp -> t exp
    val of_string : string exp -> t exp
    val of_bool : bool exp -> t exp
    val of_seq : t Seq.t exp -> t exp
    val of_seq_assoc : (string * t) Seq.t exp -> t exp

    type 'a assoc
    (** A key-value container such as an association list or a string map. *)

    val assoc_find : string exp -> 'a assoc exp -> 'a exp
    val assoc_mem : string exp -> 'a assoc exp -> bool exp
    val assoc_to_seq : 'a assoc exp -> (string * 'a) Seq.t exp

    type 'a decoder

    val get_int : int decoder
    val get_string : string decoder
    val get_float : float decoder
    val get_bool : bool decoder
    val get_some : t decoder
    val get_seq : t Seq.t decoder
    val get_assoc : t assoc decoder

    val decode :
      'a decoder ->
      t exp ->
      ok:('a exp -> 'b stmt) ->
      error:(unit -> 'b stmt) ->
      'b stmt

    val to_string : t exp -> string exp
  end

  module Data : sig
    (** Runtime data. *)

    type t

    val int : int exp -> t exp
    val float : float exp -> t exp
    val string : string exp -> t exp
    val array : t array exp -> t exp
    val hashtbl : t hashtbl exp -> t exp
    val unknown : External.t exp -> t exp
    val to_int : t exp -> int exp
    val to_float : t exp -> float exp
    val to_string : t exp -> string exp
    val to_array : t exp -> t array exp
    val to_hashtbl : t exp -> t hashtbl exp
    val to_external_untyped : t exp -> External.t exp
    val is_int : t exp -> bool exp
  end

  (** {1 Importing and exporting.} *)

  type import
  (** Information to import a function from external code. *)

  val import :
    import -> ((External.t -> string promise) exp -> 'a stmt) -> 'a stmt

  val export : 'a exp -> 'a stmt
end

(** Create evaluation instructions for a given language implementation. *)
module Make (I : SEM) : sig
  open I

  val eval : import Compile.t -> (External.t -> string promise) obs
  (** Evaluate a template with the language implemented by {!I}. *)
end = struct
  (* NOTE: OCaml's mutability, exceptions, and effects are not guaranteed to
     work with [I]. This is particularly true if [I] is a pretty-printer. Do not
     rely on those features when crossing the boundaries of [I]'s functions. *)
  module MapInt = Map.Make (Int)
  module MapString = Map.Make (String)
  module SetInt = Set.Make (Int)
  module SetString = Set.Make (String)
  open I

  let nil_value = Data.int (int 0)
  let false_value = nil_value
  let true_value = Data.int (int 1)
  let unset_exit = int (-1)
  let list_hd e = e.%(int 0)
  let list_tl e = e.%(int 1)
  let get_nullable e = list_hd (Data.to_array e)
  let is_nil = Data.is_int
  let is_not_nil x = not (is_nil x)
  let int_to_bool i = not (i = int 0)
  let if_ x ~then_ = if_else x ~then_ ~else_:(fun () -> unit)
  let ( let@ ) = Stdlib.( @@ )

  let stmt_join =
    let rec aux s1 seq =
      match seq () with
      | Seq.Nil -> s1
      | Seq.Cons (s2, seq) ->
          let| () = s1 in
          aux s2 seq
    in
    fun seq ->
      match seq () with Seq.Nil -> unit | Seq.Cons (s1, seq) -> aux s1 seq

  type 'a hashtbl = 'a Hashtbl.MakeSeeded(String).t

  type state = {
    components : (Data.t hashtbl -> string promise) exp MapString.t;
    buf : Buffer.t exp;
    escape : (Buffer.t -> string -> unit) exp;
    props : Data.t hashtbl exp;
        (** We need to use a hash table as the root scope because components
            take a hash table as input. *)
    scope : Data.t exp MapString.t;
        (** As new bindings are added, they go into the scope to shadow previous
            props & bindings. *)
  }

  let state_make components ~props ~escape f =
    let$ buf = ("buf", buffer_create ()) in
    f { components; buf; escape; props; scope = MapString.empty }

  let state_create_buffer state f =
    let$ buf = ("buf", buffer_create ()) in
    f { state with buf }

  let props_add_scope hashtbl bindings state =
    let scope =
      List.fold_left
        (fun scope s -> MapString.add s hashtbl.%{string s} scope)
        state.scope bindings
    in
    { state with scope }

  let props_find s { props; scope; _ } =
    try MapString.find s scope with Not_found -> props.%{string s}

  let parse_escape state esc x =
    match esc with
    | Compile.No_escape -> buffer_add_string state.buf x
    | Compile.Escape -> stmt ((state.escape @@ state.buf) @@ x)

  let fmt state esc x = function
    | Compile.Fmt_string -> parse_escape state esc (Data.to_string x)
    | Compile.Fmt_int -> parse_escape state esc (string_of_int (Data.to_int x))
    | Compile.Fmt_float ->
        parse_escape state esc (string_of_float (Data.to_float x))
    | Compile.Fmt_bool ->
        parse_escape state esc (string_of_bool (int_to_bool (Data.to_int x)))

  let rec echo state (ech : Compile.echo) =
    match ech with
    | `Var s -> props_find s state
    | `String s -> Data.string (string s)
    | `Field (e, s) -> (echo state e |> Data.to_hashtbl).%{string s}

  let rec echoes state esc default default_fmt = function
    | [] -> fmt state esc (echo state default) default_fmt
    | (f, e) :: tl ->
        let$ nullable = ("nullable", echo state e) in
        if_else (is_nil nullable)
          ~then_:(fun () -> echoes state esc default default_fmt tl)
          ~else_:(fun () -> fmt state esc (get_nullable nullable) f)

  let rec construct_data blocks state (data : Compile.data) =
    match data with
    | `Null -> nil_value
    | `Int i -> Data.int (int i)
    | `String s -> Data.string (string s)
    | `Float f -> Data.float (float f)
    | `Var s -> props_find s state
    | `Array a -> construct_data_array blocks state a |> Data.array
    | `Assoc d -> construct_data_hashtbl blocks state d |> Data.hashtbl
    | `Block i -> blocks.(i) |> Data.string
    | `Field (d, s) ->
        (construct_data blocks state d |> Data.to_hashtbl).%{string s}

  and construct_data_array blocks state a =
    array (Array.map (construct_data blocks state) a)

  and construct_data_hashtbl blocks state d =
    MapString.to_seq d
    |> Seq.map (fun (k, v) -> (string k, construct_data blocks state v))
    |> hashtbl

  let add_vars ids arg vars =
    SetInt.fold (fun id vars -> MapInt.add id arg vars) ids vars

  let arg_indexed arg index ~optional:_ i f =
    match i with 0 -> f arg | _ -> f index

  let arg_int arg ~optional:_ i f = ( let$ ) ("match_arg", arg.%(int i)) f

  let arg_str arg ~optional key f =
    if optional then
      if_
        (hashtbl_mem arg (string key))
        ~then_:(fun () -> ( let$ ) ("match_arg", arg.%{string key}) f)
    else ( let$ ) ("match_arg", arg.%{string key}) f

  let rec match_tree :
        'leaf 'key.
        exit:int ref ->
        leafstmt:(vars:Data.t exp MapInt.t -> 'leaf -> unit stmt) ->
        get_arg:
          (optional:bool -> 'key -> (Data.t exp -> unit stmt) -> unit stmt) ->
        vars:Data.t exp MapInt.t ->
        ?optional:bool ->
        ('leaf, 'key) Matching.tree ->
        unit stmt =
   fun ~exit ~leafstmt ~get_arg ~vars ?(optional = false) -> function
    | Matching.Switch { key; ids; cases; wildcard; _ } ->
        let@ arg = get_arg ~optional key in
        let vars = add_vars ids arg vars in
        switchcase ~exit ~leafstmt ~get_arg ~vars ~arg ~wildcard cases
    | Matching.Wildcard { key; ids; child } ->
        let@ arg = get_arg ~optional key in
        let vars = add_vars ids arg vars in
        match_tree ~exit ~leafstmt ~get_arg ~vars child
    | Matching.Nest { key; ids; child; wildcard } -> (
        let@ arg = get_arg ~optional key in
        let vars = add_vars ids arg vars in
        let s =
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
        | None -> s
        | Some tree ->
            let| () = s in
            if_ (!exit = unset_exit) ~then_:(fun () ->
                match_tree ~exit ~leafstmt ~get_arg ~vars tree))
    | Matching.Nil { key; ids; child } ->
        let@ arg = get_arg ~optional key in
        let vars = add_vars ids arg vars in
        if_ (is_nil arg) ~then_:(fun () ->
            match_tree ~exit ~leafstmt ~get_arg ~vars child)
    | Matching.Cons { key; ids; child } ->
        let@ arg = get_arg ~optional key in
        let vars = add_vars ids arg vars in
        if_ (is_not_nil arg) ~then_:(fun () ->
            match_tree ~exit ~leafstmt ~get_arg ~vars child)
    | Matching.Nil_or_cons { key; ids; nil; cons } ->
        let@ arg = get_arg ~optional key in
        let vars = add_vars ids arg vars in
        if_else (is_nil arg)
          ~then_:(fun () -> match_tree ~exit ~leafstmt ~get_arg ~vars nil)
          ~else_:(fun () -> match_tree ~exit ~leafstmt ~get_arg ~vars cons)
    | Matching.Optional { child; next } -> (
        let s =
          match_tree ~exit ~leafstmt ~get_arg ~optional:true ~vars child
        in
        match next with
        | None -> s
        | Some next ->
            let| () = s in
            if_ (!exit = unset_exit) ~then_:(fun () ->
                match_tree ~exit ~leafstmt ~get_arg ~vars next))
    | Matching.End leaf -> leafstmt ~vars leaf

  and switchcase ~exit ~leafstmt ~get_arg ~vars ~arg ~wildcard
      Matching.{ data; if_match; next } =
    if_else
      (match data with
      | `String x -> Data.to_string arg = string x
      | `Int x -> Data.to_int arg = int x
      | `Float x -> Data.to_float arg = float x)
      ~then_:(fun () -> match_tree ~exit ~leafstmt ~get_arg ~vars if_match)
      ~else_:
        (match next with
        | Some l ->
            fun () -> switchcase ~exit ~leafstmt ~get_arg ~vars ~arg ~wildcard l
        | None -> (
            match wildcard with
            | Some l -> fun () -> match_tree ~exit ~leafstmt ~get_arg ~vars l
            | None -> fun () -> unit))

  let match_leaf exitvar props ~vars Matching.{ names; exit } =
    let| () =
      MapString.to_seq names
      |> Seq.map (fun (key, id) -> props.%{string key} <- MapInt.find id vars)
      |> stmt_join
    in
    exitvar := int exit

  let dummy_props = hashtbl_create ()

  let make_match_props exits f =
    if Matching.Exits.binding_exists exits then
      ( let$ ) ("match_props", hashtbl_create ()) f
    else f dummy_props

  let rec node state = function
    | Compile.Text s -> buffer_add_string state.buf (string s)
    | Compile.Echo (echs, fmt, default, esc) ->
        echoes state esc default fmt echs
    | Compile.Match (blocks, data, { tree; exits }) ->
        let@ blocks = construct_blocks state blocks in
        let$ arg_match =
          ("arg_match", construct_data_array blocks state data)
        in
        let@ new_props = make_match_props exits in
        let& exit = ("exit", unset_exit) in
        let| () =
          match_tree ~exit
            ~leafstmt:(match_leaf exit new_props)
            ~get_arg:(arg_int arg_match) ~vars:MapInt.empty tree
        in
        make_exits exit exits state new_props
    | Compile.Map_list (blocks, data, { tree; exits }) ->
        let@ blocks = construct_blocks state blocks in
        let& index = ("index", int 0) in
        let& cell = ("cell", construct_data blocks state data) in
        while_ is_not_nil cell (fun () ->
            let@ new_props = make_match_props exits in
            let$ list = ("list", Data.to_array !cell) in
            let$ head = ("head", list_hd list) in
            let& exit = ("exit", unset_exit) in
            let| () =
              match_tree ~exit
                ~leafstmt:(match_leaf exit new_props)
                ~get_arg:(arg_indexed head (Data.int !index))
                ~vars:MapInt.empty tree
            in
            let| () = make_exits exit exits state new_props in
            let| () = incr index in
            cell := list_tl list)
    | Compile.Map_dict (blocks, data, { tree; exits }) ->
        let@ blocks = construct_blocks state blocks in
        let$ match_arg = ("match_arg", construct_data blocks state data) in
        iter
          (hashtbl_to_seq (Data.to_hashtbl match_arg))
          (fun p ->
            let k, v = unpair p in
            let@ new_props = make_match_props exits in
            let& exit = ("exit", unset_exit) in
            let| () =
              match_tree ~exit
                ~leafstmt:(match_leaf exit new_props)
                ~get_arg:(arg_indexed v (Data.string k))
                ~vars:MapInt.empty tree
            in
            make_exits exit exits state new_props)
    | Component (name, blocks, dict) ->
        let@ blocks = construct_blocks state blocks in
        buffer_add_string state.buf
          (await
             (MapString.find name state.components
             @@ construct_data_hashtbl blocks state dict))

  and construct_blocks state blocks f =
    let blocks_arr = Array.make (Compile.blocks_length blocks) (string "") in
    let rec aux seq =
      match seq () with
      | Seq.Cons ((i, block), seq) ->
          let@ state_block = state_create_buffer state in
          let| () = nodes state_block block in
          blocks_arr.(i) <- buffer_contents state_block.buf;
          aux seq
      | Seq.Nil -> f blocks_arr
    in
    aux (Compile.blocks_to_seq blocks)

  and make_exits exit exits state new_props =
    let Nonempty.(next :: tl) = Matching.Exits.to_nonempty exits in
    let rec aux Matching.Exits.{ id; bindings; nodes = n } = function
      | [] -> nodes (props_add_scope new_props bindings state) n
      | next :: tl ->
          if_else
            (!exit = int id)
            ~then_:(fun () ->
              nodes (props_add_scope new_props bindings state) n)
            ~else_:(fun () -> aux next tl)
    in
    aux next tl

  and nodes state l = List.to_seq l |> Seq.map (node state) |> stmt_join

  module T = Typechecker.Type

  type 'stack decode_runtime = {
    ty_str : string exp;
    stack : 'stack exp;
    stack_empty : 'stack exp;
    stack_is_empty : ('stack -> bool) exp;
    stack_add : (string -> 'stack -> 'stack) exp;
    decode_error : (External.t -> 'stack -> string -> unit) exp;
    key_error : ('stack -> 'stack -> string -> unit) exp;
  }

  let show_type =
    let b = Buffer.create 128 in
    let ppf = Format.formatter_of_buffer b in
    fun ty ->
      T.pp ppf ty;
      Format.pp_print_flush ppf ();
      let s = Buffer.contents b in
      Buffer.clear b; string s

  let push_error debug input =
    stmt (((debug.decode_error @@ input) @@ debug.stack) @@ debug.ty_str)

  let push_key_error debug missing_keys =
    stmt (((debug.key_error @@ missing_keys) @@ debug.stack) @@ debug.ty_str)

  let stack_add debug x = (debug.stack_add @@ x) @@ debug.stack

  let rec decode ~set ~debug input ty =
    let$ ty_str = ("type", show_type ty) in
    let debug = { debug with ty_str } in
    match ty.contents with
    | T.Unknown _ -> set (Data.unknown input)
    | T.Enum_int ({ cases; _ }, Bool) ->
        External.decode External.get_bool input
          ~ok:(fun b ->
            if_else b
              ~then_:(fun () ->
                if SetInt.mem 1 cases then set (Data.int (int 1))
                else push_error debug input)
              ~else_:(fun () ->
                if SetInt.mem 0 cases then set (Data.int (int 0))
                else push_error debug input))
          ~error:(fun () -> push_error debug input)
    | T.String | T.Enum_string { row = `Open; _ } ->
        External.decode External.get_string input
          ~ok:(fun s -> set (Data.string s))
          ~error:(fun () -> push_error debug input)
    | T.Enum_string { row = `Closed; cases } ->
        External.decode External.get_string input
          ~ok:(fun s ->
            let rec aux seq =
              match seq () with
              | Seq.Nil -> push_error debug input
              | Seq.Cons (case, seq) ->
                  if_else
                    (s = string case)
                    ~then_:(fun () -> set (Data.string s))
                    ~else_:(fun () -> aux seq)
            in
            aux (SetString.to_seq cases))
          ~error:(fun () -> push_error debug input)
    | T.Int | T.Enum_int ({ row = `Open; _ }, _) ->
        External.decode External.get_int input
          ~ok:(fun i -> set (Data.int i))
          ~error:(fun () -> push_error debug input)
    | T.Enum_int ({ row = `Closed; cases }, _) ->
        External.decode External.get_int input
          ~ok:(fun s ->
            let rec aux seq =
              match seq () with
              | Seq.Nil -> push_error debug input
              | Seq.Cons (case, seq) ->
                  if_else
                    (s = int case)
                    ~then_:(fun () -> set (Data.int s))
                    ~else_:(fun () -> aux seq)
            in
            aux (SetInt.to_seq cases))
          ~error:(fun () -> push_error debug input)
    | T.Float ->
        External.decode External.get_float input
          ~ok:(fun f -> set (Data.float f))
          ~error:(fun () -> push_error debug input)
    | T.Nullable ty ->
        External.decode External.get_some input
          ~ok:(fun input ->
            let$ decoded = ("decoded", array [| nil_value |]) in
            let$ stack = ("stack", stack_add debug (string "<nullable>")) in
            let| () =
              decode
                ~set:(fun data -> decoded.%(int 0) <- data)
                ~debug:{ debug with stack } input ty
            in
            set (Data.array decoded))
          ~error:(fun () -> set nil_value)
    | T.List ty ->
        External.decode External.get_seq input
          ~ok:(fun seq ->
            let& i = ("index", int 0) in
            let$ decoded = ("decoded", array [| nil_value; nil_value |]) in
            let& decode_dst = ("decode_dst", decoded) in
            let| () =
              iter seq (fun input ->
                  let$ decode_dst_new =
                    ("decode_dst_new", array [| nil_value; nil_value |])
                  in
                  let$ stack = ("stack", stack_add debug (string_of_int !i)) in
                  let| () =
                    decode
                      ~set:(fun data -> decode_dst_new.%(int 0) <- data)
                      ~debug:{ debug with stack } input ty
                  in
                  let| () = !decode_dst.%(int 1) <- Data.array decode_dst_new in
                  let| () = incr i in
                  decode_dst := decode_dst_new)
            in
            set (list_tl decoded))
          ~error:(fun () -> push_error debug input)
    | T.Tuple tys ->
        External.decode External.get_seq input
          ~ok:(fun seq ->
            let length = List.length tys in
            let$ decoded = ("decoded", array_make length nil_value) in
            let rec aux i seq = function
              | [] -> unit
              | ty :: tl ->
                  uncons seq
                    ~nil:(fun () -> push_error debug input)
                    ~cons:(fun input seq ->
                      let$ stack =
                        ("stack", stack_add debug (string_of_int (int i)))
                      in
                      let debug = { debug with stack } in
                      let| () =
                        decode
                          ~set:(fun data -> decoded.%(int i) <- data)
                          ~debug input ty
                      in
                      aux (succ i) seq tl)
            in
            let| () = aux 0 seq tys in
            set (Data.array decoded))
          ~error:(fun () -> push_error debug input)
    | T.Record tys ->
        External.decode External.get_assoc input
          ~ok:(fun input ->
            let$ decoded = ("decoded", hashtbl_create ()) in
            let| () = decode_record_aux ~debug decoded input tys.contents in
            set (Data.hashtbl decoded))
          ~error:(fun () -> push_error debug input)
    | T.Dict (ty, _) ->
        External.decode External.get_assoc input
          ~ok:(fun input ->
            let$ decoded = ("decoded", hashtbl_create ()) in
            iter (External.assoc_to_seq input) (fun p ->
                let k, input = unpair p in
                let$ stack = ("stack", stack_add debug k) in
                let| () =
                  decode
                    ~set:(fun data -> decoded.%{k} <- data)
                    ~debug:{ debug with stack } input ty
                in
                set (Data.hashtbl decoded)))
          ~error:(fun () -> push_error debug input)
    | T.Union_int (key, { cases; row }, Bool) ->
        decode_union External.get_bool
          ~if_equal:(fun extern ty ~then_ ~else_ ->
            match ty with
            | 0 ->
                if_else (not extern) ~then_:(fun () -> then_ false_value) ~else_
            | _ -> if_else extern ~then_:(fun () -> then_ true_value) ~else_)
          ~if_open:(fun x f ->
            if_else x
              ~then_:(fun () -> f true_value)
              ~else_:(fun () -> f false_value))
          (MapInt.to_seq cases) (string key) row ~set ~debug input
    | T.Union_int (key, { cases; row }, Not_bool) ->
        decode_union External.get_int
          ~if_equal:(fun extern ty ~then_ ~else_ ->
            let ty = int ty in
            if_else (extern = ty) ~then_:(fun () -> then_ (Data.int ty)) ~else_)
          ~if_open:(fun x f -> f (Data.int x))
          (MapInt.to_seq cases) (string key) row ~set ~debug input
    | T.Union_string (key, { cases; row }) ->
        decode_union External.get_string
          ~if_equal:(fun extern ty ~then_ ~else_ ->
            let ty = string ty in
            if_else (extern = ty)
              ~then_:(fun () -> then_ (Data.string ty))
              ~else_)
          ~if_open:(fun x f -> f (Data.string x))
          (MapString.to_seq cases) (string key) row ~set ~debug input

  and decode_union :
      type ty extern.
      extern External.decoder ->
      if_equal:
        (extern exp ->
        ty ->
        then_:(Data.t exp -> unit stmt) ->
        else_:(unit -> unit stmt) ->
        unit stmt) ->
      if_open:(extern exp -> (Data.t exp -> unit stmt) -> unit stmt) ->
      (ty * T.record) Seq.t ->
      _ =
   fun decoder ~if_equal ~if_open seq key row ~set ~debug input ->
    External.decode External.get_assoc input
      ~ok:(fun input' ->
        if_else
          (External.assoc_mem key input')
          ~then_:(fun () ->
            External.decode decoder
              (External.assoc_find key input')
              ~ok:(fun x ->
                let$ decoded = ("decoded", hashtbl_create ()) in
                let rec aux seq =
                  match seq () with
                  | Seq.Nil -> (
                      match row with
                      | `Open -> if_open x (fun x -> decoded.%{key} <- x)
                      | `Closed -> push_error debug input)
                  | Seq.Cons ((ty_x, tys), seq) ->
                      if_equal x ty_x
                        ~then_:(fun x ->
                          let| () = decoded.%{key} <- x in
                          decode_record_aux ~debug decoded input' tys.contents)
                        ~else_:(fun () -> aux seq)
                in
                let| () = aux seq in
                set (Data.hashtbl decoded))
              ~error:(fun () -> push_error debug input))
          ~else_:(fun () -> push_error debug input))
      ~error:(fun () -> push_error debug input)

  and decode_record_aux ~debug decoded input tys =
    let& missing_keys = ("missing_keys", debug.stack_empty) in
    let| () =
      MapString.to_seq tys
      |> Seq.map (fun (k, ty) ->
             let k' = string k in
             if_else
               (External.assoc_mem k' input)
               ~then_:(fun () ->
                 let$ input = ("input", External.assoc_find k' input) in
                 let$ stack = ("stack", stack_add debug k') in
                 decode
                   ~set:(fun data -> decoded.%{k'} <- data)
                   ~debug:{ debug with stack } input ty)
               ~else_:(fun () ->
                 match ty.contents with
                 | Nullable _ | Unknown _ -> decoded.%{k'} <- nil_value
                 | _ -> missing_keys := (debug.stack_add @@ k') @@ !missing_keys))
      |> stmt_join
    in
    if_
      (not (debug.stack_is_empty @@ !missing_keys))
      ~then_:(fun () -> push_key_error debug !missing_keys)

  let external_of_int_bool i = External.of_bool (int_to_bool i)

  let rec encode ~set props ty =
    match ty.contents with
    | T.Unknown _ -> set (Data.to_external_untyped props)
    | T.Enum_int (_, Bool) -> set (external_of_int_bool (Data.to_int props))
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
        let$ seq =
          ( "seq",
            generator (fun yield ->
                let& cell = ("cell", props) in
                while_ is_not_nil cell (fun () ->
                    let$ cell' = ("cell", Data.to_array !cell) in
                    let$ props = ("props", list_hd cell') in
                    let| () = cell := list_tl cell' in
                    encode ~set:yield props ty)) )
        in
        set (External.of_seq seq)
    | T.Tuple tys ->
        let$ props = ("props", Data.to_array props) in
        let$ seq =
          ( "seq",
            generator (fun yield ->
                List.to_seq tys
                |> Seq.mapi (fun i ty -> encode ~set:yield props.%(int i) ty)
                |> stmt_join) )
        in
        set (External.of_seq seq)
    | T.Record tys ->
        let$ seq =
          ("seq", encode_record (Data.to_hashtbl props) tys.contents)
        in
        set (External.of_seq_assoc seq)
    | T.Dict (ty, _) ->
        let$ seq =
          ( "seq",
            generator (fun yield ->
                iter
                  (hashtbl_to_seq (Data.to_hashtbl props))
                  (fun p ->
                    let k, props = unpair p in
                    encode ~set:(fun v -> yield (pair (k, v))) props ty)) )
        in
        set (External.of_seq_assoc seq)
    | T.Union_int (key, { cases; row }, Bool) ->
        encode_union ~of_data:Data.to_int ~to_extern:external_of_int_bool
          (MapInt.to_seq cases |> Seq.map (fun (k, v) -> (int k, v)))
          row (string key) ~set props
    | T.Union_int (key, { cases; row }, Not_bool) ->
        encode_union ~of_data:Data.to_int ~to_extern:External.of_int
          (MapInt.to_seq cases |> Seq.map (fun (k, v) -> (int k, v)))
          row (string key) ~set props
    | T.Union_string (key, { cases; row }) ->
        encode_union ~of_data:Data.to_string ~to_extern:External.of_string
          (MapString.to_seq cases |> Seq.map (fun (k, v) -> (string k, v)))
          row (string key) ~set props

  and encode_union :
      type a.
      of_data:(Data.t exp -> a exp) ->
      to_extern:(a exp -> External.t exp) ->
      (a exp * T.record) Seq.t ->
      _ =
   fun ~of_data ~to_extern cases row key ~set props ->
    let$ props = ("props", Data.to_hashtbl props) in
    let$ tag = ("tag", props.%{key} |> of_data) in
    match cases () with
    | Seq.Nil -> unit
    | Seq.Cons (hd, seq) ->
        let rec aux (tag', tys) seq =
          if_else (tag = tag')
            ~then_:(fun () ->
              let$ seq =
                ( "seq",
                  encode_record ~tag:(key, to_extern tag) props tys.contents )
              in
              set (External.of_seq_assoc seq))
            ~else_:(fun () ->
              match seq () with
              | Seq.Nil ->
                  let tag =
                    match row with
                    | `Closed -> None
                    | `Open -> Some (key, to_extern tag)
                  in
                  let$ seq =
                    ("seq", encode_record ?tag props MapString.empty)
                  in
                  set (External.of_seq_assoc seq)
              | Seq.Cons (hd, seq) -> aux hd seq)
        in
        aux hd seq

  and encode_record ?tag props tys =
    generator (fun yield ->
        let| () = match tag with Some t -> yield (pair t) | None -> unit in
        MapString.to_seq tys
        |> Seq.map (fun (k, ty) ->
               let k = string k in
               encode ~set:(fun v -> yield (pair (k, v))) props.%{k} ty)
        |> stmt_join)

  let rec make_comps_external components components_input f =
    match components_input with
    | (k, tys, v) :: tl ->
        import v (fun import ->
            let$ comp =
              ( k,
                lambda (fun props ->
                    let$ seq = ("seq", encode_record props tys) in
                    return (import @@ External.of_seq_assoc seq)) )
            in
            make_comps_external (MapString.add k comp components) tl f)
    | [] -> f components

  let rec make_comps ~escape components components_input f =
    match components_input with
    | (k, v) :: tl ->
        let$ comp =
          ( k,
            async_lambda (fun props ->
                let@ state = state_make components ~props ~escape in
                let| () = nodes state v in
                return (promise (buffer_contents state.buf))) )
        in
        make_comps ~escape (MapString.add k comp components) tl f
    | [] -> f components

  let lambdak k f = lambda (fun a -> return (k (f a)))
  let lambda2 f = lambdak lambda f
  let lambda3 f = lambdak lambda2 f
  let lambda4 f = lambdak lambda3 f

  let eval (compiled : 'a Compile.t) =
    let$ escape =
      ( "buffer_add_escape",
        lambda2 (fun buf str ->
            iter (string_to_seq str) (fun c ->
                match_char c (function
                  | '&' -> buffer_add_string buf (string "&amp;")
                  | '"' -> buffer_add_string buf (string "&quot;")
                  | '\'' -> buffer_add_string buf (string "&apos;")
                  | '>' -> buffer_add_string buf (string "&gt;")
                  | '<' -> buffer_add_string buf (string "&lt;")
                  | '/' -> buffer_add_string buf (string "&sol;")
                  | '`' -> buffer_add_string buf (string "&grave;")
                  | '=' -> buffer_add_string buf (string "&equals;")
                  | _ -> buffer_add_char buf c))) )
    in
    let$ buffer_add_sep =
      ( "buffer_add_sep",
        lambda3 (fun buf sep str ->
            let| () =
              if_
                (not (buffer_length buf = int 0))
                ~then_:(fun () -> buffer_add_string buf sep)
            in
            buffer_add_string buf str) )
    in
    (* Use a functional continuation stack to track decode progress. *)
    let$ stack_empty = ("stack_empty", lambda (fun _ -> unit)) in
    let$ stack_is_empty =
      ( "stack_is_empty",
        lambda (fun stack ->
            let& result = ("result", bool true) in
            let| () = stmt (stack @@ lambda (fun _ -> result := bool false)) in
            return !result) )
    in
    let$ stack_add =
      ( "stack_add",
        lambda3 (fun x stack f ->
            let| () = stmt (stack @@ f) (* Use FIFO evaluation. *) in
            return (f @@ x)) )
    in
    let@ components = make_comps_external MapString.empty compiled.externals in
    let@ components = make_comps ~escape components compiled.components in
    export
      (async_lambda (fun input ->
           let$ errors = ("errors", buffer_create ()) in
           let$ error_aux =
             ( "error_aux",
               lambda4 (fun msg1 msg2 stack ty ->
                   let| () =
                     if_
                       (not (buffer_length errors = int 0))
                       ~then_:(fun () ->
                         buffer_add_string errors (string "\n\n"))
                   in
                   let| () = buffer_add_string errors (string "File \"") in
                   let| () = buffer_add_string errors (string compiled.name) in
                   let| () =
                     buffer_add_string errors
                       (string
                          "\"\n\
                           Render error.\n\
                           The data supplied does not match this template's \
                           interface.\n")
                   in
                   let| () =
                     buffer_add_string errors (string "Path:\n<input>")
                   in
                   let| () =
                     stmt (stack @@ (buffer_add_sep @@ errors) @@ string " -> ")
                   in
                   let| () =
                     buffer_add_string errors (string "\nExpected type:\n")
                   in
                   let| () = buffer_add_string errors ty in
                   let| () = buffer_add_string errors msg1 in
                   buffer_add_string errors msg2) )
           in
           let$ decode_error =
             ( "decode_error",
               lambda (fun input ->
                   return
                     ((error_aux @@ string "\nReceived value:\n")
                     @@ External.to_string input)) )
           in
           let$ key_error =
             ( "key_error",
               lambda (fun keys ->
                   let$ buf = ("buf", buffer_create ()) in
                   let| () =
                     stmt (keys @@ (buffer_add_sep @@ buf) @@ string ", ")
                   in
                   return
                     ((error_aux @@ string "\nInput is missing keys:\n")
                     @@ buffer_contents buf)) )
           in
           let$ props = ("props", hashtbl_create ()) in
           let stack = stack_empty in
           let$ ty_str = ("type", show_type (T.record (ref compiled.types))) in
           let debug =
             {
               ty_str;
               stack;
               stack_empty;
               stack_is_empty;
               stack_add;
               decode_error;
               key_error;
             }
           in
           let| () =
             External.decode External.get_assoc input
               ~ok:(fun input ->
                 decode_record_aux ~debug props input compiled.types)
               ~error:(fun () -> push_error debug input)
           in
           if_else
             (buffer_length errors = int 0)
             ~then_:(fun () ->
               let@ state = state_make components ~props ~escape in
               let| () = nodes state compiled.nodes in
               return (promise (buffer_contents state.buf)))
             ~else_:(fun () -> return (error (buffer_contents errors)))))

  let eval compiled = observe (eval compiled)
end

module type TRANS = sig
  (** To transform the language's output, such as for an optimization, we need
      to define a module that can translate terms "forward" to the transformed
      state and "backward" to the original representation. *)

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
    functions to apply transformations. *)
module MakeTrans
    (T : TRANS)
    (F : SEM with type 'a exp = 'a T.from_exp and type 'a stmt = 'a T.from_stmt) :
  SEM
    with type 'a stmt = 'a T.stmt
     and type 'a obs = 'a F.obs
     and type 'a exp = 'a T.exp
     and type 'a ref = 'a F.ref
     and type 'a promise = 'a F.promise
     and type 'a External.assoc = 'a F.External.assoc
     and type External.t = F.External.t
     and type 'a External.decoder = 'a F.External.decoder
     and type Data.t = F.Data.t
     and type import = F.import = struct
  open T
  include F

  type 'a exp = 'a T.exp
  type 'a stmt = 'a T.stmt

  let observe x = F.observe (bwds x)
  let ( let| ) a f = fwds (F.( let| ) (bwds a) (fun () -> bwds (f ())))
  let return x = fwds (F.return (bwde x))
  let stmt x = fwds (F.stmt (bwde x))

  let ( let$ ) (s, x) f =
    fwds (F.( let$ ) (s, bwde x) (fun x -> bwds (f (fwde x))))

  let ( let& ) (s, x) f = fwds (F.( let& ) (s, bwde x) (fun x -> bwds (f x)))
  let ( ! ) x = fwde F.(!x)
  let ( := ) a x = fwds F.(a := bwde x)
  let incr x = fwds (F.incr x)
  let lambda f = fwde (F.lambda (fun x -> bwds (f (fwde x))))
  let ( @@ ) f x = fwde F.(bwde f @@ bwde x)

  let if_else b ~then_ ~else_ =
    fwds
      (F.if_else (bwde b)
         ~then_:(fun () -> bwds (then_ ()))
         ~else_:(fun () -> bwds (else_ ())))

  let while_ f b g =
    fwds (F.while_ (fun b -> bwde (f (fwde b))) b (fun () -> bwds (g ())))

  let unit = fwds F.unit
  let not x = fwde (F.not (bwde x))
  let int x = fwde (F.int x)
  let float x = fwde (F.float x)
  let string x = fwde (F.string x)
  let bool x = fwde (F.bool x)
  let ( = ) a b = fwde F.(bwde a = bwde b)
  let pair (a, b) = fwde (F.pair (bwde a, bwde b))

  let unpair x =
    let a, b = F.unpair (bwde x) in
    (fwde a, fwde b)

  let string_of_int x = fwde (F.string_of_int (bwde x))
  let string_of_float x = fwde (F.string_of_float (bwde x))
  let string_of_bool x = fwde (F.string_of_bool (bwde x))

  let uncons s ~nil ~cons =
    fwds
      (F.uncons (bwde s)
         ~nil:(fun () -> bwds (nil ()))
         ~cons:(fun x s -> bwds (cons (fwde x) (fwde s))))

  let generator f =
    fwde (F.generator (fun yield -> bwds (f (fun x -> fwds (yield (bwde x))))))

  let iter s f = fwds (F.iter (bwde s) (fun x -> bwds (f (fwde x))))
  let string_to_seq s = fwde (F.string_to_seq (bwde s))

  let match_char c f =
    fwds (F.match_char (bwde c) (fun shape -> bwds (f shape)))

  let array x = fwde (F.array (Array.map bwde x))
  let array_make i x = fwde (F.array_make i (bwde x))
  let ( .%() ) a i = fwde F.((bwde a).%(bwde i))
  let ( .%()<- ) a i x = fwds F.((bwde a).%(bwde i) <- bwde x)
  let hashtbl x = fwde (F.hashtbl (Seq.map (fun (a, b) -> (bwde a, bwde b)) x))
  let hashtbl_create () = fwde (F.hashtbl_create ())
  let ( .%{} ) h k = fwde F.((bwde h).%{bwde k})
  let ( .%{}<- ) h k x = fwds F.((bwde h).%{bwde k} <- bwde x)
  let hashtbl_mem h k = fwde (F.hashtbl_mem (bwde h) (bwde k))
  let hashtbl_to_seq h = fwde (F.hashtbl_to_seq (bwde h))
  let buffer_create () = fwde (F.buffer_create ())
  let buffer_add_string b s = fwds (F.buffer_add_string (bwde b) (bwde s))
  let buffer_add_char b c = fwds (F.buffer_add_char (bwde b) (bwde c))
  let buffer_contents b = fwde (F.buffer_contents (bwde b))
  let buffer_length b = fwde (F.buffer_length (bwde b))
  let promise x = fwde (F.promise (bwde x))
  let await p = fwde (F.await (bwde p))
  let error s = fwde (F.error (bwde s))
  let async_lambda f = fwde (F.async_lambda (fun x -> bwds (f (fwde x))))

  module External = struct
    include F.External

    let null = fwde F.External.null
    let some x = fwde (F.External.some (bwde x))
    let of_int x = fwde (F.External.of_int (bwde x))
    let of_float x = fwde (F.External.of_float (bwde x))
    let of_string x = fwde (F.External.of_string (bwde x))
    let of_bool x = fwde (F.External.of_bool (bwde x))
    let of_seq x = fwde (F.External.of_seq (bwde x))
    let of_seq_assoc x = fwde (F.External.of_seq_assoc (bwde x))
    let assoc_find s t = fwde (F.External.assoc_find (bwde s) (bwde t))
    let assoc_mem s t = fwde (F.External.assoc_mem (bwde s) (bwde t))
    let assoc_to_seq t = fwde (F.External.assoc_to_seq (bwde t))

    let decode d x ~ok ~error =
      fwds
        (F.External.decode d (bwde x)
           ~ok:(fun x -> bwds (ok (fwde x)))
           ~error:(fun () -> bwds (error ())))

    let to_string x = fwde (F.External.to_string (bwde x))
  end

  module Data = struct
    type t = F.Data.t

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
    let to_external_untyped x = fwde (F.Data.to_external_untyped (bwde x))
    let is_int x = fwde (F.Data.is_int (bwde x))
  end

  let import i f = fwds (F.import i (fun fi -> bwds (f (fwde fi))))
  let export x = fwds (F.export (bwde x))
end

(** Pretty-print the instructions for a compiled template. *)
let pp (type a) pp_import ppf c =
  let module F = Format in
  let module M = Make (struct
    let var = F.dprintf "%s"

    type 'a stmt = F.formatter -> unit
    type 'a obs = F.formatter -> unit

    let observe = Fun.id
    let ( let| ) s f = F.dprintf "%t@ %t" s (f ())

    type 'a exp = F.formatter -> unit

    let return = F.dprintf "(@[return@ %t@])"
    let stmt = F.dprintf "(@[stmt@ %t@])"

    let ( let$ ) (v, e) f =
      let v = var v in
      F.dprintf "(@[@[let$@ %t@ =@]@ %t@])@ %t" v e (f v)

    type 'a ref = F.formatter -> unit

    let ( let& ) (v, e) f =
      let v = var v in
      F.dprintf "(@[@[let&@ %t@ =@]@ %t@])@ %t" v e (f v)

    let ( ! ) = F.dprintf "!%t"
    let ( := ) = F.dprintf "(@[%t@ :=@ %t@])"
    let incr = F.dprintf "(@[incr@ %t@])"

    let lambda_aux s f =
      let arg = var "arg" in
      F.dprintf "(@[%slambda@ %t@ (@[<hv>%t@])@])" s arg (f arg)

    let lambda = lambda_aux ""
    let ( @@ ) = F.dprintf "(@[%t@ %@%@ %t@])"

    let if_else b ~then_ ~else_ =
      F.dprintf
        "(@[<hv>@[if_else@ %t@]@ (@[<hv>then@ %t@])@ (@[<hv>else@ %t@])@])" b
        (then_ ()) (else_ ())

    let while_ f b g = F.dprintf "(@[while@ %t@ (@[<hv>%t@])@])" (f !b) (g ())
    let unit = F.dprintf "(unit)"
    let not = F.dprintf "(@[not@ %t@])"
    let int = F.dprintf "%i"
    let float = F.dprintf "%g"
    let string = F.dprintf "%S"
    let bool = F.dprintf "%B"
    let ( = ) = F.dprintf "(@[%t@ =@ %t@])"
    let pair (a, b) = F.dprintf "(@[%t,@ %t@])" a b
    let unpair x = (F.dprintf "(@[fst@ %t@])" x, F.dprintf "(@[snd@ %t@])" x)
    let string_of_int = F.dprintf "(@[string_of_int@ %t@])"
    let string_of_float = F.dprintf "(@[string_of_float@ %t@])"
    let string_of_bool = F.dprintf "(@[string_of_bool@ %t@])"

    let uncons seq ~nil ~cons =
      let hd = var "hd" in
      let seq' = var "seq" in
      F.dprintf "(@[uncons@ %t@ (@[nil@ %t@])@ (@[cons@ %t@ %t@ %t@])@])" seq
        (nil ()) hd seq' (cons hd seq')

    let generator f =
      F.dprintf "(@[generator@ %t@])" (f (F.dprintf "(@[yield@ %t@])"))

    let iter s f =
      let arg = var "arg" in
      F.dprintf "(@[iter@ %t@ %t@])" s (f arg)

    let string_to_seq = F.dprintf "(@[string_to_seq@ %t@])"

    let match_char c f =
      F.dprintf "(@[match_char@ %t@ (@[%a@ (@[_@ (@[%t@])@])@])@])" c
        (F.pp_print_list ~pp_sep:F.pp_print_space (fun ppf c ->
             F.fprintf ppf "(@[%C@ (@[%t@])@])" c (f c)))
        [ '&'; '"'; '\''; '>'; '<'; '/'; '`'; '=' ]
        (f '\x00')

    let array a =
      F.dprintf "[@[<hv>%a@]]" (F.pp_print_array ~pp_sep:Pp.comma ( |> )) a

    let array_make = F.dprintf "(@[array_make@ %i@ %t@])"
    let bindop_get = F.dprintf "(@[%t@,.%%%c@[@,%t@,@]%c@])"
    let bindop_set = F.dprintf "(@[%t@,.%%%c@[@,%t@,@]%c@ <-@ %t@])"
    let ( .%() ) a i = bindop_get a '(' i ')'
    let ( .%()<- ) a i v = bindop_set a '(' i ')' v

    let hashtbl =
      F.dprintf "(@[hashtbl@ [@[<hv>%a@]]@])"
        (F.pp_print_seq ~pp_sep:Pp.comma (fun ppf (a, b) ->
             F.fprintf ppf "(@[%t,@ %t@])" a b))

    let hashtbl_create () = F.dprintf "(hashtbl_create)"
    let ( .%{} ) t k = bindop_get t '{' k '}'
    let ( .%{}<- ) t k v = bindop_set t '{' k '}' v
    let hashtbl_mem = F.dprintf "(@[hashtbl_mem@ %t@ %t@])"
    let hashtbl_to_seq = F.dprintf "(@[hashtbl_to_seq@ %t@])"
    let buffer_create () = F.dprintf "(buffer_create)"
    let buffer_add_string = F.dprintf "(@[buffer_add_string@ %t@ %t@])"
    let buffer_add_char = F.dprintf "(@[buffer_add_char@ %t@ %t@])"
    let buffer_contents = F.dprintf "(@[buffer_contents@ %t@])"
    let buffer_length = F.dprintf "(@[buffer_length@ %t@])"

    type 'a promise

    let promise = F.dprintf "(@[promise@ %t@])"
    let await = F.dprintf "(@[await@ %t@])"
    let error = F.dprintf "(@[error@ %t@])"
    let async_lambda = lambda_aux "async_"

    module External = struct
      type t

      let null = F.dprintf "null"
      let some = F.dprintf "(@[External.some@ %t@])"
      let of_int = F.dprintf "(@[External.of_int@ %t@])"
      let of_string = F.dprintf "(@[External.of_string@ %t@])"
      let of_float = F.dprintf "(@[External.of_float@ %t@])"
      let of_bool = F.dprintf "(@[External.of_bool@ %t@])"
      let of_seq = F.dprintf "(@[External.of_seq@ %t@])"
      let of_seq_assoc = F.dprintf "(@[External.of_seq_assoc@ %t@])"

      type 'a assoc

      let assoc_find = F.dprintf "(@[External.assoc_find@ %t@ %t@])"
      let assoc_mem = F.dprintf "(@[External.assoc_mem@ %t@ %t@])"
      let assoc_to_seq = F.dprintf "(@[External.assoc_to_seq@ %t@])"

      type 'a decoder = string

      let get_int = "(int)"
      let get_string = "(string)"
      let get_float = "(float)"
      let get_bool = "(bool)"
      let get_some = "(some)"
      let get_seq = "(seq)"
      let get_assoc = "(assoc)"

      let decode c t ~ok ~error =
        let decoded = var "decoded" in
        F.dprintf
          "(@[External.decode@ %s@ %t@ (@[<hv>@[<hv>ok@ %t@]@ %t@])@ \
           (@[<hv>error@ %t@])@])"
          c t decoded (ok decoded) (error ())

      let to_string = F.dprintf "(@[External.to_string@ %t@])"
    end

    module Data = struct
      type t

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
      let to_external_untyped = F.dprintf "(@[Data.to_external_untyped@ %t@])"
      let is_int = F.dprintf "(@[Data.is_int@ %t@])"
    end

    type import = a

    let import i f =
      let name = var "import" in
      F.dprintf "(@[import@ %t@ from@ %a@])@ %t" name pp_import i (f name)

    let export = F.dprintf "(@[export@ %t@])"
  end) in
  F.fprintf ppf "@[<hv>%t@]" (M.eval c)
