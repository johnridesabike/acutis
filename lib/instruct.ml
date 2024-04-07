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

  val stmt : 'a exp -> 'a stmt

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

  val while_ :
    ('a exp -> bool exp) -> 'a mut -> (unit -> unit stmt) -> unit stmt

  (** {1 Standard values.} *)

  val unit : unit stmt
  val not : bool exp -> bool exp
  val int : int -> int exp
  val float : float -> float exp
  val string : string -> string exp
  val bool : bool -> bool exp
  val equal_int : int exp -> int exp -> bool exp
  val equal_string : string exp -> string exp -> bool exp
  val string_of_int : int exp -> string exp
  val float_of_int : int exp -> float exp
  val string_of_float : float exp -> string exp
  val string_of_bool : int exp -> string exp

  (** {1 Promises.} *)

  type 'a promise
  (** An asynchronous monad. *)

  val promise : 'a exp -> 'a promise exp
  val bind : 'a promise exp -> ('a -> 'b promise) exp -> 'b promise exp
  val error : string exp -> 'a promise exp

  (** {1 Importing and exporting.} *)

  type external_data
  (** Data from the outside world that we need to decode. *)

  type import
  (** Information to import a function from external code. *)

  val import :
    import -> ((external_data -> string promise) exp -> 'a stmt) -> 'a stmt

  val export : 'a exp -> 'a stmt

  (** {1 Arrays.} *)

  val array : 'a exp array -> 'a array exp
  val array_make : int exp -> 'a exp -> 'a array exp
  val ( .%() ) : 'a array exp -> int exp -> 'a exp
  val ( .%()<- ) : 'a array exp -> int exp -> 'a exp -> unit stmt

  (** {1 Hash tables.} *)

  type 'a hashtbl
  (** A mutable map of string keys to ['a] values. *)

  val hashtbl : (string exp * 'a exp) Seq.t -> 'a hashtbl exp
  val hashtbl_create : unit -> 'a hashtbl exp
  val ( .%{} ) : 'a hashtbl exp -> string exp -> 'a exp
  val ( .%{}<- ) : 'a hashtbl exp -> string exp -> 'a exp -> unit stmt
  val hashtbl_mem : 'a hashtbl exp -> string exp -> bool exp
  val hashtbl_copy : 'a hashtbl exp -> 'a hashtbl exp

  val hashtbl_iter :
    'a hashtbl exp -> (string exp -> 'a exp -> unit stmt) -> unit stmt

  (** {1 Buffers.} *)

  type buffer
  (** A concurrent buffer. *)

  val buffer_create : unit -> buffer exp
  val buffer_add_string : buffer exp -> string exp -> unit stmt
  val buffer_add_buffer : buffer exp -> buffer exp -> unit stmt

  val buffer_add_escape : buffer exp -> string exp -> unit stmt
  (** E.g. sanitize HTML syntax. *)

  val buffer_contents : buffer exp -> string exp
  val buffer_length : buffer exp -> int exp
  val buffer_clear : buffer exp -> unit stmt

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

  (** We use the [data] type as linked-list stacks. *)

  let buffer_add_stack buf stack sep =
    (* Assume it's nonempty. *)
    let$ tuple = ("tuple", Data.to_array stack) in
    let s = buffer_add_string buf (Data.to_string tuple.%(int 0)) in
    s
    |: let& stack = ("stack", tuple.%(int 1)) in
       while_ is_not_nil stack (fun () ->
           let$ tuple = ("tuple", Data.to_array (deref stack)) in
           let s = buffer_add_string buf sep in
           let s = s |: buffer_add_string buf (Data.to_string tuple.%(int 0)) in
           s |: (stack := tuple.%(int 1)))

  let stack_add stack x = Data.array (array [| Data.string x; stack |])
  let stack_singleton = stack_add nil_value

  type async_buffer = { sync : buffer exp; async : buffer promise mut }
  (** Using the assumption that synchronous writes are significantly faster
      than asynchronous writes, we use a dual-buffer strategy. Most of our
      writes are to a regular sync buffer. Whenever we need to use the async
      buffer, we flush the sync buffer to it. *)

  let buffer_add_promise { sync; async } p =
    let$ sync_contents = ("sync_contents", buffer_contents sync) in
    let s = buffer_clear sync in
    s
    |: (async :=
          bind (deref async)
            (lambda (fun b ->
                 return
                   (bind p
                      (lambda (fun promise_str ->
                           let s = buffer_add_string b sync_contents in
                           let s = s |: buffer_add_string b promise_str in
                           s |: return (promise b)))))))

  (** This should always be the final use of the buffer. Writing to it
      afterwards is unsafe. *)
  let buffer_contents_async { sync; async } =
    bind (deref async)
      (lambda (fun b ->
           let s = buffer_add_buffer b sync in
           s |: return (promise (buffer_contents b))))

  type runtime = {
    components : (data hashtbl -> string promise) hashtbl exp;
    buffer : async_buffer;
  }

  let join_stmts seq =
    match seq () with
    | Seq.Nil -> unit
    | Seq.Cons (s, seq) -> Seq.fold_left ( |: ) s seq

  let parse_escape runtime esc x =
    match esc with
    | Compile.No_escape -> buffer_add_string runtime.buffer.sync x
    | Compile.Escape -> buffer_add_escape runtime.buffer.sync x

  let fmt runtime esc x = function
    | Compile.Fmt_string -> parse_escape runtime esc (Data.to_string x)
    | Compile.Fmt_int ->
        parse_escape runtime esc (string_of_int (Data.to_int x))
    | Compile.Fmt_float ->
        parse_escape runtime esc (string_of_float (Data.to_float x))
    | Compile.Fmt_bool ->
        parse_escape runtime esc (string_of_bool (Data.to_int x))

  let rec echo props (ech : Compile.echo) =
    match ech with
    | `Var s -> props.%{string s}
    | `String s -> Data.string (string s)
    | `Field (e, s) -> (echo props e |> Data.to_hashtbl).%{string s}

  let rec echoes runtime props esc default default_fmt = function
    | [] -> fmt runtime esc (echo props default) default_fmt
    | (f, e) :: tl ->
        let$ nullable = ("nullable", echo props e) in
        if_else (is_not_nil nullable)
          ~then_:(fun () -> fmt runtime esc (get_nullable nullable) f)
          ~else_:(fun () -> echoes runtime props esc default default_fmt tl)

  let rec construct_data blocks props (data : Compile.data) =
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
    |> Seq.map (fun (k, v) -> (string k, construct_data blocks props v))
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
            s
            |: if_
                 (equal_int (deref exit) unset_exit)
                 ~then_:(fun () ->
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
            s
            |: if_
                 (equal_int (deref exit) unset_exit)
                 ~then_:(fun () ->
                   match_tree ~exit ~leafstmt ~get_arg ~vars next))
    | Matching.End leaf -> leafstmt ~vars leaf

  and switchcase ~exit ~leafstmt ~get_arg ~vars ~arg ~wildcard
      Matching.{ data; if_match; next } =
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

  let match_leaf exitvar props ~vars Matching.{ names; exit } =
    let s =
      Map.String.to_seq names
      |> Seq.map (fun (key, id) -> props.%{string key} <- Map.Int.find id vars)
      |> join_stmts
    in
    s |: (exitvar := int (Matching.Exit.key_to_int exit))

  let make_exits exit exits f =
    match Matching.Exit.to_seqi exits () with
    | Seq.Nil -> Error.internal ~__POS__ "No exits."
    | Seq.Cons (hd, tl) ->
        let rec aux (i, v) seq =
          match seq () with
          | Seq.Nil -> f v
          | Seq.Cons (hd, tl) ->
              if_else
                (equal_int (deref exit) (int (Matching.Exit.key_to_int i)))
                ~then_:(fun () -> f v)
                ~else_:(fun () -> aux hd tl)
        in
        aux hd tl

  let rec node runtime props = function
    | Compile.Text s -> buffer_add_string runtime.buffer.sync (string s)
    | Compile.Echo (echs, fmt, default, esc) ->
        echoes runtime props esc default fmt echs
    | Compile.Match (blocks, data, { tree; exits }) ->
        construct_blocks runtime blocks props (fun blocks runtime ->
            let$ arg_match =
              ("arg_match", construct_data_array blocks props data)
            in
            let$ props = ("props", hashtbl_copy props) in
            let& exit = ("exit", unset_exit) in
            let s =
              match_tree ~exit ~leafstmt:(match_leaf exit props)
                ~get_arg:(arg_int arg_match) ~vars:Map.Int.empty tree
            in
            s |: make_exits exit exits (nodes runtime props))
    | Compile.Map_list (blocks, data, { tree; exits }) ->
        construct_blocks runtime blocks props (fun blocks runtime ->
            let& index = ("index", int 0) in
            let& cell = ("cell", construct_data blocks props data) in
            while_ is_not_nil cell (fun () ->
                let$ props = ("props", hashtbl_copy props) in
                let$ list = ("list", deref cell |> Data.to_array) in
                let$ head = ("head", list_hd list) in
                let& exit = ("exit", unset_exit) in
                let s =
                  match_tree ~exit ~leafstmt:(match_leaf exit props)
                    ~get_arg:(arg_indexed head (Data.int (deref index)))
                    ~vars:Map.Int.empty tree
                in
                let s = s |: make_exits exit exits (nodes runtime props) in
                let s = s |: incr index in
                s |: (cell := list_tl list)))
    | Compile.Map_dict (blocks, data, { tree; exits }) ->
        construct_blocks runtime blocks props (fun blocks runtime ->
            let$ match_arg = ("match_arg", construct_data blocks props data) in
            hashtbl_iter (Data.to_hashtbl match_arg) (fun k v ->
                let$ props = ("props", hashtbl_copy props) in
                let& exit = ("exit", unset_exit) in
                let s =
                  match_tree ~exit ~leafstmt:(match_leaf exit props)
                    ~get_arg:(arg_indexed v (Data.string k))
                    ~vars:Map.Int.empty tree
                in
                s |: make_exits exit exits (nodes runtime props)))
    | Component (name, blocks, dict) ->
        construct_blocks runtime blocks props (fun blocks runtime ->
            buffer_add_promise runtime.buffer
              (runtime.components.%{string name}
              @@ construct_data_hashtbl blocks props dict))

  and construct_blocks runtime raw_blocks props f =
    match Array.to_seqi raw_blocks () with
    (* With no blocks, just continue evaluating with a dummy value. *)
    | Seq.Nil -> f [||] runtime
    (* From the first block, we construct a chain of binded promises. *)
    | Seq.Cons ((i, block), seq) ->
        let blocks = Array.make (Array.length raw_blocks) (string "") in
        let rec aux seq =
          match seq () with
          | Seq.Cons ((i, block), seq) ->
              let$ sync = ("block_buf_sync", buffer_create ()) in
              let& async = ("block_buf_aync", promise (buffer_create ())) in
              let buffer = { sync; async } in
              let s = nodes { runtime with buffer } props block in
              s
              |: return
                   (bind
                      (buffer_contents_async buffer)
                      (lambda (fun block ->
                           blocks.(i) <- block;
                           aux seq)))
          | Seq.Nil ->
              let$ sync = ("buf_sync", buffer_create ()) in
              let& async = ("buf_async", promise (buffer_create ())) in
              let buffer = { sync; async } in
              let s = f blocks { runtime with buffer } in
              s |: return (buffer_contents_async buffer)
        in
        let$ sync = ("block_buf_sync", buffer_create ()) in
        let& async = ("block_buf_aync", promise (buffer_create ())) in
        let buffer = { sync; async } in
        let s = nodes { runtime with buffer } props block in
        s
        |: buffer_add_promise runtime.buffer
             (bind
                (buffer_contents_async buffer)
                (lambda (fun block ->
                     blocks.(i) <- block;
                     aux seq)))

  and nodes runtime props l =
    List.to_seq l |> Seq.map (node runtime props) |> join_stmts

  module T = Typechecker.Type

  type decode_runtime = {
    stack : data exp;
    decode_error : (data -> string -> external_data -> unit) exp;
    key_error : (data -> string -> data -> unit) exp;
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
    stmt (((debug.decode_error @@ debug.stack) @@ ty_str) @@ input)

  let push_key_error debug ty_str missing_keys =
    stmt (((debug.key_error @@ debug.stack) @@ ty_str) @@ deref missing_keys)

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
              ~ok:(fun i -> set (Data.float (float_of_int i)))
              ~error:(fun () -> push_error debug ty_str input))
    | T.Nullable ty ->
        if_else (External.is_null input)
          ~then_:(fun () -> set nil_value)
          ~else_:(fun () ->
            let$ decoded = ("decoded", array [| nil_value |]) in
            let$ stack =
              ("stack", stack_add debug.stack (string "<nullable>"))
            in
            let s =
              decode
                ~set:(fun data -> decoded.%(int 0) <- data)
                ~debug:{ debug with stack } input ty
            in
            let s = s |: set (Data.array decoded) in
            s)
    | T.List ty ->
        External.classify Linear input
          ~ok:(fun l ->
            let$ decoded = ("decoded", array [| nil_value; nil_value |]) in
            let& decode_dst = ("decode_dst", decoded) in
            let s =
              External.Linear.iteri l (fun i input ->
                  let$ decode_dst_new =
                    ("decode_dst_new", array [| nil_value; nil_value |])
                  in
                  let$ stack =
                    ("stack", stack_add debug.stack (string_of_int i))
                  in
                  let s =
                    decode
                      ~set:(fun data -> decode_dst_new.%(int 0) <- data)
                      ~debug:{ debug with stack } input ty
                  in
                  let s =
                    s
                    |: ((deref decode_dst).%(int 1) <- Data.array decode_dst_new)
                  in
                  s |: (decode_dst := decode_dst_new))
            in
            s |: set (list_tl decoded))
          ~error:(fun () -> push_error debug ty_str input)
    | T.Tuple tys ->
        let length = int (List.length tys) in
        External.classify Linear input
          ~ok:(fun l ->
            if_else
              (equal_int (External.Linear.length l) length)
              ~then_:(fun () ->
                let$ decoded = ("decoded", array_make length nil_value) in
                External.Linear.iteri l (fun i input ->
                    let$ stack =
                      ("stack", stack_add debug.stack (string_of_int i))
                    in
                    let debug = { debug with stack } in
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
                    let s = aux 0 tys in
                    s |: set (Data.array decoded)))
              ~else_:(fun () -> push_error debug ty_str input))
          ~error:(fun () -> push_error debug ty_str input)
    | T.Record tys ->
        External.classify Assoc input
          ~ok:(fun input' ->
            let$ decoded = ("decoded", hashtbl_create ()) in
            let s = decode_record_aux ~debug decoded input' !tys ty_str in
            s |: set (Data.hashtbl decoded))
          ~error:(fun () -> push_error debug ty_str input)
    | T.Dict (ty, _) ->
        External.classify Assoc input
          ~ok:(fun a ->
            let$ decoded = ("decoded", hashtbl_create ()) in
            External.Assoc.iter a (fun k input ->
                let$ stack = ("stack", stack_add debug.stack k) in
                let s =
                  decode
                    ~set:(fun data -> decoded.%{k} <- data)
                    ~debug:{ debug with stack } input ty
                in
                s |: set (Data.hashtbl decoded)))
          ~error:(fun () -> push_error debug ty_str input)
    | T.Union_int (key, { cases; row = _ }, Bool) ->
        let key = string key in
        External.classify Assoc input
          ~ok:(fun input' ->
            let aux i v () =
              match Map.Int.find_opt i cases with
              | Some tys ->
                  let$ decoded = ("decoded", hashtbl_create ()) in
                  let s = decoded.%{key} <- v in
                  let s =
                    s |: decode_record_aux ~debug decoded input' !tys ty_str
                  in
                  s |: set (Data.hashtbl decoded)
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
                          let s = decoded.%{key'} <- to_data i in
                          s |: set (Data.hashtbl decoded)
                      | `Closed -> push_error debug ty_str input)
                  | Seq.Cons ((ty_i, tys), seq) ->
                      if_else (equal i ty_i)
                        ~then_:(fun () ->
                          let$ decoded = ("decoded", hashtbl_create ()) in
                          let s = decoded.%{key'} <- to_data i in
                          let s =
                            s
                            |: decode_record_aux ~debug decoded input' !tys
                                 ty_str
                          in
                          s |: set (Data.hashtbl decoded))
                        ~else_:(fun () -> aux seq)
                in
                aux seq)
              ~error:(fun () -> push_error debug ty_str input))
          ~else_:(fun () -> push_error debug ty_str input))
      ~error:(fun () -> push_error debug ty_str input)

  and decode_record_aux ~debug decoded input tys ty_str =
    let& missing_keys = ("missing_keys", nil_value) in
    let s =
      Map.String.to_seq tys
      |> Seq.map (fun (k, ty) ->
             let k' = string k in
             if_else
               (External.Assoc.mem input k')
               ~then_:(fun () ->
                 let$ input = ("input", External.Assoc.find input k') in
                 let$ stack = ("stack", stack_add debug.stack k') in
                 decode
                   ~set:(fun data -> decoded.%{k'} <- data)
                   ~debug:{ debug with stack } input ty)
               ~else_:(fun () ->
                 match !ty with
                 | Nullable _ | Unknown _ -> decoded.%{k'} <- nil_value
                 | _ -> missing_keys := stack_add (deref missing_keys) k'))
      |> join_stmts
    in
    s
    |: if_
         (is_not_nil (deref missing_keys))
         ~then_:(fun () -> push_key_error debug ty_str missing_keys)

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
        let s =
          while_ is_not_nil cell (fun () ->
              let s = incr index in
              s |: (cell := list_tl (Data.to_array (deref cell))))
        in
        s
        |:
        let$ encoded = ("encoded", array_make (deref index) External.null) in
        let s = cell := props in
        let s = s |: (index := int 0) in
        let s =
          s
          |: while_ is_not_nil cell (fun () ->
                 let$ props =
                   ("props", deref cell |> Data.to_array |> list_hd)
                 in
                 let s =
                   encode ~set:(fun x -> encoded.%(deref index) <- x) props ty
                 in
                 let s = s |: incr index in
                 s |: (cell := list_tl (Data.to_array (deref cell))))
        in
        s |: set (External.of_array encoded)
    | T.Tuple tys ->
        let$ props = ("props", Data.to_array props) in
        let$ encoded =
          ("encoded", array_make (int (List.length tys)) External.null)
        in
        let s =
          List.to_seq tys
          |> Seq.mapi (fun i ty ->
                 let i = int i in
                 let$ props = ("props", props.%(i)) in
                 encode ~set:(fun x -> encoded.%(i) <- x) props ty)
          |> join_stmts
        in
        s |: set (External.of_array encoded)
    | T.Record tys ->
        let$ encoded = ("encoded", hashtbl_create ()) in
        let s = encode_record_aux encoded (Data.to_hashtbl props) !tys in
        s |: set (External.of_hashtbl encoded)
    | T.Dict (ty, _) ->
        let$ encoded = ("encoded", hashtbl_create ()) in
        let s =
          hashtbl_iter (Data.to_hashtbl props) (fun k props ->
              encode ~set:(fun x -> encoded.%{k} <- x) props ty)
        in
        s |: set (External.of_hashtbl encoded)
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
          let s = encoded.%{key} <- to_extern tag in
          let s = s |: encode_record_aux encoded props !tys in
          s |: set (External.of_hashtbl encoded))
        ~else_:
          (match seq () with
          | Seq.Cons (hd, seq) -> fun () -> aux hd seq
          | Seq.Nil -> (
              match row with
              | `Closed -> fun () -> unit
              | `Open ->
                  fun () ->
                    let$ encoded = ("encoded", hashtbl_create ()) in
                    let s = encoded.%{key} <- to_extern tag in
                    s |: set (External.of_hashtbl encoded)))
    in
    match cases () with Seq.Nil -> unit | Seq.Cons (hd, seq) -> aux hd seq

  and encode_record_aux encoded props tys =
    Map.String.to_seq tys
    |> Seq.map (fun (k, ty) ->
           let k = string k in
           let$ props = ("props", props.%{k}) in
           encode ~set:(fun x -> encoded.%{k} <- x) props ty)
    |> join_stmts

  let decode_error buf name =
    lambda (fun stack ->
        return
          (lambda (fun ty ->
               return
                 (lambda (fun input ->
                      let s =
                        if_
                          (not (equal_int (buffer_length buf) (int 0)))
                          ~then_:(fun () ->
                            buffer_add_string buf (string "\n\n"))
                      in
                      let s = s |: buffer_add_string buf (string "File \"") in
                      let s = s |: buffer_add_string buf (string name) in
                      let s =
                        s
                        |: buffer_add_string buf
                             (string
                                "\"\n\
                                 Render error.\n\
                                 The data supplied does not match this \
                                 template's interface.\n")
                      in
                      let s = s |: buffer_add_string buf (string "Path:\n") in
                      let s = s |: buffer_add_stack buf stack (string " <- ") in
                      let s =
                        s |: buffer_add_string buf (string "\nExpected type:\n")
                      in
                      let s = s |: buffer_add_string buf ty in
                      let s =
                        s
                        |: buffer_add_string buf (string "\nReceived value:\n")
                      in
                      s |: buffer_add_string buf (External.show input))))))

  let key_error buf name =
    lambda (fun stack ->
        return
          (lambda (fun ty ->
               return
                 (lambda (fun keys ->
                      let s =
                        if_
                          (not (equal_int (buffer_length buf) (int 0)))
                          ~then_:(fun () ->
                            buffer_add_string buf (string "\n\n"))
                      in
                      let s = s |: buffer_add_string buf (string "File: ") in
                      let s = s |: buffer_add_string buf (string name) in
                      let s =
                        s
                        |: buffer_add_string buf
                             (string
                                "\n\
                                 Render error.\n\
                                 The data supplied does not match this \
                                 template's interface.\n")
                      in
                      let s = s |: buffer_add_string buf (string "Path:\n") in
                      let s = s |: buffer_add_stack buf stack (string " <- ") in
                      let s =
                        s |: buffer_add_string buf (string "\nExpected type:\n")
                      in
                      let s = s |: buffer_add_string buf ty in
                      let s =
                        s
                        |: buffer_add_string buf
                             (string "\nInput is missing keys:\n")
                      in
                      s |: buffer_add_stack buf keys (string ", "))))))

  let eval compiled =
    let$ components = ("components", hashtbl_create ()) in
    let s =
      Map.String.to_seq compiled.Compile.externals
      |> Seq.map (fun (k, (tys, v)) ->
             import v (fun import ->
                 components.%{string k} <-
                   lambda (fun props ->
                       let$ encoded = ("encoded", hashtbl_create ()) in
                       let s = encode_record_aux encoded props tys in
                       s |: return (import @@ External.of_hashtbl encoded))))
      |> join_stmts
    in
    let s =
      s
      |: (Map.String.to_seq compiled.components
         |> Seq.map (fun (k, v) ->
                components.%{string k} <-
                  lambda (fun props ->
                      let$ sync = ("buf_sync", buffer_create ()) in
                      let& async = ("buf_async", promise (buffer_create ())) in
                      let buffer = { sync; async } in
                      let s = nodes { components; buffer } props v in
                      s |: return (buffer_contents_async buffer)))
         |> join_stmts)
    in
    s
    |: export
         (lambda (fun input ->
              let$ errors = ("errors", buffer_create ()) in
              let$ decode_error =
                ("decode_error", decode_error errors compiled.name)
              in
              let$ key_error = ("key_error", key_error errors compiled.name) in
              let$ props = ("props", hashtbl_create ()) in
              let$ stack = ("stack", stack_singleton (string "<input>")) in
              let debug = { stack; decode_error; key_error } in
              let$ ty_str =
                ("type", show_type (T.record (ref compiled.types)))
              in
              let s =
                External.classify Assoc input
                  ~ok:(fun input ->
                    decode_record_aux ~debug props input compiled.types ty_str)
                  ~error:(fun () -> push_error debug ty_str input)
              in
              s
              |: if_else
                   (equal_int (buffer_length errors) (int 0))
                   ~then_:(fun () ->
                     let$ sync = ("buf_sync", buffer_create ()) in
                     let& async = ("buf_async", promise (buffer_create ())) in
                     let buffer = { sync; async } in
                     let s =
                       nodes { components; buffer } props compiled.nodes
                     in
                     s |: return (buffer_contents_async buffer))
                   ~else_:(fun () -> return (error (buffer_contents errors)))))

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
  let stmt x = fwds (F.stmt (bwde x))

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

  let while_ f b g =
    fwds (F.while_ (fun b -> bwde (f (fwde b))) b (fun () -> bwds (g ())))

  let unit = fwds F.unit
  let not x = fwde (F.not (bwde x))
  let int x = fwde (F.int x)
  let float x = fwde (F.float x)
  let string x = fwde (F.string x)
  let bool x = fwde (F.bool x)
  let equal_int a b = fwde (F.equal_int (bwde a) (bwde b))
  let equal_string a b = fwde (F.equal_string (bwde a) (bwde b))
  let string_of_int x = fwde (F.string_of_int (bwde x))
  let float_of_int x = fwde (F.float_of_int (bwde x))
  let string_of_float x = fwde (F.string_of_float (bwde x))
  let string_of_bool x = fwde (F.string_of_bool (bwde x))
  let promise x = fwde (F.promise (bwde x))
  let bind p f = fwde (F.bind (bwde p) (bwde f))
  let error s = fwde (F.error (bwde s))
  let import i f = fwds (F.import i (fun fi -> bwds (f (fwde fi))))
  let export x = fwds (F.export (bwde x))
  let array x = fwde (F.array (Array.map bwde x))
  let array_make i x = fwde (F.array_make (bwde i) (bwde x))
  let ( .%() ) a i = fwde F.((bwde a).%(bwde i))
  let ( .%()<- ) a i x = fwds F.((bwde a).%(bwde i) <- bwde x)
  let hashtbl x = fwde (F.hashtbl (Seq.map (fun (a, b) -> (bwde a, bwde b)) x))
  let hashtbl_create () = fwde (F.hashtbl_create ())
  let ( .%{} ) h k = fwde F.((bwde h).%{bwde k})
  let ( .%{}<- ) h k x = fwds F.((bwde h).%{bwde k} <- bwde x)
  let hashtbl_mem h k = fwde (F.hashtbl_mem (bwde h) (bwde k))
  let hashtbl_copy h = fwde (F.hashtbl_copy (bwde h))

  let hashtbl_iter h f =
    fwds (F.hashtbl_iter (bwde h) (fun k v -> bwds (f (fwde k) (fwde v))))

  let buffer_create () = fwde (F.buffer_create ())
  let buffer_add_string b s = fwds (F.buffer_add_string (bwde b) (bwde s))
  let buffer_add_buffer b s = fwds (F.buffer_add_buffer (bwde b) (bwde s))
  let buffer_add_escape b s = fwds (F.buffer_add_escape (bwde b) (bwde s))
  let buffer_contents b = fwde (F.buffer_contents (bwde b))
  let buffer_clear b = fwds (F.buffer_clear (bwde b))
  let buffer_length b = fwde (F.buffer_length (bwde b))

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
    let stmt = F.dprintf "(@[stmt@ %t@])"

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

    let while_ f b g =
      F.dprintf "(@[while@ %t@ (@[<hv>%t@])@])" (f (deref b)) (g ())

    let unit = F.dprintf "(unit)"
    let not = F.dprintf "(@[not@ %t@])"
    let int = F.dprintf "%i"
    let float = F.dprintf "%g"
    let string = F.dprintf "%S"
    let bool = F.dprintf "%B"
    let equal_int = F.dprintf "(@[equal_int@ %t@ %t@])"
    let equal_string = F.dprintf "(@[equal_string@ %t@ %t@])"
    let string_of_int = F.dprintf "(@[string_of_int@ %t@])"
    let float_of_int = F.dprintf "(@[float_of_int@ %t@])"
    let string_of_float = F.dprintf "(@[string_of_float@ %t@])"
    let string_of_bool = F.dprintf "(@[string_of_bool@ %t@])"

    type 'a promise

    let promise = F.dprintf "(@[promise@ %t@])"
    let bind = F.dprintf "(@[bind@ %t@ %t@])"
    let error = F.dprintf "(@[error@ %t@])"

    type external_data
    type import = a

    let import i f =
      let name = var "import" in
      F.dprintf "(@[import@ %t@ from@ %a@])@ %t" name pp_import i (f name)

    let export = F.dprintf "(@[export@ %t@])"

    let array a =
      F.dprintf "[@[<hv>%a@]]"
        (F.pp_print_seq ~pp_sep:Pp.comma ( |> ))
        (Array.to_seq a)

    let array_make = F.dprintf "(@[array_make@ %t@ %t@])"
    let bindop_get = F.dprintf "(@[%t@,.%%%c@[@,%t@,@]%c@])"
    let bindop_set = F.dprintf "(@[%t@,.%%%c@[@,%t@,@]%c@ <-@ %t@])"
    let ( .%() ) a i = bindop_get a '(' i ')'
    let ( .%()<- ) a i v = bindop_set a '(' i ')' v

    type 'a hashtbl

    let hashtbl =
      F.dprintf "(@[hashtbl@ [@[<hv>%a@]]@])"
        (F.pp_print_seq ~pp_sep:Pp.comma (fun ppf (a, b) ->
             F.fprintf ppf "(@[%t,@ %t@])" a b))

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

    type buffer

    let buffer_create () = F.dprintf "(buffer_create)"
    let buffer_add_string = F.dprintf "(@[buffer_add_string@ %t@ %t@])"
    let buffer_add_buffer = F.dprintf "(@[buffer_add_buffer@ %t@ %t@])"
    let buffer_add_escape = F.dprintf "(@[buffer_add_escape@ %t@ %t@])"
    let buffer_contents = F.dprintf "(@[buffer_contents@ %t@])"
    let buffer_clear = F.dprintf "(@[buffer_clear@ %t@])"
    let buffer_length = F.dprintf "(@[buffer_length@ %t@])"

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
