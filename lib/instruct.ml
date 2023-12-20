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
    concrete language like Javascript (which is just another form of
    pretty-printing).

    Our abstract language is roughly based on JavaScript. It has expressions,
    statements, loops, let-bindings, mutable references, hash tables,
    first-class functions, and async monads (i.e. promises). *)

module type SYM = sig
  (** Define the semantics of our abstract language. *)

  (** {1 Statements and expressions.} *)

  type 'a stmt
  (** A statement, generally some side-effecting code. *)

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

  val if_ :
    bool exp ->
    then_:(unit -> unit stmt) ->
    else_:(unit -> unit stmt) option ->
    unit stmt

  val while_ : (unit -> bool exp) -> (unit -> unit stmt) -> unit stmt

  type external_data
  (** Data from the outside world that we need to decode. *)

  type 'a promise
  (** An asynchronous monad. *)

  type import
  (** Information to import a function from external code. *)

  val import :
    import -> ((external_data -> string exp promise) exp -> 'a stmt) -> 'a stmt
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

  val array : 'a exp array -> 'a exp array exp
  val array_init : int exp -> 'a exp -> 'a exp array exp
  val ( .%() ) : 'a exp array exp -> int exp -> 'a exp
  val ( .%()<- ) : 'a exp array exp -> int exp -> 'a exp -> unit stmt
  val array_concat : string exp array exp -> string exp -> string exp

  (** {1 Hash tables.} *)

  type 'a hashtbl
  (** A mutable map of string keys to ['a] values. *)

  val hashtbl : (string * 'a) exp Seq.t -> 'a exp hashtbl exp
  val hashtbl_create : unit -> 'a exp hashtbl exp
  val ( .%{} ) : 'a exp hashtbl exp -> string exp -> 'a exp
  val ( .%{}<- ) : 'a exp hashtbl exp -> string exp -> 'a exp -> unit stmt
  val hashtbl_mem : 'a exp hashtbl exp -> string exp -> bool exp
  val hashtbl_copy : 'a exp hashtbl exp -> 'a exp hashtbl exp

  val hashtbl_iter :
    'a exp hashtbl exp -> (string exp -> 'a exp -> unit stmt) -> unit stmt

  (** {1 Promises.} *)

  val promise : 'a exp -> 'a exp promise exp

  val bind_array :
    string exp promise exp array exp ->
    (string exp array -> 'a promise) exp ->
    'a promise exp

  (** {1 Buffers.} *)

  type buffer
  (** This type is not a typical "buffer" since it must work with promises. The
      language may implement it however is most suitable. *)

  val buffer_create : unit -> buffer exp
  val buffer_add_string : buffer exp -> string exp -> unit stmt
  val buffer_add_promise : buffer exp -> string exp promise exp -> unit stmt

  (** These are lambdas to minimize generated code. *)

  val buffer_to_promise : (buffer -> string exp promise) exp
  val escape : (string -> string) exp

  (** {1 Mutable stacks.} *)

  type 'a stack

  val stack_create : unit -> 'a exp stack exp
  val stack_is_empty : 'a exp stack exp -> bool exp
  val stack_push : 'a exp stack exp -> 'a exp -> unit stmt
  val stack_drop : 'a exp stack exp -> unit stmt
  val stack_concat : string exp stack exp -> string exp -> string exp

  (** {1 Raise errors.} *)

  type error

  val raise : error exp -> 'a stmt
  val error : string exp -> error exp

  (** {1 Runtime data.} *)

  type data
  (** Boxed runtime data. Either a string, an integer, a float, an array, or a
      hash table. *)

  module Data : sig
    type t = data exp

    val int : int exp -> t
    val float : float exp -> t
    val string : string exp -> t
    val array : t array exp -> t
    val hashtbl : t hashtbl exp -> t
    val unknown : external_data exp -> t
    val to_int : t -> int exp
    val to_float : t -> float exp
    val to_string : t -> string exp
    val to_array : t -> t array exp
    val to_hashtbl : t -> t hashtbl exp
    val equal : t -> t -> bool exp
  end

  module External : sig
    (** The foreign data before it's parsed into [Data.t]. *)

    module Linear : sig
      (** Some kind of one-dimensional data container, e.g. an array. *)

      type 'a t

      val length : external_data exp t -> int exp

      val iteri :
        external_data exp t ->
        (int exp -> external_data exp -> unit stmt) ->
        unit stmt
    end

    module Assoc : sig
      (** Some kind of key-value store, e.g a hash table. *)

      type 'a t

      val find : external_data exp t -> string exp -> external_data exp
      val mem : external_data exp t -> string exp -> bool exp

      val iter :
        external_data exp t ->
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
    val of_array : t array exp -> t
    val of_hashtbl : t hashtbl exp -> t
    val of_untyped : data exp -> t

    (** The classification functions are abstract. They may be implemented as
        functions, if/then statements, etc. *)

    val to_int :
      t -> ok:(int exp -> unit stmt) -> error:(unit -> unit stmt) -> unit stmt

    val to_string :
      t ->
      ok:(string exp -> unit stmt) ->
      error:(unit -> unit stmt) ->
      unit stmt

    val to_float :
      t -> ok:(float exp -> unit stmt) -> error:(unit -> unit stmt) -> unit stmt

    val to_bool :
      t -> ok:(bool exp -> unit stmt) -> error:(unit -> unit stmt) -> unit stmt

    val to_linear :
      t ->
      ok:(t Linear.t -> unit stmt) ->
      error:(unit -> unit stmt) ->
      unit stmt

    val to_assoc :
      t -> ok:(t Assoc.t -> unit stmt) -> error:(unit -> unit stmt) -> unit stmt

    val is_null : t -> bool exp
    val show : t -> string exp
  end
end

(** Create evaluation instructions for a given language implementation. *)
module Make (I : SYM) : sig
  open I

  val eval : import Compile.t -> (external_data -> string exp promise) stmt
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
    comps : (data exp hashtbl -> string exp promise) exp hashtbl exp;
    buffer_to_promise : (buffer -> string exp promise) exp;
    escape : (string -> string) exp;
  }

  let join_stmts seq =
    match seq () with
    | Seq.Nil -> unit
    | Seq.Cons (s, seq) -> Seq.fold_left ( |: ) s seq

  let parse_escape runtime buf esc x =
    match esc with
    | C.No_escape -> buffer_add_string buf x
    | C.Escape -> buffer_add_string buf (runtime.escape @@ x)

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
        let e = echo props e in
        if_ (is_not_nil e)
          ~then_:(fun () -> fmt runtime buf esc (get_nullable e) f)
          ~else_:
            (Some
               (fun () -> echoes runtime buf props esc default default_fmt tl))

  let rec construct_data blocks props (data : C.data) =
    match data with
    | `Null -> nil_value
    | `Int i -> Data.int (int i)
    | `String s -> Data.string (string s)
    | `Float f -> Data.float (float f)
    | `Var s -> props.%{string s}
    | `Array a -> construct_data_array blocks props a |> Data.array
    | `Assoc d -> construct_data_hashtbl blocks props d |> Data.hashtbl
    | `Block i -> blocks.%(int i) |> Data.string
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

  let arg_int arg ~optional:_ i f = ( let$ ) ("arg", arg.%(int i)) f

  let arg_str arg ~optional key f =
    if optional then
      if_
        (hashtbl_mem arg (string key))
        ~then_:(fun () -> ( let$ ) ("arg", arg.%{string key}) f)
        ~else_:None
    else ( let$ ) ("arg", arg.%{string key}) f

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
                ~else_:None
            in
            s1 |: s2)
    | M.Nil { key; ids; child } ->
        let@ arg = get_arg ~optional key in
        let vars = add_vars ids arg vars in
        if_ (is_nil arg)
          ~then_:(fun () -> match_tree ~exit ~leafstmt ~get_arg ~vars child)
          ~else_:None
    | M.Cons { key; ids; child } ->
        let@ arg = get_arg ~optional key in
        let vars = add_vars ids arg vars in
        if_ (is_not_nil arg)
          ~then_:(fun () -> match_tree ~exit ~leafstmt ~get_arg ~vars child)
          ~else_:None
    | M.Nil_or_cons { key; ids; nil; cons } ->
        let@ arg = get_arg ~optional key in
        let vars = add_vars ids arg vars in
        if_ (is_nil arg)
          ~then_:(fun () -> match_tree ~exit ~leafstmt ~get_arg ~vars nil)
          ~else_:
            (Some (fun () -> match_tree ~exit ~leafstmt ~get_arg ~vars cons))
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
                ~else_:None
            in
            s1 |: s2)
    | M.End leaf -> leafstmt ~vars leaf

  and switchcase ~exit ~leafstmt ~get_arg ~vars ~arg ~wildcard
      M.{ data; if_match; next } =
    if_
      (Data.equal arg (of_scalar data))
      ~then_:(fun () -> match_tree ~exit ~leafstmt ~get_arg ~vars if_match)
      ~else_:
        (match next with
        | Some l ->
            Some
              (fun () ->
                switchcase ~exit ~leafstmt ~get_arg ~vars ~arg ~wildcard l)
        | None -> (
            match wildcard with
            | Some l ->
                Some (fun () -> match_tree ~exit ~leafstmt ~get_arg ~vars l)
            | None -> None))

  let match_leaf exitvar props ~vars M.{ names; exit } =
    let s1 = exitvar := int (M.Exit.key_to_int exit) in
    let s2 =
      Map.String.to_seq names
      |> Seq.map (fun (key, id) -> props.%{string key} <- Map.Int.find id vars)
      |> join_stmts
    in
    s1 |: s2

  let make_exits exit exits f =
    match M.Exit.to_seqi exits () with
    | Seq.Nil -> Error.internal __POS__ "No exits."
    | Seq.Cons (hd, tl) ->
        let rec aux (i, v) seq =
          match seq () with
          | Seq.Nil -> f v
          | Seq.Cons (hd, tl) ->
              if_
                (equal_int (deref exit) (int (M.Exit.key_to_int i)))
                ~then_:(fun () -> f v)
                ~else_:(Some (fun () -> aux hd tl))
        in
        aux hd tl

  let rec node runtime buffer props = function
    | C.Text s -> buffer_add_string buffer (string s)
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
                let$ arg =
                  ("arg", array [| list_hd list; Data.int (deref index) |])
                in
                let& exit = ("exit", unset_exit) in
                let s1 =
                  match_tree ~exit ~leafstmt:(match_leaf exit props)
                    ~get_arg:(arg_int arg) ~vars:Map.Int.empty tree
                in
                let s2 =
                  make_exits exit exits (fun l -> nodes runtime buffer props l)
                in
                let s3 = incr index in
                let s4 = cell := list_tl list in
                s1 |: s2 |: s3 |: s4))
    | C.Map_dict (blocks, data, { tree; exits }) ->
        construct_blocks runtime buffer blocks props (fun blocks buffer ->
            let$ arg = ("arg", construct_data blocks props data) in
            hashtbl_iter (Data.to_hashtbl arg) (fun k v ->
                let$ props = ("props", hashtbl_copy props) in
                let$ arg = ("arg", array [| v; Data.string k |]) in
                let& exit = ("exit", unset_exit) in
                let s1 =
                  match_tree ~exit ~leafstmt:(match_leaf exit props)
                    ~get_arg:(arg_int arg) ~vars:Map.Int.empty tree
                in
                let s2 =
                  make_exits exit exits (fun l -> nodes runtime buffer props l)
                in
                s1 |: s2))
    | Component (name, _, blocks, dict) ->
        construct_blocks runtime buffer blocks props (fun blocks buffer ->
            buffer_add_promise buffer
              (runtime.comps.%{string name}
              @@ construct_data_hashtbl blocks props dict))

  and construct_blocks runtime buffer raw_blocks props f =
    (* With no blocks, just continue evaluating with a dummy value. *)
    if Array.length raw_blocks = 0 then f (array [||]) buffer
    else
      let$ blocks =
        ( "blocks",
          array_init (int (Array.length raw_blocks)) (promise (string "")) )
      in
      let s1 =
        Array.to_seqi raw_blocks
        |> Seq.map (fun (i, block) ->
               let$ buffer = ("buffer", buffer_create ()) in
               let s1 = nodes runtime buffer props block in
               let s2 =
                 blocks.%(int i) <- runtime.buffer_to_promise @@ buffer
               in
               s1 |: s2)
        |> join_stmts
      in
      let s2 =
        buffer_add_promise buffer
          (bind_array blocks
             (lambda (fun blocks_resolved ->
                  let$ buffer = ("buffer", buffer_create ()) in
                  let s1 = f blocks_resolved buffer in
                  let s2 = return (runtime.buffer_to_promise @@ buffer) in
                  s1 |: s2)))
      in
      s1 |: s2

  and nodes runtime buffer props l =
    List.to_seq l |> Seq.map (node runtime buffer props) |> join_stmts

  type decode_runtime = {
    is_error : bool mut;
    missing_keys : string exp stack exp;
    stack : string exp stack exp;
    decode_error : (string -> string exp stack -> external_data -> error) exp;
    key_error : (string -> string exp stack -> string exp stack -> error) exp;
  }

  let decode_error_aux =
    let b = Buffer.create 64 in
    let ppf = Format.formatter_of_buffer b in
    fun { stack; decode_error; _ } ty input ->
      T.pp ppf ty;
      Format.pp_print_flush ppf ();
      let s = Buffer.contents b in
      Buffer.clear b;
      ((decode_error @@ string s) @@ stack) @@ input

  let error_key_aux =
    let b = Buffer.create 64 in
    let ppf = Format.formatter_of_buffer b in
    fun { stack; key_error; missing_keys; _ } ty ->
      T.pp ppf ty;
      Format.pp_print_flush ppf ();
      let s = Buffer.contents b in
      Buffer.clear b;
      ((key_error @@ string s) @@ missing_keys) @@ stack

  let set_error x = x.is_error := bool true

  let decode_string ~set ~debug input =
    External.to_string input
      ~ok:(fun s -> set (Data.string s))
      ~error:(fun () -> set_error debug)

  let decode_string_enum ~set ~debug input cases =
    External.to_string input
      ~ok:(fun s ->
        let rec aux seq =
          match seq () with
          | Seq.Nil -> set_error debug
          | Seq.Cons (case, seq) ->
              if_
                (equal_string s (string case))
                ~then_:(fun () -> set (Data.string s))
                ~else_:(Some (fun () -> aux seq))
        in
        aux (Set.String.to_seq cases))
      ~error:(fun () -> set_error debug)

  let decode_int ~set ~debug input =
    External.to_int input
      ~ok:(fun i -> set (Data.int i))
      ~error:(fun () -> set_error debug)

  let decode_int_enum ~set ~debug input cases =
    External.to_int input
      ~ok:(fun s ->
        let rec aux seq =
          match seq () with
          | Seq.Nil -> set_error debug
          | Seq.Cons (case, seq) ->
              if_
                (equal_int s (int case))
                ~then_:(fun () -> set (Data.int s))
                ~else_:(Some (fun () -> aux seq))
        in
        aux (Set.Int.to_seq cases))
      ~error:(fun () -> set_error debug)

  let decode_bool ~set ~debug input cases =
    External.to_bool input
      ~ok:(fun b ->
        if_ b
          ~then_:(fun () ->
            if Set.Int.mem 1 cases then set (Data.int (int 1))
            else set_error debug)
          ~else_:
            (Some
               (fun () ->
                 if Set.Int.mem 0 cases then set (Data.int (int 0))
                 else set_error debug)))
      ~error:(fun () -> set_error debug)

  let decode_float ~set ~debug input =
    External.to_float input
      ~ok:(fun f -> set (Data.float f))
      ~error:(fun () ->
        External.to_int input
          ~ok:(fun i -> set (Data.float (int_to_float i)))
          ~error:(fun () -> set_error debug))

  type 'a union_helper = {
    equal : 'a exp -> 'a exp -> bool exp;
    to_data : 'a exp -> Data.t;
    of_data : Data.t -> 'a exp;
    to_extern : 'a exp -> external_data exp;
    of_extern :
      External.t ->
      ok:('a exp -> unit stmt) ->
      error:(unit -> unit stmt) ->
      unit stmt;
  }

  let union_helper_string =
    {
      equal = equal_string;
      to_data = Data.string;
      of_data = Data.to_string;
      to_extern = External.of_string;
      of_extern = External.to_string;
    }

  let union_helper_int =
    {
      equal = equal_int;
      to_data = Data.int;
      of_data = Data.to_int;
      to_extern = External.of_int;
      of_extern = External.to_int;
    }

  let union_helper_bool = { union_helper_int with to_extern = External.of_bool }

  let rec decode_typescheme ~set ~debug input ty =
    let s1 =
      match !ty with
      | T.Unknown _ -> set (Data.unknown input)
      | T.Enum_int ({ cases; _ }, Bool) -> decode_bool ~set ~debug input cases
      | T.String | T.Enum_string { row = `Open; _ } ->
          decode_string ~set ~debug input
      | T.Enum_string { row = `Closed; cases } ->
          decode_string_enum ~set ~debug input cases
      | T.Int | T.Enum_int ({ row = `Open; _ }, _) ->
          decode_int ~set ~debug input
      | T.Enum_int ({ row = `Closed; cases }, _) ->
          decode_int_enum ~set ~debug input cases
      | T.Float -> decode_float ~set ~debug input
      | T.Nullable ty -> decode_nullable ~set ~debug input ty
      | T.List ty -> decode_list ~set ~debug input ty
      | T.Tuple tys -> decode_tuple ~set ~debug input tys
      | T.Record tys ->
          External.to_assoc input
            ~ok:(fun input ->
              let$ decoded = ("decoded", hashtbl_create ()) in
              let s1 = decode_record_aux ~debug decoded input !tys in
              let s2 = set (Data.hashtbl decoded) in
              s1 |: s2)
            ~error:(fun () -> set_error debug)
      | T.Dict (ty, _) -> decode_dict ~set ~debug input ty
      | T.Union_int (key, { cases; row = _ }, Bool) ->
          let key = string key in
          External.to_assoc input
            ~ok:(fun input ->
              let aux i v () =
                match Map.Int.find_opt i cases with
                | Some tys ->
                    let$ decoded = ("decoded", hashtbl_create ()) in
                    let s1 = decoded.%{key} <- v in
                    let s2 = decode_record_aux ~debug decoded input !tys in
                    let s3 = set (Data.hashtbl decoded) in
                    s1 |: s2 |: s3
                | None -> set_error debug
              in
              if_
                (External.Assoc.mem input key)
                ~then_:(fun () ->
                  External.to_bool
                    (External.Assoc.find input key)
                    ~ok:(fun b ->
                      if_ b ~then_:(aux 1 true_value)
                        ~else_:(Some (aux 0 false_value)))
                    ~error:(fun () -> set_error debug))
                ~else_:(Some (fun () -> set_error debug)))
            ~error:(fun () -> set_error debug)
      | T.Union_int (key, { cases; row }, Not_bool) ->
          decode_union union_helper_int
            (Map.Int.to_seq cases |> Seq.map (fun (k, v) -> (int k, v)))
            ~set ~debug key input row
      | Union_string (key, { cases; row }) ->
          decode_union union_helper_string
            (Map.String.to_seq cases |> Seq.map (fun (k, v) -> (string k, v)))
            ~set ~debug key input row
    in
    let s2 =
      if_ (deref debug.is_error)
        ~then_:(fun () -> raise (decode_error_aux debug ty input))
        ~else_:None
    in
    s1 |: s2

  and decode_union : 'a. 'a union_helper -> ('a exp * T.record) Seq.t -> _ =
   fun { equal; to_data; of_extern; _ } seq ~set ~debug key input row ->
    let key' = string key in
    External.to_assoc input
      ~ok:(fun input ->
        if_
          (External.Assoc.mem input key')
          ~then_:(fun () ->
            of_extern
              (External.Assoc.find input key')
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
                      | `Closed -> set_error debug)
                  | Seq.Cons ((ty_i, tys), seq) ->
                      if_ (equal i ty_i)
                        ~then_:(fun () ->
                          let$ decoded = ("decoded", hashtbl_create ()) in
                          let s1 = decoded.%{key'} <- to_data i in
                          let s2 =
                            decode_record_aux ~debug decoded input !tys
                          in
                          let s3 = set (Data.hashtbl decoded) in
                          s1 |: s2 |: s3)
                        ~else_:(Some (fun () -> aux seq))
                in
                aux seq)
              ~error:(fun () -> set_error debug))
          ~else_:(Some (fun () -> set_error debug)))
      ~error:(fun () -> set_error debug)

  and decode_tuple ~set ~debug input tys =
    let length = int (List.length tys) in
    External.to_linear input
      ~ok:(fun l ->
        if_
          (equal_int (External.Linear.length l) length)
          ~then_:(fun () ->
            let$ decoded = ("decoded", array_init length nil_value) in
            External.Linear.iteri l (fun i input ->
                let rec aux i' = function
                  | [] -> set_error debug
                  | ty :: tl ->
                      if_
                        (equal_int i (int i'))
                        ~then_:(fun () ->
                          decode_typescheme
                            ~set:(fun data -> decoded.%(i) <- data)
                            ~debug input ty)
                        ~else_:(Some (fun () -> aux (succ i') tl))
                in
                let s1 = stack_push debug.stack (int_to_string i) in
                let s2 = aux 0 tys in
                let s3 = set (Data.array decoded) in
                let s4 = stack_drop debug.stack in
                s1 |: s2 |: s3 |: s4))
          ~else_:(Some (fun () -> set_error debug)))
      ~error:(fun () -> set_error debug)

  and decode_dict ~set ~debug input ty =
    External.to_assoc input
      ~ok:(fun a ->
        let$ decoded = ("decoded", hashtbl_create ()) in
        External.Assoc.iter a (fun k input ->
            let s1 = stack_push debug.stack k in
            let s2 =
              decode_typescheme
                ~set:(fun data -> decoded.%{k} <- data)
                ~debug input ty
            in
            let s3 = set (Data.hashtbl decoded) in
            let s4 = stack_drop debug.stack in
            s1 |: s2 |: s3 |: s4))
      ~error:(fun () -> set_error debug)

  and decode_list ~set ~debug input ty =
    External.to_linear input
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
                decode_typescheme
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
      ~error:(fun () -> set_error debug)

  and decode_nullable ~set ~debug input ty =
    if_ (External.is_null input)
      ~then_:(fun () -> set nil_value)
      ~else_:
        (Some
           (fun () ->
             let$ decoded = ("decoded", array [| nil_value |]) in
             let s1 = stack_push debug.stack (string "<nullable>") in
             let s2 =
               decode_typescheme
                 ~set:(fun data -> decoded.%(int 0) <- data)
                 ~debug input ty
             in
             let s3 = set (Data.array decoded) in
             let s4 = stack_drop debug.stack in
             s1 |: s2 |: s3 |: s4))

  and decode_record_aux ~debug decoded input tys =
    let s1 =
      Map.String.to_seq tys
      |> Seq.map (fun (k, ty) ->
             let k' = string k in
             if_
               (External.Assoc.mem input k')
               ~then_:(fun () ->
                 let$ input = ("input", External.Assoc.find input k') in
                 let s1 = stack_push debug.stack k' in
                 let s2 =
                   decode_typescheme
                     ~set:(fun data -> decoded.%{k'} <- data)
                     ~debug input ty
                 in
                 let s3 = stack_drop debug.stack in
                 s1 |: s2 |: s3)
               ~else_:
                 (Some
                    (fun () ->
                      match !ty with
                      | Nullable _ | Unknown _ -> decoded.%{k'} <- nil_value
                      | _ -> stack_push debug.missing_keys k')))
      |> join_stmts
    in
    let s2 =
      if_
        (not (stack_is_empty debug.missing_keys))
        ~then_:(fun () -> raise (error_key_aux debug (T.record (ref tys))))
        ~else_:None
    in
    s1 |: s2

  let rec encode_typescheme ~set props ty =
    match !ty with
    | T.Unknown _ -> set (External.of_untyped props)
    | T.Enum_int (_, Bool) -> set (External.of_bool (Data.to_int props))
    | T.String | T.Enum_string _ ->
        set (External.of_string (Data.to_string props))
    | T.Int | T.Enum_int _ -> set (External.of_int (Data.to_int props))
    | T.Float -> set (External.of_float (Data.to_float props))
    | T.Nullable ty -> encode_nullable ~set props ty
    | T.List ty -> encode_list ~set props ty
    | T.Tuple tys -> encode_tuple ~set props tys
    | T.Record tys ->
        let$ encoded = ("encoded", hashtbl_create ()) in
        let s1 = encode_record_aux encoded (Data.to_hashtbl props) !tys in
        let s2 = set (External.of_hashtbl encoded) in
        s1 |: s2
    | T.Dict (ty, _) -> encode_dict ~set props ty
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
      if_ (equal tag tag')
        ~then_:(fun () ->
          let$ encoded = ("encoded", hashtbl_create ()) in
          let s1 = encoded.%{key} <- to_extern tag in
          let s2 = encode_record_aux encoded props !tys in
          let s3 = set (External.of_hashtbl encoded) in
          s1 |: s2 |: s3)
        ~else_:
          (match seq () with
          | Seq.Cons (hd, seq) -> Some (fun () -> aux hd seq)
          | Seq.Nil -> (
              match row with
              | `Closed -> None
              | `Open ->
                  Some
                    (fun () ->
                      let$ encoded = ("encoded", hashtbl_create ()) in
                      let s1 = encoded.%{key} <- to_extern tag in
                      let s2 = set (External.of_hashtbl encoded) in
                      s1 |: s2)))
    in
    match cases () with Seq.Nil -> unit | Seq.Cons (hd, seq) -> aux hd seq

  and encode_nullable ~set props ty =
    if_ (is_nil props)
      ~then_:(fun () -> set External.null)
      ~else_:
        (Some
           (fun () ->
             let$ props = ("props", get_nullable props) in
             encode_typescheme ~set:(fun x -> set (External.some x)) props ty))

  and encode_list ~set props ty =
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
            let$ props = ("props", deref cell |> Data.to_array |> list_hd) in
            let s1 =
              encode_typescheme
                ~set:(fun x -> encoded.%(deref index) <- x)
                props ty
            in
            let s2 = incr index in
            let s3 = cell := list_tl (Data.to_array (deref cell)) in
            s1 |: s2 |: s3)
      in
      let s4 = set (External.of_array encoded) in
      s1 |: s2 |: s3 |: s4
    in
    s1 |: s2

  and encode_tuple ~set props tys =
    let$ props = ("props", Data.to_array props) in
    let$ encoded =
      ("encoded", array_init (int (List.length tys)) External.null)
    in
    let s1 =
      List.to_seq tys
      |> Seq.mapi (fun i ty ->
             let i = int i in
             let$ props = ("props", props.%(i)) in
             encode_typescheme ~set:(fun x -> encoded.%(i) <- x) props ty)
      |> join_stmts
    in
    let s2 = set (External.of_array encoded) in
    s1 |: s2

  and encode_record_aux encoded props tys =
    Map.String.to_seq tys
    |> Seq.map (fun (k, ty) ->
           let k = string k in
           let$ props = ("props", props.%{k}) in
           encode_typescheme ~set:(fun x -> encoded.%{k} <- x) props ty)
    |> join_stmts

  and encode_dict ~set props ty =
    let$ encoded = ("encoded", hashtbl_create ()) in
    let s1 =
      hashtbl_iter (Data.to_hashtbl props) (fun k props ->
          encode_typescheme ~set:(fun x -> encoded.%{k} <- x) props ty)
    in
    let s2 = set (External.of_hashtbl encoded) in
    s1 |: s2

  let eval compiled =
    let comp_defs, externals =
      Map.String.fold
        (fun k v (comp_defs, externals) ->
          match v with
          | C.Src v -> ((k, v) :: comp_defs, externals)
          | C.Fun (tys, v) -> (comp_defs, (k, tys, v) :: externals))
        compiled.C.components ([], [])
    in
    let$ escape = ("runtime_escape", escape) in
    let$ decode_error =
      ( "decode_error",
        lambda (fun ty ->
            return
              (lambda (fun stack ->
                   return
                     (lambda (fun input ->
                          return
                            (error
                               (array_concat
                                  (array
                                     (Error.decode ~fname:compiled.name
                                        ~stack:
                                          (stack_concat stack (string " <- "))
                                        ~ty ~input:(External.show input) string))
                                  (string "")))))))) )
    in
    let$ key_error =
      ( "key_error",
        lambda (fun ty ->
            return
              (lambda (fun keys ->
                   return
                     (lambda (fun stack ->
                          return
                            (error
                               (array_concat
                                  (array
                                     (Error.missing_keys ~fname:compiled.name
                                        ~stack:
                                          (stack_concat stack (string " <- "))
                                        ~ty
                                        ~keys:(stack_concat keys (string ", "))
                                        string))
                                  (string "")))))))) )
    in
    let$ buffer_to_promise = ("buffer_to_promise", buffer_to_promise) in
    let$ comps = ("components", hashtbl_create ()) in
    let runtime = { escape; comps; buffer_to_promise } in
    let s1 =
      List.to_seq externals
      |> Seq.map (fun (k, tys, v) ->
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
      List.to_seq comp_defs
      |> Seq.map (fun (k, v) ->
             comps.%{string k} <-
               lambda (fun props ->
                   let$ buffer = ("bufer", buffer_create ()) in
                   let s1 = nodes runtime buffer props v in
                   let s2 = return (buffer_to_promise @@ buffer) in
                   s1 |: s2))
      |> join_stmts
    in
    let s3 =
      export
        (lambda (fun input ->
             let$ props = ("props", hashtbl_create ()) in
             let$ stack = ("stack", stack_create ()) in
             let& is_error = ("is_error", bool false) in
             let$ missing_keys = ("missing_keys", stack_create ()) in
             let debug =
               { is_error; missing_keys; stack; decode_error; key_error }
             in
             let s1 = stack_push stack (string "<input>") in
             let s2 =
               External.to_assoc input
                 ~ok:(fun input ->
                   decode_record_aux ~debug props input compiled.types)
                 ~error:(fun () ->
                   raise
                     (decode_error_aux debug
                        (T.record (ref compiled.types))
                        input))
             in
             let s3 =
               let$ buffer = ("buffer", buffer_create ()) in
               let s1 = nodes runtime buffer props compiled.nodes in
               let s2 = return (buffer_to_promise @@ buffer) in
               s1 |: s2
             in
             s1 |: s2 |: s3))
    in
    s1 |: s2 |: s3
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

    let ( |: ) = F.dprintf "%t@ %t"

    type 'a exp = F.formatter -> unit

    let return = F.dprintf "(@[<hv>return@ %t@])"

    let ( let$ ) (v, e) f =
      let v = var v in
      F.dprintf "(@[<hv>@[<hv>let$@ %t@ =@]@ %t@])@ %t" v e (f v)

    type 'a mut = F.formatter -> unit

    let ( let& ) (v, e) f =
      let v = var v in
      F.dprintf "(@[<hv>@[<hv>let&@ %t@ =@]@ %t@])@ %t" v e (f v)

    let deref = F.dprintf "(@[<hv>deref@ %t@])"
    let ( := ) = F.dprintf "(@[<hv>%t@ :=@ %t@])"
    let incr = F.dprintf "(@[<hv>incr@ %t@])"

    let lambda f =
      let arg = var "arg" in
      F.dprintf "(@[<hv>lambda@ %t@ (@[<hv>%t@])@])@ " arg (f arg)

    let ( @@ ) = F.dprintf "(@[<hv>%t@ %@%@ %t@])"

    let if_ b ~then_ ~else_ =
      F.dprintf "(@[<hv>if@ %t@ (@[<hv>then@ %t@])%a@])" b (then_ ())
        (F.pp_print_option (fun ppf else_ ->
             F.fprintf ppf "@ (@[<hv>else@ %t@])" (else_ ())))
        else_

    let while_ b x = F.dprintf "(@[<hv>while@ %t@ (@[<hv>%t@])@])" (b ()) (x ())

    type external_data
    type 'a promise
    type import = a

    let import i f =
      let name = var "import" in
      F.dprintf "(@[<hv>import@ %t@ from@ %a@])@ %t" name pp_import i (f name)

    let export = F.dprintf "(@[<hv>export@ %t@])"
    let unit = F.dprintf "(unit)"
    let not = F.dprintf "(@[<hv>not@ %t@])"
    let int = F.dprintf "%i"
    let float = F.dprintf "%g"
    let string = F.dprintf "%S"
    let bool = F.dprintf "%B"
    let pair (a, b) = F.dprintf "(@[<hv>%t,@ %t@])" a b
    let equal_int = F.dprintf "(@[<hv>equal_int@ %t@ %t@])"
    let equal_string = F.dprintf "(@[<hv>equal_string@ %t@ %t@])"
    let int_to_string = F.dprintf "(@[<hv>int_to_string@ %t@])"
    let int_to_float = F.dprintf "(@[<hv>int_to_float@ %t@])"
    let float_to_string = F.dprintf "(@[<hv>float_to_string@ %t@])"
    let bool_to_string = F.dprintf "(@[<hv>bool_to_string@ %t@])"

    let array a =
      F.dprintf "[@[<hv>%a@]]"
        (F.pp_print_seq ~pp_sep:Pp.comma (fun ppf x -> x ppf))
        (Array.to_seq a)

    let array_init i v = F.dprintf "(@[<hv>array_init@ %t@ %t@])" i v
    let bindop_get = F.dprintf "(@[<hv>%t@,.%%%c@[<hv>@,%t@,@]%c@])"
    let bindop_set = F.dprintf "(@[<hv>%t@,.%%%c@[<hv>@,%t@,@]%c@ <-@ %t@])"
    let ( .%() ) a i = bindop_get a '(' i ')'
    let ( .%()<- ) a i v = bindop_set a '(' i ')' v
    let array_concat = F.dprintf "(@[<hv>array_concat@ %t@ %t@])"

    type 'a hashtbl

    let hashtbl =
      F.dprintf "(@[<hv>hashtbl@ [@[<hv>%a@]]@])"
        (F.pp_print_seq ~pp_sep:Pp.comma (fun ppf x -> x ppf))

    let hashtbl_create () = F.dprintf "(@[<hv>hashtbl_create@])"
    let ( .%{} ) t k = bindop_get t '{' k '}'
    let ( .%{}<- ) t k v = bindop_set t '{' k '}' v
    let hashtbl_mem = F.dprintf "(@[<hv>hashtbl_mem@ %t@ %t@])"
    let hashtbl_copy = F.dprintf "(@[<hv>hashtbl_copy@ %t @])"

    let hashtbl_iter t f =
      let arg_k = var "key" in
      let arg_v = var "value" in
      F.dprintf "(@[<hv>hashtbl_iter@ %t@ (@[<hv>%t@ %t@])@ %t@])" t arg_k arg_v
        (f arg_k arg_v)

    let promise = F.dprintf "(@[<hv>promise@ %t@])"
    let bind_array = F.dprintf "(@[<hv>bind_array@ %t@ %t@@])"

    type buffer

    let buffer_create () = F.dprintf "(@[<hv>buffer_create@])"
    let buffer_add_string = F.dprintf "(@[<hv>buffer_add_string@ %t@ %t@])"
    let buffer_add_promise = F.dprintf "(@[<hv>buffer_add_promise@ %t@ %t@])"
    let buffer_to_promise = F.dprintf "(buffer_to_promise)"
    let escape = F.dprintf "(escape)"

    type 'a stack

    let stack_create () = F.dprintf "(@[<hv>stack_create@])"
    let stack_is_empty = F.dprintf "(@[<hv>stack_is_empty@ %t@])"
    let stack_push = F.dprintf "(@[<hv>stack_push@ %t@ %t@])"
    let stack_drop = F.dprintf "(@[<hv>stack_drop@ %t@])"
    let stack_concat = F.dprintf "(@[<hv>stack_concat@ %t@ %t@])"

    type error = F.formatter -> unit

    let raise = F.dprintf "(@[<hv>raise@ %t@])"
    let error = F.dprintf "(@[<hv>error@ %t@])"

    type data

    module Data = struct
      type t = data exp

      let int = F.dprintf "(@[<hv>Data.int@ %t@])"
      let float = F.dprintf "(@[<hv>Data.float@ %t@])"
      let string = F.dprintf "(@[<hv>Data.string@ %t@])"
      let array = F.dprintf "(@[<hv>Data.array@ %t@])"
      let hashtbl = F.dprintf "(@[<hv>Data.hashtbl@ %t@])"
      let unknown = F.dprintf "(@[<hv>Data.unknown@ %t@])"
      let to_int = F.dprintf "(@[<hv>Data.to_int@ %t@])"
      let to_float = F.dprintf "(@[<hv>Data.to_float@ %t@])"
      let to_string = F.dprintf "(@[<hv>Data.to_string@ %t@])"
      let to_array = F.dprintf "(@[<hv>Data.to_array@ %t@])"
      let to_hashtbl = F.dprintf "(@[<hv>Data.to_hashtbl@ %t@])"
      let equal = F.dprintf "(@[<hv>Data.equal@ %t@ %t@])"
    end

    module External = struct
      module Linear = struct
        type 'a t = F.formatter -> unit

        let length = F.dprintf "(@[<hv>External.Linear.length@ %t@])"

        let iteri a f =
          let arg_k = var "key" in
          let arg_v = var "value" in
          F.dprintf "(@[<hv>External.Linear.iteri@ %t@ (@[<hv>%t@ %t@])@ %t@])"
            a arg_k arg_v (f arg_k arg_v)
      end

      module Assoc = struct
        type 'a t = F.formatter -> unit

        let find = F.dprintf "(@[<hv>External.Assoc.find@ %t@ %t@])"
        let mem = F.dprintf "(@[<hv>External.Assoc.mem@ %t@ %t@])"

        let iter a f =
          let arg_k = var "key" in
          let arg_v = var "value" in
          F.dprintf "(@[<hv>External.Assoc.iter@ %t@ (@[<hv>%t@ %t@])@ %t@])" a
            arg_k arg_v (f arg_k arg_v)
      end

      type t = external_data exp

      let null = F.dprintf "null"
      let some = F.dprintf "(@[<hv>External.some@ %t@])"
      let of_bool = F.dprintf "(@[<hv>External.of_bool@ %t@])"
      let of_int = F.dprintf "(@[<hv>External.of_int@ %t@])"
      let of_string = F.dprintf "(@[<hv>External.of_string@ %t@])"
      let of_float = F.dprintf "(@[<hv>External.of_float@ %t@])"
      let of_array = F.dprintf "(@[<hv>External.of_array@ %t@])"
      let of_hashtbl = F.dprintf "(@[<hv>External.of_hashtbl@ %t@])"
      let of_untyped = F.dprintf "(@[<hv>External.of_untyped@ %t@])"

      let to_aux name t ~ok ~error =
        let arg = var "classified" in
        F.dprintf
          "(@[<hv>External.%s@ %t@ %t@ (@[<hv>ok@ %t@])@ (@[<hv>error@ %t@])@])"
          name t arg (ok arg) (error ())

      let to_int = to_aux "to_int"
      let to_string = to_aux "to_string"
      let to_float = to_aux "to_float"
      let to_bool = to_aux "to_bool"
      let to_linear = to_aux "to_linear"
      let to_assoc = to_aux "to_assoc"
      let is_null = F.dprintf "(@[<hv>External.is_null@ %t@])"
      let show = F.dprintf "(@[<hv>External.show@ %t@])"
    end
  end) in
  F.fprintf ppf "@[<hv>%t@]" (M.eval c)
