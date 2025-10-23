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
    first-class functions, and asynchronous monads (i.e. Promises). *)

module type SEM = sig
  (** Define the semantics of our abstract language. *)

  (** {1 Statements and expressions.} *)

  type 'a stm
  (** A statement, generally some side-effecting code. *)

  type 'a obs
  (** The final evaluation result. *)

  val observe : 'a stm -> 'a obs
  (** Observe the evaluation result after all transformations have applied. *)

  type 'a exp
  (** An expression. *)

  val return : 'a exp -> 'a stm
  (** Return value ['a] from a function. *)

  val stm : unit exp -> unit stm

  type 'a ref
  (** A mutable reference variable. This is not compatible with expressions. *)

  val ignore : unit stm -> (unit -> 'a stm) -> 'a stm
  (** Evaluate a unit statement. *)

  val let_ : string -> 'a exp -> ('a exp -> 'b stm) -> 'b stm
  (** Define an immutable binding. The string is used for pretty-printing. *)

  val ref : string -> 'a exp -> ('a ref -> 'b stm) -> 'b stm
  (** Define a reference variable. The string is used for pretty-printing. *)

  val ( ! ) : 'a ref -> 'a exp
  val ( := ) : 'a ref -> 'a exp -> unit stm
  val incr : int ref -> unit stm

  (** {1 Functions.} *)

  val lambda : ('a exp -> 'b stm) -> ('a -> 'b) exp
  val ( @@ ) : ('a -> 'b) exp -> 'a exp -> 'b exp

  (** {1 Control flow.} *)

  val if_else :
    bool exp -> then_:(unit -> 'a stm) -> else_:(unit -> 'a stm) -> 'a stm

  val while_ : ('a exp -> bool exp) -> 'a ref -> (unit -> unit stm) -> unit stm

  (** {1 Standard values.} *)

  val unit : unit stm
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
    nil:(unit -> 'b stm) ->
    cons:('a exp -> 'a Seq.t exp -> 'b stm) ->
    'b stm

  val generator : (('a exp -> unit stm) -> unit stm) -> 'a Seq.t exp
  (** Create an ephemeral sequence from a function. *)

  val iter : 'a Seq.t exp -> ('a exp -> unit stm) -> unit stm

  (** {1 Strings and characters.} *)

  val string_to_seq : string exp -> char Seq.t exp

  val escape :
    char exp -> (string exp -> unit stm) -> (char exp -> unit stm) -> unit stm
  (** For a given character, call a function with its replacement string or
      character. The replacement can be the same character. *)

  (** {1 Arrays.} *)

  val array : 'a exp array -> 'a array exp
  val array_make : int -> 'a exp -> 'a array exp
  val ( .%() ) : 'a array exp -> int exp -> 'a exp
  val ( .%()<- ) : 'a array exp -> int exp -> 'a exp -> unit stm

  (** {1 Hash tables.} *)

  type 'a hashtbl := 'a Hashtbl.MakeSeeded(String).t

  val hashtbl : (string exp * 'a exp) Seq.t -> 'a hashtbl exp
  val hashtbl_create : unit -> 'a hashtbl exp
  val ( .%{} ) : 'a hashtbl exp -> string exp -> 'a exp
  val ( .%{}<- ) : 'a hashtbl exp -> string exp -> 'a exp -> unit stm
  val hashtbl_mem : 'a hashtbl exp -> string exp -> bool exp
  val hashtbl_to_seq : 'a hashtbl exp -> (string * 'a) Seq.t exp

  (** {1 Mutable string buffers.} *)

  val buffer_create : unit -> Buffer.t exp
  val buffer_add_string : Buffer.t exp -> string exp -> unit stm
  val buffer_add_char : Buffer.t exp -> char exp -> unit stm

  val buffer_add_fmt :
    Buffer.t exp -> (Format.formatter -> unit) exp -> unit stm

  val buffer_contents : Buffer.t exp -> string exp
  val buffer_length : Buffer.t exp -> int exp
  val dfmt : (Format.formatter -> unit) -> (Format.formatter -> unit) exp

  (** {1 Promises.} *)

  type 'a promise
  (** An asynchronous computation. *)

  val await : 'a promise exp -> 'a exp

  val async_lambda : ('a exp -> 'b stm) -> ('a -> 'b promise) exp
  (** This is necessary for JavaScript async/await syntax compatibility. *)

  (** {1 Results.} *)

  val errors_of_seq : string Seq.t exp -> Error.t list exp
  val errors_is_empty : Error.t list exp -> bool exp
  val ok : 'a exp -> ('a, 'e) result exp
  val error : 'e exp -> ('a, 'e) result exp

  (** {1 Data.} *)

  type untyped

  module type UNTYPED = sig
    type t

    val inject : t exp -> untyped exp
    val project : untyped exp -> t exp
    val test : untyped exp -> bool exp
  end

  val untyped :
    string -> ((module UNTYPED with type t = 'a) -> 'b stm) -> 'b stm
  (** Make a new untyped wrapper. The string is used for pretty-printing. *)

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
      ok:('a exp -> 'b stm) ->
      error:(unit -> 'b stm) ->
      'b stm

    val to_string : t exp -> string exp
    val marshal : 'a exp -> t exp
  end

  (** {1 Importing and exporting.} *)

  type import
  (** Information to import a function from external code. *)

  val import :
    import -> ((External.t -> string promise) exp -> 'a stm) -> 'a stm

  val export : 'a exp -> 'a stm
end

(** Create evaluation instructions for a given language implementation. *)
module Make (I : SEM) : sig
  open I

  val eval :
    import Compile.t ->
    (External.t -> (string, Error.t list) result promise) obs
  (** Evaluate a template with the language implemented by {!I}. *)
end = struct
  (* NOTE: OCaml's mutability, exceptions, and effects are not guaranteed to
     work with [I]. This is particularly true if [I] is a pretty-printer. Do not
     rely on those features when crossing the boundaries of [I]'s functions. *)
  module Map_int = Map.Make (Int)
  module Map_string = Map.Make (String)
  module Set_int = Set.Make (Int)
  module Set_string = Set.Make (String)
  open I

  type 'a hashtbl = 'a Hashtbl.MakeSeeded(String).t

  let unset_exit = int (-1)
  let list_hd e = e.%(int 0)
  let list_tl e = e.%(int 1)
  let int_to_bool i = not (i = int 0)
  let if_ x ~then_ = if_else x ~then_ ~else_:(fun () -> unit)
  let ( let@ ) = Stdlib.( @@ )
  let ( let| ) = ignore

  let stm_join =
    let rec aux s1 seq =
      match seq () with
      | Seq.Nil -> s1
      | Seq.Cons (s2, seq) ->
          let| () = s1 in
          aux s2 seq
    in
    fun seq ->
      match seq () with Seq.Nil -> unit | Seq.Cons (s1, seq) -> aux s1 seq

  (** For convenience, we'll parameterize this section with values that we'll
      define later during evaluation. *)
  module With_runtime (Runtime : sig
    module UInt : UNTYPED with type t = int
    module UString : UNTYPED with type t = string
    module UFloat : UNTYPED with type t = float
    module UArray : UNTYPED with type t = untyped array
    module UHashtbl : UNTYPED with type t = untyped hashtbl
    module UUnknown : UNTYPED with type t = External.t

    val escape : (Buffer.t -> string -> unit) exp
    val zero : untyped exp
    val one : untyped exp
  end) =
  struct
    open Runtime

    let true_value = one
    let false_value = zero
    let nil_value = zero
    let is_nil = UInt.test
    let is_not_nil x = not (is_nil x)
    let get_nullable e = list_hd (UArray.project e)

    type state = {
      comps : (UHashtbl.t -> string promise) exp Map_string.t;
      buf : Buffer.t exp;
      props : untyped hashtbl exp;  (** The root scope bindings. *)
      scope : untyped exp Map_string.t;
          (** New variables go into the scope to shadow previous variables. *)
    }

    let state buf comps props = { buf; comps; props; scope = Map_string.empty }
    let make_buffer state buf = { state with buf }
    let component { comps; _ } k props = Map_string.find k comps @@ props

    let make_var_scope state hashtbl bindings =
      let scope =
        List.fold_left
          (fun scope s -> Map_string.add s hashtbl.%{string s} scope)
          state.scope bindings
      in
      { state with scope }

    let var { scope; props; _ } k =
      try Map_string.find k scope with Not_found -> props.%{string k}

    let parse_escape state esc x =
      match esc with
      | Compile.No_escape -> buffer_add_string state.buf x
      | Compile.Escape -> stm ((escape @@ state.buf) @@ x)

    let fmt state esc x = function
      | Compile.Fmt_string -> parse_escape state esc (UString.project x)
      | Compile.Fmt_int ->
          parse_escape state esc (string_of_int (UInt.project x))
      | Compile.Fmt_float ->
          parse_escape state esc (string_of_float (UFloat.project x))
      | Compile.Fmt_bool ->
          parse_escape state esc (string_of_bool (int_to_bool (UInt.project x)))

    let rec echo state esc (ech : Compile.echo) k =
      match ech with
      (* Short-circuit the CPS calls on strings to avoid untyped allocations. *)
      | `String s -> parse_escape state esc (string s)
      | `Var x -> k (var state x)
      | `Field (e, s) ->
          let@ tbl = echo state esc e in
          k (UHashtbl.project tbl).%{string s}

    let rec echoes state esc default default_fmt = function
      | [] ->
          let@ e = echo state esc default in
          fmt state esc e default_fmt
      | (f, e) :: tl ->
          let@ e = echo state esc e in
          let@ nullable = let_ "nullable" e in
          if_else (is_nil nullable)
            ~then_:(fun () -> echoes state esc default default_fmt tl)
            ~else_:(fun () -> fmt state esc (get_nullable nullable) f)

    let rec construct_data blocks state (data : Compile.data) =
      match data with
      | `Null -> nil_value
      | `Int i -> UInt.inject (int i)
      | `String s -> UString.inject (string s)
      | `Float f -> UFloat.inject (float f)
      | `Var x -> var state x
      | `Array a -> construct_data_array blocks state a |> UArray.inject
      | `Assoc d -> construct_data_hashtbl blocks state d |> UHashtbl.inject
      | `Block i -> blocks.(i) |> UString.inject
      | `Field (d, s) ->
          (construct_data blocks state d |> UHashtbl.project).%{string s}

    and construct_data_array blocks state a =
      array (Array.map (construct_data blocks state) a)

    and construct_data_hashtbl blocks state d =
      Map_string.to_seq d
      |> Seq.map (fun (k, v) -> (string k, construct_data blocks state v))
      |> hashtbl

    let add_vars ids arg vars =
      Set_int.fold (fun id vars -> Map_int.add id arg vars) ids vars

    let arg_indexed arg index ~optional:_ i f =
      match i with 0 -> f arg | _ -> f index

    let arg_int arg ~optional:_ i f = let_ "match_arg" arg.%(int i) f

    let arg_str arg ~optional key f =
      if optional then
        if_
          (hashtbl_mem arg (string key))
          ~then_:(fun () -> let_ "match_arg" arg.%{string key} f)
      else let_ "match_arg" arg.%{string key} f

    let rec match_tree :
        'leaf 'key.
        exit:int ref ->
        leafstm:(vars:untyped exp Map_int.t -> 'leaf -> unit stm) ->
        get_arg:(optional:bool -> 'key -> (untyped exp -> unit stm) -> unit stm) ->
        vars:untyped exp Map_int.t ->
        ?optional:bool ->
        ('leaf, 'key) Matching.tree ->
        unit stm =
     fun ~exit ~leafstm ~get_arg ~vars ?(optional = false) -> function
      | Matching.Switch { key; ids; cases; wildcard; _ } ->
          let@ arg = get_arg ~optional key in
          let vars = add_vars ids arg vars in
          switchcase ~exit ~leafstm ~get_arg ~vars ~arg ~wildcard cases
      | Matching.Wildcard { key; ids; child } ->
          let@ arg = get_arg ~optional key in
          let vars = add_vars ids arg vars in
          match_tree ~exit ~leafstm ~get_arg ~vars child
      | Matching.Nest { key; ids; child; wildcard } -> (
          let@ arg = get_arg ~optional key in
          let vars = add_vars ids arg vars in
          let s =
            let leafstm ~vars t = match_tree ~exit ~leafstm ~get_arg ~vars t in
            match child with
            | Int_keys tree ->
                match_tree ~exit ~leafstm
                  ~get_arg:(arg_int (UArray.project arg))
                  ~vars tree
            | String_keys tree ->
                match_tree ~exit ~leafstm
                  ~get_arg:(arg_str (UHashtbl.project arg))
                  ~vars tree
          in
          match wildcard with
          | None -> s
          | Some tree ->
              let| () = s in
              if_ (!exit = unset_exit) ~then_:(fun () ->
                  match_tree ~exit ~leafstm ~get_arg ~vars tree))
      | Matching.Nil { key; ids; child } ->
          let@ arg = get_arg ~optional key in
          let vars = add_vars ids arg vars in
          if_ (is_nil arg) ~then_:(fun () ->
              match_tree ~exit ~leafstm ~get_arg ~vars child)
      | Matching.Cons { key; ids; child } ->
          let@ arg = get_arg ~optional key in
          let vars = add_vars ids arg vars in
          if_ (is_not_nil arg) ~then_:(fun () ->
              match_tree ~exit ~leafstm ~get_arg ~vars child)
      | Matching.Nil_or_cons { key; ids; nil; cons } ->
          let@ arg = get_arg ~optional key in
          let vars = add_vars ids arg vars in
          if_else (is_nil arg)
            ~then_:(fun () -> match_tree ~exit ~leafstm ~get_arg ~vars nil)
            ~else_:(fun () -> match_tree ~exit ~leafstm ~get_arg ~vars cons)
      | Matching.Optional { child; next } -> (
          let s =
            match_tree ~exit ~leafstm ~get_arg ~optional:true ~vars child
          in
          match next with
          | None -> s
          | Some next ->
              let| () = s in
              if_ (!exit = unset_exit) ~then_:(fun () ->
                  match_tree ~exit ~leafstm ~get_arg ~vars next))
      | Matching.End leaf -> leafstm ~vars leaf

    and switchcase ~exit ~leafstm ~get_arg ~vars ~arg ~wildcard
        Matching.{ data; if_match; next } =
      if_else
        (match data with
        | `String x -> UString.project arg = string x
        | `Int x -> UInt.project arg = int x
        | `Float x -> UFloat.project arg = float x)
        ~then_:(fun () -> match_tree ~exit ~leafstm ~get_arg ~vars if_match)
        ~else_:
          (match next with
          | Some l ->
              fun () ->
                switchcase ~exit ~leafstm ~get_arg ~vars ~arg ~wildcard l
          | None -> (
              match wildcard with
              | Some l -> fun () -> match_tree ~exit ~leafstm ~get_arg ~vars l
              | None -> fun () -> unit))

    let match_leaf exitvar props ~vars Matching.{ names; exit } =
      let| () =
        Map_string.to_seq names
        |> Seq.map (fun (key, id) ->
               props.%{string key} <- Map_int.find id vars)
        |> stm_join
      in
      exitvar := int exit

    let dummy_props = hashtbl_create ()

    let make_match_props exits f =
      if Matching.Exits.binding_exists exits then
        let_ "match_props" (hashtbl_create ()) f
      else f dummy_props

    let rec node state = function
      | Compile.Text s -> buffer_add_string state.buf (string s)
      | Compile.Echo (echs, fmt, default, esc) ->
          echoes state esc default fmt echs
      | Compile.Match (blocks, data, { tree; exits }) ->
          let@ blocks = construct_blocks state blocks in
          let@ arg_match =
            let_ "arg_match" (construct_data_array blocks state data)
          in
          let@ new_props = make_match_props exits in
          let@ exit = ref "exit" unset_exit in
          let| () =
            match_tree ~exit
              ~leafstm:(match_leaf exit new_props)
              ~get_arg:(arg_int arg_match) ~vars:Map_int.empty tree
          in
          make_exits exit exits state new_props
      | Compile.Map_list (blocks, data, { tree; exits }) ->
          let@ blocks = construct_blocks state blocks in
          let@ index = ref "index" (int 0) in
          let@ cell = ref "cell" (construct_data blocks state data) in
          while_ is_not_nil cell (fun () ->
              let@ new_props = make_match_props exits in
              let@ list = let_ "list" (UArray.project !cell) in
              let@ head = let_ "head" (list_hd list) in
              let@ exit = ref "exit" unset_exit in
              let| () =
                match_tree ~exit
                  ~leafstm:(match_leaf exit new_props)
                  ~get_arg:(arg_indexed head (UInt.inject !index))
                  ~vars:Map_int.empty tree
              in
              let| () = make_exits exit exits state new_props in
              let| () = incr index in
              cell := list_tl list)
      | Compile.Map_dict (blocks, data, { tree; exits }) ->
          let@ blocks = construct_blocks state blocks in
          let@ arg = let_ "match_arg" (construct_data blocks state data) in
          iter
            (hashtbl_to_seq (UHashtbl.project arg))
            (fun p ->
              let k, v = unpair p in
              let@ new_props = make_match_props exits in
              let@ exit = ref "exit" unset_exit in
              let| () =
                match_tree ~exit
                  ~leafstm:(match_leaf exit new_props)
                  ~get_arg:(arg_indexed v (UString.inject k))
                  ~vars:Map_int.empty tree
              in
              make_exits exit exits state new_props)
      | Component (name, blocks, dict) ->
          let@ blocks = construct_blocks state blocks in
          let props = construct_data_hashtbl blocks state dict in
          buffer_add_string state.buf (await (component state name props))

    and construct_blocks state blocks f =
      let blocks_arr = Array.make (Compile.blocks_length blocks) (string "") in
      let rec aux seq =
        match seq () with
        | Seq.Cons ((i, block), seq) ->
            let@ buf = let_ "buf" (buffer_create ()) in
            let state_block = make_buffer state buf in
            let| () = nodes state_block block in
            blocks_arr.(i) <- buffer_contents buf;
            aux seq
        | Seq.Nil -> f blocks_arr
      in
      aux (Compile.blocks_to_seq blocks)

    and make_exits exit exits state props =
      let Nonempty.(next :: tl) = Matching.Exits.to_nonempty exits in
      let rec aux Matching.Exits.{ id; bindings; nodes = n } = function
        | [] -> nodes (make_var_scope state props bindings) n
        | next :: tl ->
            if_else
              (!exit = int id)
              ~then_:(fun () -> nodes (make_var_scope state props bindings) n)
              ~else_:(fun () -> aux next tl)
      in
      aux next tl

    and nodes state l = List.to_seq l |> Seq.map (node state) |> stm_join

    module T = Typechecker.Type

    let rec decode state input ty =
      let@ state = state#ty ty in
      match ty.contents with
      | T.Unknown _ -> state#set (UUnknown.inject input)
      | T.Enum_int ({ cases; _ }, Bool) ->
          External.decode External.get_bool input
            ~ok:(fun b ->
              if_else b
                ~then_:(fun () ->
                  if Set_int.mem 1 cases then state#set true_value
                  else state#yield_error input)
                ~else_:(fun () ->
                  if Set_int.mem 0 cases then state#set false_value
                  else state#yield_error input))
            ~error:(fun () -> state#yield_error input)
      | T.String | T.Enum_string { row = `Open; _ } ->
          External.decode External.get_string input
            ~ok:(fun s -> state#set (UString.inject s))
            ~error:(fun () -> state#yield_error input)
      | T.Enum_string { row = `Closed; cases } ->
          External.decode External.get_string input
            ~ok:(fun s ->
              let rec aux seq =
                match seq () with
                | Seq.Nil -> state#yield_error input
                | Seq.Cons (case, seq) ->
                    if_else
                      (s = string case)
                      ~then_:(fun () -> state#set (UString.inject s))
                      ~else_:(fun () -> aux seq)
              in
              aux (Set_string.to_seq cases))
            ~error:(fun () -> state#yield_error input)
      | T.Int | T.Enum_int ({ row = `Open; _ }, _) ->
          External.decode External.get_int input
            ~ok:(fun i -> state#set (UInt.inject i))
            ~error:(fun () -> state#yield_error input)
      | T.Enum_int ({ row = `Closed; cases }, _) ->
          External.decode External.get_int input
            ~ok:(fun s ->
              let rec aux seq =
                match seq () with
                | Seq.Nil -> state#yield_error input
                | Seq.Cons (case, seq) ->
                    if_else
                      (s = int case)
                      ~then_:(fun () -> state#set (UInt.inject s))
                      ~else_:(fun () -> aux seq)
              in
              aux (Set_int.to_seq cases))
            ~error:(fun () -> state#yield_error input)
      | T.Float ->
          External.decode External.get_float input
            ~ok:(fun f -> state#set (UFloat.inject f))
            ~error:(fun () -> state#yield_error input)
      | T.Nullable ty ->
          External.decode External.get_some input
            ~ok:(fun input ->
              let@ decoded = let_ "decoded" (array [| nil_value |]) in
              let@ state' =
                state#copy (string "<nullable>") ~setter:(fun data ->
                    decoded.%(int 0) <- data)
              in
              let| () = decode state' input ty in
              state#set (UArray.inject decoded))
            ~error:(fun () -> state#set nil_value)
      | T.List ty ->
          External.decode External.get_seq input
            ~ok:(fun seq ->
              let@ i = ref "index" (int 0) in
              let@ decoded =
                let_ "decoded" (array [| nil_value; nil_value |])
              in
              let@ decode_dst = ref "decode_dst" decoded in
              let| () =
                iter seq (fun input ->
                    let@ decode_dst_new =
                      let_ "decode_dst_new" (array [| nil_value; nil_value |])
                    in
                    let@ state =
                      state#copy (string_of_int !i) ~setter:(fun data ->
                          decode_dst_new.%(int 0) <- data)
                    in
                    let| () = decode state input ty in
                    let| () =
                      !decode_dst.%(int 1) <- UArray.inject decode_dst_new
                    in
                    let| () = incr i in
                    decode_dst := decode_dst_new)
              in
              state#set (list_tl decoded))
            ~error:(fun () -> state#yield_error input)
      | T.Tuple tys ->
          External.decode External.get_seq input
            ~ok:(fun seq ->
              let length = List.length tys in
              let@ decoded = let_ "decoded" (array_make length nil_value) in
              let rec aux i seq = function
                | [] -> unit
                | ty :: tl ->
                    uncons seq
                      ~nil:(fun () -> state#yield_error input)
                      ~cons:(fun input seq ->
                        let@ state =
                          state#copy
                            (string_of_int (int i))
                            ~setter:(fun data -> decoded.%(int i) <- data)
                        in
                        let| () = decode state input ty in
                        aux (succ i) seq tl)
              in
              let| () = aux 0 seq tys in
              state#set (UArray.inject decoded))
            ~error:(fun () -> state#yield_error input)
      | T.Record tys ->
          External.decode External.get_assoc input
            ~ok:(fun input ->
              let@ decoded = let_ "decoded" (hashtbl_create ()) in
              let| () = decode_record state decoded input tys.contents in
              state#set (UHashtbl.inject decoded))
            ~error:(fun () -> state#yield_error input)
      | T.Dict (ty, _) ->
          External.decode External.get_assoc input
            ~ok:(fun input ->
              let@ decoded = let_ "decoded" (hashtbl_create ()) in
              iter (External.assoc_to_seq input) (fun p ->
                  let k, input = unpair p in
                  let@ state' =
                    state#copy k ~setter:(fun data -> decoded.%{k} <- data)
                  in
                  let| () = decode state' input ty in
                  state#set (UHashtbl.inject decoded)))
            ~error:(fun () -> state#yield_error input)
      | T.Union_int (key, { cases; row }, Bool) ->
          decode_union External.get_bool
            ~if_equal:(fun extern ty ~then_ ~else_ ->
              match ty with
              | 0 ->
                  if_else (not extern)
                    ~then_:(fun () -> then_ false_value)
                    ~else_
              | _ -> if_else extern ~then_:(fun () -> then_ true_value) ~else_)
            ~if_open:(fun x f ->
              if_else x
                ~then_:(fun () -> f true_value)
                ~else_:(fun () -> f false_value))
            (Map_int.to_seq cases) (string key) row state input
      | T.Union_int (key, { cases; row }, Not_bool) ->
          decode_union External.get_int
            ~if_equal:(fun extern ty ~then_ ~else_ ->
              let ty = int ty in
              if_else (extern = ty)
                ~then_:(fun () -> then_ (UInt.inject ty))
                ~else_)
            ~if_open:(fun x f -> f (UInt.inject x))
            (Map_int.to_seq cases) (string key) row state input
      | T.Union_string (key, { cases; row }) ->
          decode_union External.get_string
            ~if_equal:(fun extern ty ~then_ ~else_ ->
              let ty = string ty in
              if_else (extern = ty)
                ~then_:(fun () -> then_ (UString.inject ty))
                ~else_)
            ~if_open:(fun x f -> f (UString.inject x))
            (Map_string.to_seq cases) (string key) row state input

    and decode_union : type ty extern.
        extern External.decoder ->
        if_equal:
          (extern exp ->
          ty ->
          then_:(untyped exp -> unit stm) ->
          else_:(unit -> unit stm) ->
          unit stm) ->
        if_open:(extern exp -> (untyped exp -> unit stm) -> unit stm) ->
        (ty * T.record) Seq.t ->
        _ =
     fun decoder ~if_equal ~if_open seq key row state input ->
      External.decode External.get_assoc input
        ~ok:(fun input' ->
          if_else
            (External.assoc_mem key input')
            ~then_:(fun () ->
              External.decode decoder
                (External.assoc_find key input')
                ~ok:(fun x ->
                  let@ decoded = let_ "decoded" (hashtbl_create ()) in
                  let rec aux seq =
                    match seq () with
                    | Seq.Nil -> (
                        match row with
                        | `Open -> if_open x (fun x -> decoded.%{key} <- x)
                        | `Closed -> state#yield_error input)
                    | Seq.Cons ((ty_x, tys), seq) ->
                        if_equal x ty_x
                          ~then_:(fun x ->
                            let| () = decoded.%{key} <- x in
                            decode_record state decoded input' tys.contents)
                          ~else_:(fun () -> aux seq)
                  in
                  let| () = aux seq in
                  state#set (UHashtbl.inject decoded))
                ~error:(fun () -> state#yield_error input))
            ~else_:(fun () -> state#yield_error input))
        ~error:(fun () -> state#yield_error input)

    and decode_record state decoded input tys =
      let@ add_missing_key = state#manage_missing_keys in
      Map_string.to_seq tys
      |> Seq.map (fun (k, ty) ->
             let k = string k in
             if_else
               (External.assoc_mem k input)
               ~then_:(fun () ->
                 let@ input = let_ "input" (External.assoc_find k input) in
                 let@ state =
                   state#copy k ~setter:(fun data -> decoded.%{k} <- data)
                 in
                 decode state input ty)
               ~else_:(fun () ->
                 match ty.contents with
                 | Nullable _ | Unknown _ -> decoded.%{k} <- nil_value
                 | _ -> add_missing_key k))
      |> stm_join

    let external_of_int_bool i = External.of_bool (int_to_bool i)

    let rec encode ~set props ty =
      match ty.contents with
      | T.Unknown _ ->
          if_else (UUnknown.test props)
            ~then_:(fun () -> set (UUnknown.project props))
            ~else_:(fun () -> set (External.marshal props))
      | T.Enum_int (_, Bool) -> set (external_of_int_bool (UInt.project props))
      | T.String | T.Enum_string _ ->
          set (External.of_string (UString.project props))
      | T.Int | T.Enum_int _ -> set (External.of_int (UInt.project props))
      | T.Float -> set (External.of_float (UFloat.project props))
      | T.Nullable ty ->
          if_else (is_nil props)
            ~then_:(fun () -> set External.null)
            ~else_:(fun () ->
              let@ props = let_ "props" (get_nullable props) in
              encode ~set:(fun x -> set (External.some x)) props ty)
      | T.List ty ->
          let@ seq =
            let_ "seq"
              (generator (fun yield ->
                   let@ cell = ref "cell" props in
                   while_ is_not_nil cell (fun () ->
                       let@ cell' = let_ "cell" (UArray.project !cell) in
                       let@ props = let_ "props" (list_hd cell') in
                       let| () = cell := list_tl cell' in
                       encode ~set:yield props ty)))
          in
          set (External.of_seq seq)
      | T.Tuple tys ->
          let@ props = let_ "props" (UArray.project props) in
          let@ seq =
            let_ "seq"
              (generator (fun yield ->
                   List.to_seq tys
                   |> Seq.mapi (fun i ty -> encode ~set:yield props.%(int i) ty)
                   |> stm_join))
          in
          set (External.of_seq seq)
      | T.Record tys ->
          let@ seq =
            let_ "seq" (encode_record (UHashtbl.project props) tys.contents)
          in
          set (External.of_seq_assoc seq)
      | T.Dict (ty, _) ->
          let@ seq =
            let_ "seq"
              (generator (fun yield ->
                   iter
                     (hashtbl_to_seq (UHashtbl.project props))
                     (fun p ->
                       let k, props = unpair p in
                       encode ~set:(fun v -> yield (pair (k, v))) props ty)))
          in
          set (External.of_seq_assoc seq)
      | T.Union_int (key, { cases; row }, Bool) ->
          encode_union ~project:UInt.project ~to_extern:external_of_int_bool
            (Map_int.to_seq cases |> Seq.map (fun (k, v) -> (int k, v)))
            row (string key) ~set props
      | T.Union_int (key, { cases; row }, Not_bool) ->
          encode_union ~project:UInt.project ~to_extern:External.of_int
            (Map_int.to_seq cases |> Seq.map (fun (k, v) -> (int k, v)))
            row (string key) ~set props
      | T.Union_string (key, { cases; row }) ->
          encode_union ~project:UString.project ~to_extern:External.of_string
            (Map_string.to_seq cases |> Seq.map (fun (k, v) -> (string k, v)))
            row (string key) ~set props

    and encode_union : type a.
        project:(untyped exp -> a exp) ->
        to_extern:(a exp -> External.t exp) ->
        (a exp * T.record) Seq.t ->
        _ =
     fun ~project ~to_extern cases row key ~set props ->
      let@ props = let_ "props" (UHashtbl.project props) in
      let@ tag = let_ "tag" (props.%{key} |> project) in
      match cases () with
      | Seq.Nil -> unit
      | Seq.Cons (hd, seq) ->
          let rec aux (tag', tys) seq =
            if_else (tag = tag')
              ~then_:(fun () ->
                let@ seq =
                  let_ "seq"
                    (encode_record ~tag:(key, to_extern tag) props tys.contents)
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
                    let@ seq =
                      let_ "seq" (encode_record ?tag props Map_string.empty)
                    in
                    set (External.of_seq_assoc seq)
                | Seq.Cons (hd, seq) -> aux hd seq)
          in
          aux hd seq

    and encode_record ?tag props tys =
      generator (fun yield ->
          let| () = match tag with Some t -> yield (pair t) | None -> unit in
          Map_string.to_seq tys
          |> Seq.map (fun (k, ty) ->
                 let k = string k in
                 encode ~set:(fun v -> yield (pair (k, v))) props.%{k} ty)
          |> stm_join)

    let rec make_comps_external components components_input f =
      match components_input with
      | (k, tys, v) :: tl ->
          import v (fun import ->
              let@ comp =
                let_ k
                  (lambda (fun props ->
                       let@ seq = let_ "seq" (encode_record props tys) in
                       return (import @@ External.of_seq_assoc seq)))
              in
              make_comps_external (Map_string.add k comp components) tl f)
      | [] -> f components

    let rec make_comps components components_input f =
      match components_input with
      | (k, v) :: tl ->
          let@ comp =
            let_ k
              (async_lambda (fun props ->
                   let@ buf = let_ "buf" (buffer_create ()) in
                   let| () = nodes (state buf components props) v in
                   return (buffer_contents buf)))
          in
          make_comps (Map_string.add k comp components) tl f
      | [] -> f components
  end

  let lambdak k f = lambda (fun a -> return (k (f a)))
  let lambda2 f = lambdak lambda f
  let lambda3 f = lambdak lambda2 f
  let lambda4 f = lambdak lambda3 f

  let eval (compiled : 'a Compile.t) =
    let@ escape =
      let_ "buffer_add_escape"
        (lambda2 (fun buf str ->
             iter (string_to_seq str) (fun c ->
                 escape c (buffer_add_string buf) (buffer_add_char buf))))
    in
    let@ buffer_add_sep =
      let_ "buffer_add_sep"
        (lambda3 (fun buf sep str ->
             let| () =
               if_
                 (not (buffer_length buf = int 0))
                 ~then_:(fun () -> buffer_add_string buf sep)
             in
             buffer_add_string buf str))
    in
    (* Use a functional continuation stack to track decode progress. *)
    let@ stack_empty = let_ "stack_empty" (lambda (fun _ -> unit)) in
    let@ stack_is_empty =
      let_ "stack_is_empty"
        (lambda (fun stack ->
             let@ result = ref "result" (bool true) in
             let| () = stm (stack @@ lambda (fun _ -> result := bool false)) in
             return !result))
    in
    let@ stack_add =
      let_ "stack_add"
        (lambda3 (fun x stack f ->
             let| () = (* Use FIFO evaluation. *) stm (stack @@ f) in
             return (f @@ x)))
    in
    let@ error_aux =
      let_ "error_aux"
        (lambda4 (fun msg1 msg2 stack ty ->
             let@ buf = let_ "buf" (buffer_create ()) in
             let| () =
               buffer_add_string buf (string "Error while rendering \"")
             in
             let| () = buffer_add_string buf (string compiled.fname) in
             let| () =
               buffer_add_string buf
                 (string
                    "\".\n\
                     The data supplied does not match this template's interface.\n")
             in
             let| () = buffer_add_string buf (string "Path:\n<input>") in
             let| () =
               stm (stack @@ (buffer_add_sep @@ buf) @@ string " -> ")
             in
             let| () = buffer_add_string buf (string "\nExpected type:\n") in
             let| () = buffer_add_fmt buf ty in
             let| () = buffer_add_string buf msg1 in
             let| () = buffer_add_string buf msg2 in
             return (buffer_contents buf)))
    in
    let@ decode_error =
      let_ "decode_error"
        (lambda (fun input ->
             return
               ((error_aux @@ string "\nReceived value:\n")
               @@ External.to_string input)))
    in
    let@ key_error =
      let_ "key_error"
        (lambda (fun keys ->
             let@ buf = let_ "buf" (buffer_create ()) in
             let| () = stm (keys @@ (buffer_add_sep @@ buf) @@ string ", ") in
             return
               ((error_aux @@ string "\nInput is missing keys:\n")
               @@ buffer_contents buf)))
    in
    let@ (module UInt : UNTYPED with type t = int) = untyped "int" in
    let@ (module UString : UNTYPED with type t = string) = untyped "string" in
    let@ (module UFloat : UNTYPED with type t = float) = untyped "float" in
    let@ (module UArray : UNTYPED with type t = untyped array) =
      untyped "array"
    in
    let@ (module UHashtbl : UNTYPED with type t = untyped hashtbl) =
      untyped "hashtbl"
    in
    let@ (module UUnknown : UNTYPED with type t = External.t) =
      untyped "unknown"
    in
    let@ zero = let_ "zero" (UInt.inject (int 0)) in
    let@ one = let_ "one" (UInt.inject (int 1)) in
    let open With_runtime (struct
      module UInt = UInt
      module UString = UString
      module UFloat = UFloat
      module UArray = UArray
      module UHashtbl = UHashtbl
      module UUnknown = UUnknown

      let escape = escape
      let zero = zero
      let one = one
    end) in
    let@ components = make_comps_external Map_string.empty compiled.externals in
    let@ components = make_comps components compiled.components in
    export
      (async_lambda (fun input ->
           let@ props = let_ "props" (hashtbl_create ()) in
           let@ ty_fmt =
             let_ "type" (dfmt (fun ppf -> T.pp_interface ppf compiled.types))
           in
           let@ decode_or_errors =
             let_ "decode_or_errors"
               (generator (fun yield ->
                    let state =
                      object
                        val stack = stack_empty
                        val fmt = ty_fmt
                        val setter = fun _ -> unit
                        method set = setter

                        method ty ty f =
                          let@ fmt =
                            let_ "type" (dfmt (fun ppf -> T.pp ppf ty))
                          in
                          f {<fmt>}

                        method yield_error input =
                          yield (((decode_error @@ input) @@ stack) @@ fmt)

                        method copy debug ~setter f =
                          let@ stack =
                            let_ "stack" ((stack_add @@ debug) @@ stack)
                          in
                          f {<stack; setter>}

                        method manage_missing_keys f =
                          let@ missing_keys = ref "missing_keys" stack_empty in
                          let| () =
                            f (fun k ->
                                missing_keys :=
                                  (stack_add @@ k) @@ !missing_keys)
                          in
                          if_
                            (not (stack_is_empty @@ !missing_keys))
                            ~then_:(fun () ->
                              yield
                                (((key_error @@ !missing_keys) @@ stack) @@ fmt))
                      end
                    in
                    External.decode External.get_assoc input
                      ~ok:(fun input ->
                        decode_record state props input compiled.types)
                      ~error:(fun () -> state#yield_error input)))
           in
           let@ errors = let_ "errors" (errors_of_seq decode_or_errors) in
           if_else (errors_is_empty errors)
             ~then_:(fun () ->
               let@ buf = let_ "buf" (buffer_create ()) in
               let| () = nodes (state buf components props) compiled.nodes in
               return (ok (buffer_contents buf)))
             ~else_:(fun () -> return (error errors))))

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

  type 'a from_stm
  type 'a stm

  val fwds : 'a from_stm -> 'a stm
  val bwds : 'a stm -> 'a from_stm
end

(** Apply a transformation module and a semantics module to produce a new
    semantics module that uses the transformation state.

    The module this creates won't do any transformations by itself, and its
    values are defined as identity functions. You need to override specific
    functions to apply transformations. *)
module Make_trans
    (T : TRANS)
    (F : SEM with type 'a exp = 'a T.from_exp and type 'a stm = 'a T.from_stm) :
  SEM
    with type 'a stm = 'a T.stm
     and type 'a obs = 'a F.obs
     and type 'a exp = 'a T.exp
     and type 'a ref = 'a F.ref
     and type 'a promise = 'a F.promise
     and type untyped = F.untyped
     and type External.t = F.External.t
     and type 'a External.assoc = 'a F.External.assoc
     and type 'a External.decoder = 'a F.External.decoder
     and type import = F.import = struct
  open T
  include F

  type 'a exp = 'a T.exp
  type 'a stm = 'a T.stm

  let observe x = F.observe (bwds x)
  let ignore a f = fwds (F.ignore (bwds a) (fun () -> bwds (f ())))
  let return x = fwds (F.return (bwde x))
  let stm x = fwds (F.stm (bwde x))
  let let_ s x f = fwds (F.let_ s (bwde x) (fun x -> bwds (f (fwde x))))
  let ref s x f = fwds (F.ref s (bwde x) (fun x -> bwds (f x)))
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

  let escape c f g =
    fwds
      (F.escape (bwde c)
         (fun x -> bwds (f (fwde x)))
         (fun x -> bwds (g (fwde x))))

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
  let buffer_add_fmt b f = fwds (F.buffer_add_fmt (bwde b) (bwde f))
  let buffer_contents b = fwde (F.buffer_contents (bwde b))
  let buffer_length b = fwde (F.buffer_length (bwde b))
  let dfmt f = fwde (F.dfmt f)
  let await p = fwde (F.await (bwde p))
  let async_lambda f = fwde (F.async_lambda (fun x -> bwds (f (fwde x))))
  let errors_of_seq s = fwde (F.errors_of_seq (bwde s))
  let errors_is_empty s = fwde (F.errors_is_empty (bwde s))
  let ok x = fwde (F.ok (bwde x))
  let error x = fwde (F.error (bwde x))

  module type UNTYPED = sig
    type t

    val inject : t exp -> untyped exp
    val project : untyped exp -> t exp
    val test : untyped exp -> bool exp
  end

  let untyped (type a) name (f : (module UNTYPED with type t = a) -> _) =
    fwds
      (F.untyped name (fun (module M : F.UNTYPED with type t = a) ->
           bwds
             (f
                (module struct
                  type t = M.t

                  let inject a = fwde (M.inject (bwde a))
                  let project a = fwde (M.project (bwde a))
                  let test a = fwde (M.test (bwde a))
                end))))

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
    let marshal x = fwde (F.External.marshal (bwde x))
  end

  let import i f = fwds (F.import i (fun fi -> bwds (f (fwde fi))))
  let export x = fwds (F.export (bwde x))
end

(** Pretty-print the instructions for a compiled template. *)
let pp (type a) ~escape pp_import ppf c =
  let module F = Format in
  let module M = Make (struct
    let value = F.dprintf "%s"
    let call0 s () = F.dprintf "(@[%s@ ())@])" s
    let call1 = F.dprintf "(@[%s@ %t@])"
    let call2 = F.dprintf "(@[%s@ %t@ %t@])"
    let call3 = F.dprintf "(@[%s@ %t@ %t@ %t@])"
    let call4 = F.dprintf "(@[%s@ %t@ %t@ %t@ %t@])"

    let bind name v e f =
      F.dprintf "(@[@[%s@ %s@ =@]@ %t@])@ %t" name v e (f (value v))

    let infix symbol a b = F.dprintf "(@[%t %s@ %t@])" a symbol b
    let block = F.dprintf "(@[<hv>%t@])"
    let block_named = F.dprintf "(@[<hv>%s@ %t@])"
    let fn argf arg f = F.dprintf "(@[%s ->@ @[<hv>%t@]@])" arg (f (argf arg))
    let indexop_get c1 c2 x k = F.dprintf "%t@,.%%%c@[@,%t@,@]%c" x c1 k c2

    let indexop_set c1 c2 x k v =
      F.dprintf "(@[%t@,.%%%c@[@,%t@,@]%c@ <-@ %t@])" x c1 k c2 v

    type 'a stm = F.formatter -> unit
    type 'a obs = F.formatter -> unit

    let observe = Fun.id
    let ignore s f = F.dprintf "%t@ %t" s (f ())

    type 'a exp = F.formatter -> unit

    let return = call1 "return"
    let stm = call1 "stm"
    let let_ = bind "let"

    type 'a ref = F.formatter -> unit

    let ref = bind "ref"
    let ( ! ) = F.dprintf "!%t"
    let ( := ) = infix ":="
    let incr = call1 "incr"
    let lambda f = call1 "lambda" (fn value "arg" f)
    let ( @@ ) = infix "@@"

    let if_else b ~then_ ~else_ =
      call3 "if_else" b
        (block_named "then" (then_ ()))
        (block_named "else" (else_ ()))

    let while_ f b g = call2 "while" (f !b) (block (g ()))
    let unit = value "(unit)"
    let not = call1 "not"
    let int = F.dprintf "%i"
    let float = F.dprintf "%g"
    let string = F.dprintf "%S"
    let bool = F.dprintf "%B"
    let ( = ) = infix "="
    let pair (a, b) = F.dprintf "(@[%t,@ %t@])" a b
    let unpair x = (call1 "fst" x, call1 "snd" x)
    let string_of_int = call1 "string_of_int"
    let string_of_float = call1 "string_of_float"
    let string_of_bool = call1 "string_of_bool"

    let uncons seq ~nil ~cons =
      call3 "uncons" seq
        (block_named "nil" (nil ()))
        (block_named "cons"
           (fn value "hd" (fun hd -> fn value "seq" (fun seq -> cons hd seq))))

    let generator f = call1 "generator" (fn call1 "yield" f)
    let iter s f = call2 "iter" s (fn value "arg" f)
    let string_to_seq = call1 "string_to_seq"

    let escape c on_string default =
      F.dprintf "(@[escape@ %t@ (@[%a@ (@[_ ->@ %t@])@])@])" c
        (F.pp_print_seq ~pp_sep:F.pp_print_space (fun ppf (c, s) ->
             F.fprintf ppf "(@[%C ->@ %t@])" c
               (on_string (F.dprintf "%a" Pp.syntax_string s))))
        escape (default c)

    let array a =
      F.dprintf "[@[<hv>%a@]]" (F.pp_print_array ~pp_sep:Pp.comma ( |> )) a

    let array_make i a = call2 "array_make" (int i) a
    let ( .%() ) = indexop_get '(' ')'
    let ( .%()<- ) = indexop_set '(' ')'

    let hashtbl s =
      call1 "hashtbl"
        (F.dprintf "(@[<hv>%a@])"
           (F.pp_print_seq ~pp_sep:Pp.comma (fun ppf p -> pair p ppf))
           s)

    let hashtbl_create = call0 "hashtbl_create"
    let ( .%{} ) = indexop_get '{' '}'
    let ( .%{}<- ) = indexop_set '{' '}'
    let hashtbl_mem = call2 "hashtbl_mem"
    let hashtbl_to_seq = call1 "hashtbl_to_seq"
    let buffer_create = call0 "buffer_create"
    let buffer_add_string = call2 "buffer_add_string"
    let buffer_add_char = call2 "buffer_add_char"
    let buffer_add_fmt = call2 "buffer_add_fmt"
    let buffer_contents = call1 "buffer_contents"
    let buffer_length = call1 "buffer_length"
    let dfmt f = F.dprintf "(@[dfmt@ %S@])" (F.asprintf "%t" f)

    type 'a promise

    let await = call1 "await"
    let async_lambda f = call1 "async_lambda" (fn value "arg" f)
    let errors_of_seq = call1 "errors_of_seq"
    let errors_is_empty = call1 "errors_is_empty"
    let ok = call1 "ok"
    let error = call1 "error"

    type untyped

    module type UNTYPED = sig
      type t

      val inject : t exp -> untyped exp
      val project : untyped exp -> t exp
      val test : untyped exp -> bool exp
    end

    let untyped (type a) name (f : (module UNTYPED with type t = a) -> _) =
      let inject = "inj_" ^ name in
      let project = "prj_" ^ name in
      let test = "test_" ^ name in
      F.dprintf "(@[module@ (@[%s@ %s@ %s@]) =@ (@[untyped@ %s@])@])@ %t" inject
        project test name
        (f
           (module struct
             type t = a

             let inject = call1 inject
             let project = call1 project
             let test = call1 test
           end))

    module External = struct
      type t

      let null = value "null"
      let some = call1 "External.some"
      let of_int = call1 "External.of_int"
      let of_string = call1 "External.of_string"
      let of_float = call1 "External.of_float"
      let of_bool = call1 "External.of_bool"
      let of_seq = call1 "External.of_seq"
      let of_seq_assoc = call1 "External.of_seq_assoc"

      type 'a assoc

      let assoc_find = call2 "External.assoc_find"
      let assoc_mem = call2 "External.assoc_mem"
      let assoc_to_seq = call1 "External.assoc_to_seq"

      type 'a decoder = F.formatter -> unit

      let get_int = value "int"
      let get_string = value "string"
      let get_float = value "float"
      let get_bool = value "bool"
      let get_some = value "some"
      let get_seq = value "seq"
      let get_assoc = value "assoc"

      let decode c t ~ok ~error =
        call4 "External.decode" c t
          (block_named "ok" (fn value "decoded" ok))
          (block_named "error" (error ()))

      let to_string = call1 "External.to_string"
      let marshal = call1 "External.marshal"
    end

    type import = a

    let import i f =
      let name = value "import" in
      F.dprintf "(@[import@ %t@ from@ %a@])@ %t" name pp_import i (f name)

    let export = call1 "export"
  end) in
  F.fprintf ppf "@[<hv>%t@]" (M.eval c)
