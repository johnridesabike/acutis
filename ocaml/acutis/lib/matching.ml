(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open Utils
module TPat = Typechecker.Pattern
module Ty = Typescheme
module Const = Data.Const

(*
  Every pattern represents a one-dimensional path across a multi-dimensional
  data structure. A list of patterns is a two-dimensional matrix of paths. In
  order to transverse these paths efficiently, we need to combine them into a
  tree.

  We take advantage of a few properties that can make our tree simpler. We only
  test discrete static values, integers, strings, etc. We sort record fields
  and replace ommited fields with wildcards, so every pattern "lines up" with
  the others. We also sort the tested values (the integers, strings, etc.).

  Every node has a set of integer "IDs." These keep track of bindings. If a
  pattern has a binding, then we store that binding's ID in its node. Due to
  merging and expanding trees, nodes can have multiple IDs. Each leaf contains
  a map of binding names to their IDs. This is necessary because multiple
  patterns may merge that use different or overlapping binding names.

  The most complicated part of this tree is how we handle wildcards. When we
  merge trees, each wildcard "expands" into its joining node. All of the nodes
  that come after the wildcard will also expand into all of the nodes after the
  other node. This has tradeoffs. One advantage is that we can guarantee that
  every node is only visited once at runtime. One disadvantage is that some
  patterns may produce extremely large trees.

  One feature that this structure gives us is that redundant patterns are
  automatically detected because merging them with an existing tree fails.
The tree type is a polymorphic "nested" type. Each tree can use itself as its own type variable, i.e. tree<tree<'a>>. This allows the end nodes to be fully polymorphic. They can either lead to a leaf or back to their containing tree.
  This nesting corresponds to the nesting of patterns.

  Nested types are simple to create, but complicated to manipulate. Functions
  cannot consume these types under normal polymorphism rules. We need to use
  explicitly polymorphic type annotations and GADTs.
*)

type extra_nest_info = Tuple | Dict | Record | Union of Ty.Variant.extra
[@@deriving eq, show]

type extra_switch_info = Extra_none | Extra_enum_closed [@@deriving eq, show]

type ('leaf, 'key) tree =
  (*
    A Switch represents a list of discreet values to test (i.e., 1, "a", etc.).
    If none of the values match the input, then the wildcard is used.
 *)
  | Switch of {
      key : 'key;
      ids : SetInt.t;
      cases : ('leaf, 'key) switchcase;
      extra : extra_switch_info;
      wildcard : ('leaf, 'key) tree option;
    }
  (*
    A Nest represents a structure such as tuple or a record.
 *)
  | Nest of {
      key : 'key;
      ids : SetInt.t;
      child : ('leaf, 'key) nest;
      wildcard : ('leaf, 'key) tree option;
      extra : extra_nest_info;
    }
  (*
    A Construct represents one of the built-in variant types: lists and
    nullables.
    nil represents an empty list or a null value. It is like a wildcard in that
    it always points to the *next* node, whatever that may be.
    cons always either points to a wildcard node or a Nest + Tuple node.
 *)
  | Construct of {
      key : 'key;
      ids : SetInt.t;
      nil : ('leaf, 'key) tree option;
      cons : ('leaf, 'key) tree option;
      extra : TPat.construct;
    }
  (*
    Wildcards simply point to the next node in the tree.
 *)
  | Wildcard of { key : 'key; ids : SetInt.t; child : ('leaf, 'key) tree }
  | End of 'leaf

and ('leaf, 'key) nest =
  | IntKeys of (('leaf, 'key) tree, int) tree
  | StringKeys of (('leaf, 'key) tree, string) tree

(*
  The switch cases work like linked lists of values. If an input matches a
  value, then we follow its associated tree. If not, we try the next case.
*)
and ('leaf, 'key) switchcase = {
  data : Const.t;
  if_match : ('leaf, 'key) tree;
  next_case : ('leaf, 'key) switchcase option;
}
[@@deriving eq, show]

(*
  Each "exit" is given an integer ID which we can use to look up the AST nodes
  to follow after the tree. We do this because exits can be duplicated when
  trees expand, and we don't want to duplicate entire ASTs.
*)
module Exit = struct
  type key = int

  let equal_key = Int.equal
  let pp_key = Format.pp_print_int

  type 'a t = 'a array

  let get = Array.get
  let map = Array.map
  let unsafe_key i = i
end

type leaf = { names : int MapString.t; exit : Exit.key } [@@deriving eq, show]
type 'a t = { tree : (leaf, int) tree; exits : 'a Exit.t }

(*
  One challenge when merging the trees is that we need to keep track of "where
  we are" in the tree on the type level. This natural-number GADT does that for
  us.
*)

type (_, _) nat =
  | Z : ('z, 'z) nat
  | S : ('a, 'z) nat -> (('a, 'key) tree, 'z) nat

(*
  Whenever we iterate through cases, we do it tail-recursively which creates
  a reversed accumulator. We need to reverse it again back to normal.
*)
let rec rev_cases ?tail t =
  let tail = { t with next_case = tail } in
  match t.next_case with None -> tail | Some t -> rev_cases ~tail t

(*
  If a tree cannot merge with another tree, then we throw that tree away.
  If an entire tree failed to merge, then we know its pattern was redundant.
*)
exception MergeFail

(*
  We sort the data for easier analysis.
  If the new value is lower than the original, then append it to the beginning.
  If the new value equals the original, then merge them.
  If the new value is greater than the original, then check if we're at the end
  of the list. If we are, then add this new case. If not, then keep searching.
*)

let rec merge_testcases_aux :
    type a k.
    ?init:(a, k) switchcase ->
    (a, k) switchcase ->
    Const.t ->
    (a, k) tree ->
    (a, leaf) nat ->
    (a, k) switchcase option =
 fun ?init original data if_match n ->
  let cmp = Const.compare data original.data in
  if cmp < 0 then
    let tail = { data; if_match; next_case = Some original } in
    match init with
    | None -> Some tail
    | Some init -> Some (rev_cases ~tail init)
  else if cmp = 0 then
    try
      let tail =
        { original with if_match = merge original.if_match if_match n }
      in
      match init with
      | None -> Some tail
      | Some init -> Some (rev_cases ~tail init)
    with MergeFail -> None
  else
    let init = { original with next_case = init } in
    match original.next_case with
    | None -> Some (rev_cases ~tail:{ data; if_match; next_case = None } init)
    | Some original -> merge_testcases_aux ~init original data if_match n

(*
  When merging a and b, we take each value from b and try to merge it with the
  items in a. At least one value must merge in order to be considered a success.
*)
and merge_testcases :
    type a k.
    (a, k) switchcase ->
    (a, k) switchcase ->
    (a, leaf) nat ->
    bool ->
    (a, k) switchcase option =
 fun original { data; if_match; next_case } n success ->
  match merge_testcases_aux original data if_match n with
  | Some result -> (
      match next_case with
      | None -> Some result
      | Some b -> merge_testcases result b n true)
  | None -> (
      match (next_case, success) with
      | None, false -> None
      | None, true -> Some original
      | Some b, success -> merge_testcases original b n success)

(*
  When we merge a list of values with a wildcard, some of them may not merge
  successfully. As long as at least one merges, then this function succeeds.
  We filter out any unsuccessful mergers, since their paths are covered by the
  wildcard case.
*)
and merge_testcases_into_wildcard :
    type a k.
    ?init:(a, k) switchcase ->
    (a, k) tree ->
    (a, k) switchcase ->
    (a, leaf) nat ->
    (a, k) switchcase option =
 fun ?init wildcard t n ->
  let init =
    try Some { t with if_match = merge wildcard t.if_match n; next_case = init }
    with MergeFail -> init
  in
  match (t.next_case, init) with
  | None, None -> None
  | None, Some init -> Some (rev_cases init)
  | Some t, init -> merge_testcases_into_wildcard ?init wildcard t n

(*
  When we expand a wildcard into a list of test values, we merge it with each
  child and ignore any errors. Merge failures are replaced by their unmerged
  originals.
*)

and expand_wildcard_into_testcases :
    type a k.
    ?init:(a, k) switchcase ->
    (a, k) switchcase ->
    (a, k) tree ->
    (a, leaf) nat ->
    (a, k) switchcase =
 fun ?init { data; if_match; next_case } wildcard n ->
  let init =
    try { data; if_match = merge if_match wildcard n; next_case = init }
    with MergeFail -> { data; if_match; next_case = init }
  in
  match next_case with
  | None -> rev_cases init
  | Some a -> expand_wildcard_into_testcases ~init a wildcard n

(*
  When we expand a wildcard into a nest, we need to expand all of the wildcard's
  child nodes after the nest's child nodes. This maps through the nest to do
  so.
  We need a second nat value to track "where we are" relative to the beginning
  of the nest. Z (zero) represents the depth where the nest began.
  It doesn't matter if the expansion succeeds or not. If it fails, we just keep
  the original tree.
*)

and expand_wildcard_after_nest :
    type a b ka kb.
    (a, ka) tree ->
    (a, leaf) nat ->
    wildcard:(b, kb) tree ->
    (a, (b, kb) tree) nat ->
    (a, ka) tree =
 fun a na ~wildcard nb ->
  match (a, na, nb) with
  | End a, S n, Z -> ( try End (merge a wildcard n) with MergeFail -> End a)
  | End a, S na, S nb -> End (expand_wildcard_after_nest a na ~wildcard nb)
  | Nest a, na, nb ->
      let child =
        match a.child with
        | IntKeys c ->
            IntKeys (expand_wildcard_after_nest c (S na) ~wildcard (S nb))
        | StringKeys c ->
            StringKeys (expand_wildcard_after_nest c (S na) ~wildcard (S nb))
      in
      Nest { a with child }
  | Construct a, na, nb ->
      Construct
        {
          a with
          nil =
            (match a.nil with
            | None -> None
            | Some c -> Some (expand_wildcard_after_nest c na ~wildcard nb));
          cons =
            (match a.cons with
            | None -> None
            | Some c -> Some (expand_wildcard_after_nest c na ~wildcard nb));
        }
  | Wildcard a, na, nb ->
      Wildcard
        { a with child = expand_wildcard_after_nest a.child na ~wildcard nb }
  | Switch a, na, nb ->
      let wildcard' =
        match a.wildcard with
        | None -> None
        | Some a -> Some (expand_wildcard_after_nest a na ~wildcard nb)
      in
      let rec aux init case =
        let if_match =
          expand_wildcard_after_nest case.if_match na ~wildcard nb
        in
        let init = { case with if_match; next_case = init } in
        match case.next_case with
        | None -> Switch { a with cases = rev_cases init; wildcard = wildcard' }
        | Some a -> aux (Some init) a
      in
      aux None a.cases
  | _ -> .

(*
  When we merge a wildcard after a nest, we can only keep successful mergers.
  For constructs and switch nodes, then at least one path must merge
  succesfully. Unsuccesful paths are filtered out.
*)

and merge_wildcard_after_nest :
    type a b ka kb.
    wildcard:(a, ka) tree ->
    (b, (a, ka) tree) nat ->
    (b, kb) tree ->
    (b, leaf) nat ->
    (b, kb) tree =
 fun ~wildcard na b nb ->
  match (b, na, nb) with
  | End b, Z, S n -> End (merge wildcard b n)
  | End b, S na, S nb -> End (merge_wildcard_after_nest ~wildcard na b nb)
  | Nest b, na, nb ->
      let child =
        match b.child with
        | IntKeys c ->
            IntKeys (merge_wildcard_after_nest ~wildcard (S na) c (S nb))
        | StringKeys c ->
            StringKeys (merge_wildcard_after_nest ~wildcard (S na) c (S nb))
      in
      Nest { b with child }
  | Construct b, na, nb -> (
      let nil =
        match b.nil with
        | None -> Ok None
        | Some c -> (
            try Ok (Some (merge_wildcard_after_nest ~wildcard na c nb))
            with MergeFail -> Error None)
      in
      let cons =
        match b.cons with
        | None -> Ok None
        | Some c -> (
            try Ok (Some (merge_wildcard_after_nest ~wildcard na c nb))
            with MergeFail -> Error None)
      in
      match (nil, cons) with
      | Error _, Error _ | Ok None, Error None | Error None, Ok None ->
          raise MergeFail
      | Ok nil, Ok cons | Ok nil, Error cons | Error nil, Ok cons ->
          Construct { b with nil; cons })
  | Wildcard b, na, nb ->
      Wildcard
        { b with child = merge_wildcard_after_nest ~wildcard na b.child nb }
  | Switch b, na, nb ->
      let wildcard' =
        match b.wildcard with
        | None -> None
        | Some b -> (
            try Some (merge_wildcard_after_nest ~wildcard na b nb)
            with MergeFail -> None)
      in
      let rec aux init case =
        let init =
          try
            let if_match =
              merge_wildcard_after_nest ~wildcard na case.if_match nb
            in
            Some { case with if_match; next_case = init }
          with MergeFail -> init
        in
        match (case.next_case, init) with
        | None, None -> raise MergeFail
        | None, Some init ->
            Switch { b with cases = rev_cases init; wildcard = wildcard' }
        | Some a, init -> aux init a
      in
      aux None b.cases

and merge : type a k. (a, k) tree -> (a, k) tree -> (a, leaf) nat -> (a, k) tree
    =
 fun a b n ->
  match (a, b) with
  | End a, End b -> (
      match n with Z -> raise MergeFail | S n -> End (merge a b n))
  | Wildcard a, Wildcard b ->
      let ids = SetInt.union a.ids b.ids in
      let child = merge a.child b.child n in
      Wildcard { a with ids; child }
  | Wildcard a, Nest b ->
      let wildcard =
        match b.wildcard with None -> a.child | Some b -> merge a.child b n
      in
      let child =
        match b.child with
        | IntKeys c ->
            IntKeys (merge_wildcard_after_nest ~wildcard:a.child Z c (S n))
        | StringKeys c ->
            StringKeys (merge_wildcard_after_nest ~wildcard:a.child Z c (S n))
      in
      let ids = SetInt.union a.ids b.ids in
      Nest { b with ids; child; wildcard = Some wildcard }
  | (Wildcard a as a'), Construct b -> (
      let nil =
        match b.nil with
        | None -> Ok (Some a.child)
        | Some b -> (
            try Ok (Some (merge a.child b n)) with MergeFail -> Error None)
      in
      let cons =
        match b.cons with
        | None -> Ok (Some a')
        | Some b -> (
            try Ok (Some (merge a' b n)) with MergeFail -> Error None)
      in
      let ids = SetInt.union a.ids b.ids in
      match (nil, cons) with
      | Error _, Error _ | Ok None, Error None | Error None, Ok None ->
          raise MergeFail
      | Ok nil, Ok cons | Error nil, Ok cons | Ok nil, Error cons ->
          Construct { b with ids; nil; cons })
  | Wildcard a, Switch b ->
      let cases =
        match merge_testcases_into_wildcard a.child b.cases n with
        | None -> raise MergeFail
        | Some c -> c
      in
      let wildcard =
        match b.wildcard with None -> a.child | Some b -> merge a.child b n
      in
      let ids = SetInt.union a.ids b.ids in
      Switch { b with ids; cases; wildcard = Some wildcard }
  | Nest a, Nest b ->
      let wildcard =
        match (a.wildcard, b.wildcard) with
        | None, None -> None
        | Some x, None | None, Some x -> Some x
        | Some a, Some b -> Some (merge a b n)
      in
      let child =
        match (a.child, b.child) with
        | IntKeys a, IntKeys b -> (
            let child = merge a b (S n) in
            match wildcard with
            | None -> IntKeys child
            | Some wildcard ->
                IntKeys (merge_wildcard_after_nest ~wildcard Z child (S n)))
        | StringKeys a, StringKeys b -> (
            let child = merge a b (S n) in
            match wildcard with
            | None -> StringKeys child
            | Some wildcard ->
                StringKeys (merge_wildcard_after_nest ~wildcard Z child (S n)))
        | _ -> assert false
      in
      let ids = SetInt.union a.ids b.ids in
      Nest { a with ids; child; wildcard }
  | Nest a, Wildcard b ->
      let child =
        match a.child with
        | StringKeys child ->
            StringKeys
              (expand_wildcard_after_nest child (S n) ~wildcard:b.child Z)
        | IntKeys child ->
            IntKeys (expand_wildcard_after_nest child (S n) ~wildcard:b.child Z)
      in
      let wildcard =
        match a.wildcard with None -> b.child | Some a -> merge a b.child n
      in
      let ids = SetInt.union a.ids b.ids in
      Nest { a with ids; child; wildcard = Some wildcard }
  | Construct a, (Wildcard b as b') -> (
      let nil =
        match a.nil with
        | None -> Ok b.child
        | Some a -> ( try Ok (merge a b.child n) with MergeFail -> Error a)
      in
      let cons =
        match a.cons with
        | None -> Ok b'
        | Some a -> ( try Ok (merge a b' n) with MergeFail -> Error a)
      in
      let ids = SetInt.union a.ids b.ids in
      match (nil, cons) with
      | Error _, Error _ -> raise MergeFail
      | Ok nil, Ok cons | Ok nil, Error cons | Error nil, Ok cons ->
          Construct { a with ids; nil = Some nil; cons = Some cons })
  | Construct a, Construct b ->
      let nil =
        match (a.nil, b.nil) with
        | None, None -> None
        | Some x, None | None, Some x -> Some x
        | Some a, Some b -> Some (merge a b n)
      in
      let cons =
        match (a.cons, b.cons) with
        | None, None -> None
        | Some x, None | None, Some x -> Some x
        | Some a, Some b -> Some (merge a b n)
      in
      let ids = SetInt.union a.ids b.ids in
      Construct { a with ids; nil; cons }
  | Switch a, Wildcard b ->
      let wildcard =
        match a.wildcard with None -> b.child | Some a -> merge a b.child n
      in
      let cases = expand_wildcard_into_testcases a.cases b.child n in
      let ids = SetInt.union a.ids b.ids in
      Switch { a with ids; cases; wildcard = Some wildcard }
  | Switch a, Switch b ->
      let wildcard =
        match (a.wildcard, b.wildcard) with
        | None, None -> None
        | Some x, None | None, Some x -> Some x
        | Some a, Some b -> Some (merge a b n)
      in
      let bcases =
        match a.wildcard with
        | Some wildcard -> (
            match merge_testcases_into_wildcard wildcard b.cases n with
            | Some cases -> cases
            | None -> raise MergeFail)
        | None -> b.cases
      in
      let cases =
        match merge_testcases a.cases bcases n false with
        | None -> raise MergeFail
        | Some cases -> cases
      in
      let cases =
        match b.wildcard with
        | None -> cases
        | Some wildcard -> expand_wildcard_into_testcases cases wildcard n
      in
      let ids = SetInt.union a.ids b.ids in
      Switch { a with ids; cases; wildcard }
  | Switch _, (Nest _ | Construct _)
  | Construct _, (Switch _ | Nest _)
  | Nest _, (Switch _ | Construct _)
  | (Switch _ | Nest _ | Construct _ | Wildcard _), End _
  | End _, (Switch _ | Nest _ | Construct _ | Wildcard _) ->
      assert false

(*
  To turn a typed pattern into a tree (albiet a single-branch of tree), we use
  CPS as an easy way deal with the nested data type.
*)

type bindings = { next_id : unit -> int; names : int MapString.t }
type ('a, 'k) cont = bindings -> ('a, 'k) tree

let of_const key data if_match enum =
  Switch
    {
      key;
      ids = SetInt.empty;
      cases = { data; if_match; next_case = None };
      extra =
        (match enum with
        | Some { Ty.Variant.row = `Closed; _ } -> Extra_enum_closed
        | Some { row = `Open; _ } | None -> Extra_none);
      wildcard = None;
    }

let rec of_tpat :
          'a 'k. key:'k -> bindings -> ('a, 'k) cont -> TPat.t -> ('a, 'k) tree
    =
 fun ~key b k -> function
  | TPat.TAny -> Wildcard { ids = SetInt.empty; key; child = k b }
  | TVar x | TOptionalVar x ->
      let id = b.next_id () in
      let b = { b with names = MapString.add x id b.names } in
      Wildcard { ids = SetInt.singleton id; key; child = k b }
  | TConstruct (kind, Some cons) ->
      let child = of_tpat ~key b k cons in
      Construct
        { key; ids = SetInt.empty; extra = kind; nil = None; cons = Some child }
  | TConstruct (kind, None) ->
      Construct
        { key; ids = SetInt.empty; extra = kind; nil = Some (k b); cons = None }
  | TConst (data, enum) -> of_const key data (k b) enum
  | TTuple l ->
      let child = IntKeys (of_list ~key:0 b (fun b -> End (k b)) l) in
      Nest { key; ids = SetInt.empty; child; wildcard = None; extra = Tuple }
  | TRecord (tag, m, tys) ->
      (* We need to expand the map to include all of its type's keys. *)
      let l =
        MapString.merge
          (fun _k _ty p ->
            match p with None -> Some TPat.TAny | Some p -> Some p)
          !tys m
        |> MapString.to_seq
      in
      let child = of_keyvalues b (fun b -> End (k b)) l in
      let child, extra =
        match tag with
        | Some (key, data, union) ->
            (of_const key data child (Some union), Union union.extra)
        | None -> (child, Record)
      in
      Nest
        {
          key;
          ids = SetInt.empty;
          child = StringKeys child;
          wildcard = None;
          extra;
        }
  | TDict (m, kys) ->
      (* We need to expand the map to include all of its type's keys. *)
      let l =
        !kys |> SetString.to_seq
        |> Seq.map (fun key -> (key, TPat.TAny))
        |> MapString.of_seq
        |> MapString.merge
             (fun _ p kpat ->
               match (p, kpat) with
               | Some p, _ | None, Some p -> Some p
               | None, None -> None)
             m
        |> MapString.to_seq
      in
      let child = of_keyvalues b (fun b -> End (k b)) l in
      Nest
        {
          key;
          ids = SetInt.empty;
          child = StringKeys child;
          wildcard = None;
          extra = Dict;
        }

and of_list :
      'a. key:int -> bindings -> ('a, int) cont -> TPat.t list -> ('a, int) tree
    =
 fun ~key b k -> function
  | [] -> k b
  | p :: l -> of_tpat ~key b (fun b -> of_list ~key:(succ key) b k l) p

and of_keyvalues :
    bindings ->
    ('a, string) cont ->
    (string * TPat.t) Seq.t ->
    ('a, string) tree =
 fun b k s ->
  match s () with
  | Nil -> k b
  | Cons ((key, v), s) -> of_tpat ~key b (fun b -> of_keyvalues b k s) v

let of_nonempty ~exit next_id Nonempty.(hd :: tl) =
  let k { names; _ } = End { names; exit } in
  let k b = of_list ~key:1 b k tl in
  of_tpat ~key:0 { next_id; names = MapString.empty } k hd

let rec make_case ~exit next_id tree = function
  | [] -> tree
  | ps :: l -> (
      let b = of_nonempty ~exit next_id ps in
      match merge tree b Z with
      | tree -> make_case ~exit next_id tree l
      | exception MergeFail -> failwith "unused case")

let make Nonempty.(Typechecker.{ pats; nodes } :: tl_cases) =
  let exitq = Queue.create () in
  (* IDs must be unique across all branches of the tree. *)
  let next_id = ref 0 in
  let next_id () =
    let id = !next_id in
    incr next_id;
    id
  in
  let hd_tree =
    let (hd_pats :: tl_pats) = pats in
    Queue.add nodes exitq;
    let exit = Queue.length exitq - 1 in
    let hd_tree = of_nonempty ~exit next_id hd_pats in
    make_case ~exit next_id hd_tree tl_pats
  in
  let rec aux tree = function
    | [] -> { tree; exits = exitq |> Queue.to_seq |> Array.of_seq }
    | Typechecker.{ pats = hd_pats :: tl_pats; nodes } :: l -> (
        Queue.add nodes exitq;
        let exit = Queue.length exitq - 1 in
        let hd_tree = of_nonempty ~exit next_id hd_pats in
        match merge tree hd_tree Z with
        | tree ->
            let tree = make_case ~exit next_id tree tl_pats in
            aux tree l
        | exception MergeFail -> failwith "unused case")
  in
  aux hd_tree tl_cases

module ParMatch = struct
  type flag = Partial | Exhaustive
  type 'a t = { flag : flag; pats : (string * TPat.t) list; next : 'a }

  let values l = List.map (fun (_, v) -> v) l
  let to_keyvalues l = l |> List.to_seq |> MapString.of_seq
  let pp_sep ppf () = Format.fprintf ppf ",@ "
  let pp_pats ppf l = Format.pp_print_list ~pp_sep TPat.pp ppf l
  let key_str = Fun.id
  let key_int _ = ""

  let exhaustive key { pats; flag; next } =
    let pat = match key with "" -> TPat.TAny | k -> TVar k in
    { flag; next; pats = (key, pat) :: pats }

  let rec check : 'a 'k. ('k -> string) -> ('a, 'k) tree -> 'a t =
   fun kf -> function
    | End next -> { next; flag = Exhaustive; pats = [] }
    | Wildcard { key; child; _ } -> exhaustive (kf key) (check kf child)
    | Nest { key; child; wildcard; extra; _ } -> (
        (* Either the child _or_ the wildcard can be exhaustive.
           A nest filled with exhaustive patterns e.g. tuple (_, _, _) can lead
           to an exhaustive path even if it's paired with a wildcard _ that
           doesn't. (In which case, the wildcard is redundant and will never be
           used.) *)
        let result =
          match child with
          | IntKeys child -> check key_int child
          | StringKeys child -> check key_str child
        in
        match result with
        | { flag = Exhaustive; next; _ } -> (
            match extra with
            | Tuple | Record | Union _ -> exhaustive (kf key) (check kf next)
            | Dict -> (
                match wildcard with
                | Some wildcard -> exhaustive (kf key) (check kf wildcard)
                | None ->
                    let { pats; next; _ } = check kf next in
                    { next; flag = Partial; pats = (kf key, TAny) :: pats }))
        | { flag = Partial; pats; next } -> (
            match wildcard with
            | Some wildcard -> exhaustive (kf key) (check kf wildcard)
            | None ->
                let nest =
                  match extra with
                  | Tuple -> TPat.TTuple (values pats)
                  | Record ->
                      TRecord (None, to_keyvalues pats, ref MapString.empty)
                  | Union extra -> (
                      match pats with
                      | (k, TConst (v, _)) :: pats ->
                          TRecord
                            ( Some
                                ( k,
                                  v,
                                  {
                                    cases = String MapString.empty;
                                    row = `Closed;
                                    extra;
                                  } ),
                              to_keyvalues pats,
                              ref MapString.empty )
                          (* If the child (tag) was not a constant, then it was
                             a TAny and we report the entire union pattern as a
                             TAny. *)
                      | _ -> TAny)
                  | Dict -> TDict (to_keyvalues pats, ref SetString.empty)
                in
                let { pats; next; _ } = check kf next in
                { next; flag = Partial; pats = (kf key, nest) :: pats }))
    | Construct { key; extra; nil; cons; _ } -> (
        match (nil, cons) with
        | None, Some cons ->
            let { pats; next; _ } = check kf cons in
            {
              flag = Partial;
              pats =
                (match pats with
                | _ :: pats -> (kf key, TConstruct (extra, None)) :: pats
                | _ -> assert false);
              next;
            }
        | Some nil, None ->
            let { pats; next; _ } = check kf nil in
            {
              flag = Partial;
              pats = (kf key, TConstruct (extra, Some TAny)) :: pats;
              next;
            }
        | Some nil, Some cons -> (
            match check kf nil with
            | { flag = Exhaustive; _ } -> (
                match check kf cons with
                | { flag = Exhaustive; _ } as x -> x
                | { flag = Partial; pats; next } ->
                    let pats =
                      match pats with
                      | (key, cons) :: pats ->
                          (key, TPat.TConstruct (extra, Some cons)) :: pats
                      | _ -> assert false
                    in
                    { pats; next; flag = Partial })
            | { flag = Partial; pats; next } ->
                {
                  flag = Partial;
                  pats = (kf key, TPat.TConstruct (extra, None)) :: pats;
                  next;
                })
        | None, None -> assert false)
    | Switch { key; cases; wildcard; extra; _ } -> (
        match wildcard with
        | Some wildcard -> exhaustive (kf key) (check kf wildcard)
        | None -> (
            match extra with
            | Extra_none ->
                let { pats; next; _ } = check kf cases.if_match in
                { flag = Partial; pats = (kf key, TAny) :: pats; next }
            | Extra_enum_closed ->
                let rec aux { data; if_match; next_case } =
                  match check kf if_match with
                  | { flag = Partial; pats; next } ->
                      {
                        flag = Partial;
                        pats = (kf key, TConst (data, None)) :: pats;
                        next;
                      }
                  | { flag = Exhaustive; pats; next } -> (
                      match next_case with
                      | None ->
                          {
                            next;
                            flag = Exhaustive;
                            pats = (kf key, TAny) :: pats;
                          }
                      | Some case -> aux case)
                in
                aux cases))
end

let partial_match_check tree =
  match ParMatch.check ParMatch.key_int tree with
  | { flag = Exhaustive; _ } -> ()
  | { flag = Partial; pats; _ } ->
      Format.printf "%a" ParMatch.pp_pats (ParMatch.values pats);
      failwith "partial match"
