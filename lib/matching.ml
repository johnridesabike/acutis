(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module T = Typechecker
module Ty = Typescheme
module Const = Data.Const

type ('leaf, 'key) tree =
  | Switch of {
      key : 'key;
      ids : Set.Int.t;
      cases : ('leaf, 'key) switchcase;
      wildcard : ('leaf, 'key) tree option;
      debug_row : Ty.row;
    }
  | Nest of {
      key : 'key;
      ids : Set.Int.t;
      child : ('leaf, 'key) nest;
      wildcard : ('leaf, 'key) tree option;
    }
  | Construct of {
      key : 'key;
      ids : Set.Int.t;
      nil : ('leaf, 'key) tree option;
      cons : ('leaf, 'key) tree option;
    }
  | Wildcard of { key : 'key; ids : Set.Int.t; child : ('leaf, 'key) tree }
  | Optional of {
      child : ('leaf, 'key) tree option;
      next : ('leaf, 'key) tree option;
    }
  | End of 'leaf

and ('leaf, 'key) nest =
  | Int_keys of (('leaf, 'key) tree, int) tree
  | String_keys of (('leaf, 'key) tree, string) tree

and ('leaf, 'key) switchcase = {
  data : Const.t;
  if_match : ('leaf, 'key) tree;
  next : ('leaf, 'key) switchcase option;
}

module Exit = struct
  type key = int

  let equal_key = Int.equal

  type 'a t = 'a array

  let get = Array.get
  let map = Array.map
  let unsafe_key i = i
  let key_to_int i = i
  let pp_key = Format.pp_print_int
  let to_seqi = Array.to_seqi
end

type leaf = { names : int Map.String.t; exit : Exit.key }
type 'a t = { tree : (leaf, int) tree; exits : 'a Exit.t }

let rec equal_tree :
          'leaf 'key.
          ('leaf -> 'leaf -> bool) ->
          ('key -> 'key -> bool) ->
          ('leaf, 'key) tree ->
          ('leaf, 'key) tree ->
          bool =
 fun equal_leaf equal_key a b ->
  match (a, b) with
  | Switch a, Switch { key; ids; cases; wildcard; debug_row } ->
      equal_key a.key key && Set.Int.equal a.ids ids
      && equal_switchcase equal_leaf equal_key a.cases cases
      && Option.equal (equal_tree equal_leaf equal_key) a.wildcard wildcard
      && Ty.equal_row a.debug_row debug_row
  | Nest a, Nest { key; ids; child; wildcard } ->
      equal_key a.key key && Set.Int.equal a.ids ids
      && equal_nest equal_leaf equal_key a.child child
      && Option.equal (equal_tree equal_leaf equal_key) a.wildcard wildcard
  | Construct a, Construct { key; ids; nil; cons } ->
      equal_key a.key key && Set.Int.equal a.ids ids
      && Option.equal (equal_tree equal_leaf equal_key) a.nil nil
      && Option.equal (equal_tree equal_leaf equal_key) a.cons cons
  | Wildcard a, Wildcard { key; ids; child } ->
      equal_key a.key key && Set.Int.equal a.ids ids
      && equal_tree equal_leaf equal_key a.child child
  | End a, End b -> equal_leaf a b
  | _ -> false

and equal_nest :
      'leaf 'key.
      ('leaf -> 'leaf -> bool) ->
      ('key -> 'key -> bool) ->
      ('leaf, 'key) nest ->
      ('leaf, 'key) nest ->
      bool =
 fun equal_leaf equal_key a b ->
  match (a, b) with
  | Int_keys a, Int_keys b ->
      equal_tree (equal_tree equal_leaf equal_key) Int.equal a b
  | String_keys a, String_keys b ->
      equal_tree (equal_tree equal_leaf equal_key) String.equal a b
  | _ -> false

and equal_switchcase :
      'leaf 'key.
      ('leaf -> 'leaf -> bool) ->
      ('key -> 'key -> bool) ->
      ('leaf, 'key) switchcase ->
      ('leaf, 'key) switchcase ->
      bool =
 fun equal_leaf equal_key a { data; if_match; next } ->
  Const.equal a.data data
  && equal_tree equal_leaf equal_key a.if_match if_match
  && Option.equal (equal_switchcase equal_leaf equal_key) a.next next

let equal_leaf a { names; exit } =
  Map.String.equal Int.equal a.names names && Exit.equal_key a.exit exit

(** One challenge when merging the trees is that we need to keep track of where
    we are in the tree. This natural-number GADT tracks that for us on the type
    level. *)
type (_, _) depth =
  | Z : ('z, 'z) depth
  | S : ('a, 'z) depth -> (('a, 'key) tree, 'z) depth

(** Equality for n-depth nested trees. Keys are ignored. *)
let equal_tree_nested =
  let equal_noop _ _ = true in
  let rec aux : type a. (a, leaf) depth -> a -> a -> bool = function
    | Z -> equal_leaf
    | S n -> equal_tree (aux n) equal_noop
  in
  fun n -> equal_tree (aux n) equal_noop

(** The merge algorithm must do some extra work to detect when merges fail.
    A failure is defined by when a merger returns a tree that is identical to
    its input tree. This is important because it enables us to detect unused
    patterns.

    In some cases, it's possible to produce structurally different trees even
    though a merge really failed. For example, a construct failing to merge into
    a wildcard can be represented by a new construct that only contains the
    wildcard's contents. We need to manually check these cases and return the
    original structure if there's a failure. *)

(** Sort the data for easier analysis. If the new value is lower than the
    original, then append it to the beginning. If the new value equals the
    original, then merge them. If the new value is greater than the original,
    then check if we're at the end of the list. If we are, then add this new
    case. If not, then keep searching. *)
let[@tail_mod_cons] rec merge_testcases_aux :
    type a k.
    (a, leaf) depth ->
    (a, k) switchcase ->
    Const.t ->
    (a, k) tree ->
    (a, k) switchcase =
 fun n original data if_match ->
  let cmp = Const.compare data original.data in
  if cmp < 0 then { data; if_match; next = Some original }
  else if cmp = 0 then
    { original with if_match = merge n original.if_match if_match }
  else
    {
      original with
      next =
        (match original.next with
        | None -> Some { data; if_match; next = None }
        | Some next -> Some (merge_testcases_aux n next data if_match));
    }

(** When merging [a] and [b], we take each value from [b] and try to merge
    them with the items in [a]. *)
and merge_testcases :
    type a k.
    (a, leaf) depth ->
    (a, k) switchcase ->
    (a, k) switchcase ->
    (a, k) switchcase =
 fun n original { data; if_match; next } ->
  let result = merge_testcases_aux n original data if_match in
  match next with None -> result | Some b -> merge_testcases n result b

(** When we merge a list of values with a wildcard, some of them may not merge
    successfully. We filter out any unsuccessful mergers, since their paths are
    covered by the wildcard case. *)
and[@tail_mod_cons] merge_testcases_into_wildcard :
    type a k.
    (a, leaf) depth ->
    (a, k) tree ->
    (a, k) switchcase ->
    (a, k) switchcase option =
 fun n wildcard t ->
  let if_match = merge n wildcard t.if_match in
  match (equal_tree_nested n wildcard if_match, t.next) with
  | true, None -> None
  | true, Some next -> merge_testcases_into_wildcard n wildcard next
  | false, None -> Some { t with if_match }
  | false, Some next ->
      Some
        {
          t with
          if_match;
          next = merge_testcases_into_wildcard n wildcard next;
        }

(** When we expand a wildcard into a list of test values, we merge it with each
    child. *)
and[@tail_mod_cons] expand_wildcard_into_testcases :
    type a k.
    (a, leaf) depth -> (a, k) switchcase -> (a, k) tree -> (a, k) switchcase =
 fun n { data; if_match; next } wildcard ->
  let if_match = merge n if_match wildcard in
  match next with
  | None -> { data; if_match; next }
  | Some next ->
      {
        data;
        if_match;
        next = Some (expand_wildcard_into_testcases n next wildcard);
      }

(** When we expand a wildcard into a nest, we need to expand all of the
    wildcard's child nodes after the nest's child nodes. We use a second [depth]
    value to track where we are relative to the beginning of the nest. [Z]
    represents the depth where the nest began. *)
and expand_wildcard_after_nest :
    type a b ka kb.
    (a, leaf) depth ->
    (a, (b, kb) tree) depth ->
    (a, ka) tree ->
    wildcard:(b, kb) tree ->
    (a, ka) tree =
 fun na nb a ~wildcard ->
  match a with
  | End a -> (
      match (na, nb) with
      | S n, Z -> End (merge n a wildcard)
      | S na, S nb -> End (expand_wildcard_after_nest na nb a ~wildcard)
      | _ -> .)
  | Nest a ->
      Nest
        {
          a with
          child =
            (match a.child with
            | Int_keys c ->
                Int_keys (expand_wildcard_after_nest (S na) (S nb) c ~wildcard)
            | String_keys c ->
                String_keys
                  (expand_wildcard_after_nest (S na) (S nb) c ~wildcard));
          wildcard =
            (match a.wildcard with
            | None -> None
            | Some c -> Some (expand_wildcard_after_nest na nb c ~wildcard));
        }
  | Construct a ->
      Construct
        {
          a with
          nil =
            (match a.nil with
            | None -> None
            | Some c -> Some (expand_wildcard_after_nest na nb c ~wildcard));
          cons =
            (match a.cons with
            | None -> None
            | Some c -> Some (expand_wildcard_after_nest na nb c ~wildcard));
        }
  | Wildcard a ->
      Wildcard
        { a with child = expand_wildcard_after_nest na nb a.child ~wildcard }
  | Switch a ->
      let[@tail_mod_cons] rec aux case =
        let if_match =
          expand_wildcard_after_nest na nb case.if_match ~wildcard
        in
        match case.next with
        | None -> { case with if_match }
        | Some next -> { case with if_match; next = Some (aux next) }
      in
      Switch
        {
          a with
          cases = aux a.cases;
          wildcard =
            (match a.wildcard with
            | None -> None
            | Some a -> Some (expand_wildcard_after_nest na nb a ~wildcard));
        }
  | Optional a ->
      Optional
        {
          child =
            (match a.child with
            | None -> None
            | Some a -> Some (expand_wildcard_after_nest na nb a ~wildcard));
          next =
            (match a.next with
            | None -> None
            | Some a -> Some (expand_wildcard_after_nest na nb a ~wildcard));
        }

(** When we merge the contents after a nest into a wildcard, we can only keep
    successful mergers. For constructs and switch nodes, then at least one path
    must merge successfully. Unsuccessful paths are filtered out, since their
    paths are covered by the wildcard case. *)
and merge_wildcard_after_nest :
    type a b ka kb.
    equal:((a, ka) tree -> (a, ka) tree -> bool) ->
    (b, (a, ka) tree) depth ->
    (b, leaf) depth ->
    wildcard:(a, ka) tree ->
    (b, kb) tree ->
    (b, kb) tree option =
 fun ~equal na nb ~wildcard b ->
  match b with
  | End b -> (
      match (na, nb) with
      | Z, S n ->
          let x = merge n wildcard b in
          if equal x wildcard then None else Some (End x)
      | S na, S nb -> (
          match merge_wildcard_after_nest ~equal na nb ~wildcard b with
          | None -> None
          | Some x -> Some (End x)))
  | Nest b -> (
      let w =
        match b.wildcard with
        | None -> None
        | Some b -> merge_wildcard_after_nest ~equal na nb ~wildcard b
      in
      match b.child with
      | Int_keys c -> (
          match merge_wildcard_after_nest ~equal (S na) (S nb) ~wildcard c with
          | None -> None
          | Some c -> Some (Nest { b with child = Int_keys c; wildcard = w }))
      | String_keys c -> (
          match merge_wildcard_after_nest ~equal (S na) (S nb) ~wildcard c with
          | None -> None
          | Some c -> Some (Nest { b with child = String_keys c; wildcard = w })
          ))
  | Construct b -> (
      let nil =
        match b.nil with
        | None -> None
        | Some c -> merge_wildcard_after_nest ~equal na nb ~wildcard c
      in
      let cons =
        match b.cons with
        | None -> None
        | Some c -> merge_wildcard_after_nest ~equal na nb ~wildcard c
      in
      match (nil, cons) with
      | None, None -> None
      | nil, cons -> Some (Construct { b with nil; cons }))
  | Wildcard b -> (
      match merge_wildcard_after_nest ~equal na nb ~wildcard b.child with
      | None -> None
      | Some child -> Some (Wildcard { b with child }))
  | Switch b -> (
      let w =
        match b.wildcard with
        | None -> None
        | Some b -> merge_wildcard_after_nest ~equal na nb ~wildcard b
      in
      let[@tail_mod_cons] rec aux case =
        let if_match =
          merge_wildcard_after_nest ~equal na nb ~wildcard case.if_match
        in
        match (if_match, case.next) with
        | None, None -> None
        | Some if_match, None -> Some { case with if_match }
        | None, Some next -> aux next
        | Some if_match, Some next ->
            Some { case with if_match; next = aux next }
      in
      match aux b.cases with
      | None -> None
      | Some cases -> Some (Switch { b with cases; wildcard = w }))
  | Optional b -> (
      let child =
        match b.child with
        | None -> None
        | Some b -> merge_wildcard_after_nest ~equal na nb ~wildcard b
      in
      let next =
        match b.next with
        | None -> None
        | Some b -> merge_wildcard_after_nest ~equal na nb ~wildcard b
      in
      match (child, next) with
      | None, None -> None
      | child, next -> Some (Optional { child; next }))

(** Merge two trees. If the merge is unsuccessful, then the result will be
    structurally equal to the first tree. *)
and merge :
    type a k. (a, leaf) depth -> (a, k) tree -> (a, k) tree -> (a, k) tree =
 fun n a b ->
  match (a, b) with
  | End a, End b -> ( match n with Z -> End a | S n -> End (merge n a b))
  | Wildcard a, Wildcard b ->
      Wildcard
        {
          a with
          ids = Set.Int.union a.ids b.ids;
          child = merge n a.child b.child;
        }
  | Wildcard a, Nest b -> (
      let equal = equal_tree_nested n in
      let ids = Set.Int.union a.ids b.ids in
      let wildcard =
        match b.wildcard with
        | None -> Some a.child
        | Some b -> Some (merge n a.child b)
      in
      match b.child with
      | Int_keys c -> (
          match
            merge_wildcard_after_nest ~equal Z (S n) ~wildcard:a.child c
          with
          | None -> Wildcard a
          | Some c -> Nest { b with ids; child = Int_keys c; wildcard })
      | String_keys c -> (
          match
            merge_wildcard_after_nest ~equal Z (S n) ~wildcard:a.child c
          with
          | None -> Wildcard a
          | Some c -> Nest { b with ids; child = String_keys c; wildcard }))
  | (Wildcard { ids; child; _ } as a), Construct b ->
      (* At least one merger must succeed. *)
      let nil =
        match b.nil with None -> child | Some nil -> merge n child nil
      in
      let cons = match b.cons with None -> a | Some cons -> merge n a cons in
      let ids = Set.Int.union ids b.ids in
      let equal = equal_tree_nested n in
      if equal child nil && equal a cons then a
      else Construct { b with ids; nil = Some nil; cons = Some cons }
  | Wildcard a, Switch b -> (
      match merge_testcases_into_wildcard n a.child b.cases with
      | None -> Wildcard a
      | Some cases ->
          let wildcard =
            match b.wildcard with
            | None -> a.child
            | Some b -> merge n a.child b
          in
          let ids = Set.Int.union a.ids b.ids in
          Switch { b with ids; cases; wildcard = Some wildcard })
  | Nest a, Nest b ->
      let equal = equal_tree_nested n in
      let ids = Set.Int.union a.ids b.ids in
      let wildcard =
        match (a.wildcard, b.wildcard) with
        | None, None -> None
        | Some x, None | None, Some x -> Some x
        | Some a, Some b -> Some (merge n a b)
      in
      let merge_child a b =
        let child = merge (S n) a b in
        match wildcard with
        | None -> child
        | Some wildcard -> (
            match merge_wildcard_after_nest ~equal Z (S n) ~wildcard child with
            | None -> child
            | Some child -> child)
      in
      let child =
        match (a.child, b.child) with
        | Int_keys a, Int_keys b -> Int_keys (merge_child a b)
        | String_keys a, String_keys b -> String_keys (merge_child a b)
        | _ ->
            Error.internal __POS__
              "Type error between int and string keys. This means the \
               typechecker failed."
      in
      Nest { a with ids; child; wildcard }
  | Nest a, Wildcard b ->
      let child =
        match a.child with
        | String_keys a ->
            String_keys (expand_wildcard_after_nest (S n) Z a ~wildcard:b.child)
        | Int_keys a ->
            Int_keys (expand_wildcard_after_nest (S n) Z a ~wildcard:b.child)
      in
      let wildcard =
        match a.wildcard with None -> b.child | Some a -> merge n a b.child
      in
      let ids = Set.Int.union a.ids b.ids in
      Nest { a with ids; child; wildcard = Some wildcard }
  | Construct a, (Wildcard { ids; child; _ } as b) ->
      Construct
        {
          a with
          ids = Set.Int.union a.ids ids;
          nil =
            (match a.nil with
            | None -> Some child
            | Some nil -> Some (merge n nil child));
          cons =
            (match a.cons with
            | None -> Some b
            | Some cons -> Some (merge n cons b));
        }
  | Construct a, Construct b ->
      Construct
        {
          a with
          ids = Set.Int.union a.ids b.ids;
          nil =
            (match (a.nil, b.nil) with
            | None, None -> None
            | Some x, None | None, Some x -> Some x
            | Some a, Some b -> Some (merge n a b));
          cons =
            (match (a.cons, b.cons) with
            | None, None -> None
            | Some x, None | None, Some x -> Some x
            | Some a, Some b -> Some (merge n a b));
        }
  | Switch a, Wildcard b ->
      Switch
        {
          a with
          ids = Set.Int.union a.ids b.ids;
          cases = expand_wildcard_into_testcases n a.cases b.child;
          wildcard =
            (match a.wildcard with
            | None -> Some b.child
            | Some a -> Some (merge n a b.child));
        }
  | Switch a, Switch b ->
      let wildcard =
        match (a.wildcard, b.wildcard) with
        | None, None -> None
        | Some x, None | None, Some x -> Some x
        | Some a, Some b -> Some (merge n a b)
      in
      let bcases =
        match a.wildcard with
        | Some wildcard -> (
            match merge_testcases_into_wildcard n wildcard b.cases with
            | Some cases -> cases
            | None -> b.cases)
        | None -> b.cases
      in
      let cases = merge_testcases n a.cases bcases in
      let cases =
        match b.wildcard with
        | None -> cases
        | Some wildcard -> expand_wildcard_into_testcases n cases wildcard
      in
      let ids = Set.Int.union a.ids b.ids in
      Switch { a with ids; cases; wildcard }
  | Optional { child; next }, Optional b ->
      let child =
        match (child, b.child) with
        | None, None -> None
        | Some a, None | None, Some a -> Some a
        | Some a, Some b -> Some (merge n a b)
      in
      let next =
        match (next, b.next) with
        | None, None -> None
        | Some a, None | None, Some a -> Some a
        | Some a, Some b -> Some (merge n a b)
      in
      Optional { child; next }
  | Wildcard _, Optional _
  | Optional _, (Switch _ | Nest _ | Construct _ | Wildcard _)
  | Switch _, (Nest _ | Construct _ | Optional _)
  | Construct _, (Switch _ | Nest _ | Optional _)
  | Nest _, (Switch _ | Construct _ | Optional _)
  | (Switch _ | Nest _ | Construct _ | Wildcard _ | Optional _), End _
  | End _, (Switch _ | Nest _ | Construct _ | Wildcard _ | Optional _) ->
      Error.internal __POS__
        "Tried to merge incompatible trees. Either the typechecker failed or \
         the function that constructs trees failed."

let merge = merge Z

(** To turn a typed pattern into a tree (albeit a single-branch of tree), we use
    CPS as an easy way deal with the nested tree type. *)

type bindings = { next_id : unit -> int; names : int Map.String.t }
type ('a, 'k) cont = bindings -> ('a, 'k) tree

let of_const key data if_match enum =
  Switch
    {
      key;
      ids = Set.Int.empty;
      cases = { data; if_match; next = None };
      debug_row = (match enum with Some { Ty.row; _ } -> row | None -> `Open);
      wildcard = None;
    }

let rec of_tpat :
          'a 'k. key:'k -> bindings -> ('a, 'k) cont -> T.pat -> ('a, 'k) tree =
 fun ~key b k -> function
  | TAny -> Wildcard { ids = Set.Int.empty; key; child = k b }
  | TVar x ->
      let id = b.next_id () in
      let b = { b with names = Map.String.add x id b.names } in
      Wildcard { ids = Set.Int.singleton id; key; child = k b }
  | TConstruct (_, Some cons) ->
      let child = of_tpat ~key b k cons in
      Construct { key; ids = Set.Int.empty; nil = None; cons = Some child }
  | TConstruct (_, None) ->
      Construct { key; ids = Set.Int.empty; nil = Some (k b); cons = None }
  | TConst (data, enum) -> of_const key data (k b) enum
  | TTuple l ->
      let child = Int_keys (of_list ~key:0 b (fun b -> End (k b)) l) in
      Nest { key; ids = Set.Int.empty; child; wildcard = None }
  | TRecord (tag, m, tys) ->
      (* We need to expand the map to include all of its type's keys. *)
      let child =
        Map.String.merge
          (fun _k _ty p ->
            match p with None -> Some T.TAny | Some _ as p -> p)
          !tys m
        |> Map.String.to_seq
        |> of_keyvalues b (fun b -> End (k b))
      in
      let child =
        match tag with
        | Some (key, data, union) -> of_const key data child (Some union)
        | None -> child
      in
      Nest
        { key; ids = Set.Int.empty; child = String_keys child; wildcard = None }
  | TDict (m, kys) ->
      (* We need to expand the map to include all of its type's keys. *)
      let s =
        Map.String.map Option.some m
        |> Set.String.fold
             (fun key map ->
               Map.String.update key
                 (function None -> Some None | Some p -> Some p)
                 map)
             !kys
        |> Map.String.to_seq
      in
      Nest
        {
          key;
          ids = Set.Int.empty;
          child = String_keys (of_keyvalues_dict b (fun b -> End (k b)) s);
          wildcard = None;
        }
  | TBlock _ | TField _ ->
      Error.internal __POS__
        "This is not allowed in a destructure pattern. The type checker failed \
         to catch this error."

and of_list :
      'a. key:int -> bindings -> ('a, int) cont -> T.pat list -> ('a, int) tree
    =
 fun ~key b k -> function
  | [] -> k b
  | p :: l -> of_tpat ~key b (fun b -> of_list ~key:(succ key) b k l) p

and of_keyvalues b k s =
  match s () with
  | Nil -> k b
  | Cons ((key, v), l) -> of_tpat ~key b (fun b -> of_keyvalues b k l) v

and of_keyvalues_dict b k s =
  match s () with
  | Nil -> k b
  | Cons ((_, None), s) ->
      Optional { child = None; next = Some (of_keyvalues_dict b k s) }
  | Cons ((key, Some v), s) ->
      Optional
        {
          child = Some (of_tpat ~key b (fun b -> of_keyvalues_dict b k s) v);
          next = None;
        }

let of_nonempty ~exit next_id Nonempty.(hd :: tl) =
  let k { names; _ } = End { names; exit } in
  let k b = of_list ~key:1 b k tl in
  of_tpat ~key:0 { next_id; names = Map.String.empty } k hd

let rec make_case ~exit next_id tree = function
  | [] -> tree
  | (loc, ps) :: l ->
      let b = of_nonempty ~exit next_id ps in
      let tree' = merge tree b in
      if equal_tree equal_leaf Int.equal tree tree' then Error.unused_case loc
      else make_case ~exit next_id tree' l

let make Nonempty.(Typechecker.{ pats; nodes } :: tl_cases) =
  let exits = Array.make (1 + List.length tl_cases) [] in
  (* IDs must be unique across all branches of the tree. *)
  let next_id = ref 0 in
  let next_id () =
    let id = !next_id in
    incr next_id;
    id
  in
  let hd_tree =
    let ((_, hd_pats) :: tl_pats) = pats in
    exits.(0) <- nodes;
    let hd_tree = of_nonempty ~exit:0 next_id hd_pats in
    make_case ~exit:0 next_id hd_tree tl_pats
  in
  let rec aux tree exit = function
    | [] -> { tree; exits }
    | Typechecker.{ pats = (loc, hd_pats) :: tl_pats; nodes } :: l ->
        exits.(exit) <- nodes;
        let hd_tree = of_nonempty ~exit next_id hd_pats in
        let tree' = merge tree hd_tree in
        if equal_tree equal_leaf Int.equal tree tree' then Error.unused_case loc
        else
          let tree = make_case ~exit next_id tree' tl_pats in
          aux tree (succ exit) l
  in
  aux hd_tree 1 tl_cases

module ParMatch = struct
  (** Searches a given tree to find an example of a missing branch. If found,
      the match is partial. *)

  (** Represents a path through a tree. This is an intermediary structure which
      we later convert into a proper pattern.*)
  type t = Any | Const of Const.t | Nil | Cons of t | Nest of t list

  let rec to_pat ty path =
    match (!ty, path) with
    | Ty.Enum ty, Const c -> T.TConst (c, Some ty)
    | _, Const c -> TConst (c, None)
    | Tuple tys, Nest path -> TTuple (to_list tys path)
    | List ty, path -> to_list_pat ty path
    | Nullable ty, Cons (Nest [ path ]) ->
        TConstruct (TNullable, Some (to_pat ty path))
    | Nullable _, Cons Any -> TConstruct (TNullable, Some TAny)
    | Nullable _, Nil -> TConstruct (TNullable, None)
    | Record tys, Nest path ->
        let s = Map.String.to_seq !tys in
        TRecord (None, to_map Map.String.empty s path, tys)
    | Union (key, ({ cases; _ } as ty)), Nest (Const c :: path) -> (
        let key = Some (key, c, ty) in
        match (cases, c) with
        | Ty.Union.Int m, Int i ->
            let tys = Map.Int.find i m in
            let s = Map.String.to_seq !tys in
            TRecord (key, to_map Map.String.empty s path, tys)
        | Ty.Union.String m, String s ->
            let tys = Map.String.find s m in
            let s = Map.String.to_seq !tys in
            TRecord (key, to_map Map.String.empty s path, tys)
        | _ ->
            Error.internal __POS__
              "Type mismatch while parsing a union. This means the typechecker \
               failed.")
    | _ -> TAny

  and[@tail_mod_cons] to_list_pat ty = function
    | Cons (Nest [ hd; tl ]) ->
        TConstruct (TList, Some (TTuple [ to_pat ty hd; to_list_pat ty tl ]))
    | Cons Any -> TAny
    | Nil -> TConstruct (TList, None)
    | _ -> TAny

  and to_list tys path = List.map2 to_pat tys path

  and to_map acc tys path =
    match (tys (), path) with
    | Seq.Cons ((key, ty), tys), hd :: path ->
        let pat = to_pat ty hd in
        let acc = Map.String.add key pat acc in
        to_map acc tys path
    | _ -> acc

  let pp tys ppf l =
    Format.fprintf ppf "@[%a@]"
      (Format.pp_print_list ~pp_sep:Pp.sep_comma T.pp_pat)
      (to_list tys l)

  module List = struct
    (** A list indexed by its length. We describe length in terms of
        nested trees. This is useful to eliminate one case where we parse a nest
        and want to guarantee that the list returned will not be empty. *)
    type (_, _) t =
      | [] : ('a, leaf) t
      | ( :: ) : 'a * ('a, 'n) t -> ('a, ('n, 'k) tree) t
  end

  type flag = Partial | Exhaustive

  type 'a check = {
    flag : flag;
    pats : t list;
    after_nest : (t list, 'a) List.t;
  }

  let exhaustive c = { c with pats = Any :: c.pats }

  let rec check : type a k. (a, leaf) depth -> (a, k) tree -> a check =
   fun n tree ->
    match tree with
    | End x -> (
        match n with
        | Z -> { flag = Exhaustive; pats = []; after_nest = [] }
        | S n ->
            let r = check n x in
            { r with pats = []; after_nest = r.pats :: r.after_nest })
    | Wildcard { child; _ } -> exhaustive (check n child)
    | Nest { child; wildcard; _ } -> (
        (* Either the child OR the wildcard can be exhaustive.
           A nest filled with exhaustive patterns, e.g. tuple (_, _, _), can
           lead to an exhaustive path even if it's paired with a wildcard _ that
           doesn't. (In which case, the wildcard is redundant and will never be
           used.) *)
        let r =
          match child with
          | Int_keys c -> check (S n) c
          | String_keys c -> check (S n) c
        in
        match r with
        | { flag = Exhaustive; pats; after_nest = hd :: tl } -> (
            match child with
            | String_keys (Optional _) -> (
                (* Dicts always require a wildcard. *)
                match wildcard with
                | Some wildcard -> exhaustive (check n wildcard)
                | None -> { flag = Partial; pats = Any :: hd; after_nest = tl })
            | _ ->
                { flag = Exhaustive; pats = Nest pats :: hd; after_nest = tl })
        | { flag = Partial; pats; after_nest = hd :: tl } -> (
            match wildcard with
            | Some wildcard -> exhaustive (check n wildcard)
            | None ->
                { flag = Partial; pats = Nest pats :: hd; after_nest = tl }))
    | Construct { nil = None; cons = Some cons; _ } ->
        let r = check n cons in
        let pats = match r.pats with _ :: pats -> Nil :: pats | [] -> [] in
        { r with flag = Partial; pats }
    | Construct { nil = Some nil; cons = None; _ } ->
        let r = check n nil in
        { r with flag = Partial; pats = Cons Any :: r.pats }
    | Construct { nil = Some nil; cons = Some cons; _ } -> (
        match check n nil with
        | { flag = Exhaustive; _ } -> (
            match check n cons with
            | { flag = Exhaustive; _ } as r -> r
            | { flag = Partial; pats; after_nest } ->
                let pats =
                  match pats with hd :: tl -> Cons hd :: tl | [] -> []
                in
                { pats; flag = Partial; after_nest })
        | { flag = Partial; pats; after_nest } ->
            { flag = Partial; pats = Nil :: pats; after_nest })
    | Construct { nil = None; cons = None; _ } ->
        Error.internal __POS__
          "Tried to analyze a construct with neither nil nor cons."
    | Switch { wildcard = Some wildcard; _ } -> exhaustive (check n wildcard)
    | Switch { cases; wildcard = None; debug_row = `Open; _ } ->
        let r = check n cases.if_match in
        { r with flag = Partial; pats = Any :: r.pats }
    | Switch { cases; wildcard = None; debug_row = `Closed; _ } ->
        let rec aux { data; if_match; next } =
          match check n if_match with
          | { flag = Partial; pats; after_nest } ->
              { flag = Partial; pats = Const data :: pats; after_nest }
          | { flag = Exhaustive; pats; after_nest } -> (
              match next with
              | None ->
                  { flag = Exhaustive; pats = Const data :: pats; after_nest }
              | Some case -> aux case)
        in
        aux cases
    | Optional { child = Some x; _ }
    | Optional { child = None; next = Some x; _ } ->
        check n x
    | Optional { child = None; next = None; _ } ->
        Error.internal __POS__ "Tried to analyze an empty optional."

  let check = check Z
end

let partial_match_check loc tys tree =
  match ParMatch.check tree with
  | { flag = Exhaustive; _ } -> ()
  | { flag = Partial; pats; _ } -> Error.parmatch loc (ParMatch.pp tys) pats

let set_to_sexp s = Sexp.of_seq Sexp.int (Set.Int.to_seq s)

let rec tree_to_sexp :
          'leaf 'key.
          ('leaf -> Sexp.t) -> ('key -> Sexp.t) -> ('leaf, 'key) tree -> Sexp.t
    =
 fun leaf_f key_f -> function
  | Switch { key; ids; cases; wildcard; debug_row } ->
      Sexp.make "switch"
        [
          Sexp.make "key" [ key_f key ];
          Sexp.make "ids" [ set_to_sexp ids ];
          Sexp.make "cases" [ switchcase_to_sexp leaf_f key_f cases ];
          Sexp.make "wildcard"
            [ Sexp.option (tree_to_sexp leaf_f key_f) wildcard ];
          Sexp.make "debug_row" [ Ty.row_to_sexp debug_row ];
        ]
  | Nest { key; ids; child; wildcard } ->
      Sexp.make "nest"
        [
          Sexp.make "key" [ key_f key ];
          Sexp.make "ids" [ set_to_sexp ids ];
          Sexp.make "child" [ nest_to_sexp leaf_f key_f child ];
          Sexp.make "wildcard"
            [ Sexp.option (tree_to_sexp leaf_f key_f) wildcard ];
        ]
  | Construct { key; ids; nil; cons } ->
      Sexp.make "construct"
        [
          Sexp.make "key" [ key_f key ];
          Sexp.make "ids" [ set_to_sexp ids ];
          Sexp.make "nil" [ Sexp.option (tree_to_sexp leaf_f key_f) nil ];
          Sexp.make "cons" [ Sexp.option (tree_to_sexp leaf_f key_f) cons ];
        ]
  | Wildcard { key; ids; child } ->
      Sexp.make "wildcard"
        [
          Sexp.make "key" [ key_f key ];
          Sexp.make "ids" [ set_to_sexp ids ];
          Sexp.make "child" [ tree_to_sexp leaf_f key_f child ];
        ]
  | Optional { child; next } ->
      Sexp.make "optional"
        [
          Sexp.make "child" [ Sexp.option (tree_to_sexp leaf_f key_f) child ];
          Sexp.make "next" [ Sexp.option (tree_to_sexp leaf_f key_f) next ];
        ]
  | End leaf -> Sexp.make "end" [ leaf_f leaf ]

and nest_to_sexp :
      'leaf 'key.
      ('leaf -> Sexp.t) -> ('key -> Sexp.t) -> ('leaf, 'key) nest -> Sexp.t =
 fun leaf_f key_f -> function
  | Int_keys tree ->
      Sexp.make "int_keys"
        [ tree_to_sexp (tree_to_sexp leaf_f key_f) Sexp.int tree ]
  | String_keys tree ->
      Sexp.make "string_keys"
        [ tree_to_sexp (tree_to_sexp leaf_f key_f) Sexp.string tree ]

and switchcase_to_sexp :
      'leaf 'key.
      ('leaf -> Sexp.t) ->
      ('key -> Sexp.t) ->
      ('leaf, 'key) switchcase ->
      Sexp.t =
 fun leaf_f key_f { data; if_match; next } ->
  Sexp.make "case"
    [
      Sexp.make "data" [ Data.Const.to_sexp data ];
      Sexp.make "if_match" [ tree_to_sexp leaf_f key_f if_match ];
      Sexp.make "next" [ Sexp.option (switchcase_to_sexp leaf_f key_f) next ];
    ]

let leaf_to_sexp { names; exit } =
  Sexp.make "leaf"
    [
      Sexp.make "names" [ Sexp.map_string Sexp.int names ];
      Sexp.make "exit" [ Sexp.int exit ];
    ]

let to_sexp f { tree; exits } =
  Sexp.make "matching"
    [
      Sexp.make "tree" [ tree_to_sexp leaf_to_sexp Sexp.int tree ];
      Sexp.make "exits"
        [ Sexp.of_seq (Sexp.pair Sexp.int f) (Array.to_seqi exits) ];
    ]
