(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module TPat = Typechecker.Pattern
module Ty = Typescheme
module Const = Data.Const

type debug_nest_info = Tuple | Dict | Record | Union of Ty.Variant.extra
[@@deriving eq, show]

type ('leaf, 'key) tree =
  | Switch of {
      key : 'key;
      ids : Set.Int.t; [@printer Pp.set_int]
      cases : ('leaf, 'key) switchcase;
      row : [ `Open | `Closed ];
      wildcard : ('leaf, 'key) tree option;
    }
  | Nest of {
      key : 'key;
      ids : Set.Int.t; [@printer Pp.set_int]
      child : ('leaf, 'key) nest;
      wildcard : ('leaf, 'key) tree option;
      debug : debug_nest_info;
    }
  | Construct of {
      key : 'key;
      ids : Set.Int.t; [@printer Pp.set_int]
      nil : ('leaf, 'key) tree option;
      cons : ('leaf, 'key) tree option;
      debug : TPat.construct;
    }
  | Wildcard of {
      key : 'key;
      ids : Set.Int.t; [@printer Pp.set_int]
      child : ('leaf, 'key) tree;
    }
  | End of 'leaf

and ('leaf, 'key) nest =
  | Int_keys of (('leaf, 'key) tree, int) tree
  | String_keys of (('leaf, 'key) tree, string) tree

and ('leaf, 'key) switchcase = {
  data : Const.t;
  if_match : ('leaf, 'key) tree;
  next_case : ('leaf, 'key) switchcase option;
}
[@@deriving eq, show]

module Exit = struct
  type key = int

  let equal_key = Int.equal
  let pp_key = Format.pp_print_int

  type 'a t = 'a array

  let get = Array.get
  let map = Array.map
  let unsafe_key i = i
end

type leaf = {
  names : int Map.String.t; [@printer Pp.map_string Format.pp_print_int]
  exit : Exit.key;
}
[@@deriving eq, show]

type 'a t = { tree : (leaf, int) tree; exits : 'a Exit.t }

(** One challenge when merging the trees is that we need to keep track of "where
    we are" in the tree on the type level. This natural-number GADT does that
    for us.*)
type (_, _) nat =
  | Z : ('z, 'z) nat
  | S : ('a, 'z) nat -> (('a, 'key) tree, 'z) nat

(** Whenever we iterate through cases, we do it tail-recursively which creates
    a reversed accumulator. We need to reverse it again back to normal. *)
let rec rev_cases ?tail t =
  let tail = { t with next_case = tail } in
  match t.next_case with None -> tail | Some t -> rev_cases ~tail t

exception Merge_fail
(** If a branch of tree cannot merge with another branch, then we throw that
    branch away. If an entire tree failed to merge, then we know its pattern was
    redundant.
    *)

(** Sort the data for easier analysis. If the new value is lower than the
    original, then append it to the beginning. If the new value equals the
    original, then merge them. If the new value is greater than the original,
    then check if we're at the end of the list. If we are, then add this new
    case. If not, then keep searching. *)
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
        { original with if_match = merge n original.if_match if_match }
      in
      match init with
      | None -> Some tail
      | Some init -> Some (rev_cases ~tail init)
    with Merge_fail -> None
  else
    let init = { original with next_case = init } in
    match original.next_case with
    | None -> Some (rev_cases ~tail:{ data; if_match; next_case = None } init)
    | Some original -> merge_testcases_aux ~init original data if_match n

(** When merging a and b, we take each value from b and try to merge it with the
    items in a. At least one value must merge in order to be considered a
    success. *)
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

(** When we merge a list of values with a wildcard, some of them may not merge
    successfully. As long as at least one merges, then this function succeeds.
    We filter out any unsuccessful mergers, since their paths are covered by the
    wildcard case. *)
and merge_testcases_into_wildcard :
    type a k.
    ?init:(a, k) switchcase ->
    (a, k) tree ->
    (a, k) switchcase ->
    (a, leaf) nat ->
    (a, k) switchcase option =
 fun ?init wildcard t n ->
  let init =
    try Some { t with if_match = merge n wildcard t.if_match; next_case = init }
    with Merge_fail -> init
  in
  match (t.next_case, init) with
  | None, None -> None
  | None, Some init -> Some (rev_cases init)
  | Some t, init -> merge_testcases_into_wildcard ?init wildcard t n

(** When we expand a wildcard into a list of test values, we merge it with each
    child and ignore any errors. Merge failures are replaced by their unmerged
    originals. *)
and expand_wildcard_into_testcases :
    type a k.
    ?init:(a, k) switchcase ->
    (a, k) switchcase ->
    (a, k) tree ->
    (a, leaf) nat ->
    (a, k) switchcase =
 fun ?init { data; if_match; next_case } wildcard n ->
  let init =
    try { data; if_match = merge n if_match wildcard; next_case = init }
    with Merge_fail -> { data; if_match; next_case = init }
  in
  match next_case with
  | None -> rev_cases init
  | Some a -> expand_wildcard_into_testcases ~init a wildcard n

(** When we expand a wildcard into a nest, we need to expand all of the
    wildcard's child nodes after the nest's child nodes.  We need a second [nat]
    value to track "where we are" relative to the beginning of the nest. [Z]
    represents the depth where the nest began. It doesn't matter if the
    expansion succeeds or not. If it fails, we just keep the original tree.*)
and expand_wildcard_after_nest :
    type a b ka kb.
    (a, ka) tree ->
    (a, leaf) nat ->
    wildcard:(b, kb) tree ->
    (a, (b, kb) tree) nat ->
    (a, ka) tree =
 fun a na ~wildcard nb ->
  match (a, na, nb) with
  | End a, S n, Z -> ( try End (merge n a wildcard) with Merge_fail -> End a)
  | End a, S na, S nb -> End (expand_wildcard_after_nest a na ~wildcard nb)
  | Nest a, na, nb ->
      let child =
        match a.child with
        | Int_keys c ->
            Int_keys (expand_wildcard_after_nest c (S na) ~wildcard (S nb))
        | String_keys c ->
            String_keys (expand_wildcard_after_nest c (S na) ~wildcard (S nb))
      in
      Nest { a with child }
  | Construct a, na, nb ->
      let nil =
        match a.nil with
        | None -> None
        | Some c -> Some (expand_wildcard_after_nest c na ~wildcard nb)
      in
      let cons =
        match a.cons with
        | None -> None
        | Some c -> Some (expand_wildcard_after_nest c na ~wildcard nb)
      in
      Construct { a with nil; cons }
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

(** When we merge a wildcard after a nest, we can only keep successful mergers.
    For constructs and switch nodes, then at least one path must merge
    succesfully. Unsuccesful paths are filtered out. *)
and merge_wildcard_after_nest :
    type a b ka kb.
    wildcard:(a, ka) tree ->
    (b, (a, ka) tree) nat ->
    (b, kb) tree ->
    (b, leaf) nat ->
    (b, kb) tree =
 fun ~wildcard na b nb ->
  match (b, na, nb) with
  | End b, Z, S n -> End (merge n wildcard b)
  | End b, S na, S nb -> End (merge_wildcard_after_nest ~wildcard na b nb)
  | Nest b, na, nb ->
      let child =
        match b.child with
        | Int_keys c ->
            Int_keys (merge_wildcard_after_nest ~wildcard (S na) c (S nb))
        | String_keys c ->
            String_keys (merge_wildcard_after_nest ~wildcard (S na) c (S nb))
      in
      Nest { b with child }
  | Construct b, na, nb -> (
      (* At least one branch must successfully merge. Unsuccessful
          mergers are discarded either way. *)
      let nil =
        match b.nil with
        | None -> Ok None
        | Some c -> (
            try Ok (Some (merge_wildcard_after_nest ~wildcard na c nb))
            with Merge_fail -> Error None)
      in
      let cons =
        match b.cons with
        | None -> Ok None
        | Some c -> (
            try Ok (Some (merge_wildcard_after_nest ~wildcard na c nb))
            with Merge_fail -> Error None)
      in
      match (nil, cons) with
      | Error _, Error _ | Ok None, Error None | Error None, Ok None ->
          raise_notrace Merge_fail
      | Ok nil, Ok cons | Ok nil, Error cons | Error nil, Ok cons ->
          Construct { b with nil; cons })
  | Wildcard b, na, nb ->
      Wildcard
        { b with child = merge_wildcard_after_nest ~wildcard na b.child nb }
  | Switch b, na, nb ->
      (* At least one branch must successfully merge. Unsuccessful
          mergers are discarded either way. *)
      let wildcard' =
        match b.wildcard with
        | None -> None
        | Some b -> (
            try Some (merge_wildcard_after_nest ~wildcard na b nb)
            with Merge_fail -> None)
      in
      let rec aux init case =
        let init =
          try
            let if_match =
              merge_wildcard_after_nest ~wildcard na case.if_match nb
            in
            Some { case with if_match; next_case = init }
          with Merge_fail -> init
        in
        match (case.next_case, init) with
        | None, None -> raise_notrace Merge_fail
        | None, Some init ->
            Switch { b with cases = rev_cases init; wildcard = wildcard' }
        | Some a, init -> aux init a
      in
      aux None b.cases

and merge : type a k. (a, leaf) nat -> (a, k) tree -> (a, k) tree -> (a, k) tree
    =
 fun n a b ->
  match (a, b) with
  | End a, End b -> (
      match n with Z -> raise_notrace Merge_fail | S n -> End (merge n a b))
  | Wildcard a, Wildcard b ->
      let ids = Set.Int.union a.ids b.ids in
      let child = merge n a.child b.child in
      Wildcard { a with ids; child }
  | Wildcard a, Nest b ->
      let wildcard =
        match b.wildcard with None -> a.child | Some b -> merge n a.child b
      in
      let child =
        match b.child with
        | Int_keys c ->
            Int_keys (merge_wildcard_after_nest ~wildcard:a.child Z c (S n))
        | String_keys c ->
            String_keys (merge_wildcard_after_nest ~wildcard:a.child Z c (S n))
      in
      let ids = Set.Int.union a.ids b.ids in
      Nest { b with ids; child; wildcard = Some wildcard }
  | (Wildcard a as a'), Construct b -> (
      (* Try to merge the wildcard with either construct branch. At least one
         must succeed. *)
      let nil =
        match b.nil with
        | None -> Ok (Some a.child)
        | Some b -> (
            try Ok (Some (merge n a.child b)) with Merge_fail -> Error None)
      in
      let cons =
        match b.cons with
        | None -> Ok (Some a')
        | Some b -> (
            try Ok (Some (merge n a' b)) with Merge_fail -> Error None)
      in
      let ids = Set.Int.union a.ids b.ids in
      match (nil, cons) with
      | Error _, Error _ | Ok None, Error None | Error None, Ok None ->
          raise_notrace Merge_fail
      | Ok nil, Ok cons | Error nil, Ok cons | Ok nil, Error cons ->
          Construct { b with ids; nil; cons })
  | Wildcard a, Switch b -> (
      match merge_testcases_into_wildcard a.child b.cases n with
      | None -> raise_notrace Merge_fail
      | Some cases ->
          let wildcard =
            match b.wildcard with
            | None -> a.child
            | Some b -> merge n a.child b
          in
          let ids = Set.Int.union a.ids b.ids in
          Switch { b with ids; cases; wildcard = Some wildcard })
  | Nest a, Nest b ->
      let wildcard =
        match (a.wildcard, b.wildcard) with
        | None, None -> None
        | Some x, None | None, Some x -> Some x
        | Some a, Some b -> Some (merge n a b)
      in
      let child =
        match (a.child, b.child) with
        | Int_keys a, Int_keys b -> (
            let child = merge (S n) a b in
            match wildcard with
            | None -> Int_keys child
            | Some wildcard ->
                Int_keys (merge_wildcard_after_nest ~wildcard Z child (S n)))
        | String_keys a, String_keys b -> (
            let child = merge (S n) a b in
            match wildcard with
            | None -> String_keys child
            | Some wildcard ->
                String_keys (merge_wildcard_after_nest ~wildcard Z child (S n)))
        | _ -> assert false
      in
      let ids = Set.Int.union a.ids b.ids in
      Nest { a with ids; child; wildcard }
  | Nest a, Wildcard b ->
      let child =
        match a.child with
        | String_keys child ->
            String_keys
              (expand_wildcard_after_nest child (S n) ~wildcard:b.child Z)
        | Int_keys child ->
            Int_keys
              (expand_wildcard_after_nest child (S n) ~wildcard:b.child Z)
      in
      let wildcard =
        match a.wildcard with None -> b.child | Some a -> merge n a b.child
      in
      let ids = Set.Int.union a.ids b.ids in
      Nest { a with ids; child; wildcard = Some wildcard }
  | Construct a, (Wildcard b as b') -> (
      let nil =
        match a.nil with
        | None -> Ok b.child
        | Some a -> ( try Ok (merge n a b.child) with Merge_fail -> Error a)
      in
      let cons =
        match a.cons with
        | None -> Ok b'
        | Some a -> ( try Ok (merge n a b') with Merge_fail -> Error a)
      in
      let ids = Set.Int.union a.ids b.ids in
      match (nil, cons) with
      | Error _, Error _ -> raise_notrace Merge_fail
      | Ok nil, Ok cons | Ok nil, Error cons | Error nil, Ok cons ->
          Construct { a with ids; nil = Some nil; cons = Some cons })
  | Construct a, Construct b ->
      let nil =
        match (a.nil, b.nil) with
        | None, None -> None
        | Some x, None | None, Some x -> Some x
        | Some a, Some b -> Some (merge n a b)
      in
      let cons =
        match (a.cons, b.cons) with
        | None, None -> None
        | Some x, None | None, Some x -> Some x
        | Some a, Some b -> Some (merge n a b)
      in
      let ids = Set.Int.union a.ids b.ids in
      Construct { a with ids; nil; cons }
  | Switch a, Wildcard b ->
      let wildcard =
        match a.wildcard with None -> b.child | Some a -> merge n a b.child
      in
      let cases = expand_wildcard_into_testcases a.cases b.child n in
      let ids = Set.Int.union a.ids b.ids in
      Switch { a with ids; cases; wildcard = Some wildcard }
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
            match merge_testcases_into_wildcard wildcard b.cases n with
            | Some cases -> cases
            | None -> raise_notrace Merge_fail)
        | None -> b.cases
      in
      let cases =
        match merge_testcases a.cases bcases n false with
        | None -> raise_notrace Merge_fail
        | Some cases -> cases
      in
      let cases =
        match b.wildcard with
        | None -> cases
        | Some wildcard -> expand_wildcard_into_testcases cases wildcard n
      in
      let ids = Set.Int.union a.ids b.ids in
      Switch { a with ids; cases; wildcard }
  | Switch _, (Nest _ | Construct _)
  | Construct _, (Switch _ | Nest _)
  | Nest _, (Switch _ | Construct _)
  | (Switch _ | Nest _ | Construct _ | Wildcard _), End _
  | End _, (Switch _ | Nest _ | Construct _ | Wildcard _) ->
      assert false

let merge = merge Z

(** To turn a typed pattern into a tree (albiet a single-branch of tree), we use
    CPS as an easy way deal with the nested data type. *)

type bindings = { next_id : unit -> int; names : int Map.String.t }
type ('a, 'k) cont = bindings -> ('a, 'k) tree

let of_const key data if_match enum =
  Switch
    {
      key;
      ids = Set.Int.empty;
      cases = { data; if_match; next_case = None };
      row =
        (match enum with Some { Ty.Variant.row; _ } -> row | None -> `Open);
      wildcard = None;
    }

let rec of_tpat :
          'a 'k. key:'k -> bindings -> ('a, 'k) cont -> TPat.t -> ('a, 'k) tree
    =
 fun ~key b k -> function
  | TAny -> Wildcard { ids = Set.Int.empty; key; child = k b }
  | TVar x ->
      let id = b.next_id () in
      let b = { b with names = Map.String.add x id b.names } in
      Wildcard { ids = Set.Int.singleton id; key; child = k b }
  | TConstruct (kind, Some cons) ->
      let child = of_tpat ~key b k cons in
      Construct
        {
          key;
          ids = Set.Int.empty;
          debug = kind;
          nil = None;
          cons = Some child;
        }
  | TConstruct (kind, None) ->
      Construct
        {
          key;
          ids = Set.Int.empty;
          debug = kind;
          nil = Some (k b);
          cons = None;
        }
  | TConst (data, enum) -> of_const key data (k b) enum
  | TTuple l ->
      let child = Int_keys (of_list ~key:0 b (fun b -> End (k b)) l) in
      Nest { key; ids = Set.Int.empty; child; wildcard = None; debug = Tuple }
  | TRecord (tag, m, tys) ->
      (* We need to expand the map to include all of its type's keys. *)
      let l =
        Map.String.merge
          (fun _k _ty p ->
            match p with None -> Some TPat.TAny | Some p -> Some p)
          !tys m
        |> Map.String.to_seq
      in
      let child = of_keyvalues b (fun b -> End (k b)) l in
      let child, debug =
        match tag with
        | Some (key, data, union) ->
            (of_const key data child (Some union), Union union.extra)
        | None -> (child, Record)
      in
      Nest
        {
          key;
          ids = Set.Int.empty;
          child = String_keys child;
          wildcard = None;
          debug;
        }
  | TDict (m, kys) ->
      (* We need to expand the map to include all of its type's keys. *)
      let l =
        !kys |> Set.String.to_seq
        |> Seq.map (fun key -> (key, TPat.TAny))
        |> Map.String.of_seq
        |> Map.String.merge
             (fun _ p kpat ->
               match (p, kpat) with
               | Some p, _ | None, Some p -> Some p
               | None, None -> None)
             m
        |> Map.String.to_seq
      in
      let child = of_keyvalues b (fun b -> End (k b)) l in
      Nest
        {
          key;
          ids = Set.Int.empty;
          child = String_keys child;
          wildcard = None;
          debug = Dict;
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
  of_tpat ~key:0 { next_id; names = Map.String.empty } k hd

let rec make_case ~exit next_id tree = function
  | [] -> tree
  | (loc, ps) :: l ->
      let b = of_nonempty ~exit next_id ps in
      let tree = try merge tree b with Merge_fail -> Error.unused_case loc in
      make_case ~exit next_id tree l

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
    let ((_, hd_pats) :: tl_pats) = pats in
    Queue.add nodes exitq;
    let exit = Queue.length exitq - 1 in
    let hd_tree = of_nonempty ~exit next_id hd_pats in
    make_case ~exit next_id hd_tree tl_pats
  in
  let rec aux tree = function
    | [] -> { tree; exits = exitq |> Queue.to_seq |> Array.of_seq }
    | Typechecker.{ pats = (loc, hd_pats) :: tl_pats; nodes } :: l ->
        Queue.add nodes exitq;
        let exit = Queue.length exitq - 1 in
        let hd_tree = of_nonempty ~exit next_id hd_pats in
        let tree =
          try merge tree hd_tree with Merge_fail -> Error.unused_case loc
        in
        let tree = make_case ~exit next_id tree tl_pats in
        aux tree l
  in
  aux hd_tree tl_cases

module ParMatch = struct
  type flag = Partial | Exhaustive
  type 'a t = { flag : flag; pats : (string * TPat.t) list; next : 'a }

  let values l = List.map (fun (_, v) -> v) l
  let to_keyvalues l = l |> List.to_seq |> Map.String.of_seq
  let pp_pats ppf l = Format.pp_print_list ~pp_sep:Pp.sep_comma TPat.pp ppf l
  let key_str = Fun.id
  let key_int _ = ""

  let exhaustive key { pats; flag; next } =
    { flag; next; pats = (key, TAny) :: pats }

  let rec check : 'a 'k. ('k -> string) -> ('a, 'k) tree -> 'a t =
   fun kf -> function
    | End next -> { next; flag = Exhaustive; pats = [] }
    | Wildcard { key; child; _ } -> exhaustive (kf key) (check kf child)
    | Nest { key; child; wildcard; debug; _ } -> (
        (* Either the child _or_ the wildcard can be exhaustive.
           A nest filled with exhaustive patterns e.g. tuple (_, _, _) can lead
           to an exhaustive path even if it's paired with a wildcard _ that
           doesn't. (In which case, the wildcard is redundant and will never be
           used.) *)
        let result =
          match child with
          | Int_keys child -> check key_int child
          | String_keys child -> check key_str child
        in
        match result with
        | { flag = Exhaustive; next; _ } -> (
            match debug with
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
                  match debug with
                  | Tuple -> TPat.TTuple (values pats)
                  | Record ->
                      TRecord (None, to_keyvalues pats, ref Map.String.empty)
                  | Union extra -> (
                      match pats with
                      | (k, TConst (v, _)) :: pats ->
                          TRecord
                            ( Some
                                ( k,
                                  v,
                                  {
                                    cases = String Map.String.empty;
                                    row = `Closed;
                                    extra;
                                  } ),
                              to_keyvalues pats,
                              ref Map.String.empty )
                          (* If the child (tag) was not a constant, then it was
                             a TAny and we report the entire union pattern as a
                             TAny. *)
                      | _ -> TAny)
                  | Dict -> TDict (to_keyvalues pats, ref Set.String.empty)
                in
                let { pats; next; _ } = check kf next in
                { next; flag = Partial; pats = (kf key, nest) :: pats }))
    | Construct { key; debug; nil; cons; _ } -> (
        match (nil, cons) with
        | None, Some cons ->
            let { pats; next; _ } = check kf cons in
            {
              flag = Partial;
              pats =
                (match pats with
                | _ :: pats -> (kf key, TConstruct (debug, None)) :: pats
                | _ -> assert false);
              next;
            }
        | Some nil, None ->
            let { pats; next; _ } = check kf nil in
            {
              flag = Partial;
              pats = (kf key, TConstruct (debug, Some TAny)) :: pats;
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
                          (key, TPat.TConstruct (debug, Some cons)) :: pats
                      | _ -> assert false
                    in
                    { pats; next; flag = Partial })
            | { flag = Partial; pats; next } ->
                {
                  flag = Partial;
                  pats = (kf key, TPat.TConstruct (debug, None)) :: pats;
                  next;
                })
        | None, None -> assert false)
    | Switch { key; cases; wildcard; row; _ } -> (
        match wildcard with
        | Some wildcard -> exhaustive (kf key) (check kf wildcard)
        | None -> (
            match row with
            | `Open ->
                let { pats; next; _ } = check kf cases.if_match in
                { flag = Partial; pats = (kf key, TAny) :: pats; next }
            | `Closed ->
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

let partial_match_check loc tree =
  match ParMatch.check ParMatch.key_int tree with
  | { flag = Exhaustive; _ } -> ()
  | { flag = Partial; pats; _ } ->
      Error.parmatch loc ParMatch.pp_pats (ParMatch.values pats)