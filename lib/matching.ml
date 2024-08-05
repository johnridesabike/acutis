(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module MapInt = Map.Make (Int)
module MapString = Map.Make (String)
module SetInt = Set.Make (Int)
module SetString = Set.Make (String)
module T = Typechecker

type scalar = [ `Int of int | `Float of float | `String of string ]

let int i = `Int i
let string s = `String s

let scalar_compare a b =
  match (a, b) with
  | `Int a, `Int b -> Int.compare a b
  | `String a, `String b -> String.compare a b
  | `Float a, `Float b -> Float.compare a b
  | `Int _, (`String _ | `Float _) | `String _, `Float _ -> -1
  | `String _, `Int _ | `Float _, (`Int _ | `String _) -> 1

let scalar_equal a b = scalar_compare a b = 0

type internal_check_cases = scalar Seq.t option

type ('leaf, 'key) tree =
  | Switch of {
      key : 'key;
      ids : SetInt.t;
      cases : ('leaf, 'key) switchcase;
      wildcard : ('leaf, 'key) tree option;
      check_cases : internal_check_cases;
    }
  | Nest of {
      key : 'key;
      ids : SetInt.t;
      child : ('leaf, 'key) nest;
      wildcard : ('leaf, 'key) tree option;
          (** A nest should only contain a wildcard if its child is not
              exhaustive. This requires some extra computation, seen below. *)
    }
  | Nil of { key : 'key; ids : SetInt.t; child : ('leaf, 'key) tree }
  | Cons of { key : 'key; ids : SetInt.t; child : ('leaf, 'key) tree }
  | Nil_or_cons of {
      key : 'key;
      ids : SetInt.t;
      nil : ('leaf, 'key) tree;
      cons : ('leaf, 'key) tree;
    }
  | Wildcard of { key : 'key; ids : SetInt.t; child : ('leaf, 'key) tree }
  | Optional of { child : ('leaf, 'key) tree; next : ('leaf, 'key) tree option }
  | End of 'leaf

and ('leaf, 'key) nest =
  | Int_keys of (('leaf, 'key) tree, int) tree
  | String_keys of (('leaf, 'key) tree, string) tree

and ('leaf, 'key) switchcase = {
  data : scalar;
  if_match : ('leaf, 'key) tree;
  next : ('leaf, 'key) switchcase option;
}

module Exits = struct
  type key = int

  let equal_key = Int.equal
  let key_to_int = Fun.id

  type 'a exit = { bindings : string list; nodes : 'a }
  type 'a t = 'a exit array

  let make len = Array.make len { bindings = []; nodes = [] }
  let set a i bindings nodes = a.(i) <- { bindings; nodes }

  let map f a =
    Array.map (fun { bindings; nodes } -> { bindings; nodes = f nodes }) a

  let binding_exists a =
    Array.exists (function { bindings = _ :: _; _ } -> true | _ -> false) a

  let nodes a = Array.to_seq a |> Seq.map (fun exit -> exit.nodes)

  let to_seq a =
    Array.to_seqi a
    |> Seq.map (fun (i, { bindings; nodes }) -> (i, bindings, nodes))

  let exit_to_sexp f (i, { bindings; nodes }) =
    Sexp.make "exit"
      [
        Sexp.int i;
        Sexp.make "bindings" [ Sexp.of_seq Sexp.string (List.to_seq bindings) ];
        Sexp.make "nodes" [ f nodes ];
      ]

  let to_sexp f a = Array.to_seqi a |> Sexp.of_seq (exit_to_sexp f)
end

type leaf = { names : int MapString.t; exit : Exits.key }
type 'a t = { tree : (leaf, int) tree; exits : 'a Exits.t }

(* Don't check equality for IDs. A merge failure may still modify IDs, creating
   a false-negative equality.
   Don't check equality for keys or check_cases, because they should never
   change anyway. *)

let rec equal_tree :
      'leaf 'key.
      ('leaf -> 'leaf -> bool) ->
      ('leaf, 'key) tree ->
      ('leaf, 'key) tree ->
      bool =
 fun equal_leaf a b ->
  match (a, b) with
  | Switch a, Switch { cases; wildcard; ids = _; key = _; check_cases = _ } ->
      equal_switchcase equal_leaf a.cases cases
      && Option.equal (equal_tree equal_leaf) a.wildcard wildcard
  | Nest a, Nest { child; wildcard; key = _; ids = _ } ->
      equal_nest equal_leaf a.child child
      && Option.equal (equal_tree equal_leaf) a.wildcard wildcard
  | Wildcard { child = a; _ }, Wildcard { child = b; key = _; ids = _ }
  | Nil { child = a; _ }, Nil { child = b; key = _; ids = _ }
  | Cons { child = a; _ }, Cons { child = b; key = _; ids = _ } ->
      equal_tree equal_leaf a b
  | Nil_or_cons a, Nil_or_cons { nil; cons; key = _; ids = _ } ->
      equal_tree equal_leaf a.nil nil && equal_tree equal_leaf a.cons cons
  | Optional a, Optional { child; next } ->
      equal_tree equal_leaf a.child child
      && Option.equal (equal_tree equal_leaf) a.next next
  | End a, End b -> equal_leaf a b
  | ( ( Switch _ | Nest _ | Nil _ | Cons _ | Nil_or_cons _ | Wildcard _
      | Optional _ | End _ ),
      _ ) ->
      false

and equal_nest equal_leaf a b =
  match (a, b) with
  | Int_keys a, Int_keys b -> equal_tree (equal_tree equal_leaf) a b
  | String_keys a, String_keys b -> equal_tree (equal_tree equal_leaf) a b
  | _ -> false

and equal_switchcase equal_leaf a { data; if_match; next } =
  scalar_equal a.data data
  && equal_tree equal_leaf a.if_match if_match
  && Option.equal (equal_switchcase equal_leaf) a.next next

let equal_leaf a { names; exit } =
  MapString.equal Int.equal a.names names && Exits.equal_key a.exit exit

(** One challenge when merging the trees is that we need to keep track of where
    we are in the tree. This natural-number GADT tracks that for us on the type
    level. *)
type (_, _) depth =
  | Z : ('z, 'z) depth
  | S : ('a, 'z) depth -> (('a, 'key) tree, 'z) depth

(** Equality for n-depth nested trees. *)
let equal_tree_nested =
  let rec aux : type a. (a, leaf) depth -> a -> a -> bool = function
    | Z -> equal_leaf
    | S n -> equal_tree (aux n)
  in
  fun n -> equal_tree (aux n)

module ParMatch = struct
  (** This searches a given tree to find an example of a missing branch. *)

  (** We construct a list of values that represent a path of patterns through a
      tree. If the tree is partial, then we combine the path with a type to make
      an AST pattern and print it as a counterexample of what is not matched.

      One consideration is that our trees (and our paths) have no knowledge of
      union types. A path may contain a list of values from one case of a union,
      but also a value for a tag that belongs to another case. We need to check
      for this before printing the pattern. *)

  (** Represents whether a path follows its preceding scalar value or a
      different value. This is relevant for union tags. *)
  type scalar_path = Same | Diverge

  type path = Any | Nil | Scalar of scalar * scalar_path | Nest of path list

  let l = Loc.dummy

  (** Turn closed sum types into an example of their possible values. All other
      types become wildcards. *)
  let rec ty_to_pat ty =
    match !ty with
    | T.Type.Enum_int ({ cases; row = `Closed }, Bool) ->
        Ast.Bool (l, SetInt.min_elt cases)
    | Enum_int ({ cases; row = `Closed }, Not_bool) ->
        Ast.Enum_int (l, SetInt.min_elt cases)
    | Enum_string { cases; row = `Closed } ->
        Ast.Enum_string (l, SetString.min_elt cases)
    | Union_int (k, { cases; row = `Closed }, Bool) ->
        let tag, tys = MapInt.min_binding cases in
        Ast.Record (l, (l, k, Tag (Tag_bool (l, tag))) :: ty_map_to_pats tys)
    | Union_int (k, { cases; row = `Closed }, Not_bool) ->
        let tag, tys = MapInt.min_binding cases in
        Ast.Record (l, (l, k, Tag (Tag_int (l, tag))) :: ty_map_to_pats tys)
    | Union_string (k, { cases; row = `Closed }) ->
        let tag, tys = MapString.min_binding cases in
        Ast.Record (l, (l, k, Tag (Tag_string (l, tag))) :: ty_map_to_pats tys)
    | Int | String | Float | Tuple _ | Unknown _ | Nullable _ | List _
    | Record _ | Dict _ | Enum_int _ | Enum_string _ | Union_int _
    | Union_string _ ->
        Ast.dummy_var

  and ty_map_to_pats tys =
    MapString.to_seq !tys
    |> Seq.map (fun (k, v) -> (l, k, Ast.Value (ty_to_pat v)))
    |> List.of_seq

  let rec to_pat ty path =
    match (!ty, path) with
    | T.Type.Enum_int (_, Bool), Scalar (`Int i, _) -> Ast.Bool (l, i)
    | Enum_int _, Scalar (`Int i, _) -> Ast.Enum_int (l, i)
    | Enum_string _, Scalar (`String s, _) -> Ast.Enum_string (l, s)
    | _, Scalar (`Int i, _) -> Ast.Int (l, i)
    | _, Scalar (`String s, _) -> Ast.String (l, s)
    | _, Scalar (`Float f, _) -> Ast.Float (l, f)
    | Tuple tys, Nest path -> Ast.Tuple (l, to_list tys path)
    | List ty, path -> to_list_pat [] ty path
    | Nullable ty, Nest [ path ] -> Ast.Nullable (l, Some (to_pat ty path))
    | Nullable _, Any -> Ast.Nullable (l, Some Ast.dummy_var)
    | Nullable _, Nil -> Ast.Nullable (l, None)
    | Record tys, Nest path -> (
        let s = MapString.to_seq !tys in
        match to_assoc s path with
        | [] -> Ast.dummy_var
        | hd :: tl -> Ast.Record (l, hd :: tl))
    | Union_int (k, { cases; _ }, b), Nest (Scalar (`Int i, div) :: path) ->
        let tag =
          match b with
          | Bool -> Ast.Tag_bool (l, i)
          | Not_bool -> Ast.Tag_int (l, i)
        in
        let tys = MapInt.find i cases in
        let assoc =
          match div with
          | Same -> to_assoc (MapString.to_seq !tys) path
          | Diverge -> ty_map_to_pats tys
        in
        Ast.Record (l, (l, k, Tag tag) :: assoc)
    | Union_string (k, { cases; _ }), Nest (Scalar (`String s, div) :: path) ->
        let tys = MapString.find s cases in
        let assoc =
          match div with
          | Same -> to_assoc (MapString.to_seq !tys) path
          | Diverge -> ty_map_to_pats tys
        in
        Ast.Record (l, (l, k, Tag (Tag_string (l, s))) :: assoc)
    | _ -> Ast.dummy_var

  and to_list_pat acc ty = function
    | Nest [ hd; tl ] -> to_list_pat (to_pat ty hd :: acc) ty tl
    | Nil -> Ast.List (l, List.rev acc, None)
    | Any | _ -> Ast.List (l, List.rev acc, Some Ast.dummy_var)

  and to_list tys path = List.map2 to_pat tys path

  and[@tail_mod_cons] to_assoc tys path =
    match (tys (), path) with
    | Seq.Cons ((key, ty), tys), hd :: path ->
        (l, key, Ast.Value (to_pat ty hd)) :: to_assoc tys path
    | _ -> []

  let pp tys ppf l =
    Format.fprintf ppf "@[%a@]"
      (Format.pp_print_list ~pp_sep:Pp.comma Ast.pp_pat)
      (to_list (Nonempty.to_list tys) l)

  module L = struct
    (** A list indexed by its length. We describe length in terms of nested
        trees. This is useful to eliminate one case where we parse a nest and
        want to guarantee that the list returned will not be empty. *)
    type (_, _) t =
      | [] : ('a, leaf) t
      | ( :: ) : 'a * ('a, 'n) t -> ('a, ('n, 'k) tree) t
  end

  type status = Partial | Exhaustive

  type 'a t = {
    status : status;
    path : path list;
        (** We must record the paths for both partial and exhaustive trees
            because a partial tree may contain an exhaustive sub-tree. *)
    after_nest : (path list, 'a) L.t;
  }

  let exhaustive c = { c with path = Any :: c.path }

  let rec check : type a k. (a, leaf) depth -> (a, k) tree -> a t =
   fun n tree ->
    match tree with
    | End x -> (
        match n with
        | Z -> { status = Exhaustive; path = []; after_nest = [] }
        | S n ->
            let r = check n x in
            { r with path = []; after_nest = r.path :: r.after_nest })
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
        | { status = Exhaustive; path; after_nest = hd :: tl } -> (
            (* Dicts always require a wildcard if they're not empty. *)
            match (child, wildcard, path) with
            | String_keys (Optional _), None, _ :: _ ->
                { status = Partial; path = Any :: hd; after_nest = tl }
            | String_keys (Optional _), Some wildcard, _ ->
                exhaustive (check n wildcard)
            | _ ->
                { status = Exhaustive; path = Nest path :: hd; after_nest = tl }
            )
        | { status = Partial; path; after_nest = hd :: tl } -> (
            match wildcard with
            | Some wildcard -> exhaustive (check n wildcard)
            | None ->
                { status = Partial; path = Nest path :: hd; after_nest = tl }))
    | Cons { child; _ } ->
        let r = check n child in
        let path = match r.path with _ :: path -> Nil :: path | [] -> [] in
        { r with status = Partial; path }
    | Nil { child; _ } ->
        let r = check n child in
        { r with status = Partial; path = Any :: r.path }
    | Nil_or_cons { nil; cons; _ } -> (
        match check n nil with
        | { status = Exhaustive; _ } -> check n cons
        | { status = Partial; path; after_nest } ->
            { status = Partial; path = Nil :: path; after_nest })
    | Switch { wildcard = Some wildcard; _ } -> exhaustive (check n wildcard)
    | Switch { cases; wildcard = None; check_cases = None; _ } ->
        let r = check n cases.if_match in
        { r with status = Partial; path = Any :: r.path }
    | Switch { cases; wildcard = None; check_cases = Some s; _ } ->
        let rec aux s { data; if_match; next } =
          match check n if_match with
          | { status = Partial; path; after_nest } ->
              {
                status = Partial;
                path = Scalar (data, Same) :: path;
                after_nest;
              }
          | { status = Exhaustive; path; after_nest } -> (
              let s = Seq.filter (Fun.negate (scalar_equal data)) s in
              match next with
              | None -> (
                  match s () with
                  | Seq.Nil ->
                      {
                        status = Exhaustive;
                        path = Scalar (data, Same) :: path;
                        after_nest;
                      }
                  | Seq.Cons (data, _) ->
                      {
                        status = Partial;
                        path = Scalar (data, Diverge) :: path;
                        after_nest;
                      })
              | Some case -> aux s case)
        in
        aux s cases
    | Optional { next = Some x; _ } | Optional { child = x; next = None } ->
        (* Try to follow the [next] path first to identify empty dicts. *)
        check n x

  let get_path t =
    match check Z t with
    | { status = Exhaustive; _ } -> None
    | { status = Partial; path; _ } -> Some path
end

(** This enforces the invariant that a nest may only have a wildcard if the nest
    itself is not exhaustive. Failing to enforce this will create redundant
    wildcard branches (not optimal, but benign), and it will fail to detect
    certain wildcard patterns as unused (more problematic). *)
let safe_nest n ~key ~ids ~child ~wildcard =
  match wildcard with
  | None -> Nest { key; ids; child; wildcard }
  | Some _ -> (
      let status =
        match child with
        (* A dict (i.e. a nest with optional keys) is never exhaustive. *)
        | Int_keys (Optional _) | String_keys (Optional _) -> ParMatch.Partial
        | Int_keys t -> (ParMatch.check (S n) t).status
        | String_keys t -> (ParMatch.check (S n) t).status
      in
      match status with
      | Exhaustive -> Nest { key; ids; child; wildcard = None }
      | Partial -> Nest { key; ids; child; wildcard })

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
    scalar ->
    (a, k) tree ->
    (a, k) switchcase =
 fun n original data if_match ->
  let cmp = scalar_compare data original.data in
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
      safe_nest na ~key:a.key ~ids:a.ids
        ~child:
          (match a.child with
          | Int_keys c ->
              Int_keys (expand_wildcard_after_nest (S na) (S nb) c ~wildcard)
          | String_keys c ->
              String_keys (expand_wildcard_after_nest (S na) (S nb) c ~wildcard))
        ~wildcard:
          (match a.wildcard with
          | None -> None
          | Some c -> Some (expand_wildcard_after_nest na nb c ~wildcard))
  | Nil a ->
      Nil { a with child = expand_wildcard_after_nest na nb a.child ~wildcard }
  | Cons a ->
      Cons { a with child = expand_wildcard_after_nest na nb a.child ~wildcard }
  | Nil_or_cons a ->
      let nil = expand_wildcard_after_nest na nb a.nil ~wildcard in
      let cons = expand_wildcard_after_nest na nb a.cons ~wildcard in
      Nil_or_cons { a with nil; cons }
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
          child = expand_wildcard_after_nest na nb a.child ~wildcard;
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
    (b, (a, ka) tree) depth ->
    (b, leaf) depth ->
    wildcard:(a, ka) tree ->
    (b, kb) tree ->
    (b, kb) tree option =
 fun na nb ~wildcard b ->
  match b with
  | End b -> (
      match (na, nb) with
      | Z, S n ->
          let a = merge n wildcard b in
          if equal_tree_nested n a wildcard then None else Some (End a)
      | S na, S nb -> (
          match merge_wildcard_after_nest na nb ~wildcard b with
          | None -> None
          | Some x -> Some (End x)))
  | Nest b -> (
      let w =
        match b.wildcard with
        | None -> None
        | Some b -> merge_wildcard_after_nest na nb ~wildcard b
      in
      match b.child with
      | Int_keys c -> (
          match merge_wildcard_after_nest (S na) (S nb) ~wildcard c with
          | None -> None
          | Some c ->
              Some
                (safe_nest nb ~key:b.key ~ids:b.ids ~child:(Int_keys c)
                   ~wildcard:w))
      | String_keys c -> (
          match merge_wildcard_after_nest (S na) (S nb) ~wildcard c with
          | None -> None
          | Some c ->
              Some
                (safe_nest nb ~key:b.key ~ids:b.ids ~child:(String_keys c)
                   ~wildcard:w)))
  | Nil b -> (
      match merge_wildcard_after_nest na nb ~wildcard b.child with
      | None -> None
      | Some child -> Some (Nil { b with child }))
  | Cons b -> (
      match merge_wildcard_after_nest na nb ~wildcard b.child with
      | None -> None
      | Some child -> Some (Cons { b with child }))
  | Nil_or_cons { ids; key; nil; cons } -> (
      let nil = merge_wildcard_after_nest na nb ~wildcard nil in
      let cons = merge_wildcard_after_nest na nb ~wildcard cons in
      match (nil, cons) with
      | None, None -> None
      | Some nil, Some cons -> Some (Nil_or_cons { ids; key; nil; cons })
      | Some child, None -> Some (Nil { ids; key; child })
      | None, Some child -> Some (Cons { ids; key; child }))
  | Wildcard b -> (
      match merge_wildcard_after_nest na nb ~wildcard b.child with
      | None -> None
      | Some child -> Some (Wildcard { b with child }))
  | Switch b -> (
      let w =
        match b.wildcard with
        | None -> None
        | Some b -> merge_wildcard_after_nest na nb ~wildcard b
      in
      let[@tail_mod_cons] rec aux case =
        let if_match =
          merge_wildcard_after_nest na nb ~wildcard case.if_match
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
      match merge_wildcard_after_nest na nb ~wildcard b.child with
      | None -> None
      | Some child ->
          let next =
            match b.next with
            | None -> None
            | Some b -> merge_wildcard_after_nest na nb ~wildcard b
          in
          Some (Optional { child; next }))

(** Merge two trees. If the merge is unsuccessful, then the result will be
    structurally equal to the first tree. *)
and merge :
    type a k. (a, leaf) depth -> (a, k) tree -> (a, k) tree -> (a, k) tree =
 fun n a b ->
  match (a, b) with
  | End a, End b -> ( match n with Z -> End a | S n -> End (merge n a b))
  | Wildcard a, Wildcard b ->
      let ids = SetInt.union a.ids b.ids in
      Wildcard { a with ids; child = merge n a.child b.child }
  | Wildcard a, Nest b -> (
      let ids = SetInt.union a.ids b.ids in
      let wildcard =
        match b.wildcard with
        | None -> Some a.child
        | Some b -> Some (merge n a.child b)
      in
      match b.child with
      | Int_keys c -> (
          match merge_wildcard_after_nest Z (S n) ~wildcard:a.child c with
          | None -> Wildcard a
          | Some c -> safe_nest n ~key:b.key ~ids ~child:(Int_keys c) ~wildcard)
      | String_keys c -> (
          match merge_wildcard_after_nest Z (S n) ~wildcard:a.child c with
          | None -> Wildcard a
          | Some c ->
              safe_nest n ~key:b.key ~ids ~child:(String_keys c) ~wildcard))
  | (Wildcard { ids; key; child } as a), Nil b ->
      let ids = SetInt.union ids b.ids in
      Nil_or_cons { ids; key; nil = merge n child b.child; cons = a }
  | (Wildcard { ids; key; child } as a), Cons b ->
      let ids = SetInt.union ids b.ids in
      Nil_or_cons { ids; key; nil = child; cons = merge n a b.child }
  | (Wildcard { ids; child; _ } as a), Nil_or_cons b ->
      let ids = SetInt.union ids b.ids in
      (* At least one merge must succeed. *)
      let nil = merge n child b.nil in
      let cons = merge n a b.cons in
      if equal_tree_nested n child nil && equal_tree_nested n a cons then a
      else Nil_or_cons { b with ids; nil; cons }
  | Wildcard a, Switch b -> (
      match merge_testcases_into_wildcard n a.child b.cases with
      | None -> Wildcard a
      | Some cases ->
          let wildcard =
            match b.wildcard with
            | None -> a.child
            | Some b -> merge n a.child b
          in
          let ids = SetInt.union a.ids b.ids in
          Switch { b with ids; cases; wildcard = Some wildcard })
  | Nest a, Nest b ->
      let ids = SetInt.union a.ids b.ids in
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
            match merge_wildcard_after_nest Z (S n) ~wildcard child with
            | None -> child
            | Some child -> child)
      in
      let child =
        match (a.child, b.child) with
        | Int_keys a, Int_keys b -> Int_keys (merge_child a b)
        | String_keys a, String_keys b -> String_keys (merge_child a b)
        | _ ->
            Error.internal ~__POS__
              "Type error between int and string keys. This means the \
               typechecker failed."
      in
      safe_nest n ~key:a.key ~ids ~child ~wildcard
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
      let ids = SetInt.union a.ids b.ids in
      safe_nest n ~key:a.key ~ids ~child ~wildcard:(Some wildcard)
  | Nil a, (Wildcard { ids; key; child } as b) ->
      let ids = SetInt.union a.ids ids in
      Nil_or_cons { ids; key; nil = merge n a.child child; cons = b }
  | Cons a, (Wildcard { ids; key; child } as b) ->
      let ids = SetInt.union a.ids ids in
      Nil_or_cons { ids; key; nil = child; cons = merge n a.child b }
  | Nil_or_cons a, (Wildcard { ids; child; _ } as b) ->
      let ids = SetInt.union a.ids ids in
      Nil_or_cons
        { a with ids; nil = merge n a.nil child; cons = merge n a.cons b }
  | Nil a, Nil b ->
      let ids = SetInt.union a.ids b.ids in
      Nil { a with ids; child = merge n a.child b.child }
  | Cons a, Cons b ->
      let ids = SetInt.union a.ids b.ids in
      Cons { a with ids; child = merge n a.child b.child }
  | Nil nil, Cons cons | Cons cons, Nil nil ->
      let ids = SetInt.union nil.ids cons.ids in
      Nil_or_cons { ids; key = nil.key; nil = nil.child; cons = cons.child }
  | Nil a, Nil_or_cons b ->
      let ids = SetInt.union a.ids b.ids in
      Nil_or_cons { b with ids; nil = merge n a.child b.nil }
  | Cons a, Nil_or_cons b ->
      let ids = SetInt.union a.ids b.ids in
      Nil_or_cons { b with ids; cons = merge n a.child b.cons }
  | Nil_or_cons a, Nil b ->
      let ids = SetInt.union a.ids b.ids in
      Nil_or_cons { a with ids; nil = merge n a.nil b.child }
  | Nil_or_cons a, Cons b ->
      let ids = SetInt.union a.ids b.ids in
      Nil_or_cons { a with ids; cons = merge n a.cons b.child }
  | Nil_or_cons a, Nil_or_cons b ->
      let ids = SetInt.union a.ids b.ids in
      Nil_or_cons
        { a with ids; nil = merge n a.nil b.nil; cons = merge n a.cons b.cons }
  | Switch a, Wildcard b ->
      Switch
        {
          a with
          ids = SetInt.union a.ids b.ids;
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
      let ids = SetInt.union a.ids b.ids in
      Switch { a with ids; cases; wildcard }
  | Optional { child; next }, Optional b ->
      Optional
        {
          child = merge n child b.child;
          next =
            (match (next, b.next) with
            | None, None -> None
            | Some a, None | None, Some a -> Some a
            | Some a, Some b -> Some (merge n a b));
        }
  | ( ( Wildcard _ | Optional _ | Switch _ | Nil _ | Cons _ | Nil_or_cons _
      | Nest _ | End _ ),
      _ ) ->
      Error.internal ~__POS__
        "Tried to merge incompatible trees. Either the typechecker failed or \
         the function that constructs trees failed."

let merge = merge Z

(** To turn a typed pattern into a tree (albeit a single-branched tree), we use
    CPS as an easy way deal with the nested tree type. *)

type bindings = { next_id : unit -> int; names : int MapString.t }

let ids = SetInt.empty

let of_scalar key data if_match check_cases =
  let cases = { data; if_match; next = None } in
  Switch { key; ids; cases; wildcard = None; check_cases }

let rec of_tpat :
      'a 'k.
      key:'k ->
      bindings ->
      (bindings -> ('a, 'k) tree) ->
      T.destruct T.pat ->
      ('a, 'k) tree =
 fun ~key b k -> function
  | Any -> Wildcard { ids; key; child = k b }
  | Var x ->
      let id = b.next_id () in
      let b = { b with names = MapString.add x id b.names } in
      Wildcard { ids = SetInt.singleton id; key; child = k b }
  | Scalar (data, Scalar_sum_none)
  | Scalar (data, Scalar_sum_int { row = `Open; _ })
  | Scalar (data, Scalar_sum_string { row = `Open; _ }) ->
      of_scalar key data (k b) None
  | Scalar (data, Scalar_sum_int { row = `Closed; cases }) ->
      of_scalar key data (k b) (Some (SetInt.to_seq cases |> Seq.map int))
  | Scalar (data, Scalar_sum_string { row = `Closed; cases }) ->
      of_scalar key data (k b) (Some (SetString.to_seq cases |> Seq.map string))
  | Nil -> Nil { key; ids; child = k b }
  | Cons cons -> Cons { key; ids; child = of_tpat ~key b k cons }
  | Tuple l ->
      let child = Int_keys (of_list ~key:0 b (fun b -> End (k b)) l) in
      Nest { key; ids; child; wildcard = None }
  | Record (tag, m, tys) ->
      (* We need to expand the map to include all of its type's keys. *)
      let child =
        MapString.merge
          (fun _k _ty p -> match p with None -> Some T.Any | Some _ as p -> p)
          !tys m
        |> MapString.to_seq
        |> of_keyvalues b (fun b -> End (k b))
      in
      let child =
        match tag with
        | Union_tag_none -> child
        | Union_tag_int (k, v, { row = `Open; _ }) ->
            of_scalar k (`Int v) child None
        | Union_tag_string (k, v, { row = `Open; _ }) ->
            of_scalar k (`String v) child None
        | Union_tag_int (k, v, { row = `Closed; cases }) ->
            of_scalar k (`Int v) child
              (Some (MapInt.to_seq cases |> Seq.map fst |> Seq.map int))
        | Union_tag_string (k, v, { row = `Closed; cases }) ->
            of_scalar k (`String v) child
              (Some (MapString.to_seq cases |> Seq.map fst |> Seq.map string))
      in
      Nest { key; ids; child = String_keys child; wildcard = None }
  | Dict (m, kys) ->
      (* We need to expand the map to include all of its type's keys. *)
      let child =
        MapString.map Option.some m
        |> SetString.fold
             (fun key map ->
               MapString.update key
                 (function None -> Some None | Some p -> Some p)
                 map)
             !kys
        |> MapString.to_seq
        |> of_keyvalues_dict b (fun b -> End (k b))
      in
      Nest { key; ids; child = String_keys child; wildcard = None }

and of_list :
      'a.
      key:int ->
      bindings ->
      (bindings -> ('a, int) tree) ->
      T.destruct T.pat list ->
      ('a, int) tree =
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
  | Cons ((key, None), s) ->
      let next = of_keyvalues_dict b k s in
      Optional { child = Wildcard { ids; key; child = next }; next = Some next }
  | Cons ((key, Some v), s) ->
      let child = of_tpat ~key b (fun b -> of_keyvalues_dict b k s) v in
      Optional { child; next = None }

let of_nonempty ~exit next_id Nonempty.(hd :: tl) =
  let k { names; _ } = End { names; exit } in
  let k b = of_list ~key:1 b k tl in
  of_tpat ~key:0 { next_id; names = MapString.empty } k hd

let rec make_case ~exit next_id tree = function
  | [] -> tree
  | (loc, ps) :: l ->
      let b = of_nonempty ~exit next_id ps in
      let tree' = merge tree b in
      if equal_tree equal_leaf tree tree' then Error.unused_case loc
      else make_case ~exit next_id tree' l

let make loc tys Nonempty.(Typechecker.{ pats; nodes; bindings } :: tl_cases) =
  let exits = Exits.make (succ (List.length tl_cases)) in
  (* IDs must be unique across all branches of the tree. *)
  let next_id = ref 0 in
  let next_id () =
    let id = !next_id in
    incr next_id; id
  in
  let hd_tree =
    let ((_, hd_pats) :: tl_pats) = pats in
    Exits.set exits 0 bindings nodes;
    let hd_tree = of_nonempty ~exit:0 next_id hd_pats in
    make_case ~exit:0 next_id hd_tree tl_pats
  in
  let rec aux tree exit = function
    | [] -> (
        match ParMatch.get_path tree with
        | None -> { tree; exits }
        | Some path -> Error.parmatch loc (ParMatch.pp tys) path)
    | Typechecker.{ pats = (loc, hd_pats) :: tl_pats; nodes; bindings } :: l ->
        Exits.set exits exit bindings nodes;
        let hd_tree = of_nonempty ~exit next_id hd_pats in
        let tree' = merge tree hd_tree in
        if equal_tree equal_leaf tree tree' then Error.unused_case loc
        else
          let tree = make_case ~exit next_id tree' tl_pats in
          aux tree (succ exit) l
  in
  aux hd_tree 1 tl_cases

let set_to_sexp s = Sexp.of_seq Sexp.int (SetInt.to_seq s)

let scalar_to_sexp = function
  | `Int i -> Sexp.int i
  | `String s -> Sexp.string s
  | `Float f -> Sexp.float f

let rec tree_to_sexp :
      'leaf 'key.
      ('leaf -> Sexp.t) -> ('key -> Sexp.t) -> ('leaf, 'key) tree -> Sexp.t =
 fun leaf_f key_f -> function
  | Switch { key; ids; cases; wildcard; check_cases } ->
      Sexp.make "switch"
        [
          Sexp.make "key" [ key_f key ];
          Sexp.make "ids" [ set_to_sexp ids ];
          Sexp.make "cases" [ switchcase_to_sexp leaf_f key_f cases ];
          Sexp.make "wildcard"
            [ Sexp.option (tree_to_sexp leaf_f key_f) wildcard ];
          Sexp.make "check_cases"
            [ Sexp.option (Sexp.of_seq scalar_to_sexp) check_cases ];
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
  | Nil { key; ids; child } ->
      Sexp.make "nil"
        [
          Sexp.make "key" [ key_f key ];
          Sexp.make "ids" [ set_to_sexp ids ];
          Sexp.make "child" [ tree_to_sexp leaf_f key_f child ];
        ]
  | Cons { key; ids; child } ->
      Sexp.make "cons"
        [
          Sexp.make "key" [ key_f key ];
          Sexp.make "ids" [ set_to_sexp ids ];
          Sexp.make "child" [ tree_to_sexp leaf_f key_f child ];
        ]
  | Nil_or_cons { key; ids; nil; cons } ->
      Sexp.make "nil_or_cons"
        [
          Sexp.make "key" [ key_f key ];
          Sexp.make "ids" [ set_to_sexp ids ];
          Sexp.make "nil" [ tree_to_sexp leaf_f key_f nil ];
          Sexp.make "cons" [ tree_to_sexp leaf_f key_f cons ];
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
          Sexp.make "child" [ tree_to_sexp leaf_f key_f child ];
          Sexp.make "next" [ Sexp.option (tree_to_sexp leaf_f key_f) next ];
        ]
  | End leaf -> Sexp.make "end" [ leaf_f leaf ]

and nest_to_sexp leaf_f key_f = function
  | Int_keys tree ->
      Sexp.make "int_keys"
        [ tree_to_sexp (tree_to_sexp leaf_f key_f) Sexp.int tree ]
  | String_keys tree ->
      Sexp.make "string_keys"
        [ tree_to_sexp (tree_to_sexp leaf_f key_f) Sexp.string tree ]

and switchcase_to_sexp leaf_f key_f { data; if_match; next } =
  Sexp.make "case"
    [
      Sexp.make "data" [ scalar_to_sexp data ];
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
      Sexp.make "exits" [ Exits.to_sexp f exits ];
    ]
