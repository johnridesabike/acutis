/**
  Copyright (c) 2021 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Array = Belt.Array
module Const = Data.Const
module MapString = Belt.Map.String
module Queue = Belt.MutableQueue
module SetInt = Belt.Set.Int
module SetString = Belt.Set.String
module Tpat = Typechecker.Pattern

exception Exit = Debug.Exit

/*
  Every pattern represents a one-dimensional path across a multi-dimensional
  data structure. A list of patterns is a two-dimensional matrix of paths. In
  order to transverse these paths efficiently, we need to combine them into
  a tree.

  We take advantage of a few properties that can make our tree simpler. We
  only test discrete static values, integers, strings, etc. We sort record
  fields and replace ommited fields with wildcards, so every pattern "lines up"
  with the others. We also sort the tested values (the integers, strings, etc.).

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

  The tree type is a polymorphic "nested" type. Each tree can use itself as its
  own type variable, i.e. tree<tree<'a>>. This allows the end nodes to be
  fully polymorphic. They can either lead to a leaf or back to their containing
  tree. This nesting corresponds to the nesting of patterns.

  Nested types are simple to create, but complicated to manipulate. Functions
  cannot consume these types under normal polymorphism rules. We need to
  use explicitly polymorphic type annotations and GADTs.
*/
type extra_nest_info = Tuple | Dict | Record

type enum =
  | Enum_Bool
  | Enum_String(Belt.Set.String.t)
  | Enum_Int(Belt.Set.Int.t)

type extra_switch_info =
  | Extra_String
  | Extra_Int
  | Extra_Float
  | Extra_Enum(enum)

type rec tree<'leaf, 'key> =
  /*
    A Switch represents a list of discreet values to test (i.e., 1, "a", etc.).
    If none of the values match the input, then the wildcard is used.
 */
  | Switch({
      key: 'key,
      ids: SetInt.t,
      cases: switchcase<'leaf, 'key>,
      extra: extra_switch_info,
      wildcard: option<tree<'leaf, 'key>>,
    })
  /*
    A Nest represents a structure such as tuple or a record.
 */
  | Nest({
      key: 'key,
      ids: SetInt.t,
      child: nest<'leaf, 'key>,
      wildcard: option<tree<'leaf, 'key>>,
      extra: extra_nest_info,
    })
  /*
    A Construct represents one of the built-in variant types: lists and
    nullables.
    nil represents an empty list or a null value. It is like a wildcard in that
    it always points to the *next* node, whatever that may be.
    cons always either points to a wildcard node or a Nest + Tuple node.
 */
  | Construct({
      key: 'key,
      ids: SetInt.t,
      nil: option<tree<'leaf, 'key>>,
      cons: option<tree<'leaf, 'key>>,
      extra: Tpat.construct,
    })
  /*
    Wildcards simply point to the next node in the tree.
 */
  | Wildcard({key: 'key, ids: SetInt.t, child: tree<'leaf, 'key>})
  | End('leaf)

and nest<'leaf, 'key> =
  | IntKeys(tree<tree<'leaf, 'key>, int>)
  | StringKeys(tree<tree<'leaf, 'key>, string>)

/*
  The switch cases work like linked lists of values. If an input matches a
  value, then we follow its associated tree. If not, we try the next case.
*/
and switchcase<'leaf, 'key> = {
  val: Const.t,
  ifMatch: tree<'leaf, 'key>,
  nextCase: option<switchcase<'leaf, 'key>>,
}

/*
  Each "exit" is given an integer ID which we can use to look up the AST nodes
  to follow after the tree. We do this because exits can be duplicated when
  trees expand, and we don't want to duplicate entire ASTs.
*/

module Exit = {
  type key = int
  type t<'a> = array<'a>
  /* Redefining these functions prevents runtime currying. */
  let get = (a, i) => Array.getExn(a, i)
  let map = (a, ~f) => Array.mapU(a, f)
  let unsafe_key = i => i
}

type leaf = {names: MapString.t<int>, exit: Exit.key}

type t<'a> = {
  tree: tree<leaf, int>,
  exits: Exit.t<'a>,
}

/*
  One challenge when merging the trees is that we need to keep track of "where
  we are" in the tree on the type level. This natural-number GADT does that for
  us.
*/

type rec nat<_, _> =
  | Z: nat<'z, 'z>
  | S(nat<'a, 'z>): nat<tree<'a, 'key>, 'z>

/*
  Whenever we iterate through cases, we do it tail-recursively which creates
  a reversed accumulator. We need to reverse it again back to normal.
*/
let rec reverseCases = (~tail=?, t) => {
  let tail = {...t, nextCase: tail}
  switch t.nextCase {
  | None => tail
  | Some(t) => reverseCases(t, ~tail)
  }
}

/*
  If a tree cannot merge with another tree, then we throw that tree away.
  If an entire tree failed to merge, then we know its pattern was redundant.
*/
exception MergeFail

/* We sort the data for easier analysis.
  If the new value is lower than the original, then append it to the beginning.
  If the new value equals the original, then merge them.
  If the new value is greater than the original, then check if we're at the end
  of the list. If we are, then add this new case. If not, then keep searching.
*/
let rec mergeTestCasesAux:
  type a k. (
    ~init: switchcase<a, k>=?,
    switchcase<a, k>,
    Const.t,
    tree<a, k>,
    nat<a, leaf>,
  ) => option<switchcase<a, k>> =
  (~init=?, original, val, ifMatch, n) => {
    let cmp = Const.compare(val, original.val)
    if cmp < 0 {
      let tail = {val: val, ifMatch: ifMatch, nextCase: Some(original)}
      switch init {
      | None => Some(tail)
      | Some(init) => Some(reverseCases(init, ~tail))
      }
    } else if cmp == 0 {
      try {
        let tail = {...original, ifMatch: merge(original.ifMatch, ifMatch, n)}
        switch init {
        | None => Some(tail)
        | Some(init) => Some(reverseCases(init, ~tail))
        }
      } catch {
      | MergeFail => None
      }
    } else {
      let init = {...original, nextCase: init}
      switch original.nextCase {
      | None => Some(reverseCases(init, ~tail={val: val, ifMatch: ifMatch, nextCase: None}))
      | Some(original) => mergeTestCasesAux(original, val, ifMatch, ~init, n)
      }
    }
  }

/*
  When merging a and b, we take each value from b and try to merge it with the
  items in a. At least one value must merge in order to be considered a success.
*/
and mergeTestCases:
  type a k. (switchcase<a, k>, switchcase<a, k>, nat<a, leaf>, bool) => option<switchcase<a, k>> =
  (original, {val, ifMatch, nextCase}, n, success) =>
    switch mergeTestCasesAux(original, val, ifMatch, n) {
    | Some(result) =>
      switch nextCase {
      | None => Some(result)
      | Some(b) => mergeTestCases(result, b, n, true)
      }
    | None =>
      switch (nextCase, success) {
      | (None, false) => None
      | (None, true) => Some(original)
      | (Some(b), success) => mergeTestCases(original, b, n, success)
      }
    }

/*
  When we merge a list of values with a wildcard, some of them may not merge
  successfully. As long as at least one merges, then this function succeeds.
  We filter out any unsuccessful mergers, since their paths are covered by the
  wildcard case.
*/
and mergeTestCasesIntoWildcard:
  type a k. (
    ~init: switchcase<a, k>=?,
    tree<a, k>,
    switchcase<a, k>,
    nat<a, leaf>,
  ) => option<switchcase<a, k>> =
  (~init=?, wildcard, t, n) => {
    let init = try {
      Some({...t, ifMatch: merge(wildcard, t.ifMatch, n), nextCase: init})
    } catch {
    | MergeFail => init
    }
    switch (t.nextCase, init) {
    | (None, None) => None
    | (None, Some(init)) => Some(reverseCases(init))
    | (Some(t), init) => mergeTestCasesIntoWildcard(~init?, wildcard, t, n)
    }
  }

/*
  When we expand a wildcard into a list of test values, we merge it with each
  child and ignore any errors. Merge failures are replaced by their unmerged
  originals.
*/
and expandWildcardIntoTestCases:
  type a k. (
    ~init: switchcase<a, k>=?,
    switchcase<a, k>,
    tree<a, k>,
    nat<a, leaf>,
  ) => switchcase<a, k> =
  (~init=?, {val, ifMatch, nextCase}, wildcard, n) => {
    let init = try {
      {val: val, ifMatch: merge(ifMatch, wildcard, n), nextCase: init}
    } catch {
    | MergeFail => {val: val, ifMatch: ifMatch, nextCase: init}
    }
    switch nextCase {
    | None => reverseCases(init)
    | Some(a) => expandWildcardIntoTestCases(~init, a, wildcard, n)
    }
  }

/*
  When we expand a wildcard into a nest, we need to expand all of the wildcard's
  child nodes after the nest's child nodes. This maps through the nest to do
  so.
  We need a second nat value to track "where we are" relative to the beginning
  of the nest. Z (zero) represents the depth where the nest began.
  It doesn't matter if the expansion succeeds or not. If it fails, we just keep
  the original tree.
*/
and expandWildcardAfterNest:
  type a b ka kb. (
    tree<a, ka>,
    nat<a, leaf>,
    ~wildcard: tree<b, kb>,
    nat<a, tree<b, kb>>,
  ) => tree<a, ka> =
  (a, na, ~wildcard, nb) =>
    switch (a, na, nb) {
    | (End(_), Z, _) => assert false
    | (End(a), S(n), Z) =>
      try {End(merge(a, wildcard, n))} catch {
      | MergeFail => End(a)
      }
    | (End(a), S(na), S(nb)) => End(expandWildcardAfterNest(a, na, ~wildcard, nb))
    | (Nest(a), na, nb) =>
      let child = switch a.child {
      | IntKeys(child) => IntKeys(expandWildcardAfterNest(child, S(na), ~wildcard, S(nb)))
      | StringKeys(child) => StringKeys(expandWildcardAfterNest(child, S(na), ~wildcard, S(nb)))
      }
      Nest({...a, child: child})
    | (Construct(a), na, nb) =>
      Construct({
        ...a,
        nil: switch a.nil {
        | None => None
        | Some(child) => Some(expandWildcardAfterNest(child, na, ~wildcard, nb))
        },
        cons: switch a.cons {
        | None => None
        | Some(child) => Some(expandWildcardAfterNest(child, na, ~wildcard, nb))
        },
      })
    | (Wildcard(a), na, nb) =>
      Wildcard({...a, child: expandWildcardAfterNest(a.child, na, ~wildcard, nb)})
    | (Switch(a), na, nb) =>
      let wildcard' = switch a.wildcard {
      | None => None
      | Some(a) => Some(expandWildcardAfterNest(a, na, ~wildcard, nb))
      }
      let rec aux = (init, case) => {
        let ifMatch = expandWildcardAfterNest(case.ifMatch, na, ~wildcard, nb)
        let init = {...case, ifMatch: ifMatch, nextCase: init}
        switch case.nextCase {
        | None => Switch({...a, cases: reverseCases(init), wildcard: wildcard'})
        | Some(a) => aux(Some(init), a)
        }
      }
      aux(None, a.cases)
    }

/*
  When we merge a wildcard after a nest, we can only keep successful mergers.
  For constructs and switch nodes, then at least one path must merge
  succesfully. Unsuccesful paths are filtered out.
*/
@raises(MergeFail)
and mergeWildcardAfterNest:
  type a b ka kb. (
    ~wildcard: tree<a, ka>,
    nat<b, tree<a, ka>>,
    tree<b, kb>,
    nat<b, leaf>,
  ) => tree<b, kb> =
  (~wildcard, na, b, nb) =>
    switch (b, na, nb) {
    | (End(b), Z, S(n)) => End(merge(wildcard, b, n))
    | (End(b), S(na), S(nb)) => End(mergeWildcardAfterNest(~wildcard, na, b, nb))
    | (Nest(b), na, nb) =>
      let child = switch b.child {
      | IntKeys(child) => IntKeys(mergeWildcardAfterNest(~wildcard, S(na), child, S(nb)))
      | StringKeys(child) => StringKeys(mergeWildcardAfterNest(~wildcard, S(na), child, S(nb)))
      }
      Nest({...b, child: child})
    | (Construct(b), na, nb) =>
      let nil = switch b.nil {
      | None => Ok(None)
      | Some(child) =>
        try {
          Ok(Some(mergeWildcardAfterNest(~wildcard, na, child, nb)))
        } catch {
        | MergeFail => Error(None)
        }
      }
      let cons = switch b.cons {
      | None => Ok(None)
      | Some(child) =>
        try {
          Ok(Some(mergeWildcardAfterNest(~wildcard, na, child, nb)))
        } catch {
        | MergeFail => Error(None)
        }
      }
      switch (nil, cons) {
      | (Error(_), Error(_)) | (Ok(None), Error(None)) | (Error(None), Ok(None)) => raise(MergeFail)
      | (Ok(nil), Ok(cons)) | (Ok(nil), Error(cons)) | (Error(nil), Ok(cons)) =>
        Construct({...b, nil: nil, cons: cons})
      }
    | (Wildcard(b), na, nb) =>
      Wildcard({...b, child: mergeWildcardAfterNest(~wildcard, na, b.child, nb)})
    | (Switch(b), na, nb) =>
      let wildcard' = switch b.wildcard {
      | None => None
      | Some(b) =>
        try {
          Some(mergeWildcardAfterNest(~wildcard, na, b, nb))
        } catch {
        | MergeFail => None
        }
      }

      @raises(MergeFail)
      let rec aux = (init, case) => {
        let init = try {
          let ifMatch = mergeWildcardAfterNest(~wildcard, na, case.ifMatch, nb)
          Some({...case, ifMatch: ifMatch, nextCase: init})
        } catch {
        | MergeFail => init
        }
        switch (case.nextCase, init) {
        | (None, None) => raise(MergeFail)
        | (None, Some(init)) => Switch({...b, cases: reverseCases(init), wildcard: wildcard'})
        | (Some(a), init) => aux(init, a)
        }
      }
      aux(None, b.cases)
    }

@raises(MergeFail)
and merge:
  type a k. (tree<a, k>, tree<a, k>, nat<a, leaf>) => tree<a, k> =
  (a, b, n) =>
    switch (a, b) {
    | (End(a), End(b)) =>
      switch n {
      | Z => raise(MergeFail)
      | S(n) => End(merge(a, b, n))
      }
    | (Wildcard(a), Wildcard(b)) =>
      Wildcard({...a, ids: SetInt.union(a.ids, b.ids), child: merge(a.child, b.child, n)})
    | (Wildcard(a), Nest(b)) =>
      let wildcard = switch b.wildcard {
      | None => a.child
      | Some(b) => merge(a.child, b, n)
      }
      let child = switch b.child {
      | IntKeys(child) => IntKeys(mergeWildcardAfterNest(~wildcard=a.child, Z, child, S(n)))
      | StringKeys(child) => StringKeys(mergeWildcardAfterNest(~wildcard=a.child, Z, child, S(n)))
      }
      let ids = SetInt.union(a.ids, b.ids)
      Nest({...b, ids: ids, child: child, wildcard: Some(wildcard)})
    | (Wildcard(a) as a', Construct(b)) =>
      let nil = switch b.nil {
      | None => Ok(Some(a.child))
      | Some(b) =>
        try {Ok(Some(merge(a.child, b, n)))} catch {
        | MergeFail => Error(None)
        }
      }
      let cons = switch b.cons {
      | None => Ok(Some(a'))
      | Some(b) =>
        try {Ok(Some(merge(a', b, n)))} catch {
        | MergeFail => Error(None)
        }
      }
      let ids = SetInt.union(a.ids, b.ids)
      switch (nil, cons) {
      | (Error(_), Error(_)) | (Ok(None), Error(None)) | (Error(None), Ok(None)) => raise(MergeFail)
      | (Ok(nil), Ok(cons)) | (Error(nil), Ok(cons)) | (Ok(nil), Error(cons)) =>
        Construct({...b, ids: ids, nil: nil, cons: cons})
      }
    | (Wildcard(a), Switch(b)) =>
      let cases = switch mergeTestCasesIntoWildcard(a.child, b.cases, n) {
      | None => raise(MergeFail)
      | Some(cases) => cases
      }
      let wildcard = switch b.wildcard {
      | None => a.child
      | Some(b) => merge(a.child, b, n)
      }
      let ids = SetInt.union(a.ids, b.ids)
      Switch({...b, ids: ids, cases: cases, wildcard: Some(wildcard)})
    | (Nest(a), Nest(b)) =>
      let wildcard = switch (a.wildcard, b.wildcard) {
      | (None, None) => None
      | (Some(x), None) | (None, Some(x)) => Some(x)
      | (Some(a), Some(b)) => Some(merge(a, b, n))
      }
      let child = switch (a.child, b.child) {
      | (IntKeys(a), IntKeys(b)) =>
        let child = merge(a, b, S(n))
        switch wildcard {
        | None => IntKeys(child)
        | Some(wildcard) => IntKeys(mergeWildcardAfterNest(~wildcard, Z, child, S(n)))
        }
      | (StringKeys(a), StringKeys(b)) =>
        let child = merge(a, b, S(n))
        switch wildcard {
        | None => StringKeys(child)
        | Some(wildcard) => StringKeys(mergeWildcardAfterNest(~wildcard, Z, child, S(n)))
        }
      | _ => assert false
      }
      let ids = SetInt.union(a.ids, b.ids)
      Nest({...a, ids: ids, child: child, wildcard: wildcard})
    | (Nest(a), Wildcard(b)) =>
      let child = switch a.child {
      | StringKeys(child) => StringKeys(expandWildcardAfterNest(child, S(n), ~wildcard=b.child, Z))
      | IntKeys(child) => IntKeys(expandWildcardAfterNest(child, S(n), ~wildcard=b.child, Z))
      }
      let wildcard = switch a.wildcard {
      | None => b.child
      | Some(a) => merge(a, b.child, n)
      }
      let ids = SetInt.union(a.ids, b.ids)
      Nest({...a, ids: ids, child: child, wildcard: Some(wildcard)})
    | (Construct(a), Wildcard(b) as b') =>
      let nil = switch a.nil {
      | None => Ok(b.child)
      | Some(a) =>
        try {Ok(merge(a, b.child, n))} catch {
        | MergeFail => Error(a)
        }
      }
      let cons = switch a.cons {
      | None => Ok(b')
      | Some(a) =>
        try {Ok(merge(a, b', n))} catch {
        | MergeFail => Error(a)
        }
      }
      let ids = SetInt.union(a.ids, b.ids)
      switch (nil, cons) {
      | (Error(_), Error(_)) => raise(MergeFail)
      | (Ok(nil), Ok(cons)) | (Ok(nil), Error(cons)) | (Error(nil), Ok(cons)) =>
        Construct({...a, ids: ids, nil: Some(nil), cons: Some(cons)})
      }
    | (Construct(a), Construct(b)) =>
      let nil = switch (a.nil, b.nil) {
      | (None, None) => None
      | (Some(x), None) | (None, Some(x)) => Some(x)
      | (Some(a), Some(b)) => Some(merge(a, b, n))
      }
      let cons = switch (a.cons, b.cons) {
      | (None, None) => None
      | (Some(x), None) | (None, Some(x)) => Some(x)
      | (Some(a), Some(b)) => Some(merge(a, b, n))
      }
      let ids = SetInt.union(a.ids, b.ids)
      Construct({...a, ids: ids, nil: nil, cons: cons})
    | (Switch(a), Wildcard(b)) =>
      let wildcard = switch a.wildcard {
      | None => b.child
      | Some(a) => merge(a, b.child, n)
      }
      let cases = expandWildcardIntoTestCases(a.cases, b.child, n)
      let ids = SetInt.union(a.ids, b.ids)
      Switch({...a, ids: ids, cases: cases, wildcard: Some(wildcard)})
    | (Switch(a), Switch(b)) =>
      let wildcard = switch (a.wildcard, b.wildcard) {
      | (None, None) => None
      | (Some(x), None) | (None, Some(x)) => Some(x)
      | (Some(a), Some(b)) => Some(merge(a, b, n))
      }
      let bcases = switch a.wildcard {
      | Some(wildcard) =>
        switch mergeTestCasesIntoWildcard(wildcard, b.cases, n) {
        | Some(cases) => cases
        | None => raise(MergeFail)
        }
      | None => b.cases
      }
      let cases = switch mergeTestCases(a.cases, bcases, n, false) {
      | None => raise(MergeFail)
      | Some(cases) => cases
      }
      let cases = switch b.wildcard {
      | None => cases
      | Some(wildcard) => expandWildcardIntoTestCases(cases, wildcard, n)
      }
      let ids = SetInt.union(a.ids, b.ids)
      Switch({...a, ids: ids, cases: cases, wildcard: wildcard})
    | (Switch(_), Nest(_) | Construct(_))
    | (Construct(_), Switch(_) | Nest(_))
    | (Nest(_), Switch(_) | Construct(_))
    | (Switch(_) | Nest(_) | Construct(_) | Wildcard(_), End(_))
    | (End(_), Switch(_) | Nest(_) | Construct(_) | Wildcard(_)) =>
      assert false
    }

/*
  To turn a typed pattern into a tree (albiet a single-branch of tree), we use
  CPS as an easy way to keep type safety.
*/
type continue<'a, 'k> = (. MapString.t<int>) => tree<'a, 'k>

@raises(Exit)
let rec fromTPat: 'a 'k. (_, _, 'k, continue<'a, 'k>) => tree<'a, 'k> = (p, b, key, k) =>
  switch p {
  | Tpat.TAny(_) => Wildcard({ids: SetInt.empty, key: key, child: k(. b)})
  | TVar(debug, x) | TOptionalVar(debug, x) =>
    let id = Debug.char(debug)
    Wildcard({
      ids: SetInt.add(SetInt.empty, id),
      key: key,
      child: k(. MapString.set(b, x, id)),
    })
  | TConstruct(_, kind, Some(cons)) =>
    Construct({
      key: key,
      ids: SetInt.empty,
      extra: kind,
      nil: None,
      cons: Some(fromTPat(cons, b, key, k)),
    })
  | TConstruct(_, kind, None) =>
    Construct({key: key, ids: SetInt.empty, extra: kind, nil: Some(k(. b)), cons: None})
  | TConst(_, val, enum) =>
    Switch({
      key: key,
      ids: SetInt.empty,
      cases: {val: Const.fromTPat(val), ifMatch: k(. b), nextCase: None},
      extra: switch enum {
      | Some({cases: Enum_String(e), _}) => Extra_Enum(Enum_String(e))
      | Some({cases: Enum_Int(e), _}) => Extra_Enum(Enum_Int(e))
      | None =>
        switch val {
        | TString(_) => Extra_String
        | TInt(_) => Extra_Int
        | TBool(_) => Extra_Enum(Enum_Bool)
        | TFloat(_) => Extra_Float
        }
      },
      wildcard: None,
    })
  | TTuple(_, a) =>
    let child = fromArray(a, b, 0, (. b) => End(k(. b)))
    Nest({key: key, ids: SetInt.empty, child: IntKeys(child), wildcard: None, extra: Tuple})
  | TRecord(dbg, m, tys) =>
    /* We need to expand the map to include all of its type's keys. */
    let a = MapString.mergeU(m, tys.contents, (. _, p, ty) =>
      switch (p, ty) {
      | (None, Some(_)) => Some(Tpat.TAny(dbg))
      | (Some(p), _) => Some(p)
      | (None, None) => None
      }
    )->MapString.toArray
    let child = fromKeyValues(a, b, 0, (. b) => End(k(. b)))
    Nest({key: key, ids: SetInt.empty, child: StringKeys(child), wildcard: None, extra: Record})
  | TDict(dbg, m, kys) =>
    /* We need to expand the map to include all of its type's keys. */
    let kys =
      SetString.toArray(kys.contents)->Array.mapU((. k) => (k, Tpat.TAny(dbg)))->MapString.fromArray
    let a = MapString.mergeU(m, kys, (. _, p, k) =>
      switch (p, k) {
      | (Some(p), _) | (None, Some(p)) => Some(p)
      | (None, None) => None
      }
    )->MapString.toArray
    let child = fromKeyValues(a, b, 0, (. b) => End(k(. b)))
    Nest({key: key, ids: SetInt.empty, child: StringKeys(child), wildcard: None, extra: Dict})
  }

@raises(Exit)
and fromArray: 'a. (_, _, _, continue<'a, int>) => tree<'a, int> = (a, b, i, k) =>
  switch a[i] {
  | None => k(. b)
  | Some(p) => fromTPat(p, b, i, (. b) => fromArray(a, b, succ(i), k))
  }

@raises(Exit)
and fromKeyValues: 'a. (_, _, _, continue<'a, string>) => tree<'a, string> = (a, b, i, k) =>
  switch a[i] {
  | None => k(. b)
  | Some((key, v)) => fromTPat(v, b, key, (. b) => fromKeyValues(a, b, succ(i), k))
  }

@raises(Exit)
let fromArray = (~exit, a) =>
  fromArray(a, MapString.empty, 0, (. names) => End({names: names, exit: exit}))

@raises(Exit)
let makeCase = (hd, a, ~exit) => {
  @raises(Exit)
  let rec aux = (t, i) =>
    switch NonEmpty.get(a, i) {
    | None => t
    | Some(ps) =>
      let b = fromArray(NonEmpty.toArray(ps), ~exit)
      switch merge(t, b, Z) {
      | t => aux(t, succ(i))
      | exception MergeFail => raise(Exit(Debug.unusedCase(ps, module(Tpat))))
      }
    }
  aux(hd, 1)
}

@raises(Exit)
let make = (cases: NonEmpty.t<Typechecker.case>) => {
  let exitq = Queue.make()
  let hdcase = NonEmpty.hd(cases)
  Queue.add(exitq, hdcase.nodes)
  let exit = Queue.size(exitq) - 1
  let hdTree = NonEmpty.hd(hdcase.pats)->NonEmpty.toArray->fromArray(~exit)
  let tree = makeCase(hdTree, hdcase.pats, ~exit)

  @raises(Exit)
  let rec aux = (tree, i) =>
    switch NonEmpty.get(cases, i) {
    | None => {tree: tree, exits: Queue.toArray(exitq)}
    | Some({pats, nodes}) =>
      Queue.add(exitq, nodes)
      let exit = Queue.size(exitq) - 1
      let hdTree = NonEmpty.hd(pats)->NonEmpty.toArray->fromArray(~exit)
      switch merge(tree, hdTree, Z) {
      | tree =>
        let tree = makeCase(tree, pats, ~exit)
        aux(tree, succ(i))
      | exception MergeFail => raise(Exit(Debug.unusedCase(NonEmpty.hd(pats), module(Tpat))))
      }
    }
  aux(tree, 1)
}

module ParMatch = {
  module List = Belt.List

  module Refutation = {
    type t = {test: Const.t, enum: enum}

    let succ_string = cases =>
      switch SetString.minimum(cases) {
      | None => None
      | Some(s) =>
        let cases = SetString.remove(cases, s)
        Some({test: PString(s), enum: Enum_String(cases)})
      }

    let succ_int = cases =>
      switch SetInt.minimum(cases) {
      | None => None
      | Some(i) =>
        let cases = SetInt.remove(cases, i)
        Some({test: PInt(i), enum: Enum_Int(cases)})
      }

    let make = x =>
      switch x {
      | Extra_Int | Extra_String | Extra_Float => None
      | Extra_Enum(Enum_Bool) => Some({test: PBool(false), enum: Enum_Bool})
      | Extra_Enum(Enum_String(enum)) => succ_string(enum)
      | Extra_Enum(Enum_Int(enum)) => succ_int(enum)
      }

    let succ = ({test, enum}) =>
      switch enum {
      | Enum_Bool =>
        switch test {
        | PBool(false) => Some({test: PBool(true), enum: enum})
        | _ => None
        }
      | Enum_String(enum) => succ_string(enum)
      | Enum_Int(enum) => succ_int(enum)
      }
  }

  type flag = Partial | Exhaustive

  type t<'a> = {
    flag: flag,
    pats: list<(string, Tpat.t)>,
    next: 'a,
  }

  let toArray = l => List.toArray(l)->Array.mapU((. (_k, v)) => v)

  let toKeyValues = l => List.reduceU(l, MapString.empty, (. m, (k, v)) => MapString.set(m, k, v))

  let toString = l => toArray(l)->Array.joinWith(", ", Tpat.toString)

  let key_str = (. s) => s
  let key_int = (. _) => ""

  let exhaustive = (key, {pats, flag, next}) => {
    let pat = switch key {
    | "" => Tpat.TAny(Debug.empty)
    | k => TVar(Debug.empty, k)
    }
    {flag: flag, pats: list{(key, pat), ...pats}, next: next}
  }

  let rec check: 'a 'k. (tree<'a, 'k>, (. 'k) => string) => t<'a> = (tree, kf) =>
    switch tree {
    | End(next) => {flag: Exhaustive, pats: list{}, next: next}
    | Wildcard({key, child, _}) => exhaustive(kf(. key), check(child, kf))
    | Nest({key, child, wildcard, extra, _}) =>
      // Either the child _or_ the wildcard can be exhaustive.
      // A nest filled with exhaustive patterns e.g. tuple (_, _, _) can lead to
      // an exhaustive path even if it's paired with a wildcard _ that doesn't.
      // (In which case, the wildcard is redundant and will never be used.)
      let result = switch child {
      | IntKeys(child) => check(child, key_int)
      | StringKeys(child) => check(child, key_str)
      }
      switch result {
      | {flag: Exhaustive, next, _} =>
        switch extra {
        | Tuple | Record => exhaustive(kf(. key), check(next, kf))
        | Dict =>
          switch wildcard {
          | Some(wildcard) => exhaustive(kf(. key), check(wildcard, kf))
          | None =>
            let {pats, next, _} = check(next, kf)
            {flag: Partial, pats: list{(kf(. key), TAny(Debug.empty)), ...pats}, next: next}
          }
        }
      | {flag: Partial, pats, next} =>
        switch wildcard {
        | Some(wildcard) => exhaustive(kf(. key), check(wildcard, kf))
        | None =>
          let nest = switch extra {
          | Tuple => Tpat.TTuple(Debug.empty, toArray(pats))
          | Record => TRecord(Debug.empty, toKeyValues(pats), ref(MapString.empty))
          | Dict => TDict(Debug.empty, toKeyValues(pats), ref(Belt.Set.String.empty))
          }
          let {pats, next, _} = check(next, kf)
          {flag: Partial, pats: list{(kf(. key), nest), ...pats}, next: next}
        }
      }
    | Construct({key, extra, nil, cons, _}) =>
      switch (nil, cons) {
      | (None, Some(cons)) =>
        let {pats, next, _} = check(cons, kf)
        {
          flag: Partial,
          pats: switch pats {
          | list{_, ...pats} => list{(kf(. key), TConstruct(Debug.empty, extra, None)), ...pats}
          | _ => assert false
          },
          next: next,
        }
      | (Some(nil), None) =>
        let {pats, next, _} = check(nil, kf)
        {
          flag: Partial,
          pats: list{(kf(. key), TConstruct(Debug.empty, extra, Some(TAny(Debug.empty)))), ...pats},
          next: next,
        }
      | (Some(nil), Some(cons)) =>
        switch check(nil, kf) {
        | {flag: Exhaustive, _} =>
          switch check(cons, kf) {
          | {flag: Exhaustive, _} as x => x
          | {flag: Partial, pats, next} =>
            let pats = switch pats {
            | list{(key, cons), ...pats} => list{
                (key, Tpat.TConstruct(Debug.empty, extra, Some(cons))),
                ...pats,
              }
            | _ => assert false
            }
            {flag: Partial, pats: pats, next: next}
          }
        | {flag: Partial, pats, next} => {
            flag: Partial,
            pats: list{(kf(. key), Tpat.TConstruct(Debug.empty, extra, None)), ...pats},
            next: next,
          }
        }
      | (None, None) => assert false
      }
    | Switch({key, cases, wildcard, extra, _}) =>
      // The enum info gets lost here. FIX THIS.
      switch wildcard {
      | Some(wildcard) => exhaustive(kf(. key), check(wildcard, kf))
      | None =>
        let rec aux = (refute, {val, ifMatch, nextCase}) =>
          switch check(ifMatch, kf) {
          | {flag: Partial, pats, next} => {
              flag: Partial,
              pats: list{(kf(. key), TConst(Debug.empty, Const.toTPat(val), None)), ...pats},
              next: next,
            }
          | {flag: Exhaustive, pats, next} =>
            if Const.equal(refute.Refutation.test, val) {
              switch Refutation.succ(refute) {
              | None => exhaustive(kf(. key), check(ifMatch, kf))
              | Some(refute) =>
                switch nextCase {
                | None => {
                    flag: Partial,
                    pats: list{
                      (kf(. key), TConst(Debug.empty, Const.toTPat(refute.test), None)),
                      ...pats,
                    },
                    next: next,
                  }
                | Some(case) => aux(refute, case)
                }
              }
            } else {
              {
                flag: Partial,
                pats: list{
                  (kf(. key), TConst(Debug.empty, Const.toTPat(refute.test), None)),
                  ...pats,
                },
                next: next,
              }
            }
          }
        switch Refutation.make(extra) {
        | None =>
          let {pats, next, _} = check(cases.ifMatch, kf)
          {
            flag: Partial,
            pats: list{(kf(. key), TAny(Debug.empty)), ...pats},
            next: next,
          }
        | Some(refute) => aux(refute, cases)
        }
      }
    }
}

@raises(Exit)
let partial_match_check = (tree, debug) =>
  switch ParMatch.check(tree, ParMatch.key_int) {
  | {flag: Exhaustive, _} => ()
  | {flag: Partial, pats, _} => raise(Exit(Debug.partialMatch(debug, pats, ParMatch.toString)))
  }
