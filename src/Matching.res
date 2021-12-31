/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Array = Belt.Array
module SetInt = Belt.Set.Int
module MapString = Belt.Map.String
module Queue = Belt.MutableQueue
module TP = Typechecker.Pattern

exception Exit = Debug.Exit

type nest = Tuple | Record | Dict

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
type rec tree<'a> =
  /*
    A Switch represents a list of discreet values to test (1, "a", etc.). If
    none of the values match the input, then the wildcard is used.
 */
  | Switch({
      idx: int,
      key: string,
      ids: SetInt.t,
      cases: switchcase<'a>,
      wildcard: option<tree<'a>>,
    })
  /*
    A Nest represents a structure such as tuple or a record.
 */
  | Nest({
      idx: int,
      key: string,
      ids: SetInt.t,
      kind: nest,
      child: tree<tree<'a>>,
      wildcard: option<tree<'a>>,
    })
  /*
    A Construct represents one of the built-in variant types: lists and
    nullables.
    nil represents an empty list or a null value. It is like a wildcard in that
    it always points to the *next* node, whatever that may be.
    cons always either points to a wildcard node or a Nest + Tuple node.
 */
  | Construct({
      idx: int,
      key: string,
      ids: SetInt.t,
      kind: TP.construct,
      nil: option<tree<'a>>,
      cons: option<tree<'a>>,
    })
  /*
    Wildcards simply point to the next node in the tree.
 */
  | Wildcard({idx: int, key: string, ids: SetInt.t, child: tree<'a>})
  | End('a)

/*
  The switch cases work like linked lists of values. If an input matches a
  value, then we follow its associated tree. If not, we try the next case.
*/
and switchcase<'a> = {
  val: Data.Const.t,
  ifMatch: tree<'a>,
  nextCase: option<switchcase<'a>>,
}

/*
  Each "exit" is given an integer ID which we can use to look up the AST nodes
  to follow after the tree. We do this because exits can be duplicated when
  trees expand, and we don't want to duplicate entire ASTs.
*/

module Exit = {
  type key = int
  type t<'a> = array<'a>
  let get = Array.getExn
  let map = Array.map
  let unsafe_key = i => i
}

type leaf = {names: MapString.t<int>, exit: Exit.key}

type t<'a> = {
  tree: tree<leaf>,
  exits: Exit.t<'a>,
}

/*
  One challenge when merging the trees is that we need to keep track of "where
  we are" in the tree on the type level. This natural-number GADT does that for
  us.
*/

type rec nat<_, _> =
  | Z: nat<'z, 'z>
  | S(nat<'a, 'z>): nat<tree<'a>, 'z>

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

let rec mergeTestCasesAux:
  type a. (
    ~init: switchcase<a>=?,
    switchcase<a>,
    Data.Const.t,
    tree<a>,
    nat<a, leaf>,
  ) => option<switchcase<a>> =
  (~init=?, original, val, ifMatch, n) => {
    /* We sort the data for easier analysis. */
    let cmp = Data.Const.compare(val, original.val)
    if cmp < 0 {
      /* If the new value is lower than the original, then append it to the
       beginning. */
      let tail = {val: val, ifMatch: ifMatch, nextCase: Some(original)}
      switch init {
      | None => Some(tail)
      | Some(init) => Some(reverseCases(init, ~tail))
      }
    } else if cmp == 0 {
      /* If the new value equals the original, then merge them. */
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
      /* If the new value is greater than the original, then check if we're
      at the end of the list. If we are, then add this new case. If not, then
      keep searching. */
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
  type a. (switchcase<a>, switchcase<a>, nat<a, leaf>, bool) => option<switchcase<a>> =
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
  type a. (~init: switchcase<a>=?, tree<a>, switchcase<a>, nat<a, leaf>) => option<switchcase<a>> =
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
  type a. (~init: switchcase<a>=?, switchcase<a>, tree<a>, nat<a, leaf>) => switchcase<a> =
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
  type a b. (tree<a>, nat<a, leaf>, ~wildcard: tree<b>, nat<a, tree<b>>) => tree<a> =
  (a, na, ~wildcard, nb) =>
    switch (a, na, nb) {
    | (End(_), Z, _) => assert false
    | (End(a), S(n), Z) =>
      try {End(merge(a, wildcard, n))} catch {
      | MergeFail => End(a)
      }
    | (End(a), S(na), S(nb)) => End(expandWildcardAfterNest(a, na, ~wildcard, nb))
    | (Nest(a), na, nb) =>
      Nest({...a, child: expandWildcardAfterNest(a.child, S(na), ~wildcard, S(nb))})
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
  type a b. (~wildcard: tree<a>, nat<b, tree<a>>, tree<b>, nat<b, leaf>) => tree<b> =
  (~wildcard, na, b, nb) =>
    switch (b, na, nb) {
    | (End(b), Z, S(n)) => End(merge(wildcard, b, n))
    | (End(b), S(na), S(nb)) => End(mergeWildcardAfterNest(~wildcard, na, b, nb))
    | (Nest(b), na, nb) =>
      Nest({...b, child: mergeWildcardAfterNest(~wildcard, S(na), b.child, S(nb))})
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
  type a. (tree<a>, tree<a>, nat<a, leaf>) => tree<a> =
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
      let child = mergeWildcardAfterNest(~wildcard=a.child, Z, b.child, S(n))
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
      assert (a.kind == b.kind)
      let wildcard = switch (a.wildcard, b.wildcard) {
      | (None, None) => None
      | (Some(x), None) | (None, Some(x)) => Some(x)
      | (Some(a), Some(b)) => Some(merge(a, b, n))
      }
      let child = {
        let child = merge(a.child, b.child, S(n))
        switch wildcard {
        | None => child
        | Some(wildcard) => mergeWildcardAfterNest(~wildcard, Z, child, S(n))
        }
      }
      let ids = SetInt.union(a.ids, b.ids)
      Nest({...a, ids: ids, child: child, wildcard: wildcard})
    | (Nest(a), Wildcard(b)) =>
      let child = expandWildcardAfterNest(a.child, S(n), ~wildcard=b.child, Z)
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
      assert (a.kind == b.kind)
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
      // let cases = expandWildcardIntoTestCases(a.cases, ~wildcard, n)
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

type continue<'a> = (. MapString.t<int>) => tree<'a>

@raises(Exit)
let rec fromTPat: 'a. (_, _, _, _, ~name: _, continue<'a>) => tree<'a> = (p, i, key, b, ~name, k) =>
  switch p {
  | TP.TAny(_) => Wildcard({ids: SetInt.empty, idx: i, key: key, child: k(. b)})
  | TVar(Loc(id), x) | TOptionalVar(Loc(id), x) =>
    if MapString.has(b, x) {
      raise(Exit(Debug.nameBoundMultipleTimes(~binding=x, ~loc=Loc(id), ~name)))
    }
    Wildcard({
      ids: SetInt.add(SetInt.empty, id),
      idx: i,
      key: key,
      child: k(. MapString.set(b, x, id)),
    })
  | TConstruct(_, kind, Some(cons)) =>
    Construct({
      idx: i,
      key: key,
      ids: SetInt.empty,
      kind: kind,
      nil: None,
      cons: Some(fromTPat(cons, i, key, b, k, ~name)),
    })
  | TConstruct(_, kind, None) =>
    Construct({idx: i, key: key, ids: SetInt.empty, kind: kind, nil: Some(k(. b)), cons: None})
  | TConst(_, val) =>
    Switch({
      idx: i,
      key: key,
      ids: SetInt.empty,
      cases: {val: Data.Const.fromTPat(val), ifMatch: k(. b), nextCase: None},
      wildcard: None,
    })
  | TTuple(_, a) =>
    let child = fromArray(a, b, 0, ~name, (. b) => End(k(. b)))
    Nest({idx: i, key: key, ids: SetInt.empty, kind: Tuple, child: child, wildcard: None})
  | TRecord(_, a) =>
    let child = fromKeyValues(a, b, 0, ~name, (. b) => End(k(. b)))
    Nest({idx: i, key: key, ids: SetInt.empty, kind: Record, child: child, wildcard: None})
  | TDict(_, a) =>
    let child = fromKeyValues(a, b, 0, ~name, (. b) => End(k(. b)))
    Nest({idx: i, key: key, ids: SetInt.empty, kind: Dict, child: child, wildcard: None})
  }

@raises(Exit)
and fromArray: 'a. (_, _, _, ~name: _, continue<'a>) => tree<'a> = (a, b, i, ~name, k) =>
  switch a[i] {
  | None => k(. b)
  | Some(p) => fromTPat(p, i, "", b, ~name, (. b) => fromArray(a, b, succ(i), ~name, k))
  }

@raises(Exit)
and fromKeyValues: 'a. (_, _, _, ~name: _, continue<'a>) => tree<'a> = (a, b, i, ~name, k) =>
  switch a[i] {
  | None => k(. b)
  | Some((key, v)) => fromTPat(v, i, key, b, ~name, (. b) => fromKeyValues(a, b, succ(i), ~name, k))
  }

@raises(Exit)
let fromArray = (~exit, ~name, a) =>
  fromArray(a, MapString.empty, 0, ~name, (. names) => End({names: names, exit: exit}))

@raises(Exit)
let makeCase = (hd, a, ~exit, ~name) => {
  @raises(Exit)
  let rec aux = (t, i) =>
    switch NonEmpty.get(a, i) {
    | None => t
    | Some(ps) =>
      let b = fromArray(NonEmpty.toArray(ps), ~exit, ~name)
      switch merge(t, b, Z) {
      | t => aux(t, succ(i))
      | exception MergeFail => raise(Exit(Debug.unusedCase(ps, module(TP))))
      }
    }
  aux(hd, 1)
}

module ParMatch = {
  let makeRefutation = x =>
    switch x {
    | Data.Const.PBool(_) => Data.Const.PBool(false)
    | PInt(_) => PInt(0)
    | PString(_) => PString("a")
    | PFloat(_) => PFloat(0.0)
    }

  let succ = x =>
    switch x {
    | Data.Const.PBool(false) => Some(Data.Const.PBool(true))
    | PBool(true) => None
    | PInt(i) => Some(PInt(succ(i)))
    | PString(s) => Some(PString(s ++ "a"))
    | PFloat(f) => Some(PFloat(f +. 1.0))
    }

  type flag = Partial | Exhaustive

  type t<'a> = {
    flag: flag,
    pats: list<(string, TP.t)>,
    next: 'a,
  }

  let toArray = l => Belt.List.toArray(l)->Array.mapU((. (_k, v)) => v)
  let toKeyValues = l => Belt.List.toArray(l)

  let exhaustive = (key, {pats, flag, next}) => {
    let pat = switch key {
    | "" => TP.TAny(Loc(0))
    | k => TVar(Loc(0), k)
    }
    {flag: flag, pats: list{(key, pat), ...pats}, next: next}
  }

  @raises(Exit)
  let rec check: 'a. tree<'a> => t<'a> = tree =>
    switch tree {
    | End(next) => {flag: Exhaustive, pats: list{}, next: next}
    | Wildcard({key, child, _}) => exhaustive(key, check(child))
    | Nest({key, kind, child, wildcard, _}) =>
      // Either the child _or_ the wildcard can be exhaustive.
      // A nest filled with exhaustive patterns e.g. tuple (_, _, _) can lead to
      // an exhaustive path even if it's paired with a wildcard _ that doesn't.
      switch check(child) {
      | {flag: Exhaustive, next, _} => exhaustive(key, check(next))
      | {flag: Partial, pats, next} =>
        switch wildcard {
        | Some(wildcard) => exhaustive(key, check(wildcard))
        | None =>
          let nest = switch kind {
          | Tuple => TP.TTuple(Loc(0), toArray(pats))
          | Record => TRecord(Loc(0), toKeyValues(pats))
          | Dict => TDict(Loc(0), toKeyValues(pats))
          }
          let {pats, next, _} = check(next)
          {flag: Partial, pats: list{(key, nest), ...pats}, next: next}
        }
      }
    | Construct({key, kind, nil, cons, _}) =>
      switch (nil, cons) {
      | (None, Some(cons)) =>
        let {pats, next, _} = check(cons)
        {
          flag: Partial,
          pats: switch pats {
          | list{_, ...pats} => list{(key, TConstruct(Loc(0), kind, None)), ...pats}
          | _ => assert false
          },
          next: next,
        }
      | (Some(nil), None) =>
        let {pats, next, _} = check(nil)
        {
          flag: Partial,
          pats: list{(key, TConstruct(Loc(0), kind, Some(TAny(Loc(0))))), ...pats},
          next: next,
        }
      | (Some(nil), Some(cons)) =>
        switch check(nil) {
        | {flag: Exhaustive, _} =>
          switch check(cons) {
          | {flag: Exhaustive, _} as x => x
          | {flag: Partial, pats, next} =>
            let pats = switch pats {
            | list{(key, cons), ...pats} => list{
                (key, TP.TConstruct(Loc(0), kind, Some(cons))),
                ...pats,
              }
            | _ => assert false
            }
            {flag: Partial, pats: pats, next: next}
          }
        | {flag: Partial, pats, next} => {
            flag: Partial,
            pats: list{(key, TP.TConstruct(Loc(0), kind, None)), ...pats},
            next: next,
          }
        }
      | (None, None) => assert false
      }
    | Switch({key, cases, wildcard, _}) =>
      switch wildcard {
      | Some(wildcard) => exhaustive(key, check(wildcard))
      | None =>
        let refute = makeRefutation(cases.val)

        @raises(Exit)
        let rec aux = (refute, {val, ifMatch, nextCase}) =>
          switch check(ifMatch) {
          | {flag: Partial, pats, next} => {
              flag: Partial,
              pats: list{(key, TConst(Loc(0), Data.Const.toTPat(val))), ...pats},
              next: next,
            }
          | {flag: Exhaustive, pats, next} =>
            if Data.Const.equal(refute, val) {
              switch succ(refute) {
              | None => exhaustive(key, check(ifMatch))
              | Some(refute) =>
                switch nextCase {
                | None => {
                    flag: Partial,
                    pats: list{(key, TConst(Loc(0), Data.Const.toTPat(refute))), ...pats},
                    next: next,
                  }
                | Some(case) => aux(refute, case)
                }
              }
            } else {
              {
                flag: Partial,
                pats: list{(key, TConst(Loc(0), Data.Const.toTPat(refute))), ...pats},
                next: next,
              }
            }
          }
        aux(refute, cases)
      }
    }

  let toString = l => toArray(l)->Array.joinWith(", ", TP.toString)

  @raises(Exit)
  let check = (tree, ~loc) =>
    switch check(tree) {
    | {flag: Exhaustive, _} => ()
    | {flag: Partial, pats, _} => raise(Exit(Debug.partialMatch(pats, toString, ~loc)))
    }
}

@raises(Exit)
let make = (~name, cases: NonEmpty.t<Typechecker.case>) => {
  let exitq = Queue.make()
  let hdcase = NonEmpty.hd(cases)
  Queue.add(exitq, hdcase.nodes)
  let exit = Queue.size(exitq) - 1
  let hdTree = NonEmpty.hd(hdcase.pats)->NonEmpty.toArray->fromArray(~exit, ~name)
  let tree = makeCase(hdTree, hdcase.pats, ~name, ~exit)

  @raises(Exit)
  let rec aux = (tree, i) =>
    switch NonEmpty.get(cases, i) {
    | None => {tree: tree, exits: Queue.toArray(exitq)}
    | Some({pats, nodes}) =>
      Queue.add(exitq, nodes)
      let exit = Queue.size(exitq) - 1
      let hdTree = NonEmpty.hd(pats)->NonEmpty.toArray->fromArray(~exit, ~name)
      switch merge(tree, hdTree, Z) {
      | tree =>
        let tree = makeCase(tree, pats, ~exit, ~name)
        aux(tree, succ(i))
      | exception MergeFail => raise(Exit(Debug.unusedCase(NonEmpty.hd(pats), module(TP))))
      }
    }
  aux(tree, 1)
}
