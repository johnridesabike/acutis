module T = Acutis_Types
module Array = Belt.Array
module Ast = T.Ast
module Ast_Pattern = T.Ast_Pattern
module Debug2 = TypeChecker_Debug
module SetInt = Belt.Set.Int
module MapString = Belt.Map.String
module NonEmpty = T.NonEmpty
module Queue = Belt.MutableQueue
module Option = Belt.Option
module SortArray = Belt.SortArray

exception Exit = Debug.Exit
exception Exit2(string)

module TestPat = {
  type t =
    | TTrue
    | TFalse
    | TNull
    | TString(string)
    | TInt(int)
    | TFloat(float)
    | TLNil
    | TLCons

  type nest =
    | TTuple
    | TRecord
    | TDict
    | TList

  let compare = (a, b) =>
    switch (a, b) {
    | (TString(a), TString(b)) => compare(a, b)
    | (TFloat(a), TFloat(b)) => compare(a, b)
    | (TInt(a), TInt(b)) => compare(a, b)
    | (TTrue, TFalse) | (TLCons, TLNil) => 1
    | (TFalse, TTrue) | (TLNil, TLCons) => -1
    | (TTrue, TTrue)
    | (TFalse, TFalse)
    | (TNull, TNull)
    | (TLCons, TLCons)
    | (TLNil, TLNil) => 0
    // If the values are incoherent (not of the same type) then we sort the
    // newer value after the older value. When the type checker validates the
    // tree, it will assume that the earlier value is correct.
    | _ => -1
    }
}

module P = {
  type t =
    | PTest(string, TestPat.t)
    | PNest(string, TestPat.nest)
    | PEnd
    | PWildcard({loc: T.loc, key: string, name: string})

  let compareByKey = (. (ka: string, _), (kb, _)) => compare(ka, kb)

  let rec add = (q, k, p: T.Ast_Pattern.t) =>
    switch p {
    | #Null(_) => Queue.add(q, PTest(k, TNull))
    | #False(_) => Queue.add(q, PTest(k, TFalse))
    | #True(_) => Queue.add(q, PTest(k, TTrue))
    | #String(_, s) => Queue.add(q, PTest(k, TString(s)))
    | #Int(_, i) => Queue.add(q, PTest(k, TInt(i)))
    | #Float(_, i) => Queue.add(q, PTest(k, TFloat(i)))
    | #Binding(loc, x) => Queue.add(q, PWildcard({loc: loc, key: k, name: x}))
    | #Tuple(_, t) =>
      Queue.add(q, PNest(k, TTuple))
      addTuple(q, t, 0)
    | #Array(_, a) =>
      let tail = Queue.make()
      Queue.add(tail, PNest("", TList))
      Queue.add(tail, PTest(k, TLNil))
      Queue.add(tail, PEnd)
      addList(q, a, 0, ~tail)
    | #ArrayWithTailBinding(loc, a, #Binding(_, x)) =>
      let tail = Queue.make()
      Queue.add(tail, PWildcard({loc: loc, key: "", name: x}))
      addList(q, a, 0, ~tail)
    | #Object(_, a) =>
      Queue.add(q, PNest(k, TRecord))
      addKeyValues(q, SortArray.stableSortByU(a, compareByKey), 0)
    | #Dict(_, a) =>
      Queue.add(q, PNest(k, TDict))
      addKeyValues(q, SortArray.stableSortByU(a, compareByKey), 0)
    }
  and addTuple = (q, a, i) =>
    switch a[i] {
    | None => Queue.add(q, PEnd)
    | Some(p) =>
      add(q, "", p)
      addTuple(q, a, succ(i))
    }
  and addList = (q, a, i, ~tail) =>
    switch a[i] {
    | None => Queue.transfer(tail, q)
    | Some(p) =>
      Queue.add(q, PNest("", TList))
      Queue.add(q, PTest("", TLCons))
      add(q, "", p)
      Queue.add(tail, PEnd)
      addList(q, a, succ(i), ~tail)
    }
  and addKeyValues = (q, a, i) =>
    switch a[i] {
    | None => Queue.add(q, PEnd)
    | Some((k, v)) =>
      add(q, k, v)
      addKeyValues(q, a, succ(i))
    }

  let make = (NonEmpty(hd, tl): NonEmpty.t<_>) => {
    let q = Queue.make()
    let rec aux = (p, i) => {
      add(q, "", p)
      switch tl[i] {
      | None => q
      | Some(p) => aux(p, succ(i))
      }
    }
    aux(hd, 0)
  }
}

type rec tree =
  | Test({key: string, cases: testcase, wildcard: option<tree>, ids: array<int>})
  | Wildcard(array<int>, string, tree)
  | Nest({ids: array<int>, key: string, val: TestPat.nest, child: tree})
  | End(tree)
  | Leaf({bindings: array<int>, exit: int})

and testcase = {
  val: TestPat.t,
  ifMatch: tree,
  nextCase: option<testcase>,
}

let rec reverseTestCases = (~init=?, t) => {
  let init = {...t, nextCase: init}
  switch t.nextCase {
  | None => init
  | Some(t) => reverseTestCases(t, ~init)
  }
}

type rec nat = Z | S(nat)

let rec lvlToEnd = (tree, lvl) =>
  switch lvl {
  | Z => End(tree)
  | S(lvl) => lvlToEnd(End(tree), lvl)
  }

let realignByKey = (ka, a, kb, b) => {
  if ka == kb {
    None
  } else if ka < kb {
    Some((a, Wildcard([], ka, b)))
  } else {
    Some((Wildcard([], kb, a), b))
  }
}

exception MergeFail

@raises(MergeFail)
let rec mergeTestCasesAux = (~init=?, original, val, ifMatch) => {
  let cmp = TestPat.compare(val, original.val)
  if cmp < 0 {
    {val: val, ifMatch: ifMatch, nextCase: Some(original)}
  } else if cmp == 0 {
    let ifMatch = merge(original.ifMatch, ifMatch)
    let cases = {...original, ifMatch: ifMatch}
    switch init {
    | None => cases
    | Some(init) => reverseTestCases(init, ~init=cases)
    }
  } else {
    let init = {...original, nextCase: init}
    switch original.nextCase {
    | None => reverseTestCases(init, ~init={val: val, ifMatch: ifMatch, nextCase: None})
    | Some(original) => mergeTestCasesAux(original, val, ifMatch, ~init)
    }
  }
}
/* At least one merge must succeed. */
@raises(MergeFail)
and mergeTestCases = (~init=?, original, {val, ifMatch, nextCase}) => {
  let init = try {
    switch init {
    | None => Some(mergeTestCasesAux(original, val, ifMatch))
    | Some(init) => Some(mergeTestCasesAux(init, val, ifMatch))
    }
  } catch {
  | MergeFail => init
  }
  switch (nextCase, init) {
  | (None, None) => raise(MergeFail)
  | (None, Some(x)) => x
  | (Some(b), init) => mergeTestCases(original, b, ~init?)
  }
}
/* This only keeps successful mergers. */
@raises(MergeFail)
and mergeAndKeepWildcardIntoTestCases = (~init=?, a, b) => {
  let init = try {
    Some({...a, ifMatch: merge(a.ifMatch, b), nextCase: init})
  } catch {
  | MergeFail => init
  }
  switch (a.nextCase, init) {
  | (None, Some(init)) => reverseTestCases(init)
  | (None, None) => raise(MergeFail)
  | (Some(a), init) => mergeAndKeepWildcardIntoTestCases(a, b, ~init?)
  }
}
/* This only keeps successful mergers. */
@raises(MergeFail)
and mergeAndKeepTestCasesIntoWildcard = (~init=?, a, b) => {
  let init = try {
    Some({...b, ifMatch: merge(a, b.ifMatch), nextCase: init})
  } catch {
  | MergeFail => init
  }
  switch (b.nextCase, init) {
  | (None, Some(init)) => reverseTestCases(init)
  | (None, None) => raise(MergeFail)
  | (Some(b), init) => mergeAndKeepTestCasesIntoWildcard(a, b, ~init?)
  }
}
and expandWildcardAfterNest = (a, b, lvl) =>
  switch (a, lvl) {
  | (End(a), Z) =>
    try {End(merge(a, b))} catch {
    | MergeFail => End(a)
    }
  | (End(a), S(lvl)) => End(expandWildcardAfterNest(a, b, lvl))
  | (Nest(a), lvl) => Nest({...a, child: expandWildcardAfterNest(a.child, b, S(lvl))})
  | (Wildcard(l, k, a), lvl) => Wildcard(l, k, expandWildcardAfterNest(a, b, lvl))
  | (Test(a), lvl) =>
    let wildcard = switch a.wildcard {
    | None => lvlToEnd(b, lvl)
    | Some(a) => expandWildcardAfterNest(a, b, lvl)
    }
    let rec aux = (~init=?, case) => {
      let init = {
        ...case,
        ifMatch: expandWildcardAfterNest(case.ifMatch, b, lvl),
        nextCase: init,
      }
      switch case.nextCase {
      | None => Test({...a, cases: reverseTestCases(init), wildcard: Some(wildcard)})
      | Some(a) => aux(a, ~init)
      }
    }
    aux(a.cases)
  | (Leaf(_), _) => a // assert false
  }
and expandWildcardIntoTestCases = (~init=?, a, b) => {
  let ifMatch = try {merge(a.ifMatch, b)} catch {
  | MergeFail => a.ifMatch
  }
  let init = {...a, ifMatch: ifMatch, nextCase: init}
  switch a.nextCase {
  | None => reverseTestCases(init)
  | Some(a) => expandWildcardIntoTestCases(a, b, ~init)
  }
}
@raises(MergeFail)
and merge = (a, b) =>
  switch (a, b) {
  /* Leaves */
  // Leaves cannot merge. This case means that the `b` path is redundant and
  // will be unused.
  | (Leaf(_), Leaf(_)) => raise(MergeFail)
  /* Ends */
  | (End(a), End(b)) => End(merge(a, b))
  // Automatically extend rows when new keys are added.
  | (End(_), Test({key: "", _}) | Wildcard(_, "", _) | Nest({key: "", _}))
  | (Wildcard(_, "", _) | Nest({key: "", _}) | Test({key: "", _}), End(_)) =>
    assert false
  | (End(_) as a, (Test({key, _}) | Wildcard(_, key, _) | Nest({key, _})) as b) =>
    merge(Wildcard([], key, a), b)
  | ((Wildcard(_, key, _) | Nest({key, _}) | Test({key, _})) as a, End(_) as b) =>
    merge(a, Wildcard([], key, b))
  /* Wildcards */
  | (Wildcard(locA, keyA, a) as a', Wildcard(locB, keyB, b) as b') =>
    switch realignByKey(keyA, a', keyB, b') {
    | Some((a, b)) => merge(a, b)
    | None => Wildcard(Array.concat(locA, locB), keyA, merge(a, b))
    }
  | (Wildcard(idsA, keyA, a) as a', Nest(b) as b') =>
    switch realignByKey(keyA, a', b.key, b') {
    | Some((a, b)) => merge(a, b)
    | None => Nest({...b, ids: Array.concat(idsA, b.ids), child: merge(a, b.child)})
    }
  | (Wildcard(idsA, keyA, a) as a', Test(b) as b') =>
    switch realignByKey(keyA, a', b.key, b') {
    | Some((a, b)) => merge(a, b)
    | None =>
      let wildcard = switch b.wildcard {
      | None => a
      | Some(b) => merge(a, b)
      }
      let cases = mergeAndKeepTestCasesIntoWildcard(wildcard, b.cases)
      Test({...b, ids: Array.concat(idsA, b.ids), cases: cases, wildcard: Some(wildcard)})
    }
  /* Nests */
  | (Nest(a) as a', Nest(b) as b') =>
    assert (a.val == b.val)
    switch realignByKey(a.key, a', b.key, b') {
    | Some((a, b)) => merge(a, b)
    | None =>
      let child = merge(a.child, b.child)
      Nest({...a, ids: Array.concat(a.ids, b.ids), child: child})
    }
  | (Nest(a) as a', Wildcard(idsB, keyB, b) as b') =>
    switch realignByKey(a.key, a', keyB, b') {
    | Some((a, b)) => merge(a, b)
    | None =>
      Nest({...a, ids: Array.concat(a.ids, idsB), child: expandWildcardAfterNest(a.child, b, Z)})
    }
  /* Tests */
  | (Test(a) as a', Wildcard(idsB, keyB, b) as b') =>
    switch realignByKey(a.key, a', keyB, b') {
    | Some((a, b)) => merge(a, b)
    | None =>
      let wildcard = switch a.wildcard {
      | None => b
      | Some(a) => merge(a, b)
      }
      Test({
        ...a,
        cases: expandWildcardIntoTestCases(a.cases, wildcard),
        ids: Array.concat(a.ids, idsB),
        wildcard: Some(wildcard),
      })
    }
  | (Test(a) as a', Test(b) as b') =>
    switch realignByKey(a.key, a', b.key, b') {
    | Some((a, b)) => merge(a, b)
    | None =>
      let wildcard = switch (a.wildcard, b.wildcard) {
      | (None, None) => Ok(None)
      | (Some(a), None) | (None, Some(a)) => Ok(Some(a))
      | (Some(a), Some(b)) =>
        try {Ok(Some(merge(a, b)))} catch {
        | e => Error(e)
        }
      }
      let bcases = switch wildcard {
      | Ok(Some(wildcard)) =>
        try {Ok(mergeAndKeepWildcardIntoTestCases(b.cases, wildcard))} catch {
        | e => Error(e)
        }
      | _ => Ok(b.cases)
      }
      switch (bcases, wildcard) {
      | (Ok(bcases), Ok(wildcard)) =>
        let cases = mergeTestCases(a.cases, bcases)
        let cases = switch wildcard {
        | None => cases
        | Some(wildcard) => expandWildcardIntoTestCases(cases, wildcard)
        }
        Test({...a, cases: cases, wildcard: wildcard})
      | (Ok(bcases), Error(_)) =>
        let cases = mergeTestCases(a.cases, bcases)
        let cases = switch a.wildcard {
        | None => cases
        | Some(wildcard) => expandWildcardIntoTestCases(cases, wildcard)
        }
        Test({...a, cases: cases})
      | (Error(_), Ok(wildcard)) => Test({...a, wildcard: wildcard})
      | (Error(_), Error(_)) => raise(MergeFail)
      }
    }
  /* Failure cases... maybe? */
  | (Test(_), Nest(_))
  | (Nest(_), Test(_))
  | (Leaf(_), Test(_) | Wildcard(_) | End(_) | Nest(_))
  | (Test(_) | Wildcard(_) | End(_) | Nest(_), Leaf(_)) =>
    assert false
  }

let rec fromQueue = (q: Queue.t<P.t>, bindings, exit) =>
  switch Queue.pop(q) {
  | None => Leaf({bindings: SetInt.toArray(bindings), exit: exit})
  | Some(p) =>
    switch p {
    | PWildcard({loc: Loc(l), key, _}) =>
      Wildcard([l], key, fromQueue(q, SetInt.add(bindings, l), exit))
    | PNest(key, val) => Nest({ids: [], key: key, val: val, child: fromQueue(q, bindings, exit)})
    | PEnd => End(fromQueue(q, bindings, exit))
    | PTest(key, p) =>
      Test({
        key: key,
        ids: [],
        cases: {
          val: p,
          ifMatch: fromQueue(q, bindings, exit),
          nextCase: None,
        },
        wildcard: None,
      })
    }
  }

let fromNonEmpty = (ne, exit) => {
  let q = P.make(ne)
  fromQueue(q, SetInt.empty, exit)
}

let mergeFromNonEmpty = (a, ne, exit) => {
  let b = fromNonEmpty(ne, exit)
  try {merge(a, b)} catch {
  | MergeFail => a
  }
}

let rec makeCase = (t, a, i, exit) =>
  switch a[i] {
  | None => t
  | Some(ps) => makeCase(mergeFromNonEmpty(t, ps, exit), a, succ(i), exit)
  }

type t<'a> = {
  tree: tree,
  nodes: array<T.Ast.nodes<'a>>,
}

let make = (NonEmpty({patterns: NonEmpty(hd, tl), nodes}, casestl): NonEmpty.t<T.Ast.case<_>>) => {
  let nodesq = Queue.make()
  Queue.add(nodesq, nodes)
  let exit = Queue.size(nodesq) - 1
  let hd = fromNonEmpty(hd, exit)
  let tree = makeCase(hd, tl, 0, exit)
  let rec aux = (tree, i) =>
    switch casestl[i] {
    | None => {tree: tree, nodes: Queue.toArray(nodesq)}
    | Some({patterns: NonEmpty(hd, tl), nodes}) =>
      Queue.add(nodesq, nodes)
      let exit = Queue.size(nodesq) - 1
      let hd = mergeFromNonEmpty(tree, hd, exit)
      let tree = makeCase(hd, tl, 0, exit)
      aux(tree, succ(i))
    }
  aux(tree, 0)
}
