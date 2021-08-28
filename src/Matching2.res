module T = Acutis_Types
module Array = Belt.Array
module Ast = T.Ast
module Ast_Pattern = T.Ast_Pattern
module Debug2 = TypeChecker_Debug
module SetString = Belt.Set.String
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
    | PWildcard({key: string, name: string})

  let compareByKey = (. (ka: string, _), (kb, _)) => compare(ka, kb)

  let rec add = (q, k, p: T.Ast_Pattern.t) =>
    switch p {
    | #Null(_) => Queue.add(q, PTest(k, TNull))
    | #False(_) => Queue.add(q, PTest(k, TFalse))
    | #True(_) => Queue.add(q, PTest(k, TTrue))
    | #String(_, s) => Queue.add(q, PTest(k, TString(s)))
    | #Int(_, i) => Queue.add(q, PTest(k, TInt(i)))
    | #Float(_, i) => Queue.add(q, PTest(k, TFloat(i)))
    | #Binding(_, x) => Queue.add(q, PWildcard({key: k, name: x}))
    | #Tuple(_, t) =>
      Queue.add(q, PNest(k, TTuple))
      addTuple(q, t, 0)
    | #Array(_, a) =>
      let tail = Queue.make()
      Queue.add(tail, PNest("", TList))
      Queue.add(tail, PTest(k, TLNil))
      Queue.add(tail, PEnd)
      addList(q, a, 0, ~tail)
    | #ArrayWithTailBinding(_, a, #Binding(_, x)) =>
      let tail = Queue.make()
      Queue.add(tail, PWildcard({key: "", name: x}))
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

type leaf = {bindings: array<string>, exit: int}

type rec tree<'a> =
  | Test({names: array<string>, key: string, cases: testcase<'a>, wildcard: option<tree<'a>>})
  | Wildcard({names: array<string>, key: string, child: tree<'a>})
  | Nest({names: array<string>, key: string, val: TestPat.nest, child: tree<tree<'a>>})
  | End('a)

and testcase<'a> = {
  val: TestPat.t,
  ifMatch: tree<'a>,
  nextCase: option<testcase<'a>>,
}

let rec reverseTestCases = (~init=?, t) => {
  let init = {...t, nextCase: init}
  switch t.nextCase {
  | None => init
  | Some(t) => reverseTestCases(t, ~init)
  }
}

type rec nat<_, _> = Z: nat<'z, 'z> | S(nat<'a, 'z>): nat<tree<'a>, 'z>

let rec lvlToEnd:
  type a z. (z, nat<a, z>) => tree<a> =
  (t, n) =>
    switch n {
    | Z => End(t)
    | S(n) => End(lvlToEnd(t, n))
    }

let realignByKey = (ka, a, kb, b) => {
  if ka == kb {
    None
  } else if ka < kb {
    Some((a, Wildcard({names: [], key: ka, child: b})))
  } else {
    Some((Wildcard({names: [], key: kb, child: a}), b))
  }
}

exception MergeFail

@raises(MergeFail)
let rec mergeTestCasesAux:
  type a. (~init: testcase<a>=?, testcase<a>, TestPat.t, tree<a>, nat<a, leaf>) => testcase<a> =
  (~init=?, original, val, ifMatch, n) => {
    let cmp = TestPat.compare(val, original.val)
    if cmp < 0 {
      {val: val, ifMatch: ifMatch, nextCase: Some(original)}
    } else if cmp == 0 {
      let ifMatch = merge(original.ifMatch, ifMatch, n)
      let cases = {...original, ifMatch: ifMatch}
      switch init {
      | None => cases
      | Some(init) => reverseTestCases(init, ~init=cases)
      }
    } else {
      let init = {...original, nextCase: init}
      switch original.nextCase {
      | None => reverseTestCases(init, ~init={val: val, ifMatch: ifMatch, nextCase: None})
      | Some(original) => mergeTestCasesAux(original, val, ifMatch, ~init, n)
      }
    }
  }
/* At least one merge must succeed. */
@raises(MergeFail)
and mergeTestCases:
  type a. (~init: testcase<a>=?, testcase<a>, testcase<a>, nat<a, leaf>) => testcase<a> =
  (~init=?, original, {val, ifMatch, nextCase}, n) => {
    let init = try {
      switch init {
      | None => Some(mergeTestCasesAux(original, val, ifMatch, n))
      | Some(init) => Some(mergeTestCasesAux(init, val, ifMatch, n))
      }
    } catch {
    | MergeFail => init
    }
    switch (nextCase, init) {
    | (None, None) => raise(MergeFail)
    | (None, Some(x)) => x
    | (Some(b), init) => mergeTestCases(original, b, ~init?, n)
    }
  }
/* This only keeps successful mergers. */
@raises(MergeFail)
and mergeAndKeepWildcardIntoTestCases:
  type a. (~init: testcase<a>=?, testcase<a>, tree<a>, nat<a, leaf>) => testcase<a> =
  (~init=?, a, b, n) => {
    let init = try {
      Some({...a, ifMatch: merge(a.ifMatch, b, n), nextCase: init})
    } catch {
    | MergeFail => init
    }
    switch (a.nextCase, init) {
    | (None, Some(init)) => reverseTestCases(init)
    | (None, None) => raise(MergeFail)
    | (Some(a), init) => mergeAndKeepWildcardIntoTestCases(a, b, ~init?, n)
    }
  }
/* This only keeps successful mergers. */
@raises(MergeFail)
and mergeAndKeepTestCasesIntoWildcard:
  type a. (~init: testcase<a>=?, tree<a>, testcase<a>, nat<a, leaf>) => testcase<a> =
  (~init=?, a, b, n) => {
    let init = try {
      Some({...b, ifMatch: merge(a, b.ifMatch, n), nextCase: init})
    } catch {
    | MergeFail => init
    }
    switch (b.nextCase, init) {
    | (None, Some(init)) => reverseTestCases(init)
    | (None, None) => raise(MergeFail)
    | (Some(b), init) => mergeAndKeepTestCasesIntoWildcard(a, b, ~init?, n)
    }
  }
and expandWildcardAfterNest:
  type a b. (tree<a>, tree<b>, nat<a, leaf>, nat<a, tree<b>>) => tree<a> =
  (a, b, na, nb) =>
    switch (na, nb, a) {
    | (Z, _, End(_)) => assert false
    | (S(n), Z, End(a)) =>
      try {End(merge(a, b, n))} catch {
      | MergeFail => End(a)
      }
    | (S(na), S(nb), End(a)) => End(expandWildcardAfterNest(a, b, na, nb))
    | (na, nb, Nest(a)) => Nest({...a, child: expandWildcardAfterNest(a.child, b, S(na), S(nb))})
    | (na, nb, Wildcard(a)) => Wildcard({...a, child: expandWildcardAfterNest(a.child, b, na, nb)})
    | (na, nb, Test(a)) =>
      let wildcard = switch a.wildcard {
      | None => lvlToEnd(b, nb)
      | Some(a) => expandWildcardAfterNest(a, b, na, nb)
      }
      let rec aux = (~init=?, case) => {
        let init = {
          ...case,
          ifMatch: expandWildcardAfterNest(case.ifMatch, b, na, nb),
          nextCase: init,
        }
        switch case.nextCase {
        | None => Test({...a, cases: reverseTestCases(init), wildcard: Some(wildcard)})
        | Some(a) => aux(a, ~init)
        }
      }
      aux(a.cases)
    }
and expandWildcardIntoTestCases:
  type a. (~init: testcase<a>=?, testcase<a>, tree<a>, nat<a, leaf>) => testcase<a> =
  (~init=?, a, b, n) => {
    let ifMatch = try {merge(a.ifMatch, b, n)} catch {
    | MergeFail => a.ifMatch
    }
    let init = {...a, ifMatch: ifMatch, nextCase: init}
    switch a.nextCase {
    | None => reverseTestCases(init)
    | Some(a) => expandWildcardIntoTestCases(a, b, ~init, n)
    }
  }
@raises(MergeFail)
and merge:
  type a. (tree<a>, tree<a>, nat<a, leaf>) => tree<a> =
  (a, b, n) =>
    switch (n, a, b) {
    | (Z, End(_), End(_)) => raise(MergeFail)
    | (S(n), End(a), End(b)) => End(merge(a, b, n))
    | (_, End(_), Test({key: "", _}) | Wildcard({key: "", _}) | Nest({key: "", _}))
    | (_, Wildcard({key: "", _}) | Nest({key: "", _}) | Test({key: "", _}), End(_)) =>
      assert false
    | (n, End(_) as a, (Test({key, _}) | Wildcard({key, _}) | Nest({key, _})) as b) =>
      merge(Wildcard({names: [], key: key, child: a}), b, n)
    | (n, (Wildcard({key, _}) | Nest({key, _}) | Test({key, _})) as a, End(_) as b) =>
      merge(a, Wildcard({names: [], key: key, child: b}), n)

    | (n, Wildcard(a) as a', Wildcard(b) as b') =>
      switch realignByKey(a.key, a', b.key, b') {
      | Some((a, b)) => merge(a, b, n)
      | None =>
        Wildcard({...a, names: Array.concat(a.names, b.names), child: merge(a.child, b.child, n)})
      }
    | (n, Wildcard(a) as a', Nest(b) as b') =>
      switch realignByKey(a.key, a', b.key, b') {
      | Some((a, b)) => merge(a, b, n)
      | None =>
        Nest({
          ...b,
          names: Array.concat(a.names, b.names),
          child: assert false,
        })
      }
    | (n, Wildcard(a) as a', Test(b) as b') =>
      switch realignByKey(a.key, a', b.key, b') {
      | Some((a, b)) => merge(a, b, n)
      | None =>
        let wildcard = switch b.wildcard {
        | None => a.child
        | Some(b) => merge(a.child, b, n)
        }
        let cases = mergeAndKeepTestCasesIntoWildcard(wildcard, b.cases, n)
        Test({...b, names: Array.concat(a.names, b.names), cases: cases, wildcard: Some(wildcard)})
      }

    | (n, Nest(a) as a', Nest(b) as b') =>
      assert (a.val == b.val)
      switch realignByKey(a.key, a', b.key, b') {
      | Some((a, b)) => merge(a, b, n)
      | None =>
        let child = merge(a.child, b.child, S(n))
        Nest({...a, names: Array.concat(a.names, b.names), child: child})
      }
    | (n, Nest(a) as a', Wildcard(b) as b') =>
      switch realignByKey(a.key, a', a.key, b') {
      | Some((a, b)) => merge(a, b, n)
      | None =>
        Nest({
          ...a,
          names: Array.concat(a.names, b.names),
          child: expandWildcardAfterNest(a.child, b.child, S(n), Z),
        })
      }

    | (n, Test(a) as a', Wildcard(b) as b') =>
      switch realignByKey(a.key, a', b.key, b') {
      | Some((a, b)) => merge(a, b, n)
      | None =>
        let wildcard = switch a.wildcard {
        | None => b.child
        | Some(a) => merge(a, b.child, n)
        }
        Test({
          ...a,
          cases: expandWildcardIntoTestCases(a.cases, wildcard, n),
          names: Array.concat(a.names, b.names),
          wildcard: Some(wildcard),
        })
      }
    | (n, Test(a) as a', Test(b) as b') =>
      switch realignByKey(a.key, a', b.key, b') {
      | Some((a, b)) => merge(a, b, n)
      | None =>
        let wildcard = switch (a.wildcard, b.wildcard) {
        | (None, None) => Ok(None)
        | (Some(a), None) | (None, Some(a)) => Ok(Some(a))
        | (Some(a), Some(b)) =>
          try {Ok(Some(merge(a, b, n)))} catch {
          | e => Error(e)
          }
        }
        let bcases = switch wildcard {
        | Ok(Some(wildcard)) =>
          try {Ok(mergeAndKeepWildcardIntoTestCases(b.cases, wildcard, n))} catch {
          | e => Error(e)
          }
        | _ => Ok(b.cases)
        }
        switch (bcases, wildcard) {
        | (Ok(bcases), Ok(wildcard)) =>
          let cases = mergeTestCases(a.cases, bcases, n)
          let cases = switch wildcard {
          | None => cases
          | Some(wildcard) => expandWildcardIntoTestCases(cases, wildcard, n)
          }
          Test({...a, cases: cases, wildcard: wildcard})
        | (Ok(bcases), Error(_)) =>
          let cases = mergeTestCases(a.cases, bcases, n)
          let cases = switch a.wildcard {
          | None => cases
          | Some(wildcard) => expandWildcardIntoTestCases(cases, wildcard, n)
          }
          Test({...a, cases: cases})
        | (Error(_), Ok(wildcard)) => Test({...a, wildcard: wildcard})
        | (Error(_), Error(_)) => raise(MergeFail)
        }
      }

    | (_, Test(_), Nest(_))
    | (_, Nest(_), Test(_)) =>
      assert false
    }

let rec fromQueue:
  type a. (Queue.t<P.t>, SetString.t, int, nat<a, leaf>) => tree<a> =
  (q, bindings, exit, n) =>
    switch (n, Queue.pop(q)) {
    | (n, Some(p)) =>
      switch (n, p) {
      | (n, PWildcard({key, name})) =>
        switch name {
        | "_" => Wildcard({names: [], key: key, child: fromQueue(q, bindings, exit, n)})
        | x =>
          Wildcard({names: [x], key: key, child: fromQueue(q, SetString.add(bindings, x), exit, n)})
        }
      | (n, PNest(key, val)) =>
        Nest({names: [], key: key, val: val, child: fromQueue(q, bindings, exit, S(n))})
      | (S(n), PEnd) => End(fromQueue(q, bindings, exit, n))
      | (Z, PEnd) => assert false
      | (n, PTest(key, p)) =>
        Test({
          key: key,
          names: [],
          cases: {
            val: p,
            ifMatch: fromQueue(q, bindings, exit, n),
            nextCase: None,
          },
          wildcard: None,
        })
      }
    | (Z, None) => End({bindings: SetString.toArray(bindings), exit: exit})
    | (S(_), None) => assert false
    }

let fromNonEmpty = (ne, exit) => fromQueue(P.make(ne), SetString.empty, exit, Z)

let merge = (a, b) =>
  try {merge(a, b, Z)} catch {
  | MergeFail => a
  }

let makeCase = (t, a, exit) => {
  let rec aux = (t, i) =>
    switch a[i] {
    | None => t
    | Some(ps) =>
      let b = fromNonEmpty(ps, exit)
      let t = merge(t, b)
      aux(t, succ(i))
    }
  aux(t, 0)
}

type t<'a> = {
  tree: tree<leaf>,
  exits: array<T.Ast.nodes<'a>>,
}

let make = (NonEmpty({patterns: NonEmpty(hd, tl), nodes}, casestl): NonEmpty.t<T.Ast.case<_>>) => {
  let exitq = Queue.make()
  Queue.add(exitq, nodes)
  let exit = Queue.size(exitq) - 1
  let hd = fromNonEmpty(hd, exit)
  let tree = makeCase(hd, tl, exit)
  let rec aux = (tree, i) =>
    switch casestl[i] {
    | None => {tree: tree, exits: Queue.toArray(exitq)}
    | Some({patterns: NonEmpty(hd, tl), nodes}) =>
      Queue.add(exitq, nodes)
      let exit = Queue.size(exitq) - 1
      let hd = merge(tree, fromNonEmpty(hd, exit))
      let tree = makeCase(hd, tl, exit)
      aux(tree, succ(i))
    }
  aux(tree, 0)
}
