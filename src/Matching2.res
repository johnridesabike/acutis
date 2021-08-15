module T = Acutis_Types
module Array = Belt.Array
module Ast = T.Ast
module Ast_Pattern = T.Ast_Pattern
module Debug2 = TypeChecker_Debug
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
    | TTuple
    | TLCons
    | TLNil
    | TRecord
    | TDict

  let compare = (a, b) =>
    switch (a, b) {
    | (TString(a), TString(b)) => compare(a, b)
    | (TFloat(a), TFloat(b)) => compare(a, b)
    | (TInt(a), TInt(b)) => compare(a, b)
    | (TTrue, TFalse) => 1
    | (TFalse, TTrue) => -1
    | (TTrue, TTrue)
    | (TFalse, TFalse)
    | (TNull, TNull)
    | (TTuple, TTuple)
    | (TLCons, TLCons)
    | (TLNil, TLNil)
    | (TRecord, TRecord)
    | (TDict, TDict) => 0
    // If the values are incoherent (not of the same type) then we sort the
    // newer value after the older value. When the type checker validates the
    // tree, it will assume that the earlier value is correct.
    | _ => -1
    }
}

module P = {
  type t = PTest(string, TestPat.t) | PEnd | PWildcard({key: string, name: string})

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
      Queue.add(q, PTest(k, TTuple))
      addTuple(q, t, 0)
    | #Array(_, a) =>
      let tail = Queue.make()
      Queue.add(tail, PTest(k, TLNil))
      addList(q, a, 0, ~tail)
    | #ArrayWithTailBinding(_, a, #Binding(_, x)) =>
      let tail = Queue.make()
      Queue.add(tail, PWildcard({key: "", name: x}))
      addList(q, a, 0, ~tail)
    | #Object(_, a) =>
      Queue.add(q, PTest(k, TRecord))
      addKeyValues(q, SortArray.stableSortByU(a, compareByKey), 0)
    | #Dict(_, a) =>
      Queue.add(q, PTest(k, TDict))
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
  | Test({key: string, tests: testcase, wildcard: option<tree>})
  | Wildcard(string, tree)
  | End(tree)
  | Leaf(int)

and testcase = {
  val: TestPat.t,
  ifmatch: tree,
  ifnomatch: option<testcase>,
}

let rec reverseTestCases = (~init=?, t) => {
  let init = {...t, ifnomatch: init}
  switch t.ifnomatch {
  | None => init
  | Some(t) => reverseTestCases(t, ~init)
  }
}

let rec lvlToEnd = (tree, lvl) =>
  switch lvl {
  | 0 => End(tree)
  | lvl => lvlToEnd(End(tree), pred(lvl))
  }

exception MergeFail

@raises(MergeFail)
let rec mergeTestCasesAux = (~init=?, original, val, ifmatch) => {
  let cmp = TestPat.compare(val, original.val)
  if cmp < 0 {
    {val: val, ifmatch: ifmatch, ifnomatch: Some(original)}
  } else if cmp == 0 {
    let ifmatch = merge(original.ifmatch, ifmatch)
    let tests = {...original, ifmatch: ifmatch}
    switch init {
    | None => tests
    | Some(init) => reverseTestCases(init, ~init=tests)
    }
  } else {
    let init = {...original, ifnomatch: init}
    switch original.ifnomatch {
    | None => reverseTestCases(init, ~init={val: val, ifmatch: ifmatch, ifnomatch: None})
    | Some(original) => mergeTestCasesAux(original, val, ifmatch, ~init)
    }
  }
}
@raises(MergeFail)
and mergeTestCases = (~init=?, original, {val, ifmatch, ifnomatch}) => {
  let init = try {
    switch init {
    | None => Some(mergeTestCasesAux(original, val, ifmatch))
    | Some(init) => Some(mergeTestCasesAux(init, val, ifmatch))
    }
  } catch {
  | MergeFail => init
  }
  switch ifnomatch {
  | None =>
    switch init {
    | None => raise(MergeFail)
    | Some(x) => x
    }
  | Some(b) => mergeTestCases(original, b, ~init?)
  }
}
/* This only keeps successful mergers. */
@raises(MergeFail)
and mergeAndKeepChildAfterNest = (a, b, lvl) =>
  switch a {
  | End(a) if lvl == 0 => merge(a, b)
  | End(a) => End(mergeAndKeepChildAfterNest(a, b, pred(lvl)))
  | Wildcard(k, a) => Wildcard(k, mergeAndKeepChildAfterNest(a, b, lvl))
  | Test({key, tests, wildcard}) =>
    let wildcard' = switch wildcard {
    | None => Ok(None)
    | Some(tree) =>
      try {Ok(Some(mergeAndKeepChildAfterNest(tree, b, lvl)))} catch {
      | e => Error(e)
      }
    }

    @raises(MergeFail)
    let rec aux = (~init=?, a) => {
      let lvl = switch a.val {
      | TTuple | TLCons | TRecord | TDict => succ(lvl)
      | _ => lvl
      }
      let init = try {
        Some({...a, ifmatch: mergeAndKeepChildAfterNest(a.ifmatch, b, lvl), ifnomatch: init})
      } catch {
      | MergeFail => init
      }
      switch a.ifnomatch {
      | None =>
        switch (init, wildcard') {
        | (Some(init), Ok(wildcard)) =>
          Test({key: key, tests: reverseTestCases(init), wildcard: wildcard})
        | (None, Ok(wildcard)) => Test({key: key, tests: tests, wildcard: wildcard})
        | (Some(init), Error(_)) =>
          Test({key: key, tests: reverseTestCases(init), wildcard: wildcard})
        | (None, Error(_)) => raise(MergeFail)
        }
      | Some(a) => aux(a, ~init?)
      }
    }
    aux(tests)
  | Leaf(_) => assert false
  }
/* This only keeps successful mergers. */
@raises(MergeFail)
and mergeAndKeepChildIntoTestCases = (~init=?, a, b) => {
  let init = try {
    let ifmatch = switch a.val {
    | TTuple | TLCons | TRecord | TDict => mergeAndKeepChildAfterNest(a.ifmatch, b, 0)
    | _ => merge(a.ifmatch, b)
    }
    Some({...a, ifmatch: ifmatch, ifnomatch: init})
  } catch {
  | MergeFail => init
  }
  switch (a.ifnomatch, init) {
  | (None, Some(init)) => reverseTestCases(init)
  | (None, None) => raise(MergeFail)
  | (Some(a), init) => mergeAndKeepChildIntoTestCases(a, b, ~init?)
  }
}
/* This only keeps successful mergers. */
@raises(MergeFail)
and mergeAndKeepChildIntoTestCases2 = (~init=?, a, b) => {
  let init = try {
    let ifmatch = switch b.val {
    | TTuple | TLCons | TRecord | TDict => mergeAndKeepChildAfterNest(a, b.ifmatch, 0)
    | _ => merge(a, b.ifmatch)
    }
    Some({...b, ifmatch: ifmatch, ifnomatch: init})
  } catch {
  | MergeFail => init
  }
  switch (b.ifnomatch, init) {
  | (None, Some(init)) => reverseTestCases(init)
  | (None, None) => raise(MergeFail)
  | (Some(b), init) => mergeAndKeepChildIntoTestCases2(a, b, ~init?)
  }
}
and expandChildAfterNest = (a, b, lvl) =>
  switch a {
  | End(a) if lvl == 0 =>
    try {End(merge(a, b))} catch {
    | MergeFail => End(a)
    }
  | End(a) => End(expandChildAfterNest(a, b, pred(lvl)))
  | Wildcard(k, a) => Wildcard(k, expandChildAfterNest(a, b, lvl))
  | Test({key, tests, wildcard}) =>
    let wildcard = switch wildcard {
    | None => lvlToEnd(b, lvl)
    | Some(tree) => expandChildAfterNest(tree, b, lvl)
    }
    let rec aux = (~init=?, a) => {
      let lvl = switch a.val {
      | TTuple | TLCons | TRecord | TDict => succ(lvl)
      | _ => lvl
      }
      let init = {...a, ifmatch: expandChildAfterNest(a.ifmatch, b, lvl), ifnomatch: init}
      switch a.ifnomatch {
      | None => Test({key: key, tests: reverseTestCases(init), wildcard: Some(wildcard)})
      | Some(a) => aux(a, ~init)
      }
    }
    aux(tests)
  | Leaf(_) => assert false
  }
and expandChildIntoTestCases = (~init=?, a, b) => {
  let ifmatch = switch a.val {
  | TTuple | TLCons | TRecord | TDict => expandChildAfterNest(a.ifmatch, b, 0)
  | _ =>
    try {merge(a.ifmatch, b)} catch {
    | MergeFail => a.ifmatch
    }
  }
  let init = {...a, ifmatch: ifmatch, ifnomatch: init}
  switch a.ifnomatch {
  | None => reverseTestCases(init)
  | Some(a) => expandChildIntoTestCases(a, b, ~init)
  }
}
@raises(MergeFail)
and merge = (a, b) =>
  switch (a, b) {
  /* Leaves */
  | (Leaf(_), Leaf(_)) => raise(MergeFail)
  /* Ends */
  | (End(a), End(b)) => End(merge(a, b))
  // Automatically extend rows when new keys are added.
  | (End(_), Test({key: "", _})) | (End(_), Wildcard("", _)) => assert false
  | (End(_) as a, Test({key, _}) as b)
  | (End(_) as a, Wildcard(key, _) as b) =>
    merge(Wildcard(key, a), b)
  /* Wildcards */
  | (Wildcard("", _), End(_)) => assert false
  | (Wildcard(key, a), End(_) as b) => Wildcard(key, merge(a, b))
  | (Wildcard(keyA, a) as a', Wildcard(keyB, b) as b') =>
    let cmp = compare(keyA, keyB)
    if cmp < 0 {
      merge(a', Wildcard(keyA, b'))
    } else if cmp == 0 {
      Wildcard(keyA, merge(a, b))
    } else {
      merge(Wildcard(keyB, a'), b')
    }
  | (Wildcard(keyA, a) as a', Test(b) as b') =>
    let cmp = compare(keyA, b.key)
    if cmp < 0 {
      merge(a', Wildcard(keyA, b'))
    } else if cmp == 0 {
      let wildcard = switch b.wildcard {
      | None => a
      | Some(b) => merge(a, b)
      }
      let tests = mergeAndKeepChildIntoTestCases2(wildcard, b.tests)
      Test({...b, tests: tests, wildcard: Some(wildcard)})
    } else {
      merge(Wildcard(b.key, a'), b')
    }
  /* Tests */
  | (Test({key: "", _}), End(_)) => assert false
  | (Test({key, _}) as a, End(_) as b) => merge(a, Wildcard(key, b))
  | (Test({key: keyA, tests, wildcard}) as a', Wildcard(keyB, b) as b') =>
    let cmp = compare(keyA, keyB)
    if cmp < 0 {
      merge(a', Wildcard(keyA, b'))
    } else if cmp == 0 {
      let wildcard = switch wildcard {
      | None => b
      | Some(a) => merge(a, b)
      }
      Test({key: keyA, tests: expandChildIntoTestCases(tests, wildcard), wildcard: Some(wildcard)})
    } else {
      merge(Wildcard(keyB, a'), b')
    }
  | (Test(a) as a', Test(b) as b') =>
    let cmp = compare(a.key, b.key)
    if cmp < 0 {
      merge(a', Wildcard(a.key, b'))
    } else if cmp == 0 {
      let wildcard = switch (a.wildcard, b.wildcard) {
      | (None, None) => Ok(None)
      | (Some(a), None) | (None, Some(a)) => Ok(Some(a))
      | (Some(a), Some(b)) =>
        try {Ok(Some(merge(a, b)))} catch {
        | e => Error(e)
        }
      }
      let b = switch wildcard {
      | Ok(Some(wildcard)) =>
        try {Ok(mergeAndKeepChildIntoTestCases(b.tests, wildcard))} catch {
        | e => Error(e)
        }
      | _ => Ok(b.tests)
      }
      switch (b, wildcard) {
      | (Ok(b), Ok(wildcard)) =>
        let tests = mergeTestCases(a.tests, b)
        let tests = switch wildcard {
        | None => tests
        | Some(wildcard) => expandChildIntoTestCases(tests, wildcard)
        }
        Test({...a, tests: tests, wildcard: wildcard})
      | (Ok(b), Error(_)) =>
        let tests = mergeTestCases(a.tests, b)
        let tests = switch a.wildcard {
        | None => tests
        | Some(wildcard) => expandChildIntoTestCases(tests, wildcard)
        }
        Test({...a, tests: tests})
      | (Error(_), Ok(wildcard)) => Test({...a, wildcard: wildcard})
      | (Error(_), Error(_)) => raise(MergeFail)
      }
    } else {
      merge(Wildcard(b.key, a'), b')
    }
  /* Failure cases... maybe? */
  | (Leaf(_), Test(_) | Wildcard(_) | End(_)) => assert false
  | (Test(_) | Wildcard(_) | End(_), Leaf(_)) => assert false
  }

let rec fromQueue = (q: Queue.t<P.t>, ~success) =>
  switch Queue.pop(q) {
  | None => success
  | Some(p) =>
    switch p {
    | PWildcard({key, _}) => Wildcard(key, fromQueue(q, ~success))
    | PEnd => End(fromQueue(q, ~success))
    | PTest(key, p) =>
      Test({
        key: key,
        tests: {
          val: p,
          ifmatch: fromQueue(q, ~success),
          ifnomatch: None,
        },
        wildcard: None,
      })
    }
  }

let fromNonEmpty = (ne, ~success) => {
  let q = P.make(ne)
  fromQueue(q, ~success)
}

let mergeFromNonEmpty = (a, ne, ~success) => {
  let b = fromNonEmpty(ne, ~success)
  try {merge(a, b)} catch {
  | MergeFail => a
  }
}

let rec makeCase = (t, a, i, ~success) =>
  switch a[i] {
  | None => t
  | Some(ps) => makeCase(mergeFromNonEmpty(t, ps, ~success), a, succ(i), ~success)
  }

type t<'a> = {
  tree: tree,
  nodes: array<T.Ast.nodes<'a>>,
}

let make = (NonEmpty({patterns: NonEmpty(hd, tl), nodes}, casestl): NonEmpty.t<T.Ast.case<_>>) => {
  let nodesq = Queue.make()
  Queue.add(nodesq, nodes)
  let success = Leaf(Queue.size(nodesq) - 1)
  let hd = fromNonEmpty(hd, ~success)
  let tree = makeCase(hd, tl, 0, ~success)
  let rec aux = (tree, i) =>
    switch casestl[i] {
    | None => {tree: tree, nodes: Queue.toArray(nodesq)}
    | Some({patterns: NonEmpty(hd, tl), nodes}) =>
      Queue.add(nodesq, nodes)
      let success = Leaf(Queue.size(nodesq) - 1)
      let hd = mergeFromNonEmpty(tree, hd, ~success)
      let tree = makeCase(hd, tl, 0, ~success)
      aux(tree, succ(i))
    }
  aux(tree, 0)
}
