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
module TC = TypeChecker

exception Exit = Debug.Exit
exception Exit2(string)

module TestPat = {
  type t =
    | TTrue
    | TFalse
    | TString(string)
    | TInt(int)
    | TFloat(float)

  type nest =
    | TTuple
    | TRecord
    | TDict

  let make = x =>
    switch x {
    | TC.TypedPattern.TPat_False => TFalse
    | TPat_True => TTrue
    | TPat_String(s) => TString(s)
    | TPat_Int(i) => TInt(i)
    | TPat_Float(f) => TFloat(f)
    }

  let compare = (a, b) =>
    switch (a, b) {
    | (TString(a), TString(b)) => compare(a, b)
    | (TFloat(a), TFloat(b)) => compare(a, b)
    | (TInt(a), TInt(b)) => compare(a, b)
    | (TTrue, TTrue)
    | (TFalse, TFalse) => 0
    | (TTrue, TFalse) => 1
    | (TFalse, TTrue) => -1
    // If the values are incoherent (not of the same type) then we sort the
    // newer value after the older value. When the type checker validates the
    // tree, it will assume that the earlier value is correct.
    | _ => -1
    }
}

module P = {
  type nc = PNil | PCons

  type t =
    | PSwitch(string, TestPat.t)
    | PNest(string, TestPat.nest)
    | PVariant(string, TC.TypedPattern.variant, nc)
    | PNull(string)
    | PEnd
    | PWildcard({key: string, name: string})

  let compareByKey = (. (ka: string, _), (kb, _)) => compare(ka, kb)

  let rec add = (q, k, p: T.Ast_Pattern.t) =>
    switch p {
    | #Null(_) => Queue.add(q, PNull(k))
    | #False(_) => Queue.add(q, PSwitch(k, TFalse))
    | #True(_) => Queue.add(q, PSwitch(k, TTrue))
    | #String(_, s) => Queue.add(q, PSwitch(k, TString(s)))
    | #Int(_, i) => Queue.add(q, PSwitch(k, TInt(i)))
    | #Float(_, i) => Queue.add(q, PSwitch(k, TFloat(i)))
    | #Binding(_, x) => Queue.add(q, PWildcard({key: k, name: x}))
    | #Tuple(_, t) =>
      Queue.add(q, PNest(k, TTuple))
      addTuple(q, t, 0)
    | #Array(_, a) =>
      let tail = Queue.make()
      Queue.add(tail, PVariant("", TPat_List, PNil))
      addList(q, k, a, 0, ~tail)
    | #ArrayWithTailBinding(_, a, #Binding(_, x)) =>
      let tail = Queue.make()
      Queue.add(tail, PWildcard({key: "", name: x}))
      addList(q, k, a, 0, ~tail)
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

  and addList = (q, k, a, i, ~tail) =>
    switch a[i] {
    | None => Queue.transfer(tail, q)
    | Some(p) =>
      Queue.add(q, PVariant(k, TPat_List, PCons))
      Queue.add(q, PNest("", TTuple))
      add(q, "", p)
      Queue.add(tail, PEnd)
      addList(q, "", a, succ(i), ~tail)
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
  | Switch({names: array<string>, key: string, cases: testcase, wildcard: option<tree>})
  | Nest({names: array<string>, key: string, kind: TestPat.nest, child: tree})
  | Variant({
      key: string,
      names: array<string>,
      kind: TC.TypedPattern.variant,
      nil: option<tree>,
      cons: option<tree>,
    })
  | Wildcard({names: array<string>, key: string, child: tree})
  | End(tree)
  | Leaf({bindings: array<string>, exit: int})

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

let realignByKey = (ka, a, kb, b) =>
  if ka == kb {
    None
  } else if ka < kb {
    Some((a, Wildcard({names: [], key: ka, child: b})))
  } else {
    Some((Wildcard({names: [], key: kb, child: a}), b))
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
and mergeAndKeepTestCasesIntoWildcard = (~init=?, ~wildcard, t) => {
  let init = try {
    Some({...t, ifMatch: merge(wildcard, t.ifMatch), nextCase: init})
  } catch {
  | MergeFail => init
  }
  switch (t.nextCase, init) {
  | (None, None) => raise(MergeFail)
  | (None, Some(init)) => reverseTestCases(init)
  | (Some(t), init) => mergeAndKeepTestCasesIntoWildcard(~wildcard, t, ~init?)
  }
}
/* This only keeps successful mergers. */
@raises(MergeFail)
and mergeWildcardAfterNest = (~wildcard, t, lvl) =>
  switch (t, lvl) {
  | (End(t), Z) => End(merge(wildcard, t))
  | (End(t), S(lvl)) => End(mergeWildcardAfterNest(~wildcard, t, lvl))
  | (Nest(t), lvl) => Nest({...t, child: mergeWildcardAfterNest(~wildcard, t.child, S(lvl))})
  | (Variant(a), lvl) =>
    let nil = switch a.nil {
    | None => Ok(None)
    | Some(child) =>
      try {Ok(Some(mergeWildcardAfterNest(~wildcard, child, lvl)))} catch {
      | MergeFail => Error(MergeFail)
      }
    }
    let cons = switch a.cons {
    | None => Ok(None)
    | Some(child) =>
      try {Ok(Some(mergeWildcardAfterNest(~wildcard, child, lvl)))} catch {
      | MergeFail => Error(MergeFail)
      }
    }
    switch (nil, cons) {
    | (Ok(nil), Ok(cons)) => Variant({...a, nil: nil, cons: cons})
    | (Ok(nil), Error(_)) => Variant({...a, nil: nil})
    | (Error(_), Ok(cons)) => Variant({...a, cons: cons})
    | (Error(_), Error(_)) => raise(MergeFail)
    }
  | (Wildcard(t), lvl) => Wildcard({...t, child: mergeWildcardAfterNest(~wildcard, t.child, lvl)})
  | (Switch(t), lvl) =>
    let wildcard = switch t.wildcard {
    | None => lvlToEnd(wildcard, lvl)
    | Some(t) => mergeWildcardAfterNest(~wildcard, t, lvl)
    }
    let rec aux = (~init=?, case) => {
      let init = try {
        Some({
          ...case,
          ifMatch: mergeWildcardAfterNest(~wildcard, case.ifMatch, lvl),
          nextCase: init,
        })
      } catch {
      | MergeFail => init
      }
      switch (case.nextCase, init) {
      | (None, None) => raise(MergeFail)
      | (None, Some(init)) =>
        Switch({...t, cases: reverseTestCases(init), wildcard: Some(wildcard)})
      | (Some(a), init) => aux(a, ~init?)
      }
    }
    aux(t.cases)
  | (Leaf(_), _) => assert false
  }
and expandWildcardAfterNest = (~wildcard, a, lvl) =>
  switch (a, lvl) {
  | (End(a), Z) =>
    try {End(merge(a, wildcard))} catch {
    | MergeFail => End(a)
    }
  | (End(a), S(lvl)) => End(expandWildcardAfterNest(~wildcard, a, lvl))
  | (Nest(a), lvl) => Nest({...a, child: expandWildcardAfterNest(~wildcard, a.child, S(lvl))})
  | (Variant(a), lvl) =>
    // Fix this.
    Variant({
      ...a,
      nil: switch a.nil {
      | None => Some(Wildcard({key: a.key, names: [], child: lvlToEnd(wildcard, lvl)}))
      | Some(child) => Some(expandWildcardAfterNest(~wildcard, child, lvl))
      },
      cons: switch a.cons {
      | None => Some(Wildcard({key: a.key, names: [], child: lvlToEnd(wildcard, lvl)}))
      | Some(child) => Some(expandWildcardAfterNest(~wildcard, child, lvl))
      },
    })
  | (Wildcard(a), lvl) => Wildcard({...a, child: expandWildcardAfterNest(~wildcard, a.child, lvl)})
  | (Switch(a), lvl) =>
    let wildcard' = switch a.wildcard {
    | None => lvlToEnd(wildcard, lvl)
    | Some(a) => expandWildcardAfterNest(~wildcard, a, lvl)
    }
    let rec aux = (~init=?, case) => {
      let init = {
        ...case,
        ifMatch: expandWildcardAfterNest(~wildcard, case.ifMatch, lvl),
        nextCase: init,
      }
      switch case.nextCase {
      | None => Switch({...a, cases: reverseTestCases(init), wildcard: Some(wildcard')})
      | Some(a) => aux(a, ~init)
      }
    }
    aux(a.cases)
  | (Leaf(_), _) => assert false
  }
and expandWildcardIntoTestCases = (~init=?, ~wildcard, t) => {
  let ifMatch = try {merge(t.ifMatch, wildcard)} catch {
  | MergeFail => t.ifMatch
  }
  let init = {...t, ifMatch: ifMatch, nextCase: init}
  switch t.nextCase {
  | None => reverseTestCases(init)
  | Some(t) => expandWildcardIntoTestCases(~wildcard, t, ~init)
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
  | (End(_) as a, (Switch({key, _}) | Wildcard({key, _}) | Nest({key, _})) as b) =>
    merge(Wildcard({names: [], key: key, child: a}), b)
  | ((Wildcard({key, _}) | Nest({key, _}) | Switch({key, _})) as a, End(_) as b) =>
    merge(a, Wildcard({names: [], key: key, child: b}))
  /* Wildcards */
  | (Wildcard(a) as a', Wildcard(b) as b') =>
    switch realignByKey(a.key, a', b.key, b') {
    | Some((a, b)) => merge(a, b)
    | None =>
      Wildcard({...a, names: Array.concat(a.names, b.names), child: merge(a.child, b.child)})
    }
  | (Wildcard(a) as a', Nest(b) as b') =>
    switch realignByKey(a.key, a', b.key, b') {
    | Some((a, b)) => merge(a, b)
    | None =>
      let child = mergeWildcardAfterNest(~wildcard=a.child, b.child, Z)
      Nest({...b, names: Array.concat(a.names, b.names), child: child})
    }
  | (Wildcard(a) as a', Variant(b)) =>
    let nil = switch b.nil {
    | None => Ok(a.child)
    | Some(b) =>
      try {Ok(merge(a.child, b))} catch {
      | MergeFail => Error(MergeFail)
      }
    }
    let cons = switch b.cons {
    | None => Ok(a')
    | Some(b) =>
      try {Ok(merge(a', b))} catch {
      | MergeFail => Error(MergeFail)
      }
    }
    let names = Array.concat(a.names, b.names)
    switch (nil, cons) {
    | (Ok(nil), Ok(cons)) => Variant({...b, names: names, nil: Some(nil), cons: Some(cons)})
    | (Ok(nil), Error(_)) => Variant({...b, names: names, nil: Some(nil)})
    | (Error(_), Ok(cons)) => Variant({...b, names: names, cons: Some(cons)})
    | (Error(_), Error(_)) => raise(MergeFail)
    }
  | (Wildcard(a) as a', Switch(b) as b') =>
    switch realignByKey(a.key, a', b.key, b') {
    | Some((a, b)) => merge(a, b)
    | None =>
      let wildcard = switch b.wildcard {
      | None => a.child
      | Some(b) => merge(a.child, b)
      }
      let cases = mergeAndKeepTestCasesIntoWildcard(~wildcard, b.cases)
      Switch({
        ...b,
        names: Array.concat(a.names, b.names),
        cases: cases,
        wildcard: Some(wildcard),
      })
    }
  /* Nests */
  | (Nest(a) as a', Nest(b) as b') =>
    assert (a.kind == b.kind)
    switch realignByKey(a.key, a', b.key, b') {
    | Some((a, b)) => merge(a, b)
    | None =>
      let child = merge(a.child, b.child)
      Nest({...a, names: Array.concat(a.names, b.names), child: child})
    }
  | (Nest(a) as a', Wildcard(b) as b') =>
    switch realignByKey(a.key, a', a.key, b') {
    | Some((a, b)) => merge(a, b)
    | None =>
      let child = expandWildcardAfterNest(a.child, ~wildcard=b.child, Z)
      Nest({...a, names: Array.concat(a.names, b.names), child: child})
    }
  /* Variants */
  | (Variant(a), Wildcard(b) as b') =>
    let nil = switch a.nil {
    | None => Ok(b.child)
    | Some(a) =>
      try {Ok(merge(a, b.child))} catch {
      | MergeFail => Error(MergeFail)
      }
    }
    let cons = switch a.cons {
    | None => Ok(b')
    | Some(a) =>
      try {Ok(merge(a, b'))} catch {
      | MergeFail => Error(MergeFail)
      }
    }
    let names = Array.concat(a.names, b.names)
    switch (nil, cons) {
    | (Ok(nil), Ok(cons)) => Variant({...a, names: names, nil: Some(nil), cons: Some(cons)})
    | (Ok(nil), Error(_)) => Variant({...a, names: names, nil: Some(nil)})
    | (Error(_), Ok(cons)) => Variant({...a, names: names, cons: Some(cons)})
    | (Error(_), Error(_)) => raise(MergeFail)
    }
  | (Variant(a), Variant(b)) =>
    assert (a.kind == b.kind)
    let nil = switch (a.nil, b.nil) {
    | (None, None) => None
    | (Some(x), None) | (None, Some(x)) => Some(x)
    | (Some(a), Some(b)) => Some(merge(a, b))
    }
    let cons = switch (a.cons, b.cons) {
    | (None, None) => None
    | (Some(x), None) | (None, Some(x)) => Some(x)
    | (Some(a), Some(b)) => Some(merge(a, b))
    }
    Variant({...a, nil: nil, cons: cons})
  /* Tests */
  | (Switch(a) as a', Wildcard(b) as b') =>
    switch realignByKey(a.key, a', b.key, b') {
    | Some((a, b)) => merge(a, b)
    | None =>
      let wildcard = switch a.wildcard {
      | None => b.child
      | Some(a) => merge(a, b.child)
      }
      let cases = expandWildcardIntoTestCases(a.cases, ~wildcard)
      Switch({
        ...a,
        names: Array.concat(a.names, b.names),
        cases: cases,
        wildcard: Some(wildcard),
      })
    }
  | (Switch(a) as a', Switch(b) as b') =>
    switch realignByKey(a.key, a', b.key, b') {
    | Some((a, b)) => merge(a, b)
    | None =>
      let wildcard = switch (a.wildcard, b.wildcard) {
      | (None, None) => Ok(None)
      | (Some(x), None) | (None, Some(x)) => Ok(Some(x))
      | (Some(a), Some(b)) =>
        try {Ok(Some(merge(a, b)))} catch {
        | MergeFail => Error(MergeFail)
        }
      }
      let bcases = switch wildcard {
      | Ok(Some(wildcard)) =>
        try {Ok(mergeAndKeepTestCasesIntoWildcard(~wildcard, b.cases))} catch {
        | MergeFail => Error(MergeFail)
        }
      | _ => Ok(b.cases)
      }
      switch (bcases, wildcard) {
      | (Ok(bcases), Ok(wildcard)) =>
        let cases = mergeTestCases(a.cases, bcases)
        let cases = switch wildcard {
        | None => cases
        | Some(wildcard) => expandWildcardIntoTestCases(cases, ~wildcard)
        }
        Switch({...a, cases: cases, wildcard: wildcard})
      | (Ok(bcases), Error(_)) =>
        let cases = mergeTestCases(a.cases, bcases)
        let cases = switch a.wildcard {
        | None => cases
        | Some(wildcard) => expandWildcardIntoTestCases(cases, ~wildcard)
        }
        Switch({...a, cases: cases})
      | (Error(_), Ok(wildcard)) => Switch({...a, wildcard: wildcard})
      | (Error(_), Error(_)) => raise(MergeFail)
      }
    }
  /* Failure cases */
  | (End(_), Variant(_))
  | (Variant(_), End(_))
  | (Switch(_), Nest(_) | Variant(_))
  | (Nest(_) | Variant(_), Switch(_))
  | (Nest(_), Variant(_))
  | (Variant(_), Nest(_))
  | (Leaf(_), Switch(_) | Wildcard(_) | End(_) | Nest(_) | Variant(_))
  | (Switch(_) | Wildcard(_) | End(_) | Nest(_) | Variant(_), Leaf(_)) =>
    assert false
  }

let rec fromQueue = (q: Queue.t<P.t>, bindings, exit) =>
  switch Queue.pop(q) {
  | None => Leaf({bindings: SetString.toArray(bindings), exit: exit})
  | Some(p) =>
    switch p {
    | PWildcard({key, name}) =>
      switch name {
      | "_" => Wildcard({names: [], key: key, child: fromQueue(q, bindings, exit)})
      | x => Wildcard({names: [x], key: key, child: fromQueue(q, SetString.add(bindings, x), exit)})
      }
    | PNest(key, kind) =>
      Nest({names: [], key: key, kind: kind, child: fromQueue(q, bindings, exit)})
    | PVariant(key, tag, nc) =>
      Variant({
        key: key,
        names: [],
        kind: tag,
        nil: switch nc {
        | PNil => Some(fromQueue(q, bindings, exit))
        | PCons => None
        },
        cons: switch nc {
        | PCons => Some(fromQueue(q, bindings, exit))
        | PNil => None
        },
      })
    | PEnd => End(fromQueue(q, bindings, exit))
    | PNull(_) => assert false
    | PSwitch(key, p) =>
      Switch({
        key: key,
        names: [],
        cases: {
          val: p,
          ifMatch: fromQueue(q, bindings, exit),
          nextCase: None,
        },
        wildcard: None,
      })
    }
  }

let rec fromTPat = (p, k, bindings, c) =>
  switch p {
  | TC.TypedPattern.TPat_Binding(name) =>
    switch name {
    | "_" => Wildcard({names: [], key: k, child: c(. bindings)})
    | x => Wildcard({names: [x], key: k, child: c(. SetString.add(bindings, x))})
    }
  | TPat_Variant(kind, Some(cons)) =>
    Variant({key: k, names: [], kind: kind, nil: None, cons: Some(fromTPat(cons, k, bindings, c))})
  | TPat_Variant(kind, None) =>
    Variant({
      key: k,
      names: [],
      kind: kind,
      nil: Some(c(. bindings)),
      cons: None,
    })
  | TPat_Const(x) =>
    Switch({
      key: k,
      names: [],
      cases: {
        val: TestPat.make(x),
        ifMatch: c(. bindings),
        nextCase: None,
      },
      wildcard: None,
    })
  | TPat_Tuple(a) =>
    Nest({
      key: k,
      names: [],
      kind: TTuple,
      child: fromArray(a, bindings, 0, (. bindings) => End(c(. bindings))),
    })
  | TPat_Record(a) =>
    Nest({
      key: k,
      names: [],
      kind: TRecord,
      child: fromKeyValues(a, bindings, 0, (. bindings) => End(c(. bindings))),
    })
  | TPat_Dict(a) =>
    Nest({
      key: k,
      names: [],
      kind: TDict,
      child: fromKeyValues(a, bindings, 0, (. bindings) => End(c(. bindings))),
    })
  }
and fromArray = (a, bindings, i, c) => {
  switch a[i] {
  | None => c(. bindings)
  | Some(p) => fromTPat(p, "", bindings, (. bindings) => fromArray(a, bindings, succ(i), c))
  }
}
and fromKeyValues = (a, bindings, i, c) => {
  switch a[i] {
  | None => c(. bindings)
  | Some((k, v)) => fromTPat(v, k, bindings, (. bindings) => fromKeyValues(a, bindings, succ(i), c))
  }
}

let fromArray = (a, ~exit) =>
  fromArray(a, SetString.empty, 0, (. bindings) => Leaf({
    bindings: SetString.toArray(bindings),
    exit: exit,
  }))

let fromNonEmpty = (ne, exit) => fromQueue(P.make(ne), SetString.empty, exit)

let merge = (a, b) =>
  try {merge(a, b)} catch {
  | MergeFail => a
  }

let makeCase2 = (hd, a, ~exit) => {
  let rec aux = (t, i) =>
    switch TC.NonEmpty2.get(a, i) {
    | None => t
    | Some(ps) =>
      let b = fromArray(TC.NonEmpty2.toArray(ps), ~exit)
      let t = merge(t, b)
      aux(t, succ(i))
    }
  aux(hd, 1)
}
let makeCaseOld = (t, a, exit) => {
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
  tree: tree,
  exits: array<T.Ast.nodes<'a>>,
}

let makeOld = (
  NonEmpty({patterns: NonEmpty(hd, tl), nodes}, casestl): NonEmpty.t<T.Ast.case<_>>,
) => {
  let exitq = Queue.make()
  Queue.add(exitq, nodes)
  let exit = Queue.size(exitq) - 1
  let hd = fromNonEmpty(hd, exit)
  let tree = makeCaseOld(hd, tl, exit)
  let rec aux = (tree, i) =>
    switch casestl[i] {
    | None => {tree: tree, exits: Queue.toArray(exitq)}
    | Some({patterns: NonEmpty(hd, tl), nodes}) =>
      Queue.add(exitq, nodes)
      let exit = Queue.size(exitq) - 1
      let hd = merge(tree, fromNonEmpty(hd, exit))
      let tree = makeCaseOld(hd, tl, exit)
      aux(tree, succ(i))
    }
  aux(tree, 0)
}

let make2 = cases => {
  let cases = TC.makeTypedCases(cases)
  let exitq = Queue.make()
  let hdcase = TC.NonEmpty2.hd(cases)
  Queue.add(exitq, hdcase.nodes)
  let exit = Queue.size(exitq) - 1
  let hdTree = TC.NonEmpty2.hd(hdcase.pats)->TC.NonEmpty2.toArray->fromArray(~exit)
  let tree = makeCase2(hdTree, hdcase.pats, ~exit)
  let rec aux = (tree, i) =>
    switch TC.NonEmpty2.get(cases, i) {
    | None => {tree: tree, exits: Queue.toArray(exitq)}
    | Some({pats, nodes}) =>
      Queue.add(exitq, nodes)
      let exit = Queue.size(exitq) - 1
      let hdTree = TC.NonEmpty2.hd(pats)->TC.NonEmpty2.toArray->fromArray(~exit)
      let tree = merge(tree, hdTree)
      let tree = makeCase2(tree, pats, ~exit)
      aux(tree, succ(i))
    }
  aux(tree, 1)
}
