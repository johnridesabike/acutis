module T = Acutis_Types
module Array = Belt.Array
module Ast = T.Ast
module Ast_Pattern = T.Ast_Pattern
module Debug2 = TypeChecker_Debug
module SetString = Belt.Set.String
module MapString = Belt.Map.String
module Queue = Belt.MutableQueue
module Option = Belt.Option
module SortArray = Belt.SortArray
module TC = TypeChecker
module TP = TypeChecker.TypedPattern

exception Exit = Debug.Exit
exception Exit2(string)

type nest = Tuple | Record | Dict

type rec tree<'a> =
  | Switch({
      idx: int,
      key: string,
      names: SetString.t,
      cases: switchcase<'a>,
      wildcard: option<tree<'a>>,
    })
  | Nest({
      idx: int,
      key: string,
      names: SetString.t,
      kind: nest,
      child: tree<tree<'a>>,
      wildcard: option<tree<'a>>,
    })
  | Construct({
      idx: int,
      key: string,
      names: SetString.t,
      kind: TP.construct,
      // nil is like a wildcard. It always points to the *next* node.
      nil: option<tree<'a>>,
      // cons is always either a Wildcard node or a Nest-Tuple node.
      cons: option<tree<'a>>,
    })
  | Wildcard({idx: int, key: string, names: SetString.t, child: tree<'a>})
  | End('a)

and switchcase<'a> = {
  val: TP.constant,
  ifMatch: tree<'a>,
  nextCase: option<switchcase<'a>>,
}

type leaf = {names: SetString.t, exit: int}

type t<'a> = {
  tree: tree<leaf>,
  exits: array<'a>,
}

type rec nat<_, _> = Z: nat<'z, 'z> | S(nat<'a, 'z>): nat<tree<'a>, 'z>

let rec reverseCases = (~tail=?, t) => {
  let tail = {...t, nextCase: tail}
  switch t.nextCase {
  | None => tail
  | Some(t) => reverseCases(t, ~tail)
  }
}

exception MergeFail

@raises(MergeFail)
let rec mergeTestCasesAux:
  type a. (
    ~init: switchcase<a>=?,
    switchcase<a>,
    TP.constant,
    tree<a>,
    nat<a, leaf>,
  ) => switchcase<a> =
  (~init=?, original, val, ifMatch, n) => {
    let cmp = TP.compareConst(val, original.val)
    if cmp < 0 {
      let tail = {val: val, ifMatch: ifMatch, nextCase: Some(original)}
      switch init {
      | None => tail
      | Some(init) => reverseCases(init, ~tail)
      }
    } else if cmp == 0 {
      let ifMatch = merge(original.ifMatch, ifMatch, n)
      let tail = {...original, ifMatch: ifMatch}
      switch init {
      | None => tail
      | Some(init) => reverseCases(init, ~tail)
      }
    } else {
      let init = {...original, nextCase: init}
      switch original.nextCase {
      | None => reverseCases(init, ~tail={val: val, ifMatch: ifMatch, nextCase: None})
      | Some(original) => mergeTestCasesAux(original, val, ifMatch, ~init, n)
      }
    }
  }

/* At least one merge must succeed. */
@raises(MergeFail)
and mergeTestCases:
  type a. (~init: switchcase<a>=?, switchcase<a>, switchcase<a>, nat<a, leaf>) => switchcase<a> =
  (~init=?, original, {val, ifMatch, nextCase}, n) => {
    let init = try {
      switch init {
      | None => Some(mergeTestCasesAux(original, val, ifMatch, n))
      | Some(init) => Some(mergeTestCasesAux(init, val, ifMatch, n))
      }
    } catch {
    | _MergeFail => init
    }
    switch (nextCase, init) {
    | (None, None) => raise(MergeFail)
    | (None, Some(x)) => x
    | (Some(b), init) => mergeTestCases(original, b, ~init?, n)
    }
  }

/* This only keeps successful mergers. */
@raises(MergeFail)
and mergeAndKeepTestCasesIntoWildcard:
  type a. (
    ~init: switchcase<a>=?,
    ~wildcard: tree<a>,
    switchcase<a>,
    nat<a, leaf>,
  ) => switchcase<a> =
  (~init=?, ~wildcard, t, n) => {
    let init = try {
      Some({...t, ifMatch: merge(wildcard, t.ifMatch, n), nextCase: init})
    } catch {
    | _MergeFail => init
    }
    switch (t.nextCase, init) {
    | (None, None) => raise(MergeFail)
    | (None, Some(init)) => reverseCases(init)
    | (Some(t), init) => mergeAndKeepTestCasesIntoWildcard(~wildcard, t, ~init?, n)
    }
  }

and expandWildcardAfterNest:
  type a b. (tree<a>, nat<a, leaf>, ~wildcard: tree<b>, nat<a, tree<b>>) => tree<a> =
  (a, na, ~wildcard, nb) =>
    switch (a, na, nb) {
    | (End(_), Z, _) => assert false
    | (End(a), S(n), Z) =>
      try {End(merge(a, wildcard, n))} catch {
      | _MergeFail => End(a)
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
      let rec aux = (~init=?, case) => {
        let ifMatch = expandWildcardAfterNest(case.ifMatch, na, ~wildcard, nb)
        let init = {...case, ifMatch: ifMatch, nextCase: init}
        switch case.nextCase {
        | None => Switch({...a, cases: reverseCases(init), wildcard: wildcard'})
        | Some(a) => aux(a, ~init)
        }
      }
      aux(a.cases)
    }

/* This only keeps successful mergers. */
@raises(MergeFail)
and mergeWildcardAfterNest:
  type a b. (~wildcard: tree<a>, nat<b, tree<a>>, tree<b>, nat<b, leaf>) => tree<b> =
  (~wildcard, na, b, nb) =>
    switch (na, b, nb) {
    | (Z, End(b), S(n)) => End(merge(wildcard, b, n))
    | (S(na), End(b), S(nb)) => End(mergeWildcardAfterNest(~wildcard, na, b, nb))
    | (na, Nest(b), nb) =>
      Nest({...b, child: mergeWildcardAfterNest(~wildcard, S(na), b.child, S(nb))})
    | (na, Construct(b), nb) =>
      let nil = switch b.nil {
      | None => Ok(None)
      | Some(child) =>
        try {Ok(Some(mergeWildcardAfterNest(~wildcard, na, child, nb)))} catch {
        | _MergeFail => Error(MergeFail)
        }
      }
      let cons = switch b.cons {
      | None => Ok(None)
      | Some(child) =>
        try {Ok(Some(mergeWildcardAfterNest(~wildcard, na, child, nb)))} catch {
        | _MergeFail => Error(MergeFail)
        }
      }
      switch (nil, cons) {
      | (Ok(nil), Ok(cons)) => Construct({...b, nil: nil, cons: cons})
      | (Ok(nil), Error(_)) => Construct({...b, nil: nil})
      | (Error(_), Ok(cons)) => Construct({...b, cons: cons})
      | (Error(_), Error(_)) => raise(MergeFail)
      }
    | (na, Wildcard(b), nb) =>
      Wildcard({...b, child: mergeWildcardAfterNest(~wildcard, na, b.child, nb)})
    | (na, Switch(b), nb) =>
      let wildcard' = switch b.wildcard {
      | None => None
      | Some(b) => Some(mergeWildcardAfterNest(~wildcard, na, b, nb))
      }

      @raises(MergeFail)
      let rec aux = (~init=?, case) => {
        let init = try {
          let ifMatch = mergeWildcardAfterNest(~wildcard, na, case.ifMatch, nb)
          Some({...case, ifMatch: ifMatch, nextCase: init})
        } catch {
        | _MergeFail => init
        }
        switch (case.nextCase, init) {
        | (None, None) => raise(MergeFail)
        | (None, Some(init)) => Switch({...b, cases: reverseCases(init), wildcard: wildcard'})
        | (Some(a), init) => aux(a, ~init?)
        }
      }
      aux(b.cases)
    }

and expandWildcardIntoTestCases:
  type a. (
    ~init: switchcase<a>=?,
    switchcase<a>,
    ~wildcard: tree<a>,
    nat<a, leaf>,
  ) => switchcase<a> =
  (~init=?, t, ~wildcard, n) => {
    let ifMatch = try {merge(t.ifMatch, wildcard, n)} catch {
    | _MergeFail => t.ifMatch
    }
    let init = {...t, ifMatch: ifMatch, nextCase: init}
    switch t.nextCase {
    | None => reverseCases(init)
    | Some(t) => expandWildcardIntoTestCases(~wildcard, t, ~init, n)
    }
  }

@raises(MergeFail)
and merge:
  type a. (tree<a>, tree<a>, nat<a, leaf>) => tree<a> =
  (a, b, n) =>
    switch (a, b, n) {
    /* Ends */
    // Root-level End nodes cannot merge.
    | (End(_), End(_), Z) => raise(MergeFail)
    | (End(a), End(b), S(n)) => End(merge(a, b, n))
    /* Wildcards */
    | (Wildcard(a), Wildcard(b), n) =>
      Wildcard({...a, names: SetString.union(a.names, b.names), child: merge(a.child, b.child, n)})
    | (Wildcard(a), Nest(b), n) =>
      let child = try {Ok(mergeWildcardAfterNest(~wildcard=a.child, Z, b.child, S(n)))} catch {
      | _MergeFail => Error(MergeFail)
      }
      let wildcard = switch b.wildcard {
      | None => Ok(Some(a.child))
      | Some(b) =>
        try {Ok(Some(merge(a.child, b, n)))} catch {
        | _MergeFail => Error(MergeFail)
        }
      }
      let names = SetString.union(a.names, b.names)
      switch (child, wildcard) {
      | (Ok(child), Ok(wildcard)) => Nest({...b, names: names, child: child, wildcard: wildcard})
      | (Ok(child), Error(_)) => Nest({...b, names: names, child: child})
      | (Error(_), Ok(wildcard)) => Nest({...b, names: names, wildcard: wildcard})
      | (Error(_), Error(_)) => raise(MergeFail)
      }
    | (Wildcard(a) as a', Construct(b), n) =>
      let nil = switch b.nil {
      | None => Ok(a.child)
      | Some(b) =>
        try {Ok(merge(a.child, b, n))} catch {
        | _MergeFail => Error(MergeFail)
        }
      }
      let cons = switch b.cons {
      | None => Ok(a')
      | Some(b) =>
        try {Ok(merge(a', b, n))} catch {
        | _MergeFail => Error(MergeFail)
        }
      }
      let names = SetString.union(a.names, b.names)
      switch (nil, cons) {
      | (Ok(nil), Ok(cons)) => Construct({...b, names: names, nil: Some(nil), cons: Some(cons)})
      | (Ok(nil), Error(_)) => Construct({...b, names: names, nil: Some(nil)})
      | (Error(_), Ok(cons)) => Construct({...b, names: names, cons: Some(cons)})
      | (Error(_), Error(_)) => raise(MergeFail)
      }
    | (Wildcard(a), Switch(b), n) =>
      let wildcard = switch b.wildcard {
      | None => a.child
      | Some(b) => merge(a.child, b, n)
      }
      let cases = mergeAndKeepTestCasesIntoWildcard(~wildcard, b.cases, n)
      let names = SetString.union(a.names, b.names)
      Switch({...b, names: names, cases: cases, wildcard: Some(wildcard)})
    /* Nests */
    | (Nest(a), Nest(b), n) =>
      assert (a.kind == b.kind)
      let child = merge(a.child, b.child, S(n))
      Nest({...a, names: SetString.union(a.names, b.names), child: child})
    | (Nest(a), Wildcard(b), n) =>
      let child = try {Ok(expandWildcardAfterNest(a.child, S(n), ~wildcard=b.child, Z))} catch {
      | _MergeFail => Error(MergeFail)
      }
      let wildcard = switch a.wildcard {
      | None => Ok(Some(b.child))
      | Some(a) =>
        try {Ok(Some(merge(a, b.child, n)))} catch {
        | _MergeFail => Error(MergeFail)
        }
      }
      let names = SetString.union(a.names, b.names)
      switch (child, wildcard) {
      | (Ok(child), Ok(wildcard)) => Nest({...a, names: names, child: child, wildcard: wildcard})
      | (Ok(child), Error(_)) => Nest({...a, names: names, child: child})
      | (Error(_), Ok(wildcard)) => Nest({...a, names: names, wildcard: wildcard})
      | (Error(_), Error(_)) => raise(MergeFail)
      }
    /* Variants */
    | (Construct(a), Wildcard(b) as b', n) =>
      let nil = switch a.nil {
      | None => Ok(b.child)
      | Some(a) =>
        try {Ok(merge(a, b.child, n))} catch {
        | _MergeFail => Error(MergeFail)
        }
      }
      let cons = switch a.cons {
      | None => Ok(b')
      | Some(a) =>
        try {Ok(merge(a, b', n))} catch {
        | _MergeFail => Error(MergeFail)
        }
      }
      let names = SetString.union(a.names, b.names)
      switch (nil, cons) {
      | (Ok(nil), Ok(cons)) => Construct({...a, names: names, nil: Some(nil), cons: Some(cons)})
      | (Ok(nil), Error(_)) => Construct({...a, names: names, nil: Some(nil)})
      | (Error(_), Ok(cons)) => Construct({...a, names: names, cons: Some(cons)})
      | (Error(_), Error(_)) => raise(MergeFail)
      }
    | (Construct(a), Construct(b), n) =>
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
      Construct({...a, nil: nil, cons: cons})
    /* Tests */
    | (Switch(a), Wildcard(b), n) =>
      let wildcard = switch a.wildcard {
      | None => b.child
      | Some(a) => merge(a, b.child, n)
      }
      let cases = expandWildcardIntoTestCases(a.cases, ~wildcard, n)
      let names = SetString.union(a.names, b.names)
      Switch({...a, names: names, cases: cases, wildcard: Some(wildcard)})
    | (Switch(a), Switch(b), n) =>
      let wildcard = switch (a.wildcard, b.wildcard) {
      | (None, None) => Ok(None)
      | (Some(x), None) | (None, Some(x)) => Ok(Some(x))
      | (Some(a), Some(b)) =>
        try {Ok(Some(merge(a, b, n)))} catch {
        | _MergeFail => Error(MergeFail)
        }
      }
      let bcases = switch wildcard {
      | Ok(Some(wildcard)) =>
        try {Ok(mergeAndKeepTestCasesIntoWildcard(~wildcard, b.cases, n))} catch {
        | _MergeFail => Error(MergeFail)
        }
      | _ => Ok(b.cases)
      }
      switch (bcases, wildcard) {
      | (Ok(bcases), Ok(wildcard)) =>
        let cases = mergeTestCases(a.cases, bcases, n)
        let cases = switch wildcard {
        | None => cases
        | Some(wildcard) => expandWildcardIntoTestCases(cases, ~wildcard, n)
        }
        Switch({...a, cases: cases, wildcard: wildcard})
      | (Ok(bcases), Error(_)) =>
        let cases = mergeTestCases(a.cases, bcases, n)
        let cases = switch a.wildcard {
        | None => cases
        | Some(wildcard) => expandWildcardIntoTestCases(cases, ~wildcard, n)
        }
        Switch({...a, cases: cases})
      | (Error(_), Ok(wildcard)) => Switch({...a, wildcard: wildcard})
      | (Error(_), Error(_)) => raise(MergeFail)
      }
    /* Failure cases */
    | (Switch(_), Nest(_) | Construct(_) | End(_), _)
    | (Construct(_), Switch(_) | Nest(_) | End(_), _)
    | (Nest(_), Switch(_) | Construct(_) | End(_), _)
    | (Wildcard(_), End(_), _)
    | (End(_), Switch(_) | Nest(_) | Construct(_) | Wildcard(_), _) =>
      assert false
    }

type continue<'a> = (. SetString.t) => tree<'a>

let rec fromTPat: 'a. (_, _, _, _, continue<'a>) => tree<'a> = (p, i, k, b, c) =>
  switch p {
  | TP.TPat_Var(name) =>
    switch name {
    | "_" => Wildcard({names: SetString.empty, idx: i, key: k, child: c(. b)})
    | x =>
      Wildcard({
        names: SetString.add(SetString.empty, x),
        idx: i,
        key: k,
        child: c(. SetString.add(b, x)),
      })
    }
  | TPat_Construct(kind, Some(cons)) =>
    Construct({
      idx: i,
      key: k,
      names: SetString.empty,
      kind: kind,
      nil: None,
      cons: Some(fromTPat(cons, i, k, b, c)),
    })
  | TPat_Construct(kind, None) =>
    Construct({idx: i, key: k, names: SetString.empty, kind: kind, nil: Some(c(. b)), cons: None})
  | TPat_Const(val) =>
    Switch({
      idx: i,
      key: k,
      names: SetString.empty,
      cases: {val: val, ifMatch: c(. b), nextCase: None},
      wildcard: None,
    })
  | TPat_Tuple(a) =>
    let child = fromArray(a, b, 0, (. b) => End(c(. b)))
    Nest({idx: i, key: k, names: SetString.empty, kind: Tuple, child: child, wildcard: None})
  | TPat_Record(a) =>
    let child = fromKeyValues(a, b, 0, (. b) => End(c(. b)))
    Nest({idx: i, key: k, names: SetString.empty, kind: Record, child: child, wildcard: None})
  | TPat_Dict(a) =>
    let child = fromKeyValues(a, b, 0, (. b) => End(c(. b)))
    Nest({idx: i, key: k, names: SetString.empty, kind: Dict, child: child, wildcard: None})
  }

and fromArray: 'a. (_, _, _, continue<'a>) => tree<'a> = (a, b, i, c) =>
  switch a[i] {
  | None => c(. b)
  | Some(p) => fromTPat(p, i, "", b, (. b) => fromArray(a, b, succ(i), c))
  }

and fromKeyValues: 'a. (_, _, _, continue<'a>) => tree<'a> = (a, b, i, c) =>
  switch a[i] {
  | None => c(. b)
  | Some((k, v)) => fromTPat(v, i, k, b, (. b) => fromKeyValues(a, b, succ(i), c))
  }

let fromArray = (a, ~exit) =>
  fromArray(a, SetString.empty, 0, (. names) => End({names: names, exit: exit}))

let merge = (a, b) =>
  try {merge(a, b, Z)} catch {
  | _MergeFail => a
  }

let makeCase = (hd, a, ~exit) => {
  let rec aux = (t, i) =>
    switch NonEmpty.get(a, i) {
    | None => t
    | Some(ps) =>
      let b = fromArray(NonEmpty.toArray(ps), ~exit)
      let t = merge(t, b)
      aux(t, succ(i))
    }
  aux(hd, 1)
}

let make = cases => {
  let cases = TC.makeTypedCases(cases)
  let exitq = Queue.make()
  let hdcase = NonEmpty.hd(cases)
  Queue.add(exitq, hdcase.nodes)
  let exit = Queue.size(exitq) - 1
  let hdTree = NonEmpty.hd(hdcase.pats)->NonEmpty.toArray->fromArray(~exit)
  let tree = makeCase(hdTree, hdcase.pats, ~exit)
  let rec aux = (tree, i) =>
    switch NonEmpty.get(cases, i) {
    | None => {tree: tree, exits: Queue.toArray(exitq)}
    | Some({pats, nodes}) =>
      Queue.add(exitq, nodes)
      let exit = Queue.size(exitq) - 1
      let hdTree = NonEmpty.hd(pats)->NonEmpty.toArray->fromArray(~exit)
      let tree = merge(tree, hdTree)
      let tree = makeCase(tree, pats, ~exit)
      aux(tree, succ(i))
    }
  aux(tree, 1)
}

module ParMatch = {
  let makeRefutation = x =>
    switch x {
    | TP.TPat_Bool(_) => TP.TPat_Bool(false)
    | TPat_Int(_) => TPat_Int(0)
    | TPat_String(_) => TPat_String("a")
    | TPat_Float(_) => TPat_Float(0.0)
    }

  let succ = x =>
    switch x {
    | TP.TPat_Bool(false) => Some(TP.TPat_Bool(true))
    | TPat_Bool(true) => None
    | TPat_Int(i) => Some(TPat_Int(succ(i)))
    | TPat_String(s) => Some(TPat_String(s ++ "a"))
    | TPat_Float(f) => Some(TPat_Float(f +. 1.0))
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
    let var = switch key {
    | "" => "_"
    | k => k
    }
    {flag: flag, pats: list{(key, TPat_Var(var)), ...pats}, next: next}
  }

  let rec check: 'a. tree<'a> => t<'a> = tree =>
    switch tree {
    | End(next) => {flag: Exhaustive, pats: list{}, next: next}
    | Wildcard({key, child, _}) => exhaustive(key, check(child))
    | Nest({key, kind, child, wildcard, _}) =>
      switch wildcard {
      | Some(child) => exhaustive(key, check(child))
      | None =>
        switch check(child) {
        | {flag: Exhaustive, next, _} => exhaustive(key, check(next))
        | {flag: Partial, pats, next} =>
          let nest = switch kind {
          | Tuple => TP.TPat_Tuple(toArray(pats))
          | Record => TPat_Record(toKeyValues(pats))
          | Dict => TPat_Dict(toKeyValues(pats))
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
          | list{_, ...pats} => list{(key, TPat_Construct(kind, None)), ...pats}
          | _ => assert false
          },
          next: next,
        }
      | (Some(nil), None) =>
        let {pats, next, _} = check(nil)
        {
          flag: Partial,
          pats: list{(key, TPat_Construct(kind, Some(TPat_Var("_")))), ...pats},
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
                (key, TP.TPat_Construct(kind, Some(cons))),
                ...pats,
              }
            | _ => assert false
            }
            {flag: Partial, pats: pats, next: next}
          }
        | {flag: Partial, pats, next} => {
            flag: Partial,
            pats: list{(key, TPat_Construct(kind, None)), ...pats},
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
        let rec aux = (refute, {val, ifMatch, nextCase}) =>
          switch check(ifMatch) {
          | {flag: Partial, pats, next} => {
              flag: Partial,
              pats: list{(key, TPat_Const(val)), ...pats},
              next: next,
            }
          | {flag: Exhaustive, pats, next} =>
            if TP.eqConst(refute, val) {
              switch succ(refute) {
              | None => exhaustive(key, check(ifMatch))
              | Some(refute) =>
                switch nextCase {
                | None => {
                    flag: Partial,
                    pats: list{(key, TPat_Const(refute)), ...pats},
                    next: next,
                  }
                | Some(case) => aux(refute, case)
                }
              }
            } else {
              {
                flag: Partial,
                pats: list{(key, TPat_Const(refute)), ...pats},
                next: next,
              }
            }
          }
        aux(refute, cases)
      }
    }

  let check = tree => {
    switch check(tree) {
    | {flag: Exhaustive, _} => None
    | {flag: Partial, pats, _} => Some(toArray(pats))
    }
  }
}
