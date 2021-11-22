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
module TP = TypeChecker.Pattern

type nest = Tuple | Record | Dict

type rec tree<'a> =
  | Switch({
      idx: int,
      key: string,
      ids: SetInt.t,
      cases: switchcase<'a>,
      wildcard: option<tree<'a>>,
    })
  | Nest({
      idx: int,
      key: string,
      ids: SetInt.t,
      kind: nest,
      child: tree<tree<'a>>,
      wildcard: option<tree<'a>>,
    })
  | Construct({
      idx: int,
      key: string,
      ids: SetInt.t,
      kind: TP.construct,
      // nil is like a wildcard. It always points to the *next* node.
      nil: option<tree<'a>>,
      // cons is always either a Wildcard node or a Nest-Tuple node.
      cons: option<tree<'a>>,
    })
  | Wildcard({idx: int, key: string, ids: SetInt.t, child: tree<'a>})
  | End('a)

and switchcase<'a> = {
  val: TP.constant,
  ifMatch: tree<'a>,
  nextCase: option<switchcase<'a>>,
}

type leaf = {names: MapString.t<int>, exit: int}

type t<'a> = {
  loc: Acutis_Types.loc,
  tree: tree<leaf>,
  exits: array<'a>,
}

type rec nat<_, _> =
  | Z: nat<'z, 'z>
  | S(nat<'a, 'z>): nat<tree<'a>, 'z>

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
    | MergeFail => init
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
        | MergeFail => Error(MergeFail)
        }
      }
      let cons = switch b.cons {
      | None => Ok(None)
      | Some(child) =>
        try {Ok(Some(mergeWildcardAfterNest(~wildcard, na, child, nb)))} catch {
        | MergeFail => Error(MergeFail)
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
        | MergeFail => init
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
  (~init=?, c, ~wildcard, n) => {
    let ifMatch = try {merge(c.ifMatch, wildcard, n)} catch {
    | MergeFail => c.ifMatch
    }
    let init = {...c, ifMatch: ifMatch, nextCase: init}
    switch c.nextCase {
    | None => reverseCases(init)
    | Some(c) => expandWildcardIntoTestCases(~init, c, ~wildcard, n)
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
      Wildcard({...a, ids: SetInt.union(a.ids, b.ids), child: merge(a.child, b.child, n)})
    | (Wildcard(a), Nest(b), n) =>
      let wildcard = switch b.wildcard {
      | None => a.child
      | Some(b) => merge(a.child, b, n)
      }
      let child = mergeWildcardAfterNest(~wildcard=a.child, Z, b.child, S(n))
      let ids = SetInt.union(a.ids, b.ids)
      Nest({...b, ids: ids, child: child, wildcard: Some(wildcard)})
    | (Wildcard(a) as a', Construct(b), n) =>
      let nil = switch b.nil {
      | None => Ok(a.child)
      | Some(b) =>
        try {Ok(merge(a.child, b, n))} catch {
        | MergeFail => Error(MergeFail)
        }
      }
      let cons = switch b.cons {
      | None => Ok(a')
      | Some(b) =>
        try {Ok(merge(a', b, n))} catch {
        | MergeFail => Error(MergeFail)
        }
      }
      let ids = SetInt.union(a.ids, b.ids)
      switch (nil, cons) {
      | (Ok(nil), Ok(cons)) => Construct({...b, ids: ids, nil: Some(nil), cons: Some(cons)})
      | (Ok(nil), Error(_)) => Construct({...b, ids: ids, nil: Some(nil)})
      | (Error(_), Ok(cons)) => Construct({...b, ids: ids, cons: Some(cons)})
      | (Error(_), Error(_)) => raise(MergeFail)
      }
    | (Wildcard(a), Switch(b), n) =>
      let cases = mergeAndKeepTestCasesIntoWildcard(~wildcard=a.child, b.cases, n)
      let wildcard = switch b.wildcard {
      | None => a.child
      | Some(b) => merge(a.child, b, n)
      }
      let ids = SetInt.union(a.ids, b.ids)
      Switch({...b, ids: ids, cases: cases, wildcard: Some(wildcard)})
    /* Nests */
    | (Nest(a), Nest(b), n) =>
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
    | (Nest(a), Wildcard(b), n) =>
      let child = expandWildcardAfterNest(a.child, S(n), ~wildcard=b.child, Z)
      let wildcard = switch a.wildcard {
      | None => b.child
      | Some(a) => merge(a, b.child, n)
      }
      let ids = SetInt.union(a.ids, b.ids)
      Nest({...a, ids: ids, child: child, wildcard: Some(wildcard)})
    /* Variants */
    | (Construct(a), Wildcard(b) as b', n) =>
      let nil = switch a.nil {
      | None => Ok(b.child)
      | Some(a) =>
        try {Ok(merge(a, b.child, n))} catch {
        | MergeFail => Error(MergeFail)
        }
      }
      let cons = switch a.cons {
      | None => Ok(b')
      | Some(a) =>
        try {Ok(merge(a, b', n))} catch {
        | MergeFail => Error(MergeFail)
        }
      }
      let ids = SetInt.union(a.ids, b.ids)
      switch (nil, cons) {
      | (Ok(nil), Ok(cons)) => Construct({...a, ids: ids, nil: Some(nil), cons: Some(cons)})
      | (Ok(nil), Error(_)) => Construct({...a, ids: ids, nil: Some(nil)})
      | (Error(_), Ok(cons)) => Construct({...a, ids: ids, cons: Some(cons)})
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
      let ids = SetInt.union(a.ids, b.ids)
      Construct({...a, ids: ids, nil: nil, cons: cons})
    /* Tests */
    | (Switch(a), Wildcard(b), n) =>
      let wildcard = switch a.wildcard {
      | None => b.child
      | Some(a) => merge(a, b.child, n)
      }
      let cases = expandWildcardIntoTestCases(a.cases, ~wildcard=b.child, n)
      // let cases = expandWildcardIntoTestCases(a.cases, ~wildcard, n)
      let ids = SetInt.union(a.ids, b.ids)
      Switch({...a, ids: ids, cases: cases, wildcard: Some(wildcard)})
    | (Switch(a), Switch(b), n) =>
      let wildcard = switch (a.wildcard, b.wildcard) {
      | (None, None) => Ok(None)
      | (Some(x), None) | (None, Some(x)) => Ok(Some(x))
      | (Some(a), Some(b)) =>
        try {Ok(Some(merge(a, b, n)))} catch {
        | MergeFail => Error(MergeFail)
        }
      }
      let cases = {
        let bcases = switch a.wildcard {
        | Some(wildcard) => mergeAndKeepTestCasesIntoWildcard(~wildcard, b.cases, n)
        | None => b.cases
        }
        mergeTestCases(a.cases, bcases, n)
      }
      let cases = switch b.wildcard {
      | None => cases
      | Some(wildcard) => expandWildcardIntoTestCases(cases, ~wildcard, n)
      }
      let ids = SetInt.union(a.ids, b.ids)
      switch wildcard {
      | Ok(wildcard) => Switch({...a, ids: ids, cases: cases, wildcard: wildcard})
      | Error(_) => Switch({...a, ids: ids, cases: cases})
      }
    /* Failure cases */
    | (Switch(_), Nest(_) | Construct(_) | End(_), _)
    | (Construct(_), Switch(_) | Nest(_) | End(_), _)
    | (Nest(_), Switch(_) | Construct(_) | End(_), _)
    | (Wildcard(_), End(_), _)
    | (End(_), Switch(_) | Nest(_) | Construct(_) | Wildcard(_), _) =>
      assert false
    }

type continue<'a> = (. MapString.t<int>) => tree<'a>

let rec fromTPat: 'a. (_, _, _, _, ~name: _, continue<'a>) => tree<'a> = (p, i, key, b, ~name, k) =>
  switch p {
  | TP.TPat_Any(_) => Wildcard({ids: SetInt.empty, idx: i, key: key, child: k(. b)})
  | TPat_Var(Loc(id), x) | TPat_OptionalVar(Loc(id), x) =>
    if MapString.has(b, x) {
      raise(Debug.Exit(Debug2.nameBoundMultipleTimes(~binding=x, ~loc=Loc(id), ~name)))
    }
    Wildcard({
      ids: SetInt.add(SetInt.empty, id),
      idx: i,
      key: key,
      child: k(. MapString.set(b, x, id)),
    })
  | TPat_Construct(_, kind, Some(cons)) =>
    Construct({
      idx: i,
      key: key,
      ids: SetInt.empty,
      kind: kind,
      nil: None,
      cons: Some(fromTPat(cons, i, key, b, k, ~name)),
    })
  | TPat_Construct(_, kind, None) =>
    Construct({idx: i, key: key, ids: SetInt.empty, kind: kind, nil: Some(k(. b)), cons: None})
  | TPat_Const(_, val) =>
    Switch({
      idx: i,
      key: key,
      ids: SetInt.empty,
      cases: {val: val, ifMatch: k(. b), nextCase: None},
      wildcard: None,
    })
  | TPat_Tuple(_, a) =>
    let child = fromArray(a, b, 0, ~name, (. b) => End(k(. b)))
    Nest({idx: i, key: key, ids: SetInt.empty, kind: Tuple, child: child, wildcard: None})
  | TPat_Record(_, a) =>
    let child = fromKeyValues(a, b, 0, ~name, (. b) => End(k(. b)))
    Nest({idx: i, key: key, ids: SetInt.empty, kind: Record, child: child, wildcard: None})
  | TPat_Dict(_, a) =>
    let child = fromKeyValues(a, b, 0, ~name, (. b) => End(k(. b)))
    Nest({idx: i, key: key, ids: SetInt.empty, kind: Dict, child: child, wildcard: None})
  }

and fromArray: 'a. (_, _, _, ~name: _, continue<'a>) => tree<'a> = (a, b, i, ~name, k) =>
  switch a[i] {
  | None => k(. b)
  | Some(p) => fromTPat(p, i, "", b, ~name, (. b) => fromArray(a, b, succ(i), ~name, k))
  }

and fromKeyValues: 'a. (_, _, _, ~name: _, continue<'a>) => tree<'a> = (a, b, i, ~name, k) =>
  switch a[i] {
  | None => k(. b)
  | Some((key, v)) => fromTPat(v, i, key, b, ~name, (. b) => fromKeyValues(a, b, succ(i), ~name, k))
  }

let fromArray = (~exit, ~name, a) =>
  fromArray(a, MapString.empty, 0, ~name, (. names) => End({names: names, exit: exit}))

let merge = (a, b) =>
  switch merge(a, b, Z) {
  | t => Some(t)
  | exception MergeFail => None
  }

let makeCase = (hd, a, ~exit, ~name) => {
  let rec aux = (t, i) =>
    switch NonEmpty.get(a, i) {
    | None => Ok(t)
    | Some(ps) =>
      let b = fromArray(NonEmpty.toArray(ps), ~exit, ~name)
      switch merge(t, b) {
      | Some(t) => aux(t, succ(i))
      | None => Error(Debug2.unusedCase(ps, ~f=TP.toString))
      }
    }
  aux(hd, 1)
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
    let pat = switch key {
    | "" => TP.TPat_Any(Loc(0))
    | k => TPat_Var(Loc(0), k)
    }
    {flag: flag, pats: list{(key, pat), ...pats}, next: next}
  }

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
          | Tuple => TP.TPat_Tuple(Loc(0), toArray(pats))
          | Record => TPat_Record(Loc(0), toKeyValues(pats))
          | Dict => TPat_Dict(Loc(0), toKeyValues(pats))
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
          | list{_, ...pats} => list{(key, TPat_Construct(Loc(0), kind, None)), ...pats}
          | _ => assert false
          },
          next: next,
        }
      | (Some(nil), None) =>
        let {pats, next, _} = check(nil)
        {
          flag: Partial,
          pats: list{(key, TPat_Construct(Loc(0), kind, Some(TPat_Any(Loc(0))))), ...pats},
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
                (key, TP.TPat_Construct(Loc(0), kind, Some(cons))),
                ...pats,
              }
            | _ => assert false
            }
            {flag: Partial, pats: pats, next: next}
          }
        | {flag: Partial, pats, next} => {
            flag: Partial,
            pats: list{(key, TP.TPat_Construct(Loc(0), kind, None)), ...pats},
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
              pats: list{(key, TPat_Const(Loc(0), val)), ...pats},
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
                    pats: list{(key, TPat_Const(Loc(0), refute)), ...pats},
                    next: next,
                  }
                | Some(case) => aux(refute, case)
                }
              }
            } else {
              {
                flag: Partial,
                pats: list{(key, TPat_Const(Loc(0), refute)), ...pats},
                next: next,
              }
            }
          }
        aux(refute, cases)
      }
    }

  let toString = l => toArray(l)->Array.joinWith(", ", TP.toString)

  let check = tree => {
    switch check(tree) {
    | {flag: Exhaustive, _} => Ok(tree)
    | {flag: Partial, pats, _} => Error(Debug2.partialMatch(pats, ~f=toString, ~loc=Loc(0)))
    }
  }
}

let make = (~loc, ~name, cases: NonEmpty.t<TypeChecker.Ast.case<_>>) => {
  let exitq = Queue.make()
  let hdcase = NonEmpty.hd(cases)
  Queue.add(exitq, hdcase.nodes)
  let exit = Queue.size(exitq) - 1
  let hdTree = NonEmpty.hd(hdcase.pats)->NonEmpty.toArray->fromArray(~exit, ~name)
  switch makeCase(hdTree, hdcase.pats, ~name, ~exit) {
  | Error(_) as e => e
  | Ok(tree) =>
    let rec aux = (tree, i) =>
      switch NonEmpty.get(cases, i) {
      | None => Ok({loc: loc, tree: tree, exits: Queue.toArray(exitq)})
      | Some({pats, nodes}) =>
        Queue.add(exitq, nodes)
        let exit = Queue.size(exitq) - 1
        let hdTree = NonEmpty.hd(pats)->NonEmpty.toArray->fromArray(~exit, ~name)
        switch merge(tree, hdTree) {
        | Some(tree) =>
          switch makeCase(tree, pats, ~exit, ~name) {
          | Ok(tree) => aux(tree, succ(i))
          | Error(_) as e => e
          }
        | None => Error(Debug2.unusedCase(NonEmpty.hd(pats), ~f=TP.toString))
        }
      }
    aux(tree, 1)
  }
}
