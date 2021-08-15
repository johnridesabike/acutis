module T = Acutis_Types
module Array = Belt.Array
module Ast = T.Ast
module Ast_Pattern = T.Ast_Pattern
module Debug2 = TypeChecker_Debug
module MapString = Belt.Map.String
module NonEmpty = T.NonEmpty
module Queue = Belt.MutableQueue
module Option = Belt.Option

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

  let make = x =>
    switch x {
    | #Null(_) => TNull
    | #False(_) => TFalse
    | #True(_) => TTrue
    | #String(_, s) => TString(s)
    | #Int(_, i) => TInt(i)
    | #Float(_, f) => TFloat(f)
    }

  let eq = (a, b) =>
    switch (a, b) {
    | (TString(a), TString(b)) => a == b
    | (TFloat(a), TFloat(b)) => a == b
    | (TInt(a), TInt(b)) => a == b
    | (TTrue, TTrue) | (TFalse, TFalse) | (TNull, TNull) => true
    | _ => false
    }

  let compare = (a, b) =>
    switch (a, b) {
    | (TString(a), TString(b)) => compare(a, b)
    | (TFloat(a), TFloat(b)) => compare(a, b)
    | (TInt(a), TInt(b)) => compare(a, b)
    | (TTrue, TFalse) => 1
    | (TFalse, TTrue) => -1
    | (TTrue, TTrue) | (TFalse, TFalse) | (TNull, TNull) => 0
    | _ => assert false
    }
}

// TODO: track wildcard names
type rec tree =
  | Test({tests: testcase, default: option<tree>})
  | Wildcard(tree)
  | Tuple({contents: tree, default: option<tree>})
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
  | _ => init
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
and mergeAndKeepChildIntoTestCases = (~init=?, a, b) => {
  let init = try {
    Some({...a, ifmatch: merge(a.ifmatch, b), ifnomatch: init})
  } catch {
  | _ => init
  }
  switch (a.ifnomatch, init) {
  | (None, Some(init)) => reverseTestCases(init)
  | (None, None) => raise(MergeFail)
  | (Some(a), init) => mergeAndKeepChildIntoTestCases(a, b, ~init?)
  }
}
and expandChildIntoTestCases = (~init=?, a, b) => {
  let ifmatch = try {merge(a.ifmatch, b)} catch {
  | _ => a.ifmatch
  }
  let init = {...a, ifmatch: ifmatch, ifnomatch: init}
  switch a.ifnomatch {
  | None => reverseTestCases(init)
  | Some(a) => expandChildIntoTestCases(a, b, ~init)
  }
}
/* This only keeps successful mergers. */
@raises(MergeFail)
and mergeAndKeepChildAfterNest = (a, b, i) =>
  switch a {
  | End(a) if i == 0 => merge(a, b)
  | End(a) => End(mergeAndKeepChildAfterNest(a, b, pred(i)))
  | Wildcard(a) => Wildcard(mergeAndKeepChildAfterNest(a, b, i))
  | Tuple(a) =>
    let contents = try {Ok(mergeAndKeepChildAfterNest(a.contents, b, succ(i)))} catch {
    | e => Error(e)
    }
    let default = switch a.default {
    | None => Ok(None)
    | Some(default) =>
      try {Ok(Some(mergeAndKeepChildAfterNest(default, b, i)))} catch {
      | e => Error(e)
      }
    }
    switch (contents, default) {
    | (Ok(contents), Ok(default)) => Tuple({contents: contents, default: default})
    | (Ok(contents), Error(_)) => Tuple({...a, contents: contents})
    | (Error(_), Ok(default)) => Tuple({...a, default: default})
    | (Error(_), Error(_)) => raise(MergeFail)
    }
  | Test({tests, default}) =>
    let default' = switch default {
    | None => Ok(None)
    | Some(default) =>
      try {Ok(Some(mergeAndKeepChildAfterNest(default, b, i)))} catch {
      | e => Error(e)
      }
    }

    @raises(MergeFail)
    let rec aux = (~init=?, a) => {
      let init = try {
        Some({...a, ifmatch: mergeAndKeepChildAfterNest(a.ifmatch, b, i), ifnomatch: init})
      } catch {
      | _ => init
      }
      switch a.ifnomatch {
      | None =>
        switch (init, default') {
        | (Some(init), Ok(default)) => Test({tests: reverseTestCases(init), default: default})
        | (None, Ok(default)) => Test({tests: tests, default: default})
        | (Some(init), Error(_)) => Test({tests: reverseTestCases(init), default: default})
        | (None, Error(_)) => raise(MergeFail)
        }
      | Some(a) => aux(a, ~init?)
      }
    }
    aux(tests)
  | Leaf(_) => assert false
  }
and expandChildAfterNest = (a, b, i) =>
  switch a {
  | End(a) if i == 0 =>
    try {End(merge(a, b))} catch {
    | _ => End(a)
    }
  | Wildcard(a) => Wildcard(expandChildAfterNest(a, b, i))
  | End(a) => End(expandChildAfterNest(a, b, pred(i)))
  | Tuple({contents, default}) =>
    let contents = expandChildAfterNest(contents, b, succ(i))
    let default = switch default {
    | None => None
    | Some(default) => Some(expandChildAfterNest(default, b, i))
    }
    Tuple({contents: contents, default: default})
  | Test({tests, default}) =>
    let default = switch default {
    | None => None
    | Some(default) => Some(expandChildAfterNest(default, b, i))
    }
    let rec aux = (~init=?, a) => {
      let init = {...a, ifmatch: expandChildAfterNest(a.ifmatch, b, i), ifnomatch: init}
      switch a.ifnomatch {
      | None => Test({tests: reverseTestCases(init), default: default})
      | Some(a) => aux(a, ~init)
      }
    }
    aux(tests)
  | Leaf(_) => assert false
  }
@raises(MergeFail)
and merge = (a, b) =>
  switch (a, b) {
  /* Leaves */
  | (Leaf(_), Leaf(_)) => raise(MergeFail)
  /* Ends */
  | (End(a), End(b)) => End(merge(a, b))
  /* Tuples */
  | (Tuple(a), Tuple(b)) =>
    let default = switch (a.default, b.default) {
    | (None, None) => Ok(None)
    | (Some(z), None) | (None, Some(z)) => Ok(Some(z))
    | (Some(a), Some(b)) =>
      try {Ok(Some(merge(a, b)))} catch {
      | e => Error(e)
      }
    }
    let contents = try {Ok(merge(a.contents, b.contents))} catch {
    | e => Error(e)
    }
    switch (contents, default) {
    | (Ok(contents), Ok(default)) =>
      let contents = switch default {
      | None => contents
      | Some(default) => expandChildAfterNest(contents, default, 0)
      }
      Tuple({contents: contents, default: default})
    | (Ok(contents), Error(_)) => Tuple({...a, contents: contents})
    | (Error(_), Ok(default)) => Tuple({...a, default: default})
    | (Error(_), Error(_)) => raise(MergeFail)
    }
  | (Tuple(a), Wildcard(b)) =>
    let default = switch a.default {
    | None => b
    | Some(a) => merge(a, b)
    }
    let contents = expandChildAfterNest(a.contents, default, 0)
    Tuple({contents: contents, default: Some(default)})
  /* Wildcards */
  | (Wildcard(a), Wildcard(b)) => Wildcard(merge(a, b))
  | (Wildcard(a), Test({tests, default})) =>
    let default = switch default {
    | None => a
    | Some(default) => merge(a, default)
    }
    let tests = mergeAndKeepChildIntoTestCases(tests, default)
    Test({tests: tests, default: Some(default)})
  // Fix this probably lol
  | (Wildcard(a), Tuple(b)) =>
    let default = switch b.default {
    | None => a
    | Some(b) => merge(a, b)
    }
    let contents = mergeAndKeepChildAfterNest(b.contents, default, 0)
    Tuple({contents: contents, default: Some(default)})
  /* Tests */
  | (Test({tests, default}), Wildcard(b)) =>
    let default = switch default {
    | None => b
    | Some(a) => merge(a, b)
    }
    Test({tests: expandChildIntoTestCases(tests, default), default: Some(default)})
  | (Test(a), Test(b)) =>
    let default = switch (a.default, b.default) {
    | (None, None) => Ok(None)
    | (Some(a), None) | (None, Some(a)) => Ok(Some(a))
    | (Some(a), Some(b)) =>
      try {Ok(Some(merge(a, b)))} catch {
      | e => Error(e)
      }
    }
    let b = switch default {
    | Ok(Some(default)) =>
      try {Ok(mergeAndKeepChildIntoTestCases(b.tests, default))} catch {
      | e => Error(e)
      }
    | _ => Ok(b.tests)
    }
    switch (b, default) {
    | (Ok(b), Ok(default)) =>
      let tests = mergeTestCases(a.tests, b)
      let tests = switch default {
      | None => tests
      | Some(default) => expandChildIntoTestCases(tests, default)
      }
      Test({tests: tests, default: default})
    | (Ok(b), Error(_)) =>
      let tests = mergeTestCases(a.tests, b)
      let tests = switch a.default {
      | None => tests
      | Some(default) => expandChildIntoTestCases(tests, default)
      }
      Test({...a, tests: tests})
    | (Error(_), Ok(default)) => Test({...a, default: default})
    | (Error(_), Error(_)) => raise(MergeFail)
    }
  /* Failure cases... maybe? */
  | (Leaf(_), Test(_) | Tuple(_) | Wildcard(_) | End(_))
  | (End(_), Test(_) | Wildcard(_) | Tuple(_) | Leaf(_))
  | (Wildcard(_), End(_) | Leaf(_))
  | (Test(_), Tuple(_) | End(_) | Leaf(_))
  | (Tuple(_), Test(_) | End(_) | Leaf(_)) =>
    assert false
  }

let rec fromArray = (p: option<T.Ast_Pattern.t>, a, ~next, ~success) =>
  switch p {
  | None => success
  | Some(p) =>
    switch p {
    | #Binding(_) => Wildcard(fromArray(a[next], a, ~next=succ(next), ~success))
    | (#Null(_) | #False(_) | #True(_) | #String(_) | #Int(_) | #Float(_)) as p =>
      Test({
        tests: {
          val: TestPat.make(p),
          ifmatch: fromArray(a[next], a, ~next=succ(next), ~success),
          ifnomatch: None,
        },
        default: None,
      })
    | #Tuple(_, tup) =>
      let t = fromArray(a[next], a, ~next=succ(next), ~success)
      let contents = fromArray(tup[0], tup, ~next=1, ~success=End(t))
      Tuple({contents: contents, default: None})
    | #Array(_)
    | #ArrayWithTailBinding(_)
    | #Dict(_)
    | #Object(_) =>
      assert false
    }
  }

let fromNonEmpty = (NonEmpty(hd, tl): NonEmpty.t<_>, ~success) => {
  fromArray(Some(hd), tl, ~next=0, ~success)
}

let mergeFromNonEmpty = (a, NonEmpty(hd, tl): NonEmpty.t<_>, ~success) => {
  let b = fromArray(Some(hd), tl, ~next=0, ~success)
  try {merge(a, b)} catch {
  | _ => a
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
