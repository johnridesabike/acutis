module Array = Belt.Array
module Dict = Js.Dict
module Json = Js.Json
module MapString = Belt.Map.String
module Option = Belt.Option

/*
let jsonEq = (j, p: Matching.TestPat.t) =>
  switch p {
  | TTrue => Json.decodeBoolean(j)->Option.getExn == true
  | TFalse => Json.decodeBoolean(j)->Option.getExn == false
  | _ => assert false
  }

let rec testCase = (json, case: Matching.testcase<_>) =>
  if jsonEq(json, case.val) {
    Some(case.ifMatch)
  } else {
    switch case.nextCase {
    | Some(case) => testCase(json, case)
    | None => None
    }
  }

let rec record = (r, tree: Matching.tree) =>
  switch tree {
  | Switch({key, cases, wildcard, _}) =>
    let v = Dict.get(r, key)->Option.getExn
    switch testCase(v, cases) {
    | Some(x) => record(r, x)
    | None => record(r, Option.getExn(wildcard))
    }
  | Wildcard({child, _}) => record(r, child)
  | End(child) => child
  | Nest(_) => assert false
  | Leaf(_) => assert false
  }

and tuple = (t, i, tree: Matching.tree) =>
  switch tree {
  | End(t) => t
  | Switch({cases, wildcard, _}) =>
    switch testCase(Array.getExn(t, i), cases) {
    | Some(x) => tuple(t, succ(i), x)
    | None => tuple(t, succ(i), Option.getExn(wildcard))
    }
  | Wildcard({child, _}) => tuple(t, succ(i), child)
  | Nest({tag: TRecord, child, _}) =>
    let r = t->Array.getExn(i)->Json.decodeObject->Option.getExn
    let tree = record(r, child)
    tuple(t, succ(i), tree)
  | Nest(_) => assert false
  | Leaf(_) => assert false
  }

let rec make = (args, i, tree: Matching.tree, exits) =>
  switch tree {
  | Leaf({bindings: _, exit}) => Array.getUnsafe(exits, exit)
  | Switch({cases, wildcard, _}) =>
    switch testCase(Array.getExn(args, i), cases) {
    | Some(x) => make(args, succ(i), x, exits)
    | None => make(args, succ(i), Option.getExn(wildcard), exits)
    }
  | Wildcard({child, _}) => make(args, succ(i), child, exits)
  | Nest({tag: TRecord, child, _}) =>
    let r = args->Array.getExn(i)->Json.decodeObject->Option.getExn
    let tree = record(r, child)
    make(args, succ(i), tree, exits)
  | Nest({tag: TTuple, child, _}) =>
    let t = args->Array.getExn(i)->Json.decodeArray->Option.getExn
    let tree = tuple(t, 0, child)
    make(args, succ(i), tree, exits)
  | Nest(_) => assert false
  | End(_) => assert false
  }
*/
