module Array = Belt.Array
module Dict = Js.Dict
module Float = Belt.Float
module Json = Js.Json
module MapString = Belt.Map.String
module Option = Belt.Option
module SetString = Belt.Set.String
module TC = TypeChecker

let jsonEq = (j, p) =>
  switch p {
  | TC.TypedPattern.TPat_Bool(b) => Json.decodeBoolean(j)->Option.getExn == b
  | TPat_String(s) => Json.decodeString(j)->Option.getExn == s
  | TPat_Int(i) => Json.decodeNumber(j)->Option.getExn->Float.toInt == i
  | TPat_Float(f) => Json.decodeNumber(j)->Option.getExn == f
  }

let rec testCase = (json, case) =>
  if jsonEq(json, case.Matching.val) {
    Some(case.ifMatch)
  } else {
    switch case.nextCase {
    | None => None
    | Some(case) => testCase(json, case)
    }
  }

let bindNames = (map, ns, val) =>
  SetString.reduceU(ns, map, (. map, name) => MapString.set(map, name, val))

type getter<'a> = (. 'a, int, string) => Json.t
let arrayGet: getter<_> = (. a, i, _) => Array.getExn(a, i)
let dictGet: getter<_> = (. d, _, k) => Js.Dict.get(d, k)->Option.getExn
let nonemptyGet: getter<_> = (. a, i, _) => NonEmpty2.getExn(a, i)

let rec make: 'a 'b. (Matching.tree<'a>, 'b, getter<'b>, _) => option<(_, 'a)> = (
  tree,
  args,
  get,
  vars,
) =>
  switch tree {
  | End(x) => Some((vars, x))
  | Switch({idx, key, cases, wildcard, names}) =>
    let val = get(. args, idx, key)
    let vars = bindNames(vars, names, val)
    switch testCase(val, cases) {
    | None =>
      switch wildcard {
      | None => None
      | Some(tree) => make(tree, args, get, vars)
      }
    | Some(tree) => make(tree, args, get, vars)
    }
  | Wildcard({idx, key, names, child}) =>
    let val = get(. args, idx, key)
    let vars = bindNames(vars, names, val)
    make(child, args, get, vars)
  | Construct({idx, key, names, nil, cons, kind: _}) =>
    let val = get(. args, idx, key)
    let vars = bindNames(vars, names, val)
    if Js.Json.test(val, Null) {
      switch nil {
      | None => None
      | Some(tree) => make(tree, args, get, vars)
      }
    } else {
      switch cons {
      | None => None
      | Some(tree) => make(tree, args, get, vars)
      }
    }
  | Nest({idx, kind: Tuple, key, names, child, wildcard}) =>
    let val = get(. args, idx, key)
    let vars = bindNames(vars, names, val)
    let tuple = val->Json.decodeArray->Option.getExn
    switch make(child, tuple, arrayGet, vars) {
    | Some((vars, tree)) => make(tree, args, get, vars)
    | None =>
      switch wildcard {
      | Some(tree) => make(tree, args, get, vars)
      | None => None
      }
    }
  | Nest({idx, kind: Record | Dict, key, names, child, wildcard}) =>
    let val = get(. args, idx, key)
    let vars = bindNames(vars, names, val)
    let dict = val->Json.decodeObject->Option.getExn
    switch make(child, dict, dictGet, vars) {
    | Some((vars, tree)) => make(tree, args, get, vars)
    | None =>
      switch wildcard {
      | Some(tree) => make(tree, args, get, vars)
      | None => None
      }
    }
  }

let make = ({Matching.tree: tree, exits}, args) =>
  switch make(tree, args, nonemptyGet, MapString.empty) {
  | Some((vars, {names, exit})) =>
    let bindings = MapString.keepU(vars, (. k, _) => SetString.has(names, k))
    Some((bindings, Array.getExn(exits, exit)))
  | None => None
  }
