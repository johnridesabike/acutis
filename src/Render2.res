/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Array = Belt.Array
module Dict = Js.Dict
module Float = Belt.Float
module MapString = Belt.Map.String
module Option = Belt.Option
module SetString = Belt.Set.String
module TC = TypeChecker
module T = Acutis_Types

module RenderMatch = {
  let propsEq = (j, p) =>
    switch p {
    | TC.TypedPattern.TPat_Bool(b) => Props.booleanExn(j) == b
    | TPat_String(s) => Props.stringExn(j) == s
    | TPat_Int(i) => Props.intExn(j) == i
    | TPat_Float(f) => Props.floatExn(j) == f
    }

  let rec testCase = (val, case, ~wildcard) =>
    if propsEq(val, case.Matching.val) {
      Some(case.ifMatch)
    } else {
      switch case.nextCase {
      | Some(case) => testCase(val, case, ~wildcard)
      | None => wildcard
      }
    }

  let bindNames = (map, ns, val) =>
    SetString.reduceU(ns, map, (. map, name) => MapString.set(map, name, val))

  type getter<'a> = (. 'a, int, string) => Props.t
  let arrayGet: getter<_> = (. a, i, _) => Array.getExn(a, i)
  let recordGet: getter<_> = (. d, _, k) => Js.Dict.get(d, k)->Option.getExn
  let nonemptyGet: getter<_> = (. a, i, _) => NonEmpty.getExn(a, i)

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
      switch testCase(val, cases, ~wildcard) {
      | Some(tree) => make(tree, args, get, vars)
      | None => None
      }
    | Wildcard({idx, key, names, child}) =>
      let val = get(. args, idx, key)
      let vars = bindNames(vars, names, val)
      make(child, args, get, vars)
    | Construct({idx, key, names, nil, cons, kind: _}) =>
      let val = get(. args, idx, key)
      let vars = bindNames(vars, names, val)
      let child = if Props.isNull(val) {
        nil
      } else {
        cons
      }
      switch child {
      | Some(tree) => make(tree, args, get, vars)
      | None => None
      }
    | Nest({idx, key, kind, names, child, wildcard}) =>
      let val = get(. args, idx, key)
      let vars = bindNames(vars, names, val)
      let result = switch kind {
      | Tuple =>
        let tuple = Props.tupleExn(val)
        make(child, tuple, arrayGet, vars)
      | Record =>
        let dict = Props.dictExn(val)
        make(child, dict, recordGet, vars)
      | Dict =>
        let dict = Props.dictExn(val)
        makeDict(child, dict, vars)
      }
      switch result {
      | Some((vars, tree)) => make(tree, args, get, vars)
      | None =>
        switch wildcard {
        | Some(tree) => make(tree, args, get, vars)
        | None => None
        }
      }
    }

  and makeDict: 'a. (Matching.tree<'a>, _, _) => option<(_, 'a)> = (tree, args, vars) =>
    switch tree {
    | End(x) => Some((vars, x))
    | Switch({idx: _, key, cases, wildcard, names}) =>
      switch Js.Dict.get(args, key) {
      | Some(val) =>
        let vars = bindNames(vars, names, val)
        switch testCase(val, cases, ~wildcard) {
        | Some(tree) => makeDict(tree, args, vars)
        | None => None
        }
      | None => None
      }
    | Wildcard({idx: _, key, names, child}) =>
      switch Js.Dict.get(args, key) {
      | Some(val) =>
        let vars = bindNames(vars, names, val)
        makeDict(child, args, vars)
      | None => None
      }
    | Construct({idx: _, key, names, nil, cons, kind: _}) =>
      switch Js.Dict.get(args, key) {
      | Some(val) =>
        let vars = bindNames(vars, names, val)
        let child = if Props.isNull(val) {
          nil
        } else {
          cons
        }
        switch child {
        | Some(tree) => makeDict(tree, args, vars)
        | None => None
        }
      | None => None
      }
    | Nest({idx: _, key, kind, names, child, wildcard}) =>
      switch Js.Dict.get(args, key) {
      | Some(val) =>
        let vars = bindNames(vars, names, val)
        let result = switch kind {
        | Tuple =>
          let tuple = Props.tupleExn(val)
          make(child, tuple, arrayGet, vars)
        | Record =>
          let dict = Props.dictExn(val)
          make(child, dict, recordGet, vars)
        | Dict =>
          let dict = Props.dictExn(val)
          makeDict(child, dict, vars)
        }
        switch result {
        | Some((vars, tree)) => makeDict(tree, args, vars)
        | None =>
          switch wildcard {
          | Some(tree) => makeDict(tree, args, vars)
          | None => None
          }
        }
      | None => None
      }
    }

  let make = ({Matching.tree: tree, exits, loc: _}, args) =>
    switch make(tree, args, nonemptyGet, MapString.empty) {
    | Some((vars, {names, exit})) =>
      let bindings = MapString.keepU(vars, (. k, _) => SetString.has(names, k))
      Some((bindings, Array.getExn(exits, exit)))
    | None => None
    }
}
