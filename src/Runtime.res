/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Array = Belt.Array
module Dict = Js.Dict
module MapString = Belt.Map.String
module MapInt = Belt.Map.Int
module Option = Belt.Option
module Queue = Belt.MutableQueue
module SetInt = Belt.Set.Int

module Match = {
  let propsEq = (j, p) =>
    switch p {
    | TypeChecker.Pattern.TPat_Bool(b) => Props.booleanExn(j) == b
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

  let bindNames = (map, ids, val) =>
    SetInt.reduceU(ids, map, (. map, id) => MapInt.set(map, id, val))

  type getter<'a> = (. 'a, int, string) => Props.t
  let arrayGet: getter<_> = (. a, i, _) =>
    switch a[i] {
    | Some(x) => x
    | None => Js.Exn.raiseError("arrayGet")
    }
  let recordGet: getter<_> = (. d, _, k) =>
    switch Dict.get(d, k) {
    | Some(x) => x
    | None => Js.Exn.raiseError("recordGet")
    }
  let nonemptyGet: getter<_> = (. a, i, _) =>
    switch NonEmpty.get(a, i) {
    | Some(x) => x
    | None => Js.Exn.raiseError(`nonemptyGet: ${Belt.Int.toString(i)}`)
    }

  let rec make: 'a 'b. (Matching.tree<'a>, 'b, getter<'b>, _) => option<(_, 'a)> = (
    tree,
    args,
    get,
    vars,
  ) =>
    switch tree {
    | End(x) => Some((vars, x))
    | Switch({idx, key, cases, wildcard, ids}) =>
      let val = get(. args, idx, key)
      let vars = bindNames(vars, ids, val)
      switch testCase(val, cases, ~wildcard) {
      | Some(tree) => make(tree, args, get, vars)
      | None => None
      }
    | Wildcard({idx, key, ids, child}) =>
      let val = get(. args, idx, key)
      let vars = bindNames(vars, ids, val)
      make(child, args, get, vars)
    | Construct({idx, key, ids, nil, cons, kind: _}) =>
      let val = get(. args, idx, key)
      let vars = bindNames(vars, ids, val)
      let child = if Props.isNull(val) {
        nil
      } else {
        cons
      }
      switch child {
      | Some(tree) => make(tree, args, get, vars)
      | None => None
      }
    | Nest({idx, key, kind, ids, child, wildcard}) =>
      let val = get(. args, idx, key)
      let vars = bindNames(vars, ids, val)
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
    | Switch({idx: _, key, cases, wildcard, ids}) =>
      switch Dict.get(args, key) {
      | Some(val) =>
        let vars = bindNames(vars, ids, val)
        switch testCase(val, cases, ~wildcard) {
        | Some(tree) => makeDict(tree, args, vars)
        | None => None
        }
      | None => None
      }
    | Wildcard({idx: _, key, ids, child}) =>
      switch Dict.get(args, key) {
      | Some(val) =>
        let vars = bindNames(vars, ids, val)
        makeDict(child, args, vars)
      | None => None
      }
    | Construct({idx: _, key, ids, nil, cons, kind: _}) =>
      switch Dict.get(args, key) {
      | Some(val) =>
        let vars = bindNames(vars, ids, val)
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
    | Nest({idx: _, key, kind, ids, child, wildcard}) =>
      switch Dict.get(args, key) {
      | Some(val) =>
        let vars = bindNames(vars, ids, val)
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
    switch make(tree, args, nonemptyGet, MapInt.empty) {
    | Some((vars, {names, exit})) =>
      let bindings = MapString.mapU(names, (. id) => MapInt.getExn(vars, id))
      Some((bindings, Array.getExn(exits, exit)))
    | None => None
    }
}

let echoNotNull = (x, ~props, ~children, ~env: Optimize.environment<'a>) =>
  switch x {
  | Optimize.Ast.Echo.Binding(_, binding, esc) =>
    switch Dict.get(props, binding) {
    | Some(x) => env.return(. Optimize.escape(esc, Props.echoExn(x)))
    | None => assert false
    }
  | Child(_, child) => Dict.get(children, child)->Option.getExn
  | String(_, x) => env.return(. x)
  }

let echoNullable = (x, ~props, ~children, ~env: Optimize.environment<'a>) =>
  switch x {
  | Optimize.Ast.Echo.Binding(_, binding, esc) =>
    switch Dict.get(props, binding) {
    | Some(x) =>
      switch Props.nullableExn(x) {
      | None => None
      | Some(x) => Some(env.return(. Optimize.escape(esc, Props.echoExn(x))))
      }
    | None => assert false
    }
  | Child(_, child) => Dict.get(children, child)
  | String(_, x) => Some(env.return(. x))
  }

let echo = (nullables, default, ~props, ~children, ~env) => {
  let rec aux = i =>
    switch nullables[i] {
    | None => echoNotNull(default, ~props, ~children, ~env)
    | Some(x) =>
      switch echoNullable(x, ~props, ~children, ~env) {
      | Some(x) => x
      | None => aux(succ(i))
      }
    }
  aux(0)
}

@val @scope("Object")
external dictCopy: (@as(json`{}`) _, Dict.t<'a>) => Dict.t<'a> = "assign"

let dictMergeMap = (d, m) => {
  let d = dictCopy(d)
  MapString.forEachU(m, (. k, v) => Dict.set(d, k, v))
  d
}

let rec make = (
  ~nodes: Optimize.Ast.nodes<Optimize.componentTemplateU<'a>>,
  ~props: Dict.t<Props.t>,
  ~children,
  ~stack,
  ~makeEnv: (. Debug.Stack.t) => Optimize.environment<'a>,
  ~error,
  ~try_,
  ~reduceQueue,
) => {
  let env = makeEnv(. stack)
  let queue = Queue.make()
  Array.forEachU(nodes, (. node: Optimize.Ast.node<_>) =>
    switch node {
    | OEcho({loc: _, nullables, default}) =>
      Queue.add(queue, echo(nullables, default, ~props, ~children, ~env))
    | OText(str) => Queue.add(queue, env.return(. str))
    | OMatch(_, args, dectree) =>
      let args = NonEmpty.map(args, (. x) => Props.fromPattern(x, props))
      // Js.log(Js.Json.stringifyWithSpace(Obj.magic(dectree.tree), 2))
      switch Match.make(dectree, args) {
      | None => assert false
      | Some(props', nodes) =>
        let props = dictMergeMap(props, props')
        let result = make(
          ~nodes,
          ~props,
          ~children,
          ~stack=list{Debug.Stack.Match, ...stack},
          ~makeEnv,
          ~error,
          ~try_,
          ~reduceQueue,
        )
        Queue.transfer(result, queue)
      }
    | OMapList(_, pattern, dectree) =>
      let l = Props.fromPattern(pattern, props)
      Props.forEachListExn(l, (. ~index, args) =>
        switch Match.make(dectree, NonEmpty.two(args, index)) {
        | None => assert false
        | Some(props', nodes) =>
          let props = dictMergeMap(props, props')
          let result = make(
            ~nodes,
            ~props,
            ~children,
            ~stack=list{Map, ...stack},
            ~makeEnv,
            ~error,
            ~try_,
            ~reduceQueue,
          )
          Queue.transfer(result, queue)
        }
      )
    | OMapDict(_, pattern, dectree) =>
      let l = Props.fromPattern(pattern, props)
      Props.forEachDictExn(l, (. ~index, args) =>
        switch Match.make(dectree, NonEmpty.two(args, index)) {
        | None => assert false
        | Some(props', nodes) =>
          let props = dictMergeMap(props, props')
          let result = make(
            ~nodes,
            ~props,
            ~children,
            ~stack=list{Map, ...stack},
            ~makeEnv,
            ~error,
            ~try_,
            ~reduceQueue,
          )
          Queue.transfer(result, queue)
        }
      )
    | OComponent({loc, name, props: compPropsRaw, children: compChildrenRaw, f}) =>
      let compProps = Dict.empty()
      let compChildren = Dict.empty()
      let errors = Queue.make()
      Array.forEachU(compChildrenRaw, (. (key, child)) =>
        switch child {
        | OChildBlock(nodes) =>
          Dict.set(
            compChildren,
            key,
            reduceQueue(.
              make(
                ~nodes,
                ~props,
                ~children,
                ~stack=list{Section({component: name, section: key}), ...stack},
                ~makeEnv,
                ~error,
                ~try_,
                ~reduceQueue,
              ),
            ),
          )
        | OChildName(child) =>
          switch Dict.get(children, child) {
          | Some(data) => Dict.set(compChildren, key, data)
          | None => Queue.add(errors, Debug.childDoesNotExist(~loc, ~child, ~stack))
          }
        }
      )
      Array.forEachU(compPropsRaw, (. (key, data)) =>
        Dict.set(compProps, key, Props.fromPattern(data, props))
      )
      if Queue.isEmpty(errors) {
        Queue.add(
          queue,
          try_(.
            (. ()) => f(. env, compProps, compChildren),
            ~catch=(. e) => error(. [Debug.uncaughtComponentError(e, ~stack)]),
          ),
        )
      } else {
        Queue.add(queue, error(. Queue.toArray(errors)))
      }
    }
  )
  queue
}
