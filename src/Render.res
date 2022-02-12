/**
  Copyright (c) 2021 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Array = Belt.Array
module MapString = Belt.Map.String
module MapInt = Belt.Map.Int
module Queue = Belt.MutableQueue
module SetInt = Belt.Set.Int

let rec testCase = (~wildcard, val, case) =>
  if Data.Const.equal(val, case.Matching.val) {
    Some(case.ifMatch)
  } else {
    switch case.nextCase {
    | Some(case) => testCase(val, case, ~wildcard)
    | None => wildcard
    }
  }

let bindNames = (map, ids, val) => SetInt.reduceU(ids, map, (. map, id) => MapInt.set(map, id, val))

let arrayGet = (. a, i) => a[i]

let mapStringGet = (. d, k) => MapString.get(d, k)

let nonemptyGet = (. a, i) => NonEmpty.get(a, i)

let rec make_match: 'a 'args 'key. (
  Matching.tree<'a, 'key>,
  'args,
  (. 'args, 'key) => option<Data.t>,
  MapInt.t<Data.t>,
) => option<(MapInt.t<Data.t>, 'a)> = (tree, args, get, vars) =>
  switch tree {
  | End(x) => Some((vars, x))
  | Switch({key, cases, wildcard, ids, _}) =>
    switch get(. args, key) {
    | Some(val) =>
      let vars = bindNames(vars, ids, val)
      let val = Data.constantExn(val)
      switch testCase(val, cases, ~wildcard) {
      | Some(tree) => make_match(tree, args, get, vars)
      | None => None
      }
    | None => None
    }
  | Wildcard({key, ids, child}) =>
    switch get(. args, key) {
    | Some(val) =>
      let vars = bindNames(vars, ids, val)
      make_match(child, args, get, vars)
    | None => None
    }
  | Construct({key, ids, nil, cons, _}) =>
    switch get(. args, key) {
    | Some(val) =>
      let vars = bindNames(vars, ids, val)
      let child = if Data.isNull(val) {
        nil
      } else {
        cons
      }
      switch child {
      | Some(tree) => make_match(tree, args, get, vars)
      | None => None
      }
    | None => None
    }
  | Nest({key, ids, child, wildcard, _}) =>
    switch get(. args, key) {
    | Some(val) =>
      let vars = bindNames(vars, ids, val)
      let result = switch child {
      | IntKeys(child) =>
        let tuple = Data.tupleExn(val)
        make_match(child, tuple, arrayGet, vars)
      | StringKeys(child) =>
        let dict = Data.dictExn(val)
        make_match(child, dict, mapStringGet, vars)
      }
      switch result {
      | Some((vars, tree)) => make_match(tree, args, get, vars)
      | None =>
        switch wildcard {
        | Some(tree) => make_match(tree, args, get, vars)
        | None => None
        }
      }
    | None => None
    }
  }

let make_match = ({Matching.tree: tree, exits}, args) =>
  switch make_match(tree, args, nonemptyGet, MapInt.empty) {
  | Some((vars, {names, exit})) =>
    let bindings = MapString.mapU(names, (. id) => MapInt.getExn(vars, id))
    Some((bindings, Matching.Exit.get(exits, exit)))
  | None => None
  }

let echoNotNull = (x, props, children, return) =>
  switch x {
  | Compile.OEBinding(binding, esc) =>
    switch MapString.get(props, binding) {
    | Some(x) => return(. Utils.escape(esc, Data.toString(x)))
    | None => assert false
    }
  | OEChild(child) =>
    switch MapString.get(children, child) {
    | None => assert false
    | Some(x) => x
    }
  | OEString(x) => return(. x)
  }

let echoNullable = (x, props, children, return) =>
  switch x {
  | Compile.OEBinding(binding, esc) =>
    switch MapString.get(props, binding) {
    | Some(x) =>
      switch Data.nullableExn(x) {
      | None => None
      | Some(x) => Some(return(. Utils.escape(esc, Data.toString(x))))
      }
    | None => assert false
    }
  | OEChild(child) => MapString.get(children, child)
  | OEString(x) => Some(return(. x))
  }

let echo = (nullables, default, props, children, return) => {
  let rec aux = i =>
    switch nullables[i] {
    | None => echoNotNull(default, props, children, return)
    | Some(x) =>
      switch echoNullable(x, props, children, return) {
      | Some(x) => x
      | None => aux(succ(i))
      }
    }
  aux(0)
}

let mapMerge = (d1, d2) =>
  MapString.mergeU(d1, d2, (. _, v1, v2) =>
    switch (v1, v2) {
    | (None, None) => None
    | (_, Some(_) as x) | (Some(_) as x, None) => x
    }
  )

let mapToDict = m => {
  let d = Js.Dict.empty()
  MapString.forEachU(m, (. k, v) => Js.Dict.set(d, k, v))
  d
}

let rec make:
  type a. (
    ~nodes: Compile.nodes<Compile.template<a>>,
    ~props: MapString.t<Data.t>,
    ~children: MapString.t<a>,
    ~env: Source.env<a>,
    ~stack: list<string>,
  ) => Queue.t<a> =
  (~nodes, ~props, ~children, ~env, ~stack) => {
    module Env = unpack(env)
    let queue = Queue.make()
    Array.forEachU(nodes, (. node) =>
      switch node {
      | Compile.OEcho(nullables, default) =>
        Queue.add(queue, echo(nullables, default, props, children, Env.return))
      | OText(str) => Queue.add(queue, Env.return(. str))
      | OMatch(args, dectree) =>
        let args = NonEmpty.map(args, (. x) => Data.fromPattern(x, props))
        switch make_match(dectree, args) {
        | None => assert false
        | Some(props', nodes) =>
          let props = mapMerge(props, props')
          let result = make(~nodes, ~props, ~children, ~stack, ~env)
          Queue.transfer(result, queue)
        }
      | OMapList(pattern, dectree) =>
        let l = Data.fromPattern(pattern, props)
        Data.forEachListExn(l, (. ~index, args) =>
          switch make_match(dectree, NonEmpty.two(args, index)) {
          | None => assert false
          | Some(props', nodes) =>
            let props = mapMerge(props, props')
            let result = make(~nodes, ~props, ~children, ~stack, ~env)
            Queue.transfer(result, queue)
          }
        )
      | OMapDict(pattern, dectree) =>
        let l = Data.fromPattern(pattern, props)
        Data.forEachDictExn(l, (. ~index, args) =>
          switch make_match(dectree, NonEmpty.two(args, index)) {
          | None => assert false
          | Some(props', nodes) =>
            let props = mapMerge(props, props')
            let result = make(~nodes, ~props, ~children, ~stack, ~env)
            Queue.transfer(result, queue)
          }
        )
      | OComponent(debug, val, compProps, compChildren) =>
        let compChildren = MapString.mapU(compChildren, (. child) =>
          switch child {
          | OChildBlock(nodes) =>
            let result = make(~nodes, ~props, ~children, ~stack, ~env)
            Env.render(. result)
          | OChildName(child) => MapString.getExn(children, child)
          }
        )
        let compProps = MapString.mapU(compProps, (. data) => Data.fromPattern(data, props))
        let result = switch val {
        | Acutis(name, nodes) =>
          Env.render(.
            make(
              ~nodes,
              ~props=compProps,
              ~children=compChildren,
              ~stack=list{name, ...stack},
              ~env,
            ),
          )
        | Function(name, propTypes, f) =>
          Env.try_(.
            (. ()) => f(. env, Data.toJson(compProps, propTypes), mapToDict(compChildren)),
            (. e) => Env.error_internal(. [Debug.uncaughtComponentError(debug, ~name, ~stack, e)]),
          )
        }
        Queue.add(queue, result)
      }
    )
    queue
  }

let make = (type a, env: Source.env<a>, {Compile.nodes: nodes, name, prop_types}, props) => {
  module Env = unpack(env)
  try {
    let props = Data.make(props, prop_types)
    Env.render(. make(~nodes, ~props, ~children=MapString.empty, ~env, ~stack=list{name}))
  } catch {
  | Debug.Exit(e) => Env.error_internal(. [e])
  }
}

let render = (forEach, x) => {
  let result = ref("")
  let errors = Queue.make()
  forEach(.x, (. x) => {
    switch x {
    | #ok(s) => result := result.contents ++ s
    | #errors(e) => e->Queue.fromArray->Queue.transfer(errors)
    }
  })
  if Queue.isEmpty(errors) {
    #ok(result.contents)
  } else {
    #errors(Queue.toArray(errors))
  }
}

module Sync = {
  type t = Result.t<string>
  type e = exn
  let return = (. s) => #ok(s)
  let return_ = return
  let error = (. s) => #errors([Debug.customError(s)])
  let error_internal = (. x) => #errors(x)
  let render = (. q) => render((. q, f) => Queue.forEachU(q, f), q)
  let try_ = (. f, catch) =>
    try {
      f(.)
    } catch {
    | e => catch(. e)
    }
  let map = (. child, f) => Result.map(child, f)
  let flatmap = (. child, f) => Result.flatMap(child, f)
}

let sync = make(module(Sync))

module Async = {
  module Promise = Js.Promise
  type t = Promise.t<Result.t<string>>
  type e = Promise.error
  let return = (. s) => Promise.resolve(#ok(s))
  let return_ = return
  let error = (. s) => Promise.resolve(#errors([Debug.customError(s)]))
  let error_internal = (. x) => Promise.resolve(#errors(x))
  let render_array = a => Promise.resolve(render((. a, f) => Array.forEachU(a, f), a))
  let render = (. q) => q |> Queue.toArray |> Promise.all |> Promise.then_(render_array)
  let try_ = (. f, catch) => Promise.catch(e => catch(. e), f(.))
  let map = (. child, f) => Promise.then_(child => Promise.resolve(Result.map(child, f)), child)
  let flatmap = (. child, f) => Promise.then_(child =>
      switch child {
      | #ok(child) => f(child)
      | #errors(_) as e => Promise.resolve(e)
      }
    , child)
}

let async = make(module(Async))
