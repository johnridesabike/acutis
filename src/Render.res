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

module Match = {
  let propsEq = (j, p) =>
    switch p {
    | Typechecker.Pattern.TBool(b) => Props.booleanExn(j) == b
    | TString(s) => Props.stringExn(j) == s
    | TInt(i) => Props.intExn(j) == i
    | TFloat(f) => Props.floatExn(j) == f
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
    switch MapString.get(d, k) {
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
      switch MapString.get(args, key) {
      | Some(val) =>
        let vars = bindNames(vars, ids, val)
        switch testCase(val, cases, ~wildcard) {
        | Some(tree) => makeDict(tree, args, vars)
        | None => None
        }
      | None => None
      }
    | Wildcard({idx: _, key, ids, child}) =>
      switch MapString.get(args, key) {
      | Some(val) =>
        let vars = bindNames(vars, ids, val)
        makeDict(child, args, vars)
      | None => None
      }
    | Construct({idx: _, key, ids, nil, cons, kind: _}) =>
      switch MapString.get(args, key) {
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
      switch MapString.get(args, key) {
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

let echoNotNull = (x, ~props, ~children, ~return) =>
  switch x {
  | Compile.OEBinding(_, binding, esc) =>
    switch MapString.get(props, binding) {
    | Some(x) => return(. Utils.escape(esc, Props.echoExn(x)))
    | None => assert false
    }
  | OEChild(_, child) =>
    switch MapString.get(children, child) {
    | None => assert false
    | Some(x) => x
    }
  | OEString(_, x) => return(. x)
  }

let echoNullable = (x, ~props, ~children, ~return) =>
  switch x {
  | Compile.OEBinding(_, binding, esc) =>
    switch MapString.get(props, binding) {
    | Some(x) =>
      switch Props.nullableExn(x) {
      | None => None
      | Some(x) => Some(return(. Utils.escape(esc, Props.echoExn(x))))
      }
    | None => assert false
    }
  | OEChild(_, child) => MapString.get(children, child)
  | OEString(_, x) => Some(return(. x))
  }

let echo = (nullables, default, ~props, ~children, ~return) => {
  let rec aux = i =>
    switch nullables[i] {
    | None => echoNotNull(default, ~props, ~children, ~return)
    | Some(x) =>
      switch echoNullable(x, ~props, ~children, ~return) {
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
    ~props: MapString.t<Props.t>,
    ~children: MapString.t<a>,
    ~env: Source.env<a>,
    ~stack: list<string>,
  ) => Queue.t<a> =
  (~nodes, ~props, ~children, ~env, ~stack) => {
    module Env = unpack(env)
    let queue = Queue.make()
    Array.forEachU(nodes, (. node) =>
      switch node {
      | Compile.OEcho({loc: _, nullables, default}) =>
        Queue.add(queue, echo(nullables, default, ~props, ~children, ~return=Env.return))
      | OText(str) => Queue.add(queue, Env.return(. str))
      | OMatch(_, args, dectree) =>
        let args = NonEmpty.map(args, (. x) => Props.fromPattern(x, props))
        switch Match.make(dectree, args) {
        | None => assert false
        | Some(props', nodes) =>
          let props = mapMerge(props, props')
          let result = make(~nodes, ~props, ~children, ~stack, ~env)
          Queue.transfer(result, queue)
        }
      | OMapList(_, pattern, dectree) =>
        let l = Props.fromPattern(pattern, props)
        Props.forEachListExn(l, (. ~index, args) =>
          switch Match.make(dectree, NonEmpty.two(args, index)) {
          | None => assert false
          | Some(props', nodes) =>
            let props = mapMerge(props, props')
            let result = make(~nodes, ~props, ~children, ~stack, ~env)
            Queue.transfer(result, queue)
          }
        )
      | OMapDict(_, pattern, dectree) =>
        let l = Props.fromPattern(pattern, props)
        Props.forEachDictExn(l, (. ~index, args) =>
          switch Match.make(dectree, NonEmpty.two(args, index)) {
          | None => assert false
          | Some(props', nodes) =>
            let props = mapMerge(props, props')
            let result = make(~nodes, ~props, ~children, ~stack, ~env)
            Queue.transfer(result, queue)
          }
        )
      | OComponent({loc: _, props: compPropsRaw, children: compChildrenRaw, val}) =>
        let compChildren = Array.mapU(compChildrenRaw, (. (key, child)) =>
          switch child {
          | OChildBlock(nodes) =>
            let result = make(~nodes, ~props, ~children, ~stack, ~env)
            (key, Env.render(. result))
          | OChildName(child) => (key, MapString.getExn(children, child))
          }
        )->MapString.fromArray
        let compProps =
          Array.mapU(compPropsRaw, (. (key, data)) => (
            key,
            Props.fromPattern(data, props),
          ))->MapString.fromArray
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
        | Function(_, propTypes, f) =>
          Env.try_(.
            (. ()) => f(. env, Props.toJson(compProps, propTypes), mapToDict(compChildren)),
            (. e) => Env.error_internal(. [Debug.uncaughtComponentError(e, ~stack)]),
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
    let props = Props.make(prop_types, props)
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
