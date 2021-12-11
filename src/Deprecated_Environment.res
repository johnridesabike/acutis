/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Array = Belt.Array
module Queue = Belt.MutableQueue

type t<'a> = Acutis_Types.environment<'a>

let return = (. x) => #ok(x)

let error = (. x) => #errors(x)

let try_ = (. f, ~catch) =>
  try {
    f(.)
  } catch {
  | e => catch(. e)
  }

let mapChild = (. child, f) => Result.map(child, f)

let flatMapChild = (. child, f) => Result.flatMap(child, f)

let reduceQueue = (. q) => {
  let result = ref("")
  let errors = Queue.make()
  Queue.forEachU(q, (. x) =>
    switch x {
    | #ok(s) => result := result.contents ++ s
    | #errors(e) => e->Queue.fromArray->Queue.transfer(errors)
    }
  )
  if Queue.isEmpty(errors) {
    #ok(result.contents)
  } else {
    #errors(Queue.toArray(errors))
  }
}

let rec makeEnv = (. stack): t<_> => {
  render: (. {name, nodes}, props, children) =>
    reduceQueue(.
      Deprecated_Render.make(
        ~nodes,
        ~props,
        ~children,
        ~stack=list{Component(name), ...stack},
        ~makeEnv,
        ~error,
        ~try_,
        ~reduceQueue,
      ),
    ),
  return: return,
  error: (. message) => #errors([Debug.Deprecated.customError(message, ~stack)]),
  mapChild: mapChild,
  flatMapChild: flatMapChild,
}

let sync = makeEnv(. list{})

let returnAsync = (. x) => Js.Promise.resolve(#ok(x))

let error = (. x) => Js.Promise.resolve(#errors(x))

let try_ = (. f, ~catch) => Js.Promise.catch(e => catch(. e), f(.))

let mapChildAsync = (. child, f) =>
  Js.Promise.then_(child => Js.Promise.resolve(Result.map(child, f)), child)

let flatMapChildAsync = (. child, f) => Js.Promise.then_(child =>
    switch child {
    | #ok(child) => f(child)
    | #errors(_) as e => Js.Promise.resolve(e)
    }
  , child)

let reduceArray = a => {
  let result = ref("")
  let errors = Queue.make()
  Array.forEachU(a, (. x) => {
    switch x {
    | #ok(s) => result := result.contents ++ s
    | #errors(e) => e->Queue.fromArray->Queue.transfer(errors)
    }
  })
  if Queue.isEmpty(errors) {
    Js.Promise.resolve(#ok(result.contents))
  } else {
    Js.Promise.resolve(#errors(Queue.toArray(errors)))
  }
}

// We could possibly replace Queue.toArray with a custom JS iterable.
let reduceQueue = (. q) => q |> Queue.toArray |> Js.Promise.all |> Js.Promise.then_(reduceArray)

let rec makeEnv = (. stack): t<_> => {
  render: (. {name, nodes}, props, children) =>
    reduceQueue(.
      Deprecated_Render.make(
        ~nodes,
        ~props,
        ~children,
        ~stack=list{Component(name), ...stack},
        ~makeEnv,
        ~error,
        ~try_,
        ~reduceQueue,
      ),
    ),
  return: returnAsync,
  error: (. message) =>
    Js.Promise.resolve(#errors([Debug.Deprecated.customError(message, ~stack)])),
  mapChild: mapChildAsync,
  flatMapChild: flatMapChildAsync,
}

let async = makeEnv(. list{})
