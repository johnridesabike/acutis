/**
   Copyright 2021 John Jackson

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
  render: (. {ast, name}, props, children) =>
    reduceQueue(.
      Render.make(
        ~ast,
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
  error: (. message) => #errors([Debug.customError(message, ~stack)]),
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
  render: (. {ast, name}, props, children) =>
    reduceQueue(.
      Render.make(
        ~ast,
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
  error: (. message) => Js.Promise.resolve(#errors([Debug.customError(message, ~stack)])),
  mapChild: mapChildAsync,
  flatMapChild: flatMapChildAsync,
}

let async = makeEnv(. list{})
