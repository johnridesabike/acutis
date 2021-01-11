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
open Acutis_Types

type result = Result.t<string, array<Errors.t>>

module Async = {
  type result = Js.Promise.t<result>
  type template = Acutis_Types.template<result>
  type t = environment<result>

  let returnAsync = (. x) => Js.Promise.resolve(#data(x))

  let error = (. x) => Js.Promise.resolve(#errors(x))

  let try_ = (. f, ~catch) => f(.) |> Js.Promise.catch(e => catch(. e))

  let mapChildAsync = (. child, f) =>
    child |> Js.Promise.then_(child =>
      switch child {
      | #data(child) => Js.Promise.resolve(#data(f(. child)))
      | #errors(_) as e => Js.Promise.resolve(e)
      }
    )

  let flatMapChildAsync = (. child, f) =>
    child |> Js.Promise.then_(child =>
      switch child {
      | #data(child) => f(. child)
      | #errors(_) as e => Js.Promise.resolve(e)
      }
    )

  let toResult = arr => {
    let result = ref("")
    let errors = Queue.make()
    Array.forEachU(arr, (. x) => {
      switch x {
      | #data(s) => result := result.contents ++ s
      | #errors(e) => Array.forEachU(e, (. x) => Queue.add(errors, x))
      }
    })
    if Queue.isEmpty(errors) {
      Js.Promise.resolve(#data(result.contents))
    } else {
      Js.Promise.resolve(#errors(Queue.toArray(errors)))
    }
  }

  let rec makeInternal = (. {components, stack}) => {
    render: (. ast, props, children) =>
      switch Valid.validate(ast) {
      | Some(#errors(e)) => Js.Promise.resolve(#errors([e]))
      | None => Js.Promise.resolve(#errors([Debug.invalidInput(~stack)]))
      | Some(#data({ast, name})) =>
        Render.make(
          ~ast,
          ~props,
          ~children,
          ~envData={components: components, stack: list{Component(name), ...stack}},
          ~makeEnv=makeInternal,
          ~error,
          ~try_,
        )
        |> Queue.toArray
        |> Js.Promise.all
        |> Js.Promise.then_(toResult)
      },
    return: returnAsync,
    error: (. message) => Js.Promise.resolve(#errors([Debug.customError(message, ~stack)])),
    mapChild: mapChildAsync,
    flatMapChild: flatMapChildAsync,
  }

  let make = components => makeInternal(.{components: components, stack: list{}})
}

type template = Acutis_Types.template<result>
type t = environment<result>

let return = (. x) => #data(x)

let error = (. x) => #errors(x)

let try_ = (. f, ~catch) =>
  try {
    f(.)
  } catch {
  | e => catch(. e)
  }

let mapChild = (. child, f) =>
  switch child {
  | #data(child) => #data(f(. child))
  | #errors(_) as e => e
  }

let flatMapChild = (. child, f) =>
  switch child {
  | #data(child) => f(. child)
  | #errors(_) as e => e
  }

let rec makeInternal = (. {components, stack}) => {
  render: (. ast, props, children) =>
    switch Valid.validate(ast) {
    | Some(#errors(x)) => #errors([x])
    | None => #errors([Debug.invalidInput(~stack)])
    | Some(#data({ast, name})) =>
      let result = ref("")
      let errors = Queue.make()
      Render.make(
        ~ast,
        ~props,
        ~children,
        ~envData={components: components, stack: list{Component(name), ...stack}},
        ~makeEnv=makeInternal,
        ~error,
        ~try_,
      )->Queue.forEachU((. x) =>
        switch x {
        | #data(s) => result := result.contents ++ s
        | #errors(e) => Array.forEachU(e, (. x) => Queue.add(errors, x))
        }
      )
      if Queue.isEmpty(errors) {
        #data(result.contents)
      } else {
        #errors(Queue.toArray(errors))
      }
    },
  return: return,
  error: (. message) => #errors([Debug.customError(message, ~stack)]),
  mapChild: mapChild,
  flatMapChild: flatMapChild,
}

let make = components => makeInternal(.{components: components, stack: list{}})
