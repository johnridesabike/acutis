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
module Float = Belt.Float
module Int = Belt.Int
module Json = Js.Json
module MapString = Belt.Map.String
module Queue = Belt.MutableQueue
open Acutis_Types

module Pattern = {
  module Result = Belt.Result

  let arrayToQueueResult = (a, ~f) => {
    let q = Queue.make()
    let rec aux = i =>
      switch a[i] {
      | None => Ok(q)
      | Some(x) =>
        switch f(. x) {
        | Ok(x) =>
          Queue.add(q, x)
          aux(succ(i))
        | Error(_) as e => e
        }
      }
    aux(0)
  }

  let rec toJson = (pattern: Ast_Pattern.t, ~props, ~stack) =>
    switch pattern {
    | #True(_) => Ok(Json.boolean(true))
    | #False(_) => Ok(Json.boolean(false))
    | #Null(_) => Ok(Json.null)
    | #String(_, x) => Ok(Json.string(x))
    | #Number(_, x) => Ok(Json.number(x))
    | #Object(_, x) =>
      x
      ->arrayToQueueResult(~f=(. (k, v)) => toJson(v, ~props, ~stack)->Result.mapU((. v) => (k, v)))
      ->Result.mapU((. q) => {
        let d = Js.Dict.empty()
        Queue.forEachU(q, (. (k, v)) => Js.Dict.set(d, k, v))
        Json.object_(d)
      })
    | #Binding(loc, binding) =>
      switch Js.Dict.get(props, binding) {
      | Some(x) => Ok(x)
      | None => Error(Debug.bindingDoesNotExist(~loc, ~binding, ~stack))
      }
    | #...Ast_Pattern.arr as pattern =>
      toArray(pattern, ~props, ~stack)->Result.mapU((. x) => Json.array(x))
    }
  and toArray = (pattern: Ast_Pattern.arr, ~props, ~stack) =>
    switch pattern {
    | #Array(_, x) =>
      x
      ->arrayToQueueResult(~f=(. x) => toJson(x, ~props, ~stack))
      ->Result.mapU((. q) => Queue.toArray(q))
    | #ArrayWithTailBinding(_, array, #Binding(bindLoc, binding)) =>
      switch Js.Dict.get(props, binding) {
      | Some(data) =>
        switch Json.classify(data) {
        | JSONArray(binding) =>
          array
          ->arrayToQueueResult(~f=(. x) => toJson(x, ~props, ~stack))
          ->Result.mapU((. q) => q->Queue.toArray->Array.concat(binding))
        | data =>
          Error(
            Debug.bindingTypeMismatch(~data, ~pattern=(pattern :> Ast_Pattern.t), ~binding, ~stack),
          )
        }
      | None => Error(Debug.bindingDoesNotExist(~loc=bindLoc, ~binding, ~stack))
      }
    }

  type errors =
    | NoMatch
    | PatternNumberMismatch
    | PatternTypeMismatch({data: Json.tagged_t, pattern: Ast_Pattern.t})
    | TooManyBindings({loc: loc, binding: string})

  let setBinding = (bindings, identifier, json, ~loc) =>
    switch identifier {
    | "_" => Ok(bindings)
    | x =>
      if MapString.has(bindings, x) {
        Error(TooManyBindings({loc: loc, binding: identifier}))
      } else {
        Ok(MapString.set(bindings, x, json))
      }
    }

  let rec testValue = (~pattern: Ast_Pattern.t, ~json, ~bindings) =>
    switch (pattern, Json.classify(json)) {
    | (#Binding(loc, identifier), _) => setBinding(bindings, identifier, json, ~loc)
    | (#Number(_, x), JSONNumber(y)) if x == y => Ok(bindings)
    | (#String(_, x), JSONString(y)) if x == y => Ok(bindings)
    | (#True(_), JSONTrue)
    | (#False(_), JSONFalse)
    | (#Null(_), JSONNull) =>
      Ok(bindings)
    | (#Number(_), JSONNumber(_))
    | (#String(_), JSONString(_))
    | (#False(_), JSONTrue)
    | (#True(_), JSONFalse) =>
      Error(NoMatch)
    | (#Array(_, []), JSONArray([])) => Ok(bindings)
    | (#Array(_, []), JSONArray(_)) => Error(NoMatch)
    | (#Array(_, x), JSONArray(data)) => testArray(~patterns=x, ~data, ~bindings)
    | (#ArrayWithTailBinding(_, array, #Binding(bindLoc, binding)), JSONArray(data)) =>
      testArray(~patterns=array, ~data, ~bindings)->Result.flatMapU((. bindings) =>
        setBinding(
          bindings,
          binding,
          data->Array.sliceToEnd(Array.size(array))->Json.array,
          ~loc=bindLoc,
        )
      )
    | (#Object(_, []), JSONObject(obj)) if obj->Js.Dict.keys->Array.size == 0 => Ok(bindings)
    | (#Object(_, []), JSONObject(_)) => Error(NoMatch)
    | (#Object(_, x), JSONObject(obj)) => testObject(~patterns=x, ~obj, ~bindings)
    | (_, JSONNull) | (#Null(_), _) => Error(NoMatch)
    | (pattern, data) => Error(PatternTypeMismatch({data: data, pattern: pattern}))
    }
  and testArray = (~patterns, ~data, ~bindings) => {
    let rec aux = (bindings, i) =>
      switch patterns[i] {
      | None => Ok(bindings)
      | Some(pattern) =>
        switch data[i] {
        | None => Error(NoMatch)
        | Some(json) =>
          switch testValue(~json, ~pattern, ~bindings) {
          | Ok(bindings) => aux(bindings, succ(i))
          | Error(_) as x => x
          }
        }
      }
    aux(bindings, 0)
  }
  and testObject = (~patterns, ~obj, ~bindings) => {
    let rec aux = (bindings, i) =>
      switch patterns[i] {
      | None => Ok(bindings)
      | Some((key, value)) =>
        switch Js.Dict.get(obj, key) {
        | None => Error(NoMatch)
        | Some(json) =>
          switch testValue(~pattern=value, ~json, ~bindings) {
          | Ok(bindings) => aux(bindings, succ(i))
          | Error(_) as x => x
          }
        }
      }
    aux(bindings, 0)
  }

  let test = (
    NonEmpty(patternHead, patternTail): NonEmpty.t<_>,
    NonEmpty(jsonHead, jsonTail): NonEmpty.t<_>,
  ) => {
    /* ALL of the patterns in the sequence need to match their data. */
    let rec aux = (pattern, json, bindings, i) =>
      switch testValue(~pattern, ~json, ~bindings) {
      | Ok(bindings) =>
        switch (patternTail[i], jsonTail[i]) {
        | (None, None) =>
          let d = Js.Dict.empty()
          MapString.forEachU(bindings, (. k, v) => Js.Dict.set(d, k, v))
          Ok(d)
        | (Some(pattern), Some(json)) => aux(pattern, json, bindings, succ(i))
        | (None, Some(_)) | (Some(_), None) => Error(PatternNumberMismatch)
        }
      | Error(_) as e => e
      }
    aux(patternHead, jsonHead, MapString.empty, 0)
  }

  type t<'a> = {
    patterns: NonEmpty.t<NonEmpty.t<Ast_Pattern.t>>,
    f: (. Js.Dict.t<Json.t>) => 'a,
  }

  let matchCase = ({patterns: NonEmpty(head, tail), f}, jsonSequence) => {
    let rec aux = (pattern, i) =>
      switch test(pattern, jsonSequence) {
      | Ok(bindings) => Ok(f(. bindings))
      | Error(NoMatch) =>
        switch tail[i] {
        | Some(pattern) => aux(pattern, succ(i))
        | None => Error(NoMatch)
        }
      | Error(_) as e => e
      }
    aux(head, 0)
  }

  let match = (NonEmpty(head, tail): NonEmpty.t<_>, data) => {
    let rec aux = (pattern, i) =>
      switch matchCase(pattern, data) {
      | Error(NoMatch) =>
        switch tail[i] {
        | Some(pattern) => aux(pattern, succ(i))
        | None => Error(NoMatch)
        }
      | (Ok(_) | Error(_)) as x => x
      }
    aux(head, 0)
  }
}

// This could possibly be defined in the environment.
let escape = c =>
  switch c {
  | "&" => "&amp;"
  | "\"" => "&quot;"
  | "'" => "&apos;"
  | ">" => "&gt;"
  | "<" => "&lt;"
  | "/" => "&#x2F;"
  | "`" => "&#x60;"
  | "=" => "&#x3D;"
  | c => c
  }

let rec escapeAux = (str, pos, result) =>
  switch Js.String2.charAt(str, pos) {
  | "" => result
  | c => escapeAux(str, succ(pos), result ++ escape(c))
  }

let escape = (esc: Ast.Echo.escape, str) =>
  switch esc {
  | Escape => escapeAux(str, 0, "")
  | NoEscape => str
  }

let getBindingOrNull = (props, binding) =>
  switch Js.Dict.get(props, binding) {
  | Some(x) => x
  | None => Json.null
  }

let echoBinding = (props, binding) =>
  switch Json.classify(getBindingOrNull(props, binding)) {
  | JSONString(x) => Ok(x)
  | JSONNumber(x) => Ok(Float.toString(x))
  | type_ => Error(type_)
  }

let addImplicitIndexBinding = (~loc, . x: NonEmpty.t<_>): NonEmpty.t<Ast_Pattern.t> =>
  switch x {
  | NonEmpty(x, []) => NonEmpty(x, [#Binding(loc, "_")])
  | x => x
  }

let match = (patterns, json, ~loc, ~stack) =>
  switch Pattern.match(patterns, json) {
  | Ok(_) as x => x
  | Error(NoMatch) => Error(Debug.noMatchFound(~loc, ~stack))
  | Error(PatternNumberMismatch) => Error(Debug.patternNumberMismatch(~loc, ~stack))
  | Error(PatternTypeMismatch({pattern, data})) =>
    Error(Debug.patternTypeMismatch(~data, ~pattern, ~stack))
  | Error(TooManyBindings({loc, binding})) =>
    Error(Debug.nameBoundMultipleTimes(~binding, ~loc, ~stack))
  }

let trimStart = string => {
  let rec aux = pos =>
    switch Js.String2.charAt(string, pos) {
    | " " | "\t" | "\r" | "\n" => aux(succ(pos))
    | _ => Js.String2.sliceToEnd(string, ~from=pos)
    }
  aux(0)
}

let trimEnd = string => {
  let rec aux = pos =>
    switch Js.String2.charAt(string, pred(pos)) {
    | " " | "\t" | "\r" | "\n" => aux(pred(pos))
    | _ => Js.String2.slice(string, ~from=0, ~to_=pos)
    }
  aux(Js.String.length(string))
}

@val @scope("Object")
external dictMerge: (@as(json`{}`) _, ~base: Js.Dict.t<'a>, Js.Dict.t<'a>) => Js.Dict.t<'a> =
  "assign"

let echo = (head, tail, ~props, ~stack, ~children, ~env, ~error) => {
  let rec aux = (head: Ast.Echo.t, i) =>
    switch head {
    | Binding(loc, binding, esc) =>
      switch echoBinding(props, binding) {
      | Ok(x) => env.return(. escape(esc, x))
      | Error(type_) =>
        switch (type_, tail[i]) {
        | (JSONNull, Some(head)) => aux(head, succ(i))
        | (type_, _) => error(. [Debug.badEchoType(~binding, ~type_, ~loc, ~stack)])
        }
      }
    | Child(loc, child) =>
      switch Js.Dict.get(children, child) {
      | Some(x) => x
      | None =>
        switch tail[i] {
        | Some(head) => aux(head, succ(i))
        | None => error(. [Debug.childDoesNotExist(~loc, ~child, ~stack)])
        }
      }
    | String(x, esc) => env.return(. escape(esc, x))
    | Number(x, esc) => env.return(. escape(esc, Float.toString(x)))
    }
  aux(head, 0)
}

let rec make = (~ast, ~props, ~children, ~envData, ~makeEnv, ~error, ~try_, ~reduceQueue) => {
  let {components, stack} = envData
  let env = makeEnv(. envData)
  let queue = Queue.make()
  Array.forEachU(ast, (. node: Ast.node) =>
    switch node {
    | Echo(_, NonEmpty(head, tail)) =>
      Queue.add(queue, echo(head, tail, ~props, ~stack, ~children, ~env, ~error))
    | Text(str, trim) =>
      Queue.add(
        queue,
        env.return(.
          switch trim {
          | NoTrim => str
          | TrimStart => trimStart(str)
          | TrimEnd => trimEnd(str)
          | TrimBoth => trimStart(trimEnd(str))
          },
        ),
      )
    | Match(loc, identifiers, cases) =>
      let patterns = NonEmpty.map(cases, ~f=(. {patterns, ast}): Pattern.t<_> => {
        patterns: patterns,
        f: (. props') =>
          make(
            ~ast,
            ~props=dictMerge(~base=props, props'),
            ~children,
            ~envData={...envData, stack: list{Match, ...stack}},
            ~makeEnv,
            ~error,
            ~try_,
            ~reduceQueue,
          ),
      })
      let data = NonEmpty.map(identifiers, ~f=(. (_loc, x)) => getBindingOrNull(props, x))
      switch match(patterns, data, ~loc, ~stack) {
      | Ok(result) => Queue.transfer(result, queue)
      | Error(e) => Queue.add(queue, error(. [e]))
      }
    | Map(loc, pattern, cases) =>
      let data = switch pattern {
      | #Binding(loc, binding) =>
        switch Js.Json.classify(getBindingOrNull(props, binding)) {
        | JSONArray(arr) => Ok(arr)
        | type_ => Error(Debug.badMapType(~binding, ~type_, ~loc, ~stack))
        }
      | #...Ast_Pattern.arr as x => Pattern.toArray(x, ~props, ~stack)
      }
      switch data {
      | Error(e) => Queue.add(queue, error(. [e]))
      | Ok(arr) =>
        Array.forEachWithIndexU(arr, (. index, json) => {
          let patterns = NonEmpty.map(cases, ~f=(. {patterns, ast}): Pattern.t<_> => {
            patterns: NonEmpty.map(patterns, ~f=addImplicitIndexBinding(~loc)),
            f: (. props') =>
              make(
                ~ast,
                ~props=dictMerge(~base=props, props'),
                ~children,
                ~envData={...envData, stack: list{Index(index), Map, ...stack}},
                ~makeEnv,
                ~error,
                ~try_,
                ~reduceQueue,
              ),
          })
          switch match(patterns, NonEmpty(json, [index->Int.toFloat->Json.number]), ~loc, ~stack) {
          | Ok(result) => Queue.transfer(result, queue)
          | Error(e) => Queue.add(queue, error(. [e]))
          }
        })
      }
    | Component({loc, name, props: compPropsRaw, children: compChildrenRaw}) =>
      switch MapString.get(components, name) {
      | Some(component) =>
        let compProps = Js.Dict.empty()
        let compChildren = Js.Dict.empty()
        let errors = Queue.make()
        Array.forEachU(compChildrenRaw, (. (key, child)) =>
          switch child {
          | ChildBlock(ast) =>
            Js.Dict.set(
              compChildren,
              key,
              reduceQueue(.
                make(
                  ~ast,
                  ~props,
                  ~children,
                  ~envData={
                    ...envData,
                    stack: list{Section({component: name, section: key}), ...stack},
                  },
                  ~makeEnv,
                  ~error,
                  ~try_,
                  ~reduceQueue,
                ),
              ),
            )
          | ChildName(child) =>
            switch Js.Dict.get(children, child) {
            | Some(data) => Js.Dict.set(compChildren, key, data)
            | None => Queue.add(errors, Debug.childDoesNotExist(~loc, ~child, ~stack))
            }
          }
        )
        Array.forEachU(compPropsRaw, (. (key, data)) =>
          switch Pattern.toJson(data, ~props, ~stack) {
          | Ok(data) => Js.Dict.set(compProps, key, data)
          | Error(e) => Queue.add(errors, e)
          }
        )
        if Queue.isEmpty(errors) {
          Queue.add(
            queue,
            try_(.
              (. ()) => component(. env, compProps, compChildren),
              ~catch=(. e) => error(. [Debug.componentExn(e, ~stack)]),
            ),
          )
        } else {
          Queue.add(queue, error(. Queue.toArray(errors)))
        }
      | None =>
        Queue.add(queue, error(. [Debug.componentDoesNotExist(~component=name, ~loc, ~stack)]))
      }
    }
  )
  queue
}
