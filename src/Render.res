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
module T = Acutis_Types

module Array = Belt.Array
module Ast = T.Ast
module Ast_Pattern = T.Ast_Pattern
module Float = Belt.Float
module Int = Belt.Int
module Json = Js.Json
module MapString = Belt.Map.String
module NonEmpty = T.NonEmpty
module Queue = Belt.MutableQueue

module Pattern = {
  let arrayToQueueResult = (a, ~f) => {
    let q = Queue.make()
    let rec aux = i =>
      switch a[i] {
      | None => #ok(q)
      | Some(x) =>
        switch f(. x) {
        | #ok(x) =>
          Queue.add(q, x)
          aux(succ(i))
        | #errors(_) as e => e
        }
      }
    aux(0)
  }

  let rec toJson = (pattern: Ast_Pattern.t, ~props, ~stack) =>
    switch pattern {
    | #True(_) => #ok(Json.boolean(true))
    | #False(_) => #ok(Json.boolean(false))
    | #Null(_) => #ok(Json.null)
    | #String(_, x) => #ok(Json.string(x))
    | #Number(_, x) => #ok(Json.number(x))
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
      | Some(x) => #ok(x)
      | None => #errors([Debug.bindingDoesNotExist(~loc, ~binding, ~stack)])
      }
    | #Tuple(_, a) =>
      a
      ->arrayToQueueResult(~f=(. x) => toJson(x, ~props, ~stack))
      ->Result.mapU((. q) => Queue.toArray(q)->Js.Json.array)
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
          #errors([
            Debug.bindingTypeMismatch(~data, ~pattern=(pattern :> Ast_Pattern.t), ~binding, ~stack),
          ])
        }
      | None => #errors([Debug.bindingDoesNotExist(~loc=bindLoc, ~binding, ~stack)])
      }
    }

  let setBinding = (bindings, #Binding(loc, identifier), json, ~stack) =>
    switch identifier {
    | "_" => #ok(bindings)
    | identifier =>
      if MapString.has(bindings, identifier) {
        #errors([Debug.nameBoundMultipleTimes(~loc, ~binding=identifier, ~stack)])
      } else {
        #ok(MapString.set(bindings, identifier, json))
      }
    }

  type testResult<'a> = NoMatch | Result(Result.t<'a>)

  let rec testValue = (~pattern: Ast_Pattern.t, ~json, ~bindings, ~stack) =>
    switch (pattern, Json.classify(json)) {
    | (#Binding(_) as b, _) => Result(setBinding(bindings, b, json, ~stack))
    | (#Number(_, x), JSONNumber(y)) if x == y => Result(#ok(bindings))
    | (#String(_, x), JSONString(y)) if x == y => Result(#ok(bindings))
    | (#True(_), JSONTrue)
    | (#False(_), JSONFalse)
    | (#Null(_), JSONNull) =>
      Result(#ok(bindings))
    | (#Number(_), JSONNumber(_))
    | (#String(_), JSONString(_))
    | (#False(_), JSONTrue)
    | (#True(_), JSONFalse) =>
      NoMatch
    | (#Array(_, []), JSONArray([])) => Result(#ok(bindings))
    | (#Array(_, []), JSONArray(_)) => NoMatch
    | (#Tuple(_, x), JSONArray(data)) => testArray(~patterns=x, ~data, ~bindings, ~stack)
    | (#Array(_, x), JSONArray(data)) => testArray(~patterns=x, ~data, ~bindings, ~stack)
    | (#ArrayWithTailBinding(_, patterns, binding), JSONArray(data)) =>
      switch testArray(~patterns, ~data, ~bindings, ~stack) {
      | Result(#ok(bindings)) =>
        Result(
          setBinding(
            bindings,
            binding,
            data->Array.sliceToEnd(Array.size(patterns))->Json.array,
            ~stack,
          ),
        )
      | (Result(#errors(_)) | NoMatch) as e => e
      }
    | (#Object(_, []), JSONObject(obj)) if obj->Js.Dict.keys->Array.size == 0 =>
      Result(#ok(bindings))
    | (#Object(_, []), JSONObject(_)) => NoMatch
    | (#Object(_, patterns), JSONObject(obj)) => testObject(~patterns, ~obj, ~bindings, ~stack)
    | (_, JSONNull) | (#Null(_), _) => NoMatch
    | (pattern, data) => Result(#errors([Debug.patternTypeMismatch(~pattern, ~data, ~stack)]))
    }
  and testArray = (~patterns, ~data, ~bindings, ~stack) => {
    let rec aux = (bindings, i) =>
      switch patterns[i] {
      | None => Result(#ok(bindings))
      | Some(pattern) =>
        switch data[i] {
        | None => NoMatch
        | Some(json) =>
          switch testValue(~json, ~pattern, ~bindings, ~stack) {
          | Result(#ok(bindings)) => aux(bindings, succ(i))
          | (Result(#errors(_)) | NoMatch) as e => e
          }
        }
      }
    aux(bindings, 0)
  }
  and testObject = (~patterns, ~obj, ~bindings, ~stack) => {
    let rec aux = (bindings, i) =>
      switch patterns[i] {
      | None => Result(#ok(bindings))
      | Some((key, value)) =>
        switch Js.Dict.get(obj, key) {
        | None => NoMatch
        | Some(json) =>
          switch testValue(~pattern=value, ~json, ~bindings, ~stack) {
          | Result(#ok(bindings)) => aux(bindings, succ(i))
          | (Result(#errors(_)) | NoMatch) as e => e
          }
        }
      }
    aux(bindings, 0)
  }

  let test = (
    NonEmpty(patternHead, patternTail): NonEmpty.t<_>,
    NonEmpty(jsonHead, jsonTail): NonEmpty.t<_>,
    ~loc,
    ~stack,
  ) => {
    /* ALL of the patterns in the sequence need to match their data. */
    let rec aux = (pattern, json, bindings, i) =>
      switch testValue(~pattern, ~json, ~bindings, ~stack) {
      | Result(#ok(bindings)) =>
        switch (patternTail[i], jsonTail[i]) {
        | (None, None) => Result(#ok(bindings))
        | (Some(pattern), Some(json)) => aux(pattern, json, bindings, succ(i))
        | (None, Some(_)) | (Some(_), None) =>
          Result(#errors([Debug.patternNumberMismatch(~loc, ~stack)]))
        }
      | (Result(#errors(_)) | NoMatch) as e => e
      }
    aux(patternHead, jsonHead, MapString.empty, 0)
  }

  /*
    Consider this pattern:
    {% match a, b, c
       with 1, 2, 3
       with 4, 5, 6 %} path 1
    {% with 7, 8, 9 %} path 2
    {% /match %}

    Here's a simplified version of how we represent that:
    NonEmpty(
      {
        patterns: NonEmpty(NonEmpty(1, [2, 3]), [NonEmpty(4, [5, 6])]),
        f: () => "path 1",
      },
      [
        {
          patterns: NonEmpty(NonEmpty(7, [8, 9]), []),
          f: () => "path 2",
        },
      ],
    )
 */
  type t<'a> = {
    patterns: NonEmpty.t<NonEmpty.t<Ast_Pattern.t>>,
    f: (. Js.Dict.t<Json.t>) => 'a,
  }

  let matchCase = ({patterns: NonEmpty(head, tail), f}, json, ~loc, ~stack) => {
    let rec aux = (pattern, i) =>
      switch test(pattern, json, ~loc, ~stack) {
      | Result(#ok(bindings)) =>
        let d = Js.Dict.empty()
        MapString.forEachU(bindings, (. k, v) => Js.Dict.set(d, k, v))
        Result(#ok(f(. d)))
      | Result(#errors(_)) as e => e
      | NoMatch =>
        switch tail[i] {
        | None => NoMatch
        | Some(pattern) => aux(pattern, succ(i))
        }
      }
    aux(head, 0)
  }

  let match = (NonEmpty(head, tail): NonEmpty.t<_>, data, ~loc, ~stack) => {
    let rec aux = (patterns, i) =>
      switch matchCase(patterns, data, ~loc, ~stack) {
      | Result((#ok(_) | #errors(_)) as x) => x
      | NoMatch =>
        switch tail[i] {
        | Some(pattern) => aux(pattern, succ(i))
        | None => #errors([Debug.noMatchFound(~loc, ~stack)])
        }
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

let echo = (head, tail, ~props, ~stack, ~children, ~env: T.environment<_>, ~error) => {
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

let rec make = (~nodes, ~props, ~children, ~stack, ~makeEnv, ~error, ~try_, ~reduceQueue) => {
  let env = makeEnv(. stack)
  let queue = Queue.make()
  Array.forEachU(nodes, (. node: Ast.node<_>) =>
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
      let patterns = NonEmpty.map(cases, ~f=(. {patterns, nodes}): Pattern.t<_> => {
        patterns: patterns,
        f: (. props') =>
          make(
            ~nodes,
            ~props=dictMerge(~base=props, props'),
            ~children,
            ~stack=list{Match, ...stack},
            ~makeEnv,
            ~error,
            ~try_,
            ~reduceQueue,
          ),
      })
      let data = NonEmpty.map(identifiers, ~f=(. (_loc, x)) => getBindingOrNull(props, x))
      switch Pattern.match(patterns, data, ~loc, ~stack) {
      | #ok(result) => Queue.transfer(result, queue)
      | #errors(e) => Queue.add(queue, error(. e))
      }
    | Map(loc, pattern, cases) =>
      let f = (index, json) => {
        let patterns = NonEmpty.map(cases, ~f=(. {patterns, nodes}): Pattern.t<_> => {
          patterns: NonEmpty.map(patterns, ~f=(. x): NonEmpty.t<_> =>
            switch x {
            // Add a default binding for the index
            | NonEmpty(x, []) => NonEmpty(x, [#Binding(loc, "_")])
            | x => x
            }
          ),
          f: (. props') =>
            make(
              ~nodes,
              ~props=dictMerge(~base=props, props'),
              ~children,
              ~stack=list{Index(index), Map, ...stack},
              ~makeEnv,
              ~error,
              ~try_,
              ~reduceQueue,
            ),
        })
        switch Pattern.match(patterns, NonEmpty(json, [index]), ~loc, ~stack) {
        | #ok(result) => Queue.transfer(result, queue)
        | #errors(e) => Queue.add(queue, error(. e))
        }
      }
      switch pattern {
      | #Binding(loc, binding) =>
        switch Json.classify(getBindingOrNull(props, binding)) {
        | JSONArray(a) => Array.forEachWithIndexU(a, (. i, x) => f(Json.number(Int.toFloat(i)), x))
        | JSONObject(o) =>
          // Js.Dict.forEach doesn't exist
          let keys = Js.Dict.keys(o)
          let l = Array.size(keys)
          for i in 0 to l - 1 {
            let key = Array.getUnsafe(keys, i)
            f(Json.string(key), Js.Dict.unsafeGet(o, key))
          }
        | type_ => Queue.add(queue, error(. [Debug.badMapType(~binding, ~type_, ~loc, ~stack)]))
        }
      | #...Ast_Pattern.arr as a =>
        Pattern.toArray(a, ~props, ~stack)
        ->Result.mapU((. a) =>
          Array.forEachWithIndexU(a, (. i, x) => f(Json.number(Int.toFloat(i)), x))
        )
        ->Result.getOrElseU((. e) => Queue.add(queue, error(. e)))
      | #Object(_, o) =>
        Pattern.arrayToQueueResult(o, ~f=(. (k, v)) =>
          Pattern.toJson(v, ~props, ~stack)->Result.mapU((. x) => (Js.Json.string(k), x))
        )
        ->Result.mapU((. q) => Queue.forEachU(q, (. (k, v)) => f(k, v)))
        ->Result.getOrElseU((. e) => Queue.add(queue, error(. e)))
      }
    | Component({loc, name, props: compPropsRaw, children: compChildrenRaw, f}) =>
      let compProps = Js.Dict.empty()
      let compChildren = Js.Dict.empty()
      let errors = Queue.make()
      Array.forEachU(compChildrenRaw, (. (key, child)) =>
        switch child {
        | ChildBlock(nodes) =>
          Js.Dict.set(
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
        | ChildName(child) =>
          switch Js.Dict.get(children, child) {
          | Some(data) => Js.Dict.set(compChildren, key, data)
          | None => Queue.add(errors, Debug.childDoesNotExist(~loc, ~child, ~stack))
          }
        }
      )
      Array.forEachU(compPropsRaw, (. (key, data)) =>
        switch Pattern.toJson(data, ~props, ~stack) {
        | #ok(data) => Js.Dict.set(compProps, key, data)
        | #errors(e) => e->Queue.fromArray->Queue.transfer(errors)
        }
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
