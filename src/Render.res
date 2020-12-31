/**
   Copyright 2020 John Jackson

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
module List = Belt.List
module Queue = Belt.MutableQueue
open Acutis_Types

module Pattern = {
  module Result = Belt.Result
  open Pattern_Ast

  type toJsonErrors =
    | BindingTypeMismatch({data: Json.tagged_t, pattern: node, binding: string})
    | BindingDoesNotExist({loc: loc, binding: string})

  let listToArrayResult = l => {
    let q = Queue.make()
    let rec aux = l =>
      switch l {
      | list{} => Ok(Queue.toArray(q))
      | list{Error(_) as e, ..._} => e
      | list{Ok(x), ...rest} =>
        Queue.add(q, x)
        aux(rest)
      }
    aux(l)
  }

  let rec toJson = (pattern, ~props) =>
    switch pattern {
    | True(_) => Ok(Json.boolean(true))
    | False(_) => Ok(Json.boolean(false))
    | Null(_) => Ok(Json.null)
    | String(_, x) => Ok(Json.string(x))
    | Number(_, x) => Ok(Json.number(x))
    | Array(_, x) =>
      x
      ->List.mapU((. x) => toJson(x, ~props))
      ->listToArrayResult
      ->Result.mapU((. x) => Json.array(x))
    | ArrayWithTailBinding({array, bindLoc, binding, _}) =>
      switch props->Js.Dict.get(binding)->Belt.Option.mapU((. x) => Json.classify(x)) {
      | Some(JSONArray(tailBinding)) =>
        array
        ->List.mapU((. x) => toJson(x, ~props))
        ->listToArrayResult
        ->Result.mapU((. x) => x->Array.concat(tailBinding)->Json.array)
      | Some(data) => Error(BindingTypeMismatch({data: data, pattern: pattern, binding: binding}))
      | None => Error(BindingDoesNotExist({loc: bindLoc, binding: binding}))
      }
    | Object(_, x) =>
      x
      ->List.mapU((. (k, v)) =>
        switch toJson(v, ~props) {
        | Ok(v) => Ok((k, v))
        | Error(_) as e => e
        }
      )
      ->listToArrayResult
      ->Result.mapU((. x) => x->Js.Dict.fromArray->Json.object_)
    | Binding(loc, x) =>
      switch Js.Dict.get(props, x) {
      | Some(x) => Ok(x)
      | None => Error(BindingDoesNotExist({loc: loc, binding: x}))
      }
    }

  type errors =
    | NoMatch
    | PatternNumberMismatch
    | PatternTypeMismatch({data: Json.tagged_t, pattern: node})
    | TooManyBindings({loc: loc, binding: string})

  let setBinding = (bindings, identifier, json, ~loc) =>
    switch identifier {
    | "_" => Ok(bindings)
    | x =>
      switch Js.Dict.get(bindings, x) {
      | Some(_) => Error(TooManyBindings({loc: loc, binding: identifier}))
      | None =>
        Js.Dict.set(bindings, x, json)
        Ok(bindings)
      }
    }

  let rec testValue = (~pattern, ~json, ~bindings) =>
    switch (pattern, Json.classify(json)) {
    | (Binding(loc, identifier), _) => setBinding(bindings, identifier, json, ~loc)
    | (Number(_, x), JSONNumber(y)) when x == y => Ok(bindings)
    | (String(_, x), JSONString(y)) when x == y => Ok(bindings)
    | (True(_), JSONTrue) | (False(_), JSONFalse) | (Null(_), JSONNull) => Ok(bindings)
    | (Number(_), JSONNumber(_))
    | (String(_), JSONString(_))
    | (False(_), JSONTrue)
    | (True(_), JSONFalse) =>
      Error(NoMatch)
    | (Array(_, list{}), JSONArray(arr)) when Array.size(arr) == 0 => Ok(bindings)
    | (Array(_, list{}), JSONArray(_)) => Error(NoMatch)
    | (Array(_, x), JSONArray(arr)) => testArray(~patterns=x, ~arr, ~bindings, ~tailBinding=None)
    | (ArrayWithTailBinding({array, bindLoc, binding, _}), JSONArray(arr)) =>
      testArray(~patterns=array, ~arr, ~bindings, ~tailBinding=Some((bindLoc, binding)))
    | (Object(_, list{}), JSONObject(obj)) when obj->Js.Dict.keys->Array.size == 0 => Ok(bindings)
    | (Object(_, list{}), JSONObject(_)) => Error(NoMatch)
    | (Object(_, x), JSONObject(obj)) => testObject(~patterns=x, ~obj, ~bindings)
    | (_, JSONNull) | (Null(_), _) => Error(NoMatch)
    | (pattern, data) => Error(PatternTypeMismatch({data: data, pattern: pattern}))
    }
  and testArray = (~patterns, ~arr, ~bindings, ~tailBinding) => {
    let rec aux = (patterns, index) =>
      switch patterns {
      | list{} =>
        switch tailBinding {
        | Some((loc, tailBinding)) =>
          setBinding(bindings, tailBinding, arr->Array.sliceToEnd(index)->Json.array, ~loc)
        | None => Ok(bindings)
        }
      | list{pattern, ...rest} =>
        switch arr[index] {
        | None => Error(NoMatch)
        | Some(json) =>
          switch testValue(~json, ~pattern, ~bindings) {
          | Ok(_) => aux(rest, index + 1)
          | Error(_) as x => x
          }
        }
      }
    aux(patterns, 0)
  }
  and testObject = (~patterns, ~obj, ~bindings) =>
    switch patterns {
    | list{} => Ok(bindings)
    | list{(key, value), ...patterns} =>
      switch Js.Dict.get(obj, key) {
      | None => Error(NoMatch)
      | Some(json) =>
        switch testValue(~pattern=value, ~json, ~bindings) {
        | Ok(_) => testObject(~patterns, ~obj, ~bindings)
        | Error(_) as x => x
        }
      }
    }

  let test = (patternSequence, jsonSequence) => {
    let bindings = Js.Dict.empty()
    /* ALL of the patterns in the sequence need to match their data. */
    let rec aux = (patternSequence, jsonSequence) =>
      switch (patternSequence, jsonSequence) {
      | (list{}, list{}) => Ok(bindings)
      | (list{pattern, ...patternSequence}, list{json, ...jsonSequence}) =>
        switch testValue(~pattern, ~json, ~bindings) {
        | Error(_) as x => x
        | Ok(_) => aux(patternSequence, jsonSequence)
        }
      | (list{}, _) | (_, list{}) => Error(PatternNumberMismatch)
      }
    aux(NonEmpty.toList(patternSequence), NonEmpty.toList(jsonSequence))
  }

  type t<'a> = {
    patterns: NonEmpty.t<Pattern_Ast.t>,
    f: (. Js.Dict.t<Json.t>) => 'a,
  }

  let matchCase = ({patterns, f}, jsonSequence) => {
    let rec aux = l =>
      switch l {
      | list{} => Error(NoMatch)
      | list{pattern, ...rest} =>
        switch test(pattern, jsonSequence) {
        | Error(NoMatch) => aux(rest)
        | Ok(bindings) => Ok(f(. bindings))
        | Error(_) as x => x
        }
      }
    aux(NonEmpty.toList(patterns))
  }

  let match = (patterns, data) => {
    let rec aux = l =>
      switch l {
      | list{} => Error(NoMatch)
      | list{pattern, ...rest} =>
        switch matchCase(pattern, data) {
        | Ok(_) as x => x
        | Error(NoMatch) => aux(rest)
        | Error(_) as x => x
        }
      }
    aux(NonEmpty.toList(patterns))
  }
}

open Ast

let escape = str => {
  let rec aux = (index, result) =>
    switch Js.String2.charAt(str, index) {
    | "" => result
    | "&" => aux(index + 1, result ++ "&amp;")
    | "\"" => aux(index + 1, result ++ "&quot;")
    | "'" => aux(index + 1, result ++ "&apos;")
    | ">" => aux(index + 1, result ++ "&gt;")
    | "<" => aux(index + 1, result ++ "&lt;")
    | "/" => aux(index + 1, result ++ "&#x2F;")
    | "`" => aux(index + 1, result ++ "&#x60;")
    | "=" => aux(index + 1, result ++ "&#x3D;")
    | c => aux(index + 1, result ++ c)
    }
  aux(0, "")
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

let addImplicitIndexBinding = (~loc, . x: Pattern_Ast.t): Pattern_Ast.t =>
  switch x {
  | List(x, list{}) => List(x, list{Binding(loc, "_")})
  | x => x
  }

let match = (patterns, json, ~loc, ~stack) =>
  switch Pattern.match(patterns, json) {
  | Ok() as x => x
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
    | " " | "\t" | "\r" | "\n" => aux(pos + 1)
    | _ => Js.String2.sliceToEnd(string, ~from=pos)
    }
  aux(0)
}

let trimEnd = string => {
  let rec aux = pos =>
    switch Js.String2.charAt(string, pos - 1) {
    | " " | "\t" | "\r" | "\n" => aux(pos - 1)
    | _ => Js.String2.slice(string, ~from=0, ~to_=pos)
    }
  aux(Js.String.length(string))
}

@val @scope("Object")
external dictMerge: (@as(json`{}`) _, ~base: Js.Dict.t<'a>, Js.Dict.t<'a>) => Js.Dict.t<'a> =
  "assign"

let rec make = (~ast, ~queue, ~props, ~children, ~envData, ~makeEnv, ~error, ~try_) => {
  let {components, stack} = envData
  let env = makeEnv(. envData)
  let rec aux = ast =>
    switch ast {
    | list{} => () // Done
    | list{EchoChild(loc, child), ...ast} =>
      let x = switch Js.Dict.get(children, child) {
      | Some(x) => x
      | None => error(.[Debug.childDoesNotExist(~loc, ~child, ~stack)])
      }
      Queue.add(queue, x)
      aux(ast)
    | list{Text(str, trim), ...ast} =>
      let str = switch trim {
      | NoTrim => str
      | TrimStart => trimStart(str)
      | TrimEnd => trimEnd(str)
      | TrimBoth => trimStart(trimEnd(str))
      }
      Queue.add(queue, env.return(. str))
      aux(ast)
    | list{EchoBinding(loc, binding), ...ast} =>
      let x = switch echoBinding(props, binding) {
      | Ok(x) => env.return(. escape(x))
      | Error(type_) => error(.[Debug.badEchoType(~binding, ~type_, ~loc, ~stack)])
      }
      Queue.add(queue, x)
      aux(ast)
    | list{Unescaped(loc, binding), ...ast} =>
      let x = switch echoBinding(props, binding) {
      | Ok(x) => env.return(. x)
      | Error(type_) => error(.[Debug.badEchoType(~binding, ~type_, ~loc, ~stack)])
      }
      Queue.add(queue, x)
      aux(ast)
    | list{EchoString(str), ...ast} =>
      Queue.add(queue, env.return(. escape(str)))
      aux(ast)
    | list{EchoNumber(num), ...ast} =>
      Queue.add(queue, env.return(. escape(Float.toString(num))))
      aux(ast)
    | list{Match(loc, identifiers, cases), ...ast} =>
      let patterns = NonEmpty.map(cases, ~f=(. {patterns, ast}) => {
        Pattern.patterns: patterns,
        f: (. props') =>
          make(
            ~queue,
            ~ast,
            ~props=dictMerge(~base=props, props'),
            ~children,
            ~envData={...envData, stack: list{Match, ...stack}},
            ~makeEnv,
            ~error,
            ~try_,
          ),
      })
      let data = NonEmpty.map(identifiers, ~f=(. (_loc, x)) => getBindingOrNull(props, x))
      switch match(patterns, data, ~loc, ~stack) {
      | Ok() => ()
      | Error(e) => Queue.add(queue, error(.[e]))
      }
      aux(ast)
    | list{Map(loc, binding, cases), ...ast} =>
      switch Json.classify(getBindingOrNull(props, binding)) {
      | JSONArray(arr) =>
        Array.forEachWithIndexU(arr, (. index, json) => {
          let patterns = NonEmpty.map(cases, ~f=(. {patterns, ast}) => {
            {
              Pattern.patterns: NonEmpty.map(patterns, ~f=addImplicitIndexBinding(~loc)),
              f: (. props') =>
                make(
                  ~queue,
                  ~ast,
                  ~props=dictMerge(~base=props, props'),
                  ~children,
                  ~envData={
                    ...envData,
                    stack: list{Index(index), Map, ...stack},
                  },
                  ~makeEnv,
                  ~error,
                  ~try_,
                ),
            }
          })
          switch match(patterns, List(json, list{index->Int.toFloat->Json.number}), ~loc, ~stack) {
          | Ok() => ()
          | Error(e) => Queue.add(queue, error(.[e]))
          }
        })
      | type_ => Queue.add(queue, error(.[Debug.badMapType(~binding, ~type_, ~loc, ~stack)]))
      }
      aux(ast)
    | list{Component({loc, name, props: compPropsRaw, children: compChildrenRaw}), ...ast} =>
      switch Js.Dict.get(components, name) {
      | Some(component) =>
        let compProps = Js.Dict.empty()
        let compChildren = Js.Dict.empty()
        let errors = Queue.make()
        List.forEachU(compChildrenRaw, (. (key, child)) => {
          switch child {
          | ChildBlock(ast) =>
            let data = env.render(.
              Valid.make(#data({name: Some(`section: ${name}#${key}`), ast: ast})),
              props,
              children,
            )
            Js.Dict.set(compChildren, key, data)
          | ChildName(child) =>
            switch Js.Dict.get(children, child) {
            | Some(data) => Js.Dict.set(compChildren, key, data)
            | None => Queue.add(errors, Debug.childDoesNotExist(~loc, ~child, ~stack))
            }
          }
        })
        List.forEachU(compPropsRaw, (. (key, data)) =>
          switch Pattern.toJson(data, ~props) {
          | Ok(data) => Js.Dict.set(compProps, key, data)
          | Error(BindingTypeMismatch({data, pattern, binding})) =>
            Queue.add(errors, Debug.bindingTypeMismatch(~data, ~pattern, ~binding, ~stack))
          | Error(BindingDoesNotExist({loc, binding})) =>
            Queue.add(errors, Debug.bindingDoesNotExist(~loc, ~binding, ~stack))
          }
        )
        if Queue.isEmpty(errors) {
          Queue.add(
            queue,
            try_(.
              (. ()) => component(. env, compProps, compChildren),
              ~catch=(. e) => error(.[Debug.componentExn(e, ~stack)]),
            ),
          )
        } else {
          Queue.add(queue, error(. Queue.toArray(errors)))
        }
      | None =>
        Queue.add(queue, error(.[Debug.componentDoesNotExist(~component=name, ~loc, ~stack)]))
      }
      aux(ast)
    }
  aux(ast)
}
