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
open Debug

module Pattern = {
  module Result = Belt.Result
  open Pattern_Ast

  type toJsonErrors =
    | BindingTypeMismatchErr({data: Js.Json.tagged_t, pattern: node, binding: identifier})
    | BindingDoesNotExistErr({loc: loc, binding: identifier})

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
      switch props->IdDict.get(~key=binding)->Belt.Option.mapU((. x) => Json.classify(x)) {
      | Some(JSONArray(tailBinding)) =>
        array
        ->List.mapU((. x) => toJson(x, ~props))
        ->listToArrayResult
        ->Result.mapU((. x) => x->Array.concat(tailBinding)->Json.array)
      | Some(data) =>
        Error(BindingTypeMismatchErr({data: data, pattern: pattern, binding: binding}))
      | None => Error(BindingDoesNotExistErr({loc: bindLoc, binding: binding}))
      }
    | Object(_, x) => x->List.mapU((. (k, v)) =>
        switch toJson(v, ~props) {
        | Ok(v) => Ok((k, v))
        | Error(_) as e => e
        }
      )->listToArrayResult->Result.mapU((. x) => x->Js.Dict.fromArray->Json.object_)
    | Binding(loc, x) =>
      switch IdDict.get(props, ~key=x) {
      | Some(x) => Ok(x)
      | None => Error(BindingDoesNotExistErr({loc: loc, binding: x}))
      }
    }

  type errors =
    | NoMatch
    | PatternNumberMismatch
    | PatternTypeMismatch({data: Json.tagged_t, pattern: node})
    | TooManyBindings({loc: loc, binding: identifier})

  let setBinding = (bindings, identifier, json, ~loc) =>
    switch identifier {
    | Id("_") => Ok(bindings)
    | x =>
      switch IdDict.get(bindings, ~key=x) {
      | Some(_) => Error(TooManyBindings({loc: loc, binding: identifier}))
      | None =>
        IdDict.set(bindings, ~key=x, ~data=json)
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

let getBindingOrNull = (props, key) =>
  switch IdDict.get(props, ~key) {
  | Some(x) => x
  | None => Js.Json.null
  }

let echoBinding = (props, binding) =>
  switch Json.classify(getBindingOrNull(props, binding)) {
  | JSONString(x) => Ok(x)
  | JSONNumber(x) => Ok(Float.toString(x))
  | type_ => Error(type_)
  }

let addImplicitIndexBinding = (~loc, . x: Pattern_Ast.t): Pattern_Ast.t =>
  switch x {
  | List(x, list{}) => List(x, list{Binding(loc, Id("_"))})
  | x => x
  }

let match = (patterns, json, ~loc, ~name) =>
  switch Pattern.match(patterns, json) {
  | Ok(x) => x
  | Error(NoMatch) => raise(NoMatchFound({loc: loc, name: name}))
  | Error(PatternNumberMismatch) => raise(PatternNumberMismatch({loc: loc, name: name}))
  | Error(PatternTypeMismatch({pattern, data})) =>
    raise(
      PatternTypeMismatch({
        data: data,
        pattern: pattern,
        name: name,
      }),
    )
  | Error(TooManyBindings({loc, binding})) =>
    raise(NameBoundMultipleTimes({binding: binding, loc: loc, name: name}))
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

let rec make = (~queue, ~pure, ~ast, ~props, ~children, ~components, ~name, ~renderContext) => {
  let rec aux = ast =>
    switch ast {
    | list{} => ()
    | list{EchoChild(loc, child), ...ast} =>
      switch IdDict.get(children, ~key=child) {
      | Some(x) => Queue.add(queue, x)
      | None => raise(ChildDoesNotExist({loc: loc, child: child, name: name}))
      }
      aux(ast)
    | list{Text(str, trim), ...ast} =>
      let str = switch trim {
      | NoTrim => str
      | TrimStart => trimStart(str)
      | TrimEnd => trimEnd(str)
      | TrimBoth => trimStart(trimEnd(str))
      }
      Queue.add(queue, pure(. str))
      aux(ast)
    | list{EchoBinding(loc, binding), ...ast} =>
      switch echoBinding(props, binding) {
      | Ok(x) => Queue.add(queue, pure(. escape(x)))
      | Error(type_) => raise(BadEchoType({binding: binding, type_: type_, loc: loc, name: name}))
      }
      aux(ast)
    | list{Unescaped(loc, binding), ...ast} =>
      switch echoBinding(props, binding) {
      | Ok(x) => Queue.add(queue, pure(. x))
      | Error(type_) => raise(BadEchoType({binding: binding, type_: type_, loc: loc, name: name}))
      }
      aux(ast)
    | list{EchoString(str), ...ast} =>
      Queue.add(queue, pure(. escape(str)))
      aux(ast)
    | list{EchoNumber(num), ...ast} =>
      Queue.add(queue, pure(. escape(Float.toString(num))))
      aux(ast)
    | list{Match(loc, identifiers, cases), ...ast} =>
      let patterns = NonEmpty.map(cases, ~f=(. {patterns, ast}) => {
        Pattern.patterns: patterns,
        f: (. props') =>
          make(
            ~queue,
            ~ast,
            ~props=IdDict.merge(~base=props, props'),
            ~children,
            ~components,
            ~name,
            ~renderContext,
            ~pure,
          ),
      })
      let data = NonEmpty.map(identifiers, ~f=(. (_loc, x)) => getBindingOrNull(props, x))
      match(patterns, data, ~loc, ~name)
      aux(ast)
    | list{Map(loc, binding, cases), ...ast} =>
      switch Json.classify(getBindingOrNull(props, binding)) {
      | JSONArray(arr) =>
        Array.forEachWithIndexU(arr, (. index, json) => {
          NonEmpty.map(cases, ~f=(. {patterns, ast}) => {
            {
              Pattern.patterns: NonEmpty.map(patterns, ~f=addImplicitIndexBinding(~loc)),
              f: (. props') =>
                make(
                  ~queue,
                  ~ast,
                  ~props=IdDict.merge(~base=props, props'),
                  ~children,
                  ~components,
                  ~name,
                  ~renderContext,
                  ~pure,
                ),
            }
          })->match(List(json, list{index->Int.toFloat->Json.number}), ~loc, ~name)
        })
        aux(ast)
      | type_ => raise(BadMapType({binding: binding, type_: type_, loc: loc, name: name}))
      }
    | list{Component({loc, name: comp, props: compPropsRaw, children: compChildrenRaw}), ...ast} =>
      switch IdDict.get(components, ~key=comp) {
      | Some(component) =>
        let compProps = Js.Dict.empty()
        let compChildren = Js.Dict.empty()
        List.forEachU(compChildrenRaw, (. (key, child)) => {
          let child = switch child {
          | ChildBlock(ast) =>
            renderContext(. {name: name, ast: ast, isCompiledAst: #VALID_AST}, props, children)
          | ChildName(child) =>
            switch IdDict.get(children, ~key=child) {
            | Some(x) => x
            | None => raise(ChildDoesNotExist({loc: loc, child: child, name: name}))
            }
          }
          IdDict.set(compChildren, ~key, ~data=child)
        })
        List.forEachU(compPropsRaw, (. (key, data)) =>
          switch Pattern.toJson(data, ~props) {
          | Ok(data) => IdDict.set(compProps, ~key, ~data)
          | Error(BindingTypeMismatchErr({data, pattern, binding})) =>
            raise(
              BindingTypeMismatch({
                data: data,
                pattern: pattern,
                binding: binding,
                name: name,
              }),
            )
          | Error(BindingDoesNotExistErr({loc, binding})) =>
            raise(BindingDoesNotExist({loc: loc, binding: binding, name: name}))
          }
        )
        let result = component(. renderContext, compProps, compChildren)
        Queue.add(queue, result)
        aux(ast)
      | None => raise(ComponentDoesNotExist({component: comp, loc: loc, name: name}))
      }
    }
  aux(ast)
}

let isAstCompiledOrThrow = x =>
  if x.isCompiledAst != #VALID_AST {
    raise(BadRenderInput)
  }

let makeContext: Js.Dict.t<templateFunctionSync> => renderContextSync = {
  let pure = (. x): string => x

  let toString = queue => {
    let rec aux = result =>
      switch Queue.pop(queue) {
      | None => result
      | Some(str) => aux(result ++ str)
      }
    aux("")
  }

  components => {
    let rec renderContext = (. ast, props, children) => {
      isAstCompiledOrThrow(ast)
      let {ast, name, _} = ast
      let queue = Queue.make()
      make(~queue, ~ast, ~props, ~children, ~components, ~name, ~renderContext, ~pure)
      toString(queue)
    }
    renderContext
  }
}

let makeContextAsync: Js.Dict.t<templateFunctionAsync> => renderContextAsync = {
  let pure = (. x): Js.Promise.t<string> => Js.Promise.resolve(x)
  let toString = x => x->Js.Array2.joinWith("")->Js.Promise.resolve

  components => {
    let rec renderContext = (. ast, props, children) =>
      try {
        isAstCompiledOrThrow(ast)
        let {ast, name, _} = ast
        let queue = Queue.make()
        make(~queue, ~ast, ~props, ~children, ~components, ~name, ~renderContext, ~pure)
        queue |> Queue.toArray |> Js.Promise.all |> Js.Promise.then_(toString)
      } catch {
      | e => Js.Promise.reject(e)
      }
    renderContext
  }
}
