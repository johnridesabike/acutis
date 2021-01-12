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

module NonEmpty = {
  type t<'a> = List('a, list<'a>)
  let map = (List(x, rest), ~f) => List(f(. x), Belt.List.mapU(rest, f))
  let toList = (List(x, rest)) => list{x, ...rest}
}

@unboxed
type loc = Loc(int)

module RegEx = {
  let identifierChar = %re("/^[a-zA-Z0-9_]$/")

  let isEndOfIdentifier = (. s) => !Js.Re.test_(identifierChar, s)

  let identifierStartChar = %re("/^[a-z_]$/")

  let isValidIdentifierStart = c => Js.Re.test_(identifierStartChar, c)

  let componentStart = %re("/^[A-Z]$/")

  let isValidComponentStart = c => Js.Re.test_(componentStart, c)

  let isReservedKeyword = s =>
    switch s {
    | "null" | "true" | "false" => true
    | _ => false
    }

  let bindingRegEx = %re("/^[a-z_][a-zA-Z0-9_]*$/")

  let isLegalBinding = x => Js.Re.test_(bindingRegEx, x) && !isReservedKeyword(x)
}

module Errors = {
  type kind = [#Type | #Render | #Compile | #Pattern | #Parse | #Syntax]

  type location = {character: int}

  let location = (Loc(x)) => Some({character: x + 1})

  @unboxed
  type rec anyExn = AnyExn(_): anyExn

  type t = {
    message: string,
    kind: kind,
    location: option<location>,
    path: array<Js.Json.t>,
    exn: option<anyExn>,
  }

  module Stack = {
    type name =
      | Component(option<string>)
      | Section({component: string, section: string})
      | Match
      | Map
      | Index(int)
    type t = list<name>

    let nameToJson = (. x) =>
      switch x {
      | Component(Some(x)) => Js.Json.string(x)
      | Component(None) => Js.Json.null
      | Section({component, section}) => Js.Json.string(`section: ${component}#${section}`)
      | Match => Js.Json.string("match")
      | Map => Js.Json.string("map")
      | Index(x) => Js.Json.number(Belt.Int.toFloat(x))
      }
  }
}

module Result = {
  /* We're using a polymorphic variant because it has nicer JS representation. */
  type t<'a, 'e> = [#data('a) | #errors('e)]
}

module Tokens = {
  type t =
    | String(loc, string)
    | JsonString(loc, string)
    | Comment(loc, string)
    | Number(loc, float)
    | Identifier(loc, string)
    | ComponentName(loc, string)
    | EchoIdentifier(loc, string)
    | EchoChildComponent(loc, string)
    | EchoString(loc, string)
    | EchoNumber(loc, float)
    | Comma(loc)
    | Colon(loc)
    | Slash(loc)
    | OpenBracket(loc)
    | CloseBracket(loc)
    | OpenBrace(loc)
    | CloseBrace(loc)
    | Spread(loc)
    | Block(loc)
    | Equals(loc)
    | Tilde(loc)
    | EndOfFile(loc)

  let toString = x =>
    switch x {
    | String(_, x) => "%} " ++ x
    | JsonString(_, x) => `"${x}"`
    | Number(_, x) => Belt.Float.toString(x)
    | Identifier(_, x) => x
    | ComponentName(_, x) => x
    | Comment(_, x) => `{*${x}*}`
    | EchoIdentifier(_, x) => `{{ ${x} }}`
    | EchoChildComponent(_, x) => `{{ ${x} }}`
    | EchoString(_, x) => `"${x}"`
    | EchoNumber(_, x) => Belt.Float.toString(x)
    | Comma(_) => ","
    | Colon(_) => ":"
    | Slash(_) => "/"
    | OpenBracket(_) => "["
    | CloseBracket(_) => "]"
    | OpenBrace(_) => "{"
    | CloseBrace(_) => "}"
    | Spread(_) => "..."
    | Block(_) => "#"
    | Equals(_) => "="
    | Tilde(_) => "~"
    | EndOfFile(_) => "[end of file]"
    }

  let toLocation = x =>
    switch x {
    | String(x, _)
    | JsonString(x, _)
    | Number(x, _)
    | Identifier(x, _)
    | ComponentName(x, _)
    | Comment(x, _)
    | EchoIdentifier(x, _)
    | EchoChildComponent(x, _)
    | EchoString(x, _)
    | EchoNumber(x, _)
    | Comma(x)
    | Colon(x)
    | Slash(x)
    | OpenBracket(x)
    | CloseBracket(x)
    | OpenBrace(x)
    | CloseBrace(x)
    | Spread(x)
    | Block(x)
    | Equals(x)
    | Tilde(x)
    | EndOfFile(x) => x
    }
}

module Pattern_Ast = {
  type rec node =
    | Null(loc)
    | False(loc)
    | True(loc)
    | String(loc, string)
    | Number(loc, float)
    | Array(loc, list<node>)
    | ArrayWithTailBinding({loc: loc, array: list<node>, bindLoc: loc, binding: string})
    | Object(loc, list<(string, node)>)
    | Binding(loc, string)

  type t = NonEmpty.t<node>

  let toString = x =>
    switch x {
    | True(_) | False(_) => "boolean"
    | Null(_) => "null"
    | String(_) => "string"
    | Number(_) => "number"
    | Array(_) | ArrayWithTailBinding(_) => "array"
    | Object(_) => "object"
    | Binding(_, x) => `binding: \`${x}\``
    }

  let toLocation = x =>
    switch x {
    | True(x)
    | False(x)
    | Null(x)
    | String(x, _)
    | Number(x, _)
    | Array(x, _)
    | ArrayWithTailBinding({loc: x, _})
    | Object(x, _)
    | Binding(x, _) => x
    }
}

module Valid = {
  /* This provides a thin layer of runtime type checking without needing
     to validate the entire data structure. It's useful for reporting when JS
     code sends the wrong data type. */
  type t<'a> = {
    data: 'a,
    acutis_is_valid: string,
  }

  let make = x => {
    data: x,
    acutis_is_valid: "ACUTIS_IS_VALID",
  }

  let validate = x =>
    switch x {
    | {data, acutis_is_valid: "ACUTIS_IS_VALID"} => Some(data)
    | _ => None
    }
}

module Ast = {
  type trim = TrimStart | TrimEnd | TrimBoth | NoTrim
  type rec node =
    | Text(string, trim)
    | Unescaped(loc, string)
    | EchoBinding(loc, string)
    | EchoChild(loc, string)
    | EchoString(string)
    | EchoNumber(float)
    | Match(loc, NonEmpty.t<(loc, string)>, NonEmpty.t<case>)
    | Map(loc, string, NonEmpty.t<case>)
    | Component({
        loc: loc,
        name: string,
        props: list<(string, Pattern_Ast.node)>,
        children: list<(string, childProp)>,
      })
  and case = {patterns: NonEmpty.t<Pattern_Ast.t>, ast: ast}
  and ast = list<node>
  and childProp = ChildName(string) | ChildBlock(ast)

  type t' = {ast: ast, name: option<string>}
  type t = Valid.t<Result.t<t', Errors.t>>
}

type props = Js.Dict.t<Js.Json.t>

type environment<'a> = {
  render: (. Ast.t, props, Js.Dict.t<'a>) => 'a,
  return: (. string) => 'a,
  error: (. string) => 'a,
  mapChild: (. 'a, (. string) => string) => 'a,
  flatMapChild: (. 'a, (. string) => 'a) => 'a,
}

type rec template<'a> = (. environment<'a>, props, Js.Dict.t<'a>) => 'a
and environmentData<'a> = {
  components: Js.Dict.t<template<'a>>,
  stack: Errors.Stack.t,
}
