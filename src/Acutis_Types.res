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
  type t<'a> = NonEmpty('a, list<'a>)
  let map = (NonEmpty(head, tail), ~f) => NonEmpty(f(. head), Belt.List.mapU(tail, f))
  let toList = (NonEmpty(head, tail)) => list{head, ...tail}
}

@unboxed
type loc = Loc(int)

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
      | Index(x) => x->Belt.Int.toFloat->Js.Json.number
      }
  }
}

module Result = {
  /* We're using a polymorphic variant because it has nicer JS representation. */
  type t<'a, 'e> = [#data('a) | #errors('e)]
}

module Token = {
  type t =
    // Static elements
    | Text(loc, string)
    | Comment(loc, string)
    // JSON values
    | String(loc, string)
    | Number(loc, float)
    | True(loc) // a reserved identifier
    | False(loc) // a reserved identifier
    | Null(loc) // a reserved identifier
    // JSON syntax
    | Comma(loc)
    | Colon(loc)
    | OpenBracket(loc)
    | CloseBracket(loc)
    | OpenBrace(loc)
    | CloseBrace(loc)
    | Spread(loc)
    // Component syntax
    | ComponentName(loc, string)
    | Slash(loc)
    | Block(loc)
    | Equals(loc)
    // Dynamic content
    | Identifier(loc, string)
    | Tilde(loc)
    | Question(loc)
    | Ampersand(loc)
    | Echo(loc)
    | EndOfFile(loc)

  let toString = x =>
    switch x {
    | Text(_, x) => "[text]: " ++ x
    | String(_, x) => `"${x}"`
    | Number(_, x) => Belt.Float.toString(x)
    | True(_) => "true"
    | False(_) => "false"
    | Null(_) => "null"
    | Identifier(_, x) => x
    | ComponentName(_, x) => x
    | Comment(_, x) => `{*${x}*}`
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
    | Question(_) => "?"
    | Ampersand(_) => "&"
    | Echo(_) => "{{"
    | EndOfFile(_) => "[end of file]"
    }

  let toLocation = x =>
    switch x {
    | Text(x, _)
    | String(x, _)
    | Number(x, _)
    | Identifier(x, _)
    | True(x)
    | False(x)
    | Null(x)
    | ComponentName(x, _)
    | Comment(x, _)
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
    | Question(x)
    | Ampersand(x)
    | Echo(x)
    | EndOfFile(x) => x
    }
}

module Pattern_Ast = {
  type rec t =
    | Null(loc)
    | False(loc)
    | True(loc)
    | String(loc, string)
    | Number(loc, float)
    | Array(loc, list<t>)
    | ArrayWithTailBinding({loc: loc, array: list<t>, bindLoc: loc, binding: string})
    | Object(loc, list<(string, t)>)
    | Binding(loc, string)

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

  let make = x => {data: x, acutis_is_valid: "ACUTIS_IS_VALID"}

  let validate = x =>
    switch x {
    | {data, acutis_is_valid: "ACUTIS_IS_VALID"} => Some(data)
    | _ => None
    }
}

module Ast = {
  module Echo = {
    type escape = NoEscape | Escape
    type t =
      | Binding(loc, string, escape)
      | Child(loc, string)
      | String(string, escape)
      | Number(float, escape)
  }
  type trim = TrimStart | TrimEnd | TrimBoth | NoTrim
  type rec node =
    | Text(string, trim)
    // The first echo item that isn't null will be returned.
    | Echo(loc, NonEmpty.t<Echo.t>)
    | Match(loc, NonEmpty.t<(loc, string)>, NonEmpty.t<case>)
    | Map(loc, string, NonEmpty.t<case>)
    | Component({
        loc: loc,
        name: string,
        props: list<(string, Pattern_Ast.t)>,
        children: list<(string, childProp)>,
      })
  and case = {patterns: NonEmpty.t<NonEmpty.t<Pattern_Ast.t>>, ast: ast}
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
