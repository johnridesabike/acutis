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
  type t<'a> = NonEmpty('a, array<'a>)
  let map = (NonEmpty(head, tail), ~f) => NonEmpty(f(. head), Belt.Array.mapU(tail, f))
}

@unboxed
type loc = Loc(int)

module Errors = {
  type kind = [#Type | #Render | #Compile | #Pattern | #Parse | #Syntax]

  type location = {@live character: int}

  let location = (Loc(x)) => {character: x + 1}

  @unboxed
  type rec anyExn = AnyExn(_): anyExn

  type t = {
    @live message: string,
    @live kind: kind,
    @live location: option<location>,
    @live path: array<Js.Json.t>,
    @live exn: option<anyExn>,
  }

  module Stack = {
    type name =
      | Component(string)
      | Section({component: string, section: string})
      | Match
      | Map
      | Index(int)
    type t = list<name>

    let nameToJson = (. x) =>
      switch x {
      | Component(x) => Js.Json.string(x)
      | Section({component, section}) => Js.Json.string(`section: ${component}#${section}`)
      | Match => Js.Json.string("match")
      | Map => Js.Json.string("map")
      | Index(x) => x->Belt.Int.toFloat->Js.Json.number
      }
  }
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

module Ast_Pattern = {
  type binding = [#Binding(loc, string)]
  type arr_<'t> = [#Array(loc, array<'t>) | #ArrayWithTailBinding(loc, array<'t>, binding)]
  type rec t = [
    | #Null(loc)
    | #False(loc)
    | #True(loc)
    | #String(loc, string)
    | #Number(loc, float)
    | #Object(loc, array<(string, t)>)
    | arr_<t>
    | binding
  ]
  type arr = arr_<t>

  let toString = (x: t) =>
    switch x {
    | #True(_) | #False(_) => "boolean"
    | #Null(_) => "null"
    | #String(_) => "string"
    | #Number(_) => "number"
    | #Array(_) | #ArrayWithTailBinding(_) => "array"
    | #Object(_) => "object"
    | #Binding(_, x) => `binding: \`${x}\``
    }

  let toLocation = (x: t) =>
    switch x {
    | #True(x)
    | #False(x)
    | #Null(x)
    | #String(x, _)
    | #Number(x, _)
    | #Array(x, _)
    | #ArrayWithTailBinding(x, _, _)
    | #Object(x, _)
    | #Binding(x, _) => x
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
  type mapPattern = [Ast_Pattern.binding | Ast_Pattern.arr]
  type rec node<'a> =
    | Text(string, trim)
    // The first echo item that isn't null will be returned.
    | Echo(loc, NonEmpty.t<Echo.t>)
    | Match(loc, NonEmpty.t<(loc, string)>, NonEmpty.t<case<'a>>)
    | Map(loc, mapPattern, NonEmpty.t<case<'a>>)
    | Component({
        loc: loc,
        name: string,
        props: array<(string, Ast_Pattern.t)>,
        children: array<(string, child<'a>)>,
        f: 'a,
      })
  and nodes<'a> = array<node<'a>>
  and case<'a> = {patterns: NonEmpty.t<NonEmpty.t<Ast_Pattern.t>>, ast: nodes<'a>}
  and child<'a> = ChildName(string) | ChildBlock(nodes<'a>)
  type t<'a> = {ast: nodes<'a>, name: string}
}

type rec ast<'a> = Ast.t<templateU<'a>>
and template<'a> = (environment<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a
and templateU<'a> = (. environment<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a
and environment<'a> = {
  render: (. ast<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a,
  return: (. string) => 'a,
  error: (. string) => 'a,
  mapChild: (. 'a, string => string) => 'a,
  flatMapChild: (. 'a, string => 'a) => 'a,
}
