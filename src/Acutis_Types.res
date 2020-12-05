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

module NonEmpty = {
  type t<'a> = List('a, list<'a>)
  let map = (List(x, rest), ~f) => List(f(. x), Belt.List.mapU(rest, f))
  let toList = (List(x, rest)) => list{x, ...rest}
}

@unboxed
type loc = Loc(int)

@unboxed
type identifier = Id(string)

module IdDict = {
  type t<'a> = Js.Dict.t<'a>
  let get = (x, ~key as Id(y)) => Js.Dict.get(x, y)
  let set = (x, ~key as Id(y), ~data) => Js.Dict.set(x, y, data)
  @val @scope("Object")
  external merge: (@as(json`{}`) _, ~base: t<'a>, t<'a>) => t<'a> = "assign"
}

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

  let isLegalBinding = (Id(x)) => Js.Re.test_(bindingRegEx, x) && !isReservedKeyword(x)
}

module Tokens = {
  type t =
    | String(loc, string)
    | JsonString(loc, string)
    | Comment(loc, string)
    | Number(loc, float)
    | Identifier(loc, identifier)
    | ComponentName(loc, identifier)
    | EchoIdentifier(loc, identifier)
    | EchoChildComponent(loc, identifier)
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
    | Identifier(_, Id(x)) => x
    | ComponentName(_, Id(x)) => x
    | Comment(_, x) => `{*${x}*}`
    | EchoIdentifier(_, Id(x)) => `{{ ${x} }}`
    | EchoChildComponent(_, Id(x)) => `{{ ${x} }}`
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
    | ArrayWithTailBinding({loc: loc, array: list<node>, bindLoc: loc, binding: identifier})
    | Object(loc, list<(string, node)>)
    | Binding(loc, identifier)

  type t = NonEmpty.t<node>

  let toString = x =>
    switch x {
    | True(_) | False(_) => "boolean"
    | Null(_) => "null"
    | String(_) => "string"
    | Number(_) => "number"
    | Array(_) | ArrayWithTailBinding(_) => "array"
    | Object(_) => "object"
    | Binding(_, Id(x)) => `binding: \`${x}\``
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

module Ast = {
  type trim = TrimStart | TrimEnd | TrimBoth | NoTrim
  type rec node =
    | Text(string, trim)
    | Unescaped(loc, identifier)
    | EchoBinding(loc, identifier)
    | EchoChild(loc, identifier)
    | EchoString(string)
    | EchoNumber(float)
    | Match(loc, NonEmpty.t<(loc, identifier)>, NonEmpty.t<case>)
    | Map(loc, identifier, NonEmpty.t<case>)
    | Component({
        loc: loc,
        name: identifier,
        props: list<(identifier, Pattern_Ast.node)>,
        children: list<(identifier, childProp)>,
      })
  and case = {patterns: NonEmpty.t<Pattern_Ast.t>, ast: ast}
  and ast = list<node>
  and childProp = ChildName(identifier) | ChildBlock(ast)

  type t = {ast: ast, name: option<identifier>, isCompiledAst: [#VALID_AST]}
}

type props = Js.Dict.t<Js.Json.t>

type renderContext<'a> = (. Ast.t, props, Js.Dict.t<'a>) => 'a
type renderContextSync = renderContext<string>
type renderContextAsync = renderContext<Js.Promise.t<string>>

type templateFunction<'a> = (. renderContext<'a>, props, Js.Dict.t<'a>) => 'a
type templateFunctionSync = templateFunction<string>
type templateFunctionAsync = templateFunction<Js.Promise.t<string>>
