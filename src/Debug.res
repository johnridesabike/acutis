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

type kind = [#Type | #Render | #Compile | #Pattern | #Parse | #Syntax]

type location = {character: int}

let location = (T.Loc(x)) => {character: x + 1}

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

exception Exit(t)

let stackToPath = x => x->Belt.List.toArray->Belt.Array.mapU(Stack.nameToJson)

/* Lexer errors. */

let unexpectedEof = (~loc, ~name) => {
  kind: #Syntax,
  message: "Unexpected end of file.",
  location: Some(location(loc)),
  path: [Js.Json.string(name)],
  exn: None,
}

let unterminatedComment = (~loc, ~name) => {
  kind: #Syntax,
  message: "Unterminated comment.",
  location: Some(location(loc)),
  path: [Js.Json.string(name)],
  exn: None,
}

let unterminatedString = (~loc, ~name) => {
  kind: #Syntax,
  message: "Unterminated string.",
  location: Some(location(loc)),
  path: [Js.Json.string(name)],
  exn: None,
}

let illegalIdentifier = (~loc, ~name, ~identifier) => {
  kind: #Syntax,
  message: `"${identifier}" is an illegal identifier name.`,
  location: Some(location(loc)),
  path: [Js.Json.string(name)],
  exn: None,
}

let invalidCharacter = (~loc, ~name, ~character) => {
  kind: #Syntax,
  message: `Invalid character: "${character}".`,
  location: Some(location(loc)),
  path: [Js.Json.string(name)],
  exn: None,
}

let unexpectedCharacter = (~loc, ~name, ~character, ~expected) => {
  kind: #Syntax,
  message: `Unexpected character: "${character}". Expected: "${expected}".`,
  location: Some(location(loc)),
  path: [Js.Json.string(name)],
  exn: None,
}

/* Parse errors. */

let unexpectedToken = (t, ~name) => {
  message: `Unexpected token: "${T.Token.toString(t)}".`,
  kind: #Parse,
  location: Some(location(T.Token.toLocation(t))),
  path: [Js.Json.string(name)],
  exn: None,
}

let badMapTypeParse = (t, ~name) => {
  message: `Bad map type: "${T.Ast_Pattern.toString(t)}". I can only map bindings and arrays.`,
  kind: #Parse,
  location: Some(location(T.Ast_Pattern.toLocation(t))),
  path: [Js.Json.string(name)],
  exn: None,
}

/* Compile errors. */

let jsonString = (. s) => Js.Json.string(s)

let cyclicDependency = (~loc, ~name, ~stack) => {
  message: `Cyclic dependency detected. I can't compile any components in this path.`,
  kind: #Compile,
  location: Some(location(loc)),
  path: list{name, ...stack}->Belt.List.toArray->Belt.Array.mapU(jsonString),
  exn: None,
}

let componentDoesNotExist = (~loc, ~name, ~stack) => {
  message: `Component "${name}" either does not exist or couldn't be compiled.`,
  kind: #Compile,
  location: Some(location(loc)),
  path: stack->Belt.List.toArray->Belt.Array.mapU(jsonString),
  exn: None,
}

let duplicateCompName = name => {
  message: `The template component name "${name}" was used twice. Every component needs a unique name.`,
  location: None,
  path: [],
  kind: #Compile,
  exn: None,
}

let uncaughtCompileError = (e, ~name) => {
  message: `An exception was thrown while compiling this template. This is probably due to malformed input.`,
  location: None,
  path: [Js.Json.string(name)],
  kind: #Compile,
  exn: Some(AnyExn(e)),
}

/* Render errors */

let jsonTaggedTToString = (x: Js.Json.tagged_t) =>
  switch x {
  | JSONTrue | JSONFalse => "boolean"
  | JSONNull => "null"
  | JSONString(_) => "string"
  | JSONNumber(_) => "number"
  | JSONArray(_) => "array"
  | JSONObject(_) => "object"
  }

let patternTypeMismatch = (~data, ~pattern, ~stack) => {
  let data = jsonTaggedTToString(data)
  let type_ = T.Ast_Pattern.toString(pattern)
  {
    message: `This pattern is type ${type_} but the data is type ${data}.`,
    kind: #Type,
    location: Some(location(T.Ast_Pattern.toLocation(pattern))),
    path: stackToPath(stack),
    exn: None,
  }
}

let bindingTypeMismatch = (~data, ~pattern, ~binding, ~stack) => {
  let data = jsonTaggedTToString(data)
  let p = T.Ast_Pattern.toString(pattern)
  {
    message: `"${binding}" is type ${p} but the data is type ${data}.`,
    kind: #Type,
    location: Some(location(T.Ast_Pattern.toLocation(pattern))),
    path: stackToPath(stack),
    exn: None,
  }
}

let nameBoundMultipleTimes = (~loc, ~binding, ~stack) => {
  message: `"${binding}" is bound multiple times in this pattern.`,
  kind: #Pattern,
  location: Some(location(loc)),
  path: stackToPath(stack),
  exn: None,
}

let noMatchFound = (~loc, ~stack) => {
  location: Some(location(loc)),
  path: stackToPath(stack),
  kind: #Pattern,
  message: "None of the patterns match the data. Consider a catch-all case to avoid this.",
  exn: None,
}

let patternNumberMismatch = (~loc, ~stack) => {
  location: Some(location(loc)),
  path: stackToPath(stack),
  kind: #Pattern,
  message: "The number of patterns does not match the number of data.",
  exn: None,
}

let badEchoType = (~loc, ~binding, ~type_, ~stack) => {
  let type_ = jsonTaggedTToString(type_)
  {
    location: Some(location(loc)),
    path: stackToPath(stack),
    kind: #Render,
    message: `"${binding}" is type ${type_}. I can only echo strings and numbers.`,
    exn: None,
  }
}

let bindingDoesNotExist = (~loc, ~binding, ~stack) => {
  location: Some(location(loc)),
  path: stackToPath(stack),
  kind: #Render,
  message: `Binding "${binding}" does not exist.`,
  exn: None,
}

let childDoesNotExist = (~loc, ~child, ~stack) => {
  location: Some(location(loc)),
  path: stackToPath(stack),
  kind: #Render,
  message: `Template child "${child}" does not exist.`,
  exn: None,
}

let badMapType = (~loc, ~binding, ~type_, ~stack) => {
  let type_ = jsonTaggedTToString(type_)
  {
    location: Some(location(loc)),
    path: stackToPath(stack),
    kind: #Type,
    message: `"${binding}" is a ${type_}. I can only map arrays.`,
    exn: None,
  }
}

let uncaughtComponentError = (e, ~stack) => {
  message: `An exception was thrown while rendering a template component.`,
  location: None,
  path: stackToPath(stack),
  kind: #Render,
  exn: Some(AnyExn(e)),
}

let customError = (message, ~stack) => {
  message: message,
  location: None,
  path: stackToPath(stack),
  kind: #Render,
  exn: None,
}
