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

open Acutis_Types
open Errors

%%private(
  let nameToJson = x =>
    switch x {
    | None => Js.Json.null
    | Some(x) => Js.Json.string(x)
    }
)

let stackToPath = x => x->Belt.List.toArray->Belt.Array.mapU(Stack.nameToJson)

exception CompileError(Errors.t)

/* Lexer errors. */

let unexpectedEofExn = (~loc, ~name) =>
  raise(
    CompileError({
      kind: #Syntax,
      message: "Unexpected end of file.",
      location: Some(location(loc)),
      path: [nameToJson(name)],
      exn: None,
    }),
  )

let unterminatedCommentExn = (~loc, ~name) =>
  raise(
    CompileError({
      kind: #Syntax,
      message: "Unterminated comment.",
      location: Some(location(loc)),
      path: [nameToJson(name)],
      exn: None,
    }),
  )

let unterminatedStringExn = (~loc, ~name) =>
  raise(
    CompileError({
      kind: #Syntax,
      message: "Unterminated string.",
      location: Some(location(loc)),
      path: [nameToJson(name)],
      exn: None,
    }),
  )

let illegalIdentifierExn = (~loc, ~name, ~identifier) =>
  raise(
    CompileError({
      kind: #Parse,
      message: `"${identifier}" is an illegal identifier name.`,
      location: Some(location(loc)),
      path: [nameToJson(name)],
      exn: None,
    }),
  )

let invalidCharacterExn = (~loc, ~name, ~character) =>
  raise(
    CompileError({
      kind: #Syntax,
      message: `Invalid character: "${character}".`,
      location: Some(location(loc)),
      path: [nameToJson(name)],
      exn: None,
    }),
  )

let unexpectedCharacterExn = (~loc, ~name, ~character, ~expected) =>
  raise(
    CompileError({
      kind: #Syntax,
      message: `Unexpected character: "${character}". Expected: "${expected}".`,
      location: Some(location(loc)),
      path: [nameToJson(name)],
      exn: None,
    }),
  )

/* Parse errors. */

let unexpectedTokenExn = (t, ~name) =>
  raise(
    CompileError({
      message: `Unexpected token: "${Token.toString(t)}".`,
      kind: #Parse,
      location: Some(location(Token.toLocation(t))),
      path: [nameToJson(name)],
      exn: None,
    }),
  )

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

let componentDoesNotExist = (~loc, ~component, ~stack) => {
  message: `Component "${component}" does not exist.`,
  kind: #Render,
  location: Some(location(loc)),
  path: stackToPath(stack),
  exn: None,
}

let patternTypeMismatch = (~data, ~pattern, ~stack) => {
  let data = jsonTaggedTToString(data)
  let type_ = Ast_Pattern.toString(pattern)
  {
    message: `This pattern is type ${type_} but the data is type ${data}.`,
    kind: #Type,
    location: Some(location(Ast_Pattern.toLocation(pattern))),
    path: stackToPath(stack),
    exn: None,
  }
}

let bindingTypeMismatch = (~data, ~pattern, ~binding, ~stack) => {
  let data = jsonTaggedTToString(data)
  let p = Ast_Pattern.toString(pattern)
  {
    message: `"${binding}" is type ${p} but the data is type ${data}.`,
    kind: #Type,
    location: Some(location(Ast_Pattern.toLocation(pattern))),
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

/* Input errors */

let invalidInput = (~stack) => {
  message: "A template AST was not valid. Did you forget to compile one?",
  location: None,
  path: stackToPath(stack),
  kind: #Render,
  exn: None,
}

let compileExn = (e, ~name) => {
  message: `An exception was thrown while rendering this template. This is probably due to malformed input.`,
  location: None,
  path: [nameToJson(name)],
  kind: #Compile,
  exn: Some(AnyExn(e)),
}

let componentExn = (e, ~stack) => {
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
