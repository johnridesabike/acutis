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

open Acutis_Types
open Errors

%%private(
  let nameToJson = x =>
    switch x {
    | None => Js.Json.null
    | Some(x) => Js.Json.string(x)
    }
)

let stackToPath = x => x->Belt.List.mapU(Stack.nameToJson)->Belt.List.toArray

exception CompileError(Errors.t)

/* Lexer errors */

let unexpectedEoF = (~loc, ~name) => {
  kind: #Syntax,
  message: "Unexpected end of file.",
  location: location(loc),
  path: [nameToJson(name)],
  exn: None,
}

let unterminatedComment = (~loc, ~name) => {
  kind: #Syntax,
  message: "Unterminated comment.",
  location: location(loc),
  path: [nameToJson(name)],
  exn: None,
}

let unterminatedString = (~loc, ~name) => {
  kind: #Syntax,
  message: "Unterminated string.",
  location: location(loc),
  path: [nameToJson(name)],
  exn: None,
}

let illegalIdentifier = (~loc, ~name, ~identifier) => {
  kind: #Parse,
  message: `"${identifier}" is an illegal identifier name.`,
  location: location(loc),
  path: [nameToJson(name)],
  exn: None,
}

let invalidCharacter = (~loc, ~name, ~character) => {
  kind: #Syntax,
  message: `Invalid character: "${character}".`,
  location: location(loc),
  path: [nameToJson(name)],
  exn: None,
}

let unexpectedCharacter = (~loc, ~name, ~character, ~expected) => {
  let message = Belt.Array.size(expected) == 1 ? "Expected" : "Expected one of"
  let expected = Js.Array2.joinWith(expected, ", ")
  {
    kind: #Syntax,
    message: `Unexpected character: "${character}". ${message}: "${expected}".`,
    location: location(loc),
    path: [nameToJson(name)],
    exn: None,
  }
}

/* Parse errors */

let unexpectedToken = (~token, ~name) => {
  let location = location(Tokens.toLocation(token))
  let token = Tokens.toString(token)
  {
    message: `Unexpected token: "${token}".`,
    kind: #Parse,
    location: location,
    path: [nameToJson(name)],
    exn: None,
  }
}

let illegalBindingName = (~loc, ~name, ~binding) => {
  kind: #Parse,
  message: `"${binding}" is a reserved name`,
  path: [nameToJson(name)],
  location: location(loc),
  exn: None,
}

let invalidStatement = (~loc, ~name, ~statement) => {
  kind: #Parse,
  message: `Invalid statement: "${statement}".`,
  path: [nameToJson(name)],
  location: location(loc),
  exn: None,
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

let componentDoesNotExist = (~loc, ~component, ~stack) => {
  message: `Component "${component}" does not exist.`,
  kind: #Render,
  location: location(loc),
  path: stackToPath(stack),
  exn: None,
}

let patternTypeMismatch = (~data, ~pattern, ~stack) => {
  let data = jsonTaggedTToString(data)
  let type_ = Pattern_Ast.toString(pattern)
  {
    message: `This pattern is type ${type_} but the data is type ${data}.`,
    kind: #Type,
    location: location(Pattern_Ast.toLocation(pattern)),
    path: stackToPath(stack),
    exn: None,
  }
}

let bindingTypeMismatch = (~data, ~pattern, ~binding, ~stack) => {
  let data = jsonTaggedTToString(data)
  let loc = Pattern_Ast.toLocation(pattern)
  let pattern = Pattern_Ast.toString(pattern)
  {
    message: `"${binding}" is type ${pattern} but the data is type ${data}.`,
    kind: #Type,
    location: location(loc),
    path: stackToPath(stack),
    exn: None,
  }
}

let nameBoundMultipleTimes = (~loc, ~binding, ~stack) => {
  message: `"${binding}" is bound multiple times in this pattern.`,
  kind: #Pattern,
  location: location(loc),
  path: stackToPath(stack),
  exn: None,
}

let noMatchFound = (~loc, ~stack) => {
  location: location(loc),
  path: stackToPath(stack),
  kind: #Pattern,
  message: "None of the patterns match the data. Consider a catch-all case to avoid this.",
  exn: None,
}

let patternNumberMismatch = (~loc, ~stack) => {
  location: location(loc),
  path: stackToPath(stack),
  kind: #Pattern,
  message: "The number of patterns does not match the number of data.",
  exn: None,
}

let badEchoType = (~loc, ~binding, ~type_, ~stack) => {
  let type_ = jsonTaggedTToString(type_)
  {
    location: location(loc),
    path: stackToPath(stack),
    kind: #Render,
    message: `"${binding}" is type ${type_}. I can only echo strings and numbers.`,
    exn: None,
  }
}

let bindingDoesNotExist = (~loc, ~binding, ~stack) => {
  location: location(loc),
  path: stackToPath(stack),
  kind: #Render,
  message: `Binding "${binding}" does not exist.`,
  exn: None,
}

let childDoesNotExist = (~loc, ~child, ~stack) => {
  location: location(loc),
  path: stackToPath(stack),
  kind: #Render,
  message: `Template child "${child}" does not exist.`,
  exn: None,
}

let badMapType = (~loc, ~binding, ~type_, ~stack) => {
  let type_ = jsonTaggedTToString(type_)
  {
    location: location(loc),
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
