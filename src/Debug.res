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

exception CompileError(Errors.t)

/* Lexer errors */

let unexpectedEoF = (~loc, ~name) => {
  kind: #Syntax,
  message: "Unexpected end of file.",
  location: location(loc),
  template: name,
  exn: None,
}

let unterminatedComment = (~loc, ~name) => {
  kind: #Syntax,
  message: "Unterminated comment.",
  location: location(loc),
  template: name,
  exn: None,
}

let unterminatedString = (~loc, ~name) => {
  kind: #Syntax,
  message: "Unterminated string.",
  location: location(loc),
  template: name,
  exn: None,
}

let illegalIdentifier = (~loc, ~name, ~identifier as Id(identifier)) => {
  kind: #Parse,
  message: `"${identifier}" is an illegal identifier name.`,
  location: location(loc),
  template: name,
  exn: None,
}

let invalidCharacter = (~loc, ~name, ~character) => {
  kind: #Syntax,
  message: `Invalid character: "${character}".`,
  location: location(loc),
  template: name,
  exn: None,
}

let unexpectedCharacter = (~loc, ~name, ~character, ~expected) => {
  let message = Belt.Array.size(expected) == 1 ? "Expected" : "Expected one of"
  let expected = Js.Array2.joinWith(expected, ", ")
  {
    kind: #Syntax,
    message: `Unexpected character: "${character}". ${message}: "${expected}".`,
    location: location(loc),
    template: name,
    exn: None,
  }
}

/* Parse errors */

let unexpectedToken = (~token, ~name) => {
  let location = location(Acutis_Types.Tokens.toLocation(token))
  let token = Acutis_Types.Tokens.toString(token)
  {
    message: `Unexpected token: "${token}".`,
    kind: #Parse,
    location: location,
    template: name,
    exn: None,
  }
}

let illegalBindingName = (~loc, ~name, ~binding as Id(binding)) => {
  kind: #Parse,
  message: `"${binding}" is a reserved name`,
  template: name,
  location: location(loc),
  exn: None,
}

let invalidStatement = (~loc, ~name, ~statement as Id(statement)) => {
  kind: #Parse,
  message: `Invalid statement: "${statement}".`,
  template: name,
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

let componentDoesNotExist = (~loc, ~component as Id(component), ~name) => {
  message: `Component "${component}" does not exist.`,
  kind: #Render,
  location: location(loc),
  template: name,
  exn: None,
}

let patternTypeMismatch = (~data, ~pattern, ~name) => {
  let data = jsonTaggedTToString(data)
  let type_ = Pattern_Ast.toString(pattern)
  {
    message: `This pattern is type ${type_} but the data is type ${data}.`,
    kind: #Type,
    location: location(Pattern_Ast.toLocation(pattern)),
    template: name,
    exn: None,
  }
}

let bindingTypeMismatch = (~data, ~pattern, ~binding as Id(binding), ~name) => {
  let data = jsonTaggedTToString(data)
  let loc = Pattern_Ast.toLocation(pattern)
  let pattern = Pattern_Ast.toString(pattern)
  {
    message: `"${binding}" is type ${pattern} but the data is type ${data}.`,
    kind: #Type,
    location: location(loc),
    template: name,
    exn: None,
  }
}

let nameBoundMultipleTimes = (~loc, ~binding as Id(binding), ~name) => {
  message: `"${binding}" is bound multiple times in this pattern.`,
  kind: #Pattern,
  location: location(loc),
  template: name,
  exn: None,
}

let noMatchFound = (~loc, ~name) => {
  location: location(loc),
  template: name,
  kind: #Pattern,
  message: "None of the patterns match the data. Consider a catch-all case to avoid this.",
  exn: None,
}

let patternNumberMismatch = (~loc, ~name) => {
  location: location(loc),
  template: name,
  kind: #Pattern,
  message: "The number of patterns does not match the number of data.",
  exn: None,
}

let badEchoType = (~loc, ~binding as Id(binding), ~type_, ~name) => {
  let type_ = jsonTaggedTToString(type_)
  {
    location: location(loc),
    template: name,
    kind: #Render,
    message: `"${binding}" is type ${type_}. I can only echo strings and numbers.`,
    exn: None,
  }
}

let bindingDoesNotExist = (~loc, ~binding as Id(binding), ~name) => {
  location: location(loc),
  template: name,
  kind: #Render,
  message: `Binding "${binding}" does not exist.`,
  exn: None,
}

let childDoesNotExist = (~loc, ~child as Id(child), ~name) => {
  location: location(loc),
  template: name,
  kind: #Render,
  message: `Template child "${child}" does not exist.`,
  exn: None,
}

let badMapType = (~loc, ~binding as Id(binding), ~type_, ~name) => {
  let type_ = jsonTaggedTToString(type_)
  {
    location: location(loc),
    template: name,
    kind: #Type,
    message: `"${binding}" is a ${type_}. I can only map arrays.`,
    exn: None,
  }
}

let exn = (e, ~name, ~kind) => {
  message: `An exception was thrown while rendering this template. This is probably due to malformed input.`,
  location: None,
  template: name,
  kind: kind,
  exn: Some(e),
}
