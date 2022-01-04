/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Array = Belt.Array
module Int = Belt.Int
module List = Belt.List

type t = {name: string, char: int}
let empty = {name: "", char: 0}
let make = (name, char) => {name: name, char: char}
let char = t => t.char

type kind = [
  | #Type
  | #Matching
  | #Render
  | #Compile
  | #Parse
  | #Syntax
]

@unboxed
type rec anyExn = AnyExn(_): anyExn

type error = {
  message: string,
  kind: kind,
  location: option<t>,
  stack: array<string>,
  exn: option<anyExn>,
}

exception Exit(error)

type debug = t

module type Debuggable = {
  type t
  let toString: t => string
  let debug: t => debug
}

type debuggable<'a> = module(Debuggable with type t = 'a)

/* Lexer errors */

let illegalIdentifier = (t, identifier) => {
  kind: #Syntax,
  message: `"${identifier}" is an illegal identifier name.`,
  location: Some(t),
  stack: [],
  exn: None,
}

let invalidCharacter = (t, character) => {
  kind: #Syntax,
  message: `Invalid character: "${character}".`,
  location: Some(t),
  stack: [],
  exn: None,
}

let unexpectedCharacter = (t, ~character, ~expected) => {
  kind: #Syntax,
  message: `Unexpected character: "${character}". Expected: "${expected}".`,
  location: Some(t),
  stack: [],
  exn: None,
}

let unexpectedEof = t => {
  kind: #Syntax,
  message: "Unexpected end of file.",
  location: Some(t),
  stack: [],
  exn: None,
}

let unknownEscapeSequence = (t, char) => {
  kind: #Syntax,
  message: `Unknown escape sequence: ${char}.`,
  location: Some(t),
  stack: [],
  exn: None,
}

let unterminatedComment = t => {
  kind: #Syntax,
  message: "Unterminated comment.",
  location: Some(t),
  stack: [],
  exn: None,
}

let unterminatedString = t => {
  kind: #Syntax,
  message: "Unterminated string.",
  location: Some(t),
  stack: [],
  exn: None,
}

/* Parse errors. */

let unexpectedToken = (type a, token, module(M): debuggable<a>) => {
  message: `Unexpected token: "${M.toString(token)}".`,
  kind: #Parse,
  location: Some(M.debug(token)),
  stack: [],
  exn: None,
}

/* Type errors */

let cantNarrowType = (t, a, b, f) => {
  message: `These types have no subset:
${f(a)}
${f(b)}`,
  kind: #Type,
  exn: None,
  location: Some(t),
  stack: [],
}

let childNotAllowedInRoot = t => {
  message: `Children are not allowed in root templates, only in components.`,
  kind: #Type,
  location: Some(t),
  stack: [],
  exn: None,
}

let childTypeMismatch = (t, a, b, f) => {
  message: `This pattern is type ${f(b)}} but expected type ${f(a)}.`,
  kind: #Type,
  exn: None,
  location: Some(t),
  stack: [],
}

let extraChild = (t, ~comp, child) => {
  message: `This call of component "${comp}" includes unexpected child "${child}."`,
  kind: #Type,
  exn: None,
  location: Some(t),
  stack: [],
}

let mapPatternSizeMismatch = t => {
  message: `Map blocks can only have two patterns per "with" clause: the item and the index.`,
  kind: #Type,
  exn: None,
  location: Some(t),
  stack: [],
}

let missingChild = (t, ~comp, child) => {
  message: `This call of component "${comp}" is missing child "${child}."`,
  kind: #Type,
  exn: None,
  location: Some(t),
  stack: [],
}

let missingProp = (t, p, ~comp, ty, f) => {
  message: `This call of component "${comp}" is missing prop "${p}" of type ${f(ty)}.`,
  kind: #Type,
  exn: None,
  location: Some(t),
  stack: [],
}

let nonNullableEchoLiteral = t => {
  message: `String, int, or float literals are not nullable and therefore allowed before a ? operator.`,
  kind: #Type,
  exn: None,
  location: Some(t),
  stack: [],
}

let patternNumberMismatch = t => {
  location: Some(t),
  stack: [],
  kind: #Type,
  message: "The number of patterns does not match the number of data.",
  exn: None,
}

let tailBindingClash = t => {
  message: `Only bindings are allowed as list tails.`,
  kind: #Type,
  exn: None,
  location: Some(t),
  stack: [],
}

let tupleSizeMismatch = (t, a, b) => {
  message: `This is a ${Int.toString(a)}-tuple but expected a ${Int.toString(b)}-tuple.`,
  kind: #Type,
  exn: None,
  location: Some(t),
  stack: [],
}

let typeMismatch = (t, a, b, f) => {
  message: `This pattern is type ${f(b)} but expected type ${f(a)}.`,
  kind: #Type,
  exn: None,
  location: Some(t),
  stack: [],
}

/* Matching errors */

let nameBoundMultipleTimes = (t, binding) => {
  message: `"${binding}" is bound multiple times in this pattern.`,
  kind: #Matching,
  location: Some(t),
  stack: [],
  exn: None,
}

let partialMatch = (t, pat, f) => {
  message: `This pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
${f(pat)}`,
  kind: #Matching,
  exn: None,
  location: Some(t),
  stack: [],
}

let unusedCase = (type a, pats, module(M): debuggable<a>) => {
  let hd = NonEmpty.hd(pats)
  let t = M.debug(hd)
  let pat = pats->NonEmpty.toArray->Array.joinWith(", ", M.toString)
  {
    message: `This match case is unused:
${pat}`,
    kind: #Matching,
    exn: None,
    location: Some(t),
    stack: [],
  }
}

/* Compile errors */

let duplicateCompName = name => {
  message: `The template component name "${name}" was used twice. Every component needs a unique name.`,
  location: None,
  stack: [],
  kind: #Compile,
  exn: None,
}

let uncaughtCompileError = (e, ~name) => {
  message: `An exception was thrown while compiling template "${name}." This is probably due to malformed input.`,
  location: None,
  stack: [],
  kind: #Compile,
  exn: Some(AnyExn(e)),
}

/* Render errors */

let customError = message => {
  message: message,
  location: None,
  stack: [],
  kind: #Render,
  exn: None,
}

let json = j =>
  switch Js.Json.classify(j) {
  | JSONFalse | JSONTrue => "JSON boolean"
  | JSONNull => "JSON null"
  | JSONString(_) => "JSON string"
  | JSONNumber(_) => "JSON number"
  | JSONObject(_) => "JSON object"
  | JSONArray(_) => "JSON array"
  }

let decodeError = (~stack, a, b, f) => {
  let a = f(a)
  let b = json(b)
  {
    message: `This input is type "${b}", which does not match the template's required type, ${a}.`,
    kind: #Render,
    exn: None,
    location: None,
    stack: List.toArray(stack),
  }
}

let decodeErrorMissingKey = (~stack, k) => {
  message: `Input is missing JSON object key "${k}" which is required.`,
  kind: #Render,
  exn: None,
  location: None,
  stack: List.toArray(stack),
}

let uncaughtComponentError = (t, ~name, ~stack, e) => {
  message: `Template component "${name}" threw an exception.`,
  location: Some(t),
  stack: List.toArray(stack),
  kind: #Render,
  exn: Some(AnyExn(e)),
}

/* Other errors */

let cyclicDependency = (t, ~stack) => {
  let stack = List.toArray(stack)->Array.reverse->Array.joinWith(" -> ", s => s)
  {
    message: `Cyclic dependency detected:
${stack}`,
    kind: #Compile,
    location: Some(t),
    stack: [],
    exn: None,
  }
}

let missingComponent = (t, a) => {
  let name' = switch t.name {
  | "" => "<root>"
  | s => s
  }
  {
    message: `Template component "${a}" is missing, which is required by "${name'}."`,
    kind: #Compile,
    exn: None,
    location: Some(t),
    stack: [],
  }
}
