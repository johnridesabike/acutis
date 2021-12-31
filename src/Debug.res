/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Array = Belt.Array
module Int = Belt.Int
module List = Belt.List

@unboxed
type loc = Loc(int)

type kind = [#Type | #Matching | #Render | #Compile | #Pattern | #Parse | #Syntax | #Decode]

type location = {character: int}

let location = (Loc(x)) => {character: x + 1}

@unboxed
type rec anyExn = AnyExn(_): anyExn

type t = {
  message: string,
  kind: kind,
  location: option<location>,
  path: array<string>,
  exn: option<anyExn>,
}

exception Exit(t)

module type Debuggable = {
  type t
  let toString: t => string
  let toLocation: t => loc
}

type debuggable<'a> = module(Debuggable with type t = 'a)

/* Lexer errors */

let illegalIdentifier = (~loc, ~name, ~identifier) => {
  kind: #Syntax,
  message: `"${identifier}" is an illegal identifier name.`,
  location: Some(location(loc)),
  path: [name],
  exn: None,
}

let invalidCharacter = (~loc, ~name, ~character) => {
  kind: #Syntax,
  message: `Invalid character: "${character}".`,
  location: Some(location(loc)),
  path: [name],
  exn: None,
}

let unexpectedCharacter = (~loc, ~name, ~character, ~expected) => {
  kind: #Syntax,
  message: `Unexpected character: "${character}". Expected: "${expected}".`,
  location: Some(location(loc)),
  path: [name],
  exn: None,
}

let unexpectedEof = (~loc, ~name) => {
  kind: #Syntax,
  message: "Unexpected end of file.",
  location: Some(location(loc)),
  path: [name],
  exn: None,
}

let unknownEscapeSequence = (~loc, ~name, ~char) => {
  kind: #Syntax,
  message: `Unknown escape sequence: ${char}.`,
  location: Some(location(loc)),
  path: [name],
  exn: None,
}

let unterminatedComment = (~loc, ~name) => {
  kind: #Syntax,
  message: "Unterminated comment.",
  location: Some(location(loc)),
  path: [name],
  exn: None,
}

let unterminatedString = (~loc, ~name) => {
  kind: #Syntax,
  message: "Unterminated string.",
  location: Some(location(loc)),
  path: [name],
  exn: None,
}

/* Parse errors. */

let unexpectedToken = (type a, t, module(M): debuggable<a>, ~name) => {
  message: `Unexpected token: "${M.toString(t)}".`,
  kind: #Parse,
  location: Some(location(M.toLocation(t))),
  path: [name],
  exn: None,
}

/* Type errors */

let cantNarrowType = (~loc, a, b, f) => {
  message: `These types have no subset:
${f(a)}
${f(b)}`,
  kind: #Type,
  exn: None,
  location: Some(location(loc)),
  path: [],
}

let childNotAllowedInRoot = loc => {
  message: `Children are not allowed in root templates, only in components.`,
  kind: #Parse,
  location: Some(location(loc)),
  path: [],
  exn: None,
}

let childTypeMismatch = (a, b, ~loc, f) => {
  message: `This pattern is type ${f(b)}} but expected type ${f(a)}.`,
  kind: #Type,
  exn: None,
  location: Some(location(loc)),
  path: [],
}

let mapPatternSizeMismatch = (~loc, ~name) => {
  message: `Map blocks can only have two patterns per "with" clause: the item and the index.`,
  kind: #Type,
  exn: None,
  location: Some(location(loc)),
  path: [name],
}

let missingProp = (p, t, ~loc, ~name, ~comp, f) => {
  message: `This call of component "${comp}" is missing prop "${p}" of type ${f(t)}.`,
  kind: #Type,
  exn: None,
  location: Some(location(loc)),
  path: [name],
}

let nonNullableEchoLiteral = loc => {
  message: `String, int, or float literals are not nullable and therefore allowed before a ? operator.`,
  kind: #Type,
  exn: None,
  location: Some(location(loc)),
  path: [],
}

let patternNumberMismatch = (~loc, ~name) => {
  location: Some(location(loc)),
  path: [name],
  kind: #Pattern,
  message: "The number of patterns does not match the number of data.",
  exn: None,
}

let tupleSizeMismatch = (~loc, ~name, a, b) => {
  message: `This is a ${Int.toString(a)}-tuple but expected a ${Int.toString(b)}-tuple.`,
  kind: #Type,
  exn: None,
  location: Some(location(loc)),
  path: [name],
}

let typeMismatch = (a, b, ~loc, ~name, f) => {
  message: `This pattern is type ${f(b)} but expected type ${f(a)}.`,
  kind: #Type,
  exn: None,
  location: Some(location(loc)),
  path: [name],
}

/* Matching errors */

let nameBoundMultipleTimes = (~loc, ~binding, ~name) => {
  message: `"${binding}" is bound multiple times in this pattern.`,
  kind: #Pattern,
  location: Some(location(loc)),
  path: [name],
  exn: None,
}

let partialMatch = (pat, f, ~loc) => {
  message: `This pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
${f(pat)}`,
  kind: #Matching,
  exn: None,
  location: Some(location(loc)),
  path: [],
}

let unusedCase = (type a, pats, module(M): debuggable<a>) => {
  let hd = NonEmpty.hd(pats)
  let loc = M.toLocation(hd)
  let pat = pats->NonEmpty.toArray->Array.joinWith(", ", M.toString)
  {
    message: `This match case is unused:
${pat}`,
    kind: #Matching,
    exn: None,
    location: Some(location(loc)),
    path: [],
  }
}

/* Compile errors */

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
  path: [name],
  kind: #Compile,
  exn: Some(AnyExn(e)),
}

/* Render errors */

let customError = message => {
  message: message,
  location: None,
  path: [],
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
    kind: #Decode,
    exn: None,
    location: None,
    path: List.toArray(stack),
  }
}

let decodeErrorMissingKey = (~stack, k) => {
  message: `Input is missing JSON object key "${k}" which is required.`,
  kind: #Decode,
  exn: None,
  location: None,
  path: List.toArray(stack),
}

let uncaughtComponentError = (e, ~stack) => {
  message: `An exception was thrown while rendering a template component.`,
  location: None,
  path: List.toArray(stack),
  kind: #Render,
  exn: Some(AnyExn(e)),
}

/* Other errors */

let cyclicDependency = (~loc, ~name, ~stack) => {
  message: `Cyclic dependency detected. I can't compile any components in this path.`,
  kind: #Compile,
  location: Some(location(loc)),
  path: List.toArray(list{name, ...stack}),
  exn: None,
}

let missingComponent = (~name, ~loc, a) => {
  let name' = switch name {
  | "" => "<root>"
  | s => s
  }
  {
    message: `Template component "${a}" is missing, which is required by "${name'}."`,
    kind: #Compile,
    exn: None,
    location: Some(location(loc)),
    path: [name],
  }
}
