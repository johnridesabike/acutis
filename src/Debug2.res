/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
open Debug
module T = Acutis_Types
module Int = Belt.Int

let location = (T.Loc(x)) => {character: x + 1}

let json = j =>
  switch Js.Json.classify(j) {
  | JSONFalse | JSONTrue => "boolean"
  | JSONNull => "null"
  | JSONString(_) => "string"
  | JSONNumber(_) => "number"
  | JSONObject(_) => "object"
  | JSONArray(_) => "array"
  }

/* Type errors */

let childTypeMismatch = (a, b, ~f) => {
  message: `This pattern is type ${f(b)}} but expected type ${f(a)}.`,
  kind: #Type,
  exn: None,
  location: None,
  path: [],
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
    path: [Js.Json.string(name)],
  }
}

let typeMismatch = (a, b, ~f, ~loc, ~name) => {
  message: `This pattern is type ${f(b)} but expected type ${f(a)}.`,
  kind: #Type,
  exn: None,
  location: Some(location(loc)),
  path: [Js.Json.string(name)],
}

let missingProp = (p, t, ~f, ~loc, ~name, ~comp) => {
  message: `This call of component "${comp}" is missing prop "${p}" of type ${f(t)}.`,
  kind: #Type,
  exn: None,
  location: Some(location(loc)),
  path: [Js.Json.string(name)],
}

let cantNarrowType = (a, b, ~f) => {
  message: `These types have no subset:
${f(a)}
${f(b)}`,
  kind: #Type,
  exn: None,
  location: None,
  path: [],
}

let tupleSizeMismatch = (a, b, ~name) => {
  message: `This is a ${Int.toString(a)}-tuple but expected a ${Int.toString(b)}-tuple.`,
  kind: #Type,
  exn: None,
  location: None,
  path: [Js.Json.string(name)],
}

let mapPatternSizeMismatch = (~loc, ~name) => {
  message: `Map blocks can only have two patterns per "with" clause: the item and the index.`,
  kind: #Type,
  exn: None,
  location: Some(location(loc)),
  path: [Js.Json.string(name)],
}

let nonNullableEchoLiteral = () => {
  message: `String, int, or float literals are not nullable and therefore allowed before a ? operator.`,
  kind: #Type,
  exn: None,
  location: None,
  path: [],
}

/* Matching errors */

let partialMatch = (pat, ~f, ~loc) => {
  message: `This pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
${f(pat)}`,
  kind: #Matching,
  exn: None,
  location: Some(location(loc)),
  path: [],
}

let unusedCase = (pat, ~f) => {
  let pat = NonEmpty.joinWith(pat, ", ", (. x) => f(x))
  {
    message: `This match case is unused:
${pat}`,
    kind: #Matching,
    exn: None,
    location: None, // fix this
    path: [],
  }
}

/////////////

let patternNumberMismatch = (~loc, ~name) => {
  location: Some(location(loc)),
  path: [Js.Json.string(name)],
  kind: #Pattern,
  message: "The number of patterns does not match the number of data.",
  exn: None,
}

let nameBoundMultipleTimes = (~loc, ~binding, ~name) => {
  message: `"${binding}" is bound multiple times in this pattern.`,
  kind: #Pattern,
  location: Some(location(loc)),
  path: [Js.Json.string(name)],
  exn: None,
}

/////////

let decodeError = (~f, ~stack, a, b) => {
  let a = f(a)
  let b = json(b)
  {
    message: `This input is type ${b}, which does not match the template's required type, ${a}.`,
    kind: #Decode,
    exn: None,
    location: None,
    path: Belt.List.toArray(stack),
  }
}

let decodeErrorMissingKey = (~stack, k) => {
  message: `Input is missing JSON object key "${k}" which is required.`,
  kind: #Decode,
  exn: None,
  location: None,
  path: Belt.List.toArray(stack),
}

let decodeErrorMissingChild = k => {
  message: `Input is missing required template child "${k}."`,
  kind: #Decode,
  exn: None,
  location: None,
  path: [],
}

let customError = message => {
  message: message,
  location: None,
  path: [],
  kind: #Render,
  exn: None,
}
