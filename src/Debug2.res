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

/* Type errors */

let childTypeMismatch = (a, b, ~f) => {
  message: `This is type ${f(a)}} but expected type ${f(b)}.`,
  kind: #Type,
  exn: None,
  location: None,
  path: [],
}

let typeMismatch = (a, b, ~f, ~loc) => {
  message: `This is type ${f(a)} but expected type ${f(b)}.`,
  kind: #Type,
  exn: None,
  location: Some(location(loc)),
  path: [],
}

let tupleSizeMismatch = (a, b) => {
  message: `This is a ${Int.toString(a)}-tuple but expected a ${Int.toString(b)}-tuple.`,
  kind: #Type,
  exn: None,
  location: None,
  path: [],
}

let mapPatternSizeMismatch = (~loc) => {
  message: `Map blocks can only have two patterns per "with" clause: the item and the index.`,
  kind: #Type,
  exn: None,
  location: Some(location(loc)),
  path: [],
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
