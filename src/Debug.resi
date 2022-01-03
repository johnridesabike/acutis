/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Loc: {
  type t = {@live char: int}
  let empty: t
  let make: int => t
  let char: t => int
}

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

type t = {
  @live message: string,
  @live kind: kind,
  @live location: option<Loc.t>,
  @live path: array<string>,
  @live exn: option<anyExn>,
}

exception Exit(t)

module type Debuggable = {
  type t
  let toString: t => string
  let toLocation: t => Loc.t
}

type debuggable<'a> = module(Debuggable with type t = 'a)

/* Lexer errors */
let illegalIdentifier: (~loc: Loc.t, ~name: string, ~identifier: string) => t
let invalidCharacter: (~loc: Loc.t, ~name: string, ~character: string) => t
let unexpectedCharacter: (~loc: Loc.t, ~name: string, ~character: string, ~expected: string) => t
let unexpectedEof: (~loc: Loc.t, ~name: string) => t
let unknownEscapeSequence: (~loc: Loc.t, ~name: string, ~char: string) => t
let unterminatedComment: (~loc: Loc.t, ~name: string) => t
let unterminatedString: (~loc: Loc.t, ~name: string) => t

/* Parse errors */
let unexpectedToken: ('a, debuggable<'a>, ~name: string) => t

/* Type errors */
let cantNarrowType: (~loc: Loc.t, ~name: string, 'a, 'a, 'a => string) => t
let childNotAllowedInRoot: Loc.t => t
let childTypeMismatch: (~loc: Loc.t, ~name: string, 'a, 'a, 'a => string) => t
let extraChild: (~loc: Loc.t, ~name: string, ~comp: string, string) => t
let mapPatternSizeMismatch: (~loc: Loc.t, ~name: string) => t
let missingChild: (~loc: Loc.t, ~name: string, ~comp: string, string) => t
let missingProp: (string, 'a, ~loc: Loc.t, ~name: string, ~comp: string, 'a => string) => t
let nonNullableEchoLiteral: (~loc: Loc.t, ~name: string) => t
let patternNumberMismatch: (~loc: Loc.t, ~name: string) => t
let tailBindingClash: (~loc: Loc.t, ~name: string) => t
let tupleSizeMismatch: (~loc: Loc.t, ~name: string, int, int) => t
let typeMismatch: ('a, 'a, ~loc: Loc.t, ~name: string, 'a => string) => t

/* Matching errors */
let nameBoundMultipleTimes: (~loc: Loc.t, ~binding: string, ~name: string) => t
let partialMatch: (~loc: Loc.t, ~name: string, 'a, 'a => string) => t
let unusedCase: (~name: string, NonEmpty.t<'a>, debuggable<'a>) => t

/* Compile errors */
let duplicateCompName: string => t
let uncaughtCompileError: ('e, ~name: string) => t

/* Render errors */
let customError: string => t
let decodeError: (~stack: list<string>, 'a, Js.Json.t, 'a => string) => t
let decodeErrorMissingKey: (~stack: list<string>, string) => t
let uncaughtComponentError: (~loc: Loc.t, ~name: string, ~stack: list<string>, 'e) => t

/* Other errors */
let cyclicDependency: (~loc: Loc.t, ~stack: list<string>) => t
let missingComponent: (~name: string, ~loc: Loc.t, string) => t
