/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

// This is a placeholder type until we add more sophisticated error reporting.
@unboxed
type loc = Loc(int)

type kind = [
  | #Type
  | #Matching
  | #Render
  | #Compile
  | #Pattern
  | #Parse
  | #Syntax
  | #Decode
]

type location = {@live character: int}

@unboxed
type rec anyExn = AnyExn(_): anyExn

type t = {
  @live message: string,
  @live kind: kind,
  @live location: option<location>,
  @live path: array<string>,
  @live exn: option<anyExn>,
}

exception Exit(t)

module type Debuggable = {
  type t
  let toString: t => string
  let toLocation: t => loc
}

type debuggable<'a> = module(Debuggable with type t = 'a)

/* Lexer errors */
let illegalIdentifier: (~loc: loc, ~name: string, ~identifier: string) => t
let invalidCharacter: (~loc: loc, ~name: string, ~character: string) => t
let unexpectedCharacter: (~loc: loc, ~name: string, ~character: string, ~expected: string) => t
let unexpectedEof: (~loc: loc, ~name: string) => t
let unknownEscapeSequence: (~loc: loc, ~name: string, ~char: string) => t
let unterminatedComment: (~loc: loc, ~name: string) => t
let unterminatedString: (~loc: loc, ~name: string) => t

/* Parse errors */
let unexpectedToken: ('a, debuggable<'a>, ~name: string) => t

/* Type errors */
let cantNarrowType: (~loc: loc, 'a, 'a, 'a => string) => t
let childNotAllowedInRoot: loc => t
let childTypeMismatch: ('a, 'a, ~loc: loc, 'a => string) => t
let mapPatternSizeMismatch: (~loc: loc, ~name: string) => t
let missingProp: (string, 'a, ~loc: loc, ~name: string, ~comp: string, 'a => string) => t
let nonNullableEchoLiteral: loc => t
let patternNumberMismatch: (~loc: loc, ~name: string) => t
let tupleSizeMismatch: (~loc: loc, ~name: string, int, int) => t
let typeMismatch: ('a, 'a, ~loc: loc, ~name: string, 'a => string) => t

/* Matching errors */
let nameBoundMultipleTimes: (~loc: loc, ~binding: string, ~name: string) => t
let partialMatch: ('a, 'a => string, ~loc: loc) => t
let unusedCase: (NonEmpty.t<'a>, debuggable<'a>) => t

/* Compile errors */
let duplicateCompName: string => t
let uncaughtCompileError: ('e, ~name: string) => t

/* Render errors */
let customError: string => t
let decodeError: (~stack: list<string>, 'a, Js.Json.t, 'a => string) => t
let decodeErrorMissingKey: (~stack: list<string>, string) => t
let uncaughtComponentError: ('e, ~stack: list<string>) => t

/* Other errors */
let cyclicDependency: (~loc: loc, ~name: string, ~stack: list<string>) => t
let missingComponent: (~name: string, ~loc: loc, string) => t
