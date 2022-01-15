/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

type t = {name: string, char: int}
let empty: t
let make: (string, int) => t
let char: t => int

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
  @live message: string,
  @live kind: kind,
  @live location: option<t>,
  @live stack: array<string>,
  @live exn: option<anyExn>,
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
let illegalIdentifier: (t, string) => error
let invalidCharacter: (t, string) => error
let unexpectedCharacter: (t, ~character: string, ~expected: string) => error
let unexpectedEof: t => error
let unknownEscapeSequence: (t, string) => error
let unterminatedComment: t => error
let unterminatedString: t => error

/* Parse errors */
let unexpectedToken: ('a, debuggable<'a>) => error

/* Type errors */
let cantNarrowType: (t, 'a, 'a, 'a => string) => error
let childNotAllowedInRoot: t => error
let childTypeMismatch: (t, 'a, 'a, 'a => string) => error
let extraChild: (t, ~comp: string, string) => error
let mapPatternSizeMismatch: t => error
let missingChild: (t, ~comp: string, string) => error
let missingRecordField: (t, string, 'a, 'a => string) => error
let nameBoundMultipleTimes: (t, string) => error
let nonNullableEchoLiteral: t => error
let patternNumberMismatch: t => error
let tailBindingClash: t => error
let tupleSizeMismatch: (t, int, int) => error
let typeMismatch: (t, 'a, 'a, 'a => string) => error
let underscoreInConstruct: t => error
let variableMissingInPattern: (t, string) => error

/* Matching errors */
let partialMatch: (t, 'a, 'a => string) => error
let unusedCase: (NonEmpty.t<'a>, debuggable<'a>) => error

/* Compile errors */
let duplicateCompName: string => error
let uncaughtCompileError: ('e, ~name: string) => error

/* Render errors */
let customError: string => error
let decodeError: (~stack: list<string>, 'a, Js.Json.t, 'a => string) => error
let decodeErrorMissingKey: (~stack: list<string>, string) => error
let uncaughtComponentError: (t, ~name: string, ~stack: list<string>, 'e) => error

/* Other errors */
let cyclicDependency: (t, ~stack: list<string>) => error
let missingComponent: (t, string) => error
