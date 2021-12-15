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
  @live path: array<Js.Json.t>,
  @live exn: option<anyExn>,
}

module Stack: {
  type name =
    | Component(string)
    | Section({component: string, section: string})
    | Match
    | Map
    | MapDict
    | Index(Js.Json.t)
  type t = list<name>
}

exception Exit(t)

module type Debuggable = {
  type t
  let toString: t => string
  let toLocation: t => loc
}

type debuggable<'a> = module(Debuggable with type t = 'a)

/* Lexer errors. */
let illegalIdentifier: (~loc: loc, ~name: string, ~identifier: string) => t
let invalidCharacter: (~loc: loc, ~name: string, ~character: string) => t
let unexpectedCharacter: (~loc: loc, ~name: string, ~character: string, ~expected: string) => t
let unexpectedEof: (~loc: loc, ~name: string) => t
let unterminatedComment: (~loc: loc, ~name: string) => t
let unterminatedString: (~loc: loc, ~name: string) => t
let unknownEscapeSequence: (~loc: loc, ~name: string, ~char: string) => t

/* Parse errors. */
let badMapListPattern: ('a, debuggable<'a>, ~name: string) => t
let badMapDictPattern: ('a, debuggable<'a>, ~name: string) => t
let unexpectedToken: ('a, debuggable<'a>, ~name: string) => t
let childNotAllowedInRoot: loc => t

/* Compile errors. */
let cyclicDependency: (~loc: loc, ~name: string, ~stack: list<string>) => t
let duplicateCompName: string => t
let uncaughtCompileError: ('e, ~name: string) => t

let childDoesNotExist: (~loc: loc, ~child: string, ~stack: Stack.t) => t

let uncaughtComponentError: ('e, ~stack: Stack.t) => t

/// NEW STUFF
let childTypeMismatch: ('a, 'a, ~loc: loc, 'a => string) => t
let missingComponent: (~name: string, ~loc: loc, string) => t
let typeMismatch: ('a, 'a, ~loc: loc, ~name: string, 'a => string) => t
let missingProp: (string, 'a, ~loc: loc, ~name: string, ~comp: string, 'a => string) => t
let cantNarrowType: ('a, 'a, 'a => string) => t
let tupleSizeMismatch: (int, int, ~name: string) => t
let mapPatternSizeMismatch: (~loc: loc, ~name: string) => t
let nonNullableEchoLiteral: unit => t
let partialMatch: ('a, 'a => string, ~loc: loc) => t
let unusedCase: (array<'a>, 'a => string) => t
let patternNumberMismatch: (~loc: loc, ~name: string) => t
let nameBoundMultipleTimes: (~loc: loc, ~binding: string, ~name: string) => t
let decodeError: (~stack: list<Js.Json.t>, 'a, Js.Json.t, 'a => string) => t
let decodeErrorMissingKey: (~stack: list<Js.Json.t>, string) => t
let customError: string => t
