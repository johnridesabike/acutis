/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

open Acutis_Types

type kind = [#Type | #Matching | #Render | #Compile | #Pattern | #Parse | #Syntax | #Decode]

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

/* Lexer errors. */
let illegalIdentifier: (~loc: loc, ~name: string, ~identifier: string) => t
let invalidCharacter: (~loc: loc, ~name: string, ~character: string) => t
let unexpectedCharacter: (~loc: loc, ~name: string, ~character: string, ~expected: string) => t
let unexpectedEof: (~loc: loc, ~name: string) => t
let unterminatedComment: (~loc: loc, ~name: string) => t
let unterminatedString: (~loc: loc, ~name: string) => t
let unknownEscapeSequence: (~loc: loc, ~name: string, ~char: string) => t

/* Parse errors. */
let badMapArrayPattern: (Ast_Pattern.t, ~name: string) => t
let badMapDictPattern: (Ast_Pattern.t, ~name: string) => t
let unexpectedToken: (Token.t, ~name: string) => t

/* Compile errors. */
let componentDoesNotExist: (~loc: loc, ~name: string, ~stack: list<string>) => t
let cyclicDependency: (~loc: loc, ~name: string, ~stack: list<string>) => t
let duplicateCompName: string => t
let uncaughtCompileError: ('e, ~name: string) => t

/* Render errors */
let badEchoType: (~loc: loc, ~binding: string, ~type_: Js.Json.tagged_t, ~stack: Stack.t) => t
let badMapArrayType: (~loc: loc, ~binding: string, ~type_: Js.Json.tagged_t, ~stack: Stack.t) => t
let badMapDictType: (~loc: loc, ~binding: string, ~type_: Js.Json.tagged_t, ~stack: Stack.t) => t
let bindingDoesNotExist: (~loc: loc, ~binding: string, ~stack: Stack.t) => t
let bindingTypeMismatch: (
  ~data: Js.Json.tagged_t,
  ~pattern: Ast_Pattern.t,
  ~binding: string,
  ~stack: Stack.t,
) => t
let childDoesNotExist: (~loc: loc, ~child: string, ~stack: Stack.t) => t
let customError: (string, ~stack: Stack.t) => t
let nameBoundMultipleTimes: (~loc: loc, ~binding: string, ~stack: Stack.t) => t
let noMatchFound: (~loc: loc, ~stack: Stack.t) => t
let patternTypeMismatch: (~data: Js.Json.tagged_t, ~pattern: Ast_Pattern.t, ~stack: Stack.t) => t
let patternNumberMismatch: (~loc: loc, ~stack: Stack.t) => t
let uncaughtComponentError: ('e, ~stack: Stack.t) => t
