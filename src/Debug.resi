/**
   Copyright 2021 John Jackson

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

type kind = [#Type | #Render | #Compile | #Pattern | #Parse | #Syntax]

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
    | Index(int)
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

/* Parse errors. */
let badMapTypeParse: (Ast_Pattern.t, ~name: string) => t
let unexpectedToken: (Token.t, ~name: string) => t

/* Compile errors. */
let componentDoesNotExist: (~loc: loc, ~name: string, ~stack: list<string>) => t
let cyclicDependency: (~loc: loc, ~name: string, ~stack: list<string>) => t
let duplicateCompName: string => t
let uncaughtCompileError: ('e, ~name: string) => t

/* Render errors */
let badEchoType: (~loc: loc, ~binding: string, ~type_: Js.Json.tagged_t, ~stack: Stack.t) => t
let badMapType: (~loc: loc, ~binding: string, ~type_: Js.Json.tagged_t, ~stack: Stack.t) => t
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
