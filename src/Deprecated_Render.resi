/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

open Acutis_Types

module Pattern: {
  let toJson: (
    Ast_Pattern.t,
    ~props: Js.Dict.t<Js.Json.t>,
    ~stack: Debug.Stack.t,
  ) => Result.t<Js.Json.t>

  type testResult<'a> = NoMatch | Result(Result.t<'a>)

  let test: (
    NonEmpty.t<Ast_Pattern.t>,
    NonEmpty.t<Js.Json.t>,
    ~loc: Debug.loc,
    ~stack: Debug.Stack.t,
  ) => testResult<Belt.Map.String.t<Js.Json.t>>
}

let make: (
  ~nodes: Ast.nodes<Untyped.templateU<'a>>,
  ~props: Js.Dict.t<Js.Json.t>,
  ~children: Js.Dict.t<'a>,
  ~stack: Debug.Stack.t,
  ~makeEnv: (. Debug.Stack.t) => environment<'a>,
  ~error: (. array<Debug.t>) => 'a,
  ~try_: (. (. unit) => 'a, ~catch: (. _) => 'a) => 'a,
  ~reduceQueue: (. Belt.MutableQueue.t<'a>) => 'a,
) => Belt.MutableQueue.t<'a>
