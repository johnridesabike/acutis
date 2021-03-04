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

module Pattern: {
  let toJson: (
    Ast_Pattern.t,
    ~props: Js.Dict.t<Js.Json.t>,
    ~stack: Debug.Stack.t,
  ) => result<Js.Json.t, Debug.t>

  type errors
  let noMatch: errors // exported for debugging

  let test: (
    NonEmpty.t<Ast_Pattern.t>,
    NonEmpty.t<Js.Json.t>,
  ) => result<Js.Dict.t<Js.Json.t>, errors>
}

let make: (
  ~ast: Ast.nodes<templateU<'a>>,
  ~props: Js.Dict.t<Js.Json.t>,
  ~children: Js.Dict.t<'a>,
  ~stack: Debug.Stack.t,
  ~makeEnv: (. Debug.Stack.t) => environment<'a>,
  ~error: (. array<Debug.t>) => 'a,
  ~try_: (. (. unit) => 'a, ~catch: (. _) => 'a) => 'a,
  ~reduceQueue: (. Belt.MutableQueue.t<'a>) => 'a,
) => Belt.MutableQueue.t<'a>
