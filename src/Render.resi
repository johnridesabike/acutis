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

module Pattern: {
  let toJson: (
    Acutis_Types.Ast_Pattern.t,
    ~props: Js.Dict.t<Js.Json.t>,
    ~stack: Acutis_Types.Errors.Stack.t,
  ) => result<Js.Json.t, Acutis_Types.Errors.t>

  type errors
  let noMatch: errors // exported for debugging

  let test: (
    Acutis_Types.NonEmpty.t<Acutis_Types.Ast_Pattern.t>,
    Acutis_Types.NonEmpty.t<Js.Json.t>,
  ) => result<Js.Dict.t<Js.Json.t>, errors>
}

let make: (
  ~ast: Acutis_Types.Ast.nodes<Acutis_Types.templateU<'a>>,
  ~props: Js.Dict.t<Js.Json.t>,
  ~children: Js.Dict.t<'a>,
  ~stack: Acutis_Types.Errors.Stack.t,
  ~makeEnv: (. Acutis_Types.Errors.Stack.t) => Acutis_Types.environment<'a>,
  ~error: (. array<Acutis_Types.Errors.t>) => 'a,
  ~try_: (. (. unit) => 'a, ~catch: (. _) => 'a) => 'a,
  ~reduceQueue: (. Belt.MutableQueue.t<'a>) => 'a,
) => Belt.MutableQueue.t<'a>
