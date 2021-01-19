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
  type toJsonErrors =
    | BindingTypeMismatch({
        data: Js.Json.tagged_t,
        pattern: Acutis_Types.Pattern_Ast.t,
        binding: string,
      })
    | BindingDoesNotExist({loc: Acutis_Types.loc, binding: string})

  let toJson: (
    Acutis_Types.Pattern_Ast.t,
    ~props: Acutis_Types.props,
  ) => result<Js.Json.t, toJsonErrors>

  type errors =
    | NoMatch
    | PatternNumberMismatch
    | PatternTypeMismatch({data: Js.Json.tagged_t, pattern: Acutis_Types.Pattern_Ast.t})
    | TooManyBindings({loc: Acutis_Types.loc, binding: string})

  let test: (
    Acutis_Types.NonEmpty.t<Acutis_Types.Pattern_Ast.t>,
    Acutis_Types.NonEmpty.t<Js.Json.t>,
  ) => result<Acutis_Types.props, errors>
}

let make: (
  ~ast: Acutis_Types.Ast.ast,
  ~props: Acutis_Types.props,
  ~children: Js.Dict.t<'a>,
  ~envData: Acutis_Types.environmentData<'a>,
  ~makeEnv: (. Acutis_Types.environmentData<'a>) => Acutis_Types.environment<'a>,
  ~error: (. array<Acutis_Types.Errors.t>) => 'a,
  ~try_: (. (. unit) => 'a, ~catch: (. _) => 'a) => 'a,
  ~reduceQueue: (. Belt.MutableQueue.t<'a>) => 'a,
) => Belt.MutableQueue.t<'a>
