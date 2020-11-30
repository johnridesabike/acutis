/**
   Copyright 2020 John Jackson

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
    | BindingTypeMismatchErr({
        data: Js.Json.tagged_t,
        pattern: Acutis_Types.Pattern_Ast.node,
        binding: Acutis_Types.identifier,
      })
    | BindingDoesNotExistErr({loc: Acutis_Types.loc, binding: Acutis_Types.identifier})
  let toJson: (
    Acutis_Types.Pattern_Ast.node,
    ~props: Js.Dict.t<Js.Json.t>,
  ) => result<Js.Json.t, toJsonErrors>

  type errors =
    | NoMatch
    | PatternNumberMismatch
    | PatternTypeMismatch({data: Js.Json.tagged_t, pattern: Acutis_Types.Pattern_Ast.node})
    | TooManyBindings({loc: Acutis_Types.loc, binding: Acutis_Types.identifier})

  let test: (
    Acutis_Types.Pattern_Ast.sequence,
    Acutis_Types.NonEmpty.t<Js.Json.t>,
  ) => result<Js.Dict.t<Js.Json.t>, errors>

  type t<'a> = {
    patterns: Acutis_Types.NonEmpty.t<Acutis_Types.Pattern_Ast.sequence>,
    f: (. Js.Dict.t<Js.Json.t>) => 'a,
  }

  let match: (
    Acutis_Types.NonEmpty.t<t<'a>>,
    Acutis_Types.NonEmpty.t<Js.Json.t>,
  ) => result<'a, errors>
}

let makeContext: Js.Dict.t<Acutis_Types.templateFunctionSync> => Acutis_Types.renderContextSync

let makeContextAsync: Js.Dict.t<
  Acutis_Types.templateFunctionAsync,
> => Acutis_Types.renderContextAsync
