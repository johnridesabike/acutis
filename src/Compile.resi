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

type t<'a> = template<'a>

module Pattern: {
  let make: Lexer.t => NonEmpty.t<Ast_Pattern.t>
}

let makeAstInternalExn: (~name: string, string) => Ast.nodes<unit>

module Components: {
  type t<'a>
  let empty: unit => t<_>
  let make: array<Source.t<'a>> => Result.t<t<'a>>
}

let make: (Source.t<'a>, Components.t<'a>) => Result.t<t<'a>>
