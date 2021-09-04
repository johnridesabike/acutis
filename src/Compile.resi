/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
