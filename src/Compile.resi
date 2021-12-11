/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module T = Acutis_Types

module Ast_Pattern = T.Ast_Pattern

module Ast: {
  module Echo: {
    type escape = T.Ast.Echo.escape = NoEscape | Escape
    type t = T.Ast.Echo.t =
      | Binding(Debug.loc, string, escape)
      | Child(Debug.loc, string)
      | String(Debug.loc, string, escape)
      | Int(Debug.loc, int, escape)
      | Float(Debug.loc, float, escape)
  }
  type trim = T.Ast.trim = TrimStart | TrimEnd | TrimBoth | NoTrim
  type mapArrayPattern = [Ast_Pattern.binding | Ast_Pattern.arr]
  type mapDictPattern = [Ast_Pattern.binding | Ast_Pattern.dict]
  type rec node<'a> = T.Ast.node<'a> =
    | Text(string, trim)
    // The first echo item that isn't null will be returned.
    | Echo({loc: Debug.loc, nullables: array<Echo.t>, default: Echo.t})
    | Match(Debug.loc, NonEmpty.t<Ast_Pattern.binding>, NonEmpty.t<case<'a>>)
    | MapArray(Debug.loc, mapArrayPattern, NonEmpty.t<case<'a>>)
    | MapDict(Debug.loc, mapDictPattern, NonEmpty.t<case<'a>>)
    | Component({
        loc: Debug.loc,
        name: string,
        props: array<(string, Ast_Pattern.t)>,
        children: array<(string, child<'a>)>,
        f: 'a,
      })
  and nodes<'a> = array<node<'a>>
  and case<'a> = T.Ast.case<'a> = {
    patterns: NonEmpty.t<NonEmpty.t<Ast_Pattern.t>>,
    nodes: nodes<'a>,
  }
  and child<'a> = T.Ast.child<'a> = ChildName(string) | ChildBlock(nodes<'a>)
  type t<'a> = T.Ast.t<'a> = {nodes: nodes<'a>, name: string}
}

type rec ast<'a> = Ast.t<templateU<'a>>
and template<'a> = (environment<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a
and templateU<'a> = (. environment<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a
and environment<'a> = T.environment<'a> = {
  render: (. ast<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a,
  return: (. string) => 'a,
  error: (. string) => 'a,
  mapChild: (. 'a, string => string) => 'a,
  flatMapChild: (. 'a, string => 'a) => 'a,
}

type t<'a> = template<'a>

module Pattern: {
  let make: Lexer.t => NonEmpty.t<Ast_Pattern.t>
}

let makeAstInternalExn: (~name: string, string) => Ast.nodes<unit>

module Deprecated_Components: {
  type t<'a>
  let empty: unit => t<_>
  let make: array<Deprecated_Source.t<'a>> => Result.t<t<'a>>
}

let make: (Deprecated_Source.t<'a>, Deprecated_Components.t<'a>) => Result.t<t<'a>>
