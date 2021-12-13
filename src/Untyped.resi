/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Ast_Pattern: {
  type binding = [#Binding(Debug.loc, string)]
  type arr_<'t> = [
    | #Array(Debug.loc, array<'t>)
    | #ArrayWithTailBinding(Debug.loc, array<'t>, binding)
  ]
  type dict_<'t> = [#Dict(Debug.loc, array<(string, 't)>)]
  type rec t = [
    | #Null(Debug.loc)
    | #Some(Debug.loc, t)
    | #False(Debug.loc)
    | #True(Debug.loc)
    | #String(Debug.loc, string)
    | #Int(Debug.loc, int)
    | #Float(Debug.loc, float)
    | #Tuple(Debug.loc, array<t>)
    | arr_<t>
    | dict_<t>
    | #Object(Debug.loc, array<(string, t)>)
    | binding
  ]
  type arr = arr_<t>
  type dict = dict_<t>

  let toString: t => string

  let toLocation: t => Debug.loc
}

module Ast: {
  module Echo: {
    type escape = NoEscape | Escape
    type t =
      | Binding(Debug.loc, string, escape)
      | Child(Debug.loc, string)
      | String(Debug.loc, string, escape)
      | Int(Debug.loc, int, escape)
      | Float(Debug.loc, float, escape)
  }
  type trim = TrimStart | TrimEnd | TrimBoth | NoTrim
  type mapArrayPattern = [Ast_Pattern.binding | Ast_Pattern.arr]
  type mapDictPattern = [Ast_Pattern.binding | Ast_Pattern.dict]
  type rec node<'a> =
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
  and case<'a> = {
    patterns: NonEmpty.t<NonEmpty.t<Ast_Pattern.t>>,
    nodes: nodes<'a>,
  }
  and child<'a> = ChildName(string) | ChildBlock(nodes<'a>)
  type t<'a> = {nodes: nodes<'a>, name: string}
}

module Pattern: {
  let make: Lexer.t => NonEmpty.t<Ast_Pattern.t>
}

let makeExn: (~name: string, string) => Ast.nodes<unit>
