/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

// These are types shared across modules. Keeping them here avoids cyclic
// dependency errors.

module Ast_Pattern = {
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

  let rec toString = (x: t) =>
    switch x {
    | #True(_) | #False(_) => "boolean"
    | #Null(_) => "null"
    | #Some(_, x) => toString(x)
    | #String(_) => "string"
    | #Int(_) => "int"
    | #Float(_) => "float"
    | #Tuple(_) => "tuple"
    | #Array(_) | #ArrayWithTailBinding(_) => "array"
    | #Object(_) => "object"
    | #Dict(_) => "dictionary"
    | #Binding(_, x) => `binding: \`${x}\``
    }

  let toLocation = (x: t) =>
    switch x {
    | #True(x)
    | #False(x)
    | #Null(x)
    | #Some(x, _)
    | #String(x, _)
    | #Int(x, _)
    | #Float(x, _)
    | #Tuple(x, _)
    | #Array(x, _)
    | #ArrayWithTailBinding(x, _, _)
    | #Object(x, _)
    | #Dict(x, _)
    | #Binding(x, _) => x
    }
}

module Ast = {
  module Echo = {
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
  and case<'a> = {patterns: NonEmpty.t<NonEmpty.t<Ast_Pattern.t>>, nodes: nodes<'a>}
  and child<'a> = ChildName(string) | ChildBlock(nodes<'a>)
  type t<'a> = {nodes: nodes<'a>, name: string}
}

type rec ast<'a> = Ast.t<templateU<'a>>
and template<'a> = (environment<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a
and templateU<'a> = (. environment<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a
and environment<'a> = {
  render: (. ast<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a,
  return: (. string) => 'a,
  error: (. string) => 'a,
  mapChild: (. 'a, string => string) => 'a,
  flatMapChild: (. 'a, string => 'a) => 'a,
}
