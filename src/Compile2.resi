/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
let escape: (Acutis_Types.Ast.Echo.escape, string) => string

module Ast: {
  module Echo: {
    type escape = Acutis_Types.Ast.Echo.escape = NoEscape | Escape
    type t =
      | Binding(Acutis_Types.loc, string, escape)
      | Child(Acutis_Types.loc, string)
      | String(Acutis_Types.loc, string)
  }
  type rec node<'a> =
    // Trimming is optimized away
    | OText(string)
    // The first echo item that isn't null will be returned.
    | OEcho({loc: Acutis_Types.loc, nullables: array<Echo.t>, default: Echo.t})
    // Case matrices are optimized into decision trees
    | OMatch(Acutis_Types.loc, NonEmpty.t<Typechecker.Pattern.t>, Matching.t<t<'a>>)
    | OMapList(Acutis_Types.loc, Typechecker.Pattern.t, Matching.t<t<'a>>)
    | OMapDict(Acutis_Types.loc, Typechecker.Pattern.t, Matching.t<t<'a>>)
    | OComponent({
        loc: Acutis_Types.loc,
        name: string,
        props: array<(string, Typechecker.Pattern.t)>,
        children: array<(string, child<'a>)>,
        f: 'a,
      })
  and child<'a> = OChildName(string) | OChildBlock(t<'a>)
  and t<'a> = array<node<'a>>
}

type t<'a> = {
  prop_types: Source2.TypeScheme.props,
  child_types: Source2.TypeScheme.Child.props,
  nodes: Ast.t<'a>,
  name: string,
}

type rec template<'a> =
  | Acutis(Ast.t<template<'a>>)
  | Function(Source2.TypeScheme.props, Source2.fnU<'a>)

module Components: {
  type t<'a>
  let empty: unit => t<'a>
  let makeExn: array<Source2.t<string, Source2.fnU<'a>>> => t<'a>
  let make: array<Source2.t<string, Source2.fnU<'a>>> => Result.t<t<'a>>
}

let make: (~name: string, string, Components.t<'a>) => Result.t<t<template<'a>>>
