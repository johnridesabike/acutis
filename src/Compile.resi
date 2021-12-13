/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
let escape: (Untyped.Ast.Echo.escape, string) => string

module Ast: {
  module Echo: {
    type escape = Untyped.Ast.Echo.escape = NoEscape | Escape
    type t =
      | Binding(Debug.loc, string, escape)
      | Child(Debug.loc, string)
      | String(Debug.loc, string)
  }
  type rec node<'a> =
    // Trimming is optimized away
    | OText(string)
    // The first echo item that isn't null will be returned.
    | OEcho({loc: Debug.loc, nullables: array<Echo.t>, default: Echo.t})
    // Case matrices are optimized into decision trees
    | OMatch(Debug.loc, NonEmpty.t<Typechecker.Pattern.t>, Matching.t<t<'a>>)
    | OMapList(Debug.loc, Typechecker.Pattern.t, Matching.t<t<'a>>)
    | OMapDict(Debug.loc, Typechecker.Pattern.t, Matching.t<t<'a>>)
    | OComponent({
        loc: Debug.loc,
        props: array<(string, Typechecker.Pattern.t)>,
        children: array<(string, child<'a>)>,
        val: 'a,
      })
  and child<'a> = OChildName(string) | OChildBlock(t<'a>)
  and t<'a> = array<node<'a>>
}

type t<'a> = {
  prop_types: Typescheme.props,
  child_types: Typescheme.Child.props,
  nodes: Ast.t<'a>,
  name: string,
}

type rec template<'a> =
  | Acutis(string, Ast.t<template<'a>>)
  | Function(string, Typescheme.props, Source.fnU<'a>)

module Components: {
  type t<'a>
  let empty: unit => t<'a>
  let makeExn: array<Source.t<string, Source.fnU<'a>>> => t<'a>
  let make: array<Source.t<string, Source.fnU<'a>>> => Result.t<t<'a>>
}

let make: (~name: string, string, Components.t<'a>) => Result.t<t<template<'a>>>
