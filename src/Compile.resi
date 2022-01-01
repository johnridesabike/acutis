/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

type echo =
  | OEBinding(string, Utils.escape)
  | OEChild(string)
  | OEString(string)

type rec node<'a> =
  // Trimming is optimized away
  | OText(string)
  // The first echo item that isn't null will be returned.
  | OEcho(array<echo>, echo)
  // Case matrices are optimized into decision trees
  | OMatch(NonEmpty.t<Typechecker.Pattern.t>, Matching.t<nodes<'a>>)
  | OMapList(Typechecker.Pattern.t, Matching.t<nodes<'a>>)
  | OMapDict(Typechecker.Pattern.t, Matching.t<nodes<'a>>)
  | OComponent({
      loc: Debug.Loc.t,
      props: array<(string, Typechecker.Pattern.t)>,
      children: array<(string, child<'a>)>,
      val: 'a,
    })

and child<'a> = OChildName(string) | OChildBlock(nodes<'a>)

and nodes<'a> = array<node<'a>>

type t<'a> = {
  prop_types: Typescheme.props,
  nodes: nodes<'a>,
  name: string,
}

type rec template<'a> =
  | Acutis(string, nodes<template<'a>>)
  | Function(string, Typescheme.props, Source.fnU<'a>)

module Components: {
  type t<'a>
  let empty: unit => t<'a>
  let makeExn: array<Source.t<string, Source.fnU<'a>>> => t<'a>
  let make: array<Source.t<string, Source.fnU<'a>>> => Result.t<t<'a>>
}

let make: (~name: string, string, Components.t<'a>) => Result.t<t<template<'a>>>
